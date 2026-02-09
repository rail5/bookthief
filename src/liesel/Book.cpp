/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#include "Book.h"

#include <algorithm>
#include <iostream>
#include <unistd.h>
#include <type_traits>
#include <cmath>

#include <hpdf.h>

namespace {
	struct HaruErrorState {
		HPDF_STATUS error = HPDF_OK;
		HPDF_STATUS detail = HPDF_OK;
	};

	extern "C" void haru_error_handler(HPDF_STATUS error_no, HPDF_STATUS detail_no, void* user_data) noexcept {
		auto* st = static_cast<HaruErrorState*>(user_data);
		if (!st) return;
		st->error = error_no;
		st->detail = detail_no;
	}

	static std::string haru_error_string(HPDF_STATUS err, HPDF_STATUS detail) {
		std::ostringstream os;
		os << "libharu error=0x" << std::hex << std::setw(4) << std::setfill('0') << static_cast<unsigned>(err)
		   << " detail=0x" << std::hex << std::setw(4) << std::setfill('0') << static_cast<unsigned>(detail);
		return os.str();
	}

	static void haru_check(HPDF_Doc doc, HaruErrorState& st, HPDF_STATUS rc, const char* what) {
		if (rc == HPDF_OK) return;

		const HPDF_STATUS err = (st.error != HPDF_OK) ? st.error : (doc ? HPDF_GetError(doc) : rc);
		const HPDF_STATUS det = (st.detail != HPDF_OK) ? st.detail : (doc ? HPDF_GetErrorDetail(doc) : 0);

		if (doc) HPDF_ResetError(doc);
		throw std::runtime_error(std::string(what) + " failed: " + haru_error_string(err, det));
	}

	template <class T>
	static T haru_require_ptr(HPDF_Doc doc, HaruErrorState& st, T ptr, const char* what) {
		if (ptr) return ptr;

		const HPDF_STATUS err = (st.error != HPDF_OK) ? st.error : (doc ? HPDF_GetError(doc) : HPDF_INVALID_OBJECT);
		const HPDF_STATUS det = (st.detail != HPDF_OK) ? st.detail : (doc ? HPDF_GetErrorDetail(doc) : 0);

		if (doc) HPDF_ResetError(doc);
		throw std::runtime_error(std::string(what) + " failed: " + haru_error_string(err, det));
	}
} // namespace

Liesel::Book::Book() {
	Magick::InitializeMagick(nullptr);
}

uint32_t Liesel::Book::_progress_percent() const {
	if (m_progress_total_units == 0) return 0;
	uint32_t percent =
		static_cast<uint32_t>(
			(static_cast<uint64_t>(m_progress_completed_units) * 100ULL)
			/ static_cast<uint64_t>(m_progress_total_units)
		);
	return (percent > 100) ? 100 : percent; // Clamp to 100%
}

void Liesel::Book::_emit_progress(ProgressEvent event, uint32_t segment_index, uint32_t page_index, const std::string& message) {
	if (!m_progress_cb) return;

	ProgressInfo info;
	info.event = event;
	info.segment_index = segment_index;
	info.page_index = page_index;
	info.percent = _progress_percent();
	info.message = message;
	m_progress_cb(info);
}

void Liesel::Book::_check_cancelled() const {
	if (m_cancel_cb && m_cancel_cb()) {
		throw Liesel::Cancelled("Cancelled");
	}
}

void Liesel::Book::verbose_output(const std::string_view& message) const {
	if (f_verbose) std::cout << "Liesel: " << message << std::endl;
}

void Liesel::Book::calculate_effective_page_indices() {
	if (!pdf_document) throw std::runtime_error("PDF document not loaded.");

	int poppler_pages = pdf_document->pages();
	if (poppler_pages <= 0) throw std::runtime_error("PDF document has no pages.");
	uint32_t number_of_pages = static_cast<uint32_t>(poppler_pages);

	m_effective_page_indices.clear();
	if (!m_page_ranges.has_value()) {
		// All pages
		for (uint32_t i = 0; i < number_of_pages; i++) m_effective_page_indices.push_back(i);
		return;
	}

	// Specific page ranges
	for (const auto& range : m_page_ranges->ranges()) {
		uint32_t start = range.start_page().has_value() ? range.start_page().value() - 1 : 0;
		uint32_t end = range.end_page().has_value() ? range.end_page().value() - 1 : number_of_pages - 1;

		if (start >= number_of_pages) throw std::out_of_range("Page " + std::to_string(start + 1)
			+ " is out of range. Document has " + std::to_string(number_of_pages) + " pages.");

		if (end >= number_of_pages) end = number_of_pages - 1; // Clamp to last page

		if (start <= end) {
			for (uint32_t i = start; i <= end; i++) m_effective_page_indices.push_back(i);
		} else {
			// Backwards range (e.g. 10-1), add the pages in reverse order
			// Make sure we don't underflow
			for (uint32_t i = start; i + 1 > end; i--) m_effective_page_indices.push_back(i);
		}
	}
}

void Liesel::Book::_render_segment(uint32_t segment_number) {
	// Use Poppler to render each page to an image and store in 'pages'
	uint32_t start_index = segment_number * m_segment_size;
	uint32_t end_index;
	if (m_segment_size == UINT32_MAX) {
		end_index = static_cast<uint32_t>(m_effective_page_indices.size());
	} else {
		end_index = std::min(start_index + m_segment_size, static_cast<uint32_t>(m_effective_page_indices.size()));
	}

	for (uint32_t i = start_index; i < end_index; i++) {
		_check_cancelled(); // Throw if something requested cancellation

		uint32_t page_index = m_effective_page_indices[i];
		verbose_output("Rendering page " + std::to_string(page_index + 1) + "...");
		_emit_progress(ProgressEvent::RenderPage, segment_number, i - start_index,
			"Rendering page " + std::to_string(page_index + 1) + "...");

		std::unique_ptr<Liesel::Page> page = std::make_unique<Liesel::Page>();
		page->load(pdf_document.get(), page_index, m_dpi_density);

		if (f_greyscale) page->set_greyscale();
		if (m_threshold_level.has_value()) page->set_threshold(m_threshold_level.value());

		if (f_divide) {
			auto left_page = page->divide();
			pages.push_back(std::move(left_page));
		}

		pages.push_back(std::move(page));

		// Crops should happen **after** dividing the page if applicable
		// because the crop percentages are relative to each half-page now
		pages.back()->crop(m_crop_percentages);
		if (f_divide) pages[pages.size() - 2]->crop(m_crop_percentages);

		verbose_output("Page " + std::to_string(page_index + 1) + " rendered and processed.");

		if (m_progress_completed_units < m_progress_total_units) m_progress_completed_units++;
		_emit_progress(ProgressEvent::RenderPage, segment_number, i - start_index,
			"Rendered page " + std::to_string(page_index + 1) + ".");
	}

	// The total number of pages per segment must ALWAYS be a multiple of 4
	// to ensure proper duplex printing alignment
	// We'll append blanks if necessary
	uint32_t pageWidth = pages[0]->columns();
	uint32_t pageHeight = pages[0]->rows();
	Liesel::Page extra_page;
	extra_page.blank(pageWidth, pageHeight);

	uint32_t pages_in_segment = end_index - start_index;
	while (pages_in_segment % 4 != 0) {
		auto extra_blank = std::make_unique<Liesel::Page>(extra_page);
		pages.push_back(std::move(extra_blank));
		pages_in_segment++;
	}

	verbose_output("All pages for segment " + std::to_string(segment_number + 1) + " rendered.");
}

void Liesel::Book::_maybe_reorder_pages() {
	// Rearrange 'pages' into booklet order and store in 'processed_pages'
	if (pages.empty()) throw std::runtime_error("No pages to arrange for booklet.");
	if (pages.size() % 4 != 0) throw std::runtime_error("Number of pages is not a multiple of 4.");

	if (!f_booklet) { processed_pages = std::move(pages); return; }

	// The pattern:
	// First: Page N on the left (N=last page), Page 1 on the right
	// Second: Page 2 on the left, Page N-1 on the right
	// Third: Page N-2 on the left, Page 3 on the right
	// Fourth: Page 4 on the left, Page N-3 on the right
	// Repeat...
	processed_pages.clear();
	processed_pages.reserve(pages.size() / 2);

	while (!pages.empty()) {
		auto left_page = std::move(pages.back());
		auto right_page = std::move(pages.front());
		pages.pop_back();
		pages.erase(pages.begin());
		left_page->pair_with(std::move(right_page), m_widen_margins_amount);
		left_page->rotate(90.0);
		processed_pages.push_back(std::move(left_page));

		if (pages.empty()) break;
		left_page = std::move(pages.front());
		right_page = std::move(pages.back());
		pages.pop_back();
		pages.erase(pages.begin());
		left_page->pair_with(std::move(right_page), m_widen_margins_amount);
		left_page->rotate(f_landscape ? -90.0 : 90.0);
		processed_pages.push_back(std::move(left_page));
	}
	verbose_output("Pages rearranged for booklet printing.");
}

void Liesel::Book::print_segment(uint32_t segment_number) {
	if (!pdf_document) throw std::runtime_error("PDF document not loaded.");
	if (output_pdf_path.empty()) throw std::runtime_error("No output PDF path specified.");
	if (m_effective_page_indices.empty()) throw std::runtime_error("No pages to print.");

	std::filesystem::path segment_output_path = output_pdf_path;
	if (m_segment_size < UINT32_MAX) {
		// One of multiple segments (a single, unsegmented PDF has m_segment_size == UINT32_MAX)
		// Calculate this segment's output path:
		// Replace the ".pdf" extension with ".NNN.pdf" where NNN is the segment number, zero-padded to 3 digits
		auto ext = segment_output_path.extension();
		std::string segment_suffix = std::to_string(segment_number + 1);
		while (segment_suffix.length() < 3) {
			segment_suffix = "0" + segment_suffix;
		}
		segment_output_path.replace_extension(segment_suffix + ext.string());
	}

	verbose_output("Printing segment " + std::to_string(segment_number + 1)
		+ " to output PDF: " + segment_output_path.string());
	_emit_progress(ProgressEvent::Info, segment_number, 0,
		"Printing segment " + std::to_string(segment_number + 1) + "...");

	_render_segment(segment_number);
	_maybe_reorder_pages();

	// This struct ensures that we automatically free the PDF document if an exception occurs
	struct HPDFDocDeleter {
		void operator()(HPDF_Doc doc) const noexcept {
			if (doc) HPDF_Free(doc);
		}
	};
	using HPDFDocUPtr = std::unique_ptr<std::remove_pointer<HPDF_Doc>::type, HPDFDocDeleter>;

	HaruErrorState haru_error_state{};

	HPDFDocUPtr doc(HPDF_New(nullptr, nullptr));
	if (!doc) throw std::runtime_error("Failed to create new PDF document.");
	haru_check(doc.get(), haru_error_state, HPDF_SetCompressionMode(doc.get(), HPDF_COMP_ALL), "HPDF_SetCompressionMode");

	for (const auto& page : processed_pages) {
		_check_cancelled(); // Throw if something requested cancellation
		auto img = page->_get_image_raw();
		auto width = img->columns();
		auto height = img->rows();
		Magick::Blob blob;
		img->magick("JPEG");
		img->write(&blob);

		HPDF_Page pdf_page = haru_require_ptr(doc.get(), haru_error_state, HPDF_AddPage(doc.get()), "HPDF_AddPage");

		HPDF_REAL effective_width = static_cast<HPDF_REAL>(m_dpi_density * width) / 72;
		HPDF_REAL effective_height = static_cast<HPDF_REAL>(m_dpi_density * height) / 72;

		if (m_rescale_size.has_value()) {
			effective_width = m_rescale_size.value().width.to_float() * 72;
			effective_height = m_rescale_size.value().height.to_float() * 72;
		}

		// Clamp to a minimum size of 10 by 10 points to avoid libharu issues
		// This is a totally arbitrary minimum, but should be small enough to not matter
		if (effective_width < 10.0f) effective_width = 10.0f;
		if (effective_height < 10.0f) effective_height = 10.0f;

		haru_check(doc.get(), haru_error_state, HPDF_Page_SetWidth(pdf_page, effective_width), "HPDF_Page_SetWidth");
		haru_check(doc.get(), haru_error_state, HPDF_Page_SetHeight(pdf_page, effective_height), "HPDF_Page_SetHeight");

		if (blob.length() == 0 || blob.data() == nullptr) throw std::runtime_error("Image blob is empty.");
		if (blob.length() > static_cast<size_t>(std::numeric_limits<HPDF_UINT>::max())) {
			throw std::runtime_error("Image blob is too large for libharu.");
		}

		HPDF_Image pdf_image = haru_require_ptr(
			doc.get(),
			haru_error_state,
			HPDF_LoadJpegImageFromMem(
				doc.get(),
				static_cast<const HPDF_BYTE*>(blob.data()),
				static_cast<HPDF_UINT>(blob.length())
			),
			"HPDF_LoadJpegImageFromMem"
		);
		haru_check(doc.get(), haru_error_state,
			HPDF_Page_DrawImage(pdf_page, pdf_image, 0, 0, effective_width, effective_height),
			"HPDF_Page_DrawImage"
		);
	}

	haru_check(doc.get(), haru_error_state, HPDF_SaveToFile(doc.get(), segment_output_path.string().c_str()), "HPDF_SaveToFile");
	verbose_output("Segment " + std::to_string(segment_number + 1) + " printed successfully.");
	_emit_progress(ProgressEvent::SegmentDone, segment_number, 0,
		"Segment " + std::to_string(segment_number + 1) + " printed successfully.");
}

void Liesel::Book::print() {
	if (!pdf_document) throw std::runtime_error("PDF document not loaded.");
	if (output_pdf_path.empty()) throw std::runtime_error("No output PDF path specified.");
	if (m_effective_page_indices.empty()) throw std::runtime_error("No pages to print.");
	if (m_segment_size == 0) throw std::invalid_argument("Segment size cannot be zero.");

	// Progress units: one per effective (source) page rendered
	m_progress_total_units = static_cast<uint32_t>(m_effective_page_indices.size());
	m_progress_completed_units = 0;

	_emit_progress(ProgressEvent::Info, 0, 0, "Starting print job...");

	size_t total_segments = static_cast<size_t>(std::ceil(static_cast<double>(m_effective_page_indices.size()) / m_segment_size));
	if (m_segment_size >= m_effective_page_indices.size()) total_segments = 1;

	for (uint32_t i = 0; i < total_segments; i++) {
		_check_cancelled(); // Throw if something requested cancellation
		print_segment(i);
		pages.clear();
		processed_pages.clear();
	}

	m_progress_completed_units = m_progress_total_units;
	_emit_progress(ProgressEvent::PrintDone, total_segments ? total_segments - 1 : 0, 0, "Done.");
}

void Liesel::Book::set_input_pdf_path(const std::string_view& path) {
	// Verify that the path is valid, the file exists, and is a PDF
	std::filesystem::path p(path);
	if (!std::filesystem::exists(p)) {
		throw std::invalid_argument("Input PDF file does not exist: " + std::string(path));
	}
	// Case-insensitive check for .pdf extension
	auto ext = p.extension().string();
	std::transform(ext.begin(), ext.end(), ext.begin(), ::tolower);
	if (ext != ".pdf") {
		throw std::invalid_argument("Input file is not a PDF: " + std::string(path));
	}

	input_pdf_path = p;
}

void Liesel::Book::set_output_pdf_path(const std::string_view& path) {
	// Verify either:
	// 0. If we're outputting multiple segmented PDFs, we need to verify write permissions on the directory
	// Otherwise:
	// 1. The file does not exist (will be created) AND we have write permissions in the directory
	// Or 2. The file exists AND we have write permissions to the file
	std::filesystem::path p(path);

	if (m_segment_size < UINT32_MAX) {
		// Segmented output, check directory write permissions
		auto dir = p;
		if (dir.has_filename()) {
			dir = dir.parent_path();
		}
		if (dir.empty()) {
			dir = std::filesystem::current_path();
		}
		if (access(dir.string().c_str(), W_OK) != 0) {
			throw std::invalid_argument("No write permission in output PDF directory: " + dir.string());
		}
		// Even in the case of segmented output, we still store the base output path
		// The segmented files will be created later with suffixes appended to this
		output_pdf_path = p;
		return;
	}

	if (std::filesystem::exists(p)) {	
		// File exists, check write permissions
		if (access(p.string().c_str(), W_OK) != 0) {
			throw std::invalid_argument("No write permission for output PDF file: " + std::string(path));
		}
	} else {
		// File does not exist, check write permissions in the directory
		auto dir = p.parent_path();
		if (dir.empty()) {
			dir = std::filesystem::current_path();
		}
		if (access(dir.string().c_str(), W_OK) != 0) {
			throw std::invalid_argument("No write permission in output PDF directory: " + dir.string());
		}
	}
	output_pdf_path = p;
}

void Liesel::Book::load_pdf() {
	verbose_output("Loading PDF document from: " + input_pdf_path.string());
	pdf_document.reset(poppler::document::load_from_file(input_pdf_path.string()));
	if (!pdf_document) {
		throw std::runtime_error("Failed to load PDF document: " + input_pdf_path.string());
	}
	verbose_output("PDF document loaded successfully. Number of pages: " + std::to_string(pdf_document->pages()));

	calculate_effective_page_indices();
}
