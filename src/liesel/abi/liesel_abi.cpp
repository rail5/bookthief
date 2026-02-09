/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#include "liesel_abi.h"

#include <exception>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <string>

#include "../version.h"

#include "../Book.h"
#include "../CropPercentages.h"
#include "../PageDimensions.h"
#include "../PageRangeList.h"

#include "../CLIConfig.h"

namespace {

static inline bool is_null_or_empty(const char* s) {
	return (s == nullptr) || (s[0] == '\0');
}

static inline LieselStatus map_exception_to_status(const std::exception& e) {
	// Keep mapping coarse; callers can fetch text via *_last_error().
	if (dynamic_cast<const Liesel::Cancelled*>(&e) != nullptr) return LIESEL_E_CANCELLED;
	if (dynamic_cast<const std::invalid_argument*>(&e) != nullptr) return LIESEL_E_INVALID_ARG;
	if (dynamic_cast<const std::out_of_range*>(&e) != nullptr) return LIESEL_E_INVALID_ARG;
	return LIESEL_E_RUNTIME;
}

} // namespace

struct LieselHandle {
	std::string last_error;
};

struct LieselBookHandle {
	LieselHandle* owner = nullptr;
	std::unique_ptr<Liesel::Book> book;
	std::string last_error;

	LieselProgressCallback progress_cb = nullptr;
	void* progress_ud = nullptr;
	LieselCancelCallback cancel_cb = nullptr;
	void* cancel_ud = nullptr;
};

static inline void set_error(LieselHandle* h, const std::string& message) {
	if (h) h->last_error = message;
}

static inline void set_error(LieselBookHandle* b, const std::string& message) {
	if (b) b->last_error = message;
}

static inline LieselStatus fail_invalid_arg(LieselBookHandle* b, const char* message) {
	set_error(b, message ? message : "Invalid argument");
	return LIESEL_E_INVALID_ARG;
}

static inline LieselStatus alloc_cstr(LieselBookHandle* b, const std::string& s, char** out) {
	if (!out) return fail_invalid_arg(b, "Output string pointer cannot be null");
	const size_t len = s.size();
	char* cstr = static_cast<char*>(std::malloc(len + 1));
	if (!cstr) throw std::bad_alloc();
	std::memcpy(cstr, s.c_str(), len);
	cstr[len] = '\0';
	*out = cstr;
	return LIESEL_OK;
}

extern "C" {

void liesel_free(void* p) {
	std::free(p);
}

LieselHandle* liesel_create(void) {
	try {
		return new LieselHandle{};
	} catch (...) {
		return nullptr;
	}
}

void liesel_destroy(LieselHandle* h) {
	delete h;
}

const char* liesel_version(void) {
	return VERSION;
}

int liesel_major_version(void) {
	return MAJOR_VERSION;
}

int liesel_minor_version(void) {
	return MINOR_VERSION;
}

int liesel_patch_version(void) {
	return PATCH_VERSION;
}

const char* liesel_last_error(LieselHandle* h) {
	if (!h) return "";
	return h->last_error.c_str();
}

LieselBookHandle* liesel_book_create(LieselHandle* h) {
	try {
		auto* b = new LieselBookHandle{};
		b->owner = h;
		b->book = std::make_unique<Liesel::Book>();
		return b;
	} catch (const std::exception& e) {
		set_error(h, e.what());
		return nullptr;
	} catch (...) {
		set_error(h, "Unknown error");
		return nullptr;
	}
}

void liesel_book_destroy(LieselBookHandle* b) {
	delete b;
}

const char* liesel_book_last_error(LieselBookHandle* b) {
	if (!b) return "";
	return b->last_error.c_str();
}

LieselStatus liesel_book_export_settings_as_commandstring(LieselBookHandle* b, char** out_string_utf8) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	if (!out_string_utf8) return fail_invalid_arg(b, "Output string pointer cannot be null");
	try {
		std::string options = GetCLIOptionStringFromBook(b->book.get());
		return alloc_cstr(b, options, out_string_utf8);
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_import_settings_from_commandstring(LieselBookHandle* b, const char* options_str_utf8) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	if (is_null_or_empty(options_str_utf8)) return fail_invalid_arg(b, "Options string cannot be null or empty");
	try {
		ConfigureBookFromCLIOptionsString(b->book.get(), std::string(options_str_utf8));
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_set_input_pdf_path(LieselBookHandle* b, const char* path_utf8) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	if (is_null_or_empty(path_utf8)) return fail_invalid_arg(b, "Input PDF path cannot be empty");
	try {
		b->book->set_input_pdf_path(path_utf8);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_set_output_pdf_path(LieselBookHandle* b, const char* path_utf8) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	if (is_null_or_empty(path_utf8)) return fail_invalid_arg(b, "Output PDF path cannot be empty");
	try {
		b->book->set_output_pdf_path(path_utf8);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_set_verbose(LieselBookHandle* b, int enabled) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	b->book->set_verbose(enabled != 0);
	return LIESEL_OK;
}

LieselStatus liesel_book_set_greyscale(LieselBookHandle* b, int enabled) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	b->book->set_greyscale(enabled != 0);
	return LIESEL_OK;
}

LieselStatus liesel_book_set_divide(LieselBookHandle* b, int enabled) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	b->book->set_divide(enabled != 0);
	return LIESEL_OK;
}

LieselStatus liesel_book_set_booklet(LieselBookHandle* b, int enabled) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	b->book->set_booklet(enabled != 0);
	return LIESEL_OK;
}

LieselStatus liesel_book_set_landscape(LieselBookHandle* b, int enabled) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	b->book->set_landscape(enabled != 0);
	return LIESEL_OK;
}

LieselStatus liesel_book_set_dpi_density(LieselBookHandle* b, uint32_t dpi) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	b->book->set_dpi_density(dpi);
	return LIESEL_OK;
}

LieselStatus liesel_book_set_threshold_level(LieselBookHandle* b, uint8_t level_0_100) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		b->book->set_threshold_level(level_0_100);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_clear_threshold_level(LieselBookHandle* b) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		b->book->set_threshold_level(std::nullopt);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_set_segment_size(LieselBookHandle* b, uint32_t pages_per_segment) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		b->book->set_segment_size(pages_per_segment);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_clear_segment_size(LieselBookHandle* b) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		b->book->set_segment_size(UINT32_MAX);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_set_widen_margins_amount(LieselBookHandle* b, uint32_t amount) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	b->book->set_widen_margins_amount(amount);
	return LIESEL_OK;
}

LieselStatus liesel_book_set_rescale_size(LieselBookHandle* b, const char* size_utf8) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	if (is_null_or_empty(size_utf8)) return fail_invalid_arg(b, "Rescale size cannot be empty");
	try {
		Liesel::PageDimensionPair size(size_utf8);
		b->book->set_rescale_size(size);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_clear_rescale_size(LieselBookHandle* b) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		b->book->set_rescale_size(std::nullopt);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_set_page_ranges(LieselBookHandle* b, const char* range_utf8) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	if (is_null_or_empty(range_utf8)) return fail_invalid_arg(b, "Page range cannot be empty");
	try {
		Liesel::PageRangeList ranges(range_utf8);
		b->book->set_page_ranges(ranges);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_clear_page_ranges(LieselBookHandle* b) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		b->book->set_page_ranges(std::nullopt);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_set_crop_percentages(LieselBookHandle* b, const char* crop_utf8) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	if (is_null_or_empty(crop_utf8)) return fail_invalid_arg(b, "Crop percentages cannot be empty");
	try {
		Liesel::CropPercentages crop;
		crop.set_from_string(crop_utf8);
		b->book->set_crop_percentages(crop);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_set_crop_percentages_lrbt(LieselBookHandle* b, uint8_t l, uint8_t r, uint8_t t, uint8_t bt) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		Liesel::CropPercentages crop;
		crop.set_left(l);
		crop.set_right(r);
		crop.set_top(t);
		crop.set_bottom(bt);
		b->book->set_crop_percentages(crop);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_get_verbose(LieselBookHandle* b, int* out_enabled) {
	if (!out_enabled) return LIESEL_E_INVALID_ARG;
	*out_enabled = 0;
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		*out_enabled = b->book->verbose() ? 1 : 0;
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_get_greyscale(LieselBookHandle* b, int* out_enabled) {
	if (!out_enabled) return LIESEL_E_INVALID_ARG;
	*out_enabled = 0;
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		*out_enabled = b->book->greyscale() ? 1 : 0;
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_get_divide(LieselBookHandle* b, int* out_enabled) {
	if (!out_enabled) return LIESEL_E_INVALID_ARG;
	*out_enabled = 0;
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		*out_enabled = b->book->divide() ? 1 : 0;
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_get_booklet(LieselBookHandle* b, int* out_enabled) {
	if (!out_enabled) return LIESEL_E_INVALID_ARG;
	*out_enabled = 0;
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		*out_enabled = b->book->booklet() ? 1 : 0;
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_get_landscape(LieselBookHandle* b, int* out_enabled) {
	if (!out_enabled) return LIESEL_E_INVALID_ARG;
	*out_enabled = 0;
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		*out_enabled = b->book->landscape() ? 1 : 0;
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_get_dpi_density(LieselBookHandle* b, uint32_t* out_dpi) {
	if (!out_dpi) return LIESEL_E_INVALID_ARG;
	*out_dpi = 0;
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		*out_dpi = b->book->dpi_density();
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_get_threshold_level(LieselBookHandle* b, int* out_is_set, uint8_t* out_level_0_100) {
	if (!out_is_set || !out_level_0_100) return LIESEL_E_INVALID_ARG;
	*out_is_set = 0;
	*out_level_0_100 = 0;
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		auto opt = b->book->threshold_level();
		if (!opt.has_value()) return LIESEL_OK;
		*out_is_set = 1;
		*out_level_0_100 = opt.value();
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_get_segment_size(LieselBookHandle* b, int* out_is_set, uint32_t* out_pages_per_segment) {
	if (!out_is_set || !out_pages_per_segment) return LIESEL_E_INVALID_ARG;
	*out_is_set = 0;
	*out_pages_per_segment = 0;
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		const uint32_t seg = b->book->segment_size();
		if (seg == UINT32_MAX) return LIESEL_OK;
		*out_is_set = 1;
		*out_pages_per_segment = seg;
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_get_widen_margins_amount(LieselBookHandle* b, uint32_t* out_amount) {
	if (!out_amount) return LIESEL_E_INVALID_ARG;
	*out_amount = 0;
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		*out_amount = b->book->widen_margins_amount();
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_get_rescale_size(LieselBookHandle* b, int* out_is_set, char** out_size_utf8) {
	if (!out_is_set || !out_size_utf8) return LIESEL_E_INVALID_ARG;
	*out_is_set = 0;
	*out_size_utf8 = nullptr;
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		auto opt = b->book->rescale_size();
		if (!opt.has_value()) return LIESEL_OK;
		*out_is_set = 1;
		return alloc_cstr(b, opt->to_string(), out_size_utf8);
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_get_page_ranges(LieselBookHandle* b, int* out_is_set, char** out_ranges_utf8) {
	if (!out_is_set || !out_ranges_utf8) return LIESEL_E_INVALID_ARG;
	*out_is_set = 0;
	*out_ranges_utf8 = nullptr;
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		auto opt = b->book->page_ranges();
		if (!opt.has_value()) return LIESEL_OK;
		*out_is_set = 1;
		return alloc_cstr(b, opt->to_string(), out_ranges_utf8);
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_get_crop_percentages_lrbt(LieselBookHandle* b, int* out_enabled, uint8_t* out_l, uint8_t* out_r, uint8_t* out_t, uint8_t* out_bt) {
	if (!out_enabled || !out_l || !out_r || !out_t || !out_bt) return LIESEL_E_INVALID_ARG;
	*out_enabled = 0;
	*out_l = 0;
	*out_r = 0;
	*out_t = 0;
	*out_bt = 0;
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		const auto crop = b->book->crop_percentages();
		*out_l = crop.left();
		*out_r = crop.right();
		*out_t = crop.top();
		*out_bt = crop.bottom();
		*out_enabled = crop.is_empty() ? 0 : 1;
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_get_input_pdf_path(LieselBookHandle* b, int* out_is_set, char** out_path_utf8) {
	if (!out_is_set || !out_path_utf8) return LIESEL_E_INVALID_ARG;
	*out_is_set = 0;
	*out_path_utf8 = nullptr;
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		if (!b->book->has_input_pdf_path()) return LIESEL_OK;
		*out_is_set = 1;
		return alloc_cstr(b, b->book->get_input_pdf_path(), out_path_utf8);
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_get_output_pdf_path(LieselBookHandle* b, int* out_is_set, char** out_path_utf8) {
	if (!out_is_set || !out_path_utf8) return LIESEL_E_INVALID_ARG;
	*out_is_set = 0;
	*out_path_utf8 = nullptr;
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		if (!b->book->has_output_pdf_path()) return LIESEL_OK;
		*out_is_set = 1;
		return alloc_cstr(b, b->book->get_output_pdf_path(), out_path_utf8);
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_load_pdf(LieselBookHandle* b) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		b->book->load_pdf();
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}
LieselStatus liesel_book_get_pdf_page_count(LieselBookHandle* b, uint32_t* out_page_count) {
	if (!out_page_count) return LIESEL_E_INVALID_ARG;
	*out_page_count = 0;
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		*out_page_count = b->book->pdf_page_count();
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_set_previewing(LieselBookHandle* b, int enabled) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		b->book->set_previewing(enabled != 0);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_set_preview_page(LieselBookHandle* b, uint32_t page_index) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		b->book->set_preview_page(page_index);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_get_preview_jpeg(LieselBookHandle* b, uint8_t** out_bytes, size_t* out_len) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	if (!out_bytes || !out_len) return fail_invalid_arg(b, "Output pointers cannot be NULL");

	*out_bytes = nullptr;
	*out_len = 0;

	try {
		Magick::Image* img = b->book->get_preview_image();
		if (!img) return LIESEL_OK; // No preview available yet

		// Encode to JPEG. Use a copy to avoid mutating the internal preview object.
		Magick::Image tmp(*img);
		Magick::Blob blob;
		tmp.magick("JPEG");
		tmp.write(&blob);

		const size_t len = static_cast<size_t>(blob.length());
		if (len == 0 || blob.data() == nullptr) return LIESEL_OK;

		auto* mem = static_cast<uint8_t*>(std::malloc(len));
		if (!mem) throw std::bad_alloc();
		std::memcpy(mem, blob.data(), len);

		*out_bytes = mem;
		*out_len = len;
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_print(
	LieselBookHandle* b,
	LieselProgressCallback progress_cb,
	void* progress_userdata,
	LieselCancelCallback cancel_cb,
	void* cancel_userdata
) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;

	b->progress_cb = progress_cb;
	b->progress_ud = progress_userdata;
	b->cancel_cb = cancel_cb;
	b->cancel_ud = cancel_userdata;

	// Bridge into C++ callbacks for the duration of this call.
	b->book->set_cancel_callback([b]() ->bool {
		if (!b->cancel_cb) return false;
		return b->cancel_cb(b->cancel_ud) != 0;
	});

	b->book->set_progress_callback([b](const Liesel::Book::ProgressInfo& info) {
		if (!b->progress_cb) return;
		// Map ProgressEvent -> ABI enum (same values)
		const auto ev = static_cast<LieselProgressEvent>(info.event);
		b->progress_cb(
			b->progress_ud,
			ev,
			info.segment_index,
			info.page_index,
			info.percent,
			info.message.c_str()
		);
	});
	
	try {
		b->book->print();
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

} // extern "C"
