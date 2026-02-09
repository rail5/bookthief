/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#pragma once

#include <memory>
#include <cstdint>
#include <optional>
#include <filesystem>
#include <vector>
#include <functional>
#include <string>

#include <Magick++.h>
#include <poppler/cpp/poppler-global.h>
#include <poppler/cpp/poppler-document.h>
#include <poppler/cpp/poppler-page.h>
#include <poppler/cpp/poppler-image.h>
#include <poppler/cpp/poppler-page-renderer.h>

#include "PageDimensions.h"
#include "PageRangeList.h"
#include "CropPercentages.h"
#include "Page.h"

namespace Liesel {

class Cancelled final : public std::runtime_error {
	public:
		explicit Cancelled(const std::string& message) : std::runtime_error(message) {}
		explicit Cancelled(const char* message) : std::runtime_error(message ? message : "Cancelled") {}
};

class Book {
	private:
		std::filesystem::path input_pdf_path;
		std::filesystem::path output_pdf_path;
		std::unique_ptr<poppler::document> pdf_document;
		std::vector<std::unique_ptr<Page>> pages;
		std::vector<std::unique_ptr<Page>> processed_pages;
		std::unique_ptr<Magick::Image> settings_preview;

		bool f_verbose = false;
		bool f_greyscale = false;
		bool f_divide = false;
		bool f_landscape = false; // Default to portrait for duplex printing

		bool f_booklet = true; // Default to booklet printing unless -N is specified

		bool f_previewing = false; // If true, generate a new preview image every time settings are changed
		uint32_t m_preview_page = 0; // Page index to use for preview generation
			// Unless f_booklet == false, the preview will also include m_preview_page + 1
			// Therefore unless f_booklet == false, setting this to the last page is invalid
		
		uint32_t m_dpi_density = 100;
		std::optional<uint8_t> m_threshold_level = std::nullopt;
		uint32_t m_segment_size = UINT32_MAX; // Default to no segmentation

		uint32_t m_widen_margins_amount = 0; // Default to no widening

		std::optional<PageDimensionPair> m_rescale_size = std::nullopt;
		std::optional<PageRangeList> m_page_ranges = std::nullopt;
		std::vector<uint32_t> m_effective_page_indices;
		CropPercentages m_crop_percentages;

		void verbose_output(const std::string_view& message) const;
		void calculate_effective_page_indices();
		void _render_segment(uint32_t segment_number);
		void _maybe_reorder_pages();
		void print_segment(uint32_t segment_number);

		void _generate_settings_preview();
	public:
		Book();

		enum class ProgressEvent : uint32_t {
			Info = 0,
			RenderPage = 1,
			SegmentDone = 2,
			PrintDone = 3
		}; // Probably to be extended later

		struct ProgressInfo {
			ProgressEvent event = ProgressEvent::Info;
			uint32_t segment_index = 0;
			uint32_t page_index = 0; // 0-indexed within segment
			uint32_t percent = 0; // 0-100
			std::string message;
		};

		using ProgressCallback = std::function<void(const ProgressInfo& info)>;
		using CancelCallback = std::function<bool(void)>; // return true to cancel
	private:
		ProgressCallback m_progress_cb;
		CancelCallback m_cancel_cb;

		uint32_t m_progress_total_units = 0;
		uint32_t m_progress_completed_units = 0;

		uint32_t _progress_percent() const;
		void _emit_progress(ProgressEvent event, uint32_t segment_index, uint32_t page_index, const std::string& message);
		void _check_cancelled() const;
	public:

		void set_input_pdf_path(const std::string_view& path);
		void set_output_pdf_path(const std::string_view& path);

		void set_verbose(bool verbose) { f_verbose = verbose; }
		void set_greyscale(bool greyscale) { f_greyscale = greyscale; _generate_settings_preview(); }
		void set_divide(bool divide) { f_divide = divide; _generate_settings_preview(); }
		void set_booklet(bool booklet) { f_booklet = booklet; _generate_settings_preview(); }
		void set_landscape(bool landscape) { f_landscape = landscape; }
		void set_dpi_density(uint32_t dpi) { m_dpi_density = dpi; _generate_settings_preview(); }
		void set_threshold_level(std::optional<uint8_t> level) { m_threshold_level = level; _generate_settings_preview(); }
		void set_segment_size(uint32_t size) {
			if (size == 0) throw std::invalid_argument("Segment size cannot be zero.");
			m_segment_size = size;
		}
		void set_widen_margins_amount(uint32_t amount) { m_widen_margins_amount = amount; _generate_settings_preview(); }
		void set_rescale_size(std::optional<PageDimensionPair> size) { m_rescale_size = size; _generate_settings_preview(); }
		void set_page_ranges(std::optional<PageRangeList> ranges) { m_page_ranges = ranges; }
		void set_crop_percentages(const CropPercentages& crop) { m_crop_percentages = crop; _generate_settings_preview(); }

		bool verbose() const { return f_verbose; }
		bool greyscale() const { return f_greyscale; }
		bool divide() const { return f_divide; }
		bool booklet() const { return f_booklet; }
		bool landscape() const { return f_landscape; }
		uint32_t dpi_density() const { return m_dpi_density; }
		std::optional<uint8_t> threshold_level() const { return m_threshold_level; }
		uint32_t segment_size() const { return m_segment_size; }
		uint32_t widen_margins_amount() const { return m_widen_margins_amount; }
		std::optional<PageDimensionPair> rescale_size() const { return m_rescale_size; }
		std::optional<PageRangeList> page_ranges() const { return m_page_ranges; }
		CropPercentages crop_percentages() const { return m_crop_percentages; }

		void load_pdf();
		void print();

		// Only used by the GUI.
		void set_previewing(bool previewing) { f_previewing = previewing; _generate_settings_preview(); }
		void set_preview_page(uint32_t page_index);
		Magick::Image* get_preview_image() { return settings_preview.get(); }

		void set_progress_callback(ProgressCallback cb) { m_progress_cb = std::move(cb); }
		void set_cancel_callback(CancelCallback cb) { m_cancel_cb = std::move(cb); }
};

} // namespace Liesel
