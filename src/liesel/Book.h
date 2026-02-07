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

#include <xgetopt.h>

#include <Magick++.h>
#include <poppler/cpp/poppler-global.h>
#include <poppler/cpp/poppler-document.h>
#include <poppler/cpp/poppler-page.h>
#include <poppler/cpp/poppler-image.h>
#include <poppler/cpp/poppler-page-renderer.h>
#include <hpdf.h>

#include "PageDimensions.h"
#include "PageRangeList.h"
#include "CropPercentages.h"
#include "Page.h"

namespace Liesel {

class Book {
	private:
		std::filesystem::path input_pdf_path;
		std::filesystem::path output_pdf_path;
		std::unique_ptr<poppler::document> pdf_document;
		std::vector<std::unique_ptr<Page>> pages;
		std::vector<std::unique_ptr<Page>> processed_pages;

		bool f_verbose = false;
		bool f_greyscale = false;
		bool f_divide = false;
		bool f_landscape = false; // Default to portrait for duplex printing
		
		uint32_t m_dpi_density = 100;
		std::optional<uint8_t> m_threshold_level = std::nullopt;
		uint32_t m_segment_size = UINT32_MAX; // Default to no segmentation

		uint32_t m_widen_margins_amount = 0; // Default to no widening
		/// m_autowiden_max == 0 means no maximum. == nullopt means disabled.
		std::optional<uint32_t> m_autowiden_max = std::nullopt;

		std::optional<PageDimensionPair> m_rescale_size = std::nullopt;
		std::optional<PageRangeList> m_page_ranges = std::nullopt;
		std::vector<uint32_t> m_effective_page_indices;
		CropPercentages m_crop_percentages;

		void verbose_output(const std::string_view& message) const;
		void calculate_effective_page_indices();
		void print_segment(uint32_t segment_number);
	public:
		Book();

		void set_input_pdf_path(const std::string_view& path);
		void set_output_pdf_path(const std::string_view& path);

		void set_verbose(bool verbose) { f_verbose = verbose; }
		void set_greyscale(bool greyscale) { f_greyscale = greyscale; }
		void set_divide(bool divide) { f_divide = divide; }
		void set_landscape(bool landscape) { f_landscape = landscape; }
		void set_dpi_density(uint32_t dpi) { m_dpi_density = dpi; }
		void set_threshold_level(uint8_t level) { m_threshold_level = level; }
		void set_segment_size(uint32_t size) {
			if (size == 0) throw std::invalid_argument("Segment size cannot be zero.");
			m_segment_size = size;
		}
		void set_widen_margins_amount(uint32_t amount) { m_widen_margins_amount = amount; }
		void set_autowiden_max(std::optional<uint32_t> max) { m_autowiden_max = max; }
		void set_rescale_size(const PageDimensionPair& size) { m_rescale_size = size; }
		void set_page_ranges(const PageRangeList& ranges) { m_page_ranges = ranges; }
		void set_crop_percentages(const CropPercentages& crop) { m_crop_percentages = crop; }

		bool verbose() const { return f_verbose; }
		bool greyscale() const { return f_greyscale; }
		bool divide() const { return f_divide; }
		bool landscape() const { return f_landscape; }
		uint32_t dpi_density() const { return m_dpi_density; }
		std::optional<uint8_t> threshold_level() const { return m_threshold_level; }
		uint32_t segment_size() const { return m_segment_size; }
		uint32_t widen_margins_amount() const { return m_widen_margins_amount; }
		std::optional<uint32_t> autowiden_max() const { return m_autowiden_max; }
		std::optional<PageDimensionPair> rescale_size() const { return m_rescale_size; }
		std::optional<PageRangeList> page_ranges() const { return m_page_ranges; }
		CropPercentages crop_percentages() const { return m_crop_percentages; }

		void configure_from_CLI_options(const XGetOpt::OptionSequence& options);

		void load_pdf();
};

} // namespace Liesel
