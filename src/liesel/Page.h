/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#pragma once

#include <memory>

#include <Magick++.h>
#include <poppler/cpp/poppler-global.h>
#include <poppler/cpp/poppler-document.h>
#include <poppler/cpp/poppler-page.h>
#include <poppler/cpp/poppler-image.h>
#include <poppler/cpp/poppler-page-renderer.h>

#include "CropPercentages.h"

namespace Liesel {

class Page {
	private:
		std::unique_ptr<Magick::Image> image;

	public:
		Page() = default;

		// Copy constructor
		Page(const Page& other) {
			if (other.image) this->image = std::make_unique<Magick::Image>(*other.image);
		}

		void load(poppler::document* document, uint32_t page_index, uint32_t dpi_density);
		void set_greyscale();
		void set_threshold(uint8_t level);
		std::unique_ptr<Page> divide();
		void crop(const CropPercentages& crop_percentages);

		uint32_t columns() const {
			if (!image) throw std::runtime_error("No image loaded.");
			return image->columns();
		}
		uint32_t rows() const {
			if (!image) throw std::runtime_error("No image loaded.");
			return image->rows();
		}

		void blank(uint32_t width, uint32_t height);

		void _set_image_raw(std::unique_ptr<Magick::Image> img) { image = std::move(img); }
		std::unique_ptr<Magick::Image> _get_image_raw() { return std::move(image); }
};

} // namespace Liesel
