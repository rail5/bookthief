/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#include "Page.h"

#include <stdexcept>

void Liesel::Page::load(poppler::document* document, uint32_t page_index, uint32_t dpi_density) {
	if (!document) throw std::invalid_argument("Invalid PDF document pointer.");

	{
		poppler::page_renderer renderer;

		std::unique_ptr<poppler::page> page(document->create_page(page_index));
		if (!page) throw std::runtime_error("Failed to create page " + std::to_string(page_index + 1) + ".");

		poppler::image page_image = renderer.render_page(page.get(), dpi_density, dpi_density);
		if (!page_image.is_valid()) throw std::runtime_error("Failed to render page "
			+ std::to_string(page_index + 1) + ".");
		
		// Kind of a hack: copy raw image data to a Magick::Image
		this->image = std::make_unique<Magick::Image>(
			page_image.width(), page_image.height(),
			"BGRA", Magick::StorageType::CharPixel,
			page_image.data());

		// Destructor is called for page_image here
		// Unfortunately, for a brief moment, two copies of the image data exist in memory
		// TODO(@rail5): Can we just transfer ownership of those bytes to Magick::Image directly?
	}

	this->image->quality(dpi_density);
	this->image->resolutionUnits(Magick::PixelsPerInchResolution);
	this->image->density(Magick::Geometry(dpi_density, dpi_density));
	this->image->matte(false); // No alpha channel
}

void Liesel::Page::set_greyscale() {
	if (this->image) this->image->type(Magick::GrayscaleType);
}

void Liesel::Page::set_threshold(uint8_t level) {
	if (this->image) this->image->threshold(static_cast<double>(level));
}

/**
 * @brief Divides the current page into two halves and returns the left half as a new Page object. This Page becomes the right half.
 * 
 * @return std::unique_ptr<Liesel::Page> The left half of the divided page.
 */
std::unique_ptr<Liesel::Page> Liesel::Page::divide() {
	if (!this->image) throw std::runtime_error("No image loaded to divide.");

	uint32_t half_width = this->image->columns() / 2;
	uint32_t height = this->image->rows();

	Magick::Geometry left_half(half_width, height, 0, 0);
	Magick::Geometry right_half(half_width, height, half_width, 0);

	auto left_image = std::make_unique<Magick::Image>(*this->image);
	left_image->crop(left_half);
	this->image->crop(right_half);

	auto left_page = std::make_unique<Liesel::Page>();
	left_page->_set_image_raw(std::move(left_image));

	return left_page;
}

void Liesel::Page::crop(const CropPercentages& crop_percentages) {
	if (!this->image) throw std::runtime_error("No image loaded to crop.");

	if (crop_percentages.is_empty()) return;

	uint32_t left_crop =
		((this->image->columns() / 100) * crop_percentages.left()) / 2;
	uint32_t right_crop =
		((this->image->columns() / 100) * crop_percentages.right()) / 2;
	uint32_t top_crop =
		((this->image->rows() / 100) * crop_percentages.top()) / 2;
	uint32_t bottom_crop =
		((this->image->rows() / 100) * crop_percentages.bottom()) / 2;
	
	Magick::Geometry left_crop_geom(
		this->image->columns() - left_crop,
		this->image->rows(),
		left_crop, 0);
	this->image->crop(left_crop_geom);

	Magick::Geometry right_crop_geom(
		this->image->columns() - right_crop,
		this->image->rows(),
		0, 0);
	this->image->crop(right_crop_geom);

	Magick::Geometry top_crop_geom(
		this->image->columns(),
		this->image->rows() - top_crop,
		0, top_crop);
	this->image->crop(top_crop_geom);
	
	Magick::Geometry bottom_crop_geom(
		this->image->columns(),
		this->image->rows() - bottom_crop,
		0, 0);
	this->image->crop(bottom_crop_geom);
}
