/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#pragma once

#include <cstdint>
#include <string_view>
#include <string>
#include <stdexcept>

namespace Liesel {

/**
 * @struct PageDimension
 * @brief 16-bit fixed-point representation of a single page dimension in inches (e.g., 8.5 in 8.5x11)
 *
 * Using two unsigned 8-bit integers to represent the whole and decimal parts of the dimension.
 */
struct PageDimension {
	uint8_t whole = 0; // In '8.5', this is '8'
	uint8_t decimal = 0; // In '8.5', this is '5'

	PageDimension() = default;

	/// Conversion to fixed-point uint16_t
	operator uint16_t() const {
		return static_cast<uint16_t>(whole) << 8 | static_cast<uint16_t>(decimal);
	}

	/// Conversion from fixed-point uint16_t
	PageDimension& operator=(const uint16_t& value) {
		whole = static_cast<uint8_t>(value >> 8);
		decimal = static_cast<uint8_t>(value & 0xFF);
		return *this;
	}

	/**
	 * @brief Construction from a string representation (e.g., "8.5")
	 *
	 * This will throw an exception if the string is not a valid number.
	 * 
	 * @param str String representation of the dimension
	 */
	explicit PageDimension(const std::string_view& str) {
		auto dot_pos = str.find('.');
		if (dot_pos == std::string_view::npos) {
			whole = static_cast<uint8_t>(std::stoi(std::string(str)));
			decimal = 0;
		} else {
			whole = static_cast<uint8_t>(std::stoi(std::string(str.substr(0, dot_pos))));
			decimal = static_cast<uint8_t>(std::stoi(std::string(str.substr(dot_pos + 1))));
		}
	}
};

struct PageDimensionPair {
	PageDimension width;
	PageDimension height;

	/// Construction from a string representation (e.g., "8.5x11")
	explicit PageDimensionPair(const std::string_view& str) {
		auto x_pos = str.find('x');
		if (x_pos == std::string_view::npos) {
			throw std::invalid_argument("Invalid PageDimensionPair string: " + std::string(str));
		}
		width = PageDimension(str.substr(0, x_pos));
		height = PageDimension(str.substr(x_pos + 1));
	}
};

} // namespace Liesel
