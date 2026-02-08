/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#pragma once

#include <cstdint>
#include <string_view>
#include <string>
#include <stdexcept>
#include <unordered_map>

namespace Liesel {

/**
 * @struct PageDimension
 * @brief 16-bit fixed-point representation of a single page dimension in inches (e.g., 8.5 in 8.5x11)
 *
 * Using two unsigned 8-bit integers to represent the whole and decimal parts of the dimension.
 */
struct PageDimension {
	uint8_t whole = 0; // In '8.5', this is '8'
	uint8_t decimal = 0; // In '8.5', this is '50'

	PageDimension() = default;

	bool operator==(const PageDimension& other) const {
		return (whole == other.whole) && (decimal == other.decimal);
	}

	bool operator!=(const PageDimension& other) const {
		return !(*this == other);
	}

	std::string to_string() const {
		return std::to_string(whole) + "." + (decimal < 10 ? "0" : "") + std::to_string(decimal);
	}

	explicit operator std::string() const {
		return to_string();
	}

	/// Conversion to fixed-point uint16_t
	operator uint16_t() const {
		return static_cast<uint16_t>(whole) << 8 | static_cast<uint16_t>(decimal);
	}

	/// Conversion to 32-bit float
	float to_float() const {
		return static_cast<float>(whole) + (static_cast<float>(decimal) / 100.0f);
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
			// '.5' should become '50', '.25' should become '25', '.256' should become '25'
			// Truncate to two decimal places
			std::string decimal_str = std::string(str.substr(dot_pos + 1));
			if (decimal_str.length() == 1) {
				decimal_str += "0";
			} else if (decimal_str.length() > 2) {
				decimal_str = decimal_str.substr(0, 2);
			}
			decimal = static_cast<uint8_t>(std::stoi(decimal_str));
		}
	}
};

struct PageDimensionPair {
	PageDimension width;
	PageDimension height;

	inline static const std::unordered_map<std::string_view, std::pair<PageDimension, PageDimension>> named_sizes = {
		{"us-letter", {PageDimension("8.5"), PageDimension("11")}},
		{"us-legal", {PageDimension("8.5"), PageDimension("14")}},
		{"a3", {PageDimension("11.69"), PageDimension("16.54")}},
		{"a4", {PageDimension("8.27"), PageDimension("11.69")}},
		{"a5", {PageDimension("5.83"), PageDimension("8.27")}}
	};

	/// Construction from a string representation (e.g., "8.5x11")
	explicit PageDimensionPair(const std::string_view& str) {
		// First: Check if the string matches a named size
		auto it = named_sizes.find(str);
		if (it != named_sizes.end()) {
			width = it->second.first;
			height = it->second.second;
			return;
		}

		auto x_pos = str.find('x');
		if (x_pos == std::string_view::npos) {
			throw std::invalid_argument("Invalid PageDimensionPair string: " + std::string(str));
		}
		width = PageDimension(str.substr(0, x_pos));
		height = PageDimension(str.substr(x_pos + 1));
	}

	std::string to_string() const {
		// If the size matches a named size, return that name
		for (const auto& [name, size] : named_sizes) {
			if (size.first == width && size.second == height) {
				return std::string(name);
			}
		}

		// Otherwise, return the "WxH" format
		return width.to_string() + "x" + height.to_string();
	}

	explicit operator std::string() const {
		return to_string();
	}
};

} // namespace Liesel
