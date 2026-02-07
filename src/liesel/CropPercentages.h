/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#pragma once

#include <cstdint>
#include <stdexcept>

namespace Liesel {

class CropPercentages {
	private:
		uint8_t m_left = 0;
		uint8_t m_right = 0;
		uint8_t m_top = 0;
		uint8_t m_bottom = 0;
		
		bool validate_percentage(uint8_t value) const {
			return value <= 100;
		}
	public:
		uint8_t left() const { return m_left; }
		uint8_t right() const { return m_right; }
		uint8_t top() const { return m_top; }
		uint8_t bottom() const { return m_bottom; }

		CropPercentages() = default;

		void set_left(uint8_t value) {
			if (!validate_percentage(value)) {
				throw std::out_of_range("Left crop percentage must be between 0 and 100");
			}
			m_left = value;
		}

		void set_right(uint8_t value) {
			if (!validate_percentage(value)) {
				throw std::out_of_range("Right crop percentage must be between 0 and 100");
			}
			m_right = value;
		}

		void set_top(uint8_t value) {
			if (!validate_percentage(value)) {
				throw std::out_of_range("Top crop percentage must be between 0 and 100");
			}
			m_top = value;
		}

		void set_bottom(uint8_t value) {
			if (!validate_percentage(value)) {
				throw std::out_of_range("Bottom crop percentage must be between 0 and 100");
			}
			m_bottom = value;
		}

		bool is_empty() const {
			return m_left == 0 && m_right == 0 && m_top == 0 && m_bottom == 0;
		}

		/// @brief Set crop percentages from a string "L,R,T,B"
		void set_from_string(const std::string_view& str) {
			size_t first_comma = str.find(',');
			size_t second_comma = str.find(',', first_comma + 1);
			size_t third_comma = str.find(',', second_comma + 1);

			if (first_comma == std::string_view::npos ||
				second_comma == std::string_view::npos ||
				third_comma == std::string_view::npos) {
				throw std::invalid_argument("Invalid crop percentages format. Expected L,R,T,B");
			}

			try {
				uint8_t left = static_cast<uint8_t>(std::stoul(std::string(str.substr(0, first_comma))));
				uint8_t right = static_cast<uint8_t>(std::stoul(std::string(str.substr(first_comma + 1, second_comma - first_comma - 1))));
				uint8_t top = static_cast<uint8_t>(std::stoul(std::string(str.substr(second_comma + 1, third_comma - second_comma - 1))));
				uint8_t bottom = static_cast<uint8_t>(std::stoul(std::string(str.substr(third_comma + 1))));

				set_left(left);
				set_right(right);
				set_top(top);
				set_bottom(bottom);
			} catch (const std::exception& e) {
				throw std::invalid_argument("Invalid crop percentages values. Must be integers between 0 and 100.");
			}
		}
};

} // namespace Liesel
