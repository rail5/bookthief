/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#pragma once

#include <cstdint>
#include <optional>
#include <string_view>
#include <stdexcept>
#include <vector>

namespace Liesel {

/**
 * @class PageRange
 * @brief Represents a range of pages in a PDF document, such as "1-5", "8", "11-", or "-3"
 * 
 */
class PageRange {
	private:
		std::optional<uint32_t> m_start_page = std::nullopt; // Inclusive
		std::optional<uint32_t> m_end_page = std::nullopt;   // Inclusive
	public:
		PageRange() = default;

		/**
		 * @brief Construct a PageRange from a string representation
		 * 
		 * @param range_str A string like "1-5", "8", "11-", or "-3"
		 */
		explicit PageRange(const std::string_view& range_str);

		std::optional<uint32_t> start_page() const {
			return m_start_page;
		}

		std::optional<uint32_t> end_page() const {
			return m_end_page;
		}
};

/**
 * @class PageRangeList
 * @brief Represents a list of PageRange objects parsed from a string like "1-5,8,11-"
 * 
 */
class PageRangeList {
	private:
		std::vector<PageRange> m_ranges;
	public:
		PageRangeList() = default;

		/**
		 * @brief Construct a PageRangeList from a string representation
		 * 
		 * @param range_str A string like "1-5,8,11-"
		 */
		explicit PageRangeList(const std::string_view& range_str);

		const std::vector<PageRange>& ranges() const {
			return m_ranges;
		}
};

} // namespace Liesel
