/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#include "PageRangeList.h"

Liesel::PageRange::PageRange(const std::string_view& range_str) {
	size_t hyphen_pos = range_str.find('-');

	// No hyphen means single page
	if (hyphen_pos == std::string_view::npos) {
		// Single page
		uint32_t page = static_cast<uint32_t>(std::stoi(std::string(range_str)));
		m_start_page = page;
		m_end_page = page;
		return;
	}

	// Multiple hyphens is invalid
	if (range_str.find('-', hyphen_pos + 1) != std::string_view::npos) {
		throw std::invalid_argument("Invalid page range: " + std::string(range_str));
	}

	// Hyphen at start means open-ended range to end_page
	if (hyphen_pos == 0) {
		m_end_page = static_cast<uint32_t>(std::stoi(std::string(range_str.substr(1))));
		return;
	}

	// Hyphen at end means open-ended range from start_page
	if (hyphen_pos == range_str.length() - 1) {
		m_start_page = static_cast<uint32_t>(std::stoi(std::string(range_str.substr(0, hyphen_pos))));
		return;
	}

	// Otherwise, it's a closed range
	m_start_page = static_cast<uint32_t>(std::stoi(std::string(range_str.substr(0, hyphen_pos))));
	m_end_page = static_cast<uint32_t>(std::stoi(std::string(range_str.substr(hyphen_pos + 1))));
}

std::string Liesel::PageRange::to_string() const {
	std::string result;
	if (m_start_page.has_value()
		&& m_end_page.has_value()
		&& m_start_page.value() == m_end_page.value()
	) {
		// Single page "range"
		// Return a string like "5"
		result = std::to_string(m_start_page.value());
		return result;
	}

	// Return a string like:
	// "5-10", "5-", or "-10"
	if (m_start_page.has_value()) result += std::to_string(m_start_page.value());
	result += "-";
	if (m_end_page.has_value()) result += std::to_string(m_end_page.value());
	return result;
}

Liesel::PageRangeList::PageRangeList(const std::string_view& range_str) {
	// First, split by commas
	std::vector<std::string_view> parts;

	size_t start = 0;
	size_t end = 0;
	while ((end = range_str.find(',', start)) != std::string_view::npos) {
		parts.push_back(range_str.substr(start, end - start));
		start = end + 1;
	}
	parts.push_back(range_str.substr(start));

	// Now, parse each part as a PageRange
	for (const auto& part : parts) {
		m_ranges.emplace_back(part);
	}
}

std::string Liesel::PageRangeList::to_string() const {
	std::string result;
	for (const auto& range : m_ranges) {
		if (!result.empty()) result += ",";
		result += range.to_string();
	}
	return result;
}
