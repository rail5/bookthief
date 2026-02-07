/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#include "Book.h"

#include <algorithm>

void Liesel::Book::set_input_pdf_path(const std::string_view& path) {
	// Verify that the path is valid, the file exists, and is a PDF
	std::filesystem::path p(path);
	if (!std::filesystem::exists(p)) {
		throw std::invalid_argument("Input PDF file does not exist: " + std::string(path));
	}
	// Case-insensitive check for .pdf extension
	auto ext = p.extension().string();
	std::transform(ext.begin(), ext.end(), ext.begin(), ::tolower);
	if (ext != ".pdf") {
		throw std::invalid_argument("Input file is not a PDF: " + std::string(path));
	}

	input_pdf_path = p;
}

void Liesel::Book::set_output_pdf_path(const std::string_view& path) {
	// Verify either:
	// 1. The file does not exist (will be created) AND we have write permissions in the directory
	// Or 2. The file exists AND we have write permissions to the file
	std::filesystem::path p(path);
	if (std::filesystem::exists(p)) {
		// File exists, check write permissions
		std::error_code ec;
		auto perms = std::filesystem::status(p, ec).permissions();
		// Check for owner/group/others write permissions
		unsigned write_perms = static_cast<unsigned>(std::filesystem::perms::owner_write) |
			static_cast<unsigned>(std::filesystem::perms::group_write) |
			static_cast<unsigned>(std::filesystem::perms::others_write);
		if (ec || !(static_cast<unsigned>(perms) & write_perms)) {
			throw std::invalid_argument("No write permission for output PDF file: " + std::string(path));
		}
	} else {
		// File does not exist, check write permissions in the directory
		auto dir = p.parent_path();
		if (dir.empty()) {
			dir = std::filesystem::current_path();
		}
		std::error_code ec;
		auto perms = std::filesystem::status(dir, ec).permissions();
		unsigned write_perms = static_cast<unsigned>(std::filesystem::perms::owner_write) |
			static_cast<unsigned>(std::filesystem::perms::group_write) |
			static_cast<unsigned>(std::filesystem::perms::others_write);
		if (ec || !(static_cast<unsigned>(perms) & write_perms)) {
			throw std::invalid_argument("No write permission in output PDF directory: " + dir.string());
		}
	}
}
