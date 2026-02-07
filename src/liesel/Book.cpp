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

void Liesel::Book::configure_from_CLI_options(const XGetOpt::OptionSequence& options) {
	for (const auto& opt : options) {
		switch (opt.getShortOpt()) {
			// -V (sets verbose)
			case 'V': {
				set_verbose(true);
				break;
			}

			// Basic flags: -g, -d, -l, -p
			case 'g': {
				set_greyscale(true);
				break;
			}
			case 'd': {
				set_divide(true);
				break;
			}
			case 'l': {
				set_landscape(true);
				break;
			}
			case 'p': {
				set_landscape(false);
				break;
			}

			// Other options with arguments
			case 'r': {
				try {
					Liesel::PageRangeList range_list(opt.getArgument());
					set_page_ranges(range_list);
				} catch (const std::exception&) {
					throw std::runtime_error("Invalid page range: " + std::string(opt.getArgument()));
				}
				break;
			}
			case 's': {
				try {
					uint32_t segment_size = static_cast<uint32_t>(std::stoul(std::string(opt.getArgument())));
					set_segment_size(segment_size);
				} catch (const std::exception&) {
					throw std::runtime_error("Invalid segment size: " + std::string(opt.getArgument()));
				}
				break;
			}
			case 't': {
				try {
					Liesel::PageDimensionPair size(opt.getArgument());
					set_rescale_size(size);
				} catch (const std::exception&) {
					throw std::runtime_error("Invalid rescale size: " + std::string(opt.getArgument()));
				}
				break;
			}
			case 'D': {
				try {
					uint32_t dpi = static_cast<uint32_t>(std::stoul(std::string(opt.getArgument())));
					set_dpi_density(dpi);
				} catch (const std::exception&) {
					throw std::runtime_error("Invalid DPI density: " + std::string(opt.getArgument()));
				}
				break;
			}
			case 'k': {
				try {
					uint8_t level = static_cast<uint8_t>(std::stoul(std::string(opt.getArgument())));
					if (level > 100) {
						throw std::out_of_range("Threshold level must be between 0 and 100");
					}
					set_threshold_level(level);
				} catch (const std::exception&) {
					throw std::runtime_error("Invalid threshold level: " + std::string(opt.getArgument()));
				}
				break;
			}
			case 'c': {
				try {
					Liesel::CropPercentages crop;
					crop.set_from_string(opt.getArgument());
					set_crop_percentages(crop);
				} catch (const std::exception&) {
					throw std::runtime_error("Invalid crop percentages: " + std::string(opt.getArgument()));
				}
				break;
			}
			case 'w': {
				try {
					uint32_t amount = static_cast<uint32_t>(std::stoul(std::string(opt.getArgument())));
					set_widen_margins_amount(amount);
				} catch (const std::exception&) {
					throw std::runtime_error("Invalid widen margins amount: " + std::string(opt.getArgument()));
				}
				break;
			}
			case 'a': {
				try {
					if (opt.hasArgument()) {
						uint32_t max = static_cast<uint32_t>(std::stoul(std::string(opt.getArgument())));
						set_autowiden_max(max);
					} else {
						set_autowiden_max(0); // No maximum
					}
				} catch (const std::exception&) {
					throw std::runtime_error("Invalid auto-widen max value: " + std::string(opt.getArgument()));
				}
				break;
			}
			default:
				break;
		}
	}

	// The first non-option argument is the input PDF
	// The second non-option argument is the output PDF
	const auto& non_option_args = options.getNonOptionArguments();
	if (non_option_args.size() == 0) {
		throw std::runtime_error("No input PDF specified.");
	}

	try {
		set_input_pdf_path(non_option_args[0]);
	} catch (const std::exception& e) {
		throw std::runtime_error(e.what());
	}

	if (non_option_args.size() == 1) {
		throw std::runtime_error("No output PDF specified.");
	}
	
	try {
		set_output_pdf_path(non_option_args[1]);
	} catch (const std::exception& e) {
		throw std::runtime_error(e.what());
	}
}
