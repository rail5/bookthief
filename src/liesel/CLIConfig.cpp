/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#include "CLIConfig.h"
#include "Book.h"

void Liesel::ConfigureBookFromCLIOptions(Liesel::Book* book, const XGetOpt::OptionSequence& options) {
	if (!book) throw std::invalid_argument("Invalid Book passed to ConfigureBookFromCLIOptions.");

	for (const auto& opt : options) {
		switch (opt.getShortOpt()) {
			// -V (sets verbose)
			case 'V': {
				book->set_verbose(true);
				break;
			}

			// Basic flags: -g, -d, -l, -p
			case 'g': {
				book->set_greyscale(true);
				break;
			}
			case 'D': {
				book->set_divide(true);
				break;
			}
			case 'l': {
				book->set_landscape(true);
				break;
			}
			case 'p': {
				book->set_landscape(false);
				break;
			}

			// Other options with arguments
			case 'r': {
				try {
					Liesel::PageRangeList range_list(opt.getArgument());
					book->set_page_ranges(range_list);
				} catch (const std::exception&) {
					throw std::runtime_error("Invalid page range: " + std::string(opt.getArgument()));
				}
				break;
			}
			case 's': {
				try {
					uint32_t segment_size = static_cast<uint32_t>(std::stoul(std::string(opt.getArgument())));
					book->set_segment_size(segment_size);
				} catch (const std::exception&) {
					throw std::runtime_error("Invalid segment size: " + std::string(opt.getArgument()));
				}
				break;
			}
			case 't': {
				try {
					Liesel::PageDimensionPair size(opt.getArgument());
					book->set_rescale_size(size);
				} catch (const std::exception&) {
					throw std::runtime_error("Invalid rescale size: " + std::string(opt.getArgument()));
				}
				break;
			}
			case 'd': {
				try {
					uint32_t dpi = static_cast<uint32_t>(std::stoul(std::string(opt.getArgument())));
					book->set_dpi_density(dpi);
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
					book->set_threshold_level(level);
				} catch (const std::exception&) {
					throw std::runtime_error("Invalid threshold level: " + std::string(opt.getArgument()));
				}
				break;
			}
			case 'c': {
				try {
					Liesel::CropPercentages crop;
					crop.set_from_string(opt.getArgument());
					book->set_crop_percentages(crop);
				} catch (const std::exception&) {
					throw std::runtime_error("Invalid crop percentages: " + std::string(opt.getArgument()));
				}
				break;
			}
			case 'w': {
				try {
					uint32_t amount = static_cast<uint32_t>(std::stoul(std::string(opt.getArgument())));
					book->set_widen_margins_amount(amount);
				} catch (const std::exception&) {
					throw std::runtime_error("Invalid widen margins amount: " + std::string(opt.getArgument()));
				}
				break;
			}
			case 'a': {
				try {
					if (opt.hasArgument()) {
						uint32_t max = static_cast<uint32_t>(std::stoul(std::string(opt.getArgument())));
						book->set_autowiden_max(max);
					} else {
						book->set_autowiden_max(0); // No maximum
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
		book->set_input_pdf_path(non_option_args[0]);
	} catch (const std::exception& e) {
		throw std::runtime_error(e.what());
	}

	if (non_option_args.size() == 1) {
		throw std::runtime_error("No output PDF specified.");
	}
	
	try {
		book->set_output_pdf_path(non_option_args[1]);
	} catch (const std::exception& e) {
		throw std::runtime_error(e.what());
	}
}
