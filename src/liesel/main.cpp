/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#include <iostream>
#include <string_view>

#include <xgetopt.h>

#include "../common/version.h"

#include "Book.h"

inline void show_error(const std::string_view& message) {
	std::cerr << "Liesel: " << message << std::endl
		<< "Use -h or --help for usage information." << std::endl;
}

int main(int argc, char* argv[]) {
	constexpr auto parser = XGETOPT_PARSER(
		XGETOPT_OPTION('r', "range", "Print only specified page range, e.g. 1-5,8,11-", XGetOpt::RequiredArgument, "range"),
		XGETOPT_OPTION('s', "segment", "Print multiple PDFs in segments of N pages", XGetOpt::RequiredArgument, "N"),
		XGETOPT_OPTION('t', "rescale", "Rescale PDF to specific size (e.g. 8.5x11)", XGetOpt::RequiredArgument, "size"),
		XGETOPT_OPTION('d', "density", "Set the DPI density for rendering (default 100)", XGetOpt::RequiredArgument, "DPI"),
		XGETOPT_OPTION('l', "landscape", "Landscape duplex printing", XGetOpt::NoArgument),
		XGETOPT_OPTION('p', "portrait", "Portrait duplex printing [default]", XGetOpt::NoArgument),
		XGETOPT_OPTION('g', "greyscale", "Convert PDF to greyscale", XGetOpt::NoArgument),
		XGETOPT_OPTION('k', "threshold", "Enable black/white thresholding at specified level (0-100)", XGetOpt::RequiredArgument, "level"),
		XGETOPT_OPTION('c', "crop", "Crop given percentage from edges", XGetOpt::RequiredArgument, "L,R,T,B"),
		XGETOPT_OPTION('w', "widen-margins", "Widen center margins by specified amount (for binding)", XGetOpt::RequiredArgument, "amount"),
		XGETOPT_OPTION('a', "auto-widen", "Increasingly widen margins for thicker documents", XGetOpt::OptionalArgument, "max"),
		XGETOPT_OPTION('D', "divide", "Split the left/right halves of each page into separate pages", XGetOpt::NoArgument),
		XGETOPT_OPTION('V', "verbose", "Enable verbose output", XGetOpt::NoArgument),
		XGETOPT_OPTION('v', "version", "Display version information", XGetOpt::NoArgument),
		XGETOPT_OPTION('h', "help", "Display this help message", XGetOpt::NoArgument)
	);

	constexpr std::string_view help_intro =
		"Liesel " VERSION "\n"
		"Usage: liesel [options] <input.pdf> <output.pdf>\n";
	
	constexpr std::string_view copyright_string =
		"Liesel " VERSION "\n"
		"Copyright (C) 2021-" COPYRIGHT_YEAR " Andrew S. Rightenburg\n"
		"\n"
		"This program is free software; you can redistribute it and/or modify\n"
		"it under the terms of the GNU General Public License as published by\n"
		"the Free Software Foundation; either version 3 of the License, or\n"
		"(at your option) any later version.\n"
		"\n"
		"This program is distributed in the hope that it will be useful,\n"
		"but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
		"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
		"GNU General Public License for more details.\n"
		"\n"
		"You should have received a copy of the GNU General Public License\n"
		"along with this program. If not, see http://www.gnu.org/licenses/.\n";
	
	XGetOpt::OptionSequence options;
	try {
		options = parser.parse(argc, argv);
	} catch (const std::exception& e) {
		show_error(e.what());
		return 1;
	}

	Liesel::Book book;

	for (const auto& opt : options) {
		switch (opt.getShortOpt()) {
			// -v, -h (early-exit options)
			case 'v': {
				std::cout << copyright_string;
				return 0;
			}
			case 'h': {
				std::cout << help_intro << parser.getHelpString();
				return 0;
			}

			// -V (sets verbose)
			case 'V': {
				book.set_verbose(true);
				break;
			}

			// Basic flags: -g, -d, -l, -p
			case 'g': {
				book.set_greyscale(true);
				break;
			}
			case 'd': {
				book.set_divide(true);
				break;
			}
			case 'l': {
				book.set_landscape(true);
				break;
			}
			case 'p': {
				book.set_landscape(false);
				break;
			}

			// Other options with arguments
			case 'r': {
				try {
					Liesel::PageRangeList range_list(opt.getArgument());
					book.set_page_ranges(range_list);
				} catch (const std::exception&) {
					show_error("Invalid page range: " + std::string(opt.getArgument()));
					return 1;
				}
				break;
			}
			case 's': {
				try {
					uint32_t segment_size = static_cast<uint32_t>(std::stoul(std::string(opt.getArgument())));
					book.set_segment_size(segment_size);
				} catch (const std::exception&) {
					show_error("Invalid segment size: " + std::string(opt.getArgument()));
					return 1;
				}
				break;
			}
			case 't': {
				try {
					Liesel::PageDimensionPair size(opt.getArgument());
					book.set_rescale_size(size);
				} catch (const std::exception&) {
					show_error("Invalid rescale size: " + std::string(opt.getArgument()));
					return 1;
				}
				break;
			}
			case 'D': {
				try {
					uint32_t dpi = static_cast<uint32_t>(std::stoul(std::string(opt.getArgument())));
					book.set_dpi_density(dpi);
				} catch (const std::exception&) {
					show_error("Invalid DPI density: " + std::string(opt.getArgument()));
					return 1;
				}
				break;
			}
			case 'k': {
				try {
					uint8_t level = static_cast<uint8_t>(std::stoul(std::string(opt.getArgument())));
					if (level > 100) {
						throw std::out_of_range("Threshold level must be between 0 and 100");
					}
					book.set_threshold_level(level);
				} catch (const std::exception&) {
					show_error("Invalid threshold level: " + std::string(opt.getArgument()));
					return 1;
				}
				break;
			}
			case 'c': {
				try {
					Liesel::CropPercentages crop;
					crop.set_from_string(opt.getArgument());
					book.set_crop_percentages(crop);
				} catch (const std::exception&) {
					show_error("Invalid crop percentages: " + std::string(opt.getArgument()));
					return 1;
				}
				break;
			}
			case 'w': {
				try {
					uint32_t amount = static_cast<uint32_t>(std::stoul(std::string(opt.getArgument())));
					book.set_widen_margins_amount(amount);
				} catch (const std::exception&) {
					show_error("Invalid widen margins amount: " + std::string(opt.getArgument()));
					return 1;
				}
				break;
			}
			case 'a': {
				try {
					if (opt.hasArgument()) {
						uint32_t max = static_cast<uint32_t>(std::stoul(std::string(opt.getArgument())));
						book.set_autowiden_max(max);
					} else {
						book.set_autowiden_max(0); // No maximum
					}
				} catch (const std::exception&) {
					show_error("Invalid auto-widen max value: " + std::string(opt.getArgument()));
					return 1;
				}
				break;
			}
			default:
				// Ignore unknown options (should not happen)
				break;
		}
	}

	// The first non-option argument is the input PDF
	// The second non-option argument is the output PDF
	const auto& non_option_args = options.getNonOptionArguments();
	if (non_option_args.size() == 0) {
		std::cerr << "Liesel: Error: No input PDF specified.\n";
		return 1;
	}

	try {
		book.set_input_pdf_path(non_option_args[0]);
	} catch (const std::exception& e) {
		show_error(e.what());
		return 1;
	}

	if (non_option_args.size() == 1) {
		std::cerr << "Liesel: Error: No output PDF specified.\n";
		return 1;
	}
	
	try {
		book.set_output_pdf_path(non_option_args[1]);
	} catch (const std::exception& e) {
		show_error(e.what());
		return 1;
	}
}
