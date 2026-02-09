/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#include "CLIConfig.h"
#include "Book.h"

void Liesel::ConfigureBookFromCLIOptions(Liesel::Book* book, const XGetOpt::OptionSequence& options, bool strict) {
	if (!book) throw std::invalid_argument("Invalid Book passed to ConfigureBookFromCLIOptions.");

	for (const auto& opt : options) {
		switch (opt.getShortOpt()) {
			// -V (sets verbose)
			case 'V': book->set_verbose(true); break;

			// Basic flags: -g, -D, -l, -p
			case 'g': book->set_greyscale(true); break;
			case 'D': book->set_divide(true); break;
			case 'l': book->set_landscape(true); break;
			case 'p': book->set_landscape(false); break;
			case 'N': book->set_booklet(false); break;

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
			default:
				break;
		}
	}

	// The first non-option argument is the input PDF
	// The second non-option argument is the output PDF
	const auto& non_option_args = options.getNonOptionArguments();
	if (non_option_args.size() == 0) {
		if (strict) throw std::runtime_error("No input PDF specified.");
		// Otherwise, allow missing input for more relaxed parsing
		return;
	}

	try {
		book->set_input_pdf_path(non_option_args[0]);
	} catch (const std::exception& e) {
		throw std::runtime_error(e.what());
	}

	if (non_option_args.size() == 1) {
		if (strict) throw std::runtime_error("No output PDF specified.");
		// Otherwise, allow missing output for more relaxed parsing
		return;
	}
	
	try {
		book->set_output_pdf_path(non_option_args[1]);
	} catch (const std::exception& e) {
		throw std::runtime_error(e.what());
	}
}

void Liesel::ConfigureBookFromCLIOptionsString(Liesel::Book* book, const std::string& options_str) {
	if (!book) throw std::invalid_argument("Invalid Book passed to ConfigureBookFromCLIOptionsString.");

	// Supports "quoted strings" via std::quoted; not full shell parsing
	std::istringstream iss(options_str);
	std::vector<std::string> args;
	std::string token;
	while (iss >> std::quoted(token)) args.push_back(token);

	// Ensure argv[0] is present and looks like "liesel"
	if (args.empty() || !args[0].contains("liesel")) {
		args.insert(args.begin(), "liesel");
	}

	// Build a writable argv buffer (safer if the parser mutates argv)
	std::vector<std::vector<char>> argv_storage;
	argv_storage.reserve(args.size());
	for (const auto& a : args) {
		argv_storage.emplace_back(a.begin(), a.end());
		argv_storage.back().push_back('\0');
	}

	std::vector<char*> argv;
	argv.reserve(argv_storage.size() + 1);
	for (auto& buf : argv_storage) argv.push_back(buf.data());

	argv.push_back(nullptr);

	const int argc = static_cast<int>(args.size());

	XGetOpt::OptionSequence options;
	try {
		options = Liesel::parser.parse(argc, argv.data());
	} catch (const std::exception& e) {
		throw std::runtime_error(e.what());
	}

	ConfigureBookFromCLIOptions(book, options, false); // Disable strict mode
}

std::string Liesel::GetCLIOptionStringFromBook(Liesel::Book* book) {
	if (!book) throw std::invalid_argument("Invalid Book passed to GetCLIOptionStringFromBook.");

	std::string options = "liesel"; // argv[0] = "liesel"

	if (book->has_input_pdf_path()) options += " \"" + book->get_input_pdf_path() + "\"";

	if (book->verbose()) options += " -V";
	if (book->greyscale()) options += " -g";
	if (book->divide()) options += " -D";

	if (book->landscape()) {
		options += " -l";
	} else {
		options += " -p";
	}

	if (!book->booklet()) options += " -N";

	if (book->page_ranges().has_value()) {
		options += " -r";
		options += " " + book->page_ranges()->to_string();
	}
	if (book->segment_size() != UINT32_MAX) {
		options += " -s";
		options += " " + std::to_string(book->segment_size());
	}
	if (book->rescale_size().has_value()) {
		options += " -t";
		options += " " + book->rescale_size()->to_string();
	}

	options += " -d";
	options += " " + std::to_string(book->dpi_density());

	if (book->threshold_level().has_value()) {
		options += " -k";
		options += " " + std::to_string(book->threshold_level().value());
	}

	if (!book->crop_percentages().is_empty()) {
		const auto& crop = book->crop_percentages();
		std::string crop_str = std::to_string(crop.left()) + "," +
			std::to_string(crop.right()) + "," +
			std::to_string(crop.top()) + "," +
			std::to_string(crop.bottom());
		options += " -c";
		options += " " + crop_str;
	}
	if (book->widen_margins_amount() > 0) {
		options += " -w";
		options += " " + std::to_string(book->widen_margins_amount());
	}

	return options;
}
