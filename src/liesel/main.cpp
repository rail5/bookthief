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
		"Usage: liesel [options] <input.pdf> <output.pdf>\n"
		"Options:\n";
	
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
	
	Liesel::Book book;
	XGetOpt::OptionSequence options;
	try {
		options = parser.parse(argc, argv);
	} catch (const std::exception& e) {
		show_error(e.what());
		return 1;
	}

	if (options.hasOption('h')) {
		std::cout << help_intro << parser.getHelpString() << std::flush;
		return 0;
	}

	if (options.hasOption('v')) {
		std::cout << copyright_string << std::flush;
		return 0;
	}

	try {
		book.configure_from_CLI_options(options);
	} catch (const std::exception& e) {
		show_error(e.what());
		return 1;
	}
}
