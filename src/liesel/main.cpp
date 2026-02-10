/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#include <iostream>
#include <string_view>

#include "version.h"

#include "Book.h"
#include "CLIConfig.h"

inline void show_error(const std::string_view& message) {
	std::cerr << "Liesel: " << message << std::endl
		<< "Use -h or --help for usage information." << std::endl;
}

int main(int argc, char* argv[]) {
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
		options = Liesel::parser.parse(argc, argv);
	} catch (const std::exception& e) {
		show_error(e.what());
		return 1;
	}

	if (options.hasOption('h')) {
		std::cout << help_intro << Liesel::parser.getHelpString() << std::flush;
		return 0;
	}
	if (options.hasOption('v')) {
		std::cout << copyright_string << std::flush;
		return 0;
	}

	try {
		Liesel::ConfigureBookFromCLIOptions(&book, options);
		book.load_pdf();
		book.print();
	} catch (const std::exception& e) {
		show_error(e.what());
		return 1;
	}
	return 0;
}
