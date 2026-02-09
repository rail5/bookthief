/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#pragma once

#include <xgetopt.h>
#include <stdexcept>

namespace Liesel {

class Book;

void ConfigureBookFromCLIOptions(Liesel::Book* book, const XGetOpt::OptionSequence& options, bool strict = true);

void ConfigureBookFromCLIOptionsString(Liesel::Book* book, const std::string& options_str);
std::string GetCLIOptionStringFromBook(Liesel::Book* book);

static inline constexpr auto parser = XGETOPT_PARSER(
	XGETOPT_OPTION('r', "range", "Print only specified page range, e.g. 1-5,8,11-", XGetOpt::RequiredArgument, "range"),
	XGETOPT_OPTION('s', "segment", "Print multiple PDFs in segments of N pages", XGetOpt::RequiredArgument, "N"),
	XGETOPT_OPTION('t', "rescale", "Rescale PDF to specific size. Supported sizes: us-letter, us-legal, a3, a4, a5, or custom dimensions like 8.5x11", XGetOpt::RequiredArgument, "size"),
	XGETOPT_OPTION('d', "density", "Set the DPI density for rendering (default 100)", XGetOpt::RequiredArgument, "DPI"),
	XGETOPT_OPTION('l', "landscape", "Landscape duplex printing", XGetOpt::NoArgument),
	XGETOPT_OPTION('p', "portrait", "Portrait duplex printing [default]", XGetOpt::NoArgument),
	XGETOPT_OPTION('g', "greyscale", "Convert PDF to greyscale", XGetOpt::NoArgument),
	XGETOPT_OPTION('k', "threshold", "Enable black/white thresholding at specified level (0-100)", XGetOpt::RequiredArgument, "level"),
	XGETOPT_OPTION('c', "crop", "Crop given percentage from edges", XGetOpt::RequiredArgument, "L,R,T,B"),
	XGETOPT_OPTION('w', "widen-margins", "Widen center margins by specified amount (for binding)", XGetOpt::RequiredArgument, "amount"),
	XGETOPT_OPTION('D', "divide", "Split the left/right halves of each page into separate pages", XGetOpt::NoArgument),
	XGETOPT_OPTION('N', "no-booklet", "Apply the changes requested, but do not rearrange pages for booklet printing", XGetOpt::NoArgument),
	XGETOPT_OPTION('V', "verbose", "Enable verbose output", XGetOpt::NoArgument),
	XGETOPT_OPTION('v', "version", "Display version information", XGetOpt::NoArgument),
	XGETOPT_OPTION('h', "help", "Display this help message", XGetOpt::NoArgument)
);

} // namespace Liesel
