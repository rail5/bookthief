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

static inline constexpr XGetOpt::OptionParser<
	XGetOpt::Option<'r', "range", "Print only specified page range, e.g. 1-5,8,11-", XGetOpt::RequiredArgument, "range">,
	XGetOpt::Option<'s', "segment", "Print multiple PDFs in segments of N pages", XGetOpt::RequiredArgument, "N">,
	XGetOpt::Option<'t', "rescale", "Rescale PDF to specific size. Supported sizes: us-letter, us-legal, a3, a4, a5, or custom dimensions like 8.5x11", XGetOpt::RequiredArgument, "size">,
	XGetOpt::Option<'d', "density", "Set the DPI density for rendering (default 100)", XGetOpt::RequiredArgument, "DPI">,
	XGetOpt::Option<'l', "landscape", "Landscape duplex printing", XGetOpt::NoArgument>,
	XGetOpt::Option<'p', "portrait", "Portrait duplex printing [default]", XGetOpt::NoArgument>,
	XGetOpt::Option<'g', "greyscale", "Convert PDF to greyscale", XGetOpt::NoArgument>,
	XGetOpt::Option<'k', "threshold", "Enable black/white thresholding at specified level (0-100)", XGetOpt::RequiredArgument, "level">,
	XGetOpt::Option<'c', "crop", "Crop given percentage from edges", XGetOpt::RequiredArgument, "L,R,T,B">,
	XGetOpt::Option<'w', "widen-margins", "Widen center margins by specified amount (for binding)", XGetOpt::RequiredArgument, "amount">,
	XGetOpt::Option<'D', "divide", "Split the left/right halves of each page into separate pages", XGetOpt::NoArgument>,
	XGetOpt::Option<'N', "no-booklet", "Apply the changes requested, but do not rearrange pages for booklet printing", XGetOpt::NoArgument>,
	XGetOpt::Option<'V', "verbose", "Enable verbose output", XGetOpt::NoArgument>,
	XGetOpt::Option<'v', "version", "Display version information", XGetOpt::NoArgument>,
	XGetOpt::Option<'h', "help", "Display this help message", XGetOpt::NoArgument>
> parser;

} // namespace Liesel
