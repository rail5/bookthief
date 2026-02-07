/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#pragma once

#include <xgetopt.h>
#include <stdexcept>

namespace Liesel {

class Book;

void ConfigureBookFromCLIOptions(Liesel::Book* book, const XGetOpt::OptionSequence& options);

} // namespace Liesel
