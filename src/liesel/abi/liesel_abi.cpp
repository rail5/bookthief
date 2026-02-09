/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#include "liesel_abi.h"

#include <exception>
#include <memory>
#include <string>

#include "../version.h"

#include "../Book.h"
#include "../CropPercentages.h"
#include "../PageDimensions.h"
#include "../PageRangeList.h"

namespace {

static inline bool is_null_or_empty(const char* s) {
	return (s == nullptr) || (s[0] == '\0');
}

static inline LieselStatus map_exception_to_status(const std::exception& e) {
	// Keep mapping coarse; callers can fetch text via *_last_error().
	if (dynamic_cast<const Liesel::Cancelled*>(&e) != nullptr) return LIESEL_E_CANCELLED;
	if (dynamic_cast<const std::invalid_argument*>(&e) != nullptr) return LIESEL_E_INVALID_ARG;
	if (dynamic_cast<const std::out_of_range*>(&e) != nullptr) return LIESEL_E_INVALID_ARG;
	return LIESEL_E_RUNTIME;
}

} // namespace

struct LieselHandle {
	std::string last_error;
};

struct LieselBookHandle {
	LieselHandle* owner = nullptr;
	std::unique_ptr<Liesel::Book> book;
	std::string last_error;

	LieselProgressCallback progress_cb = nullptr;
	void* progress_ud = nullptr;
	LieselCancelCallback cancel_cb = nullptr;
	void* cancel_ud = nullptr;
};

static inline void set_error(LieselHandle* h, const std::string& message) {
	if (h) h->last_error = message;
}

static inline void set_error(LieselBookHandle* b, const std::string& message) {
	if (b) b->last_error = message;
}

static inline LieselStatus fail_invalid_arg(LieselBookHandle* b, const char* message) {
	set_error(b, message ? message : "Invalid argument");
	return LIESEL_E_INVALID_ARG;
}

extern "C" {

LieselHandle* liesel_create(void) {
	try {
		return new LieselHandle{};
	} catch (...) {
		return nullptr;
	}
}

void liesel_destroy(LieselHandle* h) {
	delete h;
}

const char* liesel_version(void) {
	return VERSION;
}

const char* liesel_last_error(LieselHandle* h) {
	if (!h) return "";
	return h->last_error.c_str();
}

LieselBookHandle* liesel_book_create(LieselHandle* h) {
	try {
		auto* b = new LieselBookHandle{};
		b->owner = h;
		b->book = std::make_unique<Liesel::Book>();
		return b;
	} catch (const std::exception& e) {
		set_error(h, e.what());
		return nullptr;
	} catch (...) {
		set_error(h, "Unknown error");
		return nullptr;
	}
}

void liesel_book_destroy(LieselBookHandle* b) {
	delete b;
}

const char* liesel_book_last_error(LieselBookHandle* b) {
	if (!b) return "";
	return b->last_error.c_str();
}

LieselStatus liesel_book_set_input_pdf_path(LieselBookHandle* b, const char* path_utf8) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	if (is_null_or_empty(path_utf8)) return fail_invalid_arg(b, "Input PDF path cannot be empty");
	try {
		b->book->set_input_pdf_path(path_utf8);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_set_output_pdf_path(LieselBookHandle* b, const char* path_utf8) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	if (is_null_or_empty(path_utf8)) return fail_invalid_arg(b, "Output PDF path cannot be empty");
	try {
		b->book->set_output_pdf_path(path_utf8);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_set_verbose(LieselBookHandle* b, int enabled) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	b->book->set_verbose(enabled != 0);
	return LIESEL_OK;
}

LieselStatus liesel_book_set_greyscale(LieselBookHandle* b, int enabled) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	b->book->set_greyscale(enabled != 0);
	return LIESEL_OK;
}

LieselStatus liesel_book_set_divide(LieselBookHandle* b, int enabled) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	b->book->set_divide(enabled != 0);
	return LIESEL_OK;
}

LieselStatus liesel_book_set_booklet(LieselBookHandle* b, int enabled) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	b->book->set_booklet(enabled != 0);
	return LIESEL_OK;
}

LieselStatus liesel_book_set_landscape(LieselBookHandle* b, int enabled) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	b->book->set_landscape(enabled != 0);
	return LIESEL_OK;
}

LieselStatus liesel_book_set_dpi_density(LieselBookHandle* b, uint32_t dpi) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	b->book->set_dpi_density(dpi);
	return LIESEL_OK;
}

LieselStatus liesel_book_set_threshold_level(LieselBookHandle* b, uint8_t level_0_100) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		b->book->set_threshold_level(level_0_100);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_clear_threshold_level(LieselBookHandle* b) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		b->book->set_threshold_level(std::nullopt);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_set_segment_size(LieselBookHandle* b, uint32_t pages_per_segment) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		b->book->set_segment_size(pages_per_segment);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_clear_segment_size(LieselBookHandle* b) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		b->book->set_segment_size(UINT32_MAX);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_set_widen_margins_amount(LieselBookHandle* b, uint32_t amount) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	b->book->set_widen_margins_amount(amount);
	return LIESEL_OK;
}

LieselStatus liesel_book_set_rescale_size(LieselBookHandle* b, const char* size_utf8) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	if (is_null_or_empty(size_utf8)) return fail_invalid_arg(b, "Rescale size cannot be empty");
	try {
		Liesel::PageDimensionPair size(size_utf8);
		b->book->set_rescale_size(size);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_clear_rescale_size(LieselBookHandle* b) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		b->book->set_rescale_size(std::nullopt);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_set_page_ranges(LieselBookHandle* b, const char* range_utf8) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	if (is_null_or_empty(range_utf8)) return fail_invalid_arg(b, "Page range cannot be empty");
	try {
		Liesel::PageRangeList ranges(range_utf8);
		b->book->set_page_ranges(ranges);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_clear_page_ranges(LieselBookHandle* b) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		b->book->set_page_ranges(std::nullopt);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_set_crop_percentages(LieselBookHandle* b, const char* crop_utf8) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	if (is_null_or_empty(crop_utf8)) return fail_invalid_arg(b, "Crop percentages cannot be empty");
	try {
		Liesel::CropPercentages crop;
		crop.set_from_string(crop_utf8);
		b->book->set_crop_percentages(crop);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_set_crop_percentages_lrbt(LieselBookHandle* b, uint8_t l, uint8_t r, uint8_t t, uint8_t bt) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		Liesel::CropPercentages crop;
		crop.set_left(l);
		crop.set_right(r);
		crop.set_top(t);
		crop.set_bottom(bt);
		b->book->set_crop_percentages(crop);
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_load_pdf(LieselBookHandle* b) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;
	try {
		b->book->load_pdf();
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

LieselStatus liesel_book_print(
	LieselBookHandle* b,
	LieselProgressCallback progress_cb,
	void* progress_userdata,
	LieselCancelCallback cancel_cb,
	void* cancel_userdata
) {
	if (!b || !b->book) return LIESEL_E_INVALID_ARG;

	b->progress_cb = progress_cb;
	b->progress_ud = progress_userdata;
	b->cancel_cb = cancel_cb;
	b->cancel_ud = cancel_userdata;

	// Bridge into C++ callbacks for the duration of this call.
	b->book->set_cancel_callback([b]() ->bool {
		if (!b->cancel_cb) return false;
		return b->cancel_cb(b->cancel_ud) != 0;
	});

	b->book->set_progress_callback([b](const Liesel::Book::ProgressInfo& info) {
		if (!b->progress_cb) return;
		// Map ProgressEvent -> ABI enum (same values)
		const auto ev = static_cast<LieselProgressEvent>(info.event);
		b->progress_cb(
			b->progress_ud,
			ev,
			info.segment_index,
			info.page_index,
			info.percent,
			info.message.c_str()
		);
	});
	
	try {
		b->book->print();
		return LIESEL_OK;
	} catch (const std::exception& e) {
		set_error(b, e.what());
		return map_exception_to_status(e);
	}
}

} // extern "C"
