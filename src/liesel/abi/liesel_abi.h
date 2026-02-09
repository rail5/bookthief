/**
 * Copyright (C) 2021-2026 Andrew S. Rightenburg
 * GNU General Public License v3.0+
 */

#pragma once

// C ABI for Liesel
//
// Design goals:
// - No C++ types across the boundary
// - Opaque handles for ownership/lifetime
// - Return codes + last-error string (no exceptions cross ABI)
// - UTF-8 const char* strings

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#if defined(__GNUC__) || defined(__clang__)
#define LIESEL_ABI_API __attribute__((visibility("default")))
#else
#define LIESEL_ABI_API
#endif

typedef struct LieselHandle LieselHandle;
typedef struct LieselBookHandle LieselBookHandle;

typedef enum LieselStatus {
	LIESEL_OK = 0,
	LIESEL_E_INVALID_ARG = 1,
	LIESEL_E_PARSE = 2,
	LIESEL_E_IO = 3,
	LIESEL_E_RUNTIME = 4,
	LIESEL_E_CANCELLED = 5
} LieselStatus;

typedef enum LieselProgressEvent {
	LIESEL_PROGRESS_INFO = 0,
	LIESEL_PROGRESS_RENDER_PAGE = 1,
	LIESEL_PROGRESS_SEGMENT_DONE = 2,
	LIESEL_PROGRESS_PRINT_DONE = 3
} LieselProgressEvent;

// Note: Callbacks are optional and may be NULL.
// message_utf8 points to a short-lived string (valid during callback only).
typedef void (*LieselProgressCallback)(
	void* userdata,
	LieselProgressEvent event,
	uint32_t segment_index,
	uint32_t page_index,
	uint32_t percent,
	const char* message_utf8
);

// Return nonzero to request cancellation.
typedef int (*LieselCancelCallback)(void* userdata);

// Library / context
LIESEL_ABI_API LieselHandle* liesel_create(void);
LIESEL_ABI_API void liesel_destroy(LieselHandle* h);

// Returns VERSION from src/liesel/version.h
LIESEL_ABI_API const char* liesel_version(void);

// Returns a pointer to an internal string buffer (valid until next error on the same handle).
LIESEL_ABI_API const char* liesel_last_error(LieselHandle* h);

// Book object
LIESEL_ABI_API LieselBookHandle* liesel_book_create(LieselHandle* h);
LIESEL_ABI_API void liesel_book_destroy(LieselBookHandle* b);
LIESEL_ABI_API const char* liesel_book_last_error(LieselBookHandle* b);

// Configuration (based on setters in src/liesel/Book.h)
LIESEL_ABI_API LieselStatus liesel_book_set_input_pdf_path(LieselBookHandle* b, const char* path_utf8);
LIESEL_ABI_API LieselStatus liesel_book_set_output_pdf_path(LieselBookHandle* b, const char* path_utf8);

LIESEL_ABI_API LieselStatus liesel_book_set_verbose(LieselBookHandle* b, int enabled);
LIESEL_ABI_API LieselStatus liesel_book_set_greyscale(LieselBookHandle* b, int enabled);
LIESEL_ABI_API LieselStatus liesel_book_set_divide(LieselBookHandle* b, int enabled);
LIESEL_ABI_API LieselStatus liesel_book_set_booklet(LieselBookHandle* b, int enabled);
LIESEL_ABI_API LieselStatus liesel_book_set_landscape(LieselBookHandle* b, int enabled);

LIESEL_ABI_API LieselStatus liesel_book_set_dpi_density(LieselBookHandle* b, uint32_t dpi);
LIESEL_ABI_API LieselStatus liesel_book_set_threshold_level(LieselBookHandle* b, uint8_t level_0_100);
LIESEL_ABI_API LieselStatus liesel_book_clear_threshold_level(LieselBookHandle* b);

LIESEL_ABI_API LieselStatus liesel_book_set_segment_size(LieselBookHandle* b, uint32_t pages_per_segment);
LIESEL_ABI_API LieselStatus liesel_book_clear_segment_size(LieselBookHandle* b);
LIESEL_ABI_API LieselStatus liesel_book_set_widen_margins_amount(LieselBookHandle* b, uint32_t amount);

// Parsing is implemented in C++:
// - PageDimensionPair("us-letter" or "8.5x11") in src/liesel/PageDimensions.h
// - PageRangeList("1-5,8,11-") in src/liesel/PageRangeList.h
// - CropPercentages::set_from_string("L,R,T,B") in src/liesel/CropPercentages.h
LIESEL_ABI_API LieselStatus liesel_book_set_rescale_size(LieselBookHandle* b, const char* size_utf8);
LIESEL_ABI_API LieselStatus liesel_book_clear_rescale_size(LieselBookHandle* b);

LIESEL_ABI_API LieselStatus liesel_book_set_page_ranges(LieselBookHandle* b, const char* range_utf8);
LIESEL_ABI_API LieselStatus liesel_book_clear_page_ranges(LieselBookHandle* b);

LIESEL_ABI_API LieselStatus liesel_book_set_crop_percentages(LieselBookHandle* b, const char* crop_utf8);
LIESEL_ABI_API LieselStatus liesel_book_set_crop_percentages_lrbt(LieselBookHandle* b, uint8_t l, uint8_t r, uint8_t t, uint8_t bt);

// Execution
LIESEL_ABI_API LieselStatus liesel_book_load_pdf(LieselBookHandle* b);

// Note: callbacks are currently best-effort; implementation may emit only coarse events at first.
LIESEL_ABI_API LieselStatus liesel_book_print(
	LieselBookHandle* b,
	LieselProgressCallback progress_cb,
	void* progress_userdata,
	LieselCancelCallback cancel_cb,
	void* cancel_userdata
);

#ifdef __cplusplus
} // extern "C"
#endif
