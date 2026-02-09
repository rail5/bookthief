# Liesel C ABI

This folder contains a C ABI wrapper around the C++ `Liesel::Book` API so that non-C++ applications (e.g. BookThief written in Object Pascal) can link against Liesel without depending on C++ name mangling or exceptions.

## Files

- `liesel_abi.h`: C header (stable ABI surface)
- `liesel_abi.cpp`: C++ implementation that wraps `Liesel::Book` behind opaque handles

## Notes

- GNU/Linux only.
- Functions return `LieselStatus` codes; for details use `liesel_book_last_error()` / `liesel_last_error()`.
