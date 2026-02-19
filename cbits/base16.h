#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// For optimal performance, input and output should be aligned to 32 byte boundaries,
// both at input and output.
void encodeBase16(const uint8_t* restrict input, size_t len, uint8_t* restrict output);

// decodeBase16 accepts mixed-case input. Warning: No check is done by the function itself.
void decodeBase16(const uint8_t* restrict input, size_t len, uint8_t* restrict output);

// Checks that input obeys the valid alphabet
bool isValidBase16(const uint8_t* restrict input, size_t len);

// The above functions are indirect: at object load time, CPU features are probed
// and the appropriate vectorized version is chosen among the set below.
// Note that if SSE 4.1 is not supported, then they will crash!

#ifdef __x86_64__

// No feature check is performed by the code paths below. Use at your own risk.
// SSE specific code path; requires SSE 4.1
void sse_encodeBase16(const uint8_t* restrict input, size_t len, uint8_t* restrict output);
void sse_decodeBase16(const uint8_t* restrict input, size_t len, uint8_t* restrict output);
bool sse_isValidBase16(const uint8_t* restrict input, size_t len);

// AVX specific code path; requires AVX 2
void avx_encodeBase16(const uint8_t* restrict input, size_t len, uint8_t* restrict output);
void avx_decodeBase16(const uint8_t* restrict input, size_t len, uint8_t* restrict output);
bool avx_isValidBase16(const uint8_t* restrict input, size_t len);

#endif
