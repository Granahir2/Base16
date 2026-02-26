#include "base16.h"

static void (*resolve_encode (void)) (const uint8_t* restrict,size_t,uint8_t* restrict) {
	__builtin_cpu_init();

#ifdef __x86_64__
	if(__builtin_cpu_supports("avx2")) {
		return avx_encodeBase16;
	} else if(__builtin_cpu_supports("sse4.1")) {
		return sse_encodeBase16;
	}
#endif

	return nullptr;
}

static void (*resolve_decode (void)) (const uint8_t* restrict,size_t,uint8_t* restrict) {
	__builtin_cpu_init();

#ifdef __x86_64__
	if(__builtin_cpu_supports("avx2")) {
		return avx_decodeBase16;
	} else if(__builtin_cpu_supports("sse4.1")) {
		return sse_decodeBase16;
	}
#endif

	return nullptr;
}

static bool (*resolve_isvalid (void)) (const uint8_t* restrict,size_t){
	__builtin_cpu_init();

#ifdef __x86_64__
	if(__builtin_cpu_supports("avx2")) {
		return avx_isValidBase16;
	} else if(__builtin_cpu_supports("sse4.1")) {
		return sse_isValidBase16;
	}
#endif

	return nullptr;
}

void encodeBase16(const uint8_t* restrict input, size_t len, uint8_t* restrict output)
__attribute__ ((ifunc ("resolve_encode")));

void decodeBase16(const uint8_t* restrict input, size_t len, uint8_t* restrict output)
__attribute__ ((ifunc ("resolve_decode")));

bool isValidBase16(const uint8_t* restrict input, size_t len)
__attribute__ ((ifunc ("resolve_isvalid")));
