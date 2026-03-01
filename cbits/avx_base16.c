#ifdef __x86_64__ 
#include <immintrin.h>
#include "base16.h"


__attribute__((target("avx2")))
void avx_encodeBase16(const uint8_t* restrict input, size_t len,
		  uint8_t* restrict output) {

	static const _Alignas(32) uint8_t lookup[32] = {
		'0','1','2','3','4','5','6','7',
		'8','9','a','b','c','d','e','f',
		'0','1','2','3','4','5','6','7',
		'8','9','a','b','c','d','e','f',
	};

	const auto masklow =  _mm256_set1_epi8(0xf);
	const auto lut = _mm256_load_si256((__m256i*)lookup);

	const size_t iter_len = sizeof(lut);

	// Fix alignment in input buffer
	if(__builtin_expect(len >= iter_len, 1)) {
		auto extra_iters = iter_len - ((intptr_t)(input) & (iter_len-1));

		auto v = _mm256_lddqu_si256((__m256i*)(input));
		v  = _mm256_permute4x64_epi64(v, 0b11011000);

		auto lows = _mm256_and_si256(v, masklow);
		auto highs = _mm256_srli_epi16(v, 4);
		highs = _mm256_and_si256(highs, masklow);

		lows  = _mm256_shuffle_epi8(lut, lows);
		highs = _mm256_shuffle_epi8(lut, highs);

		auto ret1 = _mm256_unpacklo_epi8(highs, lows);
		auto ret2 = _mm256_unpackhi_epi8(highs, lows);

		_mm256_storeu_si256((__m256i*)output,   ret1);
		_mm256_storeu_si256(((__m256i*)output)+1, ret2);

		len -= extra_iters;
		output += 2*extra_iters;
		input += extra_iters;
	}

	auto i_ptr = (__m256i*)input;
	auto o_ptr = (__m256i*)output;

	for(size_t i = 0; i < len/iter_len; ++i, ++i_ptr, o_ptr+=2) {
		auto v = _mm256_load_si256(i_ptr);

		// Because unpack is kind of awkward in AVX, we need to
		// shuffle first. That way the first elements end
		// up in the low part of each 128 bit lane
		v  = _mm256_permute4x64_epi64(v, 0b11011000);

		auto lows = _mm256_and_si256(v, masklow); 
		auto highs = _mm256_srli_epi16(v, 4);
		highs = _mm256_and_si256(highs, masklow);

		// Do substitutions
		lows  = _mm256_shuffle_epi8(lut, lows);
		highs = _mm256_shuffle_epi8(lut, highs);

		auto ret1 = _mm256_unpacklo_epi8(highs, lows);
		auto ret2 = _mm256_unpackhi_epi8(highs, lows);

		_mm256_storeu_si256(o_ptr,   ret1);
		_mm256_storeu_si256(o_ptr+1, ret2);
	}

	const uint8_t* rem_i_ptr = (const uint8_t*)i_ptr;
	uint8_t* rem_o_ptr = (uint8_t*)o_ptr;
	for(size_t i = len%iter_len; i > 0; --i, ++rem_i_ptr, rem_o_ptr+=2) {
		uint8_t x = *rem_i_ptr;
		*rem_o_ptr     = lookup[x>>4];
		*(rem_o_ptr+1) = lookup[x&0xf];
	}
}

__attribute__((target("avx2")))
void avx_decodeBase16(const uint8_t* restrict input, size_t len, uint8_t* restrict output) {
	auto i_ptr = (const __m256i*)input;
	auto o_ptr = (__m256i*)output;

	const auto c9       = _mm256_set1_epi8('9');
	const auto lnibbles = _mm256_set1_epi8(0x0f); 

	// When vectors are fetched, the weights of each char is like so
	// H1 L1 H2 L2...
	// The following permutation allows us to go from that to
	// L1 L2... H1 H2... (inside each 128 bit lane)
	static const _Alignas(32) uint8_t perm_lohi[32] = {
		1, 3, 5, 7, 9, 11, 13, 15,
		0, 2, 4, 6, 8, 10, 12, 14,
		1, 3, 5, 7, 9, 11, 13, 15,
		0, 2, 4, 6, 8, 10, 12, 14,
	};

	const auto p_lohi = _mm256_load_si256((const __m256i*)perm_lohi);
	const auto p_hilo = _mm256_shuffle_epi32(p_lohi, 0b01001110);

	const size_t iter_len = 2*sizeof(c9);

	for(size_t i = 0; i < len/iter_len; ++i,i_ptr+=2,++o_ptr) {
		auto rl = _mm256_lddqu_si256(i_ptr);
		auto rh = _mm256_lddqu_si256(i_ptr+1);

		const auto lettermask_l = _mm256_cmpgt_epi8(rl, c9);
		const auto lettermask_h = _mm256_cmpgt_epi8(rh, c9);

		// Adds 0x9 to low nibbles of each char, where there are letters
		rl = _mm256_add_epi8(_mm256_and_si256(lettermask_l, c9), rl);
		rh = _mm256_add_epi8(_mm256_and_si256(lettermask_h, c9), rh);

		rl = _mm256_and_si256(rl, lnibbles);
		rh = _mm256_and_si256(rh, lnibbles);
		// Here, all characters have been converted to their numeric
		// value. It remains to do the horizontal sums...

		// The variable names should be understood as what happens
		// *inside each 128 bit lane*
		const auto hil_lol = _mm256_shuffle_epi8(rl, p_hilo);
		const auto loh_hih = _mm256_shuffle_epi8(rh, p_lohi);

		auto hil_hih = _mm256_blend_epi16(hil_lol, loh_hih, 0b11110000);
		auto loh_lol = _mm256_blend_epi16(hil_lol, loh_hih, 0b00001111);

		hil_hih = _mm256_slli_epi16(hil_hih, 4);
		auto lol_loh = _mm256_shuffle_epi32(loh_lol, 0b01001110);

		auto result = _mm256_add_epi8(hil_hih, lol_loh);
		// Compared to SSE, we need an additional permute. Indeed, ordering
		// the lanes of rl and rh as ( 0, 1), ( 2, 3) we have decoded and
		// stored (0' || 2', 1' || 3')

		_mm256_storeu_si256(o_ptr, _mm256_permute4x64_epi64(result, 0b11011000));
	}

	const uint8_t* rem_i_ptr = (const uint8_t*)i_ptr;
	uint8_t* rem_o_ptr = (uint8_t*)o_ptr;
	for(size_t i = len%iter_len; i > 0; i-=2, rem_i_ptr+=2, ++rem_o_ptr) {
		int h = *rem_i_ptr;
		int l = *(rem_i_ptr+1);

		h = (h > '9' ? h + 0x9 : h) & 0x0f;
		l = (l > '9' ? l + 0x9 : l) & 0x0f;

		*rem_o_ptr = (h << 4) | l;
	}
}

__attribute__((target("avx2")))
bool avx_isValidBase16(const uint8_t* restrict input, size_t len) {

	// The valid alphabet has three ranges, '0' - '9', 'a' - 'f', 'A'-'F'
	// Testing that a vector is 0 is easy, so we use _active low_ logic
	// !( ('0' <= x && x <= '9') || ('a' <= x && x <= 'f') || ... ) rewrites as
	// (x < '0' || x > '9') && (x < 'a' || x > 'f') && ...
	
	const auto c0 = _mm256_set1_epi8('0');
	const auto c9 = _mm256_set1_epi8('9');
	const auto ca = _mm256_set1_epi8('a');
	const auto cf = _mm256_set1_epi8('f');
	const auto cA = _mm256_set1_epi8('A');
	const auto cF = _mm256_set1_epi8('F');

	const size_t iter_len = sizeof(c0);

	if(__builtin_expect(len >= iter_len, 1)) {
		auto extra_iters = iter_len - ((intptr_t)(input) & (iter_len-1));

		auto x = _mm256_lddqu_si256((__m256i*)input);

		auto num = _mm256_or_si256(_mm256_cmpgt_epi8(c0, x),
		                           _mm256_cmpgt_epi8(x, c9));

		auto lower = _mm256_or_si256(_mm256_cmpgt_epi8(ca, x),
		                             _mm256_cmpgt_epi8(x, cf));

		auto upper = _mm256_or_si256(_mm256_cmpgt_epi8(cA, x),
		                             _mm256_cmpgt_epi8(x, cF));

		// If num && lower && upper == 0, then all characters are good and testz returns 1
		if(__builtin_expect(!_mm256_testz_si256(_mm256_and_si256(num, lower), upper) , 0)) {
			return false;
		}

		input += extra_iters;
		len -= extra_iters;
	}

	auto i_ptr = (const __m256i*)input;

	for(size_t i = 0; i < len/iter_len; ++i, ++i_ptr) {
		auto x = _mm256_load_si256(i_ptr);

		auto num = _mm256_or_si256(_mm256_cmpgt_epi8(c0, x),
		                           _mm256_cmpgt_epi8(x, c9));

		auto lower = _mm256_or_si256(_mm256_cmpgt_epi8(ca, x),
		                             _mm256_cmpgt_epi8(x, cf));

		auto upper = _mm256_or_si256(_mm256_cmpgt_epi8(cA, x),
		                             _mm256_cmpgt_epi8(x, cF));

		// If num && lower && upper == 0, then all characters are good and testz returns 1
		if(__builtin_expect(!_mm256_testz_si256(_mm256_and_si256(num, lower), upper) , 0)) {
			return false;
		}
	}

	const uint8_t* rem_i_ptr = (const uint8_t*)i_ptr;
	for(size_t i = len%iter_len; i > 0; --i, ++rem_i_ptr) {
		uint8_t x = *rem_i_ptr;
		if(__builtin_expect((x < '0' || x > '9') &&
				    (x < 'a' || x > 'f') &&
				    (x < 'A' || x > 'F'), 0)) {
			return false;
		}
	}
	return true;
}
#endif
