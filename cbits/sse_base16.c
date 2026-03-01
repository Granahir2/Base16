#ifdef __x86_64__

#include <smmintrin.h>
#include "base16.h"

__attribute__((target("sse4.1,no-avx")))
void sse_encodeBase16(const uint8_t* restrict input, size_t len,
		  uint8_t* restrict output) {

	static const _Alignas(16) uint8_t lookup[16] = {
		'0','1','2','3','4','5','6','7',
		'8','9','a','b','c','d','e','f',
	};

	const auto masklow =  _mm_set1_epi8(0xf);
	const auto lut = _mm_load_si128((__m128i*)lookup);

	const size_t iter_len = sizeof(lut);

	// Fix alignment in input buffer
	if(__builtin_expect(len >= iter_len, 1)) {
		auto extra_iters = iter_len - ((intptr_t)(input) & (iter_len-1));

		auto v = _mm_lddqu_si128((__m128i*)(input));
		auto lows = _mm_and_si128(v, masklow);
		auto highs = _mm_srli_epi16(v, 4);
		highs = _mm_and_si128(highs, masklow);

		lows  = _mm_shuffle_epi8(lut, lows);
		highs = _mm_shuffle_epi8(lut, highs);

		auto ret1 = _mm_unpacklo_epi8(highs, lows);
		auto ret2 = _mm_unpackhi_epi8(highs, lows);

		_mm_storeu_si128((__m128i*)output,   ret1);
		_mm_storeu_si128(((__m128i*)output)+1, ret2);

		len -= extra_iters;
		output += 2*extra_iters;
		input += extra_iters;
	}

	auto i_ptr = (__m128i*)input;
	auto o_ptr = (__m128i*)output;

	for(size_t i = 0; i < len/iter_len; ++i, ++i_ptr, o_ptr+=2) {
		auto v = _mm_load_si128(i_ptr);

		auto lows = _mm_and_si128(v, masklow); 
		auto highs = _mm_srli_epi16(v, 4);
		highs = _mm_and_si128(highs, masklow);

		// Do substitutions
		lows  = _mm_shuffle_epi8(lut, lows);
		highs = _mm_shuffle_epi8(lut, highs);

		auto ret1 = _mm_unpacklo_epi8(highs, lows);
		auto ret2 = _mm_unpackhi_epi8(highs, lows);

		_mm_storeu_si128(o_ptr,   ret1);
		_mm_storeu_si128(o_ptr+1, ret2);
	}

	const uint8_t* rem_i_ptr = (const uint8_t*)i_ptr;
	uint8_t* rem_o_ptr = (uint8_t*)o_ptr;
	for(size_t i = len%iter_len; i > 0; --i, ++rem_i_ptr, rem_o_ptr+=2) {
		uint8_t x = *rem_i_ptr;
		*rem_o_ptr     = lookup[x>>4];
		*(rem_o_ptr+1) = lookup[x&0xf];
	}
}

__attribute__((target("sse4.1,no-avx")))
void sse_decodeBase16(const uint8_t* restrict input, size_t len, uint8_t* restrict output) {
	auto i_ptr = (const __m128i*)input;
	auto o_ptr = (__m128i*)output;

	const auto c9       = _mm_set1_epi8('9');
	const auto lnibbles = _mm_set1_epi8(0x0f); 

	// When vectors are fetched, the weights of each char is like so
	// H1 L1 H2 L2...
	// The following permutation allows us to go from that to
	// L1 L2... H1 H2...
	static const _Alignas(16) uint8_t perm_lohi[16] = {
		1, 3, 5, 7, 9, 11, 13, 15,
		0, 2, 4, 6, 8, 10, 12, 14,
	};

	const auto p_lohi = _mm_load_si128((const __m128i*)perm_lohi);
	const auto p_hilo = _mm_shuffle_epi32(p_lohi, 0b01001110);

	const size_t iter_len = 2*sizeof(c9);

	for(size_t i = 0; i < len/iter_len; ++i,i_ptr+=2,++o_ptr) {
		auto rl = _mm_lddqu_si128(i_ptr);
		auto rh = _mm_lddqu_si128(i_ptr+1);

		const auto lettermask_l = _mm_cmpgt_epi8(rl, c9);
		const auto lettermask_h = _mm_cmpgt_epi8(rh, c9);

		// Adds 0x9 to low nibbles of each char, where there are letters
		rl = _mm_add_epi8(_mm_and_si128(lettermask_l, c9), rl);
		rh = _mm_add_epi8(_mm_and_si128(lettermask_h, c9), rh);

		rl = _mm_and_si128(rl, lnibbles);
		rh = _mm_and_si128(rh, lnibbles);
		// Here, all characters have been converted to their numeric
		// value. It remains to do the horizontal sums...
		
		const auto hil_lol = _mm_shuffle_epi8(rl, p_hilo);
		const auto loh_hih = _mm_shuffle_epi8(rh, p_lohi);

		auto hil_hih = _mm_blend_epi16(hil_lol, loh_hih, 0b11110000);
		auto loh_lol = _mm_blend_epi16(hil_lol, loh_hih, 0b00001111);

		hil_hih = _mm_slli_epi16(hil_hih, 4);
		auto lol_loh = _mm_shuffle_epi32(loh_lol, 0b01001110);

		_mm_storeu_si128(o_ptr, _mm_add_epi8(hil_hih, lol_loh)); 
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

__attribute__((target("sse4.1,no-avx")))
bool sse_isValidBase16(const uint8_t* restrict input, size_t len) {

	// The valid alphabet has three ranges, '0' - '9', 'a' - 'f', 'A'-'F'
	// Testing that a vector is 0 is easy, so we use _active low_ logic
	// !( ('0' <= x && x <= '9') || ('a' <= x && x <= 'f') || ... ) rewrites as
	// (x < '0' || x > '9') && (x < 'a' || x > 'f') && ...
	
	const auto c0 = _mm_set1_epi8('0');
	const auto c9 = _mm_set1_epi8('9');
	const auto ca = _mm_set1_epi8('a');
	const auto cf = _mm_set1_epi8('f');
	const auto cA = _mm_set1_epi8('A');
	const auto cF = _mm_set1_epi8('F');

	const size_t iter_len = sizeof(c0);

	if(__builtin_expect(len >= iter_len, 1)) {
		auto extra_iters = iter_len - ((intptr_t)(input) & (iter_len-1));

		auto x = _mm_lddqu_si128((__m128i*)(input));

		auto num = _mm_or_si128(_mm_cmpgt_epi8(c0, x),
		                        _mm_cmpgt_epi8(x, c9));

		auto lower = _mm_or_si128(_mm_cmpgt_epi8(ca, x),
		                          _mm_cmpgt_epi8(x, cf));

		auto upper = _mm_or_si128(_mm_cmpgt_epi8(cA, x),
		                          _mm_cmpgt_epi8(x, cF));

		// If num && lower && upper == 0, then all characters are good and testz returns 1
		if(__builtin_expect(!_mm_testz_si128(_mm_and_si128(num, lower), upper) , 0)) {
			return false;
		}

		input += extra_iters;
		len -= extra_iters;
	}

	auto i_ptr = (const __m128i*)input;
	for(size_t i = 0; i < len/iter_len; ++i, ++i_ptr) {
		auto x = _mm_load_si128(i_ptr);

		auto num = _mm_or_si128(_mm_cmpgt_epi8(c0, x),
		                        _mm_cmpgt_epi8(x, c9));

		auto lower = _mm_or_si128(_mm_cmpgt_epi8(ca, x),
		                          _mm_cmpgt_epi8(x, cf));

		auto upper = _mm_or_si128(_mm_cmpgt_epi8(cA, x),
		                          _mm_cmpgt_epi8(x, cF));

		// If num && lower && upper == 0, then all characters are good and testz returns 1
		if(__builtin_expect(!_mm_testz_si128(_mm_and_si128(num, lower), upper) , 0)) {
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
