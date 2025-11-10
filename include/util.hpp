#pragma once

#include <concepts>
#include <stdexcept>
#include <iostream>

namespace flynt {

	template <typename T>
	concept Indexable = requires(T container, std::size_t index) {
		{ container[index] } -> std::common_reference_with<decltype(container[index])>;
	};

	template <typename T>
	concept ForwardLoopable = requires(T container) {
		{ container.begin() };
		{ container.end() };
	};

	template <typename T>
	concept BackwardLoopable = requires(T container) {
		{ container.rbegin() };
		{ container.rend() };
	};

	template <typename T>
	concept Loopable = ForwardLoopable<T> && BackwardLoopable<T>;

	template <typename T>
	concept IndexableAndLoopable = Indexable<T> && Loopable<T>;

#define fassert(cond) \
	do { \
		std::cerr << "Assertion failed: " << #cond << '\n'; \
		std::cerr << "File: " << __FILE__ << '\n'; \
		std::cerr << "Function: " << __FUNCTION__ << '\n'; \
		std::cerr << "Line: " << __LINE__ << '\n'; \
		std::terminate(); \
	} while (false); \

}
