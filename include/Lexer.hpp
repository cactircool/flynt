#pragma once

#include "Token.hpp"
#include <deque>
#include <iosfwd>
#include <istream>
#include <tuple>

namespace flynt {

	class Lexer {
		struct FatToken {
			Token token;
			std::streampos pos;
			size_t line, character;
		};
		std::deque<FatToken> _buffer;
		std::istream &_in;
		FatToken _last;

		size_t _line_ctr, _char_ctr;

		unsigned _options: 3; // [left][right][binary] 000
		constexpr static unsigned LEFT = 0b100;
		constexpr static unsigned RIGHT = 0b010;
		constexpr static unsigned BINARY = 0b001;

		FatToken read_known();
		FatToken read_string();
		FatToken read_char();
		FatToken read_number();
		Token remove_top();

		FatToken dumb_lex();

		char get();
		std::istream &get(char &c);

	public:
		Lexer(std::istream &in);
		~Lexer() = default;

		Lexer(const Lexer &) = delete;
		Lexer &operator=(const Lexer &) = delete;

		Lexer(Lexer &&) = delete;
		Lexer &operator=(Lexer &&) = delete;

		Token peek();
		Token lex();

		FatToken last() const {
			return _last;
		}
		std::tuple<std::streampos, size_t, size_t> last_pos() const {
			return { _last.pos, _last.line, _last.character };
		}
	};

}
