#pragma once

#include "Token.hpp"
#include <deque>
#include <istream>

namespace flynt {

	class Lexer {
		std::deque<Token> _buffer;
		std::istream &_in;
		unsigned _options: 3; // [left][right][binary] 000
		constexpr static unsigned LEFT = 0b100;
		constexpr static unsigned RIGHT = 0b010;
		constexpr static unsigned BINARY = 0b001;

		Token::Type read_known();
		Token read_string();
		Token read_char();
		Token read_number();
		Token remove_top();

		Token dumb_lex();

	public:
		Lexer(std::istream &in);
		~Lexer() = default;

		Lexer(const Lexer &) = delete;
		Lexer &operator=(const Lexer &) = delete;

		Lexer(Lexer &&) = delete;
		Lexer &operator=(Lexer &&) = delete;

		void put_back(const Token &tok);
		Token peek();
		Token lex();
	};

}
