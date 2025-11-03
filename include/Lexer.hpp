#pragma once

#include "Token.hpp"
#include <deque>
#include <istream>

namespace flynt {

	class Lexer {
		std::deque<Token> _buffer;
		std::istream &_in;
		Token _last_lexed_token;

		Token::Type read_known();
		Token read_string();
		Token read_char();
		Token read_number();

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
