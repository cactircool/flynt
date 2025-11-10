#pragma once

#include "Token.hpp"
#include "TrackingStream.hpp"
#include <deque>
#include <ios>
#include <iosfwd>
#include <istream>

namespace flynt {

	class Lexer {
	public:
		struct Position {
			std::streampos pos;
			std::streamoff line;
			std::streamoff column;

			Position(std::streampos pos, std::streamoff line, std::streamoff column)
				: pos(pos), line(line), column(column) {}
			Position()
				: pos(-1), line(0), column(0) {}
		};

		using FatToken = std::pair<Token, Position>;

	private:
		std::deque<FatToken> _buffer;
		TrackingStream _strm;

		unsigned _options: 3; // [left][right][binary] 000
		constexpr static unsigned LEFT = 0b100;
		constexpr static unsigned RIGHT = 0b010;
		constexpr static unsigned BINARY = 0b001;

		FatToken read_known();
		FatToken read_string();
		FatToken read_char();
		FatToken read_number();
		FatToken remove_top();

		FatToken dumb_lex();

	public:
		Lexer(std::istream &in);
		~Lexer() = default;

		Lexer(const Lexer &) = delete;
		Lexer &operator=(const Lexer &) = delete;

		Lexer(Lexer &&) = delete;
		Lexer &operator=(Lexer &&) = delete;

		FatToken peek();
		FatToken lex();
	};

}
