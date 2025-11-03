#include "Lexer.hpp"
#include "Token.hpp"
#include <cctype>
#include <cstring>
#include <istream>
#include <string>

flynt::Lexer::Lexer(std::istream &in) : _buffer(), _in(in), _last_lexed_token(Token::Type::UNKNOWN) {}

flynt::Token::Type flynt::Lexer::read_known() {
	// turn into a giant switch statement but that's less debuggable so not right now
	for (size_t i = 0; i < (sizeof(Token::_values) / sizeof(*Token::_values)); ++i) {
		size_t len = strlen(Token::_values[i]);
		std::string buf(len, '\0');
		_in.read(buf.data(), len);
		if (buf == Token::_values[i])
			return static_cast<Token::Type>(static_cast<int>(Token::Type::TRUE) + i);
		_in.seekg(-len, std::ios::cur);
	}
	return Token::Type::UNKNOWN;
}

flynt::Token flynt::Lexer::peek() {
	auto tok = lex();
	put_back(tok);
	return tok;
}

void flynt::Lexer::put_back(const Token &tok) {
	_buffer.push_front(tok);
}

flynt::Token flynt::Lexer::dumb_lex() {
	char c;
	for (c = _in.peek(); std::isspace(c); c = _in.peek())
		_in.get();

	if (c == '"')
		return read_string();
	if (c == '\'')
		return read_char();
	if (std::isdigit(c) || c == '.')
		return read_number();

	if (auto type = read_known(); type != Token::Type::UNKNOWN)
		return Token(type);

	if (std::isalpha(c) || c == '_') {
		std::string buf;
		for (; std::isalnum(c) || c == '_'; c = _in.peek()) {
			buf.push_back(c);
			_in.get();
		}
		return Token(Token::Type::ID, buf);
	}
	return Token(Token::Type::UNKNOWN);
}

flynt::Token flynt::Lexer::lex() {
	// Return buffered tokens first
	if (!_buffer.empty()) {
		auto front = _buffer.front();
		_buffer.pop_front();
		_last_lexed_token = front;
		return front;
	}

	auto tok = dumb_lex();

	// If not vague, return immediately - no disambiguation needed
	if (!tok.vague()) {
		_last_lexed_token = tok;
		return tok;
	}

	// Read all consecutive vague tokens into buffer
	while (tok.vague()) {
		_buffer.push_back(tok);
		tok = dumb_lex();
	}

	// Now tok is non-vague (or UNKNOWN/EOF)
	// Disambiguate the vague tokens based on what follows

	for (size_t i = 0; i < _buffer.size(); ++i) {
		auto &vague_tok = _buffer[i];

		// Determine what follows this vague token
		Token following = (i + 1 < _buffer.size()) ? _buffer[i + 1] : tok;

		if (following.follows_right()) {
			// Next token can follow a right unary operator
			vague_tok.rightify();
		} else if (following.follows_left_binary()) {
			// Next token can follow a left unary or binary operator
			// Need to check what precedes to decide

			if (i == 0) {
				// First token in buffer - check the last lexed token
				if (_last_lexed_token.type() == Token::Type::UNKNOWN) {
					// Beginning of input - must be left unary
					vague_tok.leftify();
				} else if (_last_lexed_token.precedes_left()) {
					vague_tok.leftify();
				} else if (_last_lexed_token.precedes_right_binary()) {
					vague_tok.binaryify();
				} else {
					// Default to left unary if unclear
					vague_tok.leftify();
				}
			} else {
				// Check the previous token we just classified
				auto &prev = _buffer[i - 1];
				if (prev.precedes_left()) {
					vague_tok.leftify();
				} else if (prev.precedes_right_binary()) {
					vague_tok.binaryify();
				} else {
					// Default to left unary
					vague_tok.leftify();
				}
			}
		} else {
			// Unclear context - default to left unary
			vague_tok.leftify();
		}
	}

	// Add the non-vague token to buffer for next call
	_buffer.push_back(tok);

	// Return the first disambiguated token
	auto front = _buffer.front();
	_buffer.pop_front();
	_last_lexed_token = front;
	return front;
}

flynt::Token flynt::Lexer::read_string() {
	_in.get(); // consume opening quote
	std::string value;
	char c;

	while (_in.get(c)) {
		if (c == '\\') {
			// Backslash - just include it and skip the next character
			value.push_back(c);
			if (_in.get(c)) {
				value.push_back(c);
			} else {
				// Unexpected end of input after backslash
				return Token(Token::Type::UNKNOWN);
			}
		} else if (c == '"') {
			// End of string
			return Token(Token::Type::STR, value);
		} else {
			value.push_back(c);
		}
	}

	// Unterminated string
	return Token(Token::Type::UNKNOWN);
}

flynt::Token flynt::Lexer::read_char() {
	_in.get(); // consume opening single quote
	std::string value;
	char c;

	while (_in.get(c)) {
		if (c == '\\') {
			// Backslash - just include it and skip the next character
			value.push_back(c);
			if (_in.get(c)) {
				value.push_back(c);
			} else {
				// Unexpected end of input after backslash
				return Token(Token::Type::UNKNOWN);
			}
		} else if (c == '\'') {
			// End of char literal
			if (value.empty()) {
				// Empty char literal
				return Token(Token::Type::UNKNOWN);
			}
			return Token(Token::Type::CHAR, value);
		} else {
			value.push_back(c);
		}
	}

	// Unterminated char literal
	return Token(Token::Type::UNKNOWN);
}

flynt::Token flynt::Lexer::read_number() {
	std::string value;
	char c = _in.peek();
	bool has_dot = false;
	bool has_exponent = false;

	// Handle leading decimal point (e.g., .5)
	if (c == '.') {
		value.push_back(c);
		_in.get();
		has_dot = true;
		c = _in.peek();

		// Just a dot, not a number
		if (!std::isdigit(c)) {
			// Put the dot back - it's probably an operator
			for (auto it = value.rbegin(); it != value.rend(); ++it) {
				_in.putback(*it);
			}
			return Token(Token::Type::UNKNOWN);
		}
	}

	// Read integer part or decimal digits
	while (std::isdigit(c)) {
		value.push_back(c);
		_in.get();
		c = _in.peek();
	}

	// Check for decimal point
	if (c == '.' && !has_dot) {
		// Peek ahead to see if there's a digit or if it's range operator (..)
		value.push_back(c);
		_in.get();
		c = _in.peek();

		if (c == '.') {
			// It's a range operator, put the dot back
			_in.putback('.');
			value.pop_back();
			return Token(Token::Type::INT, value);
		}

		has_dot = true;

		// Read fractional part
		while (std::isdigit(c)) {
			value.push_back(c);
			_in.get();
			c = _in.peek();
		}
	}

	// Check for exponent (e or E)
	if (c == 'e' || c == 'E') {
		value.push_back(c);
		_in.get();
		c = _in.peek();
		has_exponent = true;

		// Optional sign
		if (c == '+' || c == '-') {
			value.push_back(c);
			_in.get();
			c = _in.peek();
		}

		// Exponent digits
		if (!std::isdigit(c)) {
			// Invalid number format
			return Token(Token::Type::UNKNOWN);
		}

		while (std::isdigit(c)) {
			value.push_back(c);
			_in.get();
			c = _in.peek();
		}
	}

	if (has_dot || has_exponent) {
		return Token(Token::Type::FLOAT, value);
	}

	return Token(Token::Type::INT, value);
}
