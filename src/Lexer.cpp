#include "Lexer.hpp"
#include "Token.hpp"
#include <cctype>
#include <cstring>
#include <iosfwd>
#include <istream>
#include <string>
#include <limits>

flynt::Lexer::Lexer(std::istream &in) : _buffer(), _strm(in), _options(LEFT) {}

flynt::Lexer::FatToken flynt::Lexer::read_known() {
	std::streampos pos = _strm.tellg();
	auto line = _strm.line(), col = _strm.column();

	constexpr size_t numValues = sizeof(Token::_values) / sizeof(*Token::_values);
	auto is_ident_char = [](char c) {
		return std::isalnum(static_cast<unsigned char>(c)) || c == '_';
	};

	auto is_alphanumeric_token = [](const char* tok) {
		return tok[0] != '\0' && std::isalnum(static_cast<unsigned char>(tok[0]));
	};

	std::streampos start = _strm.tellg();

	size_t maxLen = 0;
	for (size_t i = 0; i < numValues; ++i)
		maxLen = std::max(maxLen, strlen(Token::_values[i]));

	std::string lookahead(maxLen + 1, '\0');
	_strm.read(lookahead.data(), maxLen + 1);
	size_t bytesRead = _strm.gcount();

	int candidate = -1;
	size_t candidateLen = 0;

	for (size_t i = 0; i < numValues; ++i) {
		const char* tok = Token::_values[i];
		size_t tokLen = strlen(tok);

		if (tokLen <= bytesRead && lookahead.compare(0, tokLen, tok) == 0) {
			if (is_alphanumeric_token(tok)) {
				if (tokLen < bytesRead && is_ident_char(lookahead[tokLen])) {
					continue; // Not a complete token, skip
				}
			}

			if (tokLen > candidateLen) {
				candidate = static_cast<int>(i);
				candidateLen = tokLen;
			}
		}
	}

	if (candidate == -1) {
		_strm.clear();
		_strm.seekg(start);
		return { Token(Token::Type::UNKNOWN), {} };
	}

	_strm.clear();
	_strm.seekg(start + static_cast<std::streamoff>(candidateLen));
	return {
		flynt::Token(static_cast<Token::Type>(static_cast<int>(Token::Type::TRUE) + candidate)),
		{ pos, line, col }
	};
}

flynt::Lexer::FatToken flynt::Lexer::peek() {
	auto fat = lex();
	_buffer.push_back(fat);
	return fat;
}

flynt::Lexer::FatToken flynt::Lexer::dumb_lex() {
	while (true) {
		while (std::isspace(_strm.peek()))
			_strm.get();

		if (_strm.peek() != '/')
			break;

		_strm.get();
		if (_strm.peek() == '/')
			_strm.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
		else if (_strm.peek() == '*') {
			_strm.get();
			bool endFound = false;
			char prev = 0, curr = 0;
			while (_strm.get(curr)) {
				if (prev == '*' && curr == '/') {
					endFound = true;
					break;
				}
				prev = curr;
			}

			if (!endFound) {
				return { Token(Token::Type::UNKNOWN), {} };
			}
		}
		else {
			_strm.clear();
			_strm.unget();
			break;
		}
	}

	char c = _strm.peek();
	if (c == '&' || c == '|') {
		std::streampos pos = _strm.tellg();
		auto line = _strm.line(), col = _strm.column();

		_strm.get(c);
		char n;
		if (_strm.get(n) && c == n) {
			return {
				Token(c == '&' ? Token::Type::AND : Token::Type::OR),
				{ pos, line, col }
			};
		} else {
			_strm.clear();
			_strm.seekg(-2, std::ios::cur);
		}
	}

	if (c == '"')
		return read_string();
	if (c == '\'')
		return read_char();
	if (std::isdigit(c) || c == '.')
		if (auto [tok, pos] = read_number(); tok.type() != Token::Type::UNKNOWN)
			return { tok, pos };

	if (auto [tok, pos] = read_known(); tok.type() != Token::Type::UNKNOWN)
		return { tok, pos };

	if (std::isalpha(c) || c == '_') {
		std::streampos pos = _strm.tellg();
		auto line = _strm.line(), col = _strm.column();
		std::string buf;
		for (; std::isalnum(c) || c == '_'; c = _strm.peek()) {
			buf.push_back(c);
			_strm.get();
		}
		return {
			Token(Token::Type::ID, buf),
			{ pos, line, col }
		};
	}
	return { Token(Token::Type::UNKNOWN), {} };
}

flynt::Lexer::FatToken flynt::Lexer::remove_top() {
	static constexpr auto modify = [](unsigned ops, Token &tok) {
		if (ops == LEFT)
			tok.leftify();
		else if (ops == RIGHT)
			tok.rightify();
		else if (ops == BINARY)
			tok.binaryify();
	};

	auto [front, pos] = _buffer.front();
	_buffer.pop_front();
	modify(_options, front);
	_options = front.follow_options();
	return { front, pos };
}

flynt::Lexer::FatToken flynt::Lexer::lex() {
	static constexpr unsigned LEFT = 0b100, RIGHT = 0b010, BINARY = 0b001;
	static constexpr auto is_power_of_2 = [](unsigned n) {
		return (n > 0) && ((n & (n - 1)) == 0);
	};
	static constexpr auto modify = [](unsigned ops, Token &tok) {
		if (ops == LEFT)
			tok.leftify();
		else if (ops == RIGHT)
			tok.rightify();
		else if (ops == BINARY)
			tok.binaryify();
	};

	if (!_buffer.empty())
		return remove_top();

	auto fat = dumb_lex();
	for (; fat.first.vague(); fat = dumb_lex())
		_buffer.push_back(fat);

	if (_buffer.empty()) {
		modify(_options, fat.first);
		_options = fat.first.follow_options();
		return fat;
	}

	_buffer.push_back(fat);
	for (size_t i = 0; i < _buffer.size(); ++i) {
		unsigned prev_ops = i == 0 ? _options : _buffer[i - 1].first.follow_options();
		unsigned next_ops = i == _buffer.size() - 1 || _buffer[i + 1].first.vague() ? 0b111 : _buffer[i + 1].first.precede_options();
		unsigned constraint = prev_ops & next_ops;
		bool force = !is_power_of_2(constraint);
		Token::Type old_type = _buffer[i].first.type();
		if (force) // since LR doesn't make sense, the token must support a binary option, so clamping to binary is valid (or does nothing)
			constraint = BINARY;
		modify(constraint, _buffer[i].first);
		if (old_type == _buffer[i].first.type()) {
			constraint = (prev_ops & next_ops) & (LEFT | RIGHT);
			modify(constraint, _buffer[i].first);
		}
	}
	return remove_top();
}

flynt::Lexer::FatToken flynt::Lexer::read_string() {
	std::streampos pos = _strm.tellg();
	auto line = _strm.line(), col = _strm.column();

	_strm.get(); // consume opening quote
	std::string value;
	char c;

	while (_strm.get(c)) {
		if (c == '\\') {
			// Backslash - just include it and skip the next character
			value.push_back(c);
			if (_strm.get(c)) {
				value.push_back(c);
			} else {
				// Unexpected end of input after backslash
				return {
					Token(Token::Type::UNKNOWN),
					{ pos, line, col }
				};
			}
		} else if (c == '"') {
			// End of string
			return {
				Token(Token::Type::STR, value),
				{ pos, line, col }
			};
		} else {
			value.push_back(c);
		}
	}

	// Unterminated string
	return {
		Token(Token::Type::UNKNOWN),
		{ pos, line, col }
	};
}

flynt::Lexer::FatToken flynt::Lexer::read_char() {
	std::streampos pos = _strm.tellg();
	auto line = _strm.line(), col = _strm.column();

	_strm.get(); // consume opening single quote
	std::string value;
	char c;

	while (_strm.get(c)) {
		if (c == '\\') {
			// Backslash - just include it and skip the next character
			value.push_back(c);
			if (_strm.get(c)) {
				value.push_back(c);
			} else {
				// Unexpected end of input after backslash
				return {
					Token(Token::Type::UNKNOWN),
					{ pos, line, col }
				};
			}
		} else if (c == '\'') {
			// End of char literal
			if (value.empty()) {
				// Empty char literal
				return {
					Token(Token::Type::UNKNOWN),
					{ pos, line, col }
				};
			}
			return {
				Token(Token::Type::CHAR, value),
				{ pos, line, col }
			};
		} else {
			value.push_back(c);
		}
	}

	// Unterminated char literal
	return {
		Token(Token::Type::UNKNOWN),
		{ pos, line, col }
	};
}

flynt::Lexer::FatToken flynt::Lexer::read_number() {
	std::streampos pos = _strm.tellg();
	auto line = _strm.line(), col = _strm.column();

	std::string value;
	char c = _strm.peek();
	bool has_dot = false;
	bool has_exponent = false;

	// Handle leading decimal point (e.g., .5)
	if (c == '.') {
		value.push_back(c);
		_strm.get();
		has_dot = true;
		c = _strm.peek();

		// Just a dot, not a number
		if (!std::isdigit(c)) {
			// Put the dot back - it's probably an operator
			for (auto it = value.rbegin(); it != value.rend(); ++it) {
				_strm.putback(*it);
			}
			return {
				Token(Token::Type::UNKNOWN),
				{ pos, line, col }
			};
		}
	}

	// Read integer part or decimal digits
	while (std::isdigit(c)) {
		value.push_back(c);
		_strm.get();
		c = _strm.peek();
	}

	// Check for decimal point
	if (c == '.' && !has_dot) {
		// Peek ahead to see if there's a digit or if it's range operator (..)
		value.push_back(c);
		_strm.get();
		c = _strm.peek();

		if (c == '.') {
			// It's a range operator, put the dot back
			_strm.putback('.');
			value.pop_back();
			return {
				Token(Token::Type::INT, value),
				{ pos, line, col },
			};
		}

		has_dot = true;

		// Read fractional part
		while (std::isdigit(c)) {
			value.push_back(c);
			_strm.get();
			c = _strm.peek();
		}
	}

	// Check for exponent (e or E)
	if (c == 'e' || c == 'E') {
		value.push_back(c);
		_strm.get();
		c = _strm.peek();
		has_exponent = true;

		// Optional sign
		if (c == '+' || c == '-') {
			value.push_back(c);
			_strm.get();
			c = _strm.peek();
		}

		// Exponent digits
		if (!std::isdigit(c)) {
			// Invalid number format
			return {
				Token(Token::Type::UNKNOWN),
				{ pos, line, col }
			};
		}

		while (std::isdigit(c)) {
			value.push_back(c);
			_strm.get();
			c = _strm.peek();
		}
	}

	if (has_dot || has_exponent) {
		return {
			Token(Token::Type::FLOAT, value),
			{ pos, line, col }
		};
	}

	return {
		Token(Token::Type::INT, value),
		{ pos, line, col }
	};
}
