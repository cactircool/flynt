#include "Lexer.hpp"
#include "Token.hpp"
#include <cctype>
#include <cstring>
#include <iosfwd>
#include <istream>
#include <string>
#include <limits>

flynt::Lexer::Lexer(std::istream &in) : _buffer(), _in(in), _options(LEFT) {}

flynt::Token::Type flynt::Lexer::read_known() {
	constexpr size_t numValues = sizeof(Token::_values) / sizeof(*Token::_values);
	auto is_ident_char = [](char c) {
		return std::isalnum(static_cast<unsigned char>(c)) || c == '_';
	};

	auto is_alphanumeric_token = [](const char* tok) {
		return tok[0] != '\0' && std::isalnum(static_cast<unsigned char>(tok[0]));
	};

	std::streampos start = _in.tellg();

	size_t maxLen = 0;
	for (size_t i = 0; i < numValues; ++i)
		maxLen = std::max(maxLen, strlen(Token::_values[i]));

	std::string lookahead(maxLen + 1, '\0');
	_in.read(lookahead.data(), maxLen + 1);
	size_t bytesRead = _in.gcount();

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
		_in.clear();
		_in.seekg(start);
		return Token::Type::UNKNOWN;
	}

	_in.clear();
	_in.seekg(start + static_cast<std::streamoff>(candidateLen));
	return static_cast<Token::Type>(static_cast<int>(Token::Type::TRUE) + candidate);
}

flynt::Token flynt::Lexer::peek() {
	std::streampos pos = _in.tellg();
	size_t line = _line_ctr, c = _char_ctr;
	auto tok = lex();
	_buffer.push_back(FatToken {
		.token = tok,
		.pos = pos,
		.line = line,
		.character = c,
	});
	return tok;
}

char flynt::Lexer::get() {
	char c = _in.get();
	if (c == '\n') {
		++_line_ctr;
		_char_ctr = 1;
	} else {
		++_char_ctr;
	}
	return c;
}

std::istream &flynt::Lexer::get(char &c) {
	auto &strm = _in.get(c);
	if (c == '\n') {
		++_line_ctr;
		_char_ctr = 1;
	} else {
		++_char_ctr;
	}
	return strm;
}

flynt::Lexer::FatToken flynt::Lexer::dumb_lex() {
	while (true) {
		while (std::isspace(_in.peek()))
			get();

		if (_in.peek() != '/')
			break;

		get();
		if (_in.peek() == '/') {
			_in.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
			++_line_ctr;
			_char_ctr = 1;
		}
		else if (_in.peek() == '*') {
			get();
			bool endFound = false;
			char prev = 0, curr = 0;
			while (_in.get(curr)) {
				if (prev == '*' && curr == '/') {
					endFound = true;
					break;
				}
				prev = curr;
			}

			if (!endFound) {
				return FatToken {
					.token = Token(Token::Type::UNKNOWN),
					.pos = -1,
					.line = 0,
					.character = 0,
				};
			}
		}
		else {
			_in.clear();
			_in.unget();
			break;
		}
	}

	char c = _in.peek();
	if (c == '&' || c == '|') {
		std::streampos pos = _in.tellg();
		size_t lc = _line_ctr, cc = _char_ctr;
		get(c);
		char n;
		if (_in.get(n) && c == n) {
			return FatToken {
				.token = Token(c == '&' ? Token::Type::AND : Token::Type::OR),
				.pos = pos,
				.line = lc,
				.character = cc,
			};
		} else {
			_in.clear();
			_in.seekg(-2, std::ios::cur);
		}
	}

	if (c == '"')
		return read_string();
	if (c == '\'')
		return read_char();
	if (std::isdigit(c) || c == '.')
		if (auto tok = read_number(); tok.token.type() != Token::Type::UNKNOWN)
			return tok;

	if (auto tok = read_known(); tok.token.type() != Token::Type::UNKNOWN)
		return tok;

	if (std::isalpha(c) || c == '_') {
		std::streampos pos = _in.tellg();
		size_t lc = _line_ctr, cc = _char_ctr;
		std::string buf;
		for (; std::isalnum(c) || c == '_'; c = _in.peek()) {
			buf.push_back(c);
			get();
		}
		return FatToken {
			.token = Token(Token::Type::ID, buf),
			.pos = pos,
			.line = lc,
			.character = cc,
		};
	}
	return FatToken {
		.token = Token(Token::Type::UNKNOWN),
		.pos = -1,
		.line = 0,
		.character = 0,
	};
}

flynt::Token flynt::Lexer::remove_top() {
	static constexpr auto modify = [](unsigned ops, Token &tok) {
		if (ops == LEFT)
			tok.leftify();
		else if (ops == RIGHT)
			tok.rightify();
		else if (ops == BINARY)
			tok.binaryify();
	};

	_last = _buffer.front();
	_buffer.pop_front();
	modify(_options, _last.token);
	_options = _last.token.follow_options();
	return _last.token;
}

flynt::Token flynt::Lexer::lex() {
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

	auto tok = dumb_lex();
	for (; tok.token.vague(); tok = dumb_lex())
		_buffer.push_back(tok);

	if (_buffer.empty()) {
		modify(_options, tok.token);
		_options = tok.token.follow_options();
		_last = tok;
		return tok.token;
	}

	_buffer.push_back(tok);
	for (size_t i = 0; i < _buffer.size(); ++i) {
		unsigned prev_ops = i == 0 ? _options : _buffer[i - 1].follow_options();
		unsigned next_ops = i == _buffer.size() - 1 || _buffer[i + 1].vague() ? 0b111 : _buffer[i + 1].precede_options();
		unsigned constraint = prev_ops & next_ops;
		bool force = !is_power_of_2(constraint);
		Token::Type old_type = _buffer[i].token.type();
		if (force) // since LR doesn't make sense, the token must support a binary option, so clamping to binary is valid (or does nothing)
			constraint = BINARY;
		modify(constraint, _buffer[i].token);
		if (old_type == _buffer[i].token.type()) {
			constraint = (prev_ops & next_ops) & (LEFT | RIGHT);
			modify(constraint, _buffer[i].token);
		}
	}
	return remove_top();
}

flynt::Lexer::FatToken flynt::Lexer::read_string() {
	std::streampos pos = _in.tellg();
	size_t lc = _line_ctr, cc = _char_ctr;

	get(); // consume opening quote
	std::string value;
	char c;

	while (get(c)) {
		if (c == '\\') {
			// Backslash - just include it and skip the next character
			value.push_back(c);
			if (get(c)) {
				value.push_back(c);
			} else {
				// Unexpected end of input after backslash
				return FatToken {
					.token = Token(Token::Type::UNKNOWN),
					.pos = pos,
					.line = lc,
					.character = cc,
				};
			}
		} else if (c == '"') {
			// End of string
			return FatToken {
				.token = Token(Token::Type::STR, value),
				.pos = pos,
				.line = lc,
				.character = cc,
			};
		} else {
			value.push_back(c);
		}
	}

	// Unterminated string
	return FatToken {
		.token = Token(Token::Type::UNKNOWN),
		.pos = pos,
		.line = lc,
		.character = cc,
	};
}

flynt::Lexer::FatToken flynt::Lexer::read_char() {
	std::streampos pos = _in.tellg();
	size_t lc = _line_ctr, cc = _char_ctr;

	get(); // consume opening single quote
	std::string value;
	char c;

	while (get(c)) {
		if (c == '\\') {
			// Backslash - just include it and skip the next character
			value.push_back(c);
			if (get(c)) {
				value.push_back(c);
			} else {
				// Unexpected end of input after backslash
				return FatToken {
					.token = Token(Token::Type::UNKNOWN),
					.pos = pos,
					.line = lc,
					.character = cc,
				};
			}
		} else if (c == '\'') {
			// End of char literal
			if (value.empty()) {
				// Empty char literal
				return FatToken {
					.token = Token(Token::Type::UNKNOWN),
					.pos = pos,
					.line = lc,
					.character = cc,
				};
			}
			return FatToken {
				.token = Token(Token::Type::CHAR, value),
				.pos = pos,
				.line = lc,
				.character = cc,
			};
		} else {
			value.push_back(c);
		}
	}

	// Unterminated char literal
	return FatToken {
		.token = Token(Token::Type::UNKNOWN),
		.pos = pos,
		.line = lc,
		.character = cc,
	};
}

flynt::Lexer::FatToken flynt::Lexer::read_number() {
	std::streampos pos = _in.tellg();
	size_t lc = _line_ctr, cc = _char_ctr;

	std::string value;
	char c = _in.peek();
	bool has_dot = false;
	bool has_exponent = false;

	// Handle leading decimal point (e.g., .5)
	if (c == '.') {
		value.push_back(c);
		get();
		has_dot = true;
		c = _in.peek();

		// Just a dot, not a number
		if (!std::isdigit(c)) {
			// Put the dot back - it's probably an operator
			for (auto it = value.rbegin(); it != value.rend(); ++it) {
				_in.putback(*it);
			}
			return FatToken {
				.token = Token(Token::Type::UNKNOWN),
				.pos = pos,
				.line = lc,
				.character = cc,
			};
		}
	}

	// Read integer part or decimal digits
	while (std::isdigit(c)) {
		value.push_back(c);
		get();
		c = _in.peek();
	}

	// Check for decimal point
	if (c == '.' && !has_dot) {
		// Peek ahead to see if there's a digit or if it's range operator (..)
		value.push_back(c);
		get();
		c = _in.peek();

		if (c == '.') {
			// It's a range operator, put the dot back
			_in.putback('.');
			value.pop_back();
			return FatToken {
				.token = Token(Token::Type::INT, value),
				.pos = pos,
				.line = lc,
				.character = cc,
			};
		}

		has_dot = true;

		// Read fractional part
		while (std::isdigit(c)) {
			value.push_back(c);
			get();
			c = _in.peek();
		}
	}

	// Check for exponent (e or E)
	if (c == 'e' || c == 'E') {
		value.push_back(c);
		get();
		c = _in.peek();
		has_exponent = true;

		// Optional sign
		if (c == '+' || c == '-') {
			value.push_back(c);
			get();
			c = _in.peek();
		}

		// Exponent digits
		if (!std::isdigit(c)) {
			// Invalid number format
			return FatToken {
				.token = Token(Token::Type::UNKNOWN),
				.pos = pos,
				.line = lc,
				.character = cc,
			};
		}

		while (std::isdigit(c)) {
			value.push_back(c);
			get();
			c = _in.peek();
		}
	}

	if (has_dot || has_exponent) {
		return FatToken {
			.token = Token(Token::Type::FLOAT, value),
			.pos = pos,
			.line = lc,
			.character = cc,
		};
	}

	return FatToken {
		.token = Token(Token::Type::INT, value),
		.pos = pos,
		.line = lc,
		.character = cc,
	};
}
