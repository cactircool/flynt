#include "Lexer.hpp"
#include "Token.hpp"
#include <cctype>
#include <cstring>
#include <istream>
#include <string>

flynt::Lexer::Lexer(std::istream &in) : _buffer(), _in(in), _options(LEFT) {}

// flynt::Token::Type flynt::Lexer::read_known() {
// 	// turn into a giant switch statement but that's less debuggable so not right now
// 	size_t len = (sizeof(Token::_values) / sizeof(*Token::_values));
// 	int candidate = -1;
// 	for (int i = len - 1; i >= 0; --i) {
// 		size_t len = strlen(Token::_values[i]);
// 		std::string buf(len, '\0');
// 		_in.read(buf.data(), len);
// 		if (buf == Token::_values[i] && (candidate == -1 || buf.size() > strlen(Token::_values[candidate])))
// 			candidate = i;
// 		_in.seekg(-len, std::ios::cur);
// 	}
// 	if (candidate == -1)
// 		return Token::Type::UNKNOWN;
// 	_in.seekg(strlen(Token::_values[candidate]), std::ios::cur);
// 	return static_cast<Token::Type>(static_cast<int>(Token::Type::TRUE) + candidate);
// }

flynt::Token::Type flynt::Lexer::read_known() {
    constexpr size_t numValues = sizeof(Token::_values) / sizeof(*Token::_values);

    // Helper to check if a character is part of an identifier
    auto is_ident_char = [](char c) {
        return std::isalnum(static_cast<unsigned char>(c)) || c == '_';
    };

    // Helper to check if a token is alphanumeric (keyword/identifier-like)
    auto is_alphanumeric_token = [](const char* tok) {
        return tok[0] != '\0' && std::isalnum(static_cast<unsigned char>(tok[0]));
    };

    // Save the current position to rewind if no match
    std::streampos start = _in.tellg();

    // Read ahead the maximum possible token length + 1 (for boundary check)
    size_t maxLen = 0;
    for (size_t i = 0; i < numValues; ++i)
        maxLen = std::max(maxLen, strlen(Token::_values[i]));

    std::string lookahead(maxLen + 1, '\0');
    _in.read(lookahead.data(), maxLen + 1);
    size_t bytesRead = _in.gcount();

    // Find the longest matching known token
    int candidate = -1;
    size_t candidateLen = 0;

    for (size_t i = 0; i < numValues; ++i) {
        const char* tok = Token::_values[i];
        size_t tokLen = strlen(tok);

        if (tokLen <= bytesRead && lookahead.compare(0, tokLen, tok) == 0) {
            // For alphanumeric tokens, check word boundary
            if (is_alphanumeric_token(tok)) {
                // Ensure the next character is NOT an identifier character
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
        // no known token matched → rewind and return UNKNOWN
        _in.clear();
        _in.seekg(start);
        return Token::Type::UNKNOWN;
    }

    // Move the stream position to after the matched token
    _in.clear();
    _in.seekg(start + static_cast<std::streamoff>(candidateLen));
    return static_cast<Token::Type>(static_cast<int>(Token::Type::TRUE) + candidate);
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

	if (c == '&' || c == '|') {
		_in.get(c);
		char n;
		if (_in.get(n) && c == n) {
			return Token(c == '&' ? Token::Type::AND : Token::Type::OR);
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
		if (auto tok = read_number(); tok.type() != Token::Type::UNKNOWN)
			return tok;

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

flynt::Token flynt::Lexer::remove_top() {
	static constexpr auto modify = [](unsigned ops, Token &tok) {
		if (ops == LEFT)
			tok.leftify();
		else if (ops == RIGHT)
			tok.rightify();
		else if (ops == BINARY)
			tok.binaryify();
	};

	auto front = _buffer.front();
	_buffer.pop_front();
	modify(_options, front);
	_options = front.follow_options();
	return front;
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
	for (; tok.vague(); tok = dumb_lex())
		_buffer.push_back(tok);

	if (_buffer.empty()) {
		modify(_options, tok);
		_options = tok.follow_options();
		return tok;
	}

	_buffer.push_back(tok);
	for (size_t i = 0; i < _buffer.size(); ++i) {
		unsigned prev_ops = i == 0 ? _options : _buffer[i - 1].follow_options();
		unsigned next_ops = i == _buffer.size() - 1 || _buffer[i + 1].vague() ? 0b111 : _buffer[i + 1].precede_options();
		unsigned constraint = prev_ops & next_ops;
		bool force = !is_power_of_2(constraint);
		Token::Type old_type = _buffer[i].type();
		if (force) // since LR doesn't make sense, the token must support a binary option, so clamping to binary is valid (or does nothing)
			constraint = BINARY;
		modify(constraint, _buffer[i]);
		if (old_type == _buffer[i]._type) {
			constraint = (prev_ops & next_ops) & (LEFT | RIGHT);
			modify(constraint, _buffer[i]);
		}
	}
	return remove_top();
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
