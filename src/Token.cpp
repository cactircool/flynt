#include "Token.hpp"

flynt::Token::Token(Token::Type type) : _type(type) {}
flynt::Token::Token(Token::Type type, std::string value) : _type(type), _value(value) {}

const char *flynt::Token::value() const {
	switch (_type) {
		case Type::UNKNOWN:
		case Type::ID:
		case Type::INT:
		case Type::FLOAT:
		case Type::CHAR:
		case Type::STR:
			return _value.c_str();
		default:
			return Token::_values[static_cast<int>(_type) - static_cast<int>(Token::Type::TRUE)];
	}
}

flynt::Token::Type flynt::Token::type() const {
	return _type;
}

bool flynt::Token::symbol() const {
	switch (_type) {
		case Type::O_PAREN:
		case Type::C_PAREN:
		case Type::O_BRACE:
		case Type::C_BRACE:
		case Type::O_BRACKET:
		case Type::C_BRACKET:
		case Type::SEMI_COLON:
		case Type::COLON:
		case Type::ARROW:
		case Type::DOUBLE_ARROW:
			return true;
		default:
			return false;
	}
}
bool flynt::Token::keyword() const {
	switch (_type) {
		case Token::Type::LET:
		case Token::Type::FN:
		case Token::Type::TEMPL:
		case Token::Type::IDEA:
		case Token::Type::TYPE:
		case Token::Type::TRAIT:
		case Token::Type::PRIV:
		case Token::Type::PUB:
		case Token::Type::STAT:
		case Token::Type::FIN:
		case Token::Type::ABSTR:
		case Token::Type::OPER:
		case Token::Type::IMPL:
		case Token::Type::IF:
		case Token::Type::ELSE:
		case Token::Type::FOR:
		case Token::Type::UNTIL:
		case Token::Type::MATCH:
		case Token::Type::ENUM:
		case Token::Type::SPACE:
		case Token::Type::USE:
		case Token::Type::ASM:
			return true;
		default:
			return false;
	}
}
bool flynt::Token::oper() const {
	return unary() || binary();
}
bool flynt::Token::right() const {
	switch (_type) {
		case Token::Type::R_INC:
		case Token::Type::R_DEC:
		case Token::Type::R_PLUS:
		case Token::Type::R_MINUS:
		case Token::Type::R_NOT:
		case Token::Type::R_Q_MARK:
		case Token::Type::R_B_NOT:
		case Token::Type::R_DEREF:
		case Token::Type::R_REF:
		case Token::Type::R_DOLLAR:
			return true;
		default:
			return false;
	}
}
bool flynt::Token::left() const {
	switch (_type) {
		case Token::Type::L_INC:
		case Token::Type::L_DEC:
		case Token::Type::L_PLUS:
		case Token::Type::L_MINUS:
		case Token::Type::L_NOT:
		case Token::Type::L_Q_MARK:
		case Token::Type::L_B_NOT:
		case Token::Type::L_DEREF:
		case Token::Type::L_REF:
		case Token::Type::L_DOLLAR:
		case Token::Type::ALLOC:
		case Token::Type::CLEAN:
			return true;
		default:
			return false;
	}
}
bool flynt::Token::unary() const {
	return right() || left();
}
bool flynt::Token::binary() const {
	switch (_type) {
		case Type::SCOPE:
		case Type::DOT:
		case Type::MUL:
		case Type::DIV:
		case Type::MOD:

		case Type::ADD:
		case Type::SUB:

		case Type::LS:
		case Type::RS:

		case Type::LT:
		case Type::LE:
		case Type::GT:
		case Type::GE:
		case Type::IN:
		case Type::IS:
		case Type::AS:
		case Type::R_SPREAD:

		case Type::RANGE:
		case Type::RANGE_EQ:

		case Type::EE:
		case Type::NE:

		case Type::B_AND:

		case Type::B_XOR:

		case Type::B_OR:

		case Type::AND:

		case Type::OR:

		case Type::EQ:
		case Type::ADD_EQ:
		case Type::SUB_EQ:
		case Type::MUL_EQ:
		case Type::DIV_EQ:
		case Type::MOD_EQ:
		case Type::LS_EQ:
		case Type::RS_EQ:
		case Type::B_AND_EQ:
		case Type::B_XOR_EQ:
		case Type::B_OR_EQ:
		case Type::L_SPREAD:
			return true;
		default:
			return false;
	}
}
int flynt::Token::precedence() const {
	switch (_type) {
		case Type::SCOPE:
			return 0;
		case Type::R_INC:
		case Type::R_DEC:
		case Type::R_PLUS:
		case Type::R_MINUS:
		case Type::R_NOT:
		case Type::R_Q_MARK:
		case Type::R_B_NOT:
		case Type::R_DEREF:
		case Type::R_REF:
		case Type::R_DOLLAR:
		case Type::DOT:
			return 1;
		case Type::L_INC:
		case Type::L_DEC:
		case Type::L_PLUS:
		case Type::L_MINUS:
		case Type::L_NOT:
		case Type::L_Q_MARK:
		case Type::L_B_NOT:
		case Type::L_DEREF:
		case Type::L_REF:
		case Type::L_DOLLAR:
		case Type::ALLOC:
		case Type::CLEAN:
			return 2;
		case Type::MUL:
		case Type::DIV:
		case Type::MOD:
			return 3;
		case Type::ADD:
		case Type::SUB:
			return 4;
		case Type::LS:
		case Type::RS:
			return 5;
		case Type::LT:
		case Type::LE:
		case Type::GT:
		case Type::GE:
		case Type::IN:
		case Type::IS:
		case Type::AS:
		case Type::R_SPREAD:
			return 6;
		case Type::RANGE:
		case Type::RANGE_EQ:
			return 7;
		case Type::EE:
		case Type::NE:
			return 8;
		case Type::B_AND:
			return 9;
		case Type::B_XOR:
			return 10;
		case Type::B_OR:
			return 11;
		case Type::AND:
			return 12;
		case Type::OR:
			return 13;
		case Type::EQ:
		case Type::ADD_EQ:
		case Type::SUB_EQ:
		case Type::MUL_EQ:
		case Type::DIV_EQ:
		case Type::MOD_EQ:
		case Type::LS_EQ:
		case Type::RS_EQ:
		case Type::B_AND_EQ:
		case Type::B_XOR_EQ:
		case Type::B_OR_EQ:
		case Type::L_SPREAD:
			return 14;
		default:
			return -1;
	}
}

bool flynt::Token::id() const {
	return _type == Type::ID;
}
bool flynt::Token::literal() const {
	switch (_type) {
		case Type::INT:
		case Type::FLOAT:
		case Type::CHAR:
		case Type::STR:
		case Type::TRUE:
		case Type::FALSE:
			return true;
		default:
			return false;
	}
}

bool flynt::Token::vague() const {
	switch (_type) {
		case Type::R_INC:
		case Type::R_DEC:
		case Type::R_PLUS:
		case Type::R_MINUS:
		case Type::R_NOT:
		case Type::R_Q_MARK:
		case Type::R_B_NOT:
		case Type::R_DEREF:
		case Type::R_REF:
		case Type::R_DOLLAR:
		case Type::R_SPREAD:

		case Type::L_INC:
		case Type::L_DEC:
		case Type::L_PLUS:
		case Type::L_MINUS:
		case Type::L_NOT:
		case Type::L_Q_MARK:
		case Type::L_B_NOT:
		case Type::L_DEREF:
		case Type::L_REF:
		case Type::L_DOLLAR:
		case Type::L_SPREAD:

		case Type::MUL:
		case Type::ADD:
		case Type::SUB:
		case Type::B_AND:
			return true;
		default:
			return false;
	}
}

void flynt::Token::rightify() {
	switch (_type) {
		case Type::L_INC:
			_type = Type::R_INC;
			return;
		case Type::L_DEC:
			_type = Type::R_DEC;
			return;
		case Type::L_PLUS:
			_type = Type::R_PLUS;
			return;
		case Type::L_MINUS:
			_type = Type::R_MINUS;
			return;
		case Type::L_NOT:
			_type = Type::R_NOT;
			return;
		case Type::L_Q_MARK:
			_type = Type::R_Q_MARK;
			return;
		case Type::L_B_NOT:
			_type = Type::R_B_NOT;
			return;
		case Type::L_DEREF:
			_type = Type::R_DEREF;
			return;
		case Type::L_REF:
			_type = Type::R_REF;
			return;
		case Type::L_DOLLAR:
			_type = Type::R_DOLLAR;
			return;
		case Type::L_SPREAD:
			_type = Type::R_SPREAD;
			return;

		case Type::MUL:
			_type = Type::R_DEREF;
			return;
		case Type::ADD:
			_type = Type::R_PLUS;
			return;
		case Type::SUB:
			_type = Type::R_MINUS;
			return;
		case Type::B_AND:
			_type = Type::R_REF;
			return;
		default:
			return;
	}
}

void flynt::Token::leftify() {
	switch (_type) {
		case Type::R_INC:
			_type = Type::L_INC;
			return;
		case Type::R_DEC:
			_type = Type::L_DEC;
			return;
		case Type::R_PLUS:
			_type = Type::L_PLUS;
			return;
		case Type::R_MINUS:
			_type = Type::L_MINUS;
			return;
		case Type::R_NOT:
			_type = Type::L_NOT;
			return;
		case Type::R_Q_MARK:
			_type = Type::L_Q_MARK;
			return;
		case Type::R_B_NOT:
			_type = Type::L_B_NOT;
			return;
		case Type::R_DEREF:
			_type = Type::L_DEREF;
			return;
		case Type::R_REF:
			_type = Type::L_REF;
			return;
		case Type::R_DOLLAR:
			_type = Type::L_DOLLAR;
			return;
		case Type::R_SPREAD:
			_type = Type::L_SPREAD;
			return;

		case Type::MUL:
			_type = Type::L_DEREF;
			return;
		case Type::ADD:
			_type = Type::L_PLUS;
			return;
		case Type::SUB:
			_type = Type::L_MINUS;
			return;
		case Type::B_AND:
			_type = Type::L_REF;
			return;
		default:
			return;
	}
}

void flynt::Token::binaryify() {
	switch (_type) {
		case Type::L_PLUS:
		case Type::R_PLUS:
			_type = Type::ADD;
			return;
		case Type::L_MINUS:
		case Type::R_MINUS:
			_type = Type::SUB;
			return;
		case Type::L_DEREF:
		case Type::R_DEREF:
			_type = Type::MUL;
			return;
		case Type::L_REF:
		case Type::R_REF:
			_type = Type::B_AND;
			return;

		default:
			return;
	}
}

bool flynt::Token::open() const {
	switch (_type) {
		case Type::O_BRACE:
		case Type::O_BRACKET:
		case Type::O_PAREN:
			return true;
		default:
			return false;
	}
}

bool flynt::Token::closed() const {
	switch (_type) {
		case Type::C_BRACE:
		case Type::C_BRACKET:
		case Type::C_PAREN:
			return true;
		default:
			return false;
	}
}

int flynt::Token::scope() const {
	switch (_type) {
		case Type::O_BRACE:
		case Type::O_BRACKET:
		case Type::O_PAREN:
			return 1;
		case Type::C_BRACE:
		case Type::C_BRACKET:
		case Type::C_PAREN:
			return -1;
		default:
			return 0;
	}
}

unsigned flynt::Token::precede_options() const {
	constexpr unsigned LEFT = 0b100, RIGHT = 0b010, BINARY = 0b001;
	if (id() || literal())
		return LEFT | BINARY;
	if (keyword() || (symbol() && !open()))
		return RIGHT;
	if (open())
		return LEFT | BINARY;
	if (binary())
		return RIGHT;
	if (right())
		return RIGHT;
	if (left())
		return LEFT | BINARY;
	return 0;
}

unsigned flynt::Token::follow_options() const {
	constexpr unsigned LEFT = 0b100, RIGHT = 0b010, BINARY = 0b001;
	if (_type == Type::C_BRACE)
		return LEFT;
	if (id() || literal() || closed())
		return RIGHT | BINARY;
	if (keyword() || symbol()) // closed already taken out
		return LEFT;
	if (binary())
		return LEFT;
	if (right())
		return RIGHT | BINARY;
	if (left())
		return LEFT;
	return 0;
}

unsigned flynt::Token::option() const {
	return left() ? 0b100 : right() ? 0b010 : binary() ? 0b001 : 0;
}

const std::string &flynt::Token::str() const {
	return _value;
}
