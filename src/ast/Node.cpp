#include "ast/Node.hpp"

using flynt::ast::Node;

Node::Level Node::_level = Node::Level::GLOBAL;

Node *Node::create(Lexer &l) {
	switch (Node::level()) {
		case Level::GLOBAL:
			return Node::create_global(l);
		case Level::GENERAL_BLOCK:
			return Node::create_general_block(l);
	}
}

Node *Node::create_global(Lexer &l) {
	auto tok = l.lex();
	switch (tok.type()) {
		case Token::Type::LET:
			return Variable::create(l);
		case Token::Type::FN:
			return Function::create(l);
		case Token::Type::TYPE:
			return Type::create(l);
		case Token::Type::TRAIT:
			return Trait::create(l);
		case Token::Type::PUB:
			return Export::create(l);
		case Token::Type::OPER:
			return OperatorOverload::create(l);
		case Token::Type::IMPL:
			return Implementation::create(l);
		case Token::Type::ENUM:
			return Enum::create(l);
		case Token::Type::SPACE:
			return Space::create(l);
		case Token::Type::USE:
			return Use::create(l);
		case Token::Type::ASM:
			return Asm::create(l);
		case Token::Type::UNKNOWN:
			tok.error("Unknown token found.");
			return nullptr;
		default:
			tok.error("'" + std::string(tok.value()) + "' is not an allowed token in the global scope.");
			return nullptr;
	}
}

Node *Node::create_general_block(Lexer &l) {
	auto tok = l.lex();
	switch (tok.type()) {
		case Token::Type::UNKNOWN:

		case Token::Type::ID:
		case Token::Type::INT:
		case Token::Type::FLOAT:
		case Token::Type::CHAR:
		case Token::Type::STR:
		case Token::Type::TRUE:
		case Token::Type::FALSE:

		case Token::Type::O_PAREN:
		case Token::Type::C_PAREN:
		case Token::Type::O_BRACE:
		case Token::Type::C_BRACE:
		case Token::Type::O_BRACKET:
		case Token::Type::C_BRACKET:
		case Token::Type::SEMI_COLON:
		case Token::Type::COLON:
		case Token::Type::ARROW:
		case Token::Type::DOUBLE_ARROW:
		case Token::Type::COMMA:

		case Token::Type::LET:
		case Token::Type::FN:
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

		case Token::Type:://:Operators
		case Token::Type::SCOPE:

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
		case Token::Type::DOT:

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

		case Token::Type::MUL:
		case Token::Type::DIV:
		case Token::Type::MOD:

		case Token::Type::ADD:
		case Token::Type::SUB:

		case Token::Type::LS:
		case Token::Type::RS:

		case Token::Type::LT:
		case Token::Type::LE:
		case Token::Type::GT:
		case Token::Type::GE:
		case Token::Type::IN:
		case Token::Type::IS:
		case Token::Type::AS:
		case Token::Type::R_SPREAD:

		case Token::Type::RANGE:
		case Token::Type::RANGE_EQ:

		case Token::Type::EE:
		case Token::Type::NE:

		case Token::Type::B_AND:

		case Token::Type::B_XOR:

		case Token::Type::B_OR:

		case Token::Type::AND:

		case Token::Type::OR:

		case Token::Type::EQ:
		case Token::Type::ADD_EQ:
		case Token::Type::SUB_EQ:
		case Token::Type::MUL_EQ:
		case Token::Type::DIV_EQ:
		case Token::Type::MOD_EQ:
		case Token::Type::LS_EQ:
		case Token::Type::RS_EQ:
		case Token::Type::B_AND_EQ:
		case Token::Type::B_XOR_EQ:
		case Token::Type::B_OR_EQ:
		case Token::Type::L_SPREAD:
	}
}

Node::Node(Node *parent) : _parent(parent) {}
Node::~Node() {
	// Does not own the parent so nah
	_parent = nullptr;
}
