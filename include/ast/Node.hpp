#pragma once

#include "Lexer.hpp"
namespace flynt::ast {

	class Node {
	public:
		enum class Level {
			GLOBAL,
			GENERAL_BLOCK,
		};

	private:
		Node *_parent;
		static Level _level;

		static Node *create_global(Lexer &l);
		static Node *create_general_block(Lexer &l);
	public:
		Node(Node *parent);
		virtual ~Node();

		static Level level() { return _level; }
		static Node *create(Lexer &l);
		const Node *parent() const { return _parent; }
	};
}
