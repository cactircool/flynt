#pragma once

#include <cstdint>
#include <string>

namespace flynt {

	class Lexer;

	struct Token {
		enum class Type : uint8_t {
			UNKNOWN,

			ID,
			INT,
			FLOAT,
			CHAR,
			STR,
			TRUE,
			FALSE,

			O_PAREN,
			C_PAREN,
			O_BRACE,
			C_BRACE,
			O_BRACKET,
			C_BRACKET,
			SEMI_COLON,
			COLON,
			ARROW,
			DOUBLE_ARROW,

			LET,
			FN,
			TEMPL,
			IDEA,
			TYPE,
			TRAIT,
			PRIV,
			PUB,
			STAT,
			FIN,
			ABSTR,
			OPER,
			IMPL,
			IF,
			ELSE,
			FOR,
			UNTIL,
			MATCH,
			ENUM,
			SPACE,
			USE,
			ASM,

			// Operators
			SCOPE,

			R_INC,
			R_DEC,
			R_PLUS,
			R_MINUS,
			R_NOT,
			R_Q_MARK,
			R_B_NOT,
			R_DEREF,
			R_REF,
			R_DOLLAR,
			DOT,

			L_INC,
			L_DEC,
			L_PLUS,
			L_MINUS,
			L_NOT,
			L_Q_MARK,
			L_B_NOT,
			L_DEREF,
			L_REF,
			L_DOLLAR,
			ALLOC,
			CLEAN,

			MUL,
			DIV,
			MOD,

			ADD,
			SUB,

			LS,
			RS,

			LT,
			LE,
			GT,
			GE,
			IN,
			IS,
			AS,
			R_SPREAD,

			RANGE,
			RANGE_EQ,

			EE,
			NE,

			B_AND,

			B_XOR,

			B_OR,

			AND,

			OR,

			EQ,
			ADD_EQ,
			SUB_EQ,
			MUL_EQ,
			DIV_EQ,
			MOD_EQ,
			LS_EQ,
			RS_EQ,
			B_AND_EQ,
			B_XOR_EQ,
			B_OR_EQ,
			L_SPREAD,
		};

	private:
		Type _type;
		std::string _value;

		constexpr static const char *_values[] = {
			"true",
			"false",

			"(",
			")",
			"{",
			"}",
			"[",
			"]",
			";",
			":",
			"->",
			"=>",

			"let",
			"fn",
			"templ",
			"idea",
			"type",
			"trait",
			"priv",
			"pub",
			"stat",
			"fin",
			"abstr",
			"oper",
			"impl",
			"if",
			"else",
			"for",
			"until",
			"match",
			"enum",
			"space",
			"use",
			"asm",

			"::",

			"++",
			"--",
			"+",
			"-",
			"!",
			"?",
			"~",
			"*",
			"&",
			"$",
			".",

			"++",
			"--",
			"+",
			"-",
			"!",
			"?",
			"~",
			"*",
			"&",
			"$",
			"alloc",
			"clean",

			"*",
			"/",
			"%",

			"+",
			"-",

			"<<",
			">>",

			"<",
			"<=",
			">",
			">=",
			"in",
			"is",
			"as",
			"...",

			"..",
			"..=",

			"==",
			"!=",

			"&",

			"^",

			"|",

			"and", // or &&

			"or", // or ||

			"=",
			"+=",
			"-=",
			"*=",
			"/=",
			"%=",
			"<<=",
			">>=",
			"&=",
			"^=",
			"|=",
			"...",
		};

	public:
		Token(Type type);
		Token(Type type, std::string value);

		~Token() = default;

		Token(const Token &) = default;
		Token &operator=(const Token &) = default;

		Token(Token &&) = default;
		Token &operator=(Token &&) = default;

		const char *value() const;
		Type type() const;

		bool symbol() const;
		bool keyword() const;
		bool oper() const;
		bool right() const;
		bool left() const;
		bool unary() const;
		bool binary() const;
		int precedence() const;

		bool id() const;
		bool literal() const;

		bool vague() const;
		void rightify();
		void leftify();
		void binaryify();

		bool open() const;
		bool closed() const;
		int scope() const;

		bool precedes_left() const;
		bool precedes_right_binary() const;

		bool follows_right() const;
		bool follows_left_binary() const;

		friend Lexer;
	};

}
