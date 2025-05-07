#include "lex/lexer.h"
#include <gtest/gtest.h>

#define TEST_PATH(X) "../../test" X
#define UNIT_PATH(X) TEST_PATH("/lexer.test.units" X)

namespace flynt::lex {

TEST(LexerTest, Construction) {
    ASSERT_ANY_THROW(Lexer("invalid file path"));
    ASSERT_NO_THROW(Lexer(TEST_PATH("/lexer.test.cpp")));
}

TEST(LexerTest, Unknown) {
    Lexer lexer(UNIT_PATH("/unknown"));
    ASSERT_ANY_THROW(lexer.lex());
}

TEST(LexerTest, Ids) {
    Lexer lexer(UNIT_PATH("/ids"));

    Token *tok;
    ASSERT_NO_THROW(tok = lexer.lex());
    ASSERT_EQ(*tok, Token(Token::ID, "helloWorld", 0));

    ASSERT_NO_THROW(tok = lexer.lex());
    ASSERT_EQ(*tok, Token(Token::ID, "HelloWorld2", 11));

    ASSERT_NO_THROW(tok = lexer.lex());
    ASSERT_EQ(*tok, Token(Token::ID, "_helloWorld1", 23));

    ASSERT_NO_THROW(tok = lexer.lex());
    ASSERT_EQ(*tok, Token(Token::INT, "0", 36));

    ASSERT_NO_THROW(tok = lexer.lex());
    ASSERT_EQ(*tok, Token(Token::ID, "helloWorld2", 37));

    ASSERT_ANY_THROW(lexer.lex());
    ASSERT_FALSE(lexer);
}

TEST(LexerTest, Comments) {
    Lexer lexer(UNIT_PATH("/comments"));

    Token *tok;
    ASSERT_NO_THROW(tok = lexer.lex());
    ASSERT_EQ(*tok, Token(Token::VAR, 65));
}

TEST(LexerTest, BasicNumbers) {
    Lexer lexer(UNIT_PATH("/basic_numbers"));

    std::vector<Token> answers
    {
        { Token::INT, "1", 0 },
        { Token::INT, "23456789", 2 },
        { Token::INT, "0987654", 11 },
        { Token::INT, "77", 19 },

        { Token::FLOAT, "4.44", 23 },
        { Token::FLOAT, "3.15", 28 },
        { Token::FLOAT, "77.9", 33 },

        { Token::FLOAT, ".9", 39 },

        { Token::INT, "10", 42 },
        { Token::DOT, 44 },

        { Token::INT, "4", 47 },
        { Token::RANGE, 48 },
        { Token::INT, "4", 50 },
    };

    Token *tok;
    for (auto &answer : answers)
    {
        ASSERT_NO_THROW(tok = lexer.lex());
        ASSERT_EQ(*tok, answer);
    }

    ASSERT_ANY_THROW(lexer.lex());
    ASSERT_FALSE(lexer);
}

TEST(LexerTest, Base2) {
    Lexer lexer(UNIT_PATH("/base_2"));

    std::vector<Token> answers
    {
        { Token::B_INT, "100", 0 },
        { Token::B_INT, "0101", 6 },
        { Token::B_INT, "1011", 13 },
        { Token::B_INT, "10", 20 },
        { Token::INT, "40", 24 },
        { Token::INT, "10", 27 },
    };

    Token *tok;
    for (auto &answer : answers)
    {
        ASSERT_NO_THROW(tok = lexer.lex());
        ASSERT_EQ(*tok, answer);
    }

    ASSERT_ANY_THROW(lexer.lex());
    ASSERT_FALSE(lexer);
}

TEST(LexerTest, Base8) {
    Lexer lexer(UNIT_PATH("/base_8"));

    std::vector<Token> answers
    {
        { Token::O_INT, "100", 0 },
        { Token::O_INT, "777", 6 },
        { Token::O_INT, "765", 12 },

        { Token::O_INT, "44", 18 },
        { Token::INT, "89", 22 },

        { Token::O_INT, "44", 25 },
        { Token::ID, "ab", 29 },

        { Token::INT, "377", 32 },
    };

    Token *tok;
    for (auto &answer : answers)
    {
        ASSERT_NO_THROW(tok = lexer.lex());
        ASSERT_EQ(*tok, answer);
    }

    ASSERT_ANY_THROW(lexer.lex());
    ASSERT_FALSE(lexer);
}

TEST(LexerTest, Base16) {
    Lexer lexer(UNIT_PATH("/base_16"));

    std::vector<Token> answers
    {
        { Token::X_INT, "fff", 0 },
        { Token::X_INT, "ab0", 6 },
        { Token::X_INT, "765", 12 },

        { Token::X_INT, "44", 18 },
        { Token::ID, "zz", 22 },

        { Token::X_INT, "4410", 25 },

        { Token::INT, "377", 32 },
    };

    Token *tok;
    for (auto &answer : answers)
    {
        ASSERT_NO_THROW(tok = lexer.lex());
        ASSERT_EQ(*tok, answer);
    }

    ASSERT_ANY_THROW(lexer.lex());
    ASSERT_FALSE(lexer);
}

TEST(LexerTest, Base36) {
    Lexer lexer(UNIT_PATH("/base_36"));

    std::vector<Token> answers
    {
        { Token::T_INT, "100", 0 },
        { Token::T_INT, "aZZ0", 6 },

        { Token::T_INT, "XZ", 13 },
        { Token::FLOAT, ".4", 17 },

        { Token::T_INT, "1040", 20 },
        { Token::INT, "10", 27 },
    };

    Token *tok;
    for (auto &answer : answers)
    {
        ASSERT_NO_THROW(tok = lexer.lex());
        ASSERT_EQ(*tok, answer);
    }

    ASSERT_ANY_THROW(lexer.lex());
    ASSERT_FALSE(lexer);
}

TEST(LexerTest, BasicChars) {
    Lexer lexer(UNIT_PATH("/basic_chars"));

    std::vector<Token> answers
    {
        { Token::CHAR, "a", 0 },
        { Token::CHAR, "b", 4 },
        { Token::CHAR, "c", 8 },
        { Token::CHAR, "d", 12 },
        { Token::CHAR, "\\t", 17 },
        { Token::CHAR, "\\a", 22 },
    };

    Token *tok;
    for (auto &answer : answers)
    {
        ASSERT_NO_THROW(tok = lexer.lex());
        ASSERT_EQ(*tok, answer);
    }

    ASSERT_ANY_THROW(lexer.lex());
    ASSERT_FALSE(lexer);
}

TEST(LexerTest, BasicStrings) {
    Lexer lexer(UNIT_PATH("/basic_strings"));

    std::vector<Token> answers
    {
        { Token::STR, "hello world!", 0 },
        { Token::STR, "this is stupid", 15 },
        { Token::STR, "trad thing", 33 },
    };

    Token *tok;
    for (auto &answer : answers)
    {
        ASSERT_NO_THROW(tok = lexer.lex());
        ASSERT_EQ(*tok, answer);
    }

    ASSERT_ANY_THROW(lexer.lex());
    ASSERT_FALSE(lexer);
}

TEST(LexerTest, ComplexChars) {
    Lexer lexer(UNIT_PATH("/complex_chars"));

    std::vector<Token> answers
    {
        { Token::CHAR, "\\t", 0 },
        { Token::CHAR, "\\a", 5 },

        { Token::STR, "cool string", 10 },
        { Token::CHAR, "\\u0021", 24 },

        { Token::CHAR, "\\t73", 33 },
        { Token::STR, "\\t74", 40 },

        { Token::CHAR, "\\o377", 47 },
        { Token::STR, "\\o400", 55 },

        { Token::CHAR, "\\b11111111", 63 },
        { Token::STR, "\\b111111110", 76 },

        { Token::CHAR, "\\xff", 90 },
        { Token::STR, "\\xff1", 97 },
    };

    Token *tok;
    for (auto &answer : answers)
    {
        ASSERT_NO_THROW(tok = lexer.lex());
        ASSERT_EQ(*tok, answer);
    }

    ASSERT_ANY_THROW(lexer.lex());
    ASSERT_FALSE(lexer);
}

TEST(LexerTest, Keywords) {
    Lexer lexer(UNIT_PATH("/keywords"));

#define T(a, b) { Token::a, b }

    std::vector<Token> answers
    {
        T(VAR, 0),
        T(VOL, 4),
        T(TYPE, 8),
        T(FN, 13),
        T(IDEA, 16),
        T(TEMPL, 21),
        T(IMPL, 27),
        T(SPACE, 32),

        T(PRIV, 39),
        T(PROT, 44),
        T(PUB, 49),
        T(STAT, 53),
        T(FIN, 58),
        T(ABSTR, 62),

        T(IF, 69),
        T(WHILE, 72),
        T(UNTIL, 78),
        T(ELSE, 84),
        T(FOR, 89),
        T(PICK, 93),
        T(CONTRACT, 98),

        T(IMP, 108),
        T(EXP, 112),
        T(EXT, 116),
        T(LINK, 120),
    };

#undef T

    Token *tok;
    for (auto &answer : answers)
    {
        ASSERT_NO_THROW(tok = lexer.lex());
        ASSERT_EQ(*tok, answer);
    }

    ASSERT_ANY_THROW(lexer.lex());
    ASSERT_FALSE(lexer);
}

TEST(LexerTest, Symbols) {
    Lexer lexer(UNIT_PATH("/symbols"));
    lexer.allowTypeCoercion();

#define T(a, b) { Token::a, b }

    std::vector<Token> answers
    {
        T(O_PAREN, 0),
        T(C_PAREN, 2),
        T(O_BRACE, 4),
        T(C_BRACE, 6),
        T(O_BRACKET, 8),
        T(C_BRACKET, 10),
        T(O_ANGLE, 12),
        T(C_ANGLE, 14),
        T(SEMI_COLON, 16),
        T(COLON, 18),
        T(ARROW, 20),
        T(DOUBLE_ARROW, 23),
    };

#undef T

    Token *tok;
    for (auto &answer : answers)
    {
        if (answer == Token(Token::DOUBLE_ARROW, 23))
            volatile int k = 1;

        ASSERT_NO_THROW(tok = lexer.lex());
        ASSERT_EQ(*tok, answer);
    }

    ASSERT_ANY_THROW(lexer.lex());
    ASSERT_FALSE(lexer);
}

TEST(LexerTest, BasicOperators) {
    Lexer lexer(UNIT_PATH("/basic_operators"));

#define T(a, b) { Token::a, b }

    std::vector<Token> answers
    {
        T(SCOPE, 0),
        T(DOT, 3),
        T(AWAIT, 5),
        T(LOT, 11),
        T(DROP, 15),
        T(DIV, 20),
        T(RS, 22),
        T(LS, 25),
        T(LT, 28),
        T(GT, 30),
        T(LE, 32),
        T(GE, 35),
        T(EE, 38),
        T(NE, 41),
        T(B_AND, 44),
        T(B_XOR, 46),
        T(B_OR, 48),
        T(AND, 50),
        T(OR, 53),
        T(RANGE, 56),
        T(RANGE_EQ, 59),
        T(THROW, 63),
        T(YIELD, 69),
        T(EQ, 75),
        T(ADD_EQ, 77),
        T(SUB_EQ, 80),
        T(MUL_EQ, 83),
        T(DIV_EQ, 86),
        T(MOD_EQ, 89),
        T(RS_EQ, 92),
        T(LS_EQ, 96),
        T(B_AND_EQ, 100),
        T(B_XOR_EQ, 103),
        T(B_OR_EQ, 106),
        T(COMMA, 109),
    };

#undef T

    Token *tok;
    for (auto &answer : answers)
    {
        ASSERT_NO_THROW(tok = lexer.lex());
        ASSERT_NO_THROW(tok->coerceToBinary()); // just cuz I'm too lazy to recalculate offsets by removing B_AND, B_XOR, B_OR
        ASSERT_EQ(*tok, answer);
    }

    ASSERT_ANY_THROW(lexer.lex());
    ASSERT_FALSE(lexer);
}

TEST(LexerTest, AmbiguousOperators) {
    Lexer lexer(UNIT_PATH("/ambiguous_operators"));
    lexer.allowTypeCoercion();

#define T(a, ...) { Token::a, __VA_ARGS__ }

    std::vector<Token> answers
    {
        T(L_PLUS, 0),
        T(INT, "5", 1),
        T(SEMI_COLON, 2),
        T(L_MINUS, 4),
        T(INT, "5", 5),
        T(SEMI_COLON, 6),
        T(L_REF, 8),
        T(ID, "x", 9),
        T(SEMI_COLON, 10),
        T(L_DEREF, 12),
        T(ID, "ptr", 13),
        T(SEMI_COLON, 16),
        T(L_PERCENT, 18),
        T(ID, "val", 19),
        T(SEMI_COLON, 22),
        T(L_XOR, 24),
        T(ID, "bit", 25),
        T(SEMI_COLON, 28),

        T(ID, "a", 31),
        T(R_INC, 32),
        T(SEMI_COLON, 34),
        T(ID, "b", 36),
        T(R_DEC, 37),
        T(SEMI_COLON, 39),
        T(ID, "arr", 41),
        T(R_PLUS, 44),
        T(SEMI_COLON, 45),
        T(ID, "ptr", 47),
        T(R_PERCENT, 50),
        T(SEMI_COLON, 51),
        T(ID, "val", 53),
        T(R_XOR, 56),
        T(SEMI_COLON, 57),

        T(ID, "a", 60),
        T(ADD, 62),
        T(ID, "b", 64),
        T(SEMI_COLON, 65),
        T(ID, "x", 67),
        T(SUB, 69),
        T(ID, "y", 71),
        T(SEMI_COLON, 72),
        T(ID, "ptr", 74),
        T(B_AND, 78),
        T(ID, "ref", 80),
        T(SEMI_COLON, 83),
        T(ID, "val", 85),
        T(MUL, 89),
        T(INT, "2", 91),
        T(SEMI_COLON, 92),
        T(ID, "num", 94),
        T(MOD, 98),
        T(INT, "2", 100),
        T(SEMI_COLON, 101),
        T(ID, "bit", 103),
        T(B_XOR, 107),
        T(ID, "mask", 109),
        T(SEMI_COLON, 113),

        T(L_PLUS, 116),
        T(ID, "a", 117),
        T(ADD, 119),
        T(ID, "b", 121),
        T(SEMI_COLON, 122),
        T(L_MINUS, 124),
        T(ID, "a", 125),
        T(SUB, 127),
        T(ID, "b", 129),
        T(SEMI_COLON, 130),
        T(L_DEREF, 132),
        T(ID, "ptr", 133),
        T(MUL, 137),
        T(ID, "arr", 139),
        T(SEMI_COLON, 142),
        T(L_REF, 144),
        T(ID, "ref", 145),
        T(B_AND, 149),
        T(ID, "val", 151),
        T(SEMI_COLON, 154),
        T(L_PERCENT, 156),
        T(ID, "num", 157),
        T(MOD, 161),
        T(ID, "div", 163),
        T(SEMI_COLON, 166),
        T(L_XOR, 168),
        T(ID, "bit", 169),
        T(B_XOR, 173),
        T(ID, "mask", 175),
        T(SEMI_COLON, 179),

        T(L_INC, 182),
        T(ID, "i", 184),
        T(ADD, 186),
        T(INT, "5", 188),
        T(SEMI_COLON, 189),
        T(L_DEC, 191),
        T(ID, "j", 193),
        T(SUB, 195),
        T(INT, "3", 197),
        T(SEMI_COLON, 198),
        T(L_DEREF, 200),
        T(ID, "ptr", 201),
        T(MUL, 205),
        T(L_REF, 207),
        T(ID, "val", 208),
        T(SEMI_COLON, 211),
        T(L_REF, 213),
        T(ID, "ref", 214),
        T(B_AND, 218),
        T(L_DEREF, 220),
        T(ID, "ptr", 221),
        T(SEMI_COLON, 224),

        T(L_PLUS, 227),
        T(ID, "a", 228),
        T(ADD, 230),
        T(L_PLUS, 232),
        T(ID, "b", 233),
        T(SEMI_COLON, 234),
        T(L_MINUS, 236),
        T(ID, "x", 237),
        T(SUB, 239),
        T(L_MINUS, 241),
        T(ID, "y", 242),
        T(SEMI_COLON, 243),
        T(L_REF, 245),
        T(ID, "val", 246),
        T(B_AND, 250),
        T(L_REF, 252),
        T(ID, "ref", 253),
        T(SEMI_COLON, 256),
        T(L_DEREF, 258),
        T(ID, "ptr", 259),
        T(MUL, 263),
        T(L_DEREF, 265),
        T(ID, "arr", 266),
        T(SEMI_COLON, 269),

        T(ID, "id", 272),
        T(R_PLUS, 274),
        T(SEMI_COLON, 275),
        T(ID, "id", 277),
        T(R_MINUS, 279),
        T(SEMI_COLON, 280),
        T(ID, "id", 282),
        T(R_DEREF, 284),
        T(SEMI_COLON, 285),
        T(ID, "id", 287),
        T(R_REF, 289),
        T(SEMI_COLON, 290),
        T(ID, "id", 292),
        T(R_PERCENT, 294),
        T(SEMI_COLON, 295),
        T(ID, "id", 297),
        T(R_XOR, 299),
        T(SEMI_COLON, 300),

        T(O_PAREN, 303),
        T(L_PLUS, 304),
        T(ID, "a", 305),
        T(C_PAREN, 306),
        T(SEMI_COLON, 307),
        T(O_BRACE, 309),
        T(L_MINUS, 310),
        T(ID, "b", 311),
        T(C_BRACE, 312),
        T(SEMI_COLON, 313),
        T(O_BRACKET, 315),
        T(L_DEREF, 316),
        T(ID, "c", 317),
        T(C_BRACKET, 318),
        T(SEMI_COLON, 319),
        T(O_ANGLE, 321),
        T(L_REF, 322),
        T(ID, "d", 323),
        T(C_ANGLE, 324),
        T(SEMI_COLON, 325),

        T(L_PLUS, 328),
        T(L_MINUS, 329),
        T(ID, "x", 330),
        T(SEMI_COLON, 331),
        T(L_MINUS, 333),
        T(L_PLUS, 334),
        T(ID, "y", 335),
        T(SEMI_COLON, 336),
        T(L_REF, 338),
        T(L_DEREF, 339),
        T(ID, "ptr", 340),
        T(SEMI_COLON, 343),
        T(L_DEREF, 345),
        T(L_REF, 346),
        T(ID, "ref", 347),
        T(SEMI_COLON, 350),
        T(L_PERCENT, 352),
        T(L_XOR, 353),
        T(ID, "val", 354),
        T(SEMI_COLON, 357),
        T(L_XOR, 359),
        T(L_PERCENT, 360),
        T(ID, "bit", 361),
        T(SEMI_COLON, 364),

        T(VAR, 367),
        T(L_PLUS, 371),
        T(ID, "x", 372),
        T(SEMI_COLON, 373),
        T(FN, 375),
        T(L_MINUS, 378),
        T(ID, "y", 379),
        T(SEMI_COLON, 380),
        T(RET, 382),
        T(L_DEREF, 386),
        T(ID, "ptr", 387),
        T(SEMI_COLON, 390),
        T(IF, 392),
        T(L_REF, 395),
        T(ID, "val", 396),
        T(SEMI_COLON, 399),
        T(O_PAREN, 401),
        T(ID, "x", 402),
        T(R_PLUS, 403),
        T(C_PAREN, 404),
        T(SEMI_COLON, 405),
        T(O_BRACE, 407),
        T(ID, "y", 408),
        T(R_MINUS, 409),
        T(C_BRACE, 410),
        T(SEMI_COLON, 411),
    };

#undef T

    Token *tok;
    for (auto &answer : answers)
    {
        if (answer == Token(Token::R_MINUS, 409))
            volatile int k = 1;
        ASSERT_NO_THROW(tok = lexer.lex());
        ASSERT_EQ(*tok, answer);
    }

    ASSERT_ANY_THROW(lexer.lex());
    ASSERT_FALSE(lexer);
}

}

#undef UNIT_PATH
#undef TEST_PATH
