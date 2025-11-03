#include <gtest/gtest.h>
#include "Token.hpp"

using namespace flynt;

// Test basic Token construction
TEST(TokenTest, Construction) {
    Token tok1(Token::Type::UNKNOWN);
    EXPECT_EQ(tok1.type(), Token::Type::UNKNOWN);

    Token tok2(Token::Type::ID, "myVariable");
    EXPECT_EQ(tok2.type(), Token::Type::ID);
    EXPECT_STREQ(tok2.value(), "myVariable");

    Token tok3(Token::Type::INT, "42");
    EXPECT_EQ(tok3.type(), Token::Type::INT);
    EXPECT_STREQ(tok3.value(), "42");
}

// Test copy and move operations
TEST(TokenTest, CopyAndMove) {
    Token original(Token::Type::ID, "test");

    Token copied = original;
    EXPECT_EQ(copied.type(), Token::Type::ID);
    EXPECT_STREQ(copied.value(), "test");

    Token moved = std::move(original);
    EXPECT_EQ(moved.type(), Token::Type::ID);
    EXPECT_STREQ(moved.value(), "test");
}

// Test value() method for different token types
TEST(TokenTest, ValueMethod) {
    Token id(Token::Type::ID, "identifier");
    EXPECT_STREQ(id.value(), "identifier");

    Token integer(Token::Type::INT, "123");
    EXPECT_STREQ(integer.value(), "123");

    Token floatTok(Token::Type::FLOAT, "3.14");
    EXPECT_STREQ(floatTok.value(), "3.14");

    Token charTok(Token::Type::CHAR, "a");
    EXPECT_STREQ(charTok.value(), "a");

    Token strTok(Token::Type::STR, "hello");
    EXPECT_STREQ(strTok.value(), "hello");

    Token trueTok(Token::Type::TRUE);
    EXPECT_STREQ(trueTok.value(), "true");

    Token falseTok(Token::Type::FALSE);
    EXPECT_STREQ(falseTok.value(), "false");

    Token oParen(Token::Type::O_PAREN);
    EXPECT_STREQ(oParen.value(), "(");

    Token cParen(Token::Type::C_PAREN);
    EXPECT_STREQ(cParen.value(), ")");

    Token letTok(Token::Type::LET);
    EXPECT_STREQ(letTok.value(), "let");

    Token plusTok(Token::Type::ADD);
    EXPECT_STREQ(plusTok.value(), "+");
}

// Test symbol() method
TEST(TokenTest, SymbolMethod) {
    EXPECT_TRUE(Token(Token::Type::O_PAREN).symbol());
    EXPECT_TRUE(Token(Token::Type::C_PAREN).symbol());
    EXPECT_TRUE(Token(Token::Type::O_BRACE).symbol());
    EXPECT_TRUE(Token(Token::Type::C_BRACE).symbol());
    EXPECT_TRUE(Token(Token::Type::O_BRACKET).symbol());
    EXPECT_TRUE(Token(Token::Type::C_BRACKET).symbol());
    EXPECT_TRUE(Token(Token::Type::SEMI_COLON).symbol());
    EXPECT_TRUE(Token(Token::Type::COLON).symbol());
    EXPECT_TRUE(Token(Token::Type::ARROW).symbol());
    EXPECT_TRUE(Token(Token::Type::DOUBLE_ARROW).symbol());

    EXPECT_FALSE(Token(Token::Type::ID, "x").symbol());
    EXPECT_FALSE(Token(Token::Type::INT, "5").symbol());
    EXPECT_FALSE(Token(Token::Type::ADD).symbol());
    EXPECT_FALSE(Token(Token::Type::LET).symbol());
}

// Test keyword() method
TEST(TokenTest, KeywordMethod) {
    EXPECT_TRUE(Token(Token::Type::LET).keyword());
    EXPECT_TRUE(Token(Token::Type::FN).keyword());
    EXPECT_TRUE(Token(Token::Type::TEMPL).keyword());
    EXPECT_TRUE(Token(Token::Type::IDEA).keyword());
    EXPECT_TRUE(Token(Token::Type::TYPE).keyword());
    EXPECT_TRUE(Token(Token::Type::TRAIT).keyword());
    EXPECT_TRUE(Token(Token::Type::PRIV).keyword());
    EXPECT_TRUE(Token(Token::Type::PUB).keyword());
    EXPECT_TRUE(Token(Token::Type::STAT).keyword());
    EXPECT_TRUE(Token(Token::Type::FIN).keyword());
    EXPECT_TRUE(Token(Token::Type::ABSTR).keyword());
    EXPECT_TRUE(Token(Token::Type::OPER).keyword());
    EXPECT_TRUE(Token(Token::Type::IMPL).keyword());
    EXPECT_TRUE(Token(Token::Type::IF).keyword());
    EXPECT_TRUE(Token(Token::Type::ELSE).keyword());
    EXPECT_TRUE(Token(Token::Type::FOR).keyword());
    EXPECT_TRUE(Token(Token::Type::UNTIL).keyword());
    EXPECT_TRUE(Token(Token::Type::MATCH).keyword());
    EXPECT_TRUE(Token(Token::Type::ENUM).keyword());
    EXPECT_TRUE(Token(Token::Type::SPACE).keyword());
    EXPECT_TRUE(Token(Token::Type::USE).keyword());
    EXPECT_TRUE(Token(Token::Type::ASM).keyword());

    EXPECT_FALSE(Token(Token::Type::ID, "x").keyword());
    EXPECT_FALSE(Token(Token::Type::ADD).keyword());
}

// Test right() method
TEST(TokenTest, RightMethod) {
    EXPECT_TRUE(Token(Token::Type::R_INC).right());
    EXPECT_TRUE(Token(Token::Type::R_DEC).right());
    EXPECT_TRUE(Token(Token::Type::R_PLUS).right());
    EXPECT_TRUE(Token(Token::Type::R_MINUS).right());
    EXPECT_TRUE(Token(Token::Type::R_NOT).right());
    EXPECT_TRUE(Token(Token::Type::R_Q_MARK).right());
    EXPECT_TRUE(Token(Token::Type::R_B_NOT).right());
    EXPECT_TRUE(Token(Token::Type::R_DEREF).right());
    EXPECT_TRUE(Token(Token::Type::R_REF).right());
    EXPECT_TRUE(Token(Token::Type::R_DOLLAR).right());

    EXPECT_FALSE(Token(Token::Type::L_INC).right());
    EXPECT_FALSE(Token(Token::Type::ADD).right());
}

// Test left() method
TEST(TokenTest, LeftMethod) {
    EXPECT_TRUE(Token(Token::Type::L_INC).left());
    EXPECT_TRUE(Token(Token::Type::L_DEC).left());
    EXPECT_TRUE(Token(Token::Type::L_PLUS).left());
    EXPECT_TRUE(Token(Token::Type::L_MINUS).left());
    EXPECT_TRUE(Token(Token::Type::L_NOT).left());
    EXPECT_TRUE(Token(Token::Type::L_Q_MARK).left());
    EXPECT_TRUE(Token(Token::Type::L_B_NOT).left());
    EXPECT_TRUE(Token(Token::Type::L_DEREF).left());
    EXPECT_TRUE(Token(Token::Type::L_REF).left());
    EXPECT_TRUE(Token(Token::Type::L_DOLLAR).left());
    EXPECT_TRUE(Token(Token::Type::ALLOC).left());
    EXPECT_TRUE(Token(Token::Type::CLEAN).left());

    EXPECT_FALSE(Token(Token::Type::R_INC).left());
    EXPECT_FALSE(Token(Token::Type::ADD).left());
}

// Test unary() method
TEST(TokenTest, UnaryMethod) {
    EXPECT_TRUE(Token(Token::Type::R_INC).unary());
    EXPECT_TRUE(Token(Token::Type::L_INC).unary());
    EXPECT_TRUE(Token(Token::Type::R_PLUS).unary());
    EXPECT_TRUE(Token(Token::Type::L_MINUS).unary());
    EXPECT_TRUE(Token(Token::Type::ALLOC).unary());

    EXPECT_FALSE(Token(Token::Type::ADD).unary());
    EXPECT_FALSE(Token(Token::Type::MUL).unary());
}

// Test binary() method
TEST(TokenTest, BinaryMethod) {
    EXPECT_TRUE(Token(Token::Type::SCOPE).binary());
    EXPECT_TRUE(Token(Token::Type::DOT).binary());
    EXPECT_TRUE(Token(Token::Type::MUL).binary());
    EXPECT_TRUE(Token(Token::Type::DIV).binary());
    EXPECT_TRUE(Token(Token::Type::MOD).binary());
    EXPECT_TRUE(Token(Token::Type::ADD).binary());
    EXPECT_TRUE(Token(Token::Type::SUB).binary());
    EXPECT_TRUE(Token(Token::Type::LS).binary());
    EXPECT_TRUE(Token(Token::Type::RS).binary());
    EXPECT_TRUE(Token(Token::Type::LT).binary());
    EXPECT_TRUE(Token(Token::Type::LE).binary());
    EXPECT_TRUE(Token(Token::Type::GT).binary());
    EXPECT_TRUE(Token(Token::Type::GE).binary());
    EXPECT_TRUE(Token(Token::Type::IN).binary());
    EXPECT_TRUE(Token(Token::Type::IS).binary());
    EXPECT_TRUE(Token(Token::Type::AS).binary());
    EXPECT_TRUE(Token(Token::Type::R_SPREAD).binary());
    EXPECT_TRUE(Token(Token::Type::RANGE).binary());
    EXPECT_TRUE(Token(Token::Type::RANGE_EQ).binary());
    EXPECT_TRUE(Token(Token::Type::EE).binary());
    EXPECT_TRUE(Token(Token::Type::NE).binary());
    EXPECT_TRUE(Token(Token::Type::B_AND).binary());
    EXPECT_TRUE(Token(Token::Type::B_XOR).binary());
    EXPECT_TRUE(Token(Token::Type::B_OR).binary());
    EXPECT_TRUE(Token(Token::Type::AND).binary());
    EXPECT_TRUE(Token(Token::Type::OR).binary());
    EXPECT_TRUE(Token(Token::Type::EQ).binary());
    EXPECT_TRUE(Token(Token::Type::ADD_EQ).binary());
    EXPECT_TRUE(Token(Token::Type::L_SPREAD).binary());

    EXPECT_FALSE(Token(Token::Type::R_INC).binary());
    EXPECT_FALSE(Token(Token::Type::L_DEC).binary());
}

// Test oper() method
TEST(TokenTest, OperMethod) {
    EXPECT_TRUE(Token(Token::Type::ADD).oper());
    EXPECT_TRUE(Token(Token::Type::R_INC).oper());
    EXPECT_TRUE(Token(Token::Type::L_DEC).oper());

    EXPECT_FALSE(Token(Token::Type::ID, "x").oper());
    EXPECT_FALSE(Token(Token::Type::LET).oper());
}

// Test precedence() method
TEST(TokenTest, PrecedenceMethod) {
    EXPECT_EQ(Token(Token::Type::SCOPE).precedence(), 0);
    EXPECT_EQ(Token(Token::Type::R_INC).precedence(), 1);
    EXPECT_EQ(Token(Token::Type::DOT).precedence(), 1);
    EXPECT_EQ(Token(Token::Type::L_INC).precedence(), 2);
    EXPECT_EQ(Token(Token::Type::ALLOC).precedence(), 2);
    EXPECT_EQ(Token(Token::Type::MUL).precedence(), 3);
    EXPECT_EQ(Token(Token::Type::ADD).precedence(), 4);
    EXPECT_EQ(Token(Token::Type::LS).precedence(), 5);
    EXPECT_EQ(Token(Token::Type::LT).precedence(), 6);
    EXPECT_EQ(Token(Token::Type::RANGE).precedence(), 7);
    EXPECT_EQ(Token(Token::Type::EE).precedence(), 8);
    EXPECT_EQ(Token(Token::Type::B_AND).precedence(), 9);
    EXPECT_EQ(Token(Token::Type::B_XOR).precedence(), 10);
    EXPECT_EQ(Token(Token::Type::B_OR).precedence(), 11);
    EXPECT_EQ(Token(Token::Type::AND).precedence(), 12);
    EXPECT_EQ(Token(Token::Type::OR).precedence(), 13);
    EXPECT_EQ(Token(Token::Type::EQ).precedence(), 14);

    EXPECT_EQ(Token(Token::Type::ID, "x").precedence(), -1);
}

// Test id() method
TEST(TokenTest, IdMethod) {
    EXPECT_TRUE(Token(Token::Type::ID, "variable").id());
    EXPECT_FALSE(Token(Token::Type::INT, "5").id());
    EXPECT_FALSE(Token(Token::Type::LET).id());
}

// Test literal() method
TEST(TokenTest, LiteralMethod) {
    EXPECT_TRUE(Token(Token::Type::INT, "42").literal());
    EXPECT_TRUE(Token(Token::Type::FLOAT, "3.14").literal());
    EXPECT_TRUE(Token(Token::Type::CHAR, "a").literal());
    EXPECT_TRUE(Token(Token::Type::STR, "hello").literal());
    EXPECT_TRUE(Token(Token::Type::TRUE).literal());
    EXPECT_TRUE(Token(Token::Type::FALSE).literal());

    EXPECT_FALSE(Token(Token::Type::ID, "x").literal());
    EXPECT_FALSE(Token(Token::Type::ADD).literal());
}

// Test vague() method
TEST(TokenTest, VagueMethod) {
    // Right unary operators (vague)
    EXPECT_TRUE(Token(Token::Type::R_INC).vague());
    EXPECT_TRUE(Token(Token::Type::R_DEC).vague());
    EXPECT_TRUE(Token(Token::Type::R_PLUS).vague());
    EXPECT_TRUE(Token(Token::Type::R_MINUS).vague());
    EXPECT_TRUE(Token(Token::Type::R_DEREF).vague());
    EXPECT_TRUE(Token(Token::Type::R_REF).vague());
    EXPECT_TRUE(Token(Token::Type::R_SPREAD).vague());

    // Left unary operators (vague)
    EXPECT_TRUE(Token(Token::Type::L_INC).vague());
    EXPECT_TRUE(Token(Token::Type::L_DEC).vague());
    EXPECT_TRUE(Token(Token::Type::L_PLUS).vague());
    EXPECT_TRUE(Token(Token::Type::L_MINUS).vague());
    EXPECT_TRUE(Token(Token::Type::L_DEREF).vague());
    EXPECT_TRUE(Token(Token::Type::L_REF).vague());
    EXPECT_TRUE(Token(Token::Type::L_SPREAD).vague());

    // Binary operators (vague)
    EXPECT_TRUE(Token(Token::Type::MUL).vague());
    EXPECT_TRUE(Token(Token::Type::ADD).vague());
    EXPECT_TRUE(Token(Token::Type::SUB).vague());
    EXPECT_TRUE(Token(Token::Type::B_AND).vague());

    // Non-vague operators
    EXPECT_FALSE(Token(Token::Type::DIV).vague());
    EXPECT_FALSE(Token(Token::Type::MOD).vague());
    EXPECT_FALSE(Token(Token::Type::ALLOC).vague());
    EXPECT_FALSE(Token(Token::Type::CLEAN).vague());
    EXPECT_FALSE(Token(Token::Type::ID, "x").vague());
}

// Test rightify() method
TEST(TokenTest, RightifyMethod) {
    Token tok1(Token::Type::L_INC);
    tok1.rightify();
    EXPECT_EQ(tok1.type(), Token::Type::R_INC);

    Token tok2(Token::Type::L_PLUS);
    tok2.rightify();
    EXPECT_EQ(tok2.type(), Token::Type::R_PLUS);

    Token tok3(Token::Type::MUL);
    tok3.rightify();
    EXPECT_EQ(tok3.type(), Token::Type::R_DEREF);

    Token tok4(Token::Type::ADD);
    tok4.rightify();
    EXPECT_EQ(tok4.type(), Token::Type::R_PLUS);

    Token tok5(Token::Type::SUB);
    tok5.rightify();
    EXPECT_EQ(tok5.type(), Token::Type::R_MINUS);

    Token tok6(Token::Type::B_AND);
    tok6.rightify();
    EXPECT_EQ(tok6.type(), Token::Type::R_REF);

    Token tok7(Token::Type::L_SPREAD);
    tok7.rightify();
    EXPECT_EQ(tok7.type(), Token::Type::R_SPREAD);
}

// Test leftify() method
TEST(TokenTest, LeftifyMethod) {
    Token tok1(Token::Type::R_INC);
    tok1.leftify();
    EXPECT_EQ(tok1.type(), Token::Type::L_INC);

    Token tok2(Token::Type::R_PLUS);
    tok2.leftify();
    EXPECT_EQ(tok2.type(), Token::Type::L_PLUS);

    Token tok3(Token::Type::MUL);
    tok3.leftify();
    EXPECT_EQ(tok3.type(), Token::Type::L_DEREF);

    Token tok4(Token::Type::ADD);
    tok4.leftify();
    EXPECT_EQ(tok4.type(), Token::Type::L_PLUS);

    Token tok5(Token::Type::SUB);
    tok5.leftify();
    EXPECT_EQ(tok5.type(), Token::Type::L_MINUS);

    Token tok6(Token::Type::B_AND);
    tok6.leftify();
    EXPECT_EQ(tok6.type(), Token::Type::L_REF);

    Token tok7(Token::Type::R_SPREAD);
    tok7.leftify();
    EXPECT_EQ(tok7.type(), Token::Type::L_SPREAD);
}

// Test binaryify() method
TEST(TokenTest, BinaryifyMethod) {
    Token tok1(Token::Type::L_PLUS);
    tok1.binaryify();
    EXPECT_EQ(tok1.type(), Token::Type::ADD);

    Token tok2(Token::Type::R_PLUS);
    tok2.binaryify();
    EXPECT_EQ(tok2.type(), Token::Type::ADD);

    Token tok3(Token::Type::L_MINUS);
    tok3.binaryify();
    EXPECT_EQ(tok3.type(), Token::Type::SUB);

    Token tok4(Token::Type::R_MINUS);
    tok4.binaryify();
    EXPECT_EQ(tok4.type(), Token::Type::SUB);

    Token tok5(Token::Type::L_DEREF);
    tok5.binaryify();
    EXPECT_EQ(tok5.type(), Token::Type::MUL);

    Token tok6(Token::Type::R_DEREF);
    tok6.binaryify();
    EXPECT_EQ(tok6.type(), Token::Type::MUL);

    Token tok7(Token::Type::L_REF);
    tok7.binaryify();
    EXPECT_EQ(tok7.type(), Token::Type::B_AND);

    Token tok8(Token::Type::R_REF);
    tok8.binaryify();
    EXPECT_EQ(tok8.type(), Token::Type::B_AND);
}

// Test open() method
TEST(TokenTest, OpenMethod) {
    EXPECT_TRUE(Token(Token::Type::O_BRACE).open());
    EXPECT_TRUE(Token(Token::Type::O_BRACKET).open());
    EXPECT_TRUE(Token(Token::Type::O_PAREN).open());

    EXPECT_FALSE(Token(Token::Type::C_BRACE).open());
    EXPECT_FALSE(Token(Token::Type::ID, "x").open());
}

// Test closed() method
TEST(TokenTest, ClosedMethod) {
    EXPECT_TRUE(Token(Token::Type::C_BRACE).closed());
    EXPECT_TRUE(Token(Token::Type::C_BRACKET).closed());
    EXPECT_TRUE(Token(Token::Type::C_PAREN).closed());

    EXPECT_FALSE(Token(Token::Type::O_BRACE).closed());
    EXPECT_FALSE(Token(Token::Type::ID, "x").closed());
}

// Test scope() method
TEST(TokenTest, ScopeMethod) {
    EXPECT_EQ(Token(Token::Type::O_BRACE).scope(), 1);
    EXPECT_EQ(Token(Token::Type::O_BRACKET).scope(), 1);
    EXPECT_EQ(Token(Token::Type::O_PAREN).scope(), 1);

    EXPECT_EQ(Token(Token::Type::C_BRACE).scope(), -1);
    EXPECT_EQ(Token(Token::Type::C_BRACKET).scope(), -1);
    EXPECT_EQ(Token(Token::Type::C_PAREN).scope(), -1);

    EXPECT_EQ(Token(Token::Type::ID, "x").scope(), 0);
    EXPECT_EQ(Token(Token::Type::ADD).scope(), 0);
}
