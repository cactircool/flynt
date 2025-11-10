#include <gtest/gtest.h>
#include "Token.hpp"

using namespace flynt;

class TokenTest : public ::testing::Test {
protected:
    void SetUp() override {}
    void TearDown() override {}
};

// Test Token construction
TEST_F(TokenTest, ConstructorWithTypeOnly) {
    Token token(Token::Type::LET);
    EXPECT_EQ(token.type(), Token::Type::LET);
    EXPECT_STREQ(token.value(), "let");
}

TEST_F(TokenTest, ConstructorWithTypeAndValue) {
    Token token(Token::Type::ID, "myVariable");
    EXPECT_EQ(token.type(), Token::Type::ID);
    EXPECT_STREQ(token.value(), "myVariable");
    EXPECT_EQ(token.str(), "myVariable");
}

TEST_F(TokenTest, IntTokenWithValue) {
    Token token(Token::Type::INT, "12345");
    EXPECT_EQ(token.type(), Token::Type::INT);
    EXPECT_STREQ(token.value(), "12345");
}

TEST_F(TokenTest, FloatTokenWithValue) {
    Token token(Token::Type::FLOAT, "3.14159");
    EXPECT_EQ(token.type(), Token::Type::FLOAT);
    EXPECT_STREQ(token.value(), "3.14159");
}

TEST_F(TokenTest, StringTokenWithValue) {
    Token token(Token::Type::STR, "hello world");
    EXPECT_EQ(token.type(), Token::Type::STR);
    EXPECT_STREQ(token.value(), "hello world");
}

TEST_F(TokenTest, CharTokenWithValue) {
    Token token(Token::Type::CHAR, "a");
    EXPECT_EQ(token.type(), Token::Type::CHAR);
    EXPECT_STREQ(token.value(), "a");
}

// Test symbol() method
TEST_F(TokenTest, SymbolRecognition) {
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

    EXPECT_FALSE(Token(Token::Type::LET).symbol());
    EXPECT_FALSE(Token(Token::Type::ID, "test").symbol());
    EXPECT_FALSE(Token(Token::Type::ADD).symbol());
}

// Test keyword() method
TEST_F(TokenTest, KeywordRecognition) {
    EXPECT_TRUE(Token(Token::Type::LET).keyword());
    EXPECT_TRUE(Token(Token::Type::FN).keyword());
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

    EXPECT_FALSE(Token(Token::Type::ID, "test").keyword());
    EXPECT_FALSE(Token(Token::Type::INT, "123").keyword());
    EXPECT_FALSE(Token(Token::Type::ADD).keyword());
}

// Test right() method
TEST_F(TokenTest, RightUnaryOperators) {
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
TEST_F(TokenTest, LeftUnaryOperators) {
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
TEST_F(TokenTest, UnaryOperators) {
    EXPECT_TRUE(Token(Token::Type::R_INC).unary());
    EXPECT_TRUE(Token(Token::Type::L_DEC).unary());
    EXPECT_FALSE(Token(Token::Type::ADD).unary());
}

// Test binary() method
TEST_F(TokenTest, BinaryOperators) {
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
    EXPECT_TRUE(Token(Token::Type::SUB_EQ).binary());
    EXPECT_TRUE(Token(Token::Type::MUL_EQ).binary());
    EXPECT_TRUE(Token(Token::Type::DIV_EQ).binary());
    EXPECT_TRUE(Token(Token::Type::MOD_EQ).binary());
    EXPECT_TRUE(Token(Token::Type::LS_EQ).binary());
    EXPECT_TRUE(Token(Token::Type::RS_EQ).binary());
    EXPECT_TRUE(Token(Token::Type::B_AND_EQ).binary());
    EXPECT_TRUE(Token(Token::Type::B_XOR_EQ).binary());
    EXPECT_TRUE(Token(Token::Type::B_OR_EQ).binary());
    EXPECT_TRUE(Token(Token::Type::L_SPREAD).binary());

    EXPECT_FALSE(Token(Token::Type::R_INC).binary());
    EXPECT_FALSE(Token(Token::Type::ID, "test").binary());
}

// Test oper() method
TEST_F(TokenTest, OperatorRecognition) {
    EXPECT_TRUE(Token(Token::Type::ADD).oper());
    EXPECT_TRUE(Token(Token::Type::R_INC).oper());
    EXPECT_TRUE(Token(Token::Type::L_DEC).oper());
    EXPECT_FALSE(Token(Token::Type::ID, "test").oper());
    EXPECT_FALSE(Token(Token::Type::LET).oper());
}

// Test precedence() method
TEST_F(TokenTest, OperatorPrecedence) {
    EXPECT_EQ(Token(Token::Type::SCOPE).precedence(), 0);
    EXPECT_EQ(Token(Token::Type::R_INC).precedence(), 1);
    EXPECT_EQ(Token(Token::Type::DOT).precedence(), 1);
    EXPECT_EQ(Token(Token::Type::L_INC).precedence(), 2);
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

    EXPECT_EQ(Token(Token::Type::ID, "test").precedence(), -1);
    EXPECT_EQ(Token(Token::Type::LET).precedence(), -1);
}

// Test id() method
TEST_F(TokenTest, IdentifierRecognition) {
    EXPECT_TRUE(Token(Token::Type::ID, "variable").id());
    EXPECT_FALSE(Token(Token::Type::INT, "123").id());
    EXPECT_FALSE(Token(Token::Type::LET).id());
}

// Test literal() method
TEST_F(TokenTest, LiteralRecognition) {
    EXPECT_TRUE(Token(Token::Type::INT, "42").literal());
    EXPECT_TRUE(Token(Token::Type::FLOAT, "3.14").literal());
    EXPECT_TRUE(Token(Token::Type::CHAR, "a").literal());
    EXPECT_TRUE(Token(Token::Type::STR, "hello").literal());
    EXPECT_TRUE(Token(Token::Type::TRUE).literal());
    EXPECT_TRUE(Token(Token::Type::FALSE).literal());

    EXPECT_FALSE(Token(Token::Type::ID, "test").literal());
    EXPECT_FALSE(Token(Token::Type::LET).literal());
}

// Test vague() method
TEST_F(TokenTest, VagueTokenRecognition) {
    EXPECT_TRUE(Token(Token::Type::R_INC).vague());
    EXPECT_TRUE(Token(Token::Type::L_PLUS).vague());
    EXPECT_TRUE(Token(Token::Type::MUL).vague());
    EXPECT_TRUE(Token(Token::Type::ADD).vague());
    EXPECT_TRUE(Token(Token::Type::SUB).vague());
    EXPECT_TRUE(Token(Token::Type::B_AND).vague());
    EXPECT_TRUE(Token(Token::Type::R_SPREAD).vague());
    EXPECT_TRUE(Token(Token::Type::L_SPREAD).vague());

    EXPECT_FALSE(Token(Token::Type::DIV).vague());
    EXPECT_FALSE(Token(Token::Type::LET).vague());
    EXPECT_FALSE(Token(Token::Type::ID, "test").vague());
}

// Test rightify() method
TEST_F(TokenTest, RightifyConversion) {
    Token token1(Token::Type::L_INC);
    token1.rightify();
    EXPECT_EQ(token1.type(), Token::Type::R_INC);

    Token token2(Token::Type::L_PLUS);
    token2.rightify();
    EXPECT_EQ(token2.type(), Token::Type::R_PLUS);

    Token token3(Token::Type::MUL);
    token3.rightify();
    EXPECT_EQ(token3.type(), Token::Type::R_DEREF);

    Token token4(Token::Type::ADD);
    token4.rightify();
    EXPECT_EQ(token4.type(), Token::Type::R_PLUS);

    Token token5(Token::Type::SUB);
    token5.rightify();
    EXPECT_EQ(token5.type(), Token::Type::R_MINUS);

    Token token6(Token::Type::B_AND);
    token6.rightify();
    EXPECT_EQ(token6.type(), Token::Type::R_REF);

    Token token7(Token::Type::L_SPREAD);
    token7.rightify();
    EXPECT_EQ(token7.type(), Token::Type::R_SPREAD);
}

// Test leftify() method
TEST_F(TokenTest, LeftifyConversion) {
    Token token1(Token::Type::R_INC);
    token1.leftify();
    EXPECT_EQ(token1.type(), Token::Type::L_INC);

    Token token2(Token::Type::R_MINUS);
    token2.leftify();
    EXPECT_EQ(token2.type(), Token::Type::L_MINUS);

    Token token3(Token::Type::MUL);
    token3.leftify();
    EXPECT_EQ(token3.type(), Token::Type::L_DEREF);

    Token token4(Token::Type::ADD);
    token4.leftify();
    EXPECT_EQ(token4.type(), Token::Type::L_PLUS);

    Token token5(Token::Type::SUB);
    token5.leftify();
    EXPECT_EQ(token5.type(), Token::Type::L_MINUS);

    Token token6(Token::Type::B_AND);
    token6.leftify();
    EXPECT_EQ(token6.type(), Token::Type::L_REF);

    Token token7(Token::Type::R_SPREAD);
    token7.leftify();
    EXPECT_EQ(token7.type(), Token::Type::L_SPREAD);
}

// Test binaryify() method
TEST_F(TokenTest, BinaryifyConversion) {
    Token token1(Token::Type::L_PLUS);
    token1.binaryify();
    EXPECT_EQ(token1.type(), Token::Type::ADD);

    Token token2(Token::Type::R_PLUS);
    token2.binaryify();
    EXPECT_EQ(token2.type(), Token::Type::ADD);

    Token token3(Token::Type::L_MINUS);
    token3.binaryify();
    EXPECT_EQ(token3.type(), Token::Type::SUB);

    Token token4(Token::Type::R_MINUS);
    token4.binaryify();
    EXPECT_EQ(token4.type(), Token::Type::SUB);

    Token token5(Token::Type::L_DEREF);
    token5.binaryify();
    EXPECT_EQ(token5.type(), Token::Type::MUL);

    Token token6(Token::Type::R_DEREF);
    token6.binaryify();
    EXPECT_EQ(token6.type(), Token::Type::MUL);

    Token token7(Token::Type::L_REF);
    token7.binaryify();
    EXPECT_EQ(token7.type(), Token::Type::B_AND);

    Token token8(Token::Type::R_REF);
    token8.binaryify();
    EXPECT_EQ(token8.type(), Token::Type::B_AND);
}

// Test open() method
TEST_F(TokenTest, OpenBracketRecognition) {
    EXPECT_TRUE(Token(Token::Type::O_BRACE).open());
    EXPECT_TRUE(Token(Token::Type::O_BRACKET).open());
    EXPECT_TRUE(Token(Token::Type::O_PAREN).open());

    EXPECT_FALSE(Token(Token::Type::C_BRACE).open());
    EXPECT_FALSE(Token(Token::Type::LET).open());
}

// Test closed() method
TEST_F(TokenTest, ClosedBracketRecognition) {
    EXPECT_TRUE(Token(Token::Type::C_BRACE).closed());
    EXPECT_TRUE(Token(Token::Type::C_BRACKET).closed());
    EXPECT_TRUE(Token(Token::Type::C_PAREN).closed());

    EXPECT_FALSE(Token(Token::Type::O_BRACE).closed());
    EXPECT_FALSE(Token(Token::Type::LET).closed());
}

// Test scope() method
TEST_F(TokenTest, ScopeValue) {
    EXPECT_EQ(Token(Token::Type::O_BRACE).scope(), 1);
    EXPECT_EQ(Token(Token::Type::O_BRACKET).scope(), 1);
    EXPECT_EQ(Token(Token::Type::O_PAREN).scope(), 1);

    EXPECT_EQ(Token(Token::Type::C_BRACE).scope(), -1);
    EXPECT_EQ(Token(Token::Type::C_BRACKET).scope(), -1);
    EXPECT_EQ(Token(Token::Type::C_PAREN).scope(), -1);

    EXPECT_EQ(Token(Token::Type::LET).scope(), 0);
    EXPECT_EQ(Token(Token::Type::ADD).scope(), 0);
}

// Test precede_options() method
TEST_F(TokenTest, PrecedeOptions) {
    constexpr unsigned LEFT = 0b100, RIGHT = 0b010, BINARY = 0b001;

    EXPECT_EQ(Token(Token::Type::ID, "x").precede_options(), LEFT | BINARY);
    EXPECT_EQ(Token(Token::Type::INT, "42").precede_options(), LEFT | BINARY);
    EXPECT_EQ(Token(Token::Type::LET).precede_options(), RIGHT);
    EXPECT_EQ(Token(Token::Type::O_PAREN).precede_options(), LEFT | BINARY);
    EXPECT_EQ(Token(Token::Type::ADD).precede_options(), RIGHT);
    EXPECT_EQ(Token(Token::Type::R_INC).precede_options(), RIGHT);
    EXPECT_EQ(Token(Token::Type::L_INC).precede_options(), LEFT | BINARY);
}

// Test follow_options() method
TEST_F(TokenTest, FollowOptions) {
    constexpr unsigned LEFT = 0b100, RIGHT = 0b010, BINARY = 0b001;

    EXPECT_EQ(Token(Token::Type::C_BRACE).follow_options(), LEFT);
    EXPECT_EQ(Token(Token::Type::ID, "x").follow_options(), RIGHT | BINARY);
    EXPECT_EQ(Token(Token::Type::INT, "42").follow_options(), RIGHT | BINARY);
    EXPECT_EQ(Token(Token::Type::C_PAREN).follow_options(), RIGHT | BINARY);
    EXPECT_EQ(Token(Token::Type::LET).follow_options(), LEFT);
    EXPECT_EQ(Token(Token::Type::O_PAREN).follow_options(), LEFT);
    EXPECT_EQ(Token(Token::Type::ADD).follow_options(), LEFT);
    EXPECT_EQ(Token(Token::Type::R_INC).follow_options(), RIGHT | BINARY);
    EXPECT_EQ(Token(Token::Type::L_INC).follow_options(), LEFT);
}

// Test option() method
TEST_F(TokenTest, OptionValue) {
    EXPECT_EQ(Token(Token::Type::L_INC).option(), 0b100);
    EXPECT_EQ(Token(Token::Type::R_INC).option(), 0b010);
    EXPECT_EQ(Token(Token::Type::ADD).option(), 0b001);
    EXPECT_EQ(Token(Token::Type::LET).option(), 0);
}

// Test copy constructor and assignment
TEST_F(TokenTest, CopyConstructor) {
    Token token1(Token::Type::ID, "test");
    Token token2(token1);

    EXPECT_EQ(token2.type(), Token::Type::ID);
    EXPECT_STREQ(token2.value(), "test");
}

TEST_F(TokenTest, CopyAssignment) {
    Token token1(Token::Type::ID, "test");
    Token token2(Token::Type::LET);

    token2 = token1;

    EXPECT_EQ(token2.type(), Token::Type::ID);
    EXPECT_STREQ(token2.value(), "test");
}

// Test move constructor and assignment
TEST_F(TokenTest, MoveConstructor) {
    Token token1(Token::Type::ID, "test");
    Token token2(std::move(token1));

    EXPECT_EQ(token2.type(), Token::Type::ID);
    EXPECT_STREQ(token2.value(), "test");
}

TEST_F(TokenTest, MoveAssignment) {
    Token token1(Token::Type::ID, "test");
    Token token2(Token::Type::LET);

    token2 = std::move(token1);

    EXPECT_EQ(token2.type(), Token::Type::ID);
    EXPECT_STREQ(token2.value(), "test");
}

// Test all token type values
TEST_F(TokenTest, AllTokenValues) {
    EXPECT_STREQ(Token(Token::Type::TRUE).value(), "true");
    EXPECT_STREQ(Token(Token::Type::FALSE).value(), "false");
    EXPECT_STREQ(Token(Token::Type::O_PAREN).value(), "(");
    EXPECT_STREQ(Token(Token::Type::C_PAREN).value(), ")");
    EXPECT_STREQ(Token(Token::Type::O_BRACE).value(), "{");
    EXPECT_STREQ(Token(Token::Type::C_BRACE).value(), "}");
    EXPECT_STREQ(Token(Token::Type::O_BRACKET).value(), "[");
    EXPECT_STREQ(Token(Token::Type::C_BRACKET).value(), "]");
    EXPECT_STREQ(Token(Token::Type::SEMI_COLON).value(), ";");
    EXPECT_STREQ(Token(Token::Type::COLON).value(), ":");
    EXPECT_STREQ(Token(Token::Type::ARROW).value(), "->");
    EXPECT_STREQ(Token(Token::Type::DOUBLE_ARROW).value(), "=>");
    EXPECT_STREQ(Token(Token::Type::COMMA).value(), ",");
    EXPECT_STREQ(Token(Token::Type::SCOPE).value(), "::");
    EXPECT_STREQ(Token(Token::Type::DOT).value(), ".");
    EXPECT_STREQ(Token(Token::Type::RANGE).value(), "..");
    EXPECT_STREQ(Token(Token::Type::RANGE_EQ).value(), "..=");
}
