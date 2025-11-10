#include <gtest/gtest.h>
#include "Token.hpp"
#include <string>

using namespace flynt;

class TokenTest : public ::testing::Test {
protected:
    void SetUp() override {}
    void TearDown() override {}
};

// Test Token Construction
TEST_F(TokenTest, ConstructWithTypeOnly) {
    Token tok(Token::Type::TRUE);
    EXPECT_EQ(tok.type(), Token::Type::TRUE);
    EXPECT_STREQ(tok.value(), "true");
}

TEST_F(TokenTest, ConstructWithTypeAndValue) {
    Token tok(Token::Type::ID, "myVariable");
    EXPECT_EQ(tok.type(), Token::Type::ID);
    EXPECT_STREQ(tok.value(), "myVariable");
    EXPECT_EQ(tok.str(), "myVariable");
}

TEST_F(TokenTest, ConstructIntegerToken) {
    Token tok(Token::Type::INT, "42");
    EXPECT_EQ(tok.type(), Token::Type::INT);
    EXPECT_STREQ(tok.value(), "42");
}

TEST_F(TokenTest, ConstructFloatToken) {
    Token tok(Token::Type::FLOAT, "3.14");
    EXPECT_EQ(tok.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok.value(), "3.14");
}

TEST_F(TokenTest, ConstructStringToken) {
    Token tok(Token::Type::STR, "hello world");
    EXPECT_EQ(tok.type(), Token::Type::STR);
    EXPECT_STREQ(tok.value(), "hello world");
}

TEST_F(TokenTest, ConstructCharToken) {
    Token tok(Token::Type::CHAR, "a");
    EXPECT_EQ(tok.type(), Token::Type::CHAR);
    EXPECT_STREQ(tok.value(), "a");
}

// Test Symbol Recognition
TEST_F(TokenTest, SymbolTokens) {
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
    EXPECT_FALSE(Token(Token::Type::LET).symbol());
}

// Test Keyword Recognition
TEST_F(TokenTest, KeywordTokens) {
    EXPECT_TRUE(Token(Token::Type::LET).keyword());
    EXPECT_TRUE(Token(Token::Type::FN).keyword());
    EXPECT_TRUE(Token(Token::Type::TYPE).keyword());
    EXPECT_TRUE(Token(Token::Type::TRAIT).keyword());
    EXPECT_TRUE(Token(Token::Type::IF).keyword());
    EXPECT_TRUE(Token(Token::Type::ELSE).keyword());
    EXPECT_TRUE(Token(Token::Type::FOR).keyword());
    EXPECT_TRUE(Token(Token::Type::MATCH).keyword());
    EXPECT_TRUE(Token(Token::Type::ENUM).keyword());
    EXPECT_TRUE(Token(Token::Type::USE).keyword());

    EXPECT_FALSE(Token(Token::Type::ID, "let").keyword());
    EXPECT_FALSE(Token(Token::Type::ADD).keyword());
}

// Test Right Unary Operators
TEST_F(TokenTest, RightUnaryOperators) {
    EXPECT_TRUE(Token(Token::Type::R_INC).right());
    EXPECT_TRUE(Token(Token::Type::R_DEC).right());
    EXPECT_TRUE(Token(Token::Type::R_PLUS).right());
    EXPECT_TRUE(Token(Token::Type::R_MINUS).right());
    EXPECT_TRUE(Token(Token::Type::R_NOT).right());
    EXPECT_TRUE(Token(Token::Type::R_DEREF).right());
    EXPECT_TRUE(Token(Token::Type::R_REF).right());

    EXPECT_FALSE(Token(Token::Type::L_INC).right());
    EXPECT_FALSE(Token(Token::Type::ADD).right());
}

// Test Left Unary Operators
TEST_F(TokenTest, LeftUnaryOperators) {
    EXPECT_TRUE(Token(Token::Type::L_INC).left());
    EXPECT_TRUE(Token(Token::Type::L_DEC).left());
    EXPECT_TRUE(Token(Token::Type::L_PLUS).left());
    EXPECT_TRUE(Token(Token::Type::L_MINUS).left());
    EXPECT_TRUE(Token(Token::Type::L_NOT).left());
    EXPECT_TRUE(Token(Token::Type::L_DEREF).left());
    EXPECT_TRUE(Token(Token::Type::L_REF).left());
    EXPECT_TRUE(Token(Token::Type::ALLOC).left());
    EXPECT_TRUE(Token(Token::Type::CLEAN).left());

    EXPECT_FALSE(Token(Token::Type::R_INC).left());
    EXPECT_FALSE(Token(Token::Type::ADD).left());
}

// Test Unary Operators
TEST_F(TokenTest, UnaryOperators) {
    EXPECT_TRUE(Token(Token::Type::R_INC).unary());
    EXPECT_TRUE(Token(Token::Type::L_INC).unary());
    EXPECT_TRUE(Token(Token::Type::R_MINUS).unary());
    EXPECT_TRUE(Token(Token::Type::L_PLUS).unary());

    EXPECT_FALSE(Token(Token::Type::ADD).unary());
    EXPECT_FALSE(Token(Token::Type::MUL).unary());
}

// Test Binary Operators
TEST_F(TokenTest, BinaryOperators) {
    EXPECT_TRUE(Token(Token::Type::ADD).binary());
    EXPECT_TRUE(Token(Token::Type::SUB).binary());
    EXPECT_TRUE(Token(Token::Type::MUL).binary());
    EXPECT_TRUE(Token(Token::Type::DIV).binary());
    EXPECT_TRUE(Token(Token::Type::MOD).binary());
    EXPECT_TRUE(Token(Token::Type::EQ).binary());
    EXPECT_TRUE(Token(Token::Type::LT).binary());
    EXPECT_TRUE(Token(Token::Type::GT).binary());
    EXPECT_TRUE(Token(Token::Type::AND).binary());
    EXPECT_TRUE(Token(Token::Type::OR).binary());

    EXPECT_FALSE(Token(Token::Type::R_INC).binary());
    EXPECT_FALSE(Token(Token::Type::L_DEC).binary());
}

// Test Operator Recognition
TEST_F(TokenTest, OperatorRecognition) {
    EXPECT_TRUE(Token(Token::Type::ADD).oper());
    EXPECT_TRUE(Token(Token::Type::R_INC).oper());
    EXPECT_TRUE(Token(Token::Type::L_DEC).oper());
    EXPECT_TRUE(Token(Token::Type::MUL).oper());

    EXPECT_FALSE(Token(Token::Type::ID, "x").oper());
    EXPECT_FALSE(Token(Token::Type::LET).oper());
}

// Test Precedence
TEST_F(TokenTest, OperatorPrecedence) {
    EXPECT_EQ(Token(Token::Type::SCOPE).precedence(), 0);
    EXPECT_EQ(Token(Token::Type::R_INC).precedence(), 1);
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

    EXPECT_EQ(Token(Token::Type::ID, "x").precedence(), -1);
    EXPECT_EQ(Token(Token::Type::LET).precedence(), -1);
}

// Test ID Recognition
TEST_F(TokenTest, IdentifierRecognition) {
    EXPECT_TRUE(Token(Token::Type::ID, "variable").id());
    EXPECT_FALSE(Token(Token::Type::INT, "42").id());
    EXPECT_FALSE(Token(Token::Type::LET).id());
}

// Test Literal Recognition
TEST_F(TokenTest, LiteralRecognition) {
    EXPECT_TRUE(Token(Token::Type::INT, "42").literal());
    EXPECT_TRUE(Token(Token::Type::FLOAT, "3.14").literal());
    EXPECT_TRUE(Token(Token::Type::CHAR, "a").literal());
    EXPECT_TRUE(Token(Token::Type::STR, "hello").literal());
    EXPECT_TRUE(Token(Token::Type::TRUE).literal());
    EXPECT_TRUE(Token(Token::Type::FALSE).literal());

    EXPECT_FALSE(Token(Token::Type::ID, "x").literal());
    EXPECT_FALSE(Token(Token::Type::ADD).literal());
}

// Test Vague Token Recognition
TEST_F(TokenTest, VagueTokens) {
    EXPECT_TRUE(Token(Token::Type::R_INC).vague());
    EXPECT_TRUE(Token(Token::Type::L_INC).vague());
    EXPECT_TRUE(Token(Token::Type::R_PLUS).vague());
    EXPECT_TRUE(Token(Token::Type::L_MINUS).vague());
    EXPECT_TRUE(Token(Token::Type::MUL).vague());
    EXPECT_TRUE(Token(Token::Type::ADD).vague());
    EXPECT_TRUE(Token(Token::Type::SUB).vague());
    EXPECT_TRUE(Token(Token::Type::B_AND).vague());
    EXPECT_TRUE(Token(Token::Type::R_SPREAD).vague());
    EXPECT_TRUE(Token(Token::Type::L_SPREAD).vague());

    EXPECT_FALSE(Token(Token::Type::DIV).vague());
    EXPECT_FALSE(Token(Token::Type::LT).vague());
    EXPECT_FALSE(Token(Token::Type::ID, "x").vague());
}

// Test Rightify
TEST_F(TokenTest, RightifyTransformations) {
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
}

// Test Leftify
TEST_F(TokenTest, LeftifyTransformations) {
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
}

// Test Binaryify
TEST_F(TokenTest, BinaryifyTransformations) {
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

// Test Scope Recognition
TEST_F(TokenTest, OpenBrackets) {
    EXPECT_TRUE(Token(Token::Type::O_PAREN).open());
    EXPECT_TRUE(Token(Token::Type::O_BRACE).open());
    EXPECT_TRUE(Token(Token::Type::O_BRACKET).open());

    EXPECT_FALSE(Token(Token::Type::C_PAREN).open());
    EXPECT_FALSE(Token(Token::Type::ID, "x").open());
}

TEST_F(TokenTest, CloseBrackets) {
    EXPECT_TRUE(Token(Token::Type::C_PAREN).closed());
    EXPECT_TRUE(Token(Token::Type::C_BRACE).closed());
    EXPECT_TRUE(Token(Token::Type::C_BRACKET).closed());

    EXPECT_FALSE(Token(Token::Type::O_PAREN).closed());
    EXPECT_FALSE(Token(Token::Type::ID, "x").closed());
}

TEST_F(TokenTest, ScopeValue) {
    EXPECT_EQ(Token(Token::Type::O_PAREN).scope(), 1);
    EXPECT_EQ(Token(Token::Type::O_BRACE).scope(), 1);
    EXPECT_EQ(Token(Token::Type::O_BRACKET).scope(), 1);

    EXPECT_EQ(Token(Token::Type::C_PAREN).scope(), -1);
    EXPECT_EQ(Token(Token::Type::C_BRACE).scope(), -1);
    EXPECT_EQ(Token(Token::Type::C_BRACKET).scope(), -1);

    EXPECT_EQ(Token(Token::Type::ID, "x").scope(), 0);
    EXPECT_EQ(Token(Token::Type::ADD).scope(), 0);
}

// Test Precede Options
TEST_F(TokenTest, PrecedeOptions) {
    // ID and literals can be preceded by left unary or binary
    EXPECT_EQ(Token(Token::Type::ID, "x").precede_options(), 0b100 | 0b001);
    EXPECT_EQ(Token(Token::Type::INT, "42").precede_options(), 0b100 | 0b001);

    // Keywords can be preceded by right unary
    EXPECT_EQ(Token(Token::Type::LET).precede_options(), 0b010);

    // Open brackets can be preceded by left unary or binary
    EXPECT_EQ(Token(Token::Type::O_PAREN).precede_options(), 0b100 | 0b001);

    // Binary operators can be preceded by right unary
    EXPECT_EQ(Token(Token::Type::ADD).precede_options(), 0b010);

    // Right unary can be preceded by right unary
    EXPECT_EQ(Token(Token::Type::R_INC).precede_options(), 0b010);

    // Left unary can be preceded by left unary or binary
    EXPECT_EQ(Token(Token::Type::L_INC).precede_options(), 0b100 | 0b001);
}

// Test Follow Options
TEST_F(TokenTest, FollowOptions) {
    // C_BRACE can be followed by left unary
    EXPECT_EQ(Token(Token::Type::C_BRACE).follow_options(), 0b100);

    // ID, literals, and close brackets can be followed by right unary or binary
    EXPECT_EQ(Token(Token::Type::ID, "x").follow_options(), 0b010 | 0b001);
    EXPECT_EQ(Token(Token::Type::INT, "42").follow_options(), 0b010 | 0b001);
    EXPECT_EQ(Token(Token::Type::C_PAREN).follow_options(), 0b010 | 0b001);

    // Keywords can be followed by left unary
    EXPECT_EQ(Token(Token::Type::LET).follow_options(), 0b100);

    // Binary operators can be followed by left unary
    EXPECT_EQ(Token(Token::Type::ADD).follow_options(), 0b100);

    // Right unary can be followed by right unary or binary
    EXPECT_EQ(Token(Token::Type::R_INC).follow_options(), 0b010 | 0b001);

    // Left unary can be followed by left unary
    EXPECT_EQ(Token(Token::Type::L_INC).follow_options(), 0b100);
}

// Test Option
TEST_F(TokenTest, OptionValues) {
    EXPECT_EQ(Token(Token::Type::L_INC).option(), 0b100);
    EXPECT_EQ(Token(Token::Type::R_INC).option(), 0b010);
    EXPECT_EQ(Token(Token::Type::ADD).option(), 0b001);
    EXPECT_EQ(Token(Token::Type::ID, "x").option(), 0);
}

// Test Copy and Move
TEST_F(TokenTest, CopyConstructor) {
    Token tok1(Token::Type::ID, "test");
    Token tok2(tok1);

    EXPECT_EQ(tok2.type(), Token::Type::ID);
    EXPECT_STREQ(tok2.value(), "test");
}

TEST_F(TokenTest, CopyAssignment) {
    Token tok1(Token::Type::INT, "123");
    Token tok2(Token::Type::ID, "x");

    tok2 = tok1;

    EXPECT_EQ(tok2.type(), Token::Type::INT);
    EXPECT_STREQ(tok2.value(), "123");
}

TEST_F(TokenTest, MoveConstructor) {
    Token tok1(Token::Type::STR, "hello");
    Token tok2(std::move(tok1));

    EXPECT_EQ(tok2.type(), Token::Type::STR);
    EXPECT_STREQ(tok2.value(), "hello");
}

TEST_F(TokenTest, MoveAssignment) {
    Token tok1(Token::Type::FLOAT, "3.14");
    Token tok2(Token::Type::ID, "x");

    tok2 = std::move(tok1);

    EXPECT_EQ(tok2.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok2.value(), "3.14");
}

// Test All Token Values
TEST_F(TokenTest, AllTokenValues) {
    EXPECT_STREQ(Token(Token::Type::TRUE).value(), "true");
    EXPECT_STREQ(Token(Token::Type::FALSE).value(), "false");
    EXPECT_STREQ(Token(Token::Type::O_PAREN).value(), "(");
    EXPECT_STREQ(Token(Token::Type::C_PAREN).value(), ")");
    EXPECT_STREQ(Token(Token::Type::ARROW).value(), "->");
    EXPECT_STREQ(Token(Token::Type::DOUBLE_ARROW).value(), "=>");
    EXPECT_STREQ(Token(Token::Type::LET).value(), "let");
    EXPECT_STREQ(Token(Token::Type::FN).value(), "fn");
    EXPECT_STREQ(Token(Token::Type::SCOPE).value(), "::");
    EXPECT_STREQ(Token(Token::Type::RANGE).value(), "..");
    EXPECT_STREQ(Token(Token::Type::RANGE_EQ).value(), "..=");
}
