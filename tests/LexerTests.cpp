#include <gtest/gtest.h>
#include "Lexer.hpp"
#include "Token.hpp"

#include <sstream>
#include <memory>

using namespace flynt;

// Helper function to create a Lexer from a string
std::unique_ptr<Lexer> makeLexer(const std::string &input) {
    auto stream = std::make_unique<std::stringstream>(input);
    return std::make_unique<Lexer>(*stream);
}

// Test basic identifier lexing
TEST(LexerTest, Identifiers) {
    std::stringstream ss("myVar _underscore var123 _123abc");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);
    EXPECT_STREQ(tok1.value(), "myVar");

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);
    EXPECT_STREQ(tok2.value(), "_underscore");

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);
    EXPECT_STREQ(tok3.value(), "var123");

    Token tok4 = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::ID);
    EXPECT_STREQ(tok4.value(), "_123abc");
}

// Test integer lexing
TEST(LexerTest, Integers) {
    std::stringstream ss("0 42 12345 999");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::INT);
    EXPECT_STREQ(tok1.value(), "0");

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::INT);
    EXPECT_STREQ(tok2.value(), "42");

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::INT);
    EXPECT_STREQ(tok3.value(), "12345");

    Token tok4 = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::INT);
    EXPECT_STREQ(tok4.value(), "999");
}

// Test floating point lexing
TEST(LexerTest, Floats) {
    std::stringstream ss("3.14 0.5 123.456 .5 10.0");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok1.value(), "3.14");

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok2.value(), "0.5");

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok3.value(), "123.456");

    Token tok4 = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok4.value(), ".5");

    Token tok5 = lexer.lex();
    EXPECT_EQ(tok5.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok5.value(), "10.0");
}

// Test scientific notation
TEST(LexerTest, ScientificNotation) {
    std::stringstream ss("1e10 3.14e-5 2.5E+3 1E10");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok1.value(), "1e10");

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok2.value(), "3.14e-5");

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok3.value(), "2.5E+3");

    Token tok4 = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok4.value(), "1E10");
}

// Test range operator disambiguation
TEST(LexerTest, RangeOperatorWithNumber) {
    std::stringstream ss("1..10");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::INT);
    EXPECT_STREQ(tok1.value(), "1");

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::RANGE);

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::INT);
    EXPECT_STREQ(tok3.value(), "10");
}

// Test string lexing
TEST(LexerTest, Strings) {
    std::stringstream ss("\"hello\" \"world with spaces\" \"\"");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::STR);
    EXPECT_STREQ(tok1.value(), "hello");

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::STR);
    EXPECT_STREQ(tok2.value(), "world with spaces");

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::STR);
    EXPECT_STREQ(tok3.value(), "");
}

// Test string with escape sequences
TEST(LexerTest, StringEscapes) {
    std::stringstream ss("\"line1\\nline2\" \"quote: \\\"hi\\\"\" \"backslash: \\\\\"");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::STR);
    EXPECT_STREQ(tok1.value(), "line1\\nline2");

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::STR);
    EXPECT_STREQ(tok2.value(), "quote: \\\"hi\\\"");

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::STR);
    EXPECT_STREQ(tok3.value(), "backslash: \\\\");
}

// Test unterminated string
TEST(LexerTest, UnterminatedString) {
    std::stringstream ss("\"unclosed string");
    Lexer lexer(ss);

    Token tok = lexer.lex();
    EXPECT_EQ(tok.type(), Token::Type::UNKNOWN);
}

// Test character lexing
TEST(LexerTest, Characters) {
    std::stringstream ss("'a' 'z' '0' ' '");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::CHAR);
    EXPECT_STREQ(tok1.value(), "a");

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::CHAR);
    EXPECT_STREQ(tok2.value(), "z");

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::CHAR);
    EXPECT_STREQ(tok3.value(), "0");

    Token tok4 = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::CHAR);
    EXPECT_STREQ(tok4.value(), " ");
}

// Test character with escape sequences
TEST(LexerTest, CharacterEscapes) {
    std::stringstream ss("'\\n' '\\t' '\\'' '\\\\'");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::CHAR);
    EXPECT_STREQ(tok1.value(), "\\n");

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::CHAR);
    EXPECT_STREQ(tok2.value(), "\\t");

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::CHAR);
    EXPECT_STREQ(tok3.value(), "\\'");

    Token tok4 = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::CHAR);
    EXPECT_STREQ(tok4.value(), "\\\\");
}

// Test empty and unterminated char
TEST(LexerTest, InvalidCharacters) {
    std::stringstream ss1("''");
    Lexer lexer1(ss1);
    Token tok1 = lexer1.lex();
    EXPECT_EQ(tok1.type(), Token::Type::UNKNOWN);

    std::stringstream ss2("'unclosed");
    Lexer lexer2(ss2);
    Token tok2 = lexer2.lex();
    EXPECT_EQ(tok2.type(), Token::Type::UNKNOWN);
}

// Test all keywords
TEST(LexerTest, Keywords) {
    std::stringstream ss("let fn templ idea type trait priv pub stat fin abstr oper impl if else for until match enum space use asm");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::LET);
    EXPECT_EQ(lexer.lex().type(), Token::Type::FN);
    EXPECT_EQ(lexer.lex().type(), Token::Type::TEMPL);
    EXPECT_EQ(lexer.lex().type(), Token::Type::IDEA);
    EXPECT_EQ(lexer.lex().type(), Token::Type::TYPE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::TRAIT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::PRIV);
    EXPECT_EQ(lexer.lex().type(), Token::Type::PUB);
    EXPECT_EQ(lexer.lex().type(), Token::Type::STAT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::FIN);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ABSTR);
    EXPECT_EQ(lexer.lex().type(), Token::Type::OPER);
    EXPECT_EQ(lexer.lex().type(), Token::Type::IMPL);
    EXPECT_EQ(lexer.lex().type(), Token::Type::IF);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ELSE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::FOR);
    EXPECT_EQ(lexer.lex().type(), Token::Type::UNTIL);
    EXPECT_EQ(lexer.lex().type(), Token::Type::MATCH);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ENUM);
    EXPECT_EQ(lexer.lex().type(), Token::Type::SPACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::USE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ASM);
}

// Test boolean literals
TEST(LexerTest, Booleans) {
    std::stringstream ss("true false");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::TRUE);

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::FALSE);
}

// Test symbols
TEST(LexerTest, Symbols) {
    std::stringstream ss("( ) { } [ ] ; : -> =>");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::O_PAREN);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_PAREN);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_BRACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_BRACKET);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_BRACKET);
    EXPECT_EQ(lexer.lex().type(), Token::Type::SEMI_COLON);
    EXPECT_EQ(lexer.lex().type(), Token::Type::COLON);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ARROW);
    EXPECT_EQ(lexer.lex().type(), Token::Type::DOUBLE_ARROW);
}

// Test operators
TEST(LexerTest, Operators) {
    std::stringstream ss(":: << >> <= >= == != && || .. ..= ... alloc clean");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::SCOPE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::LS);
    EXPECT_EQ(lexer.lex().type(), Token::Type::RS);
    EXPECT_EQ(lexer.lex().type(), Token::Type::LE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::GE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::EE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::NE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::AND);
    EXPECT_EQ(lexer.lex().type(), Token::Type::OR);
    EXPECT_EQ(lexer.lex().type(), Token::Type::RANGE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::RANGE_EQ);
    EXPECT_EQ(lexer.lex().type(), Token::Type::L_SPREAD); // Non-vague context
    EXPECT_EQ(lexer.lex().type(), Token::Type::ALLOC);
    EXPECT_EQ(lexer.lex().type(), Token::Type::CLEAN);
}

// Test assignment operators
TEST(LexerTest, AssignmentOperators) {
    std::stringstream ss("= += -= *= /= %= <<= >>= &= ^= |=");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::EQ);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ADD_EQ);
    EXPECT_EQ(lexer.lex().type(), Token::Type::SUB_EQ);
    EXPECT_EQ(lexer.lex().type(), Token::Type::MUL_EQ);
    EXPECT_EQ(lexer.lex().type(), Token::Type::DIV_EQ);
    EXPECT_EQ(lexer.lex().type(), Token::Type::MOD_EQ);
    EXPECT_EQ(lexer.lex().type(), Token::Type::LS_EQ);
    EXPECT_EQ(lexer.lex().type(), Token::Type::RS_EQ);
    EXPECT_EQ(lexer.lex().type(), Token::Type::B_AND_EQ);
    EXPECT_EQ(lexer.lex().type(), Token::Type::B_XOR_EQ);
    EXPECT_EQ(lexer.lex().type(), Token::Type::B_OR_EQ);
}

// Test comparison operators
TEST(LexerTest, ComparisonOperators) {
    std::stringstream ss("< > in is as");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::LT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::GT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::IN);
    EXPECT_EQ(lexer.lex().type(), Token::Type::IS);
    EXPECT_EQ(lexer.lex().type(), Token::Type::AS);
}

// Test bitwise operators
TEST(LexerTest, BitwiseOperators) {
    std::stringstream ss("^ | ~ /");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::B_XOR);
    EXPECT_EQ(lexer.lex().type(), Token::Type::B_OR);
    EXPECT_EQ(lexer.lex().type(), Token::Type::L_B_NOT); // After nothing = right unary
    EXPECT_EQ(lexer.lex().type(), Token::Type::DIV);
}

// Test vague operator disambiguation: postfix increment
TEST(LexerTest, PostfixIncrement) {
    std::stringstream ss("x++;");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::R_INC); // After identifier = right unary

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::SEMI_COLON);
}

// Test vague operator disambiguation: prefix increment
TEST(LexerTest, PrefixIncrement) {
    std::stringstream ss("++x;");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::L_INC); // Before identifier = left unary

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::SEMI_COLON);
}

// Test vague operator disambiguation: unary plus
TEST(LexerTest, UnaryPlus) {
    std::stringstream ss("+5;");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::L_PLUS); // Before literal = right unary

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::INT);

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::SEMI_COLON);
}

// Test vague operator disambiguation: unary minus
TEST(LexerTest, UnaryMinus) {
    std::stringstream ss("-42;");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::L_MINUS); // Before literal = right unary

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::INT);

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::SEMI_COLON);
}

// Test vague operator disambiguation: binary addition
TEST(LexerTest, BinaryAddition) {
    std::stringstream ss("a + b;");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::ADD); // After ID, before ID = binary

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);

    Token tok4 = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::SEMI_COLON);
}

// Test vague operator disambiguation: dereference
TEST(LexerTest, Dereference) {
    std::stringstream ss("*ptr;");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::L_DEREF); // Before identifier = right unary

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::SEMI_COLON);
}

// Test vague operator disambiguation: multiplication
TEST(LexerTest, Multiplication) {
    std::stringstream ss("a * b;");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::MUL); // After ID, before ID = binary

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);

    Token tok4 = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::SEMI_COLON);
}

// Test vague operator disambiguation: address-of
TEST(LexerTest, AddressOf) {
    std::stringstream ss("&var;");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::L_REF); // Before identifier = right unary

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::SEMI_COLON);
}

// Test vague operator disambiguation: bitwise AND
TEST(LexerTest, BitwiseAND) {
    std::stringstream ss("a & b;");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::B_AND); // After ID, before ID = binary

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);

    Token tok4 = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::SEMI_COLON);
}

// Test complex expression with mixed operators
TEST(LexerTest, ComplexExpression) {
    std::stringstream ss("(x +) -y * 2;");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::O_PAREN);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::R_PLUS);

    EXPECT_EQ(lexer.lex().type(), Token::Type::C_PAREN);

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::SUB); // After binary, before ID = right unary

    Token tok4 = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::ID);

    Token tok5 = lexer.lex();
    EXPECT_EQ(tok5.type(), Token::Type::MUL);

    Token tok6 = lexer.lex();
    EXPECT_EQ(tok6.type(), Token::Type::INT);

    Token tok7 = lexer.lex();
    EXPECT_EQ(tok7.type(), Token::Type::SEMI_COLON);
}

// Test complex expression with mixed operators
TEST(LexerTest, UnexpectedComplexExpression) {
    std::stringstream ss("x + (-y) * 2;");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::ADD);

    EXPECT_EQ(lexer.lex().type(), Token::Type::O_PAREN);

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::L_MINUS); // After binary, before ID = right unary

    Token tok4 = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::ID);

    EXPECT_EQ(lexer.lex().type(), Token::Type::C_PAREN);

    Token tok5 = lexer.lex();
    EXPECT_EQ(tok5.type(), Token::Type::MUL);

    Token tok6 = lexer.lex();
    EXPECT_EQ(tok6.type(), Token::Type::INT);

    Token tok7 = lexer.lex();
    EXPECT_EQ(tok7.type(), Token::Type::SEMI_COLON);
}

// Test consecutive vague operators
TEST(LexerTest, ConsecutiveVagueOperators) {
    std::stringstream ss("++--x;");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::L_INC); // First operator, before -- = left

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::L_DEC); // After left, before ID = left

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);

    Token tok4 = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::SEMI_COLON);
}

// Test vague operators after parenthesis
TEST(LexerTest, VagueAfterParen) {
    std::stringstream ss("(+x);");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::O_PAREN);

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::L_PLUS); // After open paren = right unary

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);

    Token tok4 = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::C_PAREN);

    Token tok5 = lexer.lex();
    EXPECT_EQ(tok5.type(), Token::Type::SEMI_COLON);
}

// Test vague operators before parenthesis
TEST(LexerTest, VagueBeforeParen) {
    std::stringstream ss("x++()");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::R_INC); // After ID, before open paren

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::O_PAREN);
}

// Test peek functionality
TEST(LexerTest, Peek) {
    std::stringstream ss("a b c");
    Lexer lexer(ss);

    Token peeked1 = lexer.peek();
    EXPECT_EQ(peeked1.type(), Token::Type::ID);
    EXPECT_STREQ(peeked1.value(), "a");

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);
    EXPECT_STREQ(tok1.value(), "a");

    Token peeked2 = lexer.peek();
    EXPECT_EQ(peeked2.type(), Token::Type::ID);
    EXPECT_STREQ(peeked2.value(), "b");

    Token peeked3 = lexer.peek();
    EXPECT_EQ(peeked3.type(), Token::Type::ID);
    EXPECT_STREQ(peeked3.value(), "b");

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);
    EXPECT_STREQ(tok2.value(), "b");
}

// Test put_back functionality
TEST(LexerTest, PutBack) {
    std::stringstream ss("a b c");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);
    EXPECT_STREQ(tok1.value(), "a");

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);
    EXPECT_STREQ(tok2.value(), "b");

    lexer.put_back(tok2);

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);
    EXPECT_STREQ(tok3.value(), "b");

    Token tok4 = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::ID);
    EXPECT_STREQ(tok4.value(), "c");
}

// Test whitespace handling
TEST(LexerTest, Whitespace) {
    std::stringstream ss("  a   \n\t  b  \r\n  c  ");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);
    EXPECT_STREQ(tok1.value(), "a");

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);
    EXPECT_STREQ(tok2.value(), "b");

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);
    EXPECT_STREQ(tok3.value(), "c");
}

// Test spread operator disambiguation
TEST(LexerTest, SpreadOperator) {
    std::stringstream ss("...args;");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::L_SPREAD); // Before identifier

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::SEMI_COLON);
}

// Test dollar operator
TEST(LexerTest, DollarOperator) {
    std::stringstream ss("$x x$;");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::L_DOLLAR);

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);

    Token tok4 = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::R_DOLLAR); // After ID = should follow context

    Token tok5 = lexer.lex();
    EXPECT_EQ(tok5.type(), Token::Type::SEMI_COLON);
}

// Test question mark operator
TEST(LexerTest, QuestionMark) {
    std::stringstream ss("?x x?;");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::L_Q_MARK);

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);

    Token tok4 = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::R_Q_MARK);

    Token tok5 = lexer.lex();
    EXPECT_EQ(tok5.type(), Token::Type::SEMI_COLON);
}

// Test modulo operator
TEST(LexerTest, ModuloOperator) {
    std::stringstream ss("a % b");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::MOD);

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);
}

// Test dot operator
TEST(LexerTest, DotOperator) {
    std::stringstream ss("obj.method");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::DOT);

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);
}

// Test real-world code snippet
TEST(LexerTest, RealWorldCodeSnippet) {
    std::stringstream ss("let x = 5 + 3 * 2;");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::LET);

    Token id = lexer.lex();
    EXPECT_EQ(id.type(), Token::Type::ID);
    EXPECT_STREQ(id.value(), "x");

    EXPECT_EQ(lexer.lex().type(), Token::Type::EQ);

    Token num1 = lexer.lex();
    EXPECT_EQ(num1.type(), Token::Type::INT);
    EXPECT_STREQ(num1.value(), "5");

    EXPECT_EQ(lexer.lex().type(), Token::Type::ADD);

    Token num2 = lexer.lex();
    EXPECT_EQ(num2.type(), Token::Type::INT);
    EXPECT_STREQ(num2.value(), "3");

    EXPECT_EQ(lexer.lex().type(), Token::Type::MUL);

    Token num3 = lexer.lex();
    EXPECT_EQ(num3.type(), Token::Type::INT);
    EXPECT_STREQ(num3.value(), "2");

    EXPECT_EQ(lexer.lex().type(), Token::Type::SEMI_COLON);
}

// Test function declaration
TEST(LexerTest, FunctionDeclaration) {
    std::stringstream ss("fn add(a: int, b: int) -> int { }");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::FN);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_PAREN);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::COLON);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::COMMA);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::COLON);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_PAREN);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ARROW);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_BRACE);
}

// Test array indexing
TEST(LexerTest, ArrayIndexing) {
    std::stringstream ss("arr[0]");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_BRACKET);
    EXPECT_EQ(lexer.lex().type(), Token::Type::INT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_BRACKET);
}

// Test pointer operations
TEST(LexerTest, PointerOperations) {
    std::stringstream ss("*ptr = &value;");
    Lexer lexer(ss);

    Token tok1 = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::L_DEREF);

    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::EQ);

    Token tok2 = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::L_REF);

    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);

    Token tok3 = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::SEMI_COLON);
}

// Test if-else statement
TEST(LexerTest, IfElseStatement) {
    std::stringstream ss("if x > 5 { } else { }");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::IF);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::GT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::INT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_BRACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ELSE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_BRACE);
}

// Test for loop
TEST(LexerTest, ForLoop) {
    std::stringstream ss("for i in 0..10 { }");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::FOR);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::IN);
    EXPECT_EQ(lexer.lex().type(), Token::Type::INT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::RANGE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::INT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_BRACE);
}

// Test match expression
TEST(LexerTest, MatchExpression) {
    std::stringstream ss("match x { }");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::MATCH);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_BRACE);
}

// Test type declaration
TEST(LexerTest, TypeDeclaration) {
    std::stringstream ss("type Point = { x: int, y: int };");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::TYPE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::EQ);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::COLON);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::COMMA);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::COLON);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_BRACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::SEMI_COLON);
}

// Test scope resolution
TEST(LexerTest, ScopeResolution) {
    std::stringstream ss("std::vector");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::SCOPE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
}

// Test lambda expression
TEST(LexerTest, LambdaExpression) {
    std::stringstream ss("fn(x) => x + 1;");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::FN);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_PAREN);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_PAREN);
    EXPECT_EQ(lexer.lex().type(), Token::Type::DOUBLE_ARROW);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ADD);
    EXPECT_EQ(lexer.lex().type(), Token::Type::INT);

    Token tok = lexer.lex();
    EXPECT_EQ(tok.type(), Token::Type::SEMI_COLON);
}

// Test complex operator expression
TEST(LexerTest, ComplexOperatorExpression) {
    std::stringstream ss("a++ + ++b;");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::R_INC);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ADD);
    EXPECT_EQ(lexer.lex().type(), Token::Type::L_INC);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);

    Token tok = lexer.lex();
    EXPECT_EQ(tok.type(), Token::Type::SEMI_COLON);
}

// Test bitwise operations
TEST(LexerTest, BitwiseOperations) {
    std::stringstream ss("a & b | c ^ d");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::B_AND);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::B_OR);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::B_XOR);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
}

// Test logical operations
TEST(LexerTest, LogicalOperations) {
    std::stringstream ss("a and b or c");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::AND);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::OR);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
}

// Test shift operations
TEST(LexerTest, ShiftOperations) {
    std::stringstream ss("a << 2 >> 1");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::LS);
    EXPECT_EQ(lexer.lex().type(), Token::Type::INT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::RS);
    EXPECT_EQ(lexer.lex().type(), Token::Type::INT);
}

// Test compound assignment
TEST(LexerTest, CompoundAssignment) {
    std::stringstream ss("x += 5");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ADD_EQ);
    EXPECT_EQ(lexer.lex().type(), Token::Type::INT);
}

// Test range operators
TEST(LexerTest, RangeOperators) {
    std::stringstream ss("0..10 0..=10");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::INT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::RANGE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::INT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::INT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::RANGE_EQ);
    EXPECT_EQ(lexer.lex().type(), Token::Type::INT);
}

// Test trait implementation
TEST(LexerTest, TraitImplementation) {
    std::stringstream ss("impl Trait for Type { }");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::IMPL);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::FOR);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_BRACE);
}

// Test visibility modifiers
TEST(LexerTest, VisibilityModifiers) {
    std::stringstream ss("pub priv stat fin abstr");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::PUB);
    EXPECT_EQ(lexer.lex().type(), Token::Type::PRIV);
    EXPECT_EQ(lexer.lex().type(), Token::Type::STAT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::FIN);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ABSTR);
}

// Test template/generics
TEST(LexerTest, Template) {
    std::stringstream ss("templ<T>");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::TEMPL);
    EXPECT_EQ(lexer.lex().type(), Token::Type::LT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::GT);
}

// Test enum declaration
TEST(LexerTest, EnumDeclaration) {
    std::stringstream ss("enum Color { }");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::ENUM);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_BRACE);
}

// Test namespace/space
TEST(LexerTest, Namespace) {
    std::stringstream ss("space myspace { }");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::SPACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_BRACE);
}

// Test use statement
TEST(LexerTest, UseStatement) {
    std::stringstream ss("use std::io;");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::USE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::SCOPE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::SEMI_COLON);
}

// Test inline assembly
TEST(LexerTest, InlineAssembly) {
    std::stringstream ss("asm { }");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::ASM);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_BRACE);
}

// Test operator overloading
TEST(LexerTest, OperatorOverloading) {
    std::stringstream ss("oper + (a, b) { }");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::OPER);

    Token add_tok = lexer.lex();
    EXPECT_TRUE(add_tok.type() == Token::Type::ADD || add_tok.type() == Token::Type::R_PLUS || add_tok.type() == Token::Type::L_PLUS);

    EXPECT_EQ(lexer.lex().type(), Token::Type::O_PAREN);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::COMMA);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_PAREN);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_BRACE);
}

// Test multiple vague operators in sequence
TEST(LexerTest, MultipleVagueOperators) {
    std::stringstream ss("a+++b;");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::R_INC);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ADD);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);

    Token tok = lexer.lex();
    EXPECT_EQ(tok.type(), Token::Type::SEMI_COLON);
}

// Test negative number vs subtraction
TEST(LexerTest, NegativeNumberVsSubtraction) {
    std::stringstream ss("x-5; x - 5;");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::SUB);
    EXPECT_EQ(lexer.lex().type(), Token::Type::INT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::SEMI_COLON);

    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::SUB);
    EXPECT_EQ(lexer.lex().type(), Token::Type::INT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::SEMI_COLON);
}

// Test decrement operator context
TEST(LexerTest, DecrementOperator) {
    std::stringstream ss("--x x--;");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::L_DEC);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::R_DEC);
    EXPECT_EQ(lexer.lex().type(), Token::Type::SEMI_COLON);
}

// Test not operator
TEST(LexerTest, NotOperator) {
    std::stringstream ss("!x");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::L_NOT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
}

// Test tilde operator
TEST(LexerTest, TildeOperator) {
    std::stringstream ss("~x");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::L_B_NOT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
}

// Test edge case: single dot
TEST(LexerTest, SingleDot) {
    std::stringstream ss(". x");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::DOT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
}

// Test empty input
TEST(LexerTest, EmptyInput) {
    std::stringstream ss("");
    Lexer lexer(ss);

    Token tok = lexer.lex();
    EXPECT_EQ(tok.type(), Token::Type::UNKNOWN);
}

// Test whitespace only
TEST(LexerTest, WhitespaceOnly) {
    std::stringstream ss("   \n\t  ");
    Lexer lexer(ss);

    Token tok = lexer.lex();
    EXPECT_EQ(tok.type(), Token::Type::UNKNOWN);
}

// Test longest match for operators
TEST(LexerTest, LongestOperatorMatch) {
    std::stringstream ss("<<= >>= ..= ===");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::LS_EQ);
    EXPECT_EQ(lexer.lex().type(), Token::Type::RS_EQ);
    EXPECT_EQ(lexer.lex().type(), Token::Type::RANGE_EQ);
    EXPECT_EQ(lexer.lex().type(), Token::Type::EE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::EQ);
}

// Test vague operators after literals
TEST(LexerTest, VagueAfterLiterals) {
    std::stringstream ss("5++;");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::INT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::R_INC);
    EXPECT_EQ(lexer.lex().type(), Token::Type::SEMI_COLON);
}

// Test vague operators after closing brackets
TEST(LexerTest, VagueAfterClosingBracket) {
    std::stringstream ss("arr[0]++;");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_BRACKET);
    EXPECT_EQ(lexer.lex().type(), Token::Type::INT);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_BRACKET);
    EXPECT_EQ(lexer.lex().type(), Token::Type::R_INC);
    EXPECT_EQ(lexer.lex().type(), Token::Type::SEMI_COLON);
}

// Test until keyword
TEST(LexerTest, UntilKeyword) {
    std::stringstream ss("until condition { }");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::UNTIL);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_BRACE);
}

// Test idea keyword
TEST(LexerTest, IdeaKeyword) {
    std::stringstream ss("idea MyInterface { }");
    Lexer lexer(ss);

    EXPECT_EQ(lexer.lex().type(), Token::Type::IDEA);
    EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer.lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer.lex().type(), Token::Type::C_BRACE);
}

TEST(LexerTest, Comments) {
	std::stringstream ss("/* hello world! */x;//hi");
	Lexer lexer(ss);

	EXPECT_EQ(lexer.lex().type(), Token::Type::ID);
	EXPECT_EQ(lexer.lex().type(), Token::Type::SEMI_COLON);
}
