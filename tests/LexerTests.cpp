#include <gtest/gtest.h>
#include "Lexer.hpp"
#include "Token.hpp"
#include <sstream>
#include <string>

using namespace flynt;

class LexerTest : public ::testing::Test {
protected:
    void SetUp() override {}
    void TearDown() override {}

    Token::Type getTokenType(const std::string& input) {
        std::istringstream iss(input);
        Lexer lexer(iss);
        return lexer.lex().first.type();
    }

    std::string getTokenValue(const std::string& input) {
        std::istringstream iss(input);
        Lexer lexer(iss);
        return lexer.lex().first.value();
    }

    Lexer::FatToken getFatToken(const std::string& input) {
        std::istringstream iss(input);
        Lexer lexer(iss);
        return lexer.lex();
    }
};

// Test Basic Token Recognition
TEST_F(LexerTest, RecognizeIdentifiers) {
    EXPECT_EQ(getTokenType("variable"), Token::Type::ID);
    EXPECT_EQ(getTokenValue("variable"), "variable");

    EXPECT_EQ(getTokenType("_test"), Token::Type::ID);
    EXPECT_EQ(getTokenValue("_test"), "_test");

    EXPECT_EQ(getTokenType("test123"), Token::Type::ID);
    EXPECT_EQ(getTokenValue("test123"), "test123");

    EXPECT_EQ(getTokenType("Test_Case_1"), Token::Type::ID);
    EXPECT_EQ(getTokenValue("Test_Case_1"), "Test_Case_1");
}

// Test Integer Literals
TEST_F(LexerTest, RecognizeIntegers) {
    EXPECT_EQ(getTokenType("0"), Token::Type::INT);
    EXPECT_EQ(getTokenValue("0"), "0");

    EXPECT_EQ(getTokenType("123"), Token::Type::INT);
    EXPECT_EQ(getTokenValue("123"), "123");

    EXPECT_EQ(getTokenType("999999"), Token::Type::INT);
    EXPECT_EQ(getTokenValue("999999"), "999999");
}

// Test Float Literals
TEST_F(LexerTest, RecognizeFloats) {
    EXPECT_EQ(getTokenType("3.14"), Token::Type::FLOAT);
    EXPECT_EQ(getTokenValue("3.14"), "3.14");

    EXPECT_EQ(getTokenType("0.5"), Token::Type::FLOAT);
    EXPECT_EQ(getTokenValue("0.5"), "0.5");

    EXPECT_EQ(getTokenType(".5"), Token::Type::FLOAT);
    EXPECT_EQ(getTokenValue(".5"), ".5");

    EXPECT_EQ(getTokenType("123.456"), Token::Type::FLOAT);
    EXPECT_EQ(getTokenValue("123.456"), "123.456");
}

// Test Scientific Notation
TEST_F(LexerTest, RecognizeScientificNotation) {
    EXPECT_EQ(getTokenType("1e10"), Token::Type::FLOAT);
    EXPECT_EQ(getTokenValue("1e10"), "1e10");

    EXPECT_EQ(getTokenType("1.5e10"), Token::Type::FLOAT);
    EXPECT_EQ(getTokenValue("1.5e10"), "1.5e10");

    EXPECT_EQ(getTokenType("2.5e-3"), Token::Type::FLOAT);
    EXPECT_EQ(getTokenValue("2.5e-3"), "2.5e-3");

    EXPECT_EQ(getTokenType("1E+5"), Token::Type::FLOAT);
    EXPECT_EQ(getTokenValue("1E+5"), "1E+5");
}

// Test String Literals
TEST_F(LexerTest, RecognizeStrings) {
    EXPECT_EQ(getTokenType("\"hello\""), Token::Type::STR);
    EXPECT_EQ(getTokenValue("\"hello\""), "hello");

    EXPECT_EQ(getTokenType("\"\""), Token::Type::STR);
    EXPECT_EQ(getTokenValue("\"\""), "");

    EXPECT_EQ(getTokenType("\"hello world\""), Token::Type::STR);
    EXPECT_EQ(getTokenValue("\"hello world\""), "hello world");
}

// Test String Escape Sequences
TEST_F(LexerTest, RecognizeStringEscapes) {
    EXPECT_EQ(getTokenType("\"hello\\nworld\""), Token::Type::STR);
    EXPECT_EQ(getTokenValue("\"hello\\nworld\""), "hello\\nworld");

    EXPECT_EQ(getTokenType("\"quote:\\\"\""), Token::Type::STR);
    EXPECT_EQ(getTokenValue("\"quote:\\\"\""), "quote:\\\"");

    EXPECT_EQ(getTokenType("\"backslash:\\\\\""), Token::Type::STR);
    EXPECT_EQ(getTokenValue("\"backslash:\\\\\""), "backslash:\\\\");
}

// Test Unterminated String
TEST_F(LexerTest, UnterminatedString) {
    EXPECT_EQ(getTokenType("\"unterminated"), Token::Type::UNKNOWN);
}

// Test Character Literals
TEST_F(LexerTest, RecognizeCharacters) {
    EXPECT_EQ(getTokenType("'a'"), Token::Type::CHAR);
    EXPECT_EQ(getTokenValue("'a'"), "a");

    EXPECT_EQ(getTokenType("'Z'"), Token::Type::CHAR);
    EXPECT_EQ(getTokenValue("'Z'"), "Z");

    EXPECT_EQ(getTokenType("'0'"), Token::Type::CHAR);
    EXPECT_EQ(getTokenValue("'0'"), "0");
}

// Test Character Escape Sequences
TEST_F(LexerTest, RecognizeCharEscapes) {
    EXPECT_EQ(getTokenType("'\\n'"), Token::Type::CHAR);
    EXPECT_EQ(getTokenValue("'\\n'"), "\\n");

    EXPECT_EQ(getTokenType("'\\''"), Token::Type::CHAR);
    EXPECT_EQ(getTokenValue("'\\''"), "\\'");
}

// Test Empty and Unterminated Char
TEST_F(LexerTest, InvalidCharLiterals) {
    EXPECT_EQ(getTokenType("''"), Token::Type::UNKNOWN);
    EXPECT_EQ(getTokenType("'unterminated"), Token::Type::UNKNOWN);
}

// Test Keywords
TEST_F(LexerTest, RecognizeKeywords) {
    EXPECT_EQ(getTokenType("let"), Token::Type::LET);
    EXPECT_EQ(getTokenType("fn"), Token::Type::FN);
    EXPECT_EQ(getTokenType("type"), Token::Type::TYPE);
    EXPECT_EQ(getTokenType("trait"), Token::Type::TRAIT);
    EXPECT_EQ(getTokenType("if"), Token::Type::IF);
    EXPECT_EQ(getTokenType("else"), Token::Type::ELSE);
    EXPECT_EQ(getTokenType("for"), Token::Type::FOR);
    EXPECT_EQ(getTokenType("match"), Token::Type::MATCH);
    EXPECT_EQ(getTokenType("enum"), Token::Type::ENUM);
    EXPECT_EQ(getTokenType("use"), Token::Type::USE);
    EXPECT_EQ(getTokenType("asm"), Token::Type::ASM);
}

// Test Keywords Not Matching Partial Identifiers
TEST_F(LexerTest, KeywordsNotPartialMatch) {
    EXPECT_EQ(getTokenType("letter"), Token::Type::ID);
    EXPECT_EQ(getTokenType("function"), Token::Type::ID);
    EXPECT_EQ(getTokenType("iffy"), Token::Type::ID);
    EXPECT_EQ(getTokenType("format"), Token::Type::ID);
}

// Test Boolean Literals
TEST_F(LexerTest, RecognizeBooleans) {
    EXPECT_EQ(getTokenType("true"), Token::Type::TRUE);
    EXPECT_EQ(getTokenType("false"), Token::Type::FALSE);
}

// Test Symbols
TEST_F(LexerTest, RecognizeSymbols) {
    EXPECT_EQ(getTokenType("("), Token::Type::O_PAREN);
    EXPECT_EQ(getTokenType(")"), Token::Type::C_PAREN);
    EXPECT_EQ(getTokenType("{"), Token::Type::O_BRACE);
    EXPECT_EQ(getTokenType("}"), Token::Type::C_BRACE);
    EXPECT_EQ(getTokenType("["), Token::Type::O_BRACKET);
    EXPECT_EQ(getTokenType("]"), Token::Type::C_BRACKET);
    EXPECT_EQ(getTokenType(";"), Token::Type::SEMI_COLON);
    EXPECT_EQ(getTokenType(":"), Token::Type::COLON);
    EXPECT_EQ(getTokenType("->"), Token::Type::ARROW);
    EXPECT_EQ(getTokenType("=>"), Token::Type::DOUBLE_ARROW);
    EXPECT_EQ(getTokenType(","), Token::Type::COMMA);
}

// Test Operators
TEST_F(LexerTest, RecognizeOperators) {
    EXPECT_EQ(getTokenType("::"), Token::Type::SCOPE);
    EXPECT_EQ(getTokenType("++"), Token::Type::R_INC);
    EXPECT_EQ(getTokenType("--"), Token::Type::R_DEC);
    EXPECT_EQ(getTokenType("."), Token::Type::DOT);
    EXPECT_EQ(getTokenType("*"), Token::Type::MUL);
    EXPECT_EQ(getTokenType("/"), Token::Type::DIV);
    EXPECT_EQ(getTokenType("%"), Token::Type::MOD);
    EXPECT_EQ(getTokenType("+"), Token::Type::ADD);
    EXPECT_EQ(getTokenType("-"), Token::Type::SUB);
}

// Test Comparison Operators
TEST_F(LexerTest, RecognizeComparisonOperators) {
    EXPECT_EQ(getTokenType("<"), Token::Type::LT);
    EXPECT_EQ(getTokenType("<="), Token::Type::LE);
    EXPECT_EQ(getTokenType(">"), Token::Type::GT);
    EXPECT_EQ(getTokenType(">="), Token::Type::GE);
    EXPECT_EQ(getTokenType("=="), Token::Type::EE);
    EXPECT_EQ(getTokenType("!="), Token::Type::NE);
}

// Test Logical Operators
TEST_F(LexerTest, RecognizeLogicalOperators) {
    EXPECT_EQ(getTokenType("and"), Token::Type::AND);
    EXPECT_EQ(getTokenType("or"), Token::Type::OR);
    EXPECT_EQ(getTokenType("!"), Token::Type::R_NOT);
}

// Test Bitwise Operators
TEST_F(LexerTest, RecognizeBitwiseOperators) {
    EXPECT_EQ(getTokenType("&"), Token::Type::B_AND);
    EXPECT_EQ(getTokenType("|"), Token::Type::B_OR);
    EXPECT_EQ(getTokenType("^"), Token::Type::B_XOR);
    EXPECT_EQ(getTokenType("~"), Token::Type::R_B_NOT);
    EXPECT_EQ(getTokenType("<<"), Token::Type::LS);
    EXPECT_EQ(getTokenType(">>"), Token::Type::RS);
}

// Test Assignment Operators
TEST_F(LexerTest, RecognizeAssignmentOperators) {
    EXPECT_EQ(getTokenType("="), Token::Type::EQ);
    EXPECT_EQ(getTokenType("+="), Token::Type::ADD_EQ);
    EXPECT_EQ(getTokenType("-="), Token::Type::SUB_EQ);
    EXPECT_EQ(getTokenType("*="), Token::Type::MUL_EQ);
    EXPECT_EQ(getTokenType("/="), Token::Type::DIV_EQ);
    EXPECT_EQ(getTokenType("%="), Token::Type::MOD_EQ);
    EXPECT_EQ(getTokenType("<<="), Token::Type::LS_EQ);
    EXPECT_EQ(getTokenType(">>="), Token::Type::RS_EQ);
    EXPECT_EQ(getTokenType("&="), Token::Type::B_AND_EQ);
    EXPECT_EQ(getTokenType("^="), Token::Type::B_XOR_EQ);
    EXPECT_EQ(getTokenType("|="), Token::Type::B_OR_EQ);
}

// Test Range Operators
TEST_F(LexerTest, RecognizeRangeOperators) {
    EXPECT_EQ(getTokenType(".."), Token::Type::RANGE);
    EXPECT_EQ(getTokenType("..="), Token::Type::RANGE_EQ);
    EXPECT_EQ(getTokenType("..."), Token::Type::R_SPREAD);
}

// Test Special Keywords
TEST_F(LexerTest, RecognizeSpecialKeywords) {
    EXPECT_EQ(getTokenType("in"), Token::Type::IN);
    EXPECT_EQ(getTokenType("is"), Token::Type::IS);
    EXPECT_EQ(getTokenType("as"), Token::Type::AS);
    EXPECT_EQ(getTokenType("alloc"), Token::Type::ALLOC);
    EXPECT_EQ(getTokenType("clean"), Token::Type::CLEAN);
}

// Test Whitespace Handling
TEST_F(LexerTest, SkipWhitespace) {
    std::istringstream iss("  \t\n  x");
    Lexer lexer(iss);
    auto [tok, pos] = lexer.lex();

    EXPECT_EQ(tok.type(), Token::Type::ID);
    EXPECT_EQ(tok.value(), "x");
}

// Test Single Line Comments
TEST_F(LexerTest, SkipSingleLineComments) {
    std::istringstream iss("// this is a comment\nx");
    Lexer lexer(iss);
    auto [tok, pos] = lexer.lex();

    EXPECT_EQ(tok.type(), Token::Type::ID);
    EXPECT_EQ(tok.value(), "x");
}

// Test Multi-line Comments
TEST_F(LexerTest, SkipMultiLineComments) {
    std::istringstream iss("/* this is a\nmulti-line comment */\ny");
    Lexer lexer(iss);
    auto [tok, pos] = lexer.lex();

    EXPECT_EQ(tok.type(), Token::Type::ID);
    EXPECT_EQ(tok.value(), "y");
}

// Test Unterminated Multi-line Comment
TEST_F(LexerTest, UnterminatedMultiLineComment) {
    std::istringstream iss("/* unterminated comment");
    Lexer lexer(iss);
    auto [tok, pos] = lexer.lex();

    EXPECT_EQ(tok.type(), Token::Type::UNKNOWN);
}

// Test Division vs Comment
TEST_F(LexerTest, DivisionNotComment) {
    std::istringstream iss("a/b");
    Lexer lexer(iss);

    auto [tok1, pos1] = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);
    EXPECT_EQ(tok1.value(), "a");

    auto [tok2, pos2] = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::DIV);

    auto [tok3, pos3] = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);
    EXPECT_EQ(tok3.value(), "b");
}

// Test Multiple Tokens
TEST_F(LexerTest, MultipleTokens) {
    std::istringstream iss("let x = 42;");
    Lexer lexer(iss);

    auto [tok1, pos1] = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::LET);

    auto [tok2, pos2] = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);
    EXPECT_EQ(tok2.value(), "x");

    auto [tok3, pos3] = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::EQ);

    auto [tok4, pos4] = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::INT);
    EXPECT_EQ(tok4.value(), "42");

    auto [tok5, pos5] = lexer.lex();
    EXPECT_EQ(tok5.type(), Token::Type::SEMI_COLON);
}

// Test Peek
TEST_F(LexerTest, PeekDoesNotConsume) {
    std::istringstream iss("abc");
    Lexer lexer(iss);

    auto [peek1, pos1] = lexer.peek();
    EXPECT_EQ(peek1.type(), Token::Type::ID);
    EXPECT_EQ(peek1.value(), "abc");

    auto [peek2, pos2] = lexer.peek();
    EXPECT_EQ(peek2.type(), Token::Type::ID);
    EXPECT_EQ(peek2.value(), "abc");

    auto [tok, pos3] = lexer.lex();
    EXPECT_EQ(tok.type(), Token::Type::ID);
    EXPECT_EQ(tok.value(), "abc");
}

// Test Position Tracking
TEST_F(LexerTest, PositionTracking) {
    std::istringstream iss("x\ny");
    Lexer lexer(iss);

    auto [tok1, pos1] = lexer.lex();
    EXPECT_EQ(pos1.line, 0);
    EXPECT_EQ(pos1.column, 0);

    auto [tok2, pos2] = lexer.lex();
    EXPECT_EQ(pos2.line, 1);
    EXPECT_EQ(pos2.column, 0);
}

// Test Vague Operator Resolution - Unary Plus/Minus
TEST_F(LexerTest, UnaryPlusResolution) {
    std::istringstream iss("+ x");
    Lexer lexer(iss);

    auto [tok1, pos1] = lexer.lex();
    EXPECT_TRUE(tok1.type() == Token::Type::R_PLUS || tok1.type() == Token::Type::L_PLUS);
}

TEST_F(LexerTest, BinaryPlusResolution) {
    std::istringstream iss("x + y");
    Lexer lexer(iss);

    lexer.lex(); // x
    auto [tok, pos] = lexer.lex();
    EXPECT_EQ(tok.type(), Token::Type::ADD);
}

// Test Increment/Decrement Resolution
TEST_F(LexerTest, PostfixIncrement) {
    std::istringstream iss("x++");
    Lexer lexer(iss);

    lexer.lex(); // x
    auto [tok, pos] = lexer.lex();
    EXPECT_EQ(tok.type(), Token::Type::L_INC);
}

TEST_F(LexerTest, PrefixIncrement) {
    std::istringstream iss("++x");
    Lexer lexer(iss);

    auto [tok, pos] = lexer.lex();
    EXPECT_EQ(tok.type(), Token::Type::R_INC);
}

// Test Dereference vs Multiplication
TEST_F(LexerTest, DereferenceResolution) {
    std::istringstream iss("*ptr");
    Lexer lexer(iss);

    auto [tok, pos] = lexer.lex();
    EXPECT_TRUE(tok.type() == Token::Type::R_DEREF || tok.type() == Token::Type::L_DEREF);
}

TEST_F(LexerTest, MultiplicationResolution) {
    std::istringstream iss("a * b");
    Lexer lexer(iss);

    lexer.lex(); // a
    auto [tok, pos] = lexer.lex();
    EXPECT_EQ(tok.type(), Token::Type::MUL);
}

// Test Reference vs Bitwise AND
TEST_F(LexerTest, ReferenceResolution) {
    std::istringstream iss("&x");
    Lexer lexer(iss);

    auto [tok, pos] = lexer.lex();
    EXPECT_TRUE(tok.type() == Token::Type::R_REF || tok.type() == Token::Type::L_REF);
}

TEST_F(LexerTest, BitwiseAndResolution) {
    std::istringstream iss("a & b");
    Lexer lexer(iss);

    lexer.lex(); // a
    auto [tok, pos] = lexer.lex();
    EXPECT_EQ(tok.type(), Token::Type::B_AND);
}

// Test Range Operator Not Confused with Float
TEST_F(LexerTest, RangeVsFloat) {
    std::istringstream iss("1..10");
    Lexer lexer(iss);

    auto [tok1, pos1] = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::INT);
    EXPECT_EQ(tok1.value(), "1");

    auto [tok2, pos2] = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::RANGE);

    auto [tok3, pos3] = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::INT);
    EXPECT_EQ(tok3.value(), "10");
}

// Test Complex Expression
TEST_F(LexerTest, ComplexExpression) {
    std::istringstream iss("(a + b) * c");
    Lexer lexer(iss);

    auto [tok1, pos1] = lexer.lex();
    EXPECT_EQ(tok1.type(), Token::Type::O_PAREN);

    auto [tok2, pos2] = lexer.lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);
    EXPECT_EQ(tok2.value(), "a");

    auto [tok3, pos3] = lexer.lex();
    EXPECT_EQ(tok3.type(), Token::Type::ADD);

    auto [tok4, pos4] = lexer.lex();
    EXPECT_EQ(tok4.type(), Token::Type::ID);
    EXPECT_EQ(tok4.value(), "b");

    auto [tok5, pos5] = lexer.lex();
    EXPECT_EQ(tok5.type(), Token::Type::C_PAREN);

    auto [tok6, pos6] = lexer.lex();
    EXPECT_EQ(tok6.type(), Token::Type::MUL);

    auto [tok7, pos7] = lexer.lex();
    EXPECT_EQ(tok7.type(), Token::Type::ID);
    EXPECT_EQ(tok7.value(), "c");
}

// Test && and || operators
TEST_F(LexerTest, LogicalAndOperator) {
    std::istringstream iss("&&");
    Lexer lexer(iss);

    auto [tok, pos] = lexer.lex();
    EXPECT_EQ(tok.type(), Token::Type::AND);
}

TEST_F(LexerTest, LogicalOrOperator) {
    std::istringstream iss("||");
    Lexer lexer(iss);

    auto [tok, pos] = lexer.lex();
    EXPECT_EQ(tok.type(), Token::Type::OR);
}

// Test edge case: single & or | should be bitwise
TEST_F(LexerTest, SingleAmpersandIsBitwise) {
    std::istringstream iss("& x");
    Lexer lexer(iss);

    auto [tok, pos] = lexer.lex();
    EXPECT_TRUE(tok.type() == Token::Type::B_AND ||
                tok.type() == Token::Type::R_REF ||
                tok.type() == Token::Type::L_REF);
}

// Test Empty Input
TEST_F(LexerTest, EmptyInput) {
    std::istringstream iss("");
    Lexer lexer(iss);

    auto [tok, pos] = lexer.lex();
    EXPECT_EQ(tok.type(), Token::Type::UNKNOWN);
}

// Test Only Whitespace
TEST_F(LexerTest, OnlyWhitespace) {
    std::istringstream iss("   \t\n  ");
    Lexer lexer(iss);

    auto [tok, pos] = lexer.lex();
    EXPECT_EQ(tok.type(), Token::Type::UNKNOWN);
}

// Test Only Comments
TEST_F(LexerTest, OnlyComments) {
    std::istringstream iss("// comment\n/* another */");
    Lexer lexer(iss);

    auto [tok, pos] = lexer.lex();
    EXPECT_EQ(tok.type(), Token::Type::UNKNOWN);
}
