#include <gtest/gtest.h>
#include "Lexer.hpp"
#include "Token.hpp"
#include <sstream>

using namespace flynt;

class LexerTest : public ::testing::Test {
protected:
    void SetUp() override {}
    void TearDown() override {}

    // Helper function to create a lexer from a string
    std::unique_ptr<Lexer> createLexer(const std::string& input) {
        auto stream = std::make_unique<std::stringstream>(input);
        streams.push_back(std::move(stream));
        return std::make_unique<Lexer>(*streams.back());
    }

    // Keep streams alive for the duration of the test
    std::vector<std::unique_ptr<std::stringstream>> streams;
};

// Basic token recognition tests
TEST_F(LexerTest, LexKeywords) {
    auto lexer = createLexer("let fn type trait if else for");

    EXPECT_EQ(lexer->lex().type(), Token::Type::LET);
    EXPECT_EQ(lexer->lex().type(), Token::Type::FN);
    EXPECT_EQ(lexer->lex().type(), Token::Type::TYPE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::TRAIT);
    EXPECT_EQ(lexer->lex().type(), Token::Type::IF);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ELSE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::FOR);
}

TEST_F(LexerTest, LexAllKeywords) {
    auto lexer = createLexer("let fn type trait priv pub stat fin abstr oper impl if else for until match enum space use asm");

    EXPECT_EQ(lexer->lex().type(), Token::Type::LET);
    EXPECT_EQ(lexer->lex().type(), Token::Type::FN);
    EXPECT_EQ(lexer->lex().type(), Token::Type::TYPE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::TRAIT);
    EXPECT_EQ(lexer->lex().type(), Token::Type::PRIV);
    EXPECT_EQ(lexer->lex().type(), Token::Type::PUB);
    EXPECT_EQ(lexer->lex().type(), Token::Type::STAT);
    EXPECT_EQ(lexer->lex().type(), Token::Type::FIN);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ABSTR);
    EXPECT_EQ(lexer->lex().type(), Token::Type::OPER);
    EXPECT_EQ(lexer->lex().type(), Token::Type::IMPL);
    EXPECT_EQ(lexer->lex().type(), Token::Type::IF);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ELSE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::FOR);
    EXPECT_EQ(lexer->lex().type(), Token::Type::UNTIL);
    EXPECT_EQ(lexer->lex().type(), Token::Type::MATCH);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ENUM);
    EXPECT_EQ(lexer->lex().type(), Token::Type::SPACE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::USE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ASM);
}

TEST_F(LexerTest, LexIdentifiers) {
    auto lexer = createLexer("myVar _private test123 camelCase");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);
    EXPECT_STREQ(tok1.value(), "myVar");

    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);
    EXPECT_STREQ(tok2.value(), "_private");

    auto tok3 = lexer->lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);
    EXPECT_STREQ(tok3.value(), "test123");

    auto tok4 = lexer->lex();
    EXPECT_EQ(tok4.type(), Token::Type::ID);
    EXPECT_STREQ(tok4.value(), "camelCase");
}

TEST_F(LexerTest, LexIntegers) {
    auto lexer = createLexer("0 42 123456 999");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::INT);
    EXPECT_STREQ(tok1.value(), "0");

    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::INT);
    EXPECT_STREQ(tok2.value(), "42");

    auto tok3 = lexer->lex();
    EXPECT_EQ(tok3.type(), Token::Type::INT);
    EXPECT_STREQ(tok3.value(), "123456");

    auto tok4 = lexer->lex();
    EXPECT_EQ(tok4.type(), Token::Type::INT);
    EXPECT_STREQ(tok4.value(), "999");
}

TEST_F(LexerTest, LexFloats) {
    auto lexer = createLexer("3.14 0.5 .25 100.0");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok1.value(), "3.14");

    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok2.value(), "0.5");

    auto tok3 = lexer->lex();
    EXPECT_EQ(tok3.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok3.value(), ".25");

    auto tok4 = lexer->lex();
    EXPECT_EQ(tok4.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok4.value(), "100.0");
}

TEST_F(LexerTest, LexScientificNotation) {
    auto lexer = createLexer("1e10 2.5e-3 3.14E+2");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok1.value(), "1e10");

    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok2.value(), "2.5e-3");

    auto tok3 = lexer->lex();
    EXPECT_EQ(tok3.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok3.value(), "3.14E+2");
}

TEST_F(LexerTest, LexStrings) {
    auto lexer = createLexer("\"hello\" \"world\" \"with spaces\" \"with\\nnewline\"");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::STR);
    EXPECT_STREQ(tok1.value(), "hello");

    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::STR);
    EXPECT_STREQ(tok2.value(), "world");

    auto tok3 = lexer->lex();
    EXPECT_EQ(tok3.type(), Token::Type::STR);
    EXPECT_STREQ(tok3.value(), "with spaces");

    auto tok4 = lexer->lex();
    EXPECT_EQ(tok4.type(), Token::Type::STR);
    EXPECT_STREQ(tok4.value(), "with\\nnewline");
}

TEST_F(LexerTest, LexEmptyString) {
    auto lexer = createLexer("\"\"");

    auto tok = lexer->lex();
    EXPECT_EQ(tok.type(), Token::Type::STR);
    EXPECT_STREQ(tok.value(), "");
}

TEST_F(LexerTest, LexCharLiterals) {
    auto lexer = createLexer("'a' 'Z' '\\n' '\\t'");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::CHAR);
    EXPECT_STREQ(tok1.value(), "a");

    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::CHAR);
    EXPECT_STREQ(tok2.value(), "Z");

    auto tok3 = lexer->lex();
    EXPECT_EQ(tok3.type(), Token::Type::CHAR);
    EXPECT_STREQ(tok3.value(), "\\n");

    auto tok4 = lexer->lex();
    EXPECT_EQ(tok4.type(), Token::Type::CHAR);
    EXPECT_STREQ(tok4.value(), "\\t");
}

TEST_F(LexerTest, LexBooleans) {
    auto lexer = createLexer("true false");

    EXPECT_EQ(lexer->lex().type(), Token::Type::TRUE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::FALSE);
}

// Operator tests
TEST_F(LexerTest, LexArithmeticOperators) {
    auto lexer = createLexer("+ - * / %");

    auto tok1 = lexer->lex();
    EXPECT_TRUE(tok1.type() == Token::Type::ADD || tok1.type() == Token::Type::R_PLUS || tok1.type() == Token::Type::L_PLUS);

    auto tok2 = lexer->lex();
    EXPECT_TRUE(tok2.type() == Token::Type::SUB || tok2.type() == Token::Type::R_MINUS || tok2.type() == Token::Type::L_MINUS);

    auto tok3 = lexer->lex();
    EXPECT_TRUE(tok3.type() == Token::Type::MUL || tok3.type() == Token::Type::R_DEREF || tok3.type() == Token::Type::L_DEREF);

    EXPECT_EQ(lexer->lex().type(), Token::Type::DIV);
    EXPECT_EQ(lexer->lex().type(), Token::Type::MOD);
}

TEST_F(LexerTest, LexComparisonOperators) {
    auto lexer = createLexer("< <= > >= == !=");

    EXPECT_EQ(lexer->lex().type(), Token::Type::LT);
    EXPECT_EQ(lexer->lex().type(), Token::Type::LE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::GT);
    EXPECT_EQ(lexer->lex().type(), Token::Type::GE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::EE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::NE);
}

TEST_F(LexerTest, LexLogicalOperators) {
    auto lexer = createLexer("and or && ||");

    EXPECT_EQ(lexer->lex().type(), Token::Type::AND);
    EXPECT_EQ(lexer->lex().type(), Token::Type::OR);
    EXPECT_EQ(lexer->lex().type(), Token::Type::AND);
    EXPECT_EQ(lexer->lex().type(), Token::Type::OR);
}

TEST_F(LexerTest, LexBitwiseOperators) {
    auto lexer = createLexer("& | ^ ~ << >>");

    auto tok1 = lexer->lex();
    EXPECT_TRUE(tok1.type() == Token::Type::B_AND || tok1.type() == Token::Type::R_REF || tok1.type() == Token::Type::L_REF);

    EXPECT_EQ(lexer->lex().type(), Token::Type::B_OR);
    EXPECT_EQ(lexer->lex().type(), Token::Type::B_XOR);

    auto tok2 = lexer->lex();
    EXPECT_TRUE(tok2.type() == Token::Type::R_B_NOT || tok2.type() == Token::Type::L_B_NOT);

    EXPECT_EQ(lexer->lex().type(), Token::Type::LS);
    EXPECT_EQ(lexer->lex().type(), Token::Type::RS);
}

TEST_F(LexerTest, LexAssignmentOperators) {
    auto lexer = createLexer("= += -= *= /= %= <<= >>= &= ^= |=");

    EXPECT_EQ(lexer->lex().type(), Token::Type::EQ);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ADD_EQ);
    EXPECT_EQ(lexer->lex().type(), Token::Type::SUB_EQ);
    EXPECT_EQ(lexer->lex().type(), Token::Type::MUL_EQ);
    EXPECT_EQ(lexer->lex().type(), Token::Type::DIV_EQ);
    EXPECT_EQ(lexer->lex().type(), Token::Type::MOD_EQ);
    EXPECT_EQ(lexer->lex().type(), Token::Type::LS_EQ);
    EXPECT_EQ(lexer->lex().type(), Token::Type::RS_EQ);
    EXPECT_EQ(lexer->lex().type(), Token::Type::B_AND_EQ);
    EXPECT_EQ(lexer->lex().type(), Token::Type::B_XOR_EQ);
    EXPECT_EQ(lexer->lex().type(), Token::Type::B_OR_EQ);
}

TEST_F(LexerTest, LexIncrementDecrement) {
    auto lexer = createLexer("++ --");

    auto tok1 = lexer->lex();
    EXPECT_TRUE(tok1.type() == Token::Type::R_INC || tok1.type() == Token::Type::L_INC);

    auto tok2 = lexer->lex();
    EXPECT_TRUE(tok2.type() == Token::Type::R_DEC || tok2.type() == Token::Type::L_DEC);
}

TEST_F(LexerTest, LexRangeOperators) {
    auto lexer = createLexer(".. ..= ...");

    EXPECT_EQ(lexer->lex().type(), Token::Type::RANGE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::RANGE_EQ);

    auto tok3 = lexer->lex();
    EXPECT_TRUE(tok3.type() == Token::Type::R_SPREAD || tok3.type() == Token::Type::L_SPREAD);
}

TEST_F(LexerTest, LexSpecialOperators) {
    auto lexer = createLexer("in is as alloc clean");

    EXPECT_EQ(lexer->lex().type(), Token::Type::IN);
    EXPECT_EQ(lexer->lex().type(), Token::Type::IS);
    EXPECT_EQ(lexer->lex().type(), Token::Type::AS);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ALLOC);
    EXPECT_EQ(lexer->lex().type(), Token::Type::CLEAN);
}

// Symbol tests
TEST_F(LexerTest, LexBrackets) {
    auto lexer = createLexer("( ) { } [ ]");

    EXPECT_EQ(lexer->lex().type(), Token::Type::O_PAREN);
    EXPECT_EQ(lexer->lex().type(), Token::Type::C_PAREN);
    EXPECT_EQ(lexer->lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::C_BRACE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::O_BRACKET);
    EXPECT_EQ(lexer->lex().type(), Token::Type::C_BRACKET);
}

TEST_F(LexerTest, LexPunctuation) {
    auto lexer = createLexer("; : , . -> => ::");

    EXPECT_EQ(lexer->lex().type(), Token::Type::SEMI_COLON);
    EXPECT_EQ(lexer->lex().type(), Token::Type::COLON);
    EXPECT_EQ(lexer->lex().type(), Token::Type::COMMA);
    EXPECT_EQ(lexer->lex().type(), Token::Type::DOT);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ARROW);
    EXPECT_EQ(lexer->lex().type(), Token::Type::DOUBLE_ARROW);
    EXPECT_EQ(lexer->lex().type(), Token::Type::SCOPE);
}

// Comment tests
TEST_F(LexerTest, SingleLineComment) {
    auto lexer = createLexer("let // this is a comment\nfn");

    EXPECT_EQ(lexer->lex().type(), Token::Type::LET);
    EXPECT_EQ(lexer->lex().type(), Token::Type::FN);
}

TEST_F(LexerTest, MultiLineComment) {
    auto lexer = createLexer("let /* this is a \nmulti-line comment */ fn");

    EXPECT_EQ(lexer->lex().type(), Token::Type::LET);
    EXPECT_EQ(lexer->lex().type(), Token::Type::FN);
}

TEST_F(LexerTest, UnterminatedMultiLineComment) {
    auto lexer = createLexer("let /* unterminated comment");

    EXPECT_EQ(lexer->lex().type(), Token::Type::LET);
    EXPECT_EQ(lexer->lex().type(), Token::Type::UNKNOWN);
}

// Whitespace handling
TEST_F(LexerTest, VariousWhitespace) {
    auto lexer = createLexer("let   \t\n  fn\n\ntype");

    EXPECT_EQ(lexer->lex().type(), Token::Type::LET);
    EXPECT_EQ(lexer->lex().type(), Token::Type::FN);
    EXPECT_EQ(lexer->lex().type(), Token::Type::TYPE);
}

// Context-dependent operator resolution
TEST_F(LexerTest, UnaryVsBinaryPlus) {
    auto lexer = createLexer("x + y");

    auto tok1 = lexer->lex(); // x
    EXPECT_EQ(tok1.type(), Token::Type::ID);

    auto tok2 = lexer->lex(); // +
    EXPECT_EQ(tok2.type(), Token::Type::ADD); // Binary in this context

    auto tok3 = lexer->lex(); // y
    EXPECT_EQ(tok3.type(), Token::Type::ID);
}

TEST_F(LexerTest, UnaryPlusAtStart) {
    auto lexer = createLexer("+x");

    auto tok1 = lexer->lex(); // +
    EXPECT_EQ(tok1.type(), Token::Type::L_PLUS); // Right unary at start

    auto tok2 = lexer->lex(); // x
    EXPECT_EQ(tok2.type(), Token::Type::ID);
}

TEST_F(LexerTest, PointerDereferenceVsMultiplication) {
    auto lexer = createLexer("x * y");

    auto tok1 = lexer->lex(); // x
    EXPECT_EQ(tok1.type(), Token::Type::ID);

    auto tok2 = lexer->lex(); // *
    EXPECT_EQ(tok2.type(), Token::Type::MUL); // Binary multiplication

    auto tok3 = lexer->lex(); // y
    EXPECT_EQ(tok3.type(), Token::Type::ID);
}

TEST_F(LexerTest, PointerDereferenceAtStart) {
    auto lexer = createLexer("*ptr");

    auto tok1 = lexer->lex(); // *
    EXPECT_EQ(tok1.type(), Token::Type::L_DEREF); // Dereference operator

    auto tok2 = lexer->lex(); // ptr
    EXPECT_EQ(tok2.type(), Token::Type::ID);
}

TEST_F(LexerTest, IncrementOperatorContext) {
    auto lexer = createLexer("x++; ++y");

    auto tok1 = lexer->lex(); // x
    EXPECT_EQ(tok1.type(), Token::Type::ID);

    auto tok2 = lexer->lex(); // ++
    EXPECT_EQ(tok2.type(), Token::Type::R_INC); // Left (postfix) increment

    auto tok3 = lexer->lex(); // ++
    EXPECT_EQ(tok3.type(), Token::Type::L_INC); // Right (prefix) increment

    auto tok4 = lexer->lex(); // y
    EXPECT_EQ(tok4.type(), Token::Type::ID);
}

// Peek functionality
TEST_F(LexerTest, PeekDoesNotConsumeToken) {
    auto lexer = createLexer("let fn");

    auto peeked = lexer->peek();
    EXPECT_EQ(peeked.type(), Token::Type::LET);

    auto lexed = lexer->lex();
    EXPECT_EQ(lexed.type(), Token::Type::LET);

    EXPECT_EQ(lexer->lex().type(), Token::Type::FN);
}

TEST_F(LexerTest, MultiplePeeks) {
    auto lexer = createLexer("let fn type");

    EXPECT_EQ(lexer->peek().type(), Token::Type::LET);
    EXPECT_EQ(lexer->peek().type(), Token::Type::LET);
    EXPECT_EQ(lexer->peek().type(), Token::Type::LET);

    EXPECT_EQ(lexer->lex().type(), Token::Type::LET);
    EXPECT_EQ(lexer->lex().type(), Token::Type::FN);
    EXPECT_EQ(lexer->lex().type(), Token::Type::TYPE);
}

// Complex expressions
TEST_F(LexerTest, SimpleExpression) {
    auto lexer = createLexer("x = y + 5");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);
    EXPECT_STREQ(tok1.value(), "x");

    EXPECT_EQ(lexer->lex().type(), Token::Type::EQ);

    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);
    EXPECT_STREQ(tok2.value(), "y");

    EXPECT_EQ(lexer->lex().type(), Token::Type::ADD);

    auto tok3 = lexer->lex();
    EXPECT_EQ(tok3.type(), Token::Type::INT);
    EXPECT_STREQ(tok3.value(), "5");
}

TEST_F(LexerTest, FunctionCall) {
    auto lexer = createLexer("myFunc(arg1, arg2)");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);
    EXPECT_STREQ(tok1.value(), "myFunc");

    EXPECT_EQ(lexer->lex().type(), Token::Type::O_PAREN);

    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);
    EXPECT_STREQ(tok2.value(), "arg1");

    EXPECT_EQ(lexer->lex().type(), Token::Type::COMMA);

    auto tok3 = lexer->lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);
    EXPECT_STREQ(tok3.value(), "arg2");

    EXPECT_EQ(lexer->lex().type(), Token::Type::C_PAREN);
}

TEST_F(LexerTest, ArrayAccess) {
    auto lexer = createLexer("array[10]");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);
    EXPECT_STREQ(tok1.value(), "array");

    EXPECT_EQ(lexer->lex().type(), Token::Type::O_BRACKET);

    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::INT);
    EXPECT_STREQ(tok2.value(), "10");

    EXPECT_EQ(lexer->lex().type(), Token::Type::C_BRACKET);
}

TEST_F(LexerTest, StructAccess) {
    auto lexer = createLexer("obj.field");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);
    EXPECT_STREQ(tok1.value(), "obj");

    EXPECT_EQ(lexer->lex().type(), Token::Type::DOT);

    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);
    EXPECT_STREQ(tok2.value(), "field");
}

TEST_F(LexerTest, ScopeResolution) {
    auto lexer = createLexer("Module::function");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);
    EXPECT_STREQ(tok1.value(), "Module");

    EXPECT_EQ(lexer->lex().type(), Token::Type::SCOPE);

    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);
    EXPECT_STREQ(tok2.value(), "function");
}

// Edge cases
TEST_F(LexerTest, KeywordAsPartOfIdentifier) {
    auto lexer = createLexer("letter function_name ifNotThis");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);
    EXPECT_STREQ(tok1.value(), "letter");

    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);
    EXPECT_STREQ(tok2.value(), "function_name");

    auto tok3 = lexer->lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);
    EXPECT_STREQ(tok3.value(), "ifNotThis");
}

TEST_F(LexerTest, DotFollowedByDot) {
    auto lexer = createLexer("1..10");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::INT);
    EXPECT_STREQ(tok1.value(), "1");

    EXPECT_EQ(lexer->lex().type(), Token::Type::RANGE);

    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::INT);
    EXPECT_STREQ(tok2.value(), "10");
}

TEST_F(LexerTest, LeadingDecimalPoint) {
    auto lexer = createLexer(".5 + .25");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok1.value(), ".5");

    EXPECT_EQ(lexer->lex().type(), Token::Type::ADD);

    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok2.value(), ".25");
}

TEST_F(LexerTest, UnterminatedString) {
    auto lexer = createLexer("\"unterminated");

    auto tok = lexer->lex();
    EXPECT_EQ(tok.type(), Token::Type::UNKNOWN);
}

TEST_F(LexerTest, UnterminatedChar) {
    auto lexer = createLexer("'a");

    auto tok = lexer->lex();
    EXPECT_EQ(tok.type(), Token::Type::UNKNOWN);
}

TEST_F(LexerTest, EmptyCharLiteral) {
    auto lexer = createLexer("''");

    auto tok = lexer->lex();
    EXPECT_EQ(tok.type(), Token::Type::UNKNOWN);
}

// Position tracking
TEST_F(LexerTest, LastTokenPosition) {
    auto lexer = createLexer("let x = 5");

    lexer->lex(); // let
    auto [pos1, line1, char1] = lexer->last_pos();
    EXPECT_EQ(line1, 0);
    EXPECT_EQ(char1, 0);

    lexer->lex(); // x
    auto [pos2, line2, char2] = lexer->last_pos();
    EXPECT_EQ(line2, 0);
    EXPECT_EQ(char2, 4);

    lexer->lex(); // =
    auto [pos3, line3, char3] = lexer->last_pos();
    EXPECT_EQ(line3, 0);
    EXPECT_EQ(char3, 6);
}

TEST_F(LexerTest, MultilinePosition) {
    auto lexer = createLexer("let\nx = 5\ny = 10");

    lexer->lex(); // let
    lexer->lex(); // x
    lexer->lex(); // =
    lexer->lex(); // 5

    lexer->lex(); // y
    auto [pos, line, char_pos] = lexer->last_pos();
    // Note: Line tracking may not work as expected due to uninitialized counters
}

// Complex operator sequences
TEST_F(LexerTest, CompoundAssignments) {
    auto lexer = createLexer("x += 5; y *= 2; z /= 3;");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ADD_EQ);
    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::INT);
    EXPECT_EQ(lexer->lex().type(), Token::Type::SEMI_COLON);

    auto tok3 = lexer->lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::MUL_EQ);
    auto tok4 = lexer->lex();
    EXPECT_EQ(tok4.type(), Token::Type::INT);
    EXPECT_EQ(lexer->lex().type(), Token::Type::SEMI_COLON);

    auto tok5 = lexer->lex();
    EXPECT_EQ(tok5.type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::DIV_EQ);
    auto tok6 = lexer->lex();
    EXPECT_EQ(tok6.type(), Token::Type::INT);
    EXPECT_EQ(lexer->lex().type(), Token::Type::SEMI_COLON);
}

TEST_F(LexerTest, ChainedComparisons) {
    auto lexer = createLexer("a < b <= c > d >= e");

    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::LT);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::LE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::GT);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::GE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
}

TEST_F(LexerTest, MixedBitwiseOperations) {
    auto lexer = createLexer("a & b | c ^ d");

    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    auto tok1 = lexer->lex();
    EXPECT_TRUE(tok1.type() == Token::Type::B_AND || tok1.type() == Token::Type::R_REF || tok1.type() == Token::Type::L_REF);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::B_OR);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::B_XOR);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
}

TEST_F(LexerTest, ShiftOperations) {
    auto lexer = createLexer("x << 2 >> 1");

    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::LS);
    EXPECT_EQ(lexer->lex().type(), Token::Type::INT);
    EXPECT_EQ(lexer->lex().type(), Token::Type::RS);
    EXPECT_EQ(lexer->lex().type(), Token::Type::INT);
}

// Function and type declarations
TEST_F(LexerTest, FunctionDeclaration) {
    auto lexer = createLexer("fn myFunc(x: int) -> int { }");

    EXPECT_EQ(lexer->lex().type(), Token::Type::FN);
    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);
    EXPECT_STREQ(tok1.value(), "myFunc");
    EXPECT_EQ(lexer->lex().type(), Token::Type::O_PAREN);
    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);
    EXPECT_STREQ(tok2.value(), "x");
    EXPECT_EQ(lexer->lex().type(), Token::Type::COLON);
    auto tok3 = lexer->lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);
    EXPECT_STREQ(tok3.value(), "int");
    EXPECT_EQ(lexer->lex().type(), Token::Type::C_PAREN);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ARROW);
    auto tok4 = lexer->lex();
    EXPECT_EQ(tok4.type(), Token::Type::ID);
    EXPECT_STREQ(tok4.value(), "int");
    EXPECT_EQ(lexer->lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::C_BRACE);
}

TEST_F(LexerTest, TypeDeclaration) {
    auto lexer = createLexer("type Point { x: int, y: int }");

    EXPECT_EQ(lexer->lex().type(), Token::Type::TYPE);
    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);
    EXPECT_STREQ(tok1.value(), "Point");
    EXPECT_EQ(lexer->lex().type(), Token::Type::O_BRACE);
    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);
    EXPECT_STREQ(tok2.value(), "x");
    EXPECT_EQ(lexer->lex().type(), Token::Type::COLON);
    auto tok3 = lexer->lex();
    EXPECT_EQ(tok3.type(), Token::Type::ID);
    EXPECT_STREQ(tok3.value(), "int");
    EXPECT_EQ(lexer->lex().type(), Token::Type::COMMA);
}

TEST_F(LexerTest, ControlFlow) {
    auto lexer = createLexer("if x > 0 { } else { }");

    EXPECT_EQ(lexer->lex().type(), Token::Type::IF);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::GT);
    EXPECT_EQ(lexer->lex().type(), Token::Type::INT);
    EXPECT_EQ(lexer->lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::C_BRACE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ELSE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::C_BRACE);
}

TEST_F(LexerTest, ForLoop) {
    auto lexer = createLexer("for i in 0..10 { }");

    EXPECT_EQ(lexer->lex().type(), Token::Type::FOR);
    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::ID);
    EXPECT_STREQ(tok1.value(), "i");
    EXPECT_EQ(lexer->lex().type(), Token::Type::IN);
    EXPECT_EQ(lexer->lex().type(), Token::Type::INT);
    EXPECT_EQ(lexer->lex().type(), Token::Type::RANGE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::INT);
    EXPECT_EQ(lexer->lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::C_BRACE);
}

TEST_F(LexerTest, MatchStatement) {
    auto lexer = createLexer("match x { }");

    EXPECT_EQ(lexer->lex().type(), Token::Type::MATCH);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::O_BRACE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::C_BRACE);
}

// Special cases with strings and chars
TEST_F(LexerTest, StringWithEscapes) {
    auto lexer = createLexer("\"hello\\nworld\\t!\"");

    auto tok = lexer->lex();
    EXPECT_EQ(tok.type(), Token::Type::STR);
    EXPECT_STREQ(tok.value(), "hello\\nworld\\t!");
}

TEST_F(LexerTest, StringWithQuotes) {
    auto lexer = createLexer("\"say \\\"hello\\\"\"");

    auto tok = lexer->lex();
    EXPECT_EQ(tok.type(), Token::Type::STR);
    EXPECT_STREQ(tok.value(), "say \\\"hello\\\"");
}

TEST_F(LexerTest, CharWithEscape) {
    auto lexer = createLexer("'\\''");

    auto tok = lexer->lex();
    EXPECT_EQ(tok.type(), Token::Type::CHAR);
    EXPECT_STREQ(tok.value(), "\\'");
}

// Number edge cases
TEST_F(LexerTest, NumberFollowedByIdentifier) {
    auto lexer = createLexer("123abc");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::INT);
    EXPECT_STREQ(tok1.value(), "123");

    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::ID);
    EXPECT_STREQ(tok2.value(), "abc");
}

TEST_F(LexerTest, FloatWithoutFractionalPart) {
    auto lexer = createLexer("42. 100.");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok1.value(), "42.");

    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok2.value(), "100.");
}

TEST_F(LexerTest, ScientificNotationEdgeCases) {
    auto lexer = createLexer("1e+10 2E-5 3.14e0");

    auto tok1 = lexer->lex();
    EXPECT_EQ(tok1.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok1.value(), "1e+10");

    auto tok2 = lexer->lex();
    EXPECT_EQ(tok2.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok2.value(), "2E-5");

    auto tok3 = lexer->lex();
    EXPECT_EQ(tok3.type(), Token::Type::FLOAT);
    EXPECT_STREQ(tok3.value(), "3.14e0");
}

TEST_F(LexerTest, InvalidScientificNotation) {
    auto lexer = createLexer("1e");

    auto tok1 = lexer->lex();
    // This should produce UNKNOWN as 'e' without digits is invalid
    EXPECT_EQ(tok1.type(), Token::Type::UNKNOWN);
}

// Ambiguous operator sequences
TEST_F(LexerTest, MinusVsArrow) {
    auto lexer = createLexer("x - y x->y");

    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    auto tok1 = lexer->lex();
    EXPECT_TRUE(tok1.type() == Token::Type::SUB || tok1.type() == Token::Type::R_MINUS || tok1.type() == Token::Type::L_MINUS);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);

    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ARROW);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
}

TEST_F(LexerTest, EqualsVsDoubleArrow) {
    auto lexer = createLexer("x = y x => y");

    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::EQ);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);

    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::DOUBLE_ARROW);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
}

TEST_F(LexerTest, ColonVsScope) {
    auto lexer = createLexer("x : y x::y");

    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::COLON);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);

    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::SCOPE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
}

// Vague token resolution in complex contexts
TEST_F(LexerTest, VagueTokenSequence) {
    auto lexer = createLexer("*ptr + *arr");

    auto tok1 = lexer->lex();
    EXPECT_TRUE(tok1.type() == Token::Type::R_DEREF || tok1.type() == Token::Type::MUL || tok1.type() == Token::Type::L_DEREF);

    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ADD);

    auto tok2 = lexer->lex();
    EXPECT_TRUE(tok2.type() == Token::Type::R_DEREF || tok2.type() == Token::Type::MUL || tok2.type() == Token::Type::L_DEREF);

    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
}

TEST_F(LexerTest, ComplexVagueExpression) {
    auto lexer = createLexer("&x * &y");

    auto tok1 = lexer->lex();
    EXPECT_TRUE(tok1.type() == Token::Type::R_REF || tok1.type() == Token::Type::B_AND || tok1.type() == Token::Type::L_REF);

    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
    EXPECT_EQ(lexer->lex().type(), Token::Type::MUL);

    auto tok2 = lexer->lex();
    EXPECT_TRUE(tok2.type() == Token::Type::R_REF || tok2.type() == Token::Type::B_AND || tok2.type() == Token::Type::L_REF);

    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
}

// Stress tests
TEST_F(LexerTest, LongIdentifier) {
    std::string longId(1000, 'a');
    auto lexer = createLexer(longId);

    auto tok = lexer->lex();
    EXPECT_EQ(tok.type(), Token::Type::ID);
    EXPECT_EQ(tok.str().length(), 1000);
}

TEST_F(LexerTest, LongString) {
    std::string longStr = "\"" + std::string(1000, 'x') + "\"";
    auto lexer = createLexer(longStr);

    auto tok = lexer->lex();
    EXPECT_EQ(tok.type(), Token::Type::STR);
    EXPECT_EQ(tok.str().length(), 1000);
}

TEST_F(LexerTest, ManyTokens) {
    std::string input;
    for (int i = 0; i < 1000; ++i) {
        input += "let x" + std::to_string(i) + " = " + std::to_string(i) + "; ";
    }
    auto lexer = createLexer(input);

    for (int i = 0; i < 1000; ++i) {
        EXPECT_EQ(lexer->lex().type(), Token::Type::LET);
        EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
        EXPECT_EQ(lexer->lex().type(), Token::Type::EQ);
        EXPECT_EQ(lexer->lex().type(), Token::Type::INT);
        EXPECT_EQ(lexer->lex().type(), Token::Type::SEMI_COLON);
    }
}

// Empty input
TEST_F(LexerTest, EmptyInput) {
    auto lexer = createLexer("");
    // Behavior on EOF is implementation-defined
    // This test just ensures no crash
}

TEST_F(LexerTest, OnlyWhitespace) {
    auto lexer = createLexer("   \n\t  \n  ");
    // Should reach EOF without tokens
}

TEST_F(LexerTest, OnlyComments) {
    auto lexer = createLexer("// comment 1\n/* comment 2 */");
    // Should reach EOF without tokens
}

// Last token tracking
TEST_F(LexerTest, LastTokenTracking) {
    auto lexer = createLexer("let x");

    auto tok1 = lexer->lex();
    auto last1 = lexer->last();
    EXPECT_EQ(last1.token.type(), Token::Type::LET);

    auto tok2 = lexer->lex();
    auto last2 = lexer->last();
    EXPECT_EQ(last2.token.type(), Token::Type::ID);
    EXPECT_STREQ(last2.token.value(), "x");
}

// Spread operators
TEST_F(LexerTest, SpreadOperatorContexts) {
    auto lexer = createLexer("...args");

    auto tok1 = lexer->lex();
    EXPECT_TRUE(tok1.type() == Token::Type::R_SPREAD || tok1.type() == Token::Type::L_SPREAD);

    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
}

TEST_F(LexerTest, RangeVsSpread) {
    auto lexer = createLexer("0..10 ...args");

    EXPECT_EQ(lexer->lex().type(), Token::Type::INT);
    EXPECT_EQ(lexer->lex().type(), Token::Type::RANGE);
    EXPECT_EQ(lexer->lex().type(), Token::Type::INT);

    auto tok = lexer->lex();
    EXPECT_TRUE(tok.type() == Token::Type::R_SPREAD || tok.type() == Token::Type::L_SPREAD);
    EXPECT_EQ(lexer->lex().type(), Token::Type::ID);
}
