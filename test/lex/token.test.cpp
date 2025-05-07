#include "lex/token.h"
#include <gtest/gtest.h>

namespace flynt::lex {

TEST(TokenTest, Construction) {
    ASSERT_EQ(Token::UNKNOWN, Token().getType());
    ASSERT_EQ(0, Token().getOffset());
    ASSERT_EQ("", Token().getValue());
}

TEST(TokenTest, Is) {
    Token tok(Token::ID, "hello_world", 0);
    ASSERT_TRUE(tok.isId());

    tok.setType(Token::UNKNOWN);
    ASSERT_TRUE(tok.isUnknown());

    tok.setType(Token::INT);
    ASSERT_TRUE(tok.isLiteral());

    tok.setType(Token::CONTRACT);
    ASSERT_TRUE(tok.isKeyword());

    tok.setType(Token::PUB);
    ASSERT_TRUE(tok.isAccessModifier());

    tok.setType(Token::TYPE);
    ASSERT_TRUE(tok.isDeclarator());

    tok.setType(Token::ELSE);
    ASSERT_TRUE(tok.isControlFlow());

    tok.setType(Token::EXT);
    ASSERT_TRUE(tok.isMeta());

    tok.setType(Token::MUL);
    ASSERT_TRUE(tok.isOperator());
    ASSERT_TRUE(tok.isBinary());
    ASSERT_TRUE(tok.isAmbiguous());
    ASSERT_FALSE(tok.isUnary());
    ASSERT_TRUE(tok.isRightCoercible());
    ASSERT_TRUE(tok.isLeftCoercible());
    ASSERT_TRUE(tok.isBinaryCoercible());
    ASSERT_FALSE(tok.isRight());
    ASSERT_FALSE(tok.isLeft());

    tok.setType(Token::SEMI_COLON);
    ASSERT_TRUE(tok.isSymbol());
    ASSERT_TRUE(tok.isSymbol(1));
    ASSERT_TRUE(tok.isSymbol(-1));
    ASSERT_FALSE(tok.isSymbol(0));
    ASSERT_FALSE(tok.isOpen());
    ASSERT_TRUE(tok.isNonScopedSymbol());
    ASSERT_TRUE(tok.isSemiColon());
    ASSERT_FALSE(tok.isColon());
    ASSERT_FALSE(tok.isArrow());
    ASSERT_FALSE(tok.isDoubleArrow());

    tok.setType(Token::O_PAREN);
    ASSERT_TRUE(tok.isOpen());
    ASSERT_FALSE(tok.isClosed());
    ASSERT_FALSE(tok.isSymbol(1));
    ASSERT_FALSE(tok.isSymbol(10));
    ASSERT_TRUE(tok.isSymbol(0));
    ASSERT_TRUE(tok.isSymbol(-10));
    ASSERT_FALSE(tok.isNonScopedSymbol());
    ASSERT_FALSE(tok.isSemiColon());
    ASSERT_FALSE(tok.isColon());
    ASSERT_FALSE(tok.isArrow());
    ASSERT_FALSE(tok.isDoubleArrow());

    tok.setType(Token::C_PAREN);
    ASSERT_FALSE(tok.isOpen());
    ASSERT_TRUE(tok.isClosed());
    ASSERT_TRUE(tok.isSymbol(1));
    ASSERT_TRUE(tok.isSymbol(10));
    ASSERT_TRUE(tok.isSymbol(0));
    ASSERT_FALSE(tok.isSymbol(-10));
    ASSERT_FALSE(tok.isNonScopedSymbol());
    ASSERT_FALSE(tok.isSemiColon());
    ASSERT_FALSE(tok.isColon());
    ASSERT_FALSE(tok.isArrow());
    ASSERT_FALSE(tok.isDoubleArrow());
}

TEST(TokenTest, Get) {
    Token tok(Token::SCOPE, 0);
    ASSERT_EQ(1, tok.getPrecedence());

    tok.setType(Token::O_PAREN);
    ASSERT_GT(tok.getScope(), 0);
}

TEST(TokenTest, CoerceAndComparisons) {
    Token tok(Token::R_REF, 0);
    tok.coerceToLeft();
    ASSERT_EQ(tok, Token::L_REF);

    tok.coerceToRight();
    ASSERT_EQ(tok, Token(Token::R_REF, 0));

    tok.coerceToBinary();
    ASSERT_EQ(tok, Token::B_AND);

    ASSERT_NE(tok, Token(Token::B_AND, "yuh", 0));
}

}
