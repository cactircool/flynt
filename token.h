#ifndef TOKEN_H
#define TOKEN_H

#include <string>
#include <cstdint>
#include <ostream>
#include <gtest/gtest.h>

namespace flynt::lex {

class Lexer;

/**
 * @brief The Token class: Lexer unit
 */
class __attribute__((__packed__)) Token
{
public:
    /**
     * @brief The Type enum: a type identifier for Tokens to minimize using the std::string value;
     */
    enum Type : uint8_t {
        UNKNOWN,

        ID,

        // Literals
        INT,
        B_INT,
        O_INT,
        X_INT,
        T_INT,
        FLOAT,
        CHAR,
        STR,

        // Keywords
        VAR,
        VOL,
        TYPE,
        FN,
        IDEA,
        TEMPL,
        IMPL,
        SPACE,

        PRIV,
        PROT,
        PUB,
        STAT,
        FIN,
        ABSTR,

        IF,
        WHILE,
        UNTIL,
        ELSE,
        FOR,
        PICK,
        RET,
        BRK,
        CONT,
        CONTRACT,

        IMP,
        EXP,
        EXT,
        LINK,

        // Operators
        SCOPE,

        R_INC,
        R_DEC,
        R_PLUS,
        R_MINUS,
        R_REF,
        R_DEREF,
        R_NOT,
        R_B_NOT,
        R_DOLLAR,
        R_XOR,
        R_AT,
        R_PERCENT,
        R_Q_MARK,
        R_SPREAD,
        DOT,

        L_INC,
        L_DEC,
        L_PLUS,
        L_MINUS,
        L_REF,
        L_DEREF,
        L_NOT,
        L_B_NOT,
        L_DOLLAR,
        L_XOR,
        L_AT,
        L_PERCENT,
        L_Q_MARK,
        L_SPREAD,
        AWAIT,
        LOT,
        DROP,

        MUL,
        DIV,
        MOD,

        ADD,
        SUB,

        RS,
        LS,

        LT,
        GT,
        LE,
        GE,

        EE,
        NE,

        B_AND,

        B_XOR,

        B_OR,

        AND,

        OR,

        RANGE,
        RANGE_EQ,

        THROW,
        YIELD,
        EQ,
        ADD_EQ,
        SUB_EQ,
        MUL_EQ,
        DIV_EQ,
        MOD_EQ,
        RS_EQ,
        LS_EQ,
        B_AND_EQ,
        B_XOR_EQ,
        B_OR_EQ,

        COMMA,

        // Symbols
        O_PAREN,
        C_PAREN,

        O_BRACE,
        C_BRACE,

        O_BRACKET,
        C_BRACKET,

        O_ANGLE,
        C_ANGLE,

        SEMI_COLON,
        COLON,
        ARROW,
        DOUBLE_ARROW,

        END,
    };

    /**
     * @brief Token: default convenience constructor
     */
    Token();
    /**
     * @brief Token: full constructor
     * @param type
     * @param value
     * @param offset
     */
    Token(Type type, std::string value, off_t offset);
    /**
     * @brief Token: valueless constructor. BTW why tf does std::optional<std::string> take 8 more bytes than an std::string like tf wdym
     * @param type
     * @param offset
     */
    Token(Type type, off_t offset);

    ~Token() = default;
    Token(Token &&) = default;
    Token(const Token &) = default;

    Type getType() const;
    void setType(Type newType);
    std::string getValue() const;
    void setValue(const std::string &newValue);
    off_t getOffset() const;
    void setOffset(const off_t &newOffset);

    bool isId() const;
    bool isUnknown() const;
    /**
     * @brief isLiteral
     * @return INT | B_INT | O_INT | X_INT | T_INT | FLOAT | CHAR | STR
     */
    bool isLiteral() const;
    bool isKeyword() const;
    /**
     * @brief isAccessModifier
     * @return PRIV | PROT | PUB | ABSTR | STAT | FIN
     */
    bool isAccessModifier() const;
    /**
     * @brief isDeclarator
     * @return anything that declares a blueprint (var, type, fn, idea, etc.)
     */
    bool isDeclarator() const;
    /**
     * @brief isControlFlow
     * @return control flow token?
     */
    bool isControlFlow() const;
    /**
     * @brief isMeta
     * @return imports, exports, anything that is a preprocessor step beyond blueprints
     */
    bool isMeta() const;

    bool isOperator() const;
    bool isBinary() const;
    bool isUnary() const;
    bool isRight() const;
    bool isLeft() const;
    bool isBinaryCoercible() const;
    bool isRightCoercible() const;
    bool isLeftCoercible() const;
    /**
     * @brief isAmbiguous
     * @return isRightCoercible() && isLeftCoercible() && isBinaryCoercible()
     */
    bool isAmbiguous() const;

    bool isSymbol() const;
    /**
     * @brief isSymbol
     * @param exclude
     * @return is a symbol excluding the scope provided through exclude in 3 categories: scope > 0, scope == 0, scope < 0
     */
    bool isSymbol(int8_t exclude);
    bool isOpen() const;
    bool isClosed() const;
    bool isNonScopedSymbol() const;
    bool isSemiColon() const;
    bool isArrow() const;
    bool isDoubleArrow() const;
    bool isColon() const;

    /**
     * @brief getPrecedence
     * @return operator precedence for expression parsing
     */
    uint8_t getPrecedence() const;
    /**
     * @brief getScope
     * @return isOpen() ? 1 : isClosed() ? -1 : 0
     */
    uint8_t getScope() const;

    /**
     * @brief coerceToBinary: convert the type to a binary version if applicable
     */
    void coerceToBinary();
    /**
     * @brief coerceToBinary: convert the type to a right version if applicable
     */
    void coerceToRight();
    /**
     * @brief coerceToBinary: convert the type to a left version if applicable
     */
    void coerceToLeft();

    /**
     * @brief valueOf
     * @param type
     * @return Token::typeValues[type]
     */
    static const char *valueOf(Type type);
    /**
     * @brief nameOf
     * @param type
     * @return Token::typeNames[type]
     */
    static const char *nameOf(Type type);

    /**
     * @brief operator ==
     * @param other
     * @return all matches -> type, value, offset
     */
    bool operator==(const Token &other) const;
    /**
     * @brief operator ==
     * @param type
     * @return Token::type == type
     */
    bool operator==(Type type) const;
    /**
     * @brief operator ==
     * @param value
     * @return Token::value == value
     */
    bool operator==(const std::string &value) const;

    /**
     * @brief operator !=: convenience I was tired man, could not be bothered, compiler optimization probably got my back hopefully
     * @param rhs
     * @return !operator==
     */
    inline bool operator!=(auto rhs) const { return !(*this == rhs); }

    /**
     * @brief expandedValue: getValue() but if the type requires a value be provided and parts of the value are removed for redundancy (STR, CHAR, B_INT, etc.), it is expanded to include the redundancies
     * @return getValue() but expanded :]
     */
    std::string expandedValue() const;

    // Fwiend :]
    friend Lexer;

    /**
     * @brief PrintTo: appease the gtest gods
     * @param token
     * @param os
     */
    friend void PrintTo(const Token &token, std::ostream *os);

    /**
     * @brief operator <<: make Tokens printable. y this didn't appease the gtest gods I will never know, but its useful so whatever
     * @param strm
     * @param token
     * @return strm
     */
    friend std::ostream &operator<<(std::ostream &strm, const flynt::lex::Token &token);

private:
    Type type;
    std::string value;
    off_t offset;

    static const char *typeValues[Token::END];
    static const char *typeNames[Token::END];
};

}

#endif // TOKEN_H
