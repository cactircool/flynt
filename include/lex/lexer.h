#ifndef LEXER_H
#define LEXER_H

#include <fstream>
#include <deque>
#include "lex/token.h"
#include "util/binarytree.h"

namespace flynt::lex {

/**
 * @brief The Lexer class: an iterative stream lexer
 */
class Lexer
{   
public:
    /**
     * @brief Lexer: file constructor by path
     * @param path
     */
    Lexer(const char *path);

    /**
     * @brief Lexer: file constructor by path (std::string convenience constructor)
     * @param path
     */
    Lexer(const std::string &path);

    Lexer(Lexer &&) = default;
    ~Lexer() = default;

    /**
     * @brief lex: get one token out of the stream
     * @return extracted Token *
     */
    Token *lex();
    /**
     * @brief put: put back one token into the stream
     * @param token
     */
    void put(Token *token);
    /**
     * @brief allowTypeCoercion: true ? LT, GT, GE, RS, RS_EQ all get converted to O_ANGLE or C_ANGLE equivalents
     * @param allow
     */
    void allowTypeCoercion(bool allow = true);

    /**
     * @brief operator bool: !(stream exhausted)
     */
    operator bool() const noexcept;

    /**
     * @brief init: static initializer for Lexer BinaryTree<StaticData> keyword/operator/symbol finder
     */
    static void init();

private:
    /**
     * @brief pop: pop off token from buffer, do housekeeping, and return
     * @return popped Token *
     */
    Token *pop();

    /**
     * @brief unget: strm.unget() wrapper to handle EOF edge cases
     */
    void unget();

    /**
     * @brief lexUnit: lex wrapper without worrying about buffers or ambiguity
     * @return lexed Token *
     */
    Token *lexUnit();

    /**
     * @brief lexNumber: lex a number token: INT, FLOAT, B_INT, O_INT, X_INT, T_INT
     * @param start: starting character
     * @param offset: offset to set the resulting token's offset to
     * @return number Token *
     */
    Token *lexNumber(char start, off_t offset);

    /**
     * @brief lexCharOrString: lex a character or a string, implicitly determines whether '' strings are chars or strings based on length of content
     * @param start: starting character
     * @param offset: offset to set the resulting token's offset to
     * @return char/string Token *
     */
    Token *lexCharOrString(char start, off_t offset);

    /**
     * @brief lexIdOrKeyword: lex an id or keyword using Lexer::staticData to logarithmically find keywords
     * @param start: staring character
     * @param offset: offset to set the resulting token's offset to
     * @return id/keyword Token *
     */
    Token *lexIdOrKeyword(char start, off_t offset);

    /**
     * @brief lexOperatorOrSymbol: lex an operator or symbol from SCOPE to DOUBLE_ARROW using Lexer::staticData
     * @param start: starting character
     * @param offset: offset to set the resulting token's offset to
     * @return operator/symbol Token *
     */
    Token *lexOperatorOrSymbol(char start, off_t offset);

    /**
     * @brief collapseBuffer: removes all ambiguity from the buffer through backward determination
     */
    void collapseBuffer();

    std::ifstream strm;
    std::deque<Token *> buffer;
    Token *last;
    bool typeCoerce;

    /**
     * @brief The StaticData class: BinaryTree holder convenience class
     */
    struct StaticData {
        std::string data;
        Token::Type type;

        /**
         * @brief StaticData: create a StaticData object when you're lazy, useful for basic comparisons
         * @param data
         */
        StaticData(std::string data = "") : data(std::move(data)), type(Token::UNKNOWN) {}

        /**
         * @brief StaticData: fully create a StaticData object
         * @param data
         * @param type
         */
        StaticData(std::string data, Token::Type type) : data(std::move(data)), type(type) {}

        /**
         * @brief operator std::string: implicitly allow std::string conversion
         */
        operator std::string() const { return data; }

        /**
         * @brief operator <<: makes StaticData streamable
         * @param strm
         * @param data
         * @return strm
         */
        friend std::ostream &operator<<(std::ostream &strm, const StaticData &data) { return strm << "{ '" << data.data << "', " << Token::nameOf(data.type) << " }"; }

        /**
         * @brief operator <: compare strings
         * @param rhs
         * @return lhs.data < rhs.data
         */
        inline bool operator<(const StaticData &rhs) const { return data < rhs.data; }

        /**
         * @brief operator <=: compare strings
         * @param rhs
         * @return lhs.data <= rhs.data
         */
        inline bool operator<=(const StaticData &rhs) const { return data <= rhs.data; }

        /**
         * @brief operator >: compare strings
         * @param rhs
         * @return lhs.data > rhs.data
         */
        inline bool operator>(const StaticData &rhs) const { return data > rhs.data; }

        /**
         * @brief operator >=: compare strings
         * @param rhs
         * @return lhs.data >= rhs.data
         */
        inline bool operator>=(const StaticData &rhs) const { return data >= rhs.data; }
    };

public:
    static flynt::util::BinaryTree<StaticData> staticData;
};

}

#endif // LEXER_H
