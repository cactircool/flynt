#include "lexer.h"
#include "preprocessor.h"

namespace flynt::lex {

flynt::util::BinaryTree<Lexer::StaticData> Lexer::staticData = {};

void Lexer::unget()
{
    strm.clear();
    strm.seekg(-1, std::ios::cur);
}

void Lexer::init()
{
    for (int i = Token::VAR; i < Token::END; ++i)
        Lexer::staticData.insert({ Token::valueOf(Token::Type(i)), Token::Type(i) });
}

Lexer::operator bool() const noexcept
{
    return const_cast<Lexer *>(this)->strm.tellg() > -1;
}

Lexer::Lexer(const char *path) : strm(path), buffer(), last(nullptr), typeCoerce(false)
{
    FLYNT_ASSERT(strm.good());
}

Lexer::Lexer(const std::string &path) : strm(path), buffer(), last(nullptr), typeCoerce(false)
{
    FLYNT_ASSERT(strm.good());
}

Token *Lexer::lex()
{
    if (!buffer.empty())
        return pop();

    Token *unit;
    do {
        unit = lexUnit();
        buffer.emplace_back(unit);
    } while (unit && (unit->isAmbiguous() || unit->isUnary()));

    collapseBuffer();
    return pop();
}

void Lexer::put(Token *token)
{
    buffer.push_front(token);
}

void Lexer::allowTypeCoercion(bool allow)
{
    typeCoerce = allow;
}

Token *Lexer::pop()
{
    last = buffer.front();
    buffer.pop_front();

    if (typeCoerce)
        switch (last->getType())
        {
        case Token::LT:
            last->setType(Token::O_ANGLE);
            return last;
        case Token::GT:
            last->setType(Token::C_ANGLE);
            return last;
        case Token::GE:
            last->setType(Token::C_ANGLE);
            buffer.push_front(new Token(Token::EQ, last->getOffset() + 1));
            return last;
        case Token::RS:
            last->setType(Token::C_ANGLE);
            buffer.push_front(new Token(Token::C_ANGLE, last->getOffset() + 1));
            return last;
        case Token::RS_EQ:
            last->setType(Token::C_ANGLE);
            buffer.push_front(new Token(Token::C_ANGLE, last->getOffset() + 1));
            buffer.push_front(new Token(Token::EQ, last->getOffset() + 2));
            return last;
        default: break;
        }
    return last;
}

Token *Lexer::lexUnit()
{
    char c;
    while (std::isspace(c = strm.get()));
    while (c == '/')
    {
        char _c = strm.get();
        if (_c == '/')
        {
            while ((c = strm.get()) != '\n');
            while (std::isspace(c = strm.get()));
        }

        else if (_c == '*')
        {
            _c = strm.get();
            while (c != '*' || _c != '/')
            {
                c = _c;
                _c = strm.get();
            }
            while (std::isspace(c = strm.get()));
        }

        else
        {
            unget();
            break;
        }
    }

    FLYNT_ASSERT(c != EOF && "Reached EOF!");

    auto offset = off_t(strm.tellg()) - 1;

    if (std::isdigit(c))
        return lexNumber(c, offset);

    if (c == '.') {
        char _c = strm.get();
        unget();
        if (std::isdigit(_c))
            return lexNumber(c, offset);
    }

    if (c == '"' || c == '\'')
        return lexCharOrString(c, offset);

    if (std::isalpha(c) || c == '_')
        return lexIdOrKeyword(c, offset);

    switch (c) {
    case '+':
    case '-':
    case '%':
    case '!':
    case '@':
    case '#':
    case '$':
    case '^':
    case '&':
    case '*':
    case '(':
    case ')':
    case '=':
    case '{':
    case '}':
    case '[':
    case ']':
    case '|':
    case ':':
    case ';':
    case '<':
    case '>':
    case ',':
    case '.':
    case '?':
    case '~':
    case '/':
        return lexOperatorOrSymbol(c, offset);
    default:
        FLYNT_ASSERT(false && "Invalid character being lexed!");
    }
}

Token *Lexer::lexNumber(char start, off_t offset)
{
    std::string value;

    bool dot = false;
    for (; std::isdigit(start) || (!dot && start == '.' && (dot = true)); start = strm.get())
        value.push_back(start);

    if (value.size() == 1 && value.front() == '0') {
        switch (start) {
        case 'b':
        case 'B':
            value.clear();
            for (start = strm.get(); start == '1' || start == '0'; value.push_back(start), start = strm.get());
            unget();
            return new Token(Token::B_INT, value, offset);
        case 'o':
        case 'O':
            value.clear();
            for (start = strm.get(); start >= '0' && start <= '7'; value.push_back(start), start = strm.get());
            unget();
            return new Token(Token::O_INT, value, offset);
        case 'x':
        case 'X':
            value.clear();
            for (start = strm.get(); std::isdigit(start) || ((start >= 'a' && start <= 'f') || (start >= 'A' && start <= 'F')); value.push_back(start), start = strm.get());
            unget();
            return new Token(Token::X_INT, value, offset);
        case 't':
        case 'T':
            value.clear();
            for (start = strm.get(); std::isalnum(start); value.push_back(start), start = strm.get());
            unget();
            return new Token(Token::T_INT, value, offset);
        default:
            break;
        }
    }

    if (value.back() == '.') {
        value.pop_back();
        dot = false;
        unget();
    }
    unget();
    return new Token(dot ? Token::FLOAT : Token::INT, value, offset);
}

Token *Lexer::lexCharOrString(char start, off_t offset)
{
    std::string value;
    char c;
    bool slash = false;
    for (c = strm.get(); slash || c != start; c = strm.get())
    {
        if (slash)
            slash = false;
        else if (c == '\\')
            slash = true;
        value.push_back(c);
    }

    if (start == '"')
        return new Token(Token::STR, value, offset);

    FLYNT_ASSERT(!value.empty());
    FLYNT_ASSERT(value.size() != 1 || value.front() != '\\');

    if (value.size() == 1)
        return new Token(Token::CHAR, value, offset);

    if (value.size() == 2 && value.front() == '\\')
        return new Token(Token::CHAR, value, offset);

    if (value.front() == '\\')
    {
        switch (value[1])
        {
        case 'b':
        case 'B':
            for (int i = 2; i < value.size(); ++i)
                if (i > 9 || (value[i] != '1' && value[i] != '0'))
                    return new Token(Token::STR, value, offset);
            return new Token(Token::CHAR, value, offset);
        case 'o':
        case 'O':
            for (int i = 2; i < value.size(); ++i)
                if ((i > 4 || (i == 4 && value[2] > '3')) || (value[i] < '0' || value[i] > '7'))
                    return new Token(Token::STR, value, offset);
            return new Token(Token::CHAR, value, offset);
        case 'x':
        case 'X':
            for (int i = 2; i < value.size(); ++i)
                if (i > 3 || (!std::isdigit(value[i]) && (value[i] < 'a' || value[i] > 'f') && (value[i] < 'A' || value[i] > 'F')))
                    return new Token(Token::STR, value, offset);
            return new Token(Token::CHAR, value, offset);
        case 't':
        case 'T':
            for (int i = 2; i < value.size(); ++i)
                if ((i > 3 || (i == 3 && (value[i - 1] > '7' || (value[i - 1] == '7' && value[i] > '3')))) || !std::isalnum(value[i]))
                    return new Token(Token::STR, value, offset);
            return new Token(Token::CHAR, value, offset);

        case 'u':
        case 'U':
            for (int i = 2; i < value.size(); ++i)
                if ((i > 6 || (i == 6 && (value[i] > '1' || value[i - 1] > '0'))) || (!std::isdigit(value[i]) && (value[i] < 'a' || value[i] > 'f') && (value[i] < 'A' || value[i] > 'F')))
                    return new Token(Token::STR, value, offset);
            return new Token(Token::CHAR, value, offset);

        default:
            return new Token(Token::STR, value, offset);
        }
    }
    return new Token(Token::STR, value, offset);
}

Token *Lexer::lexIdOrKeyword(char start, off_t offset)
{
    std::string value;
    for (; std::isalnum(start) || start == '_'; start = strm.get())
        value.push_back(start);

    unget();
    auto bt = Lexer::staticData.narrow({value});
    return bt ? new Token(bt.data().type, offset) : new Token(Token::ID, value, offset);
}

Token *Lexer::lexOperatorOrSymbol(char start, off_t offset)
{
    std::string holder;
    for (int i = 0; !strm.eof() && i < 3; ++i, start = strm.get())
        holder.push_back(start);
    if (!strm.eof())
        unget();

    flynt::util::BinaryTree<Lexer::StaticData> bTree;
    int i = 0;
    for (bTree = Lexer::staticData.narrow(holder); !bTree && i < holder.size(); ++i, bTree = Lexer::staticData.narrow(holder))
    {
        holder.pop_back();
        unget();
    }

    FLYNT_ASSERT(bTree);
    return new Token(bTree.data().type, offset);
}

void Lexer::collapseBuffer()
{
    buffer.emplace_front(last);
    for (auto it = buffer.rbegin(); it != (buffer.rend() - 2); ++it)
    {
        auto next = *it, cur = *(it + 1), prev = *(it + 2);

        // [left] [binary] [right]
        uint8_t flags = 0b111;

        if (next->isBinary() || next->isRight() || next->isSymbol(1) || next->isKeyword())
        {
            cur->coerceToRight();
            continue;
        }
        else if (next->isLeft() || next->isId() || next->isLiteral() || next->isOpen())
            flags = 0b110;

        // if (!prev || prev->isSymbol(-1) || prev->isLeft() || prev->isBinary())
        //     flags &= 0b100;
        // else if (prev->isRight() || prev->isKeyword() || prev->isId() || prev->isLiteral() || prev->isClosed())
        //     flags &= 0b011;

        if (!prev || prev->isSymbol(-1) || prev->isLeftCoercible() || prev->isBinaryCoercible() || prev->isKeyword())
            flags &= 0b100;
        else if (prev->isRightCoercible() || prev->isId() || prev->isLiteral() || prev->isClosed())
            flags &= 0b011;

        switch (flags)
        {
        case 0b100:
            cur->coerceToLeft();
            break;

        case 0b010:
            cur->coerceToBinary();
            break;

        case 0b001:
            cur->coerceToRight();
            break;

        default:
            if (cur->isRightCoercible() && cur->isLeftCoercible())
                FLYNT_ASSERT(flags == 0b100 || flags == 0b010 || flags == 0b001);
            break;
        }
    }
    buffer.pop_front();
}

}
