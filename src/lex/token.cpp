#include "lex/token.h"
#include <ostream>

namespace flynt::lex {

const char *Token::valueOf(Type type)
{
    return Token::typeValues[type];
}

const char *Token::nameOf(Type type)
{
    return Token::typeNames[type];
}

Token::Token() : type(UNKNOWN), value(), offset(0) {}

Token::Token(Type type, std::string value, off_t offset) : type(type), value(value), offset(offset) {}

Token::Token(Type type, off_t offset) : type(type), value(), offset(offset) {}


Token::Type Token::getType() const
{
    return type;
}

void Token::setType(Type newType)
{
    type = newType;
}

std::string Token::getValue() const
{
    if (value.empty())
        return Token::valueOf(type);
    return value;
}

std::string Token::expandedValue() const
{
    switch (type)
    {
    case STR:
        return "\"" + getValue() + "\"";
    case CHAR:
        return "'" + getValue() + "'";
    case B_INT:
        return "0b" + getValue();
    case O_INT:
        return "0o" + getValue();
    case X_INT:
        return "0x" + getValue();
    case T_INT:
        return "0t" + getValue();
    default:
        return getValue();
    }
}

void Token::setValue(const std::string &newValue)
{
    value = newValue;
}

off_t Token::getOffset() const
{
    return offset;
}

void Token::setOffset(const off_t &newOffset)
{
    offset = newOffset;
}

bool Token::isId() const
{
    return type == ID;
}

bool Token::isUnknown() const
{
    return type == UNKNOWN;
}

bool Token::isLiteral() const
{
    return type >= INT && type <= STR;
}

bool Token::isKeyword() const
{
    return type >= VAR && type <= LINK;
}

bool Token::isAccessModifier() const
{
    return type >= PRIV && type <= ABSTR;
}

bool Token::isDeclarator() const
{
    return type >= VAR && type <= SPACE;
}

bool Token::isControlFlow() const
{
    return type >= IF && type <= CONTRACT;
}

bool Token::isMeta() const
{
    return type >= IMP && type <= LINK;
}

bool Token::isOperator() const
{
    return type >= SCOPE && type <= COMMA;
}

bool Token::isBinary() const
{
    switch (type) {
    case SCOPE:

    case DOT:

    case MUL:
    case DIV:
    case MOD:

    case ADD:
    case SUB:

    case RS:
    case LS:

    case LT:
    case GT:
    case LE:
    case GE:

    case EE:
    case NE:

    case B_AND:

    case B_XOR:

    case B_OR:

    case AND:

    case OR:

    case RANGE:
    case RANGE_EQ:

    case EQ:
    case ADD_EQ:
    case SUB_EQ:
    case MUL_EQ:
    case DIV_EQ:
    case MOD_EQ:
    case RS_EQ:
    case LS_EQ:
    case B_AND_EQ:
    case B_XOR_EQ:
    case B_OR_EQ:
        return true;
    default:
        return false;
    }
}

bool Token::isUnary() const
{
    return isRight() || isLeft();
}

bool Token::isRight() const
{
    return type >= R_INC && type < DOT;
}

bool Token::isLeft() const
{
    switch (type) {
    case THROW:
    case YIELD:
        return true;
    default:
        return type >= L_INC && type <= DROP;
    }
}

bool Token::isBinaryCoercible() const
{
    return isAmbiguous();
}

bool Token::isRightCoercible() const
{
    switch (type) {
    case THROW:
    case YIELD:
    case LOT:
    case DROP:
    case AWAIT:
        return false;
    default:
        return isUnary() || isAmbiguous();
    }
}

bool Token::isLeftCoercible() const
{
    return isUnary() || isAmbiguous();
}

bool Token::isAmbiguous() const
{
    switch (type) {
    case R_PLUS:
    case L_PLUS:
    case ADD:

    case R_MINUS:
    case L_MINUS:
    case SUB:

    case R_REF:
    case L_REF:
    case B_AND:

    case R_DEREF:
    case L_DEREF:
    case MUL:

    case R_PERCENT:
    case L_PERCENT:
    case MOD:

    case R_XOR:
    case L_XOR:
    case B_XOR:
        return true;
    default:
        return false;
    }
}

bool Token::isSymbol() const
{
    return type >= O_PAREN && type <= DOUBLE_ARROW;
}

bool Token::isSymbol(int8_t exclude)
{
    switch (type) {
    case O_PAREN:
    case O_BRACE:
    case O_BRACKET:
    case O_ANGLE:
        return exclude <= 0;

    case C_PAREN:
    case C_BRACE:
    case C_BRACKET:
    case C_ANGLE:
        return exclude >= 0;

    case SEMI_COLON:
    case COLON:
    case ARROW:
    case DOUBLE_ARROW:
        return exclude != 0;

    default:
        return false;
    }
}

bool Token::isOpen() const
{
    switch (type) {
    case O_PAREN:
    case O_BRACE:
    case O_BRACKET:
    case O_ANGLE:
        return true;
    default:
        return false;
    }
}

bool Token::isClosed() const
{
    switch (type) {
    case C_PAREN:
    case C_BRACE:
    case C_BRACKET:
    case C_ANGLE:
        return true;
    default:
        return false;
    }
}

bool Token::isNonScopedSymbol() const
{
    switch (type) {
    case SEMI_COLON:
    case COLON:
    case ARROW:
    case DOUBLE_ARROW:
        return true;
    default:
        return false;
    }
}

bool Token::isSemiColon() const
{
    return type == SEMI_COLON;
}

bool Token::isArrow() const
{
    return type == ARROW;
}

bool Token::isDoubleArrow() const
{
    return type == DOUBLE_ARROW;
}

bool Token::isColon() const
{
    return type == COLON;
}

uint8_t Token::getPrecedence() const
{
    switch (type) {
    case SCOPE:
        return 1;
    case R_INC:
    case R_DEC:
    case R_PLUS:
    case R_MINUS:
    case R_REF:
    case R_DEREF:
    case R_NOT:
    case R_B_NOT:
    case R_DOLLAR:
    case R_XOR:
    case R_AT:
    case R_PERCENT:
    case R_Q_MARK:
    case R_SPREAD:
    case DOT:
        return 2;
    case L_INC:
    case L_DEC:
    case L_PLUS:
    case L_MINUS:
    case L_REF:
    case L_DEREF:
    case L_NOT:
    case L_B_NOT:
    case L_DOLLAR:
    case L_XOR:
    case L_AT:
    case L_PERCENT:
    case L_Q_MARK:
    case L_SPREAD:
    case AWAIT:
    case LOT:
    case DROP:
        return 3;
    case MUL:
    case DIV:
    case MOD:
        return 4;
    case ADD:
    case SUB:
        return 5;
    case RS:
    case LS:
        return 6;
    case LT:
    case GT:
    case LE:
    case GE:
        return 7;
    case EE:
    case NE:
        return 8;
    case B_AND:
        return 9;
    case B_XOR:
        return 10;
    case B_OR:
        return 11;
    case AND:
        return 12;
    case OR:
        return 13;
    case RANGE:
    case RANGE_EQ:
        return 14;
    case THROW:
    case YIELD:
    case EQ:
    case ADD_EQ:
    case SUB_EQ:
    case MUL_EQ:
    case DIV_EQ:
    case MOD_EQ:
    case RS_EQ:
    case LS_EQ:
    case B_AND_EQ:
    case B_XOR_EQ:
    case B_OR_EQ:
        return 15;
    case COMMA:
        return 16;
    default:
        return 0;
    }
}

uint8_t Token::getScope() const
{
    switch (type) {
    case O_PAREN:
    case O_BRACE:
    case O_BRACKET:
    case O_ANGLE:
        return 1;
    case C_PAREN:
    case C_BRACE:
    case C_BRACKET:
    case C_ANGLE:
        return -1;
    default:
        return 0;
    }
}

void Token::coerceToBinary()
{
    switch (type) {
    case R_PLUS:
    case L_PLUS:
        type = ADD;
        return;

    case R_MINUS:
    case L_MINUS:
        type = SUB;
        return;

    case R_REF:
    case L_REF:
        type = B_AND;
        return;

    case R_DEREF:
    case L_DEREF:
        type = MUL;
        return;

    case R_PERCENT:
    case L_PERCENT:
        type = MOD;
        return;

    case R_XOR:
    case L_XOR:
        type = B_XOR;
        return;

    default:
        return;
    }
}

void Token::coerceToRight()
{
    switch (type) {
    case L_INC:
    case L_DEC:
    case L_PLUS:
    case L_MINUS:
    case L_REF:
    case L_DEREF:
    case L_NOT:
    case L_B_NOT:
    case L_DOLLAR:
    case L_XOR:
    case L_AT:
    case L_PERCENT:
    case L_Q_MARK:
    case L_SPREAD:
        type = Type(int(type) - (L_INC - R_INC));
        return;

    case ADD:
        type = R_PLUS;
        return;

    case SUB:
        type = R_MINUS;
        return;

    case MUL:
        type = R_DEREF;
        return;

    case B_AND:
        type = R_REF;
        return;

    case MOD:
        type = R_PERCENT;
        return;

    case B_XOR:
        type = R_XOR;
        return;

    default:
        return;
    }
}

void Token::coerceToLeft()
{
    switch (type) {
    case R_INC:
    case R_DEC:
    case R_PLUS:
    case R_MINUS:
    case R_REF:
    case R_DEREF:
    case R_NOT:
    case R_B_NOT:
    case R_DOLLAR:
    case R_XOR:
    case R_AT:
    case R_PERCENT:
    case R_Q_MARK:
    case R_SPREAD:
        type = Type(int(type) + (L_INC - R_INC));
        return;

    case ADD:
        type = L_PLUS;
        return;

    case SUB:
        type = L_MINUS;
        return;

    case MUL:
        type = L_DEREF;
        return;

    case B_AND:
        type = L_REF;
        return;

    case MOD:
        type = L_PERCENT;
        return;

    case B_XOR:
        type = L_XOR;
        return;

    default:
        return;
    }
}

bool Token::operator==(const Token &other) const
{
    return type == other.type && offset == other.offset && value == other.value;
}

bool Token::operator==(Type type) const
{
    return Token::type == type;
}

bool Token::operator==(const std::string &value) const
{
    return getValue() == value;
}

const char *Token::typeValues[Token::END] = {
    "",

    "",

    // Literals
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",

    // Keywords
    "var",
    "vol",
    "type",
    "fn",
    "idea",
    "templ",
    "impl",
    "space",

    "priv",
    "prot",
    "pub",
    "stat",
    "fin",
    "abstr",

    "if",
    "while",
    "until",
    "else",
    "for",
    "pick",
    "ret",
    "brk",
    "cont",
    "contract",

    "imp",
    "exp",
    "ext",
    "link",

    // Operators
    "::",

    "++",
    "--",
    "+",
    "-",
    "&",
    "*",
    "!",
    "~",
    "$",
    "^",
    "@",
    "%",
    "?",
    "...",
    ".",

    "++",
    "--",
    "+",
    "-",
    "&",
    "*",
    "!",
    "~",
    "$",
    "^",
    "@",
    "%",
    "?",
    "...",
    "await",
    "lot",
    "drop",

    "*",
    "/",
    "%",

    "+",
    "-",

    ">>",
    "<<",

    "<",
    ">",
    "<=",
    ">=",

    "==",
    "!=",

    "&",

    "^",

    "|",

    "&&",

    "||",

    "..",
    "..=",

    "throw",
    "yield",
    "=",
    "+=",
    "-=",
    "*=",
    "/=",
    "%=",
    ">>=",
    "<<=",
    "&=",
    "^=",
    "|=",

    ",",

    // Symbols
    "(",
    ")",

    "{",
    "}",

    "[",
    "]",

    "<",
    ">",

    ";",
    ":",
    "->",
    "=>",
};

const char *Token::typeNames[Token::END] = {
    "UNKNOWN",

    "ID",

    // Literals
    "INT",
    "B_INT",
    "O_INT",
    "X_INT",
    "T_INT",
    "FLOAT",
    "CHAR",
    "STR",

    // Keywords
    "VAR",
    "VOL",
    "TYPE",
    "FN",
    "IDEA",
    "TEMPL",
    "IMPL",
    "SPACE",

    "PRIV",
    "PROT",
    "PUB",
    "STAT",
    "FIN",
    "ABSTR",

    "IF",
    "WHILE",
    "UNTIL",
    "ELSE",
    "FOR",
    "PICK",
    "RET",
    "BRK",
    "CONT",
    "CONTRACT",

    "IMP",
    "EXP",
    "EXT",
    "LINK",

    // Operators
    "SCOPE",

    "R_INC",
    "R_DEC",
    "R_PLUS",
    "R_MINUS",
    "R_REF",
    "R_DEREF",
    "R_NOT",
    "R_B_NOT",
    "R_DOLLAR",
    "R_XOR",
    "R_AT",
    "R_PERCENT",
    "R_Q_MARK",
    "R_SPREAD",
    "DOT",

    "L_INC",
    "L_DEC",
    "L_PLUS",
    "L_MINUS",
    "L_REF",
    "L_DEREF",
    "L_NOT",
    "L_B_NOT",
    "L_DOLLAR",
    "L_XOR",
    "L_AT",
    "L_PERCENT",
    "L_Q_MARK",
    "L_SPREAD",
    "AWAIT",
    "LOT",
    "DROP",

    "MUL",
    "DIV",
    "MOD",

    "ADD",
    "SUB",

    "RS",
    "LS",

    "LT",
    "GT",
    "LE",
    "GE",

    "EE",
    "NE",

    "B_AND",

    "B_XOR",

    "B_OR",

    "AND",

    "OR",

    "RANGE",
    "RANGE_EQ",

    "THROW",
    "YIELD",
    "EQ",
    "ADD_EQ",
    "SUB_EQ",
    "MUL_EQ",
    "DIV_EQ",
    "MOD_EQ",
    "RS_EQ",
    "LS_EQ",
    "B_AND_EQ",
    "B_XOR_EQ",
    "B_OR_EQ",

    "COMMA",

    // Symbols
    "O_PAREN",
    "C_PAREN",

    "O_BRACE",
    "C_BRACE",

    "O_BRACKET",
    "C_BRACKET",

    "O_ANGLE",
    "C_ANGLE",

    "SEMI_COLON",
    "COLON",
    "ARROW",
    "DOUBLE_ARROW",
};

}

void flynt::lex::PrintTo(const Token &token, std::ostream *os) {
    *os << token;
}

std::ostream &flynt::lex::operator<<(std::ostream &strm, const flynt::lex::Token &token) {
    return strm << "Token{ .type = " << flynt::lex::Token::nameOf(token.getType()) << ", .value = " << token.expandedValue() << ", .offset = " << token.getOffset() << " }";
}
