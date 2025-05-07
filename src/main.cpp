#include <iostream>
#include <gtest/gtest.h>
#include "flynt/preprocessor.h"
#include "lex/lexer.h"

int main(int argc, char **argv) {
    flynt::lex::Lexer::init();

#ifdef FLYNT_TEST
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
#else
    std::cout << "Hello World!" << std::endl;
    return 0;
#endif
}
