#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include <sstream>
#include <stdexcept>
#include <iostream>

// on true run tests
#define FLYNT_TEST (true)

// on true print all successful FLYNT_ASSERT assertions to std::cerr (they will be green :])
#define FLYNT_PRINT_GOOD_ASSERT (true)

// Basically assert from <cassert> but gtest fails on cassert so I switchted it out for std::runtime_error which is arguably so much worse since it is more expensive and all that jazz but whatever I'm tired fuck off
#define FLYNT_ASSERT(cond) \
{ \
    bool __b = (cond); \
    if (!__b) { \
        std::stringstream ss; \
        ss << "[ " << __func__ << " in " << __FILE__ << ":" << __LINE__ << " ] " << "\033[32m" << "Assertion Successful: (" << #cond << ") -> " << __b << "\033[0m" << std::endl; \
        throw std::runtime_error(ss.str()); \
    } \
    if (FLYNT_PRINT_GOOD_ASSERT) \
        std::cerr << "\033[0;37m" << "[ " << __func__ << " in " << __FILE__ << ":" << __LINE__ << " ] " << "\033[32m" << "Assertion Successful: (" << #cond << ") -> " << __b << "\033[0m" << std::endl; \
}

/**
 * @brief __warning_appeaser____a___asdfasdfasdfasdfasdfasdfasdasdfasdfasdfasdfasdfasdfasdfasdfads: appease the clang-tidy gods
 */
void __warning_appeaser____a___asdfasdfasdfasdfasdfasdfasdasdfasdfasdfasdfasdfasdfasdfasdfads()
{
    FLYNT_ASSERT(true);
}

#endif // PREPROCESSOR_H
