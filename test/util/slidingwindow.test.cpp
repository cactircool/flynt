#include "util/slidingwindow.h"
#include <gtest/gtest.h>

namespace flynt::util {

TEST(SlidingWindowTest, Static) {
    SlidingWindow<int, 3> window;
    window.push(1);
    window.push(2);
    window.push(3);

    ASSERT_EQ(window.size(), 3);
    ASSERT_EQ(window[0], 1);
    ASSERT_EQ(window[1], 2);
    ASSERT_EQ(window[2], 3);

    window.pop();
    window.push(4);
    ASSERT_EQ(window[2], 4);

    window.push(5);
    ASSERT_EQ(window[0], 2);
    ASSERT_EQ(window[1], 4);
    ASSERT_EQ(window[2], 5);

    ASSERT_EQ(window.front(), 2);
    ASSERT_EQ(window.back(), 5);
}

TEST(SlidingWindowTest, Dynamic) {
    SlidingWindow<int> window(3);
    window.push(1);
    window.push(2);
    window.push(3);

    ASSERT_EQ(window.size(), 3);
    ASSERT_EQ(window[0], 1);
    ASSERT_EQ(window[1], 2);
    ASSERT_EQ(window[2], 3);

    window.pop();
    window.push(4);
    ASSERT_EQ(window[2], 4);

    window.push(5);
    ASSERT_EQ(window[0], 2);
    ASSERT_EQ(window[1], 4);
    ASSERT_EQ(window[2], 5);

    ASSERT_EQ(window.front(), 2);
    ASSERT_EQ(window.back(), 5);
}

}
