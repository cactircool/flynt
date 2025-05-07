#include "util/binarytree.h"
#include <gtest/gtest.h>
#include <string>

namespace flynt::util {

TEST(BinaryTreeTest, Constructor) {
    ASSERT_NO_THROW(BinaryTree<std::string>());

    auto tree = new BinaryTree<std::string>();
    ASSERT_NO_THROW(delete tree);
}

TEST(BinaryTreeTest, InsertPathTo) {
    BinaryTree<std::string> tree;
    ASSERT_NO_THROW(tree.insert("hello"));
    ASSERT_NO_THROW(tree.insert("other_thing"));
    ASSERT_NO_THROW(tree.insert("a"));

    {
        auto [path, found] = tree.pathTo("hello");
        ASSERT_TRUE(found);
        ASSERT_TRUE(path.empty());
    }

    {
        auto [path, found] = tree.pathTo("other_thing");
        ASSERT_TRUE(found);
        ASSERT_TRUE(path[0]);
    }

    {
        auto [path, found] = tree.pathTo("a");
        ASSERT_TRUE(found);
        ASSERT_FALSE(path[0]);
    }

    {
        auto [path, found] = tree.pathTo("b");
        ASSERT_FALSE(found);
        ASSERT_FALSE(path[0]);
    }
}

TEST(BinaryTreeTest, Narrow) {
    BinaryTree<std::string> tree;
    ASSERT_NO_THROW(tree.insert("hello"));
    ASSERT_NO_THROW(tree.insert("other_thing"));
    ASSERT_NO_THROW(tree.insert("a"));

    {
        auto bt = tree.narrow("hello");
        ASSERT_EQ(bt.data(), "hello");
        ASSERT_TRUE(bt.left());
        ASSERT_TRUE(bt.right());
    }

    {
        auto bt = tree.simpleNarrow([](const std::string &a) -> int {
            static std::string s = "other_thing";
            return s.compare(a);
        });
        ASSERT_EQ(bt.data(), "other_thing");
        ASSERT_FALSE(bt.left());
        ASSERT_FALSE(bt.right());
    }

    {
        std::string s = "a";
        auto bt = tree.narrow([&](const std::string &a) {
            return s.compare(a);
        });
        ASSERT_EQ(bt.data(), "a");
        ASSERT_FALSE(bt.left());
        ASSERT_FALSE(bt.right());
    }
}

}
