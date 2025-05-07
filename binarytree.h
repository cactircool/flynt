#ifndef BINARYTREE_H
#define BINARYTREE_H

#include <functional>
#include <ostream>
#include <vector>

namespace flynt::util {

/**
 * @brief The BinaryTree class: very cool lightweight binary tree with narrowing functionality
 */
template <typename T>
class BinaryTree
{
    /**
     * @brief The Node class: BTree node and destructor
     */
    struct Node {
        T data;
        Node *left;
        Node *right;

        Node(const T &data) : data(data), left(nullptr), right(nullptr) {}

        ~Node() {
            delete left;
            delete right;
        }
    };

public:
    /**
     * @brief BinaryTree: default and only constructor defining the created object as the destruction point
     */
    BinaryTree() : root(nullptr), organic(true) {}

    /**
     * @brief ~BinaryTree: delete a binary tree if it is the original point of destruction
     */
    ~BinaryTree() {
        if (organic)
            delete root;
    }

    /**
     * @brief insert: insert data into the BTree
     * @param data
     */
    void insert(const T &data)
    {
        if (!root)
        {
            root = new Node(data);
            return;
        }

        auto *ptr = root;
        while (ptr)
        {
            if (data < ptr->data)
            {
                if (ptr->left) ptr = ptr->left;
                else
                {
                    ptr->left = new Node(data);
                    return;
                }
            }
            else if (data > ptr->data)
            {
                if (ptr->right) ptr = ptr->right;
                else
                {
                    ptr->right = new Node(data);
                    return;
                }
            }
            else
                return;
        }
    }

    /**
     * @brief narrow: narrow down the BTree and return a shallow BTree copy to enable iterative narrowing
     * @param data
     * @return shallow narrowed copy
     */
    BinaryTree narrow(const T &data) const
    {
        auto *ptr = root;
        while (ptr)
        {
            if (std::string(ptr->data) == "...")
                volatile int k = 1;
            if (data < ptr->data)
                ptr = ptr->left;
            else if (data > ptr->data)
                ptr = ptr->right;
            else
                return BinaryTree(ptr);
        }
        return BinaryTree(nullptr);
    }

    /**
     * @brief simpleNarrow: narrow based on a lightweight function pointer
     * @return shallow narrowed copy
     */
    BinaryTree simpleNarrow(int (*pred)(const T &))
    {
        auto *ptr = root;
        while (ptr)
        {
            auto a = pred(ptr->data);
            if (a < 0)
                ptr = ptr->left;
            else if (a > 0)
                ptr = ptr->right;
            else
                return BinaryTree(ptr);
        }
        return BinaryTree(nullptr);
    }

    /**
     * @brief narrow: narrow based on a fat function
     * @param pred
     * @return shallow narrowed copy
     */
    BinaryTree narrow(const std::function<int(const T &)> &pred)
    {
        auto *ptr = root;
        while (ptr)
        {
            auto a = pred(ptr->data);
            if (a < 0)
                ptr = ptr->left;
            else if (a > 0)
                ptr = ptr->right;
            else
                return BinaryTree(ptr);
        }
        return BinaryTree(nullptr);
    }

    /**
     * @brief left: get left of root
     * @return BTree(left)
     */
    BinaryTree left() const
    {
        return BinaryTree(root->left);
    }

    /**
     * @brief right: get right of root
     * @return BTree(right)
     */
    BinaryTree right() const
    {
        return BinaryTree(root->right);
    }

    /**
     * @brief data: get data
     * @return root.data
     */
    T &data()
    {
        return root->data;
    }

    /**
     * @brief data: get data
     * @return root.data
     */
    T &data() const
    {
        return root->data;
    }

    /**
     * @brief operator *: convert to Node
     * @return *root
     */
    Node &operator*() const
    {
        return *root;
    }

    /**
     * @brief operator ==: compare root data
     * @param data
     * @return root.data == data
     */
    bool operator==(const T &data) const
    {
        return root->data == data;
    }

    /**
     * @brief operator bool: root != nullptr
     */
    operator bool() const
    {
        return root;
    }

    /**
     * @brief pathTo: try to construct a path to a specific node
     * @param data
     * @return { path, found }
     */
    std::pair<std::vector<bool>, bool> pathTo(const T &data)
    {
        auto *ptr = root;
        std::vector<bool> path;
        while (ptr)
        {
            if (data < ptr->data)
            {
                ptr = ptr->left;
                path.push_back(false);
            }
            else if (data > ptr->data)
            {
                ptr = ptr->right;
                path.push_back(true);
            }
            else
                return {path, true};
        }
        return {path, false};
    }

    /**
     * @brief operator <<: make std::vector<bool> bits printable
     * @param strm
     * @param bits
     * @return strm
     */
    friend std::ostream &operator<<(std::ostream &strm, const std::vector<bool> &bits)
    {
        for (const auto &a : bits)
            strm << (a + '0');
        return strm;
    }

    /**
     * @brief operator <<: make BTree printable
     * @param strm
     * @param bTree
     * @return strm
     */
    friend std::ostream &operator<<(std::ostream &strm, const BinaryTree &bTree)
    {
        if (!bTree.root)
            return strm << "[ Empty BinaryTree ]";
        BinaryTree::printBinaryTree("", bTree.root, false, strm);
    }

    /**
     * @brief PrintTo: appease the gtest gods
     * @param bTree
     * @param os
     */
    friend void PrintTo(const BinaryTree &bTree, std::ostream *os) { *os << bTree; }

private:
    /**
     * @brief BinaryTree: narrowing constructor
     * @param root
     */
    BinaryTree(Node *root) : root(root), organic(false) {}

    /**
     * @brief printBinaryTree: BTree printing helper
     * @param prefix
     * @param node
     * @param isLeft
     * @param strm
     */
    static void printBinaryTree(const std::string &prefix, const Node *node, bool isLeft, std::ostream &strm)
    {
        if (!node) return;

        strm << prefix;
        strm << (isLeft ? "├──" : "└──" );

        // print the value of the node
        strm << node->data << std::endl;

        // enter the next tree level - left and right branch
        printBinaryTree(prefix + (isLeft ? "│   " : "    "), node->left, true, strm);
        printBinaryTree(prefix + (isLeft ? "│   " : "    "), node->right, false, strm);
    }

    Node *root;
    bool organic;
};

}

#endif // BINARYTREE_H
