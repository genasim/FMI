#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include <exception>

#include "../lib/doctest.h"
#include "./tree.h"

using namespace std;

template <class T>
T max_element_tree(Node<T>* root) {
    if (!root) throw runtime_error("empty tree");

    T maxValue = root->data;
    if (root->left) maxValue = max(maxValue, max_element_tree(root->left));
    if (root->right) maxValue = max(maxValue, max_element_tree(root->right));

    return maxValue;
}

TEST_CASE("Empty tree should throw or handle gracefully") {
    Node<int>* root = nullptr;
    // Depending on your implementation, either throw or return sentinel value.
    // Here we expect an exception:
    CHECK_THROWS(max_element_tree(root));
    free_tree(root);
}

TEST_CASE("Single-node tree") {
    Node<int>* root = new Node<int>(42);
    CHECK(max_element_tree(root) == 42);
    free_tree(root);
}

TEST_CASE("Multi-level tree with positive numbers") {
    /*
         10
        /  \
       5    20
      / \   /
     3   7 15
     max = 20
    */
    Node<int>* root = new Node<int>(10);
    root->left = new Node<int>(5);
    root->right = new Node<int>(20);
    root->left->left = new Node<int>(3);
    root->left->right = new Node<int>(7);
    root->right->left = new Node<int>(15);

    CHECK(max_element_tree(root) == 20);

    free_tree(root);
}

TEST_CASE("Tree with negative numbers only") {
    /*
        -10
        /  \
      -20  -5
     max = -5
    */
    Node<int>* root = new Node<int>(-10);
    root->left = new Node<int>(-20);
    root->right = new Node<int>(-5);

    CHECK(max_element_tree(root) == -5);

    free_tree(root);
}

TEST_CASE("Tree with duplicates") {
    /*
        7
       / \
      7   7
     max = 7
    */
    Node<int>* root = new Node<int>(7);
    root->left = new Node<int>(7);
    root->right = new Node<int>(7);

    CHECK(max_element_tree(root) == 7);

    free_tree(root);
}

TEST_CASE("Tree with strings (lexicographic max)") {
    /*
         "pear"
         /    \
     "apple"  "orange"
     max = "pear" (lexicographically)
    */
    Node<std::string>* root = new Node<std::string>("pear");
    root->left = new Node<std::string>("apple");
    root->right = new Node<std::string>("orange");

    CHECK(max_element_tree(root) == "pear");

    free_tree(root);
}