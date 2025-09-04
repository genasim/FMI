#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include "../lib/doctest.h"
#include "./tree.h"

bool contains(Node<int>* tree, int value) {
    if (tree == nullptr) return false;

    if (tree->data == value) return true;
    return contains(tree->left, value) || contains(tree->right, value);
}

TEST_CASE("Empty tree should not contain any value") {
    Node<int>* root = nullptr;
    CHECK_FALSE(contains(root, 10));
    free_tree(root);  // safe for nullptr
}

TEST_CASE("Single-node tree") {
    Node<int>* root = new Node<int>(42);
    CHECK(contains(root, 42));
    CHECK_FALSE(contains(root, 0));
    free_tree(root);
}

TEST_CASE("Multi-level tree with values present and absent") {
    /*
         10
        /  \
       5    20
      / \   /
     3   7 15
    */
    Node<int>* root = new Node<int>(10);
    root->left = new Node<int>(5);
    root->right = new Node<int>(20);
    root->left->left = new Node<int>(3);
    root->left->right = new Node<int>(7);
    root->right->left = new Node<int>(15);

    CHECK(contains(root, 10));         // root
    CHECK(contains(root, 7));          // leaf
    CHECK(contains(root, 15));         // internal
    CHECK_FALSE(contains(root, 100));  // absent
    CHECK_FALSE(contains(root, -5));

    free_tree(root);
}

TEST_CASE("Tree with duplicate values") {
    /*
         5
        / \
       5   5
    */
    Node<int>* root = new Node<int>(5);
    root->left = new Node<int>(5);
    root->right = new Node<int>(5);

    CHECK(contains(root, 5));
    CHECK_FALSE(contains(root, 10));

    free_tree(root);
}

TEST_CASE("Tree with negative numbers") {
    /*
        -10
        /  \
      -20   0
    */
    Node<int>* root = new Node<int>(-10);
    root->left = new Node<int>(-20);
    root->right = new Node<int>(0);

    CHECK(contains(root, -10));
    CHECK(contains(root, -20));
    CHECK(contains(root, 0));
    CHECK_FALSE(contains(root, 5));

    free_tree(root);
}
