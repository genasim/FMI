#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include "../lib/doctest.h"
#include "./tree.h"

using namespace std;

template <class T>
size_t tree_height(Node<T>* root) {
    if (root == nullptr) return 0;
    return max(tree_height(root->left), tree_height(root->right)) + 1;
}

TEST_CASE("Empty tree should have height 0") {
    Node<int>* root = nullptr;
    CHECK(tree_height(root) == 0);
    free_tree(root);
}

TEST_CASE("Single-node tree should have height 1") {
    Node<int>* root = new Node<int>(42);
    CHECK(tree_height(root) == 1);
    free_tree(root);
}

TEST_CASE("Perfectly balanced tree") {
    /*
          1
         / \
        2   3
       / \ / \
      4  5 6  7
      Height = 3
    */
    Node<int>* root = new Node<int>(1);
    root->left = new Node<int>(2);
    root->right = new Node<int>(3);
    root->left->left = new Node<int>(4);
    root->left->right = new Node<int>(5);
    root->right->left = new Node<int>(6);
    root->right->right = new Node<int>(7);

    CHECK(tree_height(root) == 3);

    free_tree(root);
}

TEST_CASE("Left-skewed tree (linked list)") {
    /*
        1
       /
      2
     /
    3
    /
   4
   Height = 4
    */
    Node<int>* root = new Node<int>(1);
    root->left = new Node<int>(2);
    root->left->left = new Node<int>(3);
    root->left->left->left = new Node<int>(4);

    CHECK(tree_height(root) == 4);

    free_tree(root);
}

TEST_CASE("Right-skewed tree (linked list)") {
    /*
    1
     \
      2
       \
        3
         \
          4
    Height = 4
    */
    Node<int>* root = new Node<int>(1);
    root->right = new Node<int>(2);
    root->right->right = new Node<int>(3);
    root->right->right->right = new Node<int>(4);

    CHECK(tree_height(root) == 4);

    free_tree(root);
}

TEST_CASE("Unbalanced tree") {
    /*
          10
         /  \
        5    20
       /
      3
     /
    1
    Height = 4
    */
    Node<int>* root = new Node<int>(10);
    root->left = new Node<int>(5);
    root->left->left = new Node<int>(3);
    root->left->left->left = new Node<int>(1);
    root->right = new Node<int>(20);

    CHECK(tree_height(root) == 4);

    free_tree(root);
}
