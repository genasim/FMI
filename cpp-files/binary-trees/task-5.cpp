#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "../lib/doctest.h"
#include "./tree.h"

template <class T>
size_t count_nodes(Node<T>* root) {
    if (root == nullptr) return 0;
    return 1 + count_nodes(root->left) + count_nodes(root->right);
}

TEST_CASE("Empty tree should have count 0") {
    Node<int>* root = nullptr;
    CHECK(count_nodes(root) == 0);
    free_tree(root);
}

TEST_CASE("Single-node tree should have count 1") {
    Node<int>* root = new Node<int>(42);
    CHECK(count_nodes(root) == 1);
    free_tree(root);
}

TEST_CASE("Perfectly balanced tree") {
    /*
          1
         / \
        2   3
       / \ / \
      4  5 6  7
      Count = 7
    */
    Node<int>* root = new Node<int>(1);
    root->left = new Node<int>(2);
    root->right = new Node<int>(3);
    root->left->left = new Node<int>(4);
    root->left->right = new Node<int>(5);
    root->right->left = new Node<int>(6);
    root->right->right = new Node<int>(7);

    CHECK(count_nodes(root) == 7);

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
   Count = 4
    */
    Node<int>* root = new Node<int>(1);
    root->left = new Node<int>(2);
    root->left->left = new Node<int>(3);
    root->left->left->left = new Node<int>(4);

    CHECK(count_nodes(root) == 4);

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
    Count = 4
    */
    Node<int>* root = new Node<int>(1);
    root->right = new Node<int>(2);
    root->right->right = new Node<int>(3);
    root->right->right->right = new Node<int>(4);

    CHECK(count_nodes(root) == 4);

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
    Count = 5
    */
    Node<int>* root = new Node<int>(10);
    root->left = new Node<int>(5);
    root->left->left = new Node<int>(3);
    root->left->left->left = new Node<int>(1);
    root->right = new Node<int>(20);

    CHECK(count_nodes(root) == 5);

    free_tree(root);
}

TEST_CASE("Tree with duplicate values") {
    /*
        7
       / \
      7   7
      Count = 3
    */
    Node<int>* root = new Node<int>(7);
    root->left = new Node<int>(7);
    root->right = new Node<int>(7);

    CHECK(count_nodes(root) == 3);

    free_tree(root);
}
