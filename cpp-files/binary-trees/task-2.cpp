#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include <string>

#include "../lib/doctest.h"
#include "./tree.h"

using namespace std;

template <class T>
T sum_tree(Node<T>* root) {
    if (root == nullptr) return T{};
    return sum_tree(root->left) + root->data + sum_tree(root->right);
}

TEST_CASE("Empty tree should return 0") {
    Node<int>* root = nullptr;
    CHECK(sum_tree(root) == 0);
    free_tree(root);  // safe
}

TEST_CASE("Single-node tree") {
    Node<int>* root = new Node<int>(42);
    CHECK(sum_tree(root) == 42);
    free_tree(root);
}

TEST_CASE("Multi-level tree") {
    /*
         10
        /  \
       5    20
      / \   /
     3   7 15
     sum = 10+5+20+3+7+15 = 60
    */
    Node<int>* root = new Node<int>(10);
    root->left = new Node<int>(5);
    root->right = new Node<int>(20);
    root->left->left = new Node<int>(3);
    root->left->right = new Node<int>(7);
    root->right->left = new Node<int>(15);

    CHECK(sum_tree(root) == 60);

    free_tree(root);
}

TEST_CASE("Tree with negative numbers") {
    /*
        -10
        /  \
      -20   5
     sum = -25
    */
    Node<int>* root = new Node<int>(-10);
    root->left = new Node<int>(-20);
    root->right = new Node<int>(5);

    CHECK(sum_tree(root) == -25);

    free_tree(root);
}

TEST_CASE("Tree with duplicates") {
    /*
        5
       / \
      5   5
     sum = 15
    */
    Node<int>* root = new Node<int>(5);
    root->left = new Node<int>(5);
    root->right = new Node<int>(5);

    CHECK(sum_tree(root) == 15);

    free_tree(root);
}

TEST_CASE("Tree with strings (concatenation)") {
    /*
         "a"
         / \
       "b" "c"
    */
    Node<string>* root = new Node<string>("a");
    root->left = new Node<string>("b");
    root->right = new Node<string>("c");
    string sum = sum_tree(root);

    CHECK(sum_tree(root) == "bac");

    free_tree(root);
}
