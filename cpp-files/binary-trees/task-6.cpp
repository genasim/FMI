#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include <functional>
#include <vector>

#include "../lib/doctest.h"
#include "./tree.h"

using namespace std;
using IntNode = Node<int>;

bool check_tree_permutation(IntNode* root, int n) {
    if (n < 0) return false;

    vector<bool> seen(n, false);
    int count = 0;

    function<bool(IntNode*)> helper = [&](IntNode* curr) -> bool {
        if (!curr) return true;
        int val = curr->data;

        if (val < 1 || val > n || seen[val - 1]) return false;

        seen[val - 1] = true;
        count++;

        if (!helper(curr->left)) return false;
        if (!helper(curr->right)) return false;

        return true;
    };

    if (!helper(root) || count != n) return false;
    for (bool numberFlag : seen) {
        if (!numberFlag) return false;
    }

    return true;
}

TEST_CASE("Empty tree with n=0 should return true") {
    IntNode* root = nullptr;
    CHECK(check_tree_permutation(root, 0) == true);
    free_tree(root);
}

TEST_CASE("Empty tree with n>0 should return false") {
    IntNode* root = nullptr;
    CHECK(check_tree_permutation(root, 3) == false);
    free_tree(root);
}

TEST_CASE("Single-node tree with value 1 and n=1") {
    IntNode* root = new IntNode(1);
    CHECK(check_tree_permutation(root, 1) == true);
    free_tree(root);
}

TEST_CASE("Single-node tree with wrong value") {
    IntNode* root = new IntNode(5);
    CHECK(check_tree_permutation(root, 1) == false);
    free_tree(root);
}

TEST_CASE("Perfect permutation tree") {
    /*
         2
        / \
       1   3
       n = 3 → values {1,2,3}, all present once
    */
    IntNode* root = new IntNode(2);
    root->left = new IntNode(1);
    root->right = new IntNode(3);

    CHECK(check_tree_permutation(root, 3) == true);

    free_tree(root);
}

TEST_CASE("Invalid permutation tree with non-positive node") {
    /*
         2
        / \
       1   -3
       n = 3 → values {1,2,3}, all present once
    */
    IntNode* root = new IntNode(2);
    root->left = new IntNode(1);
    root->right = new IntNode(-3);

    CHECK_FALSE(check_tree_permutation(root, 3) == true);

    free_tree(root);
}

TEST_CASE("Tree with duplicate values") {
    /*
        1
       / \
      2   2
      n = 3 → duplicate 2, missing 3
    */
    IntNode* root = new IntNode(1);
    root->left = new IntNode(2);
    root->right = new IntNode(2);

    CHECK(check_tree_permutation(root, 3) == false);

    free_tree(root);
}

TEST_CASE("Tree with value outside range") {
    /*
        4
       / \
      1   2
      n = 3 → contains {1,2,4}, missing 3, has extra 4
    */
    IntNode* root = new IntNode(4);
    root->left = new IntNode(1);
    root->right = new IntNode(2);

    CHECK(check_tree_permutation(root, 3) == false);

    free_tree(root);
}

TEST_CASE("Larger correct tree") {
    /*
            4
           / \
          2   5
         / \
        1   3
        n = 5 → {1,2,3,4,5}, all once
    */
    IntNode* root = new IntNode(4);
    root->left = new IntNode(2);
    root->right = new IntNode(5);
    root->left->left = new IntNode(1);
    root->left->right = new IntNode(3);

    CHECK(check_tree_permutation(root, 5) == true);

    free_tree(root);
}

TEST_CASE("Larger incorrect tree (missing element)") {
    /*
            4
           / \
          2   5
         /
        1
        n = 5 → {1,2,4,5}, missing 3
    */
    IntNode* root = new IntNode(4);
    root->left = new IntNode(2);
    root->right = new IntNode(5);
    root->left->left = new IntNode(1);

    CHECK(check_tree_permutation(root, 5) == false);

    free_tree(root);
}
