#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include <functional>
#include <string>
#include <vector>

#include "../lib/doctest.h"
#include "./tree.h"

using namespace std;
using StringNode = Node<string>;

vector<string> collect_root_to_leaf_words(StringNode* root) {
    if (root == nullptr) return vector<string>{};

    vector<string> words;
    function<void(StringNode*, string)> helper = [&](StringNode* curr,
                                                     string word) -> void {
        word += curr->data;
        if (!curr->right && !curr->left) words.push_back(word);

        if (curr->left) helper(curr->left, word);
        if (curr->right) helper(curr->right, word);
    };
    helper(root, "");

    return words;
}

TEST_CASE("Empty tree should return empty vector") {
    StringNode* root = nullptr;
    auto result = collect_root_to_leaf_words(root);
    CHECK(result.empty());
    free_tree(root);
}

TEST_CASE("Single-node tree produces one word") {
    StringNode* root = new StringNode("a");
    auto result = collect_root_to_leaf_words(root);
    CHECK(result == vector<string>{"a"});
    free_tree(root);
}

TEST_CASE("Two-level tree") {
    /*
          a
         / \
        b   c
    Words: "ab", "ac"
    */
    StringNode* root = new StringNode("a");
    root->left = new StringNode("b");
    root->right = new StringNode("c");

    auto result = collect_root_to_leaf_words(root);
    CHECK(result == vector<string>{"ab", "ac"});

    free_tree(root);
}

TEST_CASE("Multi-level tree") {
    /*
           a
          / \
         b   c
        /   / \
       d   e   f
    Words: "abd", "ace", "acf"
    */
    StringNode* root = new StringNode("a");
    root->left = new StringNode("b");
    root->right = new StringNode("c");
    root->left->left = new StringNode("d");
    root->right->left = new StringNode("e");
    root->right->right = new StringNode("f");

    auto result = collect_root_to_leaf_words(root);
    CHECK(result == vector<string>{"abd", "ace", "acf"});

    free_tree(root);
}

TEST_CASE("Unbalanced tree") {
    /*
          x
         /
        y
       /
      z
    Word: "xyz"
    */
    StringNode* root = new StringNode("x");
    root->left = new StringNode("y");
    root->left->left = new StringNode("z");

    auto result = collect_root_to_leaf_words(root);
    CHECK(result == vector<string>{"xyz"});

    free_tree(root);
}

TEST_CASE("Tree with duplicate path prefixes") {
    /*
          a
         / \
        b   b
    Words: "ab", "ab"
    */
    StringNode* root = new StringNode("a");
    root->left = new StringNode("b");
    root->right = new StringNode("b");

    auto result = collect_root_to_leaf_words(root);
    CHECK(result == vector<string>{"ab", "ab"});

    free_tree(root);
}

TEST_CASE("Four-level tree with missing nodes") {
    /*
             a
            / \
           b   c
          /   / \
         d   e   f
          \       \
           g       h
    Words: "abdg", "ace", "acf h"
           → {"abdg", "ace", "acfh"}
    */
    StringNode* root = new StringNode("a");
    root->left = new StringNode("b");
    root->right = new StringNode("c");
    root->left->left = new StringNode("d");
    root->left->left->right = new StringNode("g");
    root->right->left = new StringNode("e");
    root->right->right = new StringNode("f");
    root->right->right->right = new StringNode("h");

    auto result = collect_root_to_leaf_words(root);
    CHECK(result == vector<string>{"abdg", "ace", "acfh"});

    free_tree(root);
}

TEST_CASE("Five-level irregular tree") {
    /*
             r
            / \
           s   t
            \    \
             u    v
            /    /
           w    x
                \
                 y
    Words: "rsuw", "rtvxy"
           → {"rsuw", "rtvxy"}
    */
    StringNode* root = new StringNode("r");
    root->left = new StringNode("s");
    root->right = new StringNode("t");

    root->left->right = new StringNode("u");
    root->left->right->left = new StringNode("w");

    root->right->right = new StringNode("v");
    root->right->right->left = new StringNode("x");
    root->right->right->left->right = new StringNode("y");

    auto result = collect_root_to_leaf_words(root);
    CHECK(result == vector<string>{"rsuw", "rtvxy"});

    free_tree(root);
}
