#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include <functional>
#include <string>

#include "../lib/doctest.h"
#include "./tree.h"

using namespace std;
using CharNode = Node<char>;

string word_at_level(CharNode* root, size_t level) {
    if (level == 0) return "";

    string word;
    function<void(CharNode*, size_t)> traverser =
        [&](CharNode* node, size_t currLevel) -> void {
        if (node == nullptr) return;

        if (currLevel == level) {
            word += node->data;
            return;
        }

        traverser(node->left, currLevel + 1);
        traverser(node->right, currLevel + 1);
    };

    traverser(root, 1);
    return word;
}

TEST_CASE("Empty tree should return empty string for any k") {
    CharNode* root = nullptr;
    CHECK(word_at_level(root, 1) == "");
    CHECK(word_at_level(root, 5) == "");
    free_tree(root);
}

TEST_CASE("Single-node tree") {
    CharNode* root = new CharNode('A');
    CHECK(word_at_level(root, 1) == "A");
    CHECK(word_at_level(root, 2) == "");  // level does not exist
    free_tree(root);
}

TEST_CASE("Two-level tree") {
    /*
         A
        / \
       B   C
    */
    CharNode* root = new CharNode('A');
    root->left = new CharNode('B');
    root->right = new CharNode('C');

    CHECK(word_at_level(root, 1) == "A");
    CHECK(word_at_level(root, 2) == "BC");
    CHECK(word_at_level(root, 3) == "");  // no deeper nodes

    free_tree(root);
}

TEST_CASE("Three-level unbalanced tree") {
    /*
         A
        / \
       B   C
      /
     D
    */
    CharNode* root = new CharNode('A');
    root->left = new CharNode('B');
    root->right = new CharNode('C');
    root->left->left = new CharNode('D');

    CHECK(word_at_level(root, 1) == "A");
    CHECK(word_at_level(root, 2) == "BC");
    CHECK(word_at_level(root, 3) == "D");
    CHECK(word_at_level(root, 4) == "");

    free_tree(root);
}

TEST_CASE("Four-level irregular tree") {
    /*
             R
            / \
           S   T
            \    \
             U    V
                  /
                 W
    */
    CharNode* root = new CharNode('R');
    root->left = new CharNode('S');
    root->right = new CharNode('T');
    root->left->right = new CharNode('U');
    root->right->right = new CharNode('V');
    root->right->right->left = new CharNode('W');

    CHECK(word_at_level(root, 1) == "R");
    CHECK(word_at_level(root, 2) == "ST");
    CHECK(word_at_level(root, 3) == "UV");
    CHECK(word_at_level(root, 4) == "W");
    CHECK(word_at_level(root, 5) == "");

    free_tree(root);
}

TEST_CASE("Tree with missing nodes at some levels") {
    /*
         A
          \
           B
            \
             C
            /
           D
    */
    CharNode* root = new CharNode('A');
    root->right = new CharNode('B');
    root->right->right = new CharNode('C');
    root->right->right->left = new CharNode('D');

    CHECK(word_at_level(root, 1) == "A");
    CHECK(word_at_level(root, 2) == "B");
    CHECK(word_at_level(root, 3) == "C");
    CHECK(word_at_level(root, 4) == "D");
    CHECK(word_at_level(root, 5) == "");

    free_tree(root);
}