#pragma once

template <class T>
struct Node {
    T data;
    Node* left;
    Node* right;

    Node(T value) : data(value), left(nullptr), right(nullptr) {}
};

template <class T>
void free_tree(Node<T>* root) {
    if (root == nullptr) return;

    free_tree(root->left);
    free_tree(root->right);
    delete root;
}
