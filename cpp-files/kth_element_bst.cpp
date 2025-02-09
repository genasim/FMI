#include <fstream>
#include <iostream>

using namespace std;

struct Node {
    int data;
    Node* left;
    Node* right;

    Node(int data, Node* left = nullptr, Node* right = nullptr)
        : data(data), right(right), left(left) {}
};

void free_tree(Node* curr) {
    if (curr == nullptr) return;

    free_tree(curr->left);
    free_tree(curr->right);

    delete curr;
}

int getKthElement(Node* root, unsigned k) {
    unsigned count = 0;
    auto traverser = [k, &count](auto&& self, Node* curr) -> int {
        if (curr == nullptr)
            return -1;

        int left = self(self, curr->left);
        if (left != -1) return left;

        if (++count == k) return curr->data;

        int right = self(self, curr->right);
        return right;
    };
    return traverser(traverser, root);
}

int main() {
    Node* tree = new Node(
        50,
        new Node(30, 
            new Node(20, 
                new Node(10), 
                new Node(25)), 
            new Node(40)),
        new Node(70, 
            new Node(60), 
            new Node(80, 
                nullptr, 
                new Node(90))));

    cout << getKthElement(tree, 5) << endl;
    cout << getKthElement(tree, 3) << endl;
    cout << getKthElement(tree, 7) << endl;
    cout << getKthElement(tree, 9) << endl;

    free_tree(tree);
    return 0;
}