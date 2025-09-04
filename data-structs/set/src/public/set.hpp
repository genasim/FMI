#pragma once

#include <functional>
#include <initializer_list>
#include <stack>
#include <stdexcept>
#include <utility>

template <class Value, class Compare = std::less<Value>>
class set {
   public:
    struct iterator;

    set();
    explicit set(const Compare& comparator);
    set(const std::initializer_list<Value>& init_list);
    set(const set& other);
    set(set&& other) noexcept;
    set& operator=(const set& other);
    set& operator=(set&& other) noexcept;
    ~set();

    std::pair<iterator, bool> insert(const Value& value);
    bool erase(const iterator& it);

    bool contains(const Value& value) const noexcept;
    iterator find(const Value& value) const noexcept;

    size_t size() const noexcept;
    bool empty() const noexcept;

   private:
    struct Node {
        Value data;
        Node* left;
        Node* right;

        Node(const Value& data, Node* left = nullptr, Node* right = nullptr)
            : data(data), left(left), right(right) {}
    };

   public:
    struct iterator {
        iterator(Node* root);
        const Value& operator*() const;
        iterator& operator++();
        iterator operator++(int);
        bool operator==(const iterator& other) const noexcept;
        bool operator!=(const iterator& other) const noexcept;

       private:
        std::stack<Node*> nodeStack;
        void push_left(Node* node);
    };

    iterator begin() const noexcept;
    iterator end() const noexcept;

   private:
    Node* root;
    size_t _size;
    Compare comparator;

    Node** findMinNodeFor(Node** curr) const noexcept;

    void _free(Node* curr);
    Node* _copy(Node* curr);
    void _move(set&& other) noexcept;
};

template <class Value, class Compare>
inline set<Value, Compare>::set() : root(nullptr), _size(0), comparator(Compare{}) {}

template <class Value, class Compare>
inline set<Value, Compare>::set(const Compare& comparator)
    : root(nullptr), _size(0), comparator(comparator) {}

template <class Value, class Compare>
inline set<Value, Compare>::set(const std::initializer_list<Value>& init_list) : set() {
    for (const auto& value : init_list) {
        insert(value);
    }
}

template <class Value, class Compare>
inline set<Value, Compare>::set(const set& other)
    : root(_copy(other.root)), _size(other._size), comparator(other.comparator) {}

template <class Value, class Compare>
inline set<Value, Compare>::set(set&& other) noexcept {
    _move(std::move(other));
}

template <class Value, class Compare>
inline set<Value, Compare>& set<Value, Compare>::operator=(const set& other) {
    if (&other != this) {
        _free(root);
        comparator = other.comparator;
        _size = other._size;
        root = _copy(other.root);
    }
    return *this;
}

template <class Value, class Compare>
inline set<Value, Compare>& set<Value, Compare>::operator=(set&& other) noexcept {
    if (&other != this) {
        _free(root);
        _move(std::move(other));
    }
    return *this;
}

template <class Value, class Compare>
inline set<Value, Compare>::~set() {
    _free(root);
}

template <class Value, class Compare>
inline std::pair<typename set<Value, Compare>::iterator, bool> set<Value, Compare>::insert(
    const Value& value) {
    Node** curr = &root;
    while (*curr) {
        if (comparator((*curr)->data, value))
            curr = &(*curr)->right;
        else if (comparator(value, (*curr)->data))
            curr = &(*curr)->left;
        else
            return {iterator(*curr), false};
    }

    *curr = new Node(value);
    ++_size;
    return {iterator(*curr), true};
}

template <class Value, class Compare>
inline bool set<Value, Compare>::erase(const iterator& it) {
    if (it == end()) return false;

    Node** curr = &root;
    while (*curr) {
        if (comparator((*curr)->data, *it))
            curr = &(*curr)->right;
        else if (comparator(*it, (*curr)->data))
            curr = &(*curr)->left;
        else
            break;
    }

    if (*curr == nullptr) return false;

    Node* toDelete = *curr;
    if (!(*curr)->left && !(*curr)->right) {
        *curr = nullptr;
    } else if (!(*curr)->left) {
        *curr = (*curr)->right;
    } else if (!(*curr)->right) {
        *curr = (*curr)->left;
    } else {
        Node** minRight = findMinNodeFor(&(*curr)->right);
        *curr = *minRight;
        *minRight = (*minRight)->right;

        (*curr)->left = toDelete->left;
        (*curr)->right = toDelete->right;
    }

    delete toDelete;
    --_size;
    return true;
}

template <class Value, class Compare>
inline bool set<Value, Compare>::contains(const Value& value) const noexcept {
    Node* const* curr = &root;
    while (*curr) {
        if (comparator((*curr)->data, value))
            curr = &(*curr)->right;
        else if (comparator(value, (*curr)->data))
            curr = &(*curr)->left;
        else
            return true;
    }
    return false;
}

template <class Value, class Compare>
inline typename set<Value, Compare>::iterator set<Value, Compare>::find(
    const Value& value) const noexcept {
    Node* const* curr = &root;
    while (*curr) {
        if (comparator((*curr)->data, value))
            curr = &(*curr)->right;
        else if (comparator(value, (*curr)->data))
            curr = &(*curr)->left;
        else
            return iterator(*curr);
    }

    return end();
}

template <class Value, class Compare>
inline typename set<Value, Compare>::Node** set<Value, Compare>::findMinNodeFor(
    Node** curr) const noexcept {
    while ((*curr)->left) {
        curr = &(*curr)->left;
    }
    return curr;
}

template <class Value, class Compare>
inline void set<Value, Compare>::_free(Node* curr) {
    if (curr == nullptr) return;

    _free(curr->left);
    _free(curr->right);
    delete curr;
}

template <class Value, class Compare>
inline typename set<Value, Compare>::Node* set<Value, Compare>::_copy(Node* curr) {
    if (curr == nullptr) return nullptr;

    Node* copy = new Node(curr->data);
    copy->left = _copy(curr->left);
    copy->right = _copy(curr->right);

    return copy;
}

template <class Value, class Compare>
inline void set<Value, Compare>::_move(set&& other) noexcept {
    root = other.root;
    comparator = std::move(other.comparator);
    _size = other._size;

    other.root = nullptr;
    other._size = 0;
}

template <class Value, class Compare>
inline set<Value, Compare>::iterator::iterator(Node* root) {
    push_left(root);
}

template <class Value, class Compare>
inline const Value& set<Value, Compare>::iterator::operator*() const {
    return nodeStack.top()->data;
}

template <class Value, class Compare>
inline typename set<Value, Compare>::iterator& set<Value, Compare>::iterator::operator++() {
    Node* curr = nodeStack.top();
    nodeStack.pop();
    push_left(curr->right);
    return *this;
}

template <class Value, class Compare>
inline typename set<Value, Compare>::iterator set<Value, Compare>::iterator::operator++(int) {
    iterator it = *this;
    ++(*this);
    return it;
}

template <class Value, class Compare>
inline bool set<Value, Compare>::iterator::operator==(const iterator& other) const noexcept {
    return nodeStack == other.nodeStack;
}

template <class Value, class Compare>
inline bool set<Value, Compare>::iterator::operator!=(const iterator& other) const noexcept {
    return !(*this == other);
}

template <class Value, class Compare>
inline void set<Value, Compare>::iterator::push_left(Node* node) {
    while (node != nullptr) {
        nodeStack.push(node);
        node = node->left;
    }
}

template <class Value, class Compare>
inline typename set<Value, Compare>::iterator set<Value, Compare>::begin() const noexcept {
    return iterator(root);
}

template <class Value, class Compare>
inline typename set<Value, Compare>::iterator set<Value, Compare>::end() const noexcept {
    return iterator(nullptr);
}

template <class Value, class Compare>
inline size_t set<Value, Compare>::size() const noexcept {
    return _size;
}

template <class Value, class Compare>
inline bool set<Value, Compare>::empty() const noexcept {
    return _size == 0;
}