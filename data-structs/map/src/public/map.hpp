#pragma once

#include <functional>
#include <initializer_list>
#include <stack>
#include <stdexcept>
#include <utility>

template <class Key, class Value, class Compare = std::less<Key>>
class map {
   public:
    struct const_iterator;

    map();
    explicit map(const Compare& comparator);
    map(const std::initializer_list<std::pair<Key, Value>>& init_list);
    map(const map& other);
    map& operator=(const map& other);
    ~map();

    std::pair<const_iterator, bool> insert(const std::pair<Key, Value>& newPair);
    std::pair<const_iterator, bool> insert(const Key& key, const Value& value);
    bool has_key(const Key& key) const noexcept;
    bool remove(const Key& key);

    std::pair<Key, Value>& at(const Key& key);

    size_t size() const noexcept;
    bool empty() const noexcept;

   private:
    using Data = std::pair<Key, Value>;

    struct Node {
        Data data;
        Node* left;
        Node* right;

        Node(const Data& data, Node* left = nullptr, Node* right = nullptr)
            : data(data), left(left), right(right) {}
    };

   public:
    struct const_iterator {
        const_iterator(Node* root);
        const Data& operator*() const;
        const_iterator& operator++();
        const_iterator operator++(int);
        bool operator==(const const_iterator& other) const noexcept;
        bool operator!=(const const_iterator& other) const noexcept;

       private:
        std::stack<Node*> stack;
        void push_left(Node* node);
    };

    const_iterator begin() const noexcept;
    const_iterator end() const noexcept;

   private:
    Node* root;
    size_t _size;
    Compare comparator;

    Node** findMinNodeFor(Node** curr) const noexcept;

    void _free(Node* curr);
    Node* _copy(Node* curr);
};

template <class Key, class Value, class Compare>
inline map<Key, Value, Compare>::map() : root(nullptr), _size(0), comparator(Compare()) {}

template <class Key, class Value, class Compare>
inline map<Key, Value, Compare>::map(const Compare& comparator)
    : root(nullptr), _size(0), comparator(comparator) {}

template <class Key, class Value, class Compare>
inline map<Key, Value, Compare>::map(const std::initializer_list<std::pair<Key, Value>>& init_list)
    : map() {
    for (const auto& pair : init_list) {
        insert(pair);
    }
}

template <class Key, class Value, class Compare>
inline map<Key, Value, Compare>::map(const map& other)
    : root(_copy(other.root)), _size(other._size), comparator(other.comparator) {}

template <class Key, class Value, class Compare>
inline map<Key, Value, Compare>::~map() {
    _free(root);
}

template <class Key, class Value, class Compare>
inline std::pair<typename map<Key, Value, Compare>::const_iterator, bool>
map<Key, Value, Compare>::insert(const std::pair<Key, Value>& newPair) {
    return insert(newPair.first, newPair.second);
}

template <class Key, class Value, class Compare>
inline std::pair<typename map<Key, Value, Compare>::const_iterator, bool>
map<Key, Value, Compare>::insert(const Key& key, const Value& value) {
    Node** current = &root;
    while (*current) {
        if (comparator(key, (*current)->data.first)) {
            current = &(*current)->left;
        } else if (comparator((*current)->data.first, key)) {
            current = &(*current)->right;
        } else {
            (*current)->data.second = value;
            return {const_iterator(*current), false};
        }
    }

    *current = new Node(std::make_pair(key, value));
    ++_size;
    return {const_iterator(*current), true};
}

template <class Key, class Value, class Compare>
inline bool map<Key, Value, Compare>::has_key(const Key& key) const noexcept {
    Node* const* current = &root;
    while (*current) {
        if (comparator(key, (*current)->data.first)) {
            current = &(*current)->left;
        } else if (comparator((*current)->data.first, key)) {
            current = &(*current)->right;
        } else {
            return true;
        }
    }

    return false;
}

template <class Key, class Value, class Compare>
inline bool map<Key, Value, Compare>::remove(const Key& key) {
    Node** current = &root;
    while (*current) {
        if (comparator((*current)->data.first, key))
            current = &(*current)->right;
        else if (comparator(key, (*current)->data.first))
            current = &(*current)->left;
        else
            break;
    }

    if (!(*current)) return false;

    Node* toDelete = *current;
    if (!toDelete->left && !toDelete->right)
        *current = nullptr;
    else if (!toDelete->left)
        *current = toDelete->right;
    else if (!toDelete->right)
        *current = toDelete->left;
    else {
        Node** minRight = findMinNodeFor(&toDelete->right);
        *current = *minRight;
        *minRight = (*minRight)->right;
        (*current)->left = toDelete->left;
        (*current)->right = toDelete->right;
    }

    delete toDelete;
    --_size;
    return true;
}

template <class Key, class Value, class Compare>
inline std::pair<Key, Value>& map<Key, Value, Compare>::at(const Key& key) {
    Node** current = &root;
    while (*current) {
        if (comparator(key, (*current)->data.first)) {
            current = &(*current)->left;
        } else if (comparator((*current)->data.first, key)) {
            current = &(*current)->right;
        } else {
            return (*current)->data;
        }
    }

    throw std::out_of_range("Key not found");
}

template <class Key, class Value, class Compare>
inline size_t map<Key, Value, Compare>::size() const noexcept {
    return _size;
}

template <class Key, class Value, class Compare>
inline bool map<Key, Value, Compare>::empty() const noexcept {
    return _size == 0;
}

template <class Key, class Value, class Compare>
inline typename map<Key, Value, Compare>::const_iterator map<Key, Value, Compare>::begin()
    const noexcept {
    return const_iterator(root);
}

template <class Key, class Value, class Compare>
inline typename map<Key, Value, Compare>::const_iterator map<Key, Value, Compare>::end()
    const noexcept {
    return const_iterator(nullptr);
}

template <class Key, class Value, class Compare>
inline typename map<Key, Value, Compare>::Node** map<Key, Value, Compare>::findMinNodeFor(
    Node** curr) const noexcept {
    while ((*curr)->left) {
        curr = &(*curr)->left;
    }
    return curr;
}

template <class Key, class Value, class Compare>
inline void map<Key, Value, Compare>::_free(Node* current) {
    if (!current) return;
    _free(current->left);
    _free(current->right);
    delete current;
}

template <class Key, class Value, class Compare>
inline typename map<Key, Value, Compare>::Node* map<Key, Value, Compare>::_copy(Node* current) {
    if (!current) return nullptr;

    Node* newNode = new Node(current->data);
    newNode->left = _copy(current->left);
    newNode->right = _copy(current->right);

    return newNode;
}

template <class Key, class Value, class Compare>
void map<Key, Value, Compare>::const_iterator::push_left(Node* node) {
    while (node) {
        stack.push(node);
        node = node->left;
    }
}

template <class Key, class Value, class Compare>
map<Key, Value, Compare>::const_iterator::const_iterator(Node* root) {
    push_left(root);
}

template <class Key, class Value, class Compare>
inline const typename map<Key, Value, Compare>::Data&
map<Key, Value, Compare>::const_iterator::operator*() const {
    return stack.top()->data;
}

template <class Key, class Value, class Compare>
inline typename map<Key, Value, Compare>::const_iterator&
map<Key, Value, Compare>::const_iterator::operator++() {
    Node* current = stack.top();
    stack.pop();
    push_left(current->right);
    return *this;
}

template <class Key, class Value, class Compare>
inline typename map<Key, Value, Compare>::const_iterator
map<Key, Value, Compare>::const_iterator::operator++(int) {
    auto temp = *this;
    ++(*this);
    return temp;
}

template <class Key, class Value, class Compare>
inline bool map<Key, Value, Compare>::const_iterator::operator!=(
    const const_iterator& other) const noexcept {
    return !(*this == other);
}

template <class Key, class Value, class Compare>
inline bool map<Key, Value, Compare>::const_iterator::operator==(
    const const_iterator& other) const noexcept {
    return stack == other.stack;
}