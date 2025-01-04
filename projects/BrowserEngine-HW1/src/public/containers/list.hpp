#pragma once

#include <initializer_list>
#include <memory>

template <class T>
class list {
   public:
    class iterator;
    class const_iterator;

    list();
    list(const list& other);
    list(list&& other);
    list(std::initializer_list<T> init_list);
    ~list();

    list& operator=(const list& other);
    list& operator=(list&& other);

    void push_back(const T& value);
    void push_back(T&& value);

    void push_front(const T& value);
    void push_front(T&& value);

    void pop_back();
    void pop_front();

    iterator insert(const_iterator pos, const T& value);
    iterator insert(const_iterator pos, T&& value);
    iterator insert(const_iterator pos, size_t count, const T& value);
    iterator insert(const_iterator pos, std::initializer_list<T> init_list);

    template <class... Args>
    iterator emplace(const_iterator pos, Args&&... args);

    template <class... Args>
    iterator emplace_back(Args&&... args);

    template <class... Args>
    iterator emplace_front(Args&&... args);

    iterator erase(const_iterator pos);
    iterator erase(const_iterator first, const_iterator last);

    T& front() noexcept;
    const T& front() const noexcept;
    T& back() noexcept;
    const T& back() const noexcept;

    iterator begin() const noexcept;
    iterator end() const noexcept;

    const_iterator cbegin() const noexcept;
    const_iterator cend() const noexcept;

    bool empty() const noexcept;
    size_t size() const noexcept;
    void clear() noexcept;

    template <class Comparator = std::less<T>>
    void sort(Comparator comparator = std::less<T>());

   private:
    struct Node {
        Node(const T& value);
        Node(T&& value);

        T data;
        Node* next;
        Node* prev;
    };

    template <class Comparator = std::less<T>>
    Node* merge_sort(Node* head, Comparator comp);

    template <class Comparator = std::less<T>>
    Node* merge(Node* left, Node* right, Comparator comp);

    Node* get_middle(Node* list);
    void append_back(Node*& head, Node*& tail, Node* element);


    Node* _head = nullptr;
    Node* _tail = nullptr;
    size_t _size = 0;

    std::allocator<Node> _allocator;

    void _free();
    void _copy(const list& other);
    void _move(list&& other);

   public:
    class iterator {
       public:
        iterator(Node* node, const list<T>& list);
        iterator(const const_iterator& other);

        iterator& operator++();
        iterator operator++(int);
        iterator& operator--();
        iterator operator--(int);
        bool operator==(const iterator& other) const noexcept;
        bool operator!=(const iterator& other) const noexcept;
        T& operator*() const noexcept;

        friend class list;

        iterator& operator=(const iterator& other) = default;

       private:
        Node* _node = nullptr;
        const list<T>* _list;
    };

    class const_iterator {
       public:
        const_iterator(Node* data, const list<T>& list);
        const_iterator(const iterator& other);

        const_iterator& operator++();
        const_iterator operator++(int);
        const_iterator& operator--();
        const_iterator operator--(int);
        bool operator==(const const_iterator& other) const noexcept;
        bool operator!=(const const_iterator& other) const noexcept;
        const T& operator*() const noexcept;

        friend class list;

        const_iterator& operator=(const const_iterator& other) = default;

       private:
        Node* _node = nullptr;
        const list<T>* _list;
    };
};

template <class T>
list<T>::iterator::iterator(Node* node, const list<T>& list)
    : _node(node), _list(&list) {}

template <class T>
list<T>::iterator::iterator(const const_iterator& other)
    : _node(other._node), _list(other._list) {}

template <class T>
typename list<T>::iterator& list<T>::iterator::operator++() {
    _node = _node->next;
    return *this;
}

template <class T>
typename list<T>::iterator list<T>::iterator::operator++(int) {
    iterator tmp = *this;
    ++(*this);
    return tmp;
}

template <class T>
typename list<T>::iterator& list<T>::iterator::operator--() {
    if (!_node) {
        _node = _list->_tail;
        return *this;
    }

    _node = _node->prev;
    return *this;
}

template <class T>
typename list<T>::iterator list<T>::iterator::operator--(int) {
    iterator tmp = *this;
    --(*this);
    return tmp;
}

template <class T>
bool list<T>::iterator::operator==(const iterator& other) const noexcept {
    return _node == other._node;
}

template <class T>
bool list<T>::iterator::operator!=(const iterator& other) const noexcept {
    return !(*this == other);
}

template <class T>
T& list<T>::iterator::operator*() const noexcept {
    return _node->data;
}

template <class T>
list<T>::const_iterator::const_iterator(Node* node, const list<T>& list)
    : _node(node), _list(&list) {}

template <class T>
list<T>::const_iterator::const_iterator(const iterator& other)
    : _node(other._node), _list(other._list) {}

template <class T>
typename list<T>::const_iterator& list<T>::const_iterator::operator++() {
    _node = _node->next;
    return *this;
}

template <class T>
typename list<T>::const_iterator list<T>::const_iterator::operator++(int) {
    const_iterator tmp = *this;
    ++(*this);
    return tmp;
}

template <class T>
typename list<T>::const_iterator& list<T>::const_iterator::operator--() {
    if (!_node) {
        _node = _list->_tail;
        return *this;
    }

    _node = _node->prev;
    return *this;
}

template <class T>
typename list<T>::const_iterator list<T>::const_iterator::operator--(int) {
    const_iterator tmp = *this;
    --(*this);
    return tmp;
}

template <class T>
bool list<T>::const_iterator::operator==(
    const const_iterator& other) const noexcept {
    return _node == other._node;
}

template <class T>
bool list<T>::const_iterator::operator!=(
    const const_iterator& other) const noexcept {
    return !(*this == other);
}

template <class T>
const T& list<T>::const_iterator::operator*() const noexcept {
    return _node->data;
}

template <class T>
list<T>::list() : _head(nullptr), _tail(nullptr), _size(0) {}

template <class T>
list<T>::list(const list& other) {
    _copy(other);
}

template <class T>
list<T>::list(list&& other) {
    _move(std::move(other));
}

template <class T>
list<T>::list(std::initializer_list<T> init_list) : list() {
    for (auto&& element : init_list) {
        push_back(element);
    }
}

template <class T>
list<T>::~list() {
    _free();
}

template <class T>
list<T>& list<T>::operator=(const list& other) {
    if (this != &other) {
        _free();
        _copy(other);
    }
    return *this;
}

template <class T>
list<T>& list<T>::operator=(list&& other) {
    if (this != &other) {
        _free();
        _move(std::move(other));
    }
    return *this;
}

template <class T>
void list<T>::push_back(const T& value) {
    Node* node = new Node(value);

    if (empty()) {
        _head = _tail = node;
        ++_size;
        return;
    }

    _tail->next = node;
    node->prev = _tail;
    _tail = node;
    ++_size;
}

template <class T>
void list<T>::push_back(T&& value) {
    Node* node = new Node(std::move(value));

    if (empty()) {
        _head = _tail = node;
        ++_size;
        return;
    }

    _tail->next = node;
    node->prev = _tail;
    _tail = node;
    ++_size;
}

template <class T>
void list<T>::push_front(const T& value) {
    Node* node = new Node(value);

    if (empty()) {
        _head = _tail = node;
        ++_size;
        return;
    }

    _head->prev = node;
    node->next = _head;
    _head = node;
    ++_size;
}

template <class T>
void list<T>::push_front(T&& value) {
    Node* node = new Node(std::move(value));

    if (empty()) {
        _head = _tail = node;
        ++_size;
        return;
    }

    _head->prev = node;
    node->next = _head;
    _head = node;
    ++_size;
}

template <class T>
void list<T>::pop_back() {
    if (empty()) {
        return;
    }

    if (_head == _tail) {
        delete _head;
        _head = _tail = nullptr;
        --_size;
        return;
    }

    Node* toDelete = _tail;
    _tail = _tail->prev;
    _tail->next = nullptr;
    delete toDelete;

    --_size;
}

template <class T>
void list<T>::pop_front() {
    if (empty()) {
        return;
    }

    if (_head == _tail) {
        delete _head;
        _head = _tail = nullptr;
        --_size;
        return;
    }

    Node* toDelete = _head;
    _head = _head->next;
    delete toDelete;

    --_size;
}

template <class T>
typename list<T>::iterator list<T>::insert(const_iterator pos, const T& value) {
    if (pos == cbegin()) {
        push_front(value);
        return begin();
    }

    if (pos == cend()) {
        push_back(value);
        return --end();
    }

    Node* newNode = new Node(value);
    newNode->next = pos._node;
    newNode->prev = pos._node->prev;

    pos._node->prev->next = newNode;
    pos._node->prev = newNode;

    ++_size;
    return iterator(newNode, *this);
}

template <class T>
typename list<T>::iterator list<T>::insert(const_iterator pos, T&& value) {
    if (pos == cbegin()) {
        push_front(std::move(value));
        return begin();
    }

    if (pos == cend()) {
        push_back(std::move(value));
        return --end();
    }

    Node* newNode = new Node(std::move(value));
    newNode->next = pos._node;
    newNode->prev = pos._node->prev;

    pos._node->prev->next = newNode;
    pos._node->prev = newNode;

    ++_size;
    return iterator(newNode, *this);
}

template <class T>
typename list<T>::iterator list<T>::insert(const_iterator pos, size_t count,
                                           const T& value) {
    if (count == 0) {
        return iterator(pos._node, *this);
    }

    iterator first_inserted = iterator(nullptr, *this);

    for (size_t i = 0; i < count; ++i) {
        if (i == 0) {
            first_inserted = insert(pos, value);
        } else {
            insert(pos, value);
        }
    }

    return first_inserted;
}

template <class T>
typename list<T>::iterator list<T>::insert(const_iterator pos,
                                           std::initializer_list<T> init_list) {
    if (init_list.size() == 0) {
        return iterator(pos._node, *this);
    }

    iterator first_inserted = iterator(nullptr, *this);

    for (auto it = init_list.begin(); it != init_list.end(); ++it) {
        if (it == init_list.begin()) {
            first_inserted = insert(pos, *it);
        } else {
            insert(pos, *it);
        }
    }

    return first_inserted;
}

template <class T>
template <class Comparator>
void list<T>::sort(Comparator comparator) {
    _head = merge_sort(_head, comparator);

    list<T>::const_iterator iter = cbegin();
    while (iter._node->next) {
        ++iter;
    }

    _tail = iter._node;
}

template <class T>
template <class Comparator>
typename list<T>::Node* list<T>::merge(list<T>::Node* lhs, list<T>::Node* rhs, Comparator comp) {
    Node* head = nullptr;
    Node* tail = nullptr;

    while (lhs && rhs) {
        if (comp(lhs->data, rhs->data)) {
            append_back(head, tail, lhs);
            lhs = lhs->next;
        } else {
            append_back(head, tail, rhs);
            rhs = rhs->next;
        }
    }

    if (lhs) {
        tail->next = lhs;
    }

    if (rhs) {
        tail->next = rhs;
    }

    return head;
}

template <class T>
typename list<T>::Node* list<T>::get_middle(Node* list) {
    if (!list || !list->next) return list;

    Node* fast = list->next;
    Node* slow = list;

    while (fast && fast->next) {
        slow = slow->next;
        fast = fast->next->next;
    }
    return slow;
}

template <class T>
template <class Comparator>
typename list<T>::Node* list<T>::merge_sort(list<T>::Node* list,
                                            Comparator comp) {
    if (!list || !list->next) return list;

    Node* middle = get_middle(list);

    Node* right_part = middle->next;
    middle->next = nullptr;

    Node* left_sorted = merge_sort(list, comp);
    Node* right_sorted = merge_sort(right_part, comp);

    return merge(left_sorted, right_sorted, comp);
}

template <class T>
void list<T>::append_back(Node*& head, Node*& tail, Node* element) {
    if (head == nullptr) {
        head = tail = element;
    } else {
        tail->next = element;
        tail = tail->next;
    }
}

template <class T>
void list<T>::_free() {
    Node* curr = _head;
    Node* toDelete = _head;

    while (curr) {
        curr = curr->next;
        delete toDelete;
        toDelete = curr;
    }

    _head = _tail = nullptr;
    _size = 0;
}

template <class T>
void list<T>::_copy(const list& other) {
    Node* iter = other._head;
    while (iter) {
        push_back(iter->data);
        iter = iter->next;
    }
}

template <class T>
void list<T>::_move(list&& other) {
    _head = other._head;
    _tail = other._tail;
    _size = other._size;

    other._head = nullptr;
    other._tail = nullptr;
    other._size = 0;
}

template <class T>
typename list<T>::iterator list<T>::erase(const_iterator pos) {
    if (pos == cend()) {
        return end();
    }

    if (pos == cbegin()) {
        pop_front();
        return begin();
    }

    if (pos._node == _tail) {
        pop_back();
        return end();
    }

    Node* toDelete = pos._node;
    pos._node->prev->next = pos._node->next;
    pos._node->next->prev = pos._node->prev;

    iterator it(pos._node->next, *this);
    delete toDelete;
    --_size;
    return it;
}

template <class T>
typename list<T>::iterator list<T>::erase(const_iterator first,
                                          const_iterator last) {
    if (first == last) {
        return iterator(last._node, *this);
    }

    Node* start = first._node;
    Node* end = last._node;

    if (start == _head) {
        _head = end;
    } else {
        start->prev->next = end;
    }

    if (end) {
        end->prev = start->prev;
    } else {
        _tail = start->prev;
    }

    iterator it(end, *this);

    while (start != end) {
        Node* toDelete = start;
        start = start->next;
        delete toDelete;
        --_size;
    }

    return it;
}

template <class T>
T& list<T>::front() noexcept {
    return _head->data;
}

template <class T>
const T& list<T>::front() const noexcept {
    return _head->data;
}

template <class T>
T& list<T>::back() noexcept {
    return _tail->data;
}

template <class T>
const T& list<T>::back() const noexcept {
    return _tail->data;
}

template <class T>
typename list<T>::iterator list<T>::begin() const noexcept {
    return iterator(_head, *this);
}

template <class T>
typename list<T>::iterator list<T>::end() const noexcept {
    return iterator(nullptr, *this);
}

template <class T>
typename list<T>::const_iterator list<T>::cbegin() const noexcept {
    return const_iterator(_head, *this);
}

template <class T>
typename list<T>::const_iterator list<T>::cend() const noexcept {
    return const_iterator(nullptr, *this);
}

template <class T>
bool list<T>::empty() const noexcept {
    return _size == 0;
}

template <class T>
size_t list<T>::size() const noexcept {
    return _size;
}

template <class T>
void list<T>::clear() noexcept {
    _free();
    _head = nullptr;
    _tail = nullptr;
    _size = 0;
}

template <class T>
list<T>::Node::Node(const T& value) : data(value) {}

template <class T>
list<T>::Node::Node(T&& value) : data(std::move(value)) {}

template <class T>
template <class... Args>
typename list<T>::iterator list<T>::emplace(const_iterator pos,
                                            Args&&... args) {
    Node* newNode = new Node(std::forward<Args>(args)...);

    if (pos == cbegin()) {
        push_front(std::move(newNode->data));
        delete newNode;
        return begin();
    }

    if (pos == cend()) {
        push_back(std::move(newNode->data));
        delete newNode;
        return --end();
    }

    newNode->next = pos._node;
    newNode->prev = pos._node->prev;

    pos._node->prev->next = newNode;
    pos._node->prev = newNode;

    ++_size;
    return iterator(newNode, *this);
}

template <class T>
template <class... Args>
typename list<T>::iterator list<T>::emplace_back(Args&&... args) {
    Node* newNode = new Node(std::forward<Args>(args)...);

    if (empty()) {
        _head = _tail = newNode;
        ++_size;
        return iterator(_tail, *this);
    }

    _tail->next = newNode;
    newNode->prev = _tail;
    _tail = newNode;

    ++_size;
    return iterator(_tail, *this);
}

template <class T>
template <class... Args>
typename list<T>::iterator list<T>::emplace_front(Args&&... args) {
    Node* newNode = new Node(std::forward<Args>(args)...);

    if (empty()) {
        _head = _tail = newNode;
        ++_size;
        return iterator(_head, *this);
    }

    _head->prev = newNode;
    newNode->next = _head;
    _head = newNode;

    ++_size;
    return iterator(_head, *this);
}
