#pragma once

#include <forward_list>

template <class T>
class stack {
   public:
    stack();
    stack(const stack<T>& other);
    stack(stack<T>&& other) noexcept;

    stack& operator=(const stack<T>& other);
    stack& operator=(stack<T>&& other) noexcept;

    ~stack() = default;

    const T& top() const;
    void push(const T& element);
    void pop();

    template <class... Args>
    void emplace(Args&&... args);

    size_t size() const noexcept;
    bool empty() const noexcept;

   private:
    std::forward_list<T> _data;
    size_t _size;

    void _copy(const stack<T>& other);
    void _move(stack<T>&& other) noexcept;
    void _free();
};

template <class T>
inline stack<T>::stack() : _data(), _size(0) {}

template <class T>
inline stack<T>::stack(const stack<T>& other) {
    _copy(other);
}

template <class T>
inline stack<T>::stack(stack<T>&& other) noexcept {
    _move(std::move(other));
}

template <class T>
inline stack<T>& stack<T>::operator=(const stack<T>& other) {
    if (this != &other) {
        _free();
        _copy(other);
    }
    return *this;
}

template <class T>
inline stack<T>& stack<T>::operator=(stack<T>&& other) noexcept {
    if (this != &other) {
        _free();
        _move(std::move(other));
    }
    return *this;
}

template <class T>
inline void stack<T>::_copy(const stack<T>& other) {
    _size = other._size;
    _data = other._data;
}

template <class T>
inline void stack<T>::_move(stack<T>&& other) noexcept {
    _data = std::move(other._data);
    _size = other._size;

    other._free();
}

template <class T>
inline void stack<T>::_free() {
    _data.clear();
    _size = 0;
}

template <class T>
inline const T& stack<T>::top() const {
    return *_data.begin();
}

template <class T>
inline void stack<T>::push(const T& element) {
    _data.push_front(element);
    ++_size;
}

template <class T>
inline void stack<T>::pop() {
    if (empty()) return;
    _data.pop_front();
    --_size;
}

template <class T>
template <class... Args>
inline void stack<T>::emplace(Args&&... args) {
    _data.emplace_front(std::forward<Args>(args)...);
    ++_size;
}

template <class T>
inline bool stack<T>::empty() const noexcept {
    return _size == 0;
}

template <class T>
inline size_t stack<T>::size() const noexcept {
    return _size;
}