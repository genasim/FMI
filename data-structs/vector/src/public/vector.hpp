#pragma once

#include <initializer_list>
#include <memory>

template <class T, class Allocator = std::allocator<T>>
class vector {
   public:
    struct iterator;
    struct const_iterator;
    struct reverse_iterator;

    vector() = default;
    vector(const vector& other);
    vector(vector&& other) noexcept;
    explicit vector(size_t capacity);
    vector(const std::initializer_list<T>& init_list);

    vector& operator=(const vector& other);
    vector& operator=(vector&& other) noexcept;

    T& operator[](size_t index);
    const T& operator[](size_t index) const;
    T& at(size_t index);
    const T& at(size_t index) const;

    void push_back(const T& value);
    void push_back(T&& value);

    void pop_back();

    size_t size() const noexcept;
    size_t capacity() const noexcept;
    bool empty() const noexcept;

    void reserve(size_t new_capacity);
    void resize(size_t new_size);
    void shrink_to_fit();

    void clear() noexcept;

    ~vector();

    iterator begin() noexcept;
    iterator end() noexcept;

    struct iterator {
        iterator(T* ptr) : ptr(ptr) {}
        iterator& operator++() {
            ++ptr;
            return *this;
        }
        iterator operator++(int) {
            iterator copy = *this;
            ++ptr;
            return copy;
        }
        iterator& operator--() {
            --ptr;
            return *this;
        }
        iterator operator--(int) {
            iterator copy = *this;
            --ptr;
            return copy;
        }
        T& operator*() { return *ptr; }
        T* operator->() { return ptr; }
        bool operator==(const iterator& other) const { return ptr == other.ptr; }
        bool operator!=(const iterator& other) const { return ptr != other.ptr; }

       private:
        T* ptr;
    };

   private:
    std::allocator<T> allocator;

    T* _data;
    size_t _size;
    size_t _capacity;

    void _copy(const vector<T>& other);
    void _move(vector<T>&& other) noexcept;
    void _free();
};

template <class T, class Allocator>
inline vector<T, Allocator>::vector(size_t capacity)
    : _data(allocator.allocate(capacity)), _size(0), _capacity(capacity) {}

template <class T, class Allocator>
inline vector<T, Allocator>::vector(const std::initializer_list<T>& init_list)
    : _data(allocator.allocate(init_list.size())),
      _size(init_list.size()),
      _capacity(init_list.size()) {
    std::uninitialized_copy(init_list.begin(), init_list.end(), _data);
}

template <class T, class Allocator>
inline T& vector<T, Allocator>::operator[](size_t index) {
    return _data[index];
}

template <class T, class Allocator>
inline const T& vector<T, Allocator>::operator[](size_t index) const {
    return _data[index];
}

template <class T, class Allocator>
inline T& vector<T, Allocator>::at(size_t index) {
    if (index >= size()) throw std::out_of_range("Index out of range");
    return _data[index];
}

template <class T, class Allocator>
inline const T& vector<T, Allocator>::at(size_t index) const {
    if (index >= size()) throw std::out_of_range("Index out of range");
    return _data[index];
}

template <class T, class Allocator>
inline size_t vector<T, Allocator>::size() const noexcept {
    return _size;
}

template <class T, class Allocator>
inline size_t vector<T, Allocator>::capacity() const noexcept {
    return _capacity;
}

template <class T, class Allocator>
inline bool vector<T, Allocator>::empty() const noexcept {
    return _size == 0;
}

template <class T, class Allocator>
inline void vector<T, Allocator>::_free() {
    for (size_t i = 0; i < size(); ++i) {
        allocator.destroy(_data + i);
    }
    allocator.deallocate(_data, capacity());
}

template <class T, class Allocator>
inline void vector<T, Allocator>::clear() noexcept {
    _free();
    _size = 0;
    _capacity = 0;
}

template <class T, class Allocator>
inline vector<T, Allocator>::~vector() {
    _free();
}

template <class T, class Allocator>
inline vector<T, Allocator>::vector(const vector<T, Allocator>& other) {
    _copy(other);
}

template <class T, class Allocator>
inline vector<T, Allocator>::vector(vector<T, Allocator>&& other) noexcept {
    _move(std::move(other));
}

template <class T, class Allocator>
inline vector<T, Allocator>& vector<T, Allocator>::operator=(const vector<T, Allocator>& other) {
    if (this != &other) {
        _free();
        _copy(other);
    }
    return *this;
}

template <class T, class Allocator>
inline vector<T, Allocator>& vector<T, Allocator>::operator=(
    vector<T, Allocator>&& other) noexcept {
    if (this != &other) {
        _free();
        _move(std::move(other));
    }
    return *this;
}

template <class T, class Allocator>
inline void vector<T, Allocator>::_copy(const vector<T>& other) {
    _data = allocator.allocate(other.capacity());
    _size = other.size();
    _capacity = other.capacity();

    for (size_t i = 0; i < other.size(); ++i) {
        allocator.construct(_data + i, other[i]);
    }
}

template <class T, class Allocator>
inline void vector<T, Allocator>::_move(vector<T>&& other) noexcept {
    _data = other._data;
    _size = other._size;
    _capacity = other._capacity;

    other._data = nullptr;
    other._size = 0;
    other._capacity = 0;
}

template <class T, class Allocator>
inline typename vector<T, Allocator>::iterator vector<T, Allocator>::begin() noexcept {
    return iterator(_data);
}

template <class T, class Allocator>
inline typename vector<T, Allocator>::iterator vector<T, Allocator>::end() noexcept {
    return iterator(nullptr);
}
