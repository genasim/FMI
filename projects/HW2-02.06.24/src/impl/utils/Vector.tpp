#pragma once

#include <exception>
#include <iostream>

#include "utils/Vector.h"

template <class T>
inline Vector<T>::Vector() : Vector(nullptr, 0) {}

template <class T>
Vector<T>::Vector(const T *arr, size_t size) : _size(size), _capacity(size) {
    if (arr == nullptr) {
        _data = nullptr;
        return;
    };

    _data = new T[_size];
    for (size_t i = 0; i < _size; i++) {
        _data[i] = arr[i];
    }
}

template <class T>
Vector<T>::Vector(const Vector<T> &other) {
    _copy(other);
}

template <class T>
Vector<T>::Vector(std::initializer_list<T> init) : _size(init.size()), _capacity(init.size()) {
    _data = new T[_size];
    size_t i = 0;
    for (const T& value : init) {
        _data[i++] = value;
    }
}

template <class T>
Vector<T>::Vector(size_t size, const T &value) : _size(size), _capacity(size) {
    if (size == 0) {
        _data = nullptr;
        return;
    }

    _data = new T[_size];
    for (size_t i = 0; i < _size; i++) {
        _data[i] = value;
    }
}

template <class T>
Vector<T> &Vector<T>::operator=(const Vector<T> &other) {
    if (this != &other) {
        _free();
        _copy(other);
    }
    return *this;
}

template <class T>
Vector<T> &Vector<T>::operator=(Vector<T> &&other) {
    if (this != &other) {
        _move(std::move(other));
    }
    return *this;
}

template <class T>
Vector<T>::Vector(Vector<T> &&other) {
    _move(std::move(other));
}

template <class T>
Vector<T>::~Vector() {
    _free();
}

template <class T>
void Vector<T>::_copy(const Vector<T> &other) {
    this->_size = other._size;
    this->_capacity = other._capacity;
    this->_data = new T[_capacity];
    for (size_t i = 0; i < other._size; i++) {
        this->_data[i] = other._data[i];
    }
}

template <class T>
void Vector<T>::_move(Vector<T> &&other) {
    this->_data = other._data;
    this->_size = other._size;
    this->_capacity = other._capacity;

    other._data = nullptr;
    other._size = other._capacity = 0;
}

template <class T>
void Vector<T>::_free() {
    delete[] _data;
}

template <class T>
void Vector<T>::reserve(size_t to) {
    if (to < _capacity) return;

    T *newData = new T[to];
    for (size_t i = 0; i < _capacity; i++) {
        newData[i] = std::move(_data[i]);
    }
    _free();
    _data = newData;
    _capacity = to;
}

template <class T>
void Vector<T>::resize(size_t to, const T &value) {
    if (to == _size) {
        return;
    }

    if (to < _size) {
        for (size_t i = to; i < _size; i++) {
            _data[i].~T();
        }
        _size = to;
        return;
    }

    if (to > _capacity) {
        size_t new_cap = _nextPowerOfTwo(to);
        reserve(new_cap);
    }

    for (size_t i = _size; i < to; i++) {
        _data[i] = value;
    }
    _size = to;
}

template <class T>
void Vector<T>::shrink_to_fit() noexcept {
    T *data = new T[_size];
    for (size_t i = 0; i < _size; i++) {
        data[i] = std::move(_data[i]);
    }
    _free();
    _data = data;
    _capacity = _size;
}

template <class T>
void Vector<T>::push_back(const T &element) {
    resize(_size + 1);
    _data[_size - 1] = element;
}

template <class T>
void Vector<T>::push_back(T &&element) {
    resize(_size + 1);
    _data[_size - 1] = std::move(element);
}

template <class T>
size_t Vector<T>::_nextPowerOfTwo(size_t curr) const noexcept {
    size_t result = 1;
    while (result < curr) {
        result <<= 1;
    }
    return result;
}

template <class T>
T Vector<T>::pop_back() noexcept {
    T result = _data[_size - 1];
    resize(_size - 1);
    return result;
}

template <class T>
std::ostream &operator<<(std::ostream &out, const Vector<T> &vec) noexcept {
    out << "[ ";
    for (auto &&item : vec) {
        out << item << ' ';
    }

    out << ']';
    return out;
}

template <class T>
inline bool Vector<T>::empty() const noexcept {
    return _size == 0;
}

template <class T>
inline size_t Vector<T>::size() const noexcept {
    return _size;
}

template <class T>
inline size_t Vector<T>::capacity() const noexcept {
    return _capacity;
}

template <class T>
inline const T &Vector<T>::operator[](size_t idx) const {
    return this->at(idx);
}

template <class T>
inline T &Vector<T>::operator[](size_t idx) {
    return this->at(idx);
}

template <class T>
inline const T &Vector<T>::at(size_t idx) const {
    if (idx >= _size) {
        throw std::out_of_range("Vector<T>::at(size_t) -> Index out of bounds");
    }
    return _data[idx];
}

template <class T>
inline T &Vector<T>::at(size_t idx) {
    if (idx >= _size) {
        throw std::out_of_range("Vector<T>::at(size_t) -> Index out of bounds");
    }
    return _data[idx];
}

template <class T>
inline const T &Vector<T>::front() const {
    if (_size == 0)
        throw std::logic_error("Vector<T>::front() const -> vector is empty");
    return _data[0];
}

template <class T>
inline T &Vector<T>::front() {
    if (_size == 0)
        throw std::logic_error("Vector<T>::front() -> vector is empty");
    return _data[0];
}

template <class T>
inline const T &Vector<T>::back() const {
    if (_size == 0)
        throw std::logic_error("Vector<T>::back() const -> vector is empty");
    return _data[_size - 1];
}

template <class T>
inline T &Vector<T>::back() {
    if (_size == 0)
        throw std::logic_error("Vector<T>::back() -> vector is empty");
    return _data[_size - 1];
}

template <class T>
bool Vector<T>::operator==(const Vector<T> &other) const noexcept {
    if (this == &other) return true;

    if (this->_size != other._size) return false;

    for (size_t i = 0; i < _size; i++) {
        if (this->_data[i] != other._data[i]) return false;
    }
    return true;
}

template <class T>
bool Vector<T>::operator!=(const Vector<T> &other) const noexcept {
    return !(*this == other);
}

template <class T>
bool Vector<T>::operator<(const Vector<T> &other) const noexcept {
    if (this == &other) return false;
    return _lexicCompare(other) == -1;
}

template <class T>
bool Vector<T>::operator<=(const Vector<T> &other) const noexcept {
    if (this == &other) return true;
    return *this < other || *this == other;
}

template <class T>
bool Vector<T>::operator>(const Vector<T> &other) const noexcept {
    if (this == &other) return false;
    return _lexicCompare(other) == 1;
}

template <class T>
bool Vector<T>::operator>=(const Vector<T> &other) const noexcept {
    if (this == &other) return true;
    return *this > other || *this == other;
}

template <class T>
int Vector<T>::_lexicCompare(const Vector<T> &other) const noexcept {
    size_t min_size = (_size < other._size) ? _size : other._size;
    for (size_t i = 0; i < min_size; ++i) {
        if (_data[i] < other._data[i]) {
            return -1;
        }
        if (_data[i] > other._data[i]) {
            return 1;
        }
    }

    if (_size < other._size) {
        return -1;
    } else if (_size > other._size) {
        return 1;
    }

    return 0;
}

template <class T>
void Vector<T>::insert(const T &element, size_t index) {
    if (index > _size) {
        throw std::out_of_range(
            "Vector<T>::insert -> Insert index cannot be larger than size");
    }

    resize(_size + 1);
    for (size_t i = _size - 1; i > index; i--) {
        std::swap(_data[i], _data[i - 1]);
    }
    _data[index] = element;
}

template <class T>
void Vector<T>::insert(T &&element, size_t index) {
    if (index > _size) {
        throw std::out_of_range(
            "Vector<T>::insert -> Insert index cannot be larger than size");
    }

    resize(_size + 1);
    for (size_t i = _size - 1; i > index; i--) {
        std::swap(_data[i], _data[i - 1]);
    }
    _data[index] = std::move(element);
}

template <class T>
void Vector<T>::insertRange(const Vector<T> &other, size_t index) {
    insertRange(other._data, other._size, index);
}

template <class T>
void Vector<T>::insertRange(const T *arr, size_t size, size_t index) {
    if (index > _size) {
        throw std::out_of_range(
            "Vector<T>::insert -> Insert index cannot be larger than size");
    }

    resize(_size + size);
    for (size_t i = _size - 1; i >= size; i--) {
        std::swap(_data[i + index], _data[i - size + index]);
    }
    for (size_t i = 0; i < size; i++) {
        _data[i + index] = arr[i];
    }
}

template <class T>
void Vector<T>::clear() noexcept {
    resize(0);
}

template <class T>
void Vector<T>::eraseAll(const T &element) noexcept {
    size_t deleted = _data[0] == element;

    for (size_t i = 1; i < _size; i++) {
        if (_data[i] == element) {
            ++deleted;
            continue;
        }
        std::swap(_data[i], _data[i - deleted]);
    }
    resize(_size - deleted);
}

template <class T>
void Vector<T>::erase(const T &element) noexcept {
    bool found = _data[0] == element;
    for (size_t i = 1; i < _size; i++) {
        if (!found && _data[i] == element) {
            found = true;
            continue;
        }
        std::swap(_data[i], _data[i - found]);
    }
    resize(_size - found);
}

template <class T>
Vector<T>::iterator::iterator(T *arr, size_t idx) : _data(arr), _index(idx) {}

template <class T>
inline bool Vector<T>::iterator::operator==(
    const Vector<T>::iterator &other) const {
    return _data == other._data && _index == other._index;
}

template <class T>
inline bool Vector<T>::iterator::operator!=(
    const Vector<T>::iterator &other) const {
    return !(*this == other);
}

template <class T>
inline T &Vector<T>::iterator::operator*() const {
    return _data[_index];
}

template <class T>
inline typename Vector<T>::iterator &Vector<T>::iterator::operator++() {
    ++_index;
    return *this;
}

template <class T>
inline typename Vector<T>::iterator Vector<T>::begin() const {
    return Vector::iterator(_data, 0);
}

template <class T>
inline typename Vector<T>::iterator Vector<T>::end() const {
    return Vector::iterator(_data, _size);
}

template <class T>
bool Vector<T>::contains(const T &element) const {
    if (_data == nullptr) return false;

    for (const T &item : *this) {
        if (item == element) return true;
    }
    return false;
}