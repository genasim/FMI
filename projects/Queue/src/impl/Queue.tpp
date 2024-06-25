#pragma once

#include <iostream>

#include "Queue.h"

template <class T>
Queue<T>::Queue() : _data() {}

template <class T>
Queue<T>::Queue(const Vector<T>& vector) : _data(vector) {}

template <class T>
Queue<T>::Queue(Vector<T>&& vector) : _data(std::move(vector)) {}

template <class T>
Queue<T>::Queue(T* arr, size_t size) : _data(arr, size) {}

template <class T>
void Queue<T>::push(const T& elem) {
    _data.push_back(elem);
}

template <class T>
void Queue<T>::push(T&& elem) {
    _data.push_back(std::move(elem));
}

template <class T>
void Queue<T>::pop() {
    _data.erase(_data[0]);
}

template <class T>
size_t Queue<T>::size() const noexcept {
    return _data.size();
}

template <class T>
bool Queue<T>::empty() const noexcept {
    return _data.empty();
}

template <class T>
const T& Queue<T>::front() const noexcept {
    return _data.end();
}

template <class T>
T& Queue<T>::front() noexcept {
    return _data.end();
}

template <class T>
const T& Queue<T>::back() const noexcept {
    return _data.back();
}

template <class T>
T& Queue<T>::back() noexcept {
    return _data.back();
}

template <class T>
typename Vector<T>::iterator Queue<T>::begin() const noexcept {
    return _data.begin();
}

template <class T>
typename Vector<T>::iterator Queue<T>::end() const noexcept {
    return Vector<T>::iterator();
}

template <class E>
std::ostream& operator<<(std::ostream& out, const Queue<E>& queue) noexcept {
    out << queue._data;
    return out;
}