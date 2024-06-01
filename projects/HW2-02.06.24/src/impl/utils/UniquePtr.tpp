#pragma once

#include "utils/UniquePtr.h"

template <class T>
UniquePtr<T>::UniquePtr(T* rawPtr) : ptr(rawPtr) {}

template <class T>
UniquePtr<T>::UniquePtr(UniquePtr<T>&& other) noexcept : ptr(other.ptr) {
    other.ptr = nullptr;
}

template <class T>
UniquePtr<T>& UniquePtr<T>::operator=(UniquePtr<T>&& other) noexcept {
    if (this != &other) {
        _free();
        ptr = other.ptr;
        other.ptr = nullptr;
    }
    return *this;
}

template <class T>
UniquePtr<T>& UniquePtr<T>::operator=(T*&& other) noexcept {
    if (ptr != other) {
        _free();
        ptr = other;
        other = nullptr;
    }
    return *this;
}

template <class T>
UniquePtr<T>::~UniquePtr() {
    _free();
}

template <class T>
T& UniquePtr<T>::operator*() const {
    return *ptr;
}

template <class T>
T* UniquePtr<T>::operator->() const noexcept {
    return ptr;
}

template <class T>
bool UniquePtr<T>::isValid() const noexcept {
    return ptr != nullptr;
}

template <class T>
T* UniquePtr<T>::release() noexcept {
    T* temp = ptr;
    ptr = nullptr;
    return temp;
}

template <class T>
void UniquePtr<T>::reset(T* rawPtr) noexcept {
    if (ptr != rawPtr) {
        _free();
        ptr = rawPtr;
    }
}

template <class T>
void UniquePtr<T>::_free() {
    delete ptr;
    ptr = nullptr;
}