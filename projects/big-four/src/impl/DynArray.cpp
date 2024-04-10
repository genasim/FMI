#include "DynArray.h"

template <typename T>
DynArray<T>::DynArray() : DynArray(nullptr, 0) {}

template <typename T>
DynArray<T>::DynArray(const T arr[], const size_t size) {
    this->arr = new T[size];
    setSize(size);
    for (size_t i = 0; i < size; i++) {
        this->arr[i] = arr[i];
    }
}

template <typename T>
DynArray<T>& DynArray<T>::operator=(const DynArray<T>& other) {
    if (other != *this) {
        free();
        copyFrom(other);
    }
    return *this;
}

template <typename T>
DynArray<T>::DynArray(const DynArray<T>& other) {
    copyFrom(other);
}

template <typename T>
const size_t DynArray<T>::getSize() const {
    return size;
}

template <typename T>
const T& DynArray<T>::operator[](const size_t index) const {
    if (index >= size) throw std::runtime_error("Index out of bounds!");
    return arr[index];
}

template <typename T>
T& DynArray<T>::operator[](const size_t index) {
    if (index >= size) throw std::runtime_error("Index out of bounds!");
    return arr[index];
}

template <typename T>
void DynArray<T>::setSize(const size_t to) {
    size = to;
    if (size > capacity || size < capacity >> 1) {
        size_t newCapacity = 0;
        while (size >= capacity) {
            newCapacity <<= 1;
        }
        setCapacity(newCapacity);
    }
}

template <typename T>
void DynArray<T>::setCapacity(const size_t to) {
    capacity = to;
    reallocate();
}

template <typename T>
void DynArray<T>::copyFrom(const DynArray<T>& other) {
    capacity = 0;
    setSize(other.size);
}

template <typename T>
void DynArray<T>::reallocate() {
    T* newArr = new T[capacity];
    for (size_t i = 0; i < size; i++) {
        newArr[i] = arr[i];
    }
    free();
    arr = newArr;
}

template <typename T>
void DynArray<T>::free() {
    delete[] arr;
}

template <typename T>
std::ofstream& operator<<(std::ofstream& out, const DynArray<T>& array) {
    out << "[ ";
    for (size_t i = 0; i < array.getSize() - 1; i++) {
        out << array[i] << ", ";
    }
    out << array[array.getSize() - 1] << " ]f";

    return out;
}