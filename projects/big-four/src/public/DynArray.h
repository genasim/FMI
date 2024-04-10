#include <iostream>
#pragma once

template <typename T>
class DynArray {
   public:
    DynArray();
    DynArray(const DynArray<T>&);
    DynArray(const T[], const size_t);
    DynArray<T>& operator=(const DynArray<T>&);

    const size_t getSize() const;

    const T& operator[](const size_t) const;
    T& operator[](const size_t);

    bool operator==(const DynArray<T>& other) const {
        if (this->size != other.size) return false;

        for (size_t i = 0; i < size; i++)
            if (this->arr[i] != other.arr[i]) return false;

        return true;
    }
    bool operator!=(const DynArray<T>& other) const {
        return !(*this == other);
    }
    bool isEmpty() const { return size == 0; }

    // void push(const T& elem);
    // void pushAt(const size_t index, const T& elem);
    // void remove(const T& elem);
    // void removeAt(const size_t index);
    // bool contains(const T& elem) const;


    ~DynArray() { free(); }

   private:
    T* arr;
    size_t size;
    size_t capacity;

    void setSize(const size_t);
    void setCapacity(const size_t);

    void copyFrom(const DynArray<T>&);
    void reallocate();
    void free();
};

template <typename T>
std::ofstream& operator<<(std::ofstream&, const DynArray<T>&);

#include <impl/DynArray.cpp>