#pragma once

#include <fstream>

#include "Vector.h"

template <class T>
class Queue {
   public:
    Queue();
    Queue(const Vector<T>& vector);
    Queue(Vector<T>&& vector);
    Queue(T* arr, size_t size);

    void push(const T& elem);
    void push(T&& elem);
    void pop();

    size_t size() const noexcept;
    bool empty() const noexcept;

    const T& front() const noexcept;
    T& front() noexcept;
    const T& back() const noexcept;
    T& back() noexcept;

    typename Vector<T>::iterator begin() const noexcept;
    typename Vector<T>::iterator end() const noexcept;

    template <class F>
    friend std::ostream& operator<<(std::ostream& out,
                                    const Queue<F>& queue) noexcept;

   private:
    Vector<T> _data;
};

#include "../impl/Queue.tpp"
