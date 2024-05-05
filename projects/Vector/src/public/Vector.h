#pragma once

#include <fstream>

template <class T>
class Vector {
   public:
    Vector();
    Vector(const T*, size_t);
    explicit Vector(size_t, const T& = T());
    Vector(const Vector<T>&);
    Vector<T>& operator=(const Vector<T>&);
    Vector<T>(Vector<T>&&);
    Vector<T>& operator=(Vector<T>&&);

    void push_back(const T&);
    void push_back(T&&);
    T pop_back() noexcept;

    void insert(const T&, size_t index = 0);
    void insert(T&&, size_t index = 0);
    void insertRange(const Vector<T>&, size_t index = 0);
    void insertRange(const T*, size_t size, size_t index = 0);
    void erase(const T&) noexcept;
    void eraseAll(const T&) noexcept;
    void clear() noexcept;

    const T& at(size_t) const;
    T& at(size_t);
    const T& front() const;
    T& front();
    const T& back() const;
    T& back();
    const T& operator[](size_t) const;
    T& operator[](size_t);

    bool empty() const noexcept;
    size_t size() const noexcept;
    size_t capacity() const noexcept;

    void reserve(size_t);
    void shrink_to_fit() noexcept;
    void resize(size_t, const T& = T());

    template <class F>
    friend std::ostream& operator<<(std::ostream&, const Vector<F>&) noexcept;

    bool operator==(const Vector<T>&) const noexcept;
    bool operator!=(const Vector<T>&) const noexcept;
    bool operator>(const Vector<T>&) const noexcept;
    bool operator>=(const Vector<T>&) const noexcept;
    bool operator<(const Vector<T>&) const noexcept;
    bool operator<=(const Vector<T>&) const noexcept;

    ~Vector<T>();

   private:
    void _copy(const Vector<T>&);
    void _move(Vector<T>&&);
    void _free();

    size_t _nextPowerOfTwo(size_t) const noexcept;
    int _lexicCompare(const Vector<T>&) const noexcept;

    T* _data;
    size_t _size;
    size_t _capacity;

   public:
    class iterator {
       public:
       iterator(T*, size_t);
        bool operator==(const Vector<T>::iterator&) const;
        bool operator!=(const Vector<T>::iterator&) const;
        Vector<T>::iterator& operator++();
        T& operator*() const;

       private:
        T* _data;
        size_t _index;
    };

    Vector::iterator begin() const;
    Vector::iterator end() const;
    bool contains(const T&);
};

#include <../impl/Vector.tpp>
