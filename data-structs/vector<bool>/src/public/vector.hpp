#pragma once

#include <fstream>
#include <memory>

template <typename T>
class vector {};

template <>
class vector<bool> {
   public:
    vector() : _size(0), _capacity(1) {
        _data = allocator.allocate(_capacity);
        for (size_t i = 0; i < _capacity; i++)
            allocator.construct(_data + i, 0);
    }

    vector(const vector<bool>& other) { _copy(other); }

    vector(vector<bool>&& other) noexcept { _move(std::move(other)); }

    vector<bool>& operator=(const vector<bool>& other) {
        if (this != &other) {
            _free();
            _copy(other);
        }
        return *this;
    }

    vector<bool>& operator=(vector<bool>&& other) noexcept {
        if (this != &other) {
            _free();
            _move(std::move(other));
        }
        return *this;
    }

    ~vector() { _free(); }

    void push_back(bool value) {
        if (_size == _capacity * BYTE_SIZE) {
            _resize(_size * 2);
        }

        std::size_t bitIndex = _size % BYTE_SIZE;
        std::size_t byteIndex = _size / BYTE_SIZE;

        _data[byteIndex] |= value << bitIndex;
        _size++;
    }

    void pop_back() {
        if (_size == 0) return;

        std::size_t bitIndex = _size % BYTE_SIZE;
        std::size_t byteIndex = _size / BYTE_SIZE;

        _data[byteIndex] &= ~(1 << bitIndex);
        _size--;
    }

   private:
    class BitReference {
        uint8_t& _byte;
        size_t _bitIndex;

       public:
        BitReference(uint8_t& byte, size_t index)
            : _byte(byte), _bitIndex(index) {}

        operator bool() const { return (_byte >> _bitIndex) & 1; }

        BitReference& operator=(bool value) {
            if (value)
                _byte |= (1 << _bitIndex);
            else
                _byte &= ~(1 << _bitIndex);

            return *this;
        }

        BitReference& operator=(const BitReference& other) {
            return *this = static_cast<bool>(other);
        }
    };

   public:
    BitReference operator[](size_t index) {
        if (index > _size) throw std::out_of_range("Index out of range: ");
        return BitReference(_data[index / BYTE_SIZE], index % BYTE_SIZE);
    }

    bool operator[](size_t index) const {
        if (index > _size) throw std::out_of_range("Index out of range");
        return (_data[index / BYTE_SIZE] >> (index % BYTE_SIZE)) & 1;
    }

    size_t size() const noexcept { return _size; }

    class iterator {
       public:
        iterator(uint8_t* data, size_t index) : _data(data), _index(index) {}

        bool operator!=(const iterator& other) const {
            return !(*this == other);
        }
        bool operator==(const iterator& other) const {
            return _index == other._index;
        }

        iterator& operator++() {
            _index++;
            return *this;
        }
        iterator& operator--() {
            _index--;
            return *this;
        }

        BitReference operator*() {
            return BitReference(_data[_index / BYTE_SIZE], _index % BYTE_SIZE);
        }

       private:
        friend class vector<bool>;
        uint8_t* _data;
        size_t _index;
    };

    class const_iterator {
       public:
        const_iterator(uint8_t* data, size_t index)
            : _data(data), _index(index) {}

        bool operator!=(const const_iterator& other) const {
            return !(*this == other);
        }

        bool operator==(const const_iterator& other) const {
            return _index == other._index;
        }

        const_iterator& operator++() {
            _index++;
            return *this;
        }

        const_iterator& operator--() {
            _index--;
            return *this;
        }

        bool operator*() {
            return (_data[_index / BYTE_SIZE] >> (_index % BYTE_SIZE)) & 1;
        }

       private:
        uint8_t* _data;
        size_t _index;
    };

    class reverse_iterator {
       public:
        reverse_iterator(uint8_t* data, size_t index)
            : _data(data), _index(index) {}

        bool operator!=(const reverse_iterator& other) const {
            return !(*this == other);
        }
        bool operator==(const reverse_iterator& other) const {
            return _index == other._index;
        }

        reverse_iterator& operator++() {
            _index--;
            return *this;
        }
        reverse_iterator& operator--() {
            _index++;
            return *this;
        }

        BitReference operator*() {
            return BitReference(_data[_index / BYTE_SIZE], _index % BYTE_SIZE);
        }

       private:
        uint8_t* _data;
        size_t _index;
    };

    class reverse_const_iterator {
       public:
        reverse_const_iterator(uint8_t* data, size_t index)
            : _data(data), _index(index) {}

        bool operator!=(const reverse_const_iterator& other) const {
            return !(*this == other);
        }

        bool operator==(const reverse_const_iterator& other) const {
            return _index == other._index;
        }

        reverse_const_iterator& operator++() {
            _index--;
            return *this;
        }

        reverse_const_iterator& operator--() {
            _index++;
            return *this;
        }

        bool operator*() {
            return (_data[_index / BYTE_SIZE] >> (_index % BYTE_SIZE)) & 1;
        }

       private:
        uint8_t* _data;
        size_t _index;
    };

    iterator begin() const noexcept { return iterator(_data, 0); }
    iterator end() const noexcept { return iterator(_data, _size + 1); }

    const_iterator cbegin() const noexcept { return const_iterator(_data, 0); }
    const_iterator cend() const noexcept {
        return const_iterator(_data, _size + 1);
    }

    reverse_iterator rbegin() const noexcept {
        return reverse_iterator(_data, _size);
    }
    reverse_iterator rend() const noexcept {
        return reverse_iterator(_data, -1);
    }

    reverse_const_iterator crbegin() const noexcept {
        return reverse_const_iterator(_data, _size);
    }
    reverse_const_iterator crend() const noexcept {
        return reverse_const_iterator(_data, -1);
    }

    void insert(iterator iter, bool value) {
        size_t index = iter._index;
        if (index > _size) {
            throw std::out_of_range("Iterator out of range");
        }

        if (_size == _capacity * BYTE_SIZE) {
            _resize(_size * 2);
        }

        for (size_t i = _size; i > index; --i) {
            (*this)[i] = (*this)[i - 1];
        }

        (*this)[index] = value;
        _size++;
    }

    void remove(iterator iter) {
        size_t index = iter._index;
        if (index >= _size) {
            throw std::out_of_range("Iterator out of range");
        }

        for (size_t i = index; i < _size - 1; ++i) {
            (*this)[i] = (*this)[i + 1];
        }

        _size--;
        (*this)[_size] = 0;
    }

   private:
    static constexpr int BYTE_SIZE = 8;

    friend class iterator;

    std::allocator<uint8_t> allocator;

    uint8_t* _data = nullptr;
    std::size_t _size = 0;
    std::size_t _capacity = 0;

    void _resize(size_t to) {
        if (to <= _capacity * BYTE_SIZE) return;

        std::size_t newCapacity = ((to + BYTE_SIZE - 1) / BYTE_SIZE);
        uint8_t* newData = allocator.allocate(newCapacity);

        for (size_t i = 0; i < _capacity; i++)
            allocator.construct(newData + i, _data[i]);

        for (size_t i = _capacity; i < newCapacity; i++)
            allocator.construct(newData + i, 0);

        _free();
        _data = newData;
        _capacity = newCapacity;
    }

    void _free() {
        for (size_t i = 0; i < _capacity; i++) {
            allocator.destroy(_data + i);
        }
        allocator.deallocate(_data, _capacity);

        _data = nullptr;
    }

    void _copy(const vector<bool>& other) {
        _size = other._size;
        _capacity = other._capacity;

        _data = allocator.allocate(_capacity);
        for (size_t i = 0; i < _capacity; i++)
            allocator.construct(_data + i, other._data[i]);
    }

    void _move(vector<bool>&& other) {
        _size = other._size;
        _capacity = other._capacity;
        _data = other._data;

        other._data = nullptr;
        other._size = 0;
        other._capacity = 0;
    }
};
