#pragma once

#include <algorithm>
#include <forward_list>
#include <initializer_list>
#include <list>
#include <memory>
#include <vector>

template <class Value, class HashFunc = std::hash<Value>, class KeyEq = std::equal_to<Value>>
class unordered_set {
   public:
    struct iterator;

    unordered_set(size_t _bucket_count = 16, float load_factor = 0.75);
    unordered_set(const std::initializer_list<Value>& init_list);

    unordered_set(const unordered_set& other);
    unordered_set(unordered_set&& other) noexcept;
    unordered_set& operator=(const unordered_set& other);
    unordered_set& operator=(unordered_set&& other) noexcept;
    ~unordered_set();

    iterator begin() const noexcept;
    iterator end() const noexcept;

    iterator insert(const Value& value);
    iterator insert(Value&& value);

    iterator remove(const Value& value) noexcept;
    iterator remove(const iterator& it) noexcept;

    template <class Predicate>
    void erase_if(Predicate pred) noexcept;

    void clear() noexcept;

    iterator find(const Value& value) const noexcept;

    size_t size() const noexcept;
    size_t bucket_count() const noexcept;
    float load_factor() const noexcept;

    void rehash(size_t new_bucket_size);

   private:
    using DataContainer = std::list<Value>;
    using Bucket = std::forward_list<typename DataContainer::const_iterator>;

    HashFunc _hash;
    KeyEq _keyEq;

    size_t _size = 0;
    float _load_factor;
    size_t _bucket_count;

    DataContainer _data;
    std::vector<Bucket> _hashTable;

    void _free();
    void _copy(const unordered_set& other);
    void _move(unordered_set&& other) noexcept;

    size_t _hash_index_of(const Value& value) const noexcept;

   public:
    struct iterator {
       private:
        using BucketVectorIt = typename std::vector<Bucket>::const_iterator;
        using BucketIt = typename Bucket::const_iterator;

        BucketVectorIt bucketTableIt;
        BucketVectorIt tableEnd;
        BucketIt bucketIt;

        // befriend the container for use in remove(iter) for fast and easy access
        friend class unordered_set;

       public:
        iterator(BucketVectorIt bucketIt, BucketVectorIt tableEnd, BucketIt bucketPos)
            : bucketTableIt(bucketIt), tableEnd(tableEnd), bucketIt(bucketPos) {}

        iterator& operator++() {
            ++bucketIt;

            while (bucketTableIt != tableEnd && bucketIt == bucketTableIt->end()) {
                ++bucketTableIt;
                if (bucketTableIt != tableEnd) {
                    bucketIt = bucketTableIt->begin();
                }
            }

            return *this;
        }

        bool operator==(const iterator& other) const noexcept {
            return bucketTableIt == other.bucketTableIt &&
                   (bucketTableIt == tableEnd || bucketIt == other.bucketIt);
        }
        bool operator!=(const iterator& other) const noexcept { return !(*this == other); }

        const Value& operator*() const { return **bucketIt; }
    };
};

template <class Value, class HashFunc, class KeyEq>
unordered_set<Value, HashFunc, KeyEq>::unordered_set(size_t bucket_count, float load_factor)
    : _load_factor(load_factor), _bucket_count(bucket_count), _hashTable(bucket_count) {}

template <class Value, class HashFunc, class KeyEq>
unordered_set<Value, HashFunc, KeyEq>::unordered_set(const std::initializer_list<Value>& init_list)
    : unordered_set() {
    for (auto&& value : init_list) insert(value);
}

template <class Value, class HashFunc, class KeyEq>
unordered_set<Value, HashFunc, KeyEq>::unordered_set(const unordered_set& other) {
    _copy(other);
}

template <class Value, class HashFunc, class KeyEq>
unordered_set<Value, HashFunc, KeyEq>::unordered_set(unordered_set&& other) noexcept {
    _move(std::move(other));
}

template <class Value, class HashFunc, class KeyEq>
unordered_set<Value, HashFunc, KeyEq>::~unordered_set() {
    _free();
}

template <class Value, class HashFunc, class KeyEq>
unordered_set<Value, HashFunc, KeyEq>& unordered_set<Value, HashFunc, KeyEq>::operator=(
    const unordered_set<Value, HashFunc, KeyEq>& other) {
    if (this != &other) {
        _free();
        _copy(other);
    }
    return *this;
}

template <class Value, class HashFunc, class KeyEq>
unordered_set<Value, HashFunc, KeyEq>& unordered_set<Value, HashFunc, KeyEq>::operator=(
    unordered_set<Value, HashFunc, KeyEq>&& other) noexcept {
    if (this != &other) {
        _free();
        _move(std::move(other));
    }
    return *this;
}

template <class Value, class HashFunc, class KeyEq>
void unordered_set<Value, HashFunc, KeyEq>::_free() {
    _data.clear();
    _hashTable.clear();
    _size = 0;
}

template <class Value, class HashFunc, class KeyEq>
void unordered_set<Value, HashFunc, KeyEq>::_copy(
    const unordered_set<Value, HashFunc, KeyEq>& other) {
    _hash = other._hash;
    _keyEq = other._keyEq;
    _size = other._size;
    _bucket_count = other._bucket_count;
    _load_factor = other._load_factor;

    _data = other._data;
    rehash(bucket_count());
}

template <class Value, class HashFunc, class KeyEq>
void unordered_set<Value, HashFunc, KeyEq>::_move(
    unordered_set<Value, HashFunc, KeyEq>&& other) noexcept {
    _hash = std::move(other._hash);
    _keyEq = std::move(other._keyEq);
    _size = other._size;
    _bucket_count = other._bucket_count;
    _load_factor = other._load_factor;

    _data = std::move(other._data);
    _hashTable = std::move(other._hashTable);

    other._size = 0;
    other._bucket_count = 0;
    other._load_factor = 0;
}

template <class Value, class HashFunc, class KeyEq>
inline size_t unordered_set<Value, HashFunc, KeyEq>::_hash_index_of(
    const Value& value) const noexcept {
    return _hash(value) % _hashTable.size();
}

template <class Value, class HashFunc, class KeyEq>
inline size_t unordered_set<Value, HashFunc, KeyEq>::size() const noexcept {
    return _size;
}

template <class Value, class HashFunc, class KeyEq>
inline size_t unordered_set<Value, HashFunc, KeyEq>::bucket_count() const noexcept {
    return _bucket_count;
}

template <class Value, class HashFunc, class KeyEq>
inline float unordered_set<Value, HashFunc, KeyEq>::load_factor() const noexcept {
    return _load_factor;
}

template <class Value, class HashFunc, class KeyEq>
typename unordered_set<Value, HashFunc, KeyEq>::iterator
unordered_set<Value, HashFunc, KeyEq>::insert(const Value& value) {
    size_t bucketIdx = _hash_index_of(value);
    Bucket& bucket = _hashTable[bucketIdx];

    for (auto it = bucket.cbegin(); it != bucket.cend(); ++it) {
        if (_keyEq(**it, value))
            return iterator(_hashTable.cbegin() + bucketIdx, _hashTable.cend(), it);
    }

    _data.push_front(value);
    bucket.push_front(_data.cbegin());
    ++_size;

    if (size() >= load_factor() * bucket_count()) rehash(bucket_count() * 2);

    return iterator(_hashTable.cbegin() + bucketIdx, _hashTable.cend(), bucket.cbegin());
}

template <class Value, class HashFunc, class KeyEq>
typename unordered_set<Value, HashFunc, KeyEq>::iterator
unordered_set<Value, HashFunc, KeyEq>::insert(Value&& value) {
    size_t bucketIdx = _hash_index_of(value);
    Bucket& bucket = _hashTable[bucketIdx];

    for (auto it = bucket.cbegin(); it != bucket.cend(); ++it) {
        if (_keyEq(**it, value))
            return iterator(_hashTable.cbegin() + bucketIdx, _hashTable.cend(), it);
    }

    _data.push_front(std::move(value));
    bucket.push_front(_data.cbegin());
    ++_size;

    if (size() >= load_factor() * bucket_count()) rehash(bucket_count() * 2);

    return iterator(_hashTable.cbegin() + bucketIdx, _hashTable.cend(), bucket.cbegin());
}

template <class Value, class HashFunc, class KeyEq>
typename unordered_set<Value, HashFunc, KeyEq>::iterator
unordered_set<Value, HashFunc, KeyEq>::remove(const Value& value) noexcept {
    size_t bucketIdx = _hash_index_of(value);
    Bucket& bucket = _hashTable[bucketIdx];

    for (auto it = bucket.cbegin(); it != bucket.cend();) {
        if (!_keyEq(**it, value)) {
            ++it;
            continue;
        }

        auto nextIt = it;
        ++nextIt;

        _data.erase(*it);
        bucket.remove(*it);
        --_size;

        auto bucketTableIt = _hashTable.cbegin() + bucketIdx;
        while (bucketTableIt != _hashTable.cend() && nextIt == bucketTableIt->cend()) {
            ++bucketTableIt;
            if (bucketTableIt != _hashTable.cend()) {
                nextIt = bucketTableIt->cbegin();
            }
        }

        return iterator(bucketTableIt, _hashTable.cend(), nextIt);
    }

    return end();
}

template <class Value, class HashFunc, class KeyEq>
typename unordered_set<Value, HashFunc, KeyEq>::iterator
unordered_set<Value, HashFunc, KeyEq>::remove(const iterator& it) noexcept {
    if (it == end()) return end();

    size_t bucketIdx = std::distance(_hashTable.cbegin(), it.bucketTableIt);
    Bucket& bucket = _hashTable[bucketIdx];

    auto dataIt = *it.bucketIt;
    _data.erase(dataIt);
    bucket.remove(dataIt);
    --_size;

    auto bucketTableIt = it.bucketTableIt;
    auto nextIt = it.bucketIt;
    ++nextIt;

    while (bucketTableIt != _hashTable.cend() && nextIt == bucketTableIt->cend()) {
        ++bucketTableIt;
        if (bucketTableIt != _hashTable.cend()) {
            nextIt = bucketTableIt->cbegin();
        }
    }

    return iterator(bucketTableIt, _hashTable.cend(), nextIt);
}

template <class Value, class HashFunc, class KeyEq>
typename unordered_set<Value, HashFunc, KeyEq>::iterator
unordered_set<Value, HashFunc, KeyEq>::find(const Value& value) const noexcept {
    size_t hashIdx = _hash_index_of(value);
    const Bucket& bucket = _hashTable[hashIdx];

    for (auto it = bucket.cbegin(); it != bucket.cend(); ++it) {
        if (_keyEq(**it, value))
            return iterator(_hashTable.cbegin() + hashIdx, _hashTable.cend(), it);
    }

    return end();
}

template <class Value, class HashFunc, class KeyEq>
typename unordered_set<Value, HashFunc, KeyEq>::iterator
unordered_set<Value, HashFunc, KeyEq>::begin() const noexcept {
    if (size() == 0) return end();

    auto bucket = _hashTable.cbegin();
    auto tableEnd = _hashTable.cend();

    while (bucket != tableEnd && bucket->empty()) ++bucket;

    if (bucket == tableEnd) return end();

    return iterator(bucket, tableEnd, bucket->cbegin());
}

template <class Value, class HashFunc, class KeyEq>
typename unordered_set<Value, HashFunc, KeyEq>::iterator
unordered_set<Value, HashFunc, KeyEq>::end() const noexcept {
    return iterator(_hashTable.cend(), _hashTable.cend(), {});
}

template <class Value, class HashFunc, class KeyEq>
void unordered_set<Value, HashFunc, KeyEq>::clear() noexcept {
    _data.clear();
    _hashTable.clear();

    _hashTable.resize(bucket_count());
    _size = 0;
}

template <class Value, class HashFunc, class KeyEq>
void unordered_set<Value, HashFunc, KeyEq>::rehash(size_t new_bucket_size) {
    if (new_bucket_size == 0) return;

    _hashTable.clear();
    _hashTable.resize(new_bucket_size);
    _bucket_count = new_bucket_size;

    for (auto it = _data.cbegin(); it != _data.cend(); ++it) {
        size_t hashIdx = _hash_index_of(*it);
        _hashTable[hashIdx].push_front(it);
    }
}

template <class Value, class HashFunc, class KeyEq>
template <class Predicate>
void unordered_set<Value, HashFunc, KeyEq>::erase_if(Predicate pred) noexcept {
    for (auto it = begin(); it != end();) {
        if (pred(*it)) {
            it = remove(it);
        } else {
            ++it;
        }
    }
}
