#pragma once

#include <fstream>
#include <stdexcept>
#include <vector>

template <class Value, class Compare = std::less<Value>, class Container = std::vector<Value>>
class KHeap {
   public:
    explicit KHeap(size_t k = 2);

    void push(const Value& value) noexcept;
    void pop();

    const Value& top() const;

    bool empty() const noexcept;
    std::size_t size() const noexcept;

    template <class V>
    friend std::ostream& operator<<(std::ostream& out, const KHeap<V>& heap);

   private:
    Container container;
    Compare compare;

    size_t k;

    size_t parentIndex(size_t child) const noexcept;
    size_t childIndex(size_t parent, size_t i) const noexcept;

    void heapifyUp(size_t idx) noexcept;
    void heapifyDown(size_t idx) noexcept;
};

template <class V>
inline std::ostream& operator<<(std::ostream& out, const KHeap<V>& heap) {
    for (const auto& el : heap.container) {
        out << el << " ";
    }
    return out;
}

template <class Value, class Compare, class Container>
inline KHeap<Value, Compare, Container>::KHeap(size_t k) : k(k) {
    if (k < 1) {
        throw std::invalid_argument("K must be >= 1.");
    }
}

template <class Value, class Compare, class Container>
inline void KHeap<Value, Compare, Container>::push(const Value& value) noexcept {
    container.push_back(value);
    heapifyUp(container.size() - 1);
}

template <class Value, class Compare, class Container>
inline const Value& KHeap<Value, Compare, Container>::top() const {
    if (container.empty()) {
        throw std::out_of_range("Cannot get top element from empty heap.");
    }
    return container.front();
}

template <class Value, class Compare, class Container>
inline void KHeap<Value, Compare, Container>::pop() {
    if (container.empty()) {
        throw std::out_of_range("Cannot pop from empty heap.");
    }
    std::swap(container.front(), container.back());
    container.pop_back();
    if (!container.empty()) {
        heapifyDown(0);
    }
}

template <class Value, class Compare, class Container>
inline bool KHeap<Value, Compare, Container>::empty() const noexcept {
    return container.empty();
}

template <class Value, class Compare, class Container>
inline std::size_t KHeap<Value, Compare, Container>::size() const noexcept {
    return container.size();
}

template <class Value, class Compare, class Container>
inline size_t KHeap<Value, Compare, Container>::parentIndex(size_t child) const noexcept {
    return (child - 1) / k;
}

template <class Value, class Compare, class Container>
inline size_t KHeap<Value, Compare, Container>::childIndex(size_t parent, size_t i) const noexcept {
    return k * parent + 1 + i;
}

template <class Value, class Compare, class Container>
inline void KHeap<Value, Compare, Container>::heapifyUp(size_t idx) noexcept {
    while (idx > 0 && compare(container[idx], container[parentIndex(idx)])) {
        std::swap(container[idx], container[parentIndex(idx)]);
        idx = parentIndex(idx);
    }
}

template <class Value, class Compare, class Container>
inline void KHeap<Value, Compare, Container>::heapifyDown(size_t idx) noexcept {
    while (true) {
        size_t smallest = idx;
        for (size_t i = 0; i < k; ++i) {
            size_t childIdx = childIndex(idx, i);
            if (childIdx < container.size() && compare(container[childIdx], container[smallest])) {
                smallest = childIdx;
            }
        }
        if (smallest != idx) {
            std::swap(container[idx], container[smallest]);
            idx = smallest;
        } else {
            break;
        }
    }
}
