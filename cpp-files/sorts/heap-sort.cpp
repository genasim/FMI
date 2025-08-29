#define DOCTEST_CONFIG_IMPLEMENT

#include <fstream>
#include <iostream>
#include <vector>

#include "../lib/doctest.h"

using namespace std;

template <class T>
ostream& operator<<(ostream& os, const vector<T>& vector) {
    os << "[ ";
    for (auto&& item : vector) os << item << ' ';
    os << ']';
    return os;
}

template <class T, class Comparator = std::less<T>>
void heapify(vector<T>& arr, size_t heapSize, size_t index,
             Comparator comp = Comparator()) {
    while (true) {
        size_t largest = index;
        size_t left = 2 * index + 1;
        size_t right = 2 * index + 2;

        if (left < heapSize && comp(arr[largest], arr[left])) {
            largest = left;
        }
        if (right < heapSize && comp(arr[largest], arr[right])) {
            largest = right;
        }
        if (largest == index) {
            break;
        }
        swap(arr[index], arr[largest]);
        index = largest;
    }
}

template <class T, class Comparator = std::less<T>>
void heap_sort(vector<T>& arr, Comparator comp = Comparator()) {
    const size_t size = arr.size();
    if (size < 2) return;

    for (size_t i = size / 2; i-- > 0;) {
        heapify(arr, size, i, comp);
    }

    for (size_t i = size - 1; i > 0; --i) {
        swap(arr[i], arr[0]);
        heapify(arr, i, 0, comp);
    }
}

TEST_CASE("heap_sort with integers - basic ascending") {
    std::vector<int> arr = {14, 1, 2, 23, -5, 125, 2};
    heap_sort(arr);
    CHECK(arr == std::vector<int>({-5, 1, 2, 2, 14, 23, 125}));
}

TEST_CASE("heap_sort with integers - already sorted ascending") {
    std::vector<int> arr = {-10, -5, 0, 2, 4, 9};
    heap_sort(arr);
    CHECK(arr == std::vector<int>({-10, -5, 0, 2, 4, 9}));
}

TEST_CASE("heap_sort with integers - descending order") {
    std::vector<int> arr = {5, 4, 3, 2, 1};
    heap_sort(arr);
    CHECK(arr == std::vector<int>({1, 2, 3, 4, 5}));
}

TEST_CASE("heap_sort with single element") {
    std::vector<int> arr = {42};
    heap_sort(arr);
    CHECK(arr == std::vector<int>({42}));
}

TEST_CASE("heap_sort with empty vector") {
    std::vector<int> arr;
    heap_sort(arr);
    CHECK(arr.empty());
}

TEST_CASE("heap_sort with duplicates") {
    std::vector<int> arr = {7, 7, 3, 3, 3, 7};
    heap_sort(arr);
    CHECK(arr == std::vector<int>({3, 3, 3, 7, 7, 7}));
}

TEST_CASE("heap_sort with negative numbers") {
    std::vector<int> arr = {-1, -50, -3, -7, -2};
    heap_sort(arr);
    CHECK(arr == std::vector<int>({-50, -7, -3, -2, -1}));
}

TEST_CASE("heap_sort with strings") {
    std::vector<std::string> arr = {"pear", "apple", "orange", "banana"};
    heap_sort(arr);
    CHECK(arr ==
          std::vector<std::string>({"apple", "banana", "orange", "pear"}));
}

TEST_CASE("heap_sort with custom comparator - descending order") {
    std::vector<int> arr = {14, 1, 2, 23, -5, 125, 2};
    heap_sort(arr, std::greater<int>());
    CHECK(arr == std::vector<int>({125, 23, 14, 2, 2, 1, -5}));
}

int main() {
    doctest::Context().run();
    return 0;
}