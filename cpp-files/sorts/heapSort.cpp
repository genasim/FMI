#include <fstream>
#include <functional>
#include <iostream>
#include <vector>

using namespace std;

template <class T>
ostream& operator<<(ostream& os, const vector<T>& vec) {
    os << "[ ";
    for (const auto& item : vec) {
        os << item << " ";
    }
    os << ']';
    return os;
}

template <class T, class Comparator = std::greater<T>>
void heapify(std::vector<T>& arr, int n, int i,
             Comparator comp = Comparator()) {
    int largest = i;
    int left = 2 * i + 1;
    int right = 2 * i + 2;

    if (left < n && comp(arr[left], arr[largest])) largest = left;

    if (right < n && comp(arr[right], arr[largest])) largest = right;

    if (largest != i) {
        std::swap(arr[i], arr[largest]);
        heapify(arr, n, largest, comp);
    }
}

template <class T, class Comparator = std::greater<T>>
void heapSort(std::vector<T>& arr) {
    int n = arr.size();

    for (int i = n / 2 - 1; i >= 0; i--) {
        heapify(arr, n, i);
    }
}

int main() {
    vector<int> vec = {1, 3, 5, 4, 6, 13, 10, 9, 8, 15, 17};

    cout << vec << endl;
    heapSort<int, less<int>>(vec);
    cout << vec << endl;

    return 0;
}

