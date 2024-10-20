#include <iostream>
#include <vector>

template <typename T>
void printArray(T* arr, size_t size) {
    for (size_t i = 0; i < size; i++) {
        std::cout << arr[i] << ' ';
    }
    std::cout << std::endl;
}

template <typename T>
void merge(T* left, size_t leftSize, T* right, size_t rightSize, T* buffer) {
    size_t leftIndex = 0, rightIndex = 0, bufferIndex = 0;

    while (leftIndex < leftSize && rightIndex < rightSize) {
        buffer[bufferIndex++] = left[leftIndex] < right[rightIndex]
                                    ? left[leftIndex++]
                                    : right[rightIndex++];
    }

    while (leftIndex < leftSize) {
        buffer[bufferIndex++] = left[leftIndex++];
    }

    while (rightIndex < rightSize) {
        buffer[bufferIndex++] = right[rightIndex++];
    }
}

template <typename T>
void mergeStep(T* arr, size_t size, T* buffer) {
    if (size <= 1) {
        return;
    }

    size_t middle = size / 2;

    T *left = arr, *right = arr + middle;

    mergeStep(left, middle, buffer);
    mergeStep(right, size - middle, buffer + middle);

    merge(left, middle, right, size - middle, buffer);
}

template <typename T>
void mergeSort(T* arr, size_t size) {
    if (!arr || size <= 1) {
        return;
    }

    T* buffer = new T[size];
    mergeStep(arr, size, buffer);

    for (size_t i = 0; i < size; i++) {
        arr[i] = buffer[i];
    }
    delete[] buffer;
}

int main() {
    int arr1[] = {15,  14,   0, 13, 12, -20, 11, 30, 90, 8,
                  -10, -100, 7, 6,  5,  4,   3,  2,  1};
    size_t size1 = sizeof(arr1) / sizeof(arr1[0]);

    printArray(arr1, size1);
    mergeSort(arr1, size1);
    printArray(arr1, size1);

    return 0;
}