#include <iostream>

using namespace std;

void printArr(const int arr[], const size_t size) {
    for (size_t i = 0; i < size; i++) {
        cout << arr[i] << ' ';
    }
    cout << endl;
}

void insertionSort(int arr[], int n) {
    int key, j;
    for (int i = 1; i < n; i++) {
        key = arr[i];
        j = i - 1;

        while (j >= 0 && arr[j] > key) {
            arr[j + 1] = arr[j];
            j = j - 1;
        }
        arr[j + 1] = key; 
    }
}

int main() {
    int arr[] = { 2, 3, 1, 10, 0, 40, -5, -8, 0, 1 };
    size_t size = sizeof(arr) / sizeof(arr[0]);

    printArr(arr, size);
    insertionSort(arr, size);
    printArr(arr, size);

    return 0;
}