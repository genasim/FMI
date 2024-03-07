#include <iostream>

using namespace std;

void printArr(const int a[], const int &size) {
    for (size_t i = 0; i < size; i++)
        cout << a[i] << ' ';
    cout << endl;    
}

void swap(int &a, int &b) {
    int temp = a;
    a = b;
    b = temp;
}

void cocktailSort(int a[], const int &size) {
    bool swapped = true;
    int start = 0;
    int end = size - 1;

    while (swapped) {
        swapped = false;

        for (int i = start; i < end; i++)
            if (a[i] > a[i+1]) {
                swap(a[i], a[i+1]);
                swapped = true;
            }

        if (!swapped)
            break;
        
        swapped = false;
        end--;

        for (int i = end - 1; i >= start; i--)
            if (a[i] > a[i+1]) {
                swap(a[i], a[i+1]);
                swapped = true;
            }
        start++; 
    }
}

int main() {
    int arrays[][10] = {
        { 5, 1, 4, -3, 8, 0, 2, 9, 2, -10 },
        { 7, -14, 23, -5, 12, -19, 5, -30, 11, 12 },
        { -6, 11, -18, 25, -4, 9, -16, 0, 10, -8 },
    };

    for (auto& arr : arrays) {
        unsigned size = sizeof(arr) / sizeof(arr[0]);
        
        printArr(arr, size);
        cocktailSort(arr, size);
        printArr(arr, size);
        
        cout << endl;
    }

    return 0;
}