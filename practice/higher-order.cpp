#include <iostream>

using namespace std;

int Abs(const int &a) {
    return a >= 0 ? a : -a;
}

void printArr(const int a[], const int &size) {
    for (size_t i = 0; i < size; i++)
        cout << a[i] << ' ';
    cout << endl;    
}


void cocktailSort(int a[], const int &size, bool(*predicate)(char a, char b)) {
    bool swapped = true;
    int start = 0;
    int end = size - 1;

    while (swapped) {
        swapped = false;

        for (int i = start; i < end; i++)
            if (predicate(a[i], a[i+1])) {
                swap(a[i], a[i+1]);
                swapped = true;
            }
        end--;

        if (!swapped)
            break;
        swapped = false;

        for (int i = end - 1; i >= start; i--)
            if (predicate(a[i], a[i+1])) {
                swap(a[i], a[i+1]);
                swapped = true;
            }
        start++; 
    }
}

int main () {
    int arr[] = { 7, -14, 23, 5, 12, -19, -5, -30, 11, 12 };
    unsigned size = sizeof(arr) / sizeof(arr[0]);
    
    printArr(arr, size);
    cout << "----------------------------" << endl;

    cout << "By value:\t";
    cocktailSort(arr, size, [](char a, char b) { return a > b; });
    printArr(arr, size);

    cout << "By absolute:\t";
    cocktailSort(arr, size, [](char a, char b) { return Abs(a) > Abs(b); });
    printArr(arr, size);

    return 0;
}