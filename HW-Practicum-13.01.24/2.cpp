#include <iostream>

using namespace std;

void swap(int& a, int& b) {
    int temp = a;
    a = b;
    b = temp;
}

int Abs(const int& a) {
    return a >= 0 ? a : -a;
}

void printArr(const int* const arr, const size_t &size) {
    for (size_t i = 0; i < size; i++)
        cout << arr[i] << " ";
    cout << endl;
}

int getNextIndex(const size_t &curr, const size_t &size) {
    return curr + 1 == size ? 0 : curr + 1;
}

void shuffleRight(int *arr, const size_t &size) {
    for (size_t i = size-1; i > 0 ; i--)
        swap(arr[getNextIndex(i, size)], arr[i]);
}

int getPrevIndex(const int &curr, const size_t &size) {
    return curr - 1 < 0 ? size - 1 : curr - 1;
}

void shuffleLeft(int *arr, const size_t &size) {
    for (size_t i = 0; i < size-1 ; i++)
        swap(arr[i], arr[getPrevIndex(i, size)]);
}

void shuffleArray(int arr[], const size_t &size, const int &offset) {
    unsigned int iterations = Abs(offset);
    if (offset > 0)
        for (size_t i = 0; i < iterations; i++)
            shuffleRight(arr, size);
    
    if (offset < 0)
        for (size_t i = 0; i < iterations; i++)
            shuffleLeft(arr, size);
}

int main() {
    size_t size;
    cout << "Enter size: ";
    cin >> size;

    int arr[size];
    cout << "Enter " << size << " array elements: ";
    for (size_t i = 0; i < size; i++)
        cin >> arr[i];

    int shuffles;    
    cout << "Enter shuffles (-/+): ";
    cin >> shuffles;

    cout << endl << "========" << endl;
    printArr(arr, size);
    shuffleArray(arr, size, shuffles);
    printArr(arr, size);

    return 0;
}