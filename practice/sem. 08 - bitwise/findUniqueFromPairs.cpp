#include <iostream>

using namespace std;

int getUnique(int arr[], unsigned size) {
    int result = 0;
    for (size_t i = 0; i < size; i++)
       result ^= arr[i];
    return result;
}

int main() {
    int arr[] = {1, -12, 3, 3, 5, 6, -12, -4, 1, 6, 5};
    unsigned size = sizeof(arr) / sizeof(arr[0]);

    cout << getUnique(arr, size) << endl;

    return 0;
}