#include <iostream>

using namespace std;

int binarySearch(int arr[], size_t size, int key)
{
    int left = 0, right = size - 1;
    while (left <= right) {
        int mid = left + (right - left) / 2;
 
        if (arr[mid] == key)
            return mid;
 
        if (arr[mid] < key)
            left = mid + 1;
        else
            right = mid - 1;
    }
    return -1;
}
 

int main() {
    int arr[] = { 2, 3, 4, 10, 40 };
    size_t size = sizeof(arr) / sizeof(arr[0]);

    cout << binarySearch(arr, size, 10) << endl;
    cout << binarySearch(arr, size, 100) << endl;

    return 0;
}