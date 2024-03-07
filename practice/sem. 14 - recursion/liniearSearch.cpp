/**
 * Задача 5: Да се реализира рекурсивна функция, която приема произволен 
 * масив и число и връща дали числото се съдържа в масива. (Линейно търсене).
*/

#include <iostream>

using namespace std;

bool contains(const int arr[], const size_t size, const int elem) {
    if (size == 0)
        return false;

    if (arr[size - 1] == elem)
        return true;

    return contains(arr, size - 1, elem);
}

int main() {
    int arr[] = { 7, -14, 23, -5, 12, -19, 5, -30, 11, 12 };
    size_t size = sizeof(arr) / sizeof(arr[0]);

    cout << contains(arr, size, -30) << endl;
    cout << contains(arr, size, 100) << endl;

    return 0;
}