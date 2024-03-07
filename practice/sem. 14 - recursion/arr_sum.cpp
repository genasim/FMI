/**
 * Задача 4: Да се реализира рекурсивна функция, която приема масив от цели числа и връща сумата на елементите в него.
*/

#include <iostream>

using namespace std;

int arrSum(const int arr[], const size_t size) {
    if (size == 1)
        return arr[0];

    return arr[size - 1] + arrSum(arr, size - 1);
}

int arrSum_tail(const int arr[], const size_t size, const int sum = 0) {
    if (size == 0)
        return sum;

    return arrSum_tail(arr, size - 1, sum + arr[size - 1]);
}

int main() {
    int arr[] = { 7, -14, 23, -5, 12, -19, 5, -30, 11, 12 };
    size_t size = sizeof(arr) / sizeof(arr[0]);

    cout << "Trad: " << arrSum(arr, size) << endl;
    cout << "Tail: " << arrSum_tail(arr, size) << endl;

    return 0;
}
