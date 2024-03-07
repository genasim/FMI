/**
 * Задача 1: Да се реализира рекурсивна функция, която пресмята n!
*/

#include <iostream>

using namespace std;

int factorial(const int n) {
    if (n == 1)
        return 1;

    return n * factorial(n-1);
}

int factorial_tail(const int n, const int _result = 1) {
    if (n == 1)
        return _result;

    return factorial_tail(n - 1, _result * n);
}

int main() {
    int n = 6;

    cout << "Trad: " << factorial(n) << endl;
    cout << "Tail: " << factorial_tail(n) << endl;

    return 0;
}