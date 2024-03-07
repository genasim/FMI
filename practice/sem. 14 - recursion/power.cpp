/**
 * Задача 3: Да се реализира рекурсивна функция, която вдига число на дадена степен.
*/

#include <iostream>

using namespace std;

int power(const int num, const int pow) {
    if (pow == 1)
        return num;

    return num * power(num, pow - 1);
}

int power_tail(const int num, const int pow, const int result = 1) {
    if (pow == 0)
        return result;

    return power_tail(num, pow - 1, result * num);
}

int main() {
    int n = 2, k = 8;

    cout << "Trad: " << power(n, k) << endl;
    cout << "Tail: " << power_tail(n, k) << endl;

    return 0;
}