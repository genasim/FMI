/**
 * Задача 2: Напишете функция, която приема неотрицателно число n и връща числото обърнато
 * Вход: 288, Изход: 882
*/

/**
 * Задача 3: Напишете функция, която приема неотрицателно число и връща дали числото е палиндром.
 * Вход: 2882 Изход: true
*/

#include <iostream>

using namespace std;

unsigned reversed(unsigned num) {
    unsigned result = 0;

    while (num != 0) {
        unsigned digit = num % 10;
        result *= 10;
        result += digit;
        num /= 10;
    }

    return result;
}

bool isPalindrome(const unsigned &num) {
    return num == reversed(num);
}

int main() {
    unsigned n;
    cin >> n;

    cout << reversed(n) << endl;
    cout << (isPalindrome(n) ? "true" : "false") << endl;

    return 0;
}