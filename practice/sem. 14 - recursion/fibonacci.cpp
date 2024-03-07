/**
 * Задача 2: Да се реализира рекурсивна функция, която пресмята n-тото число на фибоначи.
*/

#include <iostream>

using namespace std;

const int MAX_N = 50;
long long memo[MAX_N + 1];

long long fibonacci(int n) {
    if (n <= 1)
        return n;

    if (memo[n] != 0)
        return memo[n];

    memo[n] = fibonacci(n-1) + fibonacci(n-2);

    return memo[n];
}

int main() {
    int nums[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 45, 53, 60, 80, 85};
    for (const int& num : nums)
        cout << "n: " << num << '\t' << fibonacci(num) << endl;

    return 0;
}