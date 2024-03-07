/**
 * Задача 1: Въвежда се неотрицателно число n. Да се отпечатат всички двойки прости числа, които са във вида 6k-1 и 6k+1.
*/

#include <iostream>
#include <valarray>
using namespace std;

bool isPrime(const unsigned &num) {
    if (num <= 1)
        return false;
    
    double temp = sqrt(num);

    for (size_t i = 2; i < temp; i++) {
        if (num % i == 0)
            return false;
    }

    return true;
}

int main() {
    unsigned n;
    cin >> n;

    for (size_t i = 6; i < n; i += 6) {
        if (isPrime(i-1) && isPrime(i+1))
            cout << i-1 << " | " << i+1 << endl;
    }
    
    return 0;
}