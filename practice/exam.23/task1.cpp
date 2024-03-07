#include <iostream>

using namespace std;

int power(const int num, int pow) {
    int result = 1;
    while (pow != 0) {
        result *= num;
        pow--;
    }
    return result;
}

int concatDigitBack(const int num, const unsigned digit) {
    return num * 10 + digit;
}

unsigned extractDigit(int &num, bool(*predicate)(int cur, int comp)) {
    int idx = 0, position = 0;
    unsigned result = num % 10;

    int original = num;
    while (original != 0) {
        unsigned cur = original % 10;
        if (predicate(cur, result)) {
            result = cur;
            position = idx;
        }

        idx++;
        original /= 10;
    }

    int mod = power(10, position);
    int rightSide = num % mod;
    int leftSide = num / (mod * 10);
    num = leftSide * mod + rightSide;

    return result;
}

int sortDigits(int num, bool(*predicate)(int cur, int comp)) {
    int result = 0;

    while (num != 0) {
        unsigned digit = extractDigit(num, predicate);
        result = concatDigitBack(result, digit);
    }

    return result;
}

int main() {
     int numbers[] = {432551, 71, 258, 0};

     for (int n : numbers) {
         cout << "Input: " << n << endl;                                                                // 432551
         cout << "Descending: " << sortDigits(n, [](int cur, int comp) { return cur > comp; }) << endl; // 123455
         cout << "Ascending: " << sortDigits(n, [](int cur, int comp) { return cur < comp; }) << endl;  // 554321
         cout << endl;
     }

    return 0;
}
