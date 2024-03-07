#include <iostream>

using namespace std;

bool isNegative(int num) {
    unsigned offset = sizeof(num) * 8 - 1;
    int mask = 1 << offset;
    return (num & mask) != 0;
}

int sum(int a, int b) {
    while(b != 0) {
        unsigned carry = (a & b) << 1;
        a ^= b;
        b = carry;
    }
    return a;
}

int abs(int num) {
    return isNegative(num) ? sum(~num, 1) : num;
}

int main() {
    const int nums[] = {2, 4, -6, 8, -19, -25, 32, 64, -100};
    for (const auto &num : nums)
        cout << num << " -> abs: " << abs(num) << endl;

    return 0;
}