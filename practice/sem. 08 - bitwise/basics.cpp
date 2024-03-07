#include <iostream>

using namespace std;

void swap(int &a, int &b) {
    a ^= b;
    b ^= a;
    a ^= b;
}

bool isOdd(const int a) {
    return a & 1;
}

void multiplyByPowerOfTwo(int &num, unsigned power) {
    num <<= power;
}

void divideByPowerOfTwo(int &num, unsigned power) {
    num >>= power;
}

void printBinary(const unsigned n) {
    size_t offset = sizeof(n) * 8 - 1;
    unsigned int mask = 1 << offset; 
    while (mask >= 1) {
        bool bit = (n & mask) != 0;
        cout << bit;
        mask >>= 1;
    }
    cout << endl;
}

int main() {
    int a = 10, b = 15;
    printBinary(a);
    printBinary(b);

    cout << (isOdd(b) ? "true" : "false") << endl;
    
    multiplyByPowerOfTwo(b, 2);
    cout << a << endl;
    divideByPowerOfTwo(b, 4);
    cout << a << endl;

    return 0;
}