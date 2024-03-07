#include <iostream>

using namespace std;

unsigned reversedBits(unsigned num) {
    unsigned result = 0;
    while (num >= 1) {
        result <<= 1;
        result |= (num & 1);
        num >>= 1;
    }
    return result;
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
    unsigned c = 23;
    unsigned cRev = reversedBits(c);

    printBinary(c);
    printBinary(cRev);

    return 0;
}