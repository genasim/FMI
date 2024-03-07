#include <iostream>

using namespace std;

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

bool areOpositeSigns(const int a, const int b) {
    size_t offset = sizeof(int) * 8 - 1;
    int mask = 1 << offset;

    return ((a & mask) ^ (b & mask));
}

int main() {
    int a = 5, b = -59;

    printBinary(a);
    printBinary(b);

    cout << a << " | " << b << " -> " << areOpositeSigns(a, b) << endl;

    return 0;
}