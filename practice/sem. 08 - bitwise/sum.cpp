#include <iostream>

using namespace std;

struct NumPair {
    int a;
    int b;
};

int sum(int a, int b) {
    while (b != 0) {
        unsigned carry = (a & b) << 1;
        a ^= b;
        b = carry;
    }
    return a;
}

int main() {
    NumPair pairs[] = {{20, 17}, {-18, 23}, {0, 3}, {40, -8}};
    for (const auto &pair : pairs)
        cout << pair.a << " + " << pair.b << " = " << sum(pair.a, pair.b) << endl;

    return 0;
}