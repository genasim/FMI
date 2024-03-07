#include <iostream>

using namespace std;

unsigned setBitsCount(unsigned num) {
    unsigned result = 0;

    while (num >= 1) {
        if (num & 1)
            result++;
        num >>= 1;
    }
    return result;
}

int main() {
    unsigned c = 23;
    cout << setBitsCount(c) << endl;

    return 0;
}