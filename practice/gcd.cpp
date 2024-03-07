#include <iostream>

using namespace std;

void swap(int &a, int &b) {
    int temp = a;
    a = b;
    b = temp;
}

int gcd(int a, int b) {
    while (b != 0) {
        a %= b;
        swap(a,b);
    }
    return a;
}

struct NumPair
{
    const int a;
    const int b;
};


int main() {
    const NumPair pairs[] = {
        {6, 2},
        {2, 6},
        {52, 288},
        {180, 1008},
        {524, 126},
    };

    for (const auto &pair : pairs)
        cout << "gcd of " << pair.a << " and " << pair.b << ": " << gcd(pair.a, pair.b) << endl;

    return 0;
}