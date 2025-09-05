#include <iostream>
#include <ostream>
#include <vector>

using namespace std;

template <class T>
ostream& operator<<(ostream& os, const vector<T>& vec) {
    os << "[ ";
    for (const auto& elem : vec) {
        os << elem;
    }
    os << " ]";
    return os;
}

void printBoolVectors(size_t n) {
    vector<unsigned short> boolVector(n, 0);

    while (true) {
        cout << boolVector << endl;

        int idx = n - 1;
        while (idx >= 0 && boolVector[idx] == 1) {
            boolVector[idx] = 0;
            --idx;
        }

        if (idx < 0) break;
        boolVector[idx] = 1;
    }
}

int main() {
    cout << "Enter size of bool vectors: ";
    int n;
    cin >> n;

    printBoolVectors(n);
    return 0;
}
