#include <iostream>

using namespace std;

bool isPowerOfTwo(int num) {
    num = num < 0 ? ~num + 1 : num;

    int mask = 1;
    while (num >= mask) {
        if (mask == num)
            return true;
        mask <<= 1;
    }
    return false;
}

int main() {
    const int numsPos[] = {2, 4, 6, 8, 19, 25, 32, 64, 100};
    for (const auto &num : numsPos)
        cout << num << ": " << (isPowerOfTwo(num) ? "true" : "false") << endl;

    cout << "------------------------------------" << endl;

    const int numsNeg[] = {-2, -4, -6, -8, -19, -25, -32, -64, -100};
    for (const auto &num : numsNeg)
        cout << num << ": " << (isPowerOfTwo(num) ? "true" : "false") << endl;

    return 0;
}