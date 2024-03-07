#include <iostream>

using namespace std;

bool isSuffix(unsigned num, unsigned pat) {
    if (num < pat)
        return false;
    
    while (pat != 0) {
        if ((num % 10) != (pat % 10))
            return false;
        
        num /= 10;
        pat /= 10;
    }
    return true;
}

bool isPrefix(unsigned num, unsigned pat) {
    while (num >= pat) {
        if (num % pat == 0)
            return true;
        num /= 10;
    }
    return false;
}

int main() {
    unsigned n, k;
    cin >> n >> k;

    cout << (isSuffix(n, k) ? "true" : "false") << endl;
    cout << (isPrefix(n, k) ? "true" : "false") << endl;

    return 0;
}