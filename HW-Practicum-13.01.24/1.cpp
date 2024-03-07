#include <iostream>

using namespace std;

bool isNumber(const char &item) {
    return item >= '0' && item <= '9';
}

void coundDigitsIn(const char* const str, size_t *count) {
    if (*str != '\0')
        coundDigitsIn(str+1, count);
    
    if (isNumber(*str))
        (*count)++;
}

int main() {
    const char* str = " 5 ducksf 151 lew 10 kilom 34 etersin 8 hou 7 rs";
    size_t result = 0;
    coundDigitsIn(str, &result);
    cout << result << endl;
    return 0;
}