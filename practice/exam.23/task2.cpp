#include <iostream>

using namespace std;

size_t countChars(const char *string, bool(*predicate)(const char ch)) {
    size_t count = 0;
    while (*string != '\0') {
        if(predicate(*string))
            count++;
        string++;
    }
    return count;
}

bool isUpper(const char ch) {
    return ch >= 'A' && ch <= 'Z';
}

bool isLower(const char ch) {
    return ch >= 'a' && ch <= 'z';
}

char* concatUpperWithLower(const char a[], const char b[]) {
    if (a == nullptr || b == nullptr)
        return nullptr;

    size_t uppersA = countChars(a, isUpper);
    size_t lowersB = countChars(b, isLower);
    
    size_t size = uppersA + lowersB;
    char *concat = new char[size + 1];

    size_t index = 0;
    for (size_t i = 0; a[i] != '\0'; i++) {
        if(isUpper(a[i]))
            concat[index++] = a[i];
    }
    
    for (size_t i = 0; b[i] != '\0' ; i++) {
        if(isLower(b[i]))
            concat[index++] = b[i];
    }

    concat[size + 1] = '\0';
    return concat;
}

int main() {
    char a[] = "aAbAbbX";
    char b[] = "bYzAb";

    char* result = concatUpperWithLower(a, b);

    cout << a << " " << b << endl;  // aAbAbbX bYzAb
    cout << result << endl;         // AAXbzb

    delete[] result;
    return 0;
}