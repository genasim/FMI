#include <iostream>

using namespace std;

size_t strLen(const char *str) {
    size_t length = 0;
    while (*str != '\0') {
        length++;
        str++;
    }
    return length;
}

void permute(char *str, int start, int end) {
    if (start == end) {
        cout << str << endl;
        return;
    }

    for (size_t i = start; i < end; i++) {
        swap(str[start], str[i]);
        permute(str, start + 1, end);
        swap(str[i], str[start]);
    }
}

int main() {
    char str[] = "ABCDF";
    size_t size = strLen(str);

    permute(str, 0, size);

    return 0;
}