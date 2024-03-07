#include <iostream>

using namespace std;

void printConcat(char* str1, char* str2) {
    if (!str1 && !str2)
        return;

    if (str1 != nullptr)
        cout << *str1;

    if (str2 != nullptr)
        cout << *str2;

    str1 = str1 == nullptr || *(str1 + 1) == '\0' ? nullptr : str1 + 1;
    str2 = str2 == nullptr || *(str2 + 1) == '\0' ? nullptr : str2 + 1;

    printConcat(str1, str2);
}

int main() {
    char* str1 = "ACE";
    char* str2 = "BDFGHI";

    printConcat(str1, str2);

    return 0;
}
