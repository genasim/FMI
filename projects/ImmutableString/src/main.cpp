#include <iostream>
#include "ImmutableString.h"

int main() {
    ImmutableString str1("Hello");
    ImmutableString str2("Hello");

    ImmutableString str3(str1);

    ImmutableString str4("World");

    std::cout << (str1 == str2) << std::endl;
    std::cout << (str1 == str3) << std::endl;
    std::cout << (str1 == str4) << std::endl;

    return 0;
}
