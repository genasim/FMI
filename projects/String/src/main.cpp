#include <iostream>

#include "String.h"

int main() {
    String* str1 = new String("Hello world!!");
    String str2("Habibi");
    std::cout << str1->compare(str2) << std::endl;

    delete str1;
    std::cout << str2 << std::endl;

    std::cin >> str2;
    std::cout << str2 << std::endl;

    str2.clear();
    std::cout << str2 << std::endl;

    return 0;
}