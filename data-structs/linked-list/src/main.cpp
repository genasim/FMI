#include <fstream>
#include <iostream>

#include "list.hpp"

template <typename T>
std::ostream& operator<<(std::ostream& os, const list<T>& list) {
    os << "[ ";
    for (const auto& item : list) {
        os << item << " ";
    }
    os << ']';
    return os;
}

int main() {
    list<int> list = {1, 2, 5, 6, 99, 3, 0, -29};
    std::cout << list << std::endl;

    auto pos = list.insert(++list.cbegin(), {-4, -7, -8, -9});

    std::cout << list << " | " << *++pos << std::endl;

    return 0;
}
