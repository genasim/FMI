#include <iostream>

#include "vector.hpp"

std::ostream& operator<<(std::ostream& os, const vector<bool>& vec) {
    os << "[ ";
    for (size_t i = 0; i < vec.size(); ++i) {
        os << vec[i] << " ";
    }
    os << "]";
    return os;
}

int main() {
    vector<bool> vec;

    vec.push_back(true);
    vec.push_back(true);
    vec.push_back(false);
    vec.push_back(false);
    vec.push_back(false);
    vec.push_back(true);
    vec.push_back(true);
    vec.push_back(true);
    vec.push_back(false);

    std::cout << vec << std::endl;

    std::cout << *vec.begin() << std::endl;

    auto iter = vec.begin();
    vec.insert(iter, false);
    std::cout << vec << std::endl;

    ++(++(++iter));
    vec.remove(iter);
    std::cout << vec << std::endl;
}
