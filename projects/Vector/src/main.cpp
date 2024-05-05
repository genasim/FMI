#include <iostream>
#include <vector>

#include "Vector.h"

int main() {
    int arr[] = {1, 1, 2, 3, 4, 1, 3, 5, 1, 2};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    Vector<int> vector;
    vector.insertRange(arr, size);
    Vector<int> defaulted(5, -1);
    std::cout << vector << "\n" << defaulted << std::endl;

    std::cout << (vector == defaulted) << std::endl;

    defaulted.reserve(10);
    defaulted.reserve(25);

    // std::cout << vector << "\n" << vector2 << std::endl;
    // std::cout << (vector < vector2) << std::endl;
    // std::cout << (vector > vector2) << std::endl;

    // int arrS[] = {9,99,9};
    // size_t sizeS = sizeof(arrS) / sizeof(arrS[0]);
    // Vector<int> sample(arrS, sizeS);
    // vector2.insertRange(sample, 4);

    // std::cout << vector << "\n" << vector2 << '\n' << sample << std::endl;
    // std::cout << (vector < vector2) << std::endl;
    // std::cout << (sample == vector) << std::endl;

    // vector.insert(20, 6);
    std::cout << vector.contains(20) << " " << vector.contains(3) << std::endl;

    for (auto &&i : vector) {
        i += 1000;
    }
    std::cout << vector << "\n" << defaulted << std::endl;

    return 0;
}
