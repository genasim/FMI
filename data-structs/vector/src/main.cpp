#define DOCTEST_CONFIG_IMPLEMENT

#include <fstream>
#include <iostream>
#include <string>

#include "doctest.h"
#include "vector.hpp"

template <typename T>
std::ostream& operator<<(std::ostream& os, const vector<T>& vector) {
    os << "[";
    for (const T& element : vector) {
        os << element << ", ";
    }
    os << "]";
    return os;
}

TEST_CASE("Vector creation") {
    SUBCASE("Default constructor") {
        vector<int> vector;
        CHECK(vector.size() == 0);
        CHECK(vector.capacity() == 0);
    }

    SUBCASE("Constructor with initializer list") {
        vector<int> vector = {1, 2, 3, 4, 5};
        CHECK(vector.size() == 5);
        CHECK(vector.capacity() == 5);
        CHECK(vector[0] == 1);
        CHECK(vector[1] == 2);
        CHECK(vector[2] == 3);
        CHECK(vector[3] == 4);
        CHECK(vector[4] == 5);
    }

    SUBCASE("Copy contsructor") {
        vector<int> vec = {1, 2, 3, 4, 5};
        vector<int> vectorCopy(vec);

        CHECK(vectorCopy.size() == 5);
        CHECK(vectorCopy.capacity() == 5);

        CHECK(vectorCopy[0] == 1);
        CHECK(vectorCopy[1] == 2);
        CHECK(vectorCopy[2] == 3);
        CHECK(vectorCopy[3] == 4);
        CHECK(vectorCopy[4] == 5);
    }

    SUBCASE("Move constructor") {
        vector<int> vec = {1, 2, 3, 4, 5};
        vector<int> vectorMove(std::move(vec));

        CHECK(vectorMove.size() == 5);
        CHECK(vectorMove.capacity() == 5);

        CHECK(vec.size() == 0);

        CHECK(vectorMove[0] == 1);
        CHECK(vectorMove[1] == 2);
        CHECK(vectorMove[2] == 3);
        CHECK(vectorMove[3] == 4);
        CHECK(vectorMove[4] == 5);
    }
}

TEST_CASE("Vector iterators") {
    vector<int> vec = {1, 2, 3, 4, 5};

    SUBCASE("Referance iterators") {
        SUBCASE("Begin and end") {
            CHECK(*vec.begin() == 1);
            // CHECK(*(--vec.end()) == 5);
        }
    }
}

int main() {
    doctest::Context().run();
    return 0;
}
