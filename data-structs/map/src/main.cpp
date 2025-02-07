#define DOCTEST_CONFIG_IMPLEMENT

#include <fstream>
#include <iostream>
#include <string>

#include "doctest.h"
#include "map.hpp"

TEST_CASE("Map instantiation") {
    SUBCASE("Default constructor") {
        Map<int, std::string> map;
        CHECK(map.size() == 0);
        CHECK(map.empty());
    }
}

TEST_CASE("Map insertion") {
    Map<int, std::string> map;
    map.insert(1, "one");
    map.insert(2, "two");
    map.insert(3, "three");
    map.insert(4, "four");
    map.insert(5, "five");

    CHECK(map.size() == 5);
    CHECK_FALSE(map.empty());

    SUBCASE("Inserting duplicate key") {
        CHECK_FALSE(map.insert(1, "uno"));
        CHECK(map.size() == 5);

        CHECK_FALSE(map.at(1).second == "one");
        CHECK(map.at(1).second == "uno");
    }

    SUBCASE("Inserting pair") {
        CHECK(map.insert({6, "six"}));
        CHECK(map.size() == 6);

        CHECK_FALSE(map.insert({6, "seis"}));
        CHECK(map.size() == 6);
    }
}

TEST_CASE()

TEST_CASE("Map iteration") {
    Map<int, std::string> map;
    map.insert(1, "one");
    map.insert(2, "two");
    map.insert(3, "three");

    SUBCASE("Manual iteration") {
        auto it = map.begin();
        CHECK((*it).first == 1);
        CHECK((*it).second == "one");

        ++it;
        CHECK((*it).first == 2);
        CHECK((*it).second == "two");

        ++it;
        CHECK((*it).first == 3);
        CHECK((*it).second == "three");

        ++it;
        CHECK(it == map.end());
    }

    SUBCASE("Range-based for loop") {
        int i = 1;
        for (const auto& [key, value] : map) {
            CHECK(key == i);
            CHECK(value == std::to_string(i));
            ++i;
        }
    }
}

int main() {
    doctest::Context().run();
    return 0;
}
