#define DOCTEST_CONFIG_IMPLEMENT

#include <fstream>
#include <iostream>
#include <string>

#include "doctest.h"
#include "map.hpp"

TEST_CASE("Map instantiation") {
    SUBCASE("Default constructor") {
        map<int, std::string> map;
        CHECK(map.size() == 0);
        CHECK(map.empty());
    }

    SUBCASE("Initializer list") {
        map<int, std::string> map({{1, "1"}, {2, "2"}, {3, "3"}, {4, "4"}, {5, "5"}});
        CHECK(map.size() == 5);
        CHECK_FALSE(map.empty());

        CHECK(map.has_key(1));
        CHECK(map.has_key(2));
        CHECK(map.has_key(3));
        CHECK(map.has_key(4));
        CHECK(map.has_key(5));
    }
}

TEST_CASE("Map insertion") {
    map<int, std::string> map({{1, "one"}, {2, "two"}, {3, "three"}, {4, "four"}, {5, "five"}});
    CHECK(map.size() == 5);
    CHECK_FALSE(map.empty());

    SUBCASE("Inserting duplicate key") {
        CHECK_FALSE(map.insert(1, "uno").second);
        CHECK(map.size() == 5);

        CHECK_FALSE(map.at(1).second == "one");
        CHECK(map.at(1).second == "uno");
    }

    SUBCASE("Inserting pair") {
        CHECK(map.insert({6, "six"}).second);
        CHECK(map.size() == 6);

        CHECK_FALSE(map.insert({6, "seis"}).second);
        CHECK(map.size() == 6);
    }
}

TEST_CASE("Map removal") {
    SUBCASE("Removing key with no children") {
        map<int, std::string> map({{1, "1"}, {2, "2"}, {3, "3"}});
        CHECK(map.remove(3));
        CHECK(map.size() == 2);
    }

    SUBCASE("Removing key with right child") {
        map<int, std::string> map({{1, "1"}, {2, "2"}, {3, "3"}});
        CHECK(map.remove(2));
        CHECK(map.size() == 2);
    }

    SUBCASE("Removing key with left child") {
        map<int, std::string> map({{9, "9"}, {4, "4"}, {3, "3"}});
        CHECK(map.remove(9));
        CHECK(map.size() == 2);
    }

    SUBCASE("Removing key with both children") {
        map<int, std::string> map({{10, "10"}, {9, "9"}, {15, "15"}, {12, "12"}, {14, "14"}});
        CHECK(map.remove(10));
        CHECK(map.size() == 4);
    }
}

TEST_CASE("Map iteration") {
    const map<int, std::string> map({{2, "two"}, {1, "one"}, {3, "three"}});

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
}

int main() {
    doctest::Context().run();
    return 0;
}
