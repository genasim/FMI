#define DOCTEST_CONFIG_IMPLEMENT

#include <fstream>
#include <iostream>
#include <string>

#include "doctest.h"
#include "unordered_set.hpp"

TEST_CASE("Test unordered_set initialization") {
    unordered_set<char> set{'a', 'b', 'c', 'd'};
    CHECK(set.size() == 4);

    CHECK(set.find('a') != set.end());
    CHECK(set.find('b') != set.end());
    CHECK(set.find('c') != set.end());
    CHECK(set.find('d') != set.end());
}

TEST_CASE("Test iterator dereference") {
    unordered_set<char> set{'a', 'b', 'c'};

    CHECK(*set.find('a') == 'a');
    CHECK(*set.find('b') == 'b');
    CHECK(*set.find('c') == 'c');
}

TEST_CASE("Test unordered_set insert") {
    unordered_set<int> set;
    CHECK(set.size() == 0);

    auto it1 = set.insert(20);
    auto it2 = set.insert(0);
    auto it3 = set.insert(100);
    auto it4 = set.insert(100);

    CHECK(set.size() == 3);

    CHECK(set.find(20) != set.end());
    CHECK(set.find(0) != set.end());
    CHECK(set.find(100) != set.end());

    CHECK(it1 == set.find(20));
    CHECK(it2 == set.find(0));
    CHECK(it3 == set.find(100));
    CHECK(it4 == it3);
}

TEST_CASE("Test unordered_set copy constructor") {
    unordered_set<int> set{1, 2, 3};
    unordered_set<int> set2(set);

    CHECK(set.size() == set2.size());
    CHECK(set.find(1) != set2.find(1));
    CHECK(set.find(2) != set2.find(2));
    CHECK(set.find(3) != set2.find(3));

    set2.insert(20);
    set.remove(3);
    CHECK(set.find(20) == set.end());
    CHECK(set2.find(3) != set2.end());
}

TEST_CASE("Test unordered_set move constructor") {
    unordered_set<int> set{1, 2, 3};
    unordered_set<int> set2(std::move(set));

    CHECK(set.size() == 0);
    CHECK(set2.size() == 3);
    CHECK(set2.find(1) != set2.end());
    CHECK(set2.find(2) != set2.end());
    CHECK(set2.find(3) != set2.end());
}

TEST_CASE("Test unordered_set copy assignment") {
    unordered_set<int> set{1, 2, 3};
    unordered_set<int> set2;
    set2 = set;

    CHECK(set.size() == set2.size());
    CHECK(set.find(1) != set2.end());
    CHECK(set.find(2) != set2.end());
    CHECK(set.find(3) != set2.end());
}

TEST_CASE("Test unordered_set move assignment") {
    unordered_set<int> set{1, 2, 3};
    unordered_set<int> set2;
    set2 = std::move(set);

    CHECK(set.size() == 0);
    CHECK(set2.size() == 3);
    CHECK(set2.find(1) != set2.end());
    CHECK(set2.find(2) != set2.end());
    CHECK(set2.find(3) != set2.end());
}

TEST_CASE("Test unordered_set remove via value") {
    unordered_set<int> set{1, 2};
    CHECK(set.size() == 2);

    CHECK(set.remove(100) == set.end());

    set.remove(1);
    CHECK(set.size() == 1);
    CHECK(set.find(1) == set.end());
    CHECK(set.find(2) != set.end());

    set.remove(2);
    CHECK(set.size() == 0);
    CHECK(set.find(1) == set.end());
    CHECK(set.find(2) == set.end());
}

TEST_CASE("Test unordered_set remove via iterator") {
    unordered_set<int> set{1, 2};
    CHECK(set.size() == 2);

    CHECK(set.remove(set.end()) == set.end());
    CHECK(set.remove(set.find(100)) == set.end());

    set.remove(set.find(1));
    CHECK(set.size() == 1);
    CHECK(set.find(1) == set.end());
    CHECK(set.find(2) != set.end());

    set.remove(set.find(2));
    CHECK(set.size() == 0);
    CHECK(set.find(1) == set.end());
    CHECK(set.find(2) == set.end());
}

TEST_CASE("Test remove single item from set via value") {
    unordered_set<int> set(1);
    set.insert(1);
    CHECK(set.size() == 1);

    auto it = set.remove(1);
    CHECK(it == set.end());
}

TEST_CASE("Test remove single item from set via iterator") {
    unordered_set<int> set{1};
    CHECK(set.size() == 1);

    auto it = set.remove(set.find(1));
    std::cout << (it == set.end()) << std::endl;
    CHECK(it == set.end());
}

TEST_CASE("Test unordered_set clear") {
    unordered_set<int> set{1, 2, 3};
    CHECK(set.size() == 3);

    set.clear();
    CHECK(set.size() == 0);
    CHECK(set.find(1) == set.end());
    CHECK(set.find(2) == set.end());
    CHECK(set.find(3) == set.end());
}

TEST_CASE("Test unordered_set erase_it with functor object") {
    unordered_set<int> set{1, 2, 3, 4, 5, 6};

    struct EvenPredicate {
        bool operator()(int value) const noexcept { return value % 2 == 0; }
    };
    set.erase_if(EvenPredicate());
    CHECK(set.size() == 3);

    CHECK(set.find(1) != set.end());
    CHECK(set.find(3) != set.end());
    CHECK(set.find(5) != set.end());

    CHECK(set.find(2) == set.end());
    CHECK(set.find(4) == set.end());
    CHECK(set.find(6) == set.end());
}

TEST_CASE("Test unordered_set erase_it with lambda function") {
    unordered_set<int> set{1, 2, 3, 4, 5, 6};

    set.erase_if([](int value) { return value % 2 == 0; });
    CHECK(set.size() == 3);

    CHECK(set.find(1) != set.end());
    CHECK(set.find(3) != set.end());
    CHECK(set.find(5) != set.end());

    CHECK(set.find(2) == set.end());
    CHECK(set.find(4) == set.end());
    CHECK(set.find(6) == set.end());
}

int main() {
    doctest::Context().run();
    return 0;
}
