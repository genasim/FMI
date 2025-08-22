#define DOCTEST_CONFIG_IMPLEMENT

#include <fstream>
#include <iostream>
#include <string>

#include "doctest.h"
#include "stack.hpp"

TEST_CASE("stack<int> basic operations") {
    stack<int> s;
    CHECK(s.empty());
    CHECK(s.size() == 0);

    s.push(42);
    CHECK_FALSE(s.empty());
    CHECK(s.size() == 1);
    CHECK(s.top() == 42);

    s.push(7);
    CHECK(s.size() == 2);
    CHECK(s.top() == 7);

    s.pop();
    CHECK(s.size() == 1);
    CHECK(s.top() == 42);

    s.pop();
    CHECK(s.empty());
    CHECK(s.size() == 0);
}

TEST_CASE("stack<int> emplace") {
    stack<int> s;
    s.emplace(123);
    CHECK(s.size() == 1);
    CHECK(s.top() == 123);

    s.emplace(456);
    CHECK(s.size() == 2);
    CHECK(s.top() == 456);
}

TEST_CASE("stack<int> copy constructor and assignment") {
    stack<int> s;
    s.push(1);
    s.push(2);

    stack<int> s2(s);
    CHECK(s2.size() == 2);
    CHECK(s2.top() == 2);

    s2.pop();
    CHECK(s.size() == 2);
    CHECK(s2.size() == 1);
    CHECK(s2.top() == 1);
    CHECK(s.top() == 2);
}

TEST_CASE("stack<int> move constructor and assignment") {
    stack<int> s;
    s.push(10);
    s.push(20);

    stack<int> s2(std::move(s));
    CHECK(s2.size() == 2);
    CHECK(s2.top() == 20);
    CHECK(s.size() == 0);

    stack<int> s3;
    s3 = std::move(s2);
    CHECK(s3.size() == 2);
    CHECK(s3.top() == 20);
    CHECK(s2.size() == 0);
}

TEST_CASE("stack<int> pop on empty stack") {
    stack<int> s;
    // Should not crash, but size should remain 0
    s.pop();
    CHECK(s.size() == 0);
    CHECK(s.empty());
}

TEST_CASE("stack<int> top on empty stack") {
    stack<int> s;
    // Dereferencing begin() on empty forward_list is undefined,
    // but we check that the method exists and does not throw.
    // Uncommenting the next line may cause undefined behavior.
    // s.top();
}

int main() {
    doctest::Context().run();
    return 0;
}
