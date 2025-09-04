#define DOCTEST_CONFIG_IMPLEMENT
#include <fstream>
#include <iostream>
#include <string>

#include "doctest.h"
#include "set.hpp"

// -------------------------------
// Construction / basics
// -------------------------------
TEST_CASE("Set instantiation") {
    SUBCASE("Default constructor") {
        set<int> s;
        CHECK(s.size() == 0);
        CHECK(s.empty());
        CHECK(s.begin() == s.end());  // iteration over empty
    }

    SUBCASE("Initializer list (dedupes)") {
        set<int> s({3, 2, 1, 4, 5, 3, 2, 1});
        CHECK(s.size() == 5);
        CHECK_FALSE(s.empty());

        for (int x = 1; x <= 5; ++x) {
            CHECK(s.contains(x));
            auto it = s.find(x);
            auto end = s.end();
            // auto res = it != end;
            CHECK(s.find(x) != s.end());
        }
        // miss
        CHECK_FALSE(s.contains(42));
        CHECK(s.find(42) == s.end());
    }

    SUBCASE("Copy constructor") {
        set<int> s1({1, 2, 3});
        set<int> s2(s1);

        CHECK(s2.size() == 3);
        CHECK_FALSE(s2.empty());
        CHECK(s2.contains(1));
        CHECK(s2.contains(2));
        CHECK(s2.contains(3));

        SUBCASE("Removing from copy (bool erase)") {
            CHECK(s2.erase(s2.find(2)));
            CHECK(s2.size() == 2);
            CHECK_FALSE(s2.contains(2));

            // original unchanged
            CHECK(s1.size() == 3);
            CHECK(s1.contains(2));
        }
    }

    SUBCASE("Move constructor") {
        set<int> src({10, 20, 30});
        set<int> dst(std::move(src));

        CHECK(dst.size() == 3);
        CHECK(dst.contains(10));
        CHECK(dst.contains(20));
        CHECK(dst.contains(30));

        CHECK(src.size() == 0);  // after move, source should be empty
        CHECK(src.begin() == src.end());
    }
}

// -------------------------------
// Insertion
// -------------------------------
TEST_CASE("Set insertion") {
    set<int> s({1, 2, 3, 4, 5});
    CHECK(s.size() == 5);
    CHECK_FALSE(s.empty());

    SUBCASE("Insert new key") {
        auto [it, inserted] = s.insert(6);
        CHECK(inserted);
        CHECK(*it == 6);
        CHECK(s.size() == 6);
        CHECK(s.contains(6));
    }

    SUBCASE("Inserting duplicate key returns {it,false}") {
        auto [it, inserted] = s.insert(1);
        CHECK_FALSE(inserted);
        CHECK(*it == 1);
        CHECK(s.size() == 5);  // size unchanged
    }
}

// -------------------------------
// Removal
// -------------------------------
TEST_CASE("Set removal") {
    SUBCASE("Removing key with no children (leaf)") {
        set<int> s({2, 1, 3});
        CHECK(s.erase(s.find(3)));
        CHECK(s.size() == 2);
        CHECK_FALSE(s.contains(3));
    }

    SUBCASE("Removing key with right child only") {
        // 1
        //  \
        //   2
        //    \
        //     3
        set<int> s({1, 2, 3});
        CHECK(s.erase(s.find(2)));
        CHECK(s.size() == 2);
        CHECK_FALSE(s.contains(2));
    }

    SUBCASE("Removing key with left child only") {
        //      9
        //     /
        //    4
        //   /
        //  3
        // (10 is extra to make sure right subtree exists but not used)
        set<int> s({9, 4, 3, 10});
        CHECK(s.erase(s.find(4)));  // node 4 has only a left child (3)
        CHECK(s.size() == 3);
        CHECK_FALSE(s.contains(4));
        // structure still a valid BST (simple inorder check)
        auto it = s.begin();
        CHECK(*it++ == 3);
        CHECK(*it++ == 9);
        CHECK(*it++ == 10);
        CHECK(it == s.end());
    }

    SUBCASE("Removing key with both children") {
        //      10
        //     /  \
        //    9    15
        //        /
        //       12
        //         \
        //          14
        set<int> s({10, 9, 15, 12, 14});
        CHECK(s.erase(s.find(10)));
        CHECK(s.size() == 4);
        CHECK_FALSE(s.contains(10));
        // still ordered
        std::vector<int> vals;
        for (auto it = s.begin(); it != s.end(); ++it) vals.push_back(*it);
        CHECK(vals == std::vector<int>({9, 12, 14, 15}));
    }

    SUBCASE("Erase end() is a no-op") {
        set<int> s({1, 2, 3});
        CHECK_FALSE(s.erase(s.end()));
        CHECK(s.size() == 3);
    }

    SUBCASE("Erase from empty set is false") {
        set<int> s;
        CHECK_FALSE(s.erase(s.end()));
        CHECK(s.size() == 0);
    }

    SUBCASE("Erasing same element twice") {
        set<int> s({1, 2, 3});
        auto it = s.find(2);
        CHECK(it != s.end());
        CHECK(s.erase(it));
        CHECK_FALSE(s.erase(s.find(2)));  // second time not found
        CHECK(s.size() == 2);
    }
}

// -------------------------------
// Iteration
// -------------------------------
TEST_CASE("Set iteration") {
    SUBCASE("Manual iteration (inorder ascending with default comparator)") {
        const set<int> s({2, 1, 3});
        auto it = s.begin();
        CHECK(*it == 1);

        ++it;
        CHECK(*it == 2);

        ++it;
        CHECK(*it == 3);

        ++it;
        CHECK(it == s.end());
    }

    SUBCASE("Iteration over empty set") {
        const set<int> s;
        CHECK(s.begin() == s.end());
    }

    SUBCASE("Postfix ++ returns previous position") {
        set<int> s({1, 2, 3});
        auto it = s.begin();
        auto old = it++;
        CHECK(*old == 1);
        CHECK(*it == 2);
    }

    SUBCASE("Custom comparator (std::greater) yields descending order in inorder") {
        const set<int, std::greater<int>> s({3, 1, 4, 2});
        // With std::greater, "left" holds larger keys, so inorder is descending
        std::vector<int> vals;
        for (auto it = s.begin(); it != s.end(); ++it) vals.push_back(*it);
        CHECK(vals == std::vector<int>({4, 3, 2, 1}));
    }
}

// -------------------------------
// Find / contains consistency
// -------------------------------
TEST_CASE("Find/contains consistency") {
    const set<int> s({5, 1, 7});
    CHECK(s.contains(1));
    CHECK(s.find(1) != s.end());

    CHECK_FALSE(s.contains(2));
    CHECK(s.find(2) == s.end());
}

// -------------------------------
// Assignment
// -------------------------------
TEST_CASE("Copy / move assignment") {
    SUBCASE("Copy assignment copies content") {
        set<int> a({1, 2, 3});
        set<int> b;
        b = a;
        CHECK(b.size() == 3);
        CHECK(b.contains(1));
        CHECK(b.contains(2));
        CHECK(b.contains(3));

        // modifying b doesn't change a
        CHECK(b.erase(b.find(2)));
        CHECK(a.contains(2));  // still present in a
    }

    SUBCASE("Move assignment transfers content and clears source") {
        set<int> a({10, 20});
        set<int> b({1});
        b = std::move(a);
        CHECK(b.size() == 2);
        CHECK(b.contains(10));
        CHECK(b.contains(20));
        CHECK(a.size() == 0);
        CHECK(a.begin() == a.end());
    }
}

int main() {
    doctest::Context().run();
    return 0;
}
