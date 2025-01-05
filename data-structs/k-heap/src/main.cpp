#define DOCTEST_CONFIG_IMPLEMENT

#include <fstream>
#include <iostream>
#include <string>

#include "doctest.h"
#include "kheap.hpp"

TEST_CASE("Testing KHeap with k=3 - Basic Operations") {
    KHeap<int> myKHeap(3);

    SUBCASE("Initially empty") {
        CHECK(myKHeap.empty() == true);
        CHECK(myKHeap.size() == 0);

        CHECK_THROWS_AS(myKHeap.top(), std::out_of_range);
        CHECK_THROWS_AS(myKHeap.pop(), std::out_of_range);
    }

    SUBCASE("Push elements") {
        myKHeap.push(10);
        myKHeap.push(20);
        myKHeap.push(1);
        myKHeap.push(5);
        myKHeap.push(7);

        CHECK(myKHeap.size() == 5);
        CHECK(myKHeap.empty() == false);

        CHECK(myKHeap.top() == 1);

        SUBCASE("Pop once") {
            myKHeap.pop();
            CHECK(myKHeap.size() == 4);
            CHECK(myKHeap.top() == 5);

            SUBCASE("Pop twice") {
                myKHeap.pop();
                CHECK(myKHeap.size() == 3);
                CHECK(myKHeap.top() == 7);
            }
        }
    }
}

int main() {
    doctest::Context().run();
    return 0;
}
