#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include "../lib/doctest.h"
#include "./list.h"

TEST_CASE("Insert head into empty singly list") {
    SinglyNode<int>* head = nullptr;
    head = insert_head(head, 10);
    CHECK(head != nullptr);
    CHECK(head->data == 10);
    CHECK(head->next == nullptr);
    free_list(head);
}

TEST_CASE("Insert head into non-empty singly list") {
    SinglyNode<int>* head = nullptr;
    head = insert_head(head, 20);
    head = insert_head(head, 10);
    CHECK(head->data == 10);
    CHECK(head->next->data == 20);
    free_list(head);
}

TEST_CASE("Insert head into empty doubly list") {
    DoublyNode<int>* head = nullptr;
    head = insert_head(head, 42);
    CHECK(head != nullptr);
    CHECK(head->data == 42);
    CHECK(head->next == nullptr);
    CHECK(head->prev == nullptr);
    free_list(head);
}

TEST_CASE("Insert head into non-empty doubly list") {
    DoublyNode<int>* head = nullptr;
    head = insert_head(head, 5);
    head = insert_head(head, 3);
    CHECK(head->data == 3);
    CHECK(head->next->data == 5);
    CHECK(head->next->prev == head);
    free_list(head);
}

TEST_CASE("Insert tail into empty singly list") {
    SinglyNode<int>* head = nullptr;
    head = insert_tail(head, 10);
    CHECK(head->data == 10);
    CHECK(head->next == nullptr);
    free_list(head);
}

TEST_CASE("Insert tail into non-empty singly list") {
    SinglyNode<int>* head = nullptr;
    head = insert_tail(head, 1);
    head = insert_tail(head, 2);
    head = insert_tail(head, 3);
    CHECK(head->next->next->data == 3);
    free_list(head);
}

TEST_CASE("Insert tail into empty doubly list") {
    DoublyNode<int>* head = nullptr;
    head = insert_tail(head, 100);
    CHECK(head->data == 100);
    CHECK(head->next == nullptr);
    CHECK(head->prev == nullptr);
    free_list(head);
}

TEST_CASE("Insert tail into non-empty doubly list") {
    DoublyNode<int>* head = nullptr;
    head = insert_tail(head, 1);
    head = insert_tail(head, 2);
    head = insert_tail(head, 3);
    CHECK(head->next->next->data == 3);
    CHECK(head->next->next->prev->data == 2);
    free_list(head);
}

TEST_CASE("Delete from empty singly list") {
    SinglyNode<int>* head = nullptr;
    head = delete_value(head, 5);
    CHECK(head == nullptr);
}

TEST_CASE("Delete head in singly list") {
    SinglyNode<int>* head = nullptr;
    head = insert_head(head, 10);
    head = delete_value(head, 10);
    CHECK(head == nullptr);
    free_list(head);
}

TEST_CASE("Delete middle element in singly list") {
    SinglyNode<int>* head = nullptr;
    head = insert_tail(head, 1);
    head = insert_tail(head, 2);
    head = insert_tail(head, 3);
    head = delete_value(head, 2);
    CHECK(head->next->data == 3);
    free_list(head);
}

TEST_CASE("Delete from empty doubly list") {
    DoublyNode<int>* head = nullptr;
    head = delete_value(head, 7);
    CHECK(head == nullptr);
}

TEST_CASE("Delete head in doubly list") {
    DoublyNode<int>* head = nullptr;
    head = insert_tail(head, 1);
    head = insert_tail(head, 2);
    head = delete_value(head, 1);
    CHECK(head->data == 2);
    CHECK(head->prev == nullptr);
    free_list(head);
}

TEST_CASE("Delete tail in doubly list") {
    DoublyNode<int>* head = nullptr;
    head = insert_tail(head, 1);
    head = insert_tail(head, 2);
    head = delete_value(head, 2);
    CHECK(head->data == 1);
    CHECK(head->next == nullptr);
    free_list(head);
}

TEST_CASE("Find in empty singly list") {
    SinglyNode<int>* head = nullptr;
    CHECK_FALSE(find_value(head, 42));
}

TEST_CASE("Find existing value in singly list") {
    SinglyNode<int>* head = nullptr;
    head = insert_tail(head, 1);
    head = insert_tail(head, 2);
    CHECK(find_value(head, 2));
    free_list(head);
}

TEST_CASE("Find non-existing value in doubly list") {
    DoublyNode<int>* head = nullptr;
    head = insert_tail(head, 10);
    CHECK_FALSE(find_value(head, 5));
    free_list(head);
}

TEST_CASE("Find existing value in doubly list") {
    DoublyNode<int>* head = nullptr;
    head = insert_tail(head, 1);
    head = insert_tail(head, 2);
    head = insert_tail(head, 3);
    CHECK(find_value(head, 3));
    free_list(head);
}
