#pragma once

template <class T>
struct SinglyNode {
    T data;
    SinglyNode* next;

    SinglyNode(const T& data, SinglyNode* next = nullptr)
        : data(data), next(next) {}
};

template <class T>
struct DoublyNode {
    T data;
    DoublyNode* next;
    DoublyNode* prev;

    DoublyNode(const T& data, DoublyNode* next = nullptr,
               DoublyNode* prev = nullptr)
        : data(data), next(next), prev(prev) {}
};

template <class T>
void free_list(SinglyNode<T>* list) {
    while (list != nullptr) {
        SinglyNode<T>* curr = list;
        list = list->next;
        delete curr;
    }
}

template <class T>
void free_list(DoublyNode<T>*& list) {
    while (list != nullptr) {
        DoublyNode<T>* curr = list;
        list = list->next;
        delete curr;
    }
}

template <class T>
SinglyNode<T>* insert_head(SinglyNode<T>* head, const T& value) {
    SinglyNode<T>* newHead = new SinglyNode(value, head);
    return newHead;
}

template <class T>
DoublyNode<T>* insert_head(DoublyNode<T>* head, const T& value) {
    DoublyNode<T>* newHead = new DoublyNode(value, head);
    if (head != nullptr) head->prev = newHead;

    return newHead;
}

template <class T>
SinglyNode<T>* insert_tail(SinglyNode<T>* head, const T& value) {
    if (head == nullptr) return new SinglyNode(value);

    SinglyNode<T>* it = head;
    while (it->next) {
        it = it->next;
    }
    it->next = new SinglyNode(value);

    return head;
}

template <class T>
DoublyNode<T>* insert_tail(DoublyNode<T>* head, const T& value) {
    if (head == nullptr) return new DoublyNode(value);

    DoublyNode<T>* it = head;
    while (it->next) {
        it = it->next;
    }
    DoublyNode<T>* newTail = new DoublyNode<T>(value, nullptr, it);
    it->next = newTail;

    return head;
}

template <class T>
SinglyNode<T>* delete_value(SinglyNode<T>* head, const T& value) {
    if (!head) return head;

    if (head->data == value) {
        SinglyNode<T>* toDelete = head;
        head = head->next;
        delete toDelete;
        return head;
    }

    SinglyNode<T>* it = head;
    while (it->next && it->next->data != value) {
        it = it->next;
    }

    if (!it->next) return head;

    SinglyNode<T>* toDelete = it->next;
    it->next = it->next->next;
    delete toDelete;

    return head;
}

template <class T>
DoublyNode<T>* delete_value(DoublyNode<T>* head, const T& value) {
    if (head == nullptr) return head;

    DoublyNode<T>* it = head;
    while (it && it->data != value) {
        it = it->next;
    }

    if (it == nullptr) return head;

    if (it == head) {
        head = head->next;
        if (head) head->prev = nullptr;

        delete it;
        return head;
    }

    if (it->prev) it->prev->next = it->next;
    if (it->next) it->next->prev = it->prev;

    delete it;
    return head;
}

template <class T>
bool find_value(SinglyNode<T>* head, const T& value) {
    if (head == nullptr) return false;
    while (head) {
        if (head->data == value) return true;
        head = head->next;
    }
    return false;
}

template <class T>
bool find_value(DoublyNode<T>* head, const T& value) {
    if (head == nullptr) return false;
    while (head) {
        if (head->data == value) return true;
        head = head->next;
    }
    return false;
}