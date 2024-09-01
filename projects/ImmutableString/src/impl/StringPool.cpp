#include "StringPool.h"

#include <cassert>
#include <cstring>

StringPool::StringNode::StringNode() : str(nullptr), refs(0) {}

const char* StringPool::get(const char* string) noexcept {
    int index = findNode(string);
    if (index == -1) {
        alocateNode(string);
        return nodes.back().str;
    }

    nodes[index].refs++;
    return nodes[index].str;
}

void StringPool::release(const char* string) noexcept {
    int index = findNode(string);
    assert(index != -1);

    if (--nodes[index].refs == 0) {
        removeNodeAt(index);
    }
}

StringPool::~StringPool() {
    for (auto&& node : nodes) {
        delete[] node.str;
    }
}

void StringPool::removeNodeAt(size_t index) noexcept {
    nodes.erase(nodes[index]);
}

void StringPool::alocateNode(const char* str) {
    StringNode node;

    size_t length = strlen(str);
    node.str = new char[length + 1];
    strcpy(node.str, str);
    node.refs = 1;

    nodes.push_back(std::move(node));
}

int StringPool::findNode(const char* string) const noexcept {
    for (size_t i = 0; i < nodes.size(); i++) {
        if (strcmp(nodes[i].str, string) == 0) {
            return i;
        }
    }
    return -1;
}
