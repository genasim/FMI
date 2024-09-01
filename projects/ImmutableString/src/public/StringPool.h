#pragma once

#include "Vector.h"

class StringPool {
   public:
    const char* get(const char*) noexcept;
    void release(const char*) noexcept;

    StringPool() = default;
    StringPool(const StringPool&) = delete;
    StringPool& operator=(const StringPool&) = delete;

    StringPool(StringPool&&) = default;
    StringPool& operator=(StringPool&&) = default;

    ~StringPool();

   private:
    struct StringNode {
        char* str;
        uint32_t refs;
    };

    Vector<StringPool::StringNode> nodes;

    void removeNodeAt(size_t index) noexcept;
    void alocateNode(const char* str);
    int findNode(const char*) const noexcept;
};
