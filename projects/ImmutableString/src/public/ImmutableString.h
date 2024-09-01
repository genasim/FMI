#pragma once

#include "StringPool.h"
#include <fstream>

class ImmutableString {
   public:
    ImmutableString();
    ImmutableString(const char* str);

    ImmutableString(const ImmutableString& other);
    ImmutableString& operator=(const ImmutableString& other);

    ImmutableString(ImmutableString&& other) noexcept;
    ImmutableString& operator=(ImmutableString&& other) noexcept;

    ~ImmutableString();

    const char* c_str() const noexcept;
    size_t size() const noexcept;

    friend std::ostream& operator<<(std::ostream& os, const ImmutableString& str);

    bool operator==(const ImmutableString& other) const noexcept;
    bool operator!=(const ImmutableString& other) const noexcept;
    bool operator<(const ImmutableString& other) const noexcept;
    bool operator>(const ImmutableString& other) const noexcept;
    bool operator<=(const ImmutableString& other) const noexcept;
    bool operator>=(const ImmutableString& other) const noexcept;

   private:
    static StringPool _pool;
    void _free() noexcept;
    void _copy(const ImmutableString& other) noexcept;
    void _move(ImmutableString& other) noexcept;

    const char* _str;
    size_t _size;
};
