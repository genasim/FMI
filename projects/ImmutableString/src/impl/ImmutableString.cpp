#include "ImmutableString.h"

#include <cstring>

StringPool ImmutableString::_pool;

ImmutableString::ImmutableString() : ImmutableString("") {}

ImmutableString::ImmutableString(const char* str) {
    _str = _pool.get(str);
    _size = strlen(_str);
}

ImmutableString::ImmutableString(const ImmutableString& other) { _copy(other); }

ImmutableString& ImmutableString::operator=(const ImmutableString& other) {
    if (this != &other) {
        _free();
        _copy(other);
    }
    return *this;
}

ImmutableString::ImmutableString(ImmutableString&& other) noexcept {
    _move(other);
}

ImmutableString& ImmutableString::operator=(ImmutableString&& other) noexcept {
    if (this != &other) {
        _free();
        _move(other);
    }
    return *this;
}

ImmutableString::~ImmutableString() { _free(); }

const char* ImmutableString::c_str() const noexcept { return _str; }

size_t ImmutableString::size() const noexcept { return _size; }

bool ImmutableString::operator==(const ImmutableString& other) const noexcept {
    return _str == other._str;
}

bool ImmutableString::operator!=(const ImmutableString& other) const noexcept {
    return !(*this == other);
}

bool ImmutableString::operator<(const ImmutableString& other) const noexcept {
    return strcmp(_str, other._str) < 0;
}

bool ImmutableString::operator>(const ImmutableString& other) const noexcept {
    return strcmp(_str, other._str) > 0;
}

bool ImmutableString::operator<=(const ImmutableString& other) const noexcept {
    return _str == other._str || strcmp(_str, other._str) < 0;
}

bool ImmutableString::operator>=(const ImmutableString& other) const noexcept {
    return _str == other._str || strcmp(_str, other._str) > 0;
}

void ImmutableString::_free() noexcept {
    _pool.release(_str);
    _str = nullptr;
    _size = 0;
}

void ImmutableString::_copy(const ImmutableString& other) noexcept {
    _str = _pool.get(other._str);
    _size = other._size;
}

void ImmutableString::_move(ImmutableString& other) noexcept {
    _str = other._str;
    _size = other._size;

    other._str = nullptr;
    other._size = 0;
}

std::ostream& operator<<(std::ostream& os, const ImmutableString& str) {
    os << str.c_str();
    return os;
}
