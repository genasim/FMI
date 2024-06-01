#include "utils/String.h"

#include <cstring>

#include "utils/String.h"

size_t nextPowerOfTwo(size_t curr) noexcept {
    size_t result = 1;
    while (result < curr) {
        result <<= 1;
    }
    return result;
}

String::String() : String("") {}

String::String(const char* str) {
    size_t size = strlen(str);
    _data = new char[size + 1];
    _length = size;
    _capacity = size + 1;

    strcpy(_data, str);
    _data[size] = '\0';
}

String::String(const String& other) { _copy(other); }

String& String::operator=(const String& other) {
    if (this != &other) {
        _free();
        _copy(other);
    }
    return *this;
}

String::String(String&& other) noexcept { _move(std::move(other)); }

String& String::operator=(String&& other) noexcept {
    if (this != &other) {
        _free();
        _move(std::move(other));
    }
    return *this;
}

bool String::operator==(const String& other) const noexcept {
    return compare(other) == 0;
}

bool String::operator!=(const String& other) const noexcept {
    return !(*this == other);
}

bool String::operator>(const String& other) const noexcept {
    return compare(other) > 0;
}

bool String::operator<(const String& other) const noexcept {
    return compare(other) < 0;
}

bool String::operator>=(const String& other) const noexcept {
    return compare(other) >= 0;
}

bool String::operator<=(const String& other) const noexcept {
    return compare(other) <= 0;
}

String::~String() { _free(); }

void String::resize(size_t to) {
    if (to == _length) {
        return;
    }

    if (to < _length) {
        for (size_t i = to; i < _length; i++) {
            _data[i] = '\0';
        }
        _length = to;
        return;
    }

    if (to > _capacity) {
        size_t new_cap = nextPowerOfTwo(to);
        reserve(new_cap);
    }

    for (size_t i = _length; i < to; i++) {
        _data[i] = '\0';
    }
    _length = to;
}

void String::reserve(size_t to) {
    if (to < _capacity) return;

    char* newData = new char[to];
    strcpy(newData, _data);
    _free();
    _data = newData;
    _capacity = to;
}

void String::_free() noexcept {
    delete[] _data;
}

void String::_copy(const String& other) {
    _data = new char[other.capacity()];
    _length = other.length();
    _capacity = other.capacity();

    strcpy(_data, other.c_str());
    _data[_length] = '\0';
}

void String::_move(String&& other) noexcept {
    _data = other._data;
    _length = other.length();
    _capacity = other.capacity();

    other._data = nullptr;
    other._length = 0;
    other._capacity = 0;
}

const char* String::c_str() const noexcept { return _data; }

size_t String::length() const noexcept { return _length; }

size_t String::capacity() const noexcept { return _capacity; }

int String::compare(const String& other) const noexcept {
    return strcmp(_data, other._data);
}

void String::append(const char* str) {
    size_t new_size = _length + strlen(str);
    if (new_size > _capacity) {
        resize(new_size + 1);
    }
    strcat(_data, str);
    _length = new_size;
}

void String::push_back(char ch) {
    resize(_length + 1);
    _data[_length - 1] = ch;
    _data[_length] = '\0';
}

void String::pop_back() noexcept {
    if (_length > 0) {
        _data[--_length] = '\0';
    }
}

void String::shrink_to_fit() {
    char* data = new char[_length];
    strcpy(data, _data);
    _free();
    _data = data;
    _capacity = _length;
}

void String::clear() noexcept {
    for (size_t i = 0; i < length(); i++) {
        _data[i] = '\0';
    }
    _length = 0;
}

std::ostream& operator<<(std::ostream& out, const String& str) {
    out << str._data;
    return out;
}

std::istream& operator>>(std::istream& in, String& str) {
    char buffer[1024];
    in >> buffer;
    str = String(buffer);
    return in;
}

char* operator+(const String& str, const char* cstr) { 
    return strcat((char*)str.c_str(), cstr);
}

char* operator+(char* cstr, const String& str) {
    return strcat(cstr, str.c_str());
}
