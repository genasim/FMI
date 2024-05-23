#pragma once
#include <fstream>

class String {
   public:
    String();
    String(const char *str);
    String(const String &other);
    String &operator=(const String &other);
    String(String &&other) noexcept;
    String &operator=(String &&other) noexcept;

    const char *c_str() const noexcept;
    size_t length() const noexcept;
    size_t capacity() const noexcept;

    int compare(const String &other) const noexcept;
    void append(const char *str);

    void push_back(char ch);
    void pop_back() noexcept;
    void shrink_to_fit();
    void clear() noexcept;
    void resize(size_t to);
    void reserve(size_t to);

    friend std::ostream &operator<<(std::ostream &out, const String &str);
    friend std::istream &operator>>(std::istream &in, String &str);

    bool operator==(const String &other) const noexcept;
    bool operator!=(const String &other) const noexcept;
    bool operator>(const String &other) const noexcept;
    bool operator<(const String &other) const noexcept;
    bool operator>=(const String &other) const noexcept;
    bool operator<=(const String &other) const noexcept;

    ~String();

   private:
    char *_data = nullptr;
    size_t _length = 0;
    size_t _capacity = 0;


    void _free() noexcept;
    void _copy(const String &other);
    void _move(String &&other) noexcept;
};