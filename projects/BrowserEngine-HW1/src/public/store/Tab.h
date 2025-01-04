#pragma once

#include <ctime>
#include <fstream>
#include <string>

class Tab {
   public:
    Tab(const std::string& url);
    Tab(const char* url);

    ~Tab() = default;

    const std::string& url() const noexcept;
    std::time_t timestamp() const noexcept;

    void setUrl(const std::string& url) noexcept;

    friend std::ostream& operator<<(std::ostream& os, const Tab& tab);

    bool operator<(const Tab& other) const noexcept;
    bool operator==(const Tab& other) const noexcept;
    bool operator!=(const Tab& other) const noexcept;

   private:
    std::string _url;
    std::time_t _timestamp;
};
