#include "store/Tab.h"

Tab::Tab(const std::string& url) : _url(url), _timestamp(std::time(nullptr)) {}

Tab::Tab(const char* url) : Tab(std::string(url)) {}

std::time_t Tab::timestamp() const noexcept { return _timestamp; }

const std::string& Tab::url() const noexcept { return _url; }

void Tab::setUrl(const std::string& url) noexcept {
    _url = url;
    _timestamp = std::time(nullptr);
}

std::ostream& operator<<(std::ostream& os, const Tab& tab) {
    os << tab._url << " " << tab._timestamp;
    return os;
}

bool Tab::operator<(const Tab& other) const noexcept { return false; }

bool Tab::operator==(const Tab& other) const noexcept {
    return _timestamp == other._timestamp && _url == other._url;
}

bool Tab::operator!=(const Tab& other) const noexcept {
    return !(*this == other);
}