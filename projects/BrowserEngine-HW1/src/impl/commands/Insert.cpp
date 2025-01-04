#include "commands/Insert.h"

#include <iostream>

#include "store/BrowserStore.h"

Insert::Insert(const std::string& url) : url(url) {}

void Insert::execute() const noexcept {
    auto store = BrowserStore::instance();
    store->insert(url);
}

Command* Insert::copy() const { return new Insert(*this); }