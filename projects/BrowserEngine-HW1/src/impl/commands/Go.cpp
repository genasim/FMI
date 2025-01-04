#include "commands/Go.h"

#include <iostream>

#include "store/BrowserStore.h"

Go::Go(const std::string& url) : url(url) {}

void Go::execute() const noexcept {
    auto store = BrowserStore::instance();
    store->goTo(url);
}

Command* Go::copy() const { return new Go(*this); }