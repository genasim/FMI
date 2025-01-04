#include "commands/Back.h"

#include <iostream>

#include "store/BrowserStore.h"

void Back::execute() const noexcept {
    auto store = BrowserStore::instance();
    store->back();
}

Command* Back::copy() const { return new Back(*this); }