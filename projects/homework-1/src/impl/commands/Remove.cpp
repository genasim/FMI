#include "commands/Remove.h"

#include <iostream>

#include "store/BrowserStore.h"

void Remove::execute() const noexcept {
    auto store = BrowserStore::instance();
    store->remove();
}

Command* Remove::copy() const { return new Remove(*this); }