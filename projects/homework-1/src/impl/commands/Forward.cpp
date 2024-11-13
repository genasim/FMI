#include "commands/Forward.h"

#include <iostream>

#include "store/BrowserStore.h"

void Forward::execute() const noexcept {
    auto store = BrowserStore::instance();
    store->forward();
}

Command* Forward::copy() const { return new Forward(*this); }