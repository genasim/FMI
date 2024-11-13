#include "commands/Print.h"

#include <iostream>
#include "store/BrowserStore.h"

void Print::execute() const noexcept {
    auto store = BrowserStore::instance();
    store->print();
}

Command* Print::copy() const { return new Print(*this); }