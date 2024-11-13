#include "commands/Sort.h"

#include <iostream>

#include "store/BrowserStore.h"

Sort::Sort(SortField field) : field(field) {}

void Sort::execute() const noexcept {
    auto store = BrowserStore::instance();
    store->sortBy(field);
}

Command* Sort::copy() const { return new Sort(*this); }