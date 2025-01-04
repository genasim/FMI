#include "store/BrowserStore.h"

#include <iostream>

BrowserStore* BrowserStore::_instance = nullptr;

BrowserStore::BrowserStore() { initEmptyStore(); }

void BrowserStore::initEmptyStore() {
    store.emplace_back("about:blank");
    currentTab = store.begin();
}

BrowserStore* BrowserStore::instance() noexcept {
    if (!_instance) {
        _instance = new BrowserStore();
    }

    return _instance;
}

void BrowserStore::destroy() noexcept {
    delete _instance;
    _instance = nullptr;
}

void BrowserStore::print() const noexcept {
    for (const auto& tab : store) {
        if (*currentTab == tab) {
            std::cout << "> ";
        } else {
            std::cout << "  ";
        }
        std::cout << tab << std::endl;
    }
}

void BrowserStore::insert(const std::string& url) {
    currentTab = store.emplace(++currentTab, url);
}

void BrowserStore::remove() {
    if (store.size() == 1) {
        store.pop_back();
        initEmptyStore();
        return;
    }

    currentTab = store.erase(currentTab);
    if (currentTab == store.end()) {
        --currentTab;
    }
}

void BrowserStore::back() {
    if (store.begin() == currentTab) return;
    --currentTab;
}

void BrowserStore::forward() {
    if (--store.end() == currentTab) return;
    ++currentTab;
}

void BrowserStore::goTo(const std::string& url) { (*currentTab).setUrl(url); }

void BrowserStore::sortBy(SortField field) {
    store.sort([&](const Tab& a, const Tab& b) {
        if (field == SortField::URL) {
            return a.url() < b.url();
        }

        if (field == SortField::TIME) {
            return a.timestamp() < b.timestamp();
        }

        return true;
    });
    currentTab = store.begin();
}
