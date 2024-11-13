#pragma once

#include <functional>

#include "containers/list.hpp"
#include "models/SortField.h"
#include "store/Tab.h"

class BrowserStore {
   public:
    static BrowserStore* instance() noexcept;
    static void destroy() noexcept;

    ~BrowserStore() = default;

    void print() const noexcept;
    void insert(const std::string& url);
    void remove();
    void back();
    void forward();
    void goTo(const std::string& url);
    void sortBy(SortField field);

   private:
    static BrowserStore* _instance;

    list<Tab>::iterator currentTab = store.begin();
    list<Tab> store;

    BrowserStore();

    void initEmptyStore();

    BrowserStore(const BrowserStore&) = delete;
    BrowserStore& operator=(const BrowserStore&) = delete;
    BrowserStore(BrowserStore&&) = delete;
    BrowserStore& operator=(BrowserStore&&) = delete;
};