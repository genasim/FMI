#pragma once

#include <functional>

class BrowserEngine {
   public:
    static BrowserEngine* init(std::function<int()> callback) noexcept;
    void start() noexcept;

    BrowserEngine(const BrowserEngine&) = delete;
    BrowserEngine& operator=(const BrowserEngine&) = delete;
    BrowserEngine(BrowserEngine&&) = delete;
    BrowserEngine& operator=(BrowserEngine&&) = delete;

    ~BrowserEngine() = default;

   private:
    BrowserEngine(std::function<int()> callback) noexcept;
    std::function<int()> _callback;

    void _clearScreen() const noexcept;
};
