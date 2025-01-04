#include "engine/BrowserEngine.h"

#include <stdlib.h>

#include <iostream>

BrowserEngine::BrowserEngine(std::function<int()> callback) noexcept
    : _callback(callback) {}

BrowserEngine* BrowserEngine::init(std::function<int()> callback) noexcept {
    return new BrowserEngine(callback);
}

void BrowserEngine::start() noexcept {
    std::cout << "To list available commands type 'help'" << std::endl;

    int status = 0;
    while (true) {
        std::cout << "$: ";
        status = _callback();
        if (status != 0) {
            break;
        }

        std::cin.clear();
        std::cin.ignore(INT32_MAX, '\n');
    }
    std::cout << "Engine closes with status " << status << " ..." << std::endl;
}

void BrowserEngine::_clearScreen() const noexcept {
#if __linux__ || __APPLE__
    system("clear");
#elif _WIN32
    system("cls");
#endif
}