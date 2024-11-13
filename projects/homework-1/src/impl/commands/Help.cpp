#include "commands/Help.h"

#include <iostream>

void Help::execute() const noexcept {
    std::cout << "Available commands: (commands are case-insensitive)"
              << std::endl;
    std::cout << "  - exit - close and exit the application" << std::endl;
    std::cout << "  - help - list available commands" << std::endl;
    std::cout << "  - go <url> - load a new url for the current tab" << std::endl;
    std::cout << "  - insert <url> - add a new tab and move to it" << std::endl;
    std::cout << "  - back - go to previous tab" << std::endl;
    std::cout << "  - forward - go to next tab" << std::endl;
    std::cout << "  - remove - remove the current tab" << std::endl;
    std::cout << "  - print - print all tabs" << std::endl;
    std::cout << "  - sort <by> - sort the tabs by either 'url' or 'timestamp'" << std::endl;
}

Command* Help::copy() const { return new Help(*this); }