#include <iostream>
#include <string>

#include "commands/Command.h"
#include "engine/BrowserEngine.h"

int main() {
    BrowserEngine* engine = BrowserEngine::init([&]() {
        Command* command = nullptr;
        try {
            auto parsedCommand = Command::factory(std::cin);
            if (parsedCommand.shouldExit) return 1;

            command = parsedCommand.command;
            if (!command) return 0;

            command->execute();
            delete command;
        } catch (const std::exception& e) {
            std::cerr << e.what() << std::endl;
            if (command) delete command;
        }

        return 0;
    });

    engine->start();
    delete engine;
}
