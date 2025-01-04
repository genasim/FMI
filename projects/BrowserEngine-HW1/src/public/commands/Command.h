#pragma once

#include <fstream>

class Command {
    struct CommandData;

   public:
    static Command::CommandData factory(std::istream& command);

    virtual void execute() const noexcept = 0;

    virtual ~Command() = default;
    virtual Command* copy() const = 0;

   private:
    struct CommandData {
        Command* command;
        bool shouldExit;
    };
};