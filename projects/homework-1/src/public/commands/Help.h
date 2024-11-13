#pragma once

#include "commands/Command.h"

class Help : public Command {
public:
    void execute() const noexcept override;

    Command* copy() const override;
};