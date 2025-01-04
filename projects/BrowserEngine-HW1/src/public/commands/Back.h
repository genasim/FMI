#pragma once

#include "commands/Command.h"

class Back : public Command {
   public:
    void execute() const noexcept override;

    Command* copy() const override;
};