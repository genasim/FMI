#pragma once

#include <string>

#include "commands/Command.h"

class Insert : public Command {
   public:
    explicit Insert(const std::string& url);

    void execute() const noexcept override;

    Command* copy() const override;

   private:
    std::string url;
};