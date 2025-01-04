#pragma once

#include <string>

#include "commands/Command.h"

class Go : public Command {
   public:
    explicit Go(const std::string& url);

    void execute() const noexcept override;

    Command* copy() const override;

   private:
    std::string url;
};