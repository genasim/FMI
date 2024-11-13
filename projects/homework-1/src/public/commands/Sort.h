#pragma once

#include <string>

#include "commands/Command.h"
#include "models/SortField.h"

class Sort : public Command {
   public:
    explicit Sort(SortField field);

    void execute() const noexcept override;

    Command* copy() const override;

   private:
    SortField field;
    std::string fieldStr;
};