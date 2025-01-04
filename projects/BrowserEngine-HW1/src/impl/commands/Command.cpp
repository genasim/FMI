#include "commands/Command.h"

#include <cstring>
#include <iostream>

#include "commands/Back.h"
#include "commands/Forward.h"
#include "commands/Go.h"
#include "commands/Help.h"
#include "commands/Insert.h"
#include "commands/Print.h"
#include "commands/Remove.h"
#include "commands/Sort.h"

std::string toLower(const std::string& str) {
    std::string result = str;
    for (char& c : result) {
        c = std::tolower(c);
    }
    return result;
}

Command::CommandData Command::factory(std::istream& in) {
    std::string input;
    in >> input;
    auto text = toLower(input);

    if (text == "exit") {
        return {nullptr, true};
    }

    if (text == "help") {
        return {new Help(), false};
    }

    if (text == "insert") {
        std::string url;
        in >> url;

        return {new Insert(url), false};
    }

    if (text == "back") {
        return {new Back(), false};
    }

    if (text == "forward") {
        return {new Forward(), false};
    }

    if (text == "remove") {
        return {new Remove(), false};
    }

    if (text == "print") {
        return {new Print(), false};
    }

    if (text == "sort") {
        std::string fieldString;
        in >> fieldString;
        SortField field;

        if (fieldString == "url") {
            field = SortField::URL;
        } else if (fieldString == "time") {
            field = SortField::TIME;
        } else {
            throw std::invalid_argument("Invalid field for sorting: " +
                                        fieldString);
        }

        return {new Sort(field), false};
    }

    if (text == "go") {
        std::string url;
        in >> url;

        return {new Go(url), false};
    }

    std::cerr << "Unknown command: " << input << std::endl;
    return {nullptr, false};
}