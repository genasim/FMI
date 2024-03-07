#include <iostream>

using namespace std;

// ##  Utilities ##

struct StringPair {
    const char *text;
    const char *pattern;
};

bool isSpecialCharacter(const char &item) {
    return item == '*' || item == '%' || item == '@';
}

bool isLetter(const char &item) {
    return (item >= 'a' && item <= 'z') || (item >= 'A' && item <= 'Z');
}

bool isNumber(const char &item) {
    return item >= '0' && item <= '9';
}

// ##  Actual program ##

bool textIsValid(const char *text) {
    while (*text != '\0') {
        bool letter = true, digit = true;
        if (!isLetter(*text))
            letter = false;
        if (!isNumber(*text))
            digit = false;

        if (!letter && !digit && *text != ' ')
            return false;

        text++;
    }
    return true;
}

bool handleSpecialCharacters(const char *text, const char &symbol, size_t *offset) {
    switch (symbol) {
        case '*':
            *offset = 1;
            return true;
        case '%':
            if (!isNumber(*text))
                return false;

            *offset = isNumber(*(text + 1)) && *(text + 2) != '\0' ? 2 : 1;
            return true;
        case '@':
            *offset = 1;
            return isLetter(*text);
        default:
            *offset = 1;
            return true;
    }
}

size_t countOccurrences(const char *mainStr, const char *substr) {
    size_t count = 0;

    // Iterate through the main string
    while (*mainStr) {
        const char *tempMainStr = mainStr;
        const char *tempSubStr = substr;

        // Check if the substring is present starting from the current position
        while (*tempMainStr &&
               *tempSubStr &&
               (*tempMainStr == *tempSubStr || isSpecialCharacter(*tempSubStr))
                ) {
            size_t offset = 0;
            if (handleSpecialCharacters(tempMainStr, *tempSubStr, &offset))
                ++tempSubStr;
            else
                break;
            tempMainStr += offset;
        }

        // If the substring is found, increment the count
        if (!*tempSubStr) {
            ++count;
        }

        // Move to the next character in the main string
        ++mainStr;
    }

    return count;
}

int main() {
    StringPair pairs[] = {
            {"aaaaaa",             "aa"},
            {"aaaaaa",             "a@"},
            {"123",                "%%"},
            {"te3t zdrte44q t33t", "t*%@"},
    };
    for (const StringPair pair: pairs) {
        if (!textIsValid(pair.text)) {
            cout << "Input text must consist of only a-z, A-Z, 0-9 and exclude *,%,@" << endl;
            continue;
        }

        size_t result = countOccurrences(pair.text, pair.pattern);
        cout << pair.text << " | " << pair.pattern << " --> " << result << endl;
    }

    return 0;
}
