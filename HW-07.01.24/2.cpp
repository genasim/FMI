#include <iostream>

using namespace std;

// ##  Utilities ##

struct StringPair {
    const char *first;
    const char *second;
};

void swap(char &a, char &b) {
    char temp = a;
    a = b;
    b = temp;
}

size_t getStrLength(const char *const str) {
    size_t length = 0;
    while (str[length] != '\0') {
        length++;
    }
    return length;
}

void copyStr(const char *src, char *dest) {
    size_t srcSize = getStrLength(src);
    for (size_t i = 0; i < srcSize; i++)
        dest[i] = src[i];
}

bool isOnlyLowerCaseLatin(const char *str) {
    while (*str != '\0') {
        if (*str < 'a' || *str > 'z')
            return false;
        str++;
    }
    return true;
}

// ##  Actual program ##

char *concatStrings(const char *first, const char *second) {
    size_t lenFirst = getStrLength(first);
    size_t lenSecond = getStrLength(second);
    size_t size = lenFirst + lenSecond;
    char *concat = new char[size + 1];

    for (size_t i = 0; i < lenFirst; i++)
        concat[i] = first[i];

    for (size_t i = lenFirst; i < size; i++)
        concat[i] = second[i - lenFirst];

    return concat;
}

bool isPalindrome(const char *str, size_t length) {
    for (size_t i = 0; i < length / 2; ++i) {
        if (str[i] != str[length - 1 - i]) {
            return false;
        }
    }
    return true;
}

bool getFirstPermutation(char *str, size_t start, size_t end, char *result) {
    // If start and end are the same, we have reached the end of a permutation
    if (start == end) {
        copyStr(str, result);
        return isPalindrome(result, end + 1);
    }

    // Recursively generate permutations by fixing one character at a time
    for (size_t i = start; i <= end; ++i) {
        // Swap characters at positions start and i
        swap(str[start], str[i]);

        // Recursively generate permutations for the remaining characters
        if (getFirstPermutation(str, start + 1, end, result)) {
            return true;  // Return true if a palindrome is found
        }

        // Undo the swap to backtrack and explore other possibilities
        swap(str[start], str[i]);
    }

    return false;  // Return false if no palindrome is found
}

char *generatePalindrome(const char *first, const char *second) {
    char *concat = concatStrings(first, second);
    size_t length = getStrLength(concat);
    char result[length];

    return getFirstPermutation(concat, 0, length - 1, result) ? result : nullptr;
}

int main() {
    StringPair pairs[] = {
            {"hello", "eh"},
            {"w",     "ggtw"},
            {"abc",   "abd"},
            {"heLLo", "eh"},
            {"hhoo",  "66"},
    };
    for (const StringPair pair: pairs) {
        if (!isOnlyLowerCaseLatin(pair.first) || !isOnlyLowerCaseLatin(pair.second)) {
            cout << "Both inputs should consist of lower case latin characters" << endl;
            continue;
        }

        char *result = generatePalindrome(pair.first, pair.second);

        if (result == nullptr)
            cout << "No palindrome can be formed for " << pair.first << " and " << pair.second << endl;
        else
            cout << "First palindrome for " << pair.first << " and " << pair.second << " -> " << result << endl;
    }

    return 0;
}
