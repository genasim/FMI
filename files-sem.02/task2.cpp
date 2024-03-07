#include <iostream>
#include <fstream>

using namespace std;

bool isLowerCase(const char ch) {
    return ch >= 'a' &&  ch <= 'z';
}

bool isUpperCase(const char ch) {
    return ch >= 'A' &&  ch <= 'Z';
}

bool isNumber(const char ch) {
    return ch >= '0' && ch <= '9';
}

struct Counts {
    unsigned lowerCase;
    unsigned upperCase;
    unsigned digits;
    unsigned rest;
};

ifstream* readFile(const char* const &path) {
    ifstream* file = new ifstream(path);

    if (!file->is_open() || file->bad()) {
        delete file;
        return nullptr;
    }

    return file;
}

Counts countCharacters(ifstream* const &file) {
    Counts result{0, 0, 0, 0};

    while (true)
    {
        char ch = file->get();
        if (file->eof())
            break;

        if (isLowerCase(ch)) {
            result.lowerCase++;
            continue;
        }

        if (isUpperCase(ch)) {
            result.upperCase++;
            continue;
        }

        if (isNumber(ch)) {
            result.digits++;
            continue;
        }

        result.rest++;
    }

    return result;
}

int main() {
    ifstream* input = readFile("../files-sem.02/task2.cpp");
    if (input == nullptr) 
        throw runtime_error("Input file could not be opened");

    Counts results = countCharacters(input);
    cout << results.lowerCase << " " 
         << results.upperCase << " " 
         << results.digits  << " " 
         << results.rest << endl;  

    input->close();
    delete input;

    return 0;
}