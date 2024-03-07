#include <iostream>
#include <cstring>

using namespace std;

bool isPunctChar(const char &ch) {
    return ch == ' ' || ch == '!' || ch == '.' || ch == '?';
}

bool isLowerCase(const char &ch) {
    return ch >= 'a' && ch <= 'z';
}

bool isUpperCase(const char &ch) {
    return ch >= 'A' && ch <= 'Z';
}

bool isLetter(const char &ch) {
    return isLowerCase(ch) || isUpperCase(ch);
}

char toLower(const char &ch) {
    if (!isLetter(ch))
        throw runtime_error("Supplied char was not a Latin letter");
    return isLowerCase(ch) ? ch : ch + 32;
}

char toUpper(const char &ch) {
    if (!isLetter(ch))
        throw runtime_error("Supplied char was not a Latin letter");
    return isUpperCase(ch) ? ch : ch - 32;
}

char* toLower(const char string[]) {
    char* result = new char[0];
    strcpy(result, string);

    for (size_t i = 0; result[i] != '\0'; i++) {
        if (isPunctChar(result[i]))
            continue;
        result[i] = toLower(result[i]);
    }
    return result;
}

char* toUpper(const char string[]) {
    char* result = new char[0];
    strcpy(result, string);

    for (size_t i = 0; result[i] != '\0'; i++) {
        if (isPunctChar(result[i]))
            continue;
        result[i] = toUpper(result[i]);
    }
    return result;
}

int main() {
    cout << "X -> " << toLower('X') << " | " << "t -> " << toUpper('t') << endl << endl;

    char text[] = "HeLLo WOrlD! Love YoU";
    
    char* upper = toLower(text);
    char* lower = toUpper(text);

    cout << text << endl;
    cout << upper << endl;
    cout << lower << endl;


    delete[] upper;
    delete[] lower;

    return 0;
}