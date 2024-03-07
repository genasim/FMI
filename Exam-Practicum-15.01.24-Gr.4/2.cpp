#include <iostream>

using namespace std;

unsigned getStrLength(char *str) {
    unsigned length = 0;
    while (*str != '\0') {
        length++;
        str++;
    }
    return length;
}

bool isLetter(const char &ch) {
    return (ch > 'a' && ch < 'z') || (ch > 'A' && ch < 'Z');
}

void censor(char &letter) {
    letter = '*';
}

void censorWordIn(char*& str, char* pattern) {
    unsigned length = getStrLength(str);
    
    unsigned index = 0;
    while (*str)
    {
        const char* tempStr = str;
        unsigned currentWordLength = 0;
        while (*tempStr != ' ' || *tempStr != '\0')
        {
            currentWordLength++;
            tempStr++;
        }
        cout << currentWordLength << endl;
        // if (str[index + length] !=)
    }
    
    
}

int main() {
    char* text = "Lorem ipsum dolor,! sit amet, consectetur amigos";
    char* pattern = "dolor";

    censorWordIn(text, pattern);

    return 0;
}