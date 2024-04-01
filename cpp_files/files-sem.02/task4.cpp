#include <iostream>
#include <fstream>

bool isUpperCase(const char ch) {
    return ch >= 'A' && ch <= 'Z';
}

char toUpper(const char ch) {
    return isUpperCase(ch) ? ch : ch - 32;
}



void iterateFile(const char file[]) {
    // std::ifstream input(file);
    std::ofstream output(file);


    // if (!input.is_open())
    //     throw std::runtime_error("Couldn't open file");

    std::cout << "Hello Wordl" << std::endl;
    // bool shouldCapitalize = true;
    // while (true)
    // {
    //     char ch = input.get();
    //     if (input.eof())
    //         break;

    //     std::cout << ch;
    // }
}

int main() {
    iterateFile("../files-sem.02/input.txt");
    
    return 0;
}