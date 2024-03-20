#include <fstream>
#include <iostream>

std::streamsize fileSize(std::istream &file) {
    std::streampos currPos = file.tellg();

    file.seekg(0, std::ios::end);
    std::streamsize fileSize = file.tellg();
    file.seekg(currPos);

    return fileSize;
}

int main() {
    std::ifstream file("../practice/files/fileSize.cpp");
    if (!file.is_open()) throw std::runtime_error("Could not open file");

    std::cout << fileSize(file) << std::endl;

    return 0;
}