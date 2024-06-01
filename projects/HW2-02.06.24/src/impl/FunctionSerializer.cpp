#include "FunctionSerializer.h"

#include <cstring>
#include <exception>
#include <filesystem>  // C++17, for checking and creating directories
#include <fstream>
#include <iostream>
#include <sstream>

#include "utils/String.h"

void FunctionSerializer::serialize(const PartialFunction& func,
                                   const char* path) {
    std::ofstream out(path, std::ios::binary);
    if (!out.is_open()) {
        std::ostringstream oss;
        oss << "Could not open file: " << path;
        auto error = oss.str();
        std::cerr << error << std::endl;
        throw std::runtime_error(error);
    }

    func.serialize(out);
}

void FunctionSerializer::deserialize(PartialFunction*& func, const char* path) {
    std::ifstream in(path, std::ios::binary);
    if (!in.is_open()) {
        std::ostringstream oss;
        oss << "Could not open file: " << path;
        auto error = oss.str();
        std::cerr << error << std::endl;
        throw std::runtime_error(error);
    }
    int T;
    in.read((char*)&T, sizeof(T));
    func = PartialFunction::factory(T);

    func->deserialize(in);
}
