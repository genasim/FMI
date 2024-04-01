#include <fstream>
#include <iostream>

class FileHandler {
   private:
    std::ifstream* file;
    void checkFile() {
        if (file == nullptr) throw std::runtime_error("File is not open");
    }

   public:
    FileHandler(const char* path) {
        file = new std::ifstream(path);

        if (!file->is_open() || file->bad()) file = nullptr;
    }
    ~FileHandler() {
        if (file == nullptr) return;
        file->close();
        delete file;
    }

    void writeTo(std::ostream& output) {
        checkFile();

        while (true) {
            char ch = file->get();
            if (file->eof() || file->fail()) break;
            output << ch;
        }
        output << std::endl;

        file->clear();
        file->seekg(std::ios::beg);
    }
};

int main() {
    FileHandler handler("../practice/files/printFileContent.cpp");
    handler.writeTo(std::cout);

    std::ofstream copyFile("../practice/tmp/copy.cpp");
    handler.writeTo(copyFile);

    return 0;
}
