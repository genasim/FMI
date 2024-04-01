#include <cstring>
#include <fstream>
#include <iostream>

struct Person {
   private:
    char name[30];
    unsigned age;

   public:
    Person() : name{""}, age{0} {};
    Person(const char name[30], const unsigned age) : age(age) {
        strncpy(this->name, name, 29);
        this->name[29] = '\0';
    };

    void writeText(std::ostream &output) const {
        output << name << ", " << age << " years old" << std::endl;
    };

    void writeBinary(std::ostream &output) const {
        output.write((const char *)this, sizeof(Person));
    };

    void readBinary(std::istream &input) {
        input.read((char *)this, sizeof(Person));
    };
};

void writePeopleBinary(const Person people[], const size_t size,
                       std::ostream &output) {
    output.write((const char *)&size, sizeof(size));
    for (size_t i = 0; i < size; i++) {
        people[i].writeBinary(output);
    }
    output.clear();
    output.seekp(0, std::ios::beg);
}

void readPeopleBinary(std::ifstream &input, Person *&arr, size_t &size) {
    input.read((char *)&size, sizeof(size));
    arr = new Person[size];
    for (size_t i = 0; i < size; i++) {
        arr[i].readBinary(input);
    }
    input.clear();
    input.seekg(0, std::ios::beg);
}

void writePeople(const Person people[], const size_t size,
                 std::ostream &output) {
    for (size_t i = 0; i < size; i++) people[i].writeText(std::cout);
}

int main() {
    Person people[] = {
        Person("David Gueta", 43),         Person("Jack Jones", 10),
        Person("Mile Kitic", 9999),        Person("Aziz", 30),
        Person("Sir Anthony Hopkins", 30), Person("Galena", 35),
    };
    size_t size = sizeof(people) / sizeof(people[0]);

    writePeople(people, size, std::cout);

    std::ofstream peopleBin("../practice/tmp/peopleBin.dat",
                            std::ios::binary | std::ios::trunc);
    if (!peopleBin.is_open())
        throw std::runtime_error("Could not open output bin file");

    writePeopleBinary(people, size, peopleBin);

    /**
     *
     *      /\              Writing to the binary file                /\
     *      ||                                                        ||
     */
    std::cout << "=======================" << std::endl;
    /**
     *      ||                                                        ||
     *      \/  Reading from the binary file and re-initing the data  \/
     *
     */

    std::ifstream peopleFile("../practice/tmp/peopleBin.dat", std::ios::binary);
    if (!peopleFile.is_open())
        throw std::runtime_error("Could not open input bin file");

    Person *newArr;
    size_t newSize;
    readPeopleBinary(peopleFile, newArr, newSize);
    writePeople(newArr, newSize, std::cout);

    delete[] newArr;
    return 0;
}