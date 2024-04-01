#include <cstring>
#include <fstream>
#include <iostream>
#include <foo.h>

struct Person {
   private:
    char *name;
    unsigned age;

   public:
    Person() : age{0} {
        name = new char[1];
        name[0] = '\0';
    };
    Person(const char name[], const unsigned age) : age(age) {
        size_t nameSize = strlen(name);
        this->name = new char[nameSize + 1];

        strncpy(this->name, name, nameSize);
        this->name[nameSize] = '\0';
    }
    ~Person() { delete[] name; }

    void serializeText(std::ostream &output) const {
        output << name << ", " << age << " years old" << std::endl;
    }

    void serialize(std::ostream &output) const {
        size_t nameSize = strlen(name);
        output.write((const char *)&nameSize, sizeof(nameSize));
        output.write(name, nameSize);
        output.write((const char *)&age, sizeof(age));
    }

    void deserialize(std::istream &input) {
        size_t nameSize;
        input.read((char *)&nameSize, sizeof(nameSize));

        delete[] name;
        name = new char[nameSize + 1];
        input.read((char *)name, nameSize);
        name[nameSize] = '\0';

        input.read((char *)&age, sizeof(age));
    }
};

void writePeopleBinary(const Person people[], const size_t size,
                       std::ostream &output) {
    output.write((const char *)&size, sizeof(size));
    for (size_t i = 0; i < size; i++) {
        people[i].serialize(output);
    }
    output.clear();
    output.seekp(0, std::ios::beg);
}

void readPeopleBinary(std::ifstream &input, Person *&arr, size_t &size) {
    input.read((char *)&size, sizeof(size));
    arr = new Person[size];
    for (size_t i = 0; i < size; i++) {
        arr[i].deserialize(input);
    }
    input.clear();
}

void writePeople(const Person people[], const size_t size,
                 std::ostream &output) {
    for (size_t i = 0; i < size; i++) people[i].serializeText(output);
}

int main() {
    Person people[] = {
        Person("David Gueta", 43),         Person("Jack Jones", 10),
        Person("Mile Kitic", 9999),        Person("Aziz", 30),
        Person("Sir Anthony Hopkins", 30), Person("Galena", 35),
    };
    size_t size = sizeof(people) / sizeof(people[0]);

    writePeople(people, size, std::cout);

    std::ofstream peopleBin("../practice/tmp/peopleBinHeap.dat",
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

    std::ifstream peopleFile("../practice/tmp/peopleBinHeap.dat",
                             std::ios::binary);
    if (!peopleFile.is_open())
        throw std::runtime_error("Could not open input bin file");

    Person *newArr;
    size_t newSize;
    readPeopleBinary(peopleFile, newArr, newSize);
    writePeople(newArr, newSize, std::cout);

    delete[] newArr;
    return 0;
}