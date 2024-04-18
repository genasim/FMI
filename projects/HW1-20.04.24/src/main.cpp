#include <bitset>
#include <cstdint>
#include <iostream>

#include "Multiset.h"
#include "ModifiableFunction.h"

int16_t multiplyByTwo(int16_t x) {
    return x * 2;
}

int main() {
    // uint8_t array[] = {0b00001000, 0b01101011, 0b10100001};
    // Multiset set(array, 3, 7);
    // std::cout << set << std::endl;

    // set.add(1);
    // set.add(1);

    // Multiset set(3, 10);
    // // std::cout << set << std::endl;

    // set.add(2);
    // set.add(2);
    // set.add(3);
    // set.add(4);
    // set.add(1);
    // set.add(2);
    // set.add(3);
    // set.add(2);

    // set.outputBinary(std::cout);
    // std::cout << set << std::endl;

    // return 0;

    ModifiableIntegerFunction func(multiplyByTwo);
    func.setOutput(3, 10);
    func.excludeInput(5);
    std::cout << "Function at 3: " << func.evaluate(3)
              << std::endl;
    std::cout << "Function at 5: " << func.evaluate(5)
              << std::endl;
}
