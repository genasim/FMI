#include "ModifiableFunction.h"
#include <iostream>

typedef int16_t (*FunctionPtr)(int16_t);

    ModifiableIntegerFunction:: ModifiableIntegerFunction(FunctionPtr func) : function(func) {
        memset(specialOutputs, 0, sizeof(specialOutputs));
        memset(isDefined, true, sizeof(isDefined));
    }

    void ModifiableIntegerFunction::setOutput(int16_t input, int16_t output) {
        specialOutputs[input] = output;
        isDefined[input] = true;
    }

    void ModifiableIntegerFunction::excludeInput(int16_t input) {
        isDefined[input] = false;
    }

    int16_t ModifiableIntegerFunction::evaluate(int16_t input) {
        if (!isDefined[input]) {
            std::cerr << "Function not defined for input: " << input << std::endl;
            return -1; // Сигнал за грешка
        }
        return specialOutputs[input] ? specialOutputs[input] : function(input);
    }
