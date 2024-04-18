#include <cstring>
#include <iostream>

typedef int16_t (*FunctionPtr)(int16_t);

class ModifiableIntegerFunction {
   public:
    ModifiableIntegerFunction(FunctionPtr func);
    void setOutput(int16_t input, int16_t output);
    void excludeInput(int16_t input);
    int16_t evaluate(int16_t input);

   private:
    FunctionPtr function;
    int16_t specialOutputs[65536];
    bool isDefined[65536];
};