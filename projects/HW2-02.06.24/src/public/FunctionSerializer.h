#pragma once

#include "PartialFunction.h"

class FunctionSerializer {
   public:
    static void serialize(const PartialFunction& func, const char* path);
    static void deserialize(PartialFunction*& func, const char* path);
};
