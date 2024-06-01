#include <iostream>
#include <sstream>

#include "BinaryCriteria.h"
#include "CriteriaFunction.h"
#include "ExclusiveIdCriteria.h"
#include "FiniteCriteria.h"
#include "FunctionSerializer.h"
#include "MaximumFunction.h"
#include "MinimumFunction.h"
#include "utils/String.h"

void initMaximumFunctionFiles() {
    FiniteCriteria finCrit({0, 1, 2, 3, 5, 6, 7}, {0, 3, 3, 3, 4, 4, 0});
    PartialFunction* finiteFunc = new CriteriaFunction(&finCrit);

    ExclusiveIdCriteria exclCrit({5, 3});
    PartialFunction* exclusiveFunc = new CriteriaFunction(&exclCrit);

    BinaryCriteria binCrit({0, 5, 6, 7});
    PartialFunction* binaryFunc = new CriteriaFunction(&binCrit);

    PartialFunction* func =
        new MaximumFunction({finiteFunc, exclusiveFunc, binaryFunc});
    FunctionSerializer::serialize(*func, "src/assets/func-max.dat");

    delete func;
}

void initMinimumFunctionFiles() {
    BinaryCriteria crit1({-4, 1, 2, 0, 5, 12, -8});
    PartialFunction* func1 = new CriteriaFunction(&crit1);

    ExclusiveIdCriteria crit2({5, 3, 2, 0});
    PartialFunction* func2 = new CriteriaFunction(&crit2);

    BinaryCriteria binCrit({0, 5, 3, 6, 7});
    PartialFunction* binaryFunc = new CriteriaFunction(&binCrit);

    FiniteCriteria finiteCrit({0, 1, 2, 3, 5, 6, 7}, {-8, 3, 12, 3, 9, 3, 2});
    PartialFunction* finiteFunc = new CriteriaFunction(&finiteCrit);

    PartialFunction* func3 = new MaximumFunction({binaryFunc, finiteFunc});

    BinaryCriteria crit4({6, 4});
    PartialFunction* func4 = new CriteriaFunction(&crit4);

    PartialFunction* func = new MinimumFunction({func1, func2, func3, func4});
    FunctionSerializer::serialize(*func, "src/assets/func-min.dat");

    delete func;
}

void intervalMode(const PartialFunction& func) {
    int a, b;
    std::cout << "a = ";
    std::cin >> a;
    std::cout << "b = ";
    std::cin >> b;

    for (int i = a; i <= b; i++) {
        try {
            int result = func.compute(i);
            std::cout << "F(" << i << ")=" << result << "  ";
        } catch (const std::exception& e) {
            // std::cerr << std::endl << e.what() << std::endl;
        }
    }
    std::cout << std::endl;
}

void incrementMode(const PartialFunction& func) {
    for (int i = -5000; i <= 5000; ++i) {
        try {
            int result = func.compute(i);
            std::cout << "F(" << i << ")=" << result << "  next(y):";

            String input;
            std::cin >> input;
            if (input == "y") {
                continue;
            }
            break;
        } catch (std::exception& e) {}
    }
}

int main() {
    try {
        std::cout
            << "Initing maximum function example .dat files in src/assets ... ";
        initMaximumFunctionFiles();
        std::cout << "finished" << std::endl;

        std::cout
            << "Initing minimum function example .dat files in src/assets ... ";
        initMinimumFunctionFiles();
        std::cout << "finished" << std::endl;
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
    }

    std::cout << std::endl << "Enter file name in src/assets/ dir: ";
    String name;
    std::cin >> name;

    std::ostringstream oss;
    oss << "src/assets/" << name;
    auto path = oss.str();

    PartialFunction* func = nullptr;
    try {
        FunctionSerializer::deserialize(func, path.c_str());
        std::cout << "Successfully opened file at: " << path << std::endl;
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }

    std::cout << std::endl << "Available commands: interval, step, exit";
    String command = "";
    while (true) {
        std::cout << std::endl << "Enter command: ";
        std::cin >> command;

        if (command == "interval") {
            intervalMode(*func);
            continue;
        }

        if (command == "step") {
            incrementMode(*func);
            continue;
        }

        if (command == "exit") break;
    }

    delete func;
    return 0;
}
