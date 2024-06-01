#include "MinimumFunction.h"

#include <cstring>
#include <exception>
#include <sstream>

#include "FunctionSerializer.h"
#include "utils/String.h"

MinimumFunction::MinimumFunction() : functions() {}

MinimumFunction::MinimumFunction(Vector<PartialFunction*>&& funcs)
    : functions(std::move(funcs)) {}

MinimumFunction::MinimumFunction(const MinimumFunction& other) { _copy(other); }

MinimumFunction& MinimumFunction::operator=(const MinimumFunction& other) {
    if (this != &other) {
        _free();
        _copy(other);
    }
    return *this;
}

MinimumFunction::MinimumFunction(MinimumFunction&& other) {
    _move(std::move(other));
}

MinimumFunction& MinimumFunction::operator=(MinimumFunction&& other) {
    if (this != &other) {
        _free();
        _move(std::move(other));
    }
    return *this;
}

bool MinimumFunction::defined(int x) const noexcept {
    for (auto&& f : *this) {
        if (!f->defined(x)) {
            return false;
        }
    }
    return true;
}

int MinimumFunction::compute(int x) const {
    if (functions.size() == 0) {
        throw std::runtime_error(
            "MinimumFunction::compute(int) -> Cannot compute minimum of 0 "
            "functions");
    }

    if (!defined(x)) {
        std::ostringstream oss;
        oss << "MinimumFunction::compute(int x) -> function is undefined for "
            << x;
        throw std::runtime_error(oss.str().c_str());
    }

    int min = functions[0]->compute(x);
    for (auto&& f : functions) {
        int result = f->compute(x);
        min = std::min(min, result);
    }
    return min;
}

PartialFunction* MinimumFunction::clone() const {
    return new MinimumFunction(*this);
}

void MinimumFunction::serialize(std::ostream& out) const noexcept {
    static int id = 0;
    int T = 4;
    int N = functions.size();

    out.write((const char*)&T, sizeof(T));
    out.write((const char*)&N, sizeof(N));

    for (auto&& function : *this) {
        std::ostringstream oss;
        oss << "src/assets/min-func-" << ++id << ".dat";
        auto name = oss.str();

        std::ofstream o(name.c_str(), std::ios::binary);
        if (!o.is_open()) continue;

        function->serialize(o);

        size_t length = strlen(name.c_str()) + 1;
        out.write(name.c_str(), length);
    }
}

void MinimumFunction::deserialize(std::istream& in) noexcept {
    int N;
    in.read((char*)&N, sizeof(N));

    functions.reserve(N);
    for (int i = 0; i < N; i++) {
        String path;
        while (true) {
            char ch = in.get();
            if (in.eof() || ch == '\0') break;
            path.push_back(ch);
        }

        PartialFunction* func;
        FunctionSerializer::deserialize(func, path.c_str());

        functions.push_back(func);
    }
}

void MinimumFunction::add(PartialFunction* func) { functions.push_back(func); }

Vector<PartialFunction*>::iterator MinimumFunction::begin() const noexcept {
    return functions.begin();
}

Vector<PartialFunction*>::iterator MinimumFunction::end() const noexcept {
    return functions.end();
}

void MinimumFunction::_free() {
    for (auto&& f : *this) {
        delete f;
    }
    functions.clear();
}

void MinimumFunction::_copy(const MinimumFunction& other) {
    functions.resize(other.functions.size());
    for (size_t i = 0; i < functions.size(); i++) {
        functions[i] = other.functions[i]->clone();
    }
}

void MinimumFunction::_move(const MinimumFunction& other) {
    functions = std::move(other.functions);
}
