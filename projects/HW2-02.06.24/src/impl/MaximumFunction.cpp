#include "MaximumFunction.h"

#include <cstring>
#include <exception>
#include <sstream>

#include "FunctionSerializer.h"
#include "utils/String.h"

MaximumFunction::MaximumFunction() : functions() {}

MaximumFunction::MaximumFunction(Vector<PartialFunction*>&& funcs)
    : functions(std::move(funcs)) {}

MaximumFunction::MaximumFunction(const MaximumFunction& other) { _copy(other); }

MaximumFunction& MaximumFunction::operator=(const MaximumFunction& other) {
    if (this != &other) {
        _free();
        _copy(other);
    }
    return *this;
}

MaximumFunction::MaximumFunction(MaximumFunction&& other) {
    _move(std::move(other));
}

MaximumFunction& MaximumFunction::operator=(MaximumFunction&& other) {
    if (this != &other) {
        _free();
        _move(std::move(other));
    }
    return *this;
}

bool MaximumFunction::defined(int x) const noexcept {
    for (auto&& f : *this) {
        if (!f->defined(x)) {
            return false;
        }
    }
    return true;
}

int MaximumFunction::compute(int x) const {
    if (functions.size() == 0) {
        throw std::runtime_error(
            "MaximumFunction::compute(int) -> Cannot compute maximum of 0 "
            "functions");
    }

    if (!defined(x)) {
        std::ostringstream oss;
        oss << "MaximumFunction::compute(int x) -> function is undefined for "
            << x;
        throw std::runtime_error(oss.str().c_str());
    }

    int max = functions[0]->compute(x);
    for (auto&& f : functions) {
        int result = f->compute(x);
        max = std::max(max, result);
    }
    return max;
}

PartialFunction* MaximumFunction::clone() const {
    return new MaximumFunction(*this);
}

void MaximumFunction::serialize(std::ostream& out) const noexcept {
    static int id = 0;
    int T = 3;
    int N = functions.size();

    out.write((const char*)&T, sizeof(T));
    out.write((const char*)&N, sizeof(N));

    for (auto&& function : *this) {
        std::ostringstream oss;
        oss << "src/assets/max-func-" << ++id << ".dat";
        auto name = oss.str();

        std::ofstream o(name.c_str(), std::ios::binary);
        if (!o.is_open()) continue;

        function->serialize(o);

        size_t length = strlen(name.c_str()) + 1;
        out.write(name.c_str(), length);
    }
}

void MaximumFunction::deserialize(std::istream& in) noexcept {
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

void MaximumFunction::add(PartialFunction* func) { functions.push_back(func); }

Vector<PartialFunction*>::iterator MaximumFunction::begin() const noexcept {
    return functions.begin();
}

Vector<PartialFunction*>::iterator MaximumFunction::end() const noexcept {
    return functions.end();
}

void MaximumFunction::_free() {
    for (auto&& f : *this) {
        delete f;
    }
    functions.clear();
}

void MaximumFunction::_copy(const MaximumFunction& other) {
    functions.resize(other.functions.size());
    for (size_t i = 0; i < functions.size(); i++) {
        functions[i] = other.functions[i]->clone();
    }
}

void MaximumFunction::_move(const MaximumFunction& other) {
    functions = std::move(other.functions);
}
