#pragma once

#include "PartialFunction.h"
#include "utils/Vector.h"

class MinimumFunction : public PartialFunction {
   public:
    MinimumFunction();
    MinimumFunction(Vector<PartialFunction*>&& funcs);

    MinimumFunction(const MinimumFunction& other);
    MinimumFunction& operator=(const MinimumFunction& other);
    MinimumFunction(MinimumFunction&& other);
    MinimumFunction& operator=(MinimumFunction&& other);

    bool defined(int x) const noexcept override;
    int compute(int x) const override;

    PartialFunction* clone() const override;

    void serialize(std::ostream& out) const noexcept override;
    void deserialize(std::istream& in) noexcept override;

    void add(PartialFunction* func);
    Vector<PartialFunction*>::iterator begin() const noexcept;
    Vector<PartialFunction*>::iterator end() const noexcept;

   private:
    Vector<PartialFunction*> functions;

    void _free();
    void _copy(const MinimumFunction& other);
    void _move(const MinimumFunction& other);
};