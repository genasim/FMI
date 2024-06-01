#pragma once

#include "PartialFunction.h"
#include "utils/Vector.h"

class MaximumFunction : public PartialFunction {
   public:
    MaximumFunction();
    MaximumFunction(Vector<PartialFunction*>&& funcs);

    MaximumFunction(const MaximumFunction& other);
    MaximumFunction& operator=(const MaximumFunction& other);
    MaximumFunction(MaximumFunction&& other);
    MaximumFunction& operator=(MaximumFunction&& other);

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
    void _copy(const MaximumFunction& other);
    void _move(const MaximumFunction& other);
};