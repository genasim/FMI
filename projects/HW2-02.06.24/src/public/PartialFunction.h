#pragma once
#include <iostream>

class PartialFunction {
   public:
    static PartialFunction* factory(int code);

    virtual bool defined(int x) const noexcept = 0;
    virtual int compute(int x) const = 0;

    virtual PartialFunction* clone() const = 0;

    virtual void serialize(std::ostream& out) const noexcept = 0;
    virtual void deserialize(std::istream& in) noexcept = 0;

    virtual ~PartialFunction() = default;
};