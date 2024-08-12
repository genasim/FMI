#pragma once

#include <istream>

#include "visitors/Visitor.h"

class Loader : public Visitor {
    public:
    Loader(std::istream& _in);

    void visit(Circle& c) noexcept override;
    void visit(Rect& r) noexcept override;
    void visit(Triangle& t) noexcept override;
    void visit(Group& g) noexcept override;

    private:
    std::istream& in;
};
