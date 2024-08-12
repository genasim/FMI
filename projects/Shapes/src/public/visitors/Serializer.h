#pragma once

#include <ostream>

#include "visitors/Visitor.h"

class Serializer : public Visitor {
    public:
    Serializer(std::ostream& _out);

    void visit(Circle& c) noexcept override;
    void visit(Rect& r) noexcept override;
    void visit(Triangle& t) noexcept override;
    void visit(Group& g) noexcept override;

    private:
    std::ostream& out;
};
