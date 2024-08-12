#pragma once

#include "Circle.h"
#include "Group.h"
#include "Rect.h"
#include "Triangle.h"

class Visitor {
   public:
    virtual void visit(Circle&) noexcept = 0;
    virtual void visit(Rect&) noexcept = 0;
    virtual void visit(Triangle&) noexcept = 0;
    virtual void visit(Group&) noexcept = 0;

    virtual ~Visitor() = default;
};