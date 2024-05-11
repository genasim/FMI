#pragma once

#include "Shape.h"
#include "data/Vector.h"

class Rect : public Shape {
   public:
    Rect();
    Rect(Point, Point);

    double getArea() const noexcept override;
    double getPerimeter() const noexcept override;

    void serialize(std::ostream&) const noexcept override;
    void deserialize(std::istream&) noexcept override;
    
    Shape* copy() const override;

   private:
    void _fillPoints(Point, Point);
    Vector<Point> _points;
};