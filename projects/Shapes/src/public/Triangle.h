#pragma once
#include "Shape.h"

class Triangle : public Shape
{
public:
    Triangle();
    Triangle(Point, Point, Point);

    double getArea() const noexcept override;
    double getPerimeter() const noexcept override;

    void serialize(std::ostream&) const noexcept override;
    void deserialize(std::istream&) noexcept override;

    Shape* copy() const override;
   
   private:
    Vector<Point> _points;
};