#pragma once

#include "Shape.h"

class Triangle : public Shape {
   public:
    Triangle();
    Triangle(Point, Point, Point);

    double getArea() const noexcept override;
    double getPerimeter() const noexcept override;

    Vector<Point> points() const noexcept;

    void deserialize(std::istream&) noexcept override;

    void accept(Visitor&) noexcept override;
    Shape* copy() const override;

   private:
    Vector<Point> _points;
};