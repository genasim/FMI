#pragma once

#include "Shape.h"

class Circle : public Shape {
   public:
    Circle();
    Circle(Point, double);

    Point center() const noexcept;
    double radius() const noexcept;

    double getArea() const noexcept override;
    double getPerimeter() const noexcept override;

    void deserialize(std::istream&) noexcept override;

    void accept(Visitor&) noexcept override;
    Shape* copy() const override;

   private:
    static const double PI;

    Point _center;
    double _radius;
};