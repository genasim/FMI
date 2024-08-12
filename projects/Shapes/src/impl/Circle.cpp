#include "Circle.h"

#include "visitors/Visitor.h"

const double Circle::PI = 3.14;

Circle::Circle() : Circle({0, 0}, 1) {}

Circle::Circle(Point center, double radius)
    : _center(center), _radius(radius) {}

Point Circle::center() const noexcept { return _center; }

double Circle::radius() const noexcept { return _radius; }

inline double Circle::getArea() const noexcept {
    return PI * _radius * _radius;
}

inline double Circle::getPerimeter() const noexcept { return 2 * PI * _radius; }

inline void Circle::deserialize(std::istream& in) noexcept {
    in >> _center >> _radius;
}

void Circle::accept(Visitor& visitor) noexcept { visitor.visit(*this); }

inline Shape* Circle::copy() const { return new Circle(*this); }
