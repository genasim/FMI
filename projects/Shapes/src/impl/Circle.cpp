#include "Circle.h"

const double Circle::PI = 3.14;

Circle::Circle() : Circle({0, 0}, 1) {}

Circle::Circle(Point center, double radius)
    : _center(center), _radius(radius) {}

inline double Circle::getArea() const noexcept {
    return PI * _radius * _radius;
}

inline double Circle::getPerimeter() const noexcept { return 2 * PI * _radius; }

inline void Circle::serialize(std::ostream& out) const noexcept {
    out << "Circle " << _center << ' ' << _radius << std::endl;
}

inline void Circle::deserialize(std::istream& in) noexcept {
    in >> _center >> _radius;
}

inline Shape* Circle::copy() const { return new Circle(*this); }
