#include "Triangle.h"
#include "visitors/Visitor.h"

#include <cmath>

Triangle::Triangle() : Triangle({0, 0}, {1, 0}, {0, 1}) {}

Triangle::Triangle(Point a, Point b, Point c) {
    _points.reserve(3);
    _points.push_back(a);
    _points.push_back(b);
    _points.push_back(c);
}

double Triangle::getArea() const noexcept {
    double sideA = Point::distance(_points[0], _points[1]);
    double sideB = Point::distance(_points[0], _points[2]);
    double sideC = Point::distance(_points[1], _points[2]);

    double halfPer = (sideA + sideB + sideC) / 2;
    return sqrt(halfPer * (halfPer - sideA) * (halfPer - sideB) *
                (halfPer - sideC));
}

double Triangle::getPerimeter() const noexcept {
    double sideA = Point::distance(_points[0], _points[1]);
    double sideB = Point::distance(_points[0], _points[2]);
    double sideC = Point::distance(_points[1], _points[2]);

    return sideA + sideB + sideC;
}

Vector<Point> Triangle::points() const noexcept { return _points; }

void Triangle::deserialize(std::istream &in) noexcept {
    for (auto &point : _points) {
        in >> point;
    }
}

void Triangle::accept(Visitor &visitor) noexcept {
    visitor.visit(*this);
}

inline Shape *Triangle::copy() const { return new Triangle(*this); }
