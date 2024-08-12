#include "Rect.h"

#include "visitors/Visitor.h"

Rect::Rect() : Rect({0, 0}, {1, 1}) {}

Rect::Rect(Point a, Point b) { _fillPoints(a, b); }

inline double Rect::getArea() const noexcept {
    return Point::distance(_points[0], _points[1]) *
           Point::distance(_points[0], _points[3]);
}

inline double Rect::getPerimeter() const noexcept {
    return 2 * (Point::distance(_points[0], _points[1]) +
                Point::distance(_points[0], _points[3]));
}

Vector<Point> Rect::points() const noexcept { return _points; }

inline void Rect::deserialize(std::istream &in) noexcept {
    Point a, b;
    in >> a;
    in >> b;
    _points.clear();
    _fillPoints(a, b);
}

void Rect::accept(Visitor &visitor) noexcept { visitor.visit(*this); }

Shape *Rect::copy() const { return new Rect(*this); }

void Rect::_fillPoints(Point a, Point b) {
    _points.push_back(a);
    _points.push_back({b.x, a.y});
    _points.push_back(b);
    _points.push_back({a.x, b.y});
}
