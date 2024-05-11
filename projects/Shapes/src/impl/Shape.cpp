#include "Shape.h"

#include <cmath>
#include <cstring>

#include "Circle.h"
#include "Group.h"
#include "Rect.h"
#include "Triangle.h"

double Point::distance(Point a, Point b) {
    return std::sqrt((b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y));
}

std::istream& operator>>(std::istream& in, Point& point) {
    in >> point.x >> point.y;
    return in;
}

std::ostream& operator<<(std::ostream& out, Point point) {
    out << point.x << ' ' << point.y;
    return out;
}

Shape* Shape::factory(const char* type) {
    if (strcmp(type, "Circle") == 0) {
        return new Circle();
    }
    if (strcmp(type, "Group") == 0) {
        return new Group();
    }
    if (strcmp(type, "Rect") == 0) {
        return new Rect();
    }
    if (strcmp(type, "Triangle") == 0) {
        return new Triangle();
    }
    return nullptr;
}