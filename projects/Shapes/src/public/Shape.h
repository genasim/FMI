#pragma once
#include <fstream>

#include "data/Vector.h"

class Visitor;

struct Point {
    static double distance(Point, Point);
    Point() : x(0), y(0) {};
    Point(double x, double y) : x(x), y(y) {};
    double x;
    double y;
};
std::istream& operator>>(std::istream&, Point&);
std::ostream& operator<<(std::ostream&, Point);

class Shape {
   public:
    static Shape* factory(const char*);

    virtual double getArea() const noexcept = 0;
    virtual double getPerimeter() const noexcept = 0;

    virtual void deserialize(std::istream&) noexcept = 0;

    virtual void accept(Visitor&) noexcept = 0;
    virtual Shape* copy() const = 0;

    virtual ~Shape() = default;
};
