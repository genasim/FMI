#include "Group.h"

#include <string>

#include "Shape.h"

Group::Group() : _shapes() {}

Group::Group(Shape **arr, size_t size) : Group() {
    for (size_t i = 0; i < size; i++) {
        _shapes.push_back(arr[i]->copy());
    }
    _shapes.shrink_to_fit();
}

Group::Group(const Group &other) { _copyFrom(other); }

Group &Group::operator=(const Group &other) {
    if (this != &other) {
        _free();
        _copyFrom(other);
    }
    return *this;
}

void Group::add(Shape *const &shape) noexcept {
    if (shape) _shapes.push_back(shape->copy());
}

void Group::add(Shape *&&shape) noexcept {
    _shapes.push_back(shape);
}

double Group::getArea() const noexcept {
    double area = 0;
    for (auto &&shape : *this) {
        area += shape->getArea();
    }
    return area;
}

double Group::getPerimeter() const noexcept {
    double perimeter = 0;
    for (auto &&shape : *this) {
        perimeter += shape->getPerimeter();
    }
    return perimeter;
}

void Group::serialize(std::ostream &out) const noexcept {
    out << "Group " << _shapes.size() << std::endl;
    for (auto &&shape : *this) {
        shape->serialize(out);
    }
}

void Group::deserialize(std::istream &in) noexcept {
    size_t size;
    in >> size;
    _shapes.resize(size);

    for (auto &&ptr : *this) {
        std::string type;
        in >> type;
        ptr = Shape::factory(type.c_str());
        ptr->deserialize(in);
    }
}

Shape *Group::copy() const { return new Group(*this); }

inline Vector<Shape *>::iterator Group::begin() const noexcept {
    return _shapes.begin();
}

inline Vector<Shape *>::iterator Group::end() const noexcept {
    return _shapes.end();
}

Group::~Group() { _free(); }

void Group::_copyFrom(const Group &other) {
    for (auto &&shape : other._shapes) {
        _shapes.push_back(shape->copy());
    }
}

void Group::_free() noexcept {
    for (auto &&shape : *this) {
        delete shape;
    }
    _shapes.clear();
}
