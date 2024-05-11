#pragma once
#include "Shape.h"

class Group : public Shape {
   public:
    Group();
    Group(Shape**, size_t);
    Group(const Group&);
    Group& operator=(const Group&);
    Group(Group&&) = default;
    Group& operator=(Group&&) = default;

    void add(Shape* const&) noexcept;
    void add(Shape*&&) noexcept;

    double getArea() const noexcept override;
    double getPerimeter() const noexcept override;

    void serialize(std::ostream&) const noexcept override;
    void deserialize(std::istream&) noexcept override;

    Shape* copy() const override;

    Vector<Shape*>::iterator begin() const noexcept;
    Vector<Shape*>::iterator end() const noexcept;

    ~Group();

   private:
    Vector<Shape*> _shapes;
    void _copyFrom(const Group&);
    void _free() noexcept;
};