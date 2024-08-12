#include "visitors/Serializer.h"

Serializer::Serializer(std::ostream& _out) : out(_out) {}

void Serializer::visit(Circle& circle) noexcept {
    out << "Circle " << circle.center() << " " << circle.radius() << std::endl;
}

void Serializer::visit(Rect& rect) noexcept {
    Vector<Point> points = rect.points();
    out << "Rect " << points[0] << ' ' << points[2] << std::endl;
}

void Serializer::visit(Triangle& triangle) noexcept {
    out << "Triangle ";
    for (auto&& point : triangle.points()) {
        out << point << ' ';
    }
    out << std::endl;
}

void Serializer::visit(Group& group) noexcept {
    out << "Group " << group.size() << std::endl;
    for (auto&& shape : group) {
        shape->accept(*this);
    }
}