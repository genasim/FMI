#include "visitors/Loader.h"

Loader::Loader(std::istream& _in) : in(_in) {}

void Loader::visit(Circle& circle) noexcept {
}

void Loader::visit(Rect& rect) noexcept {
}

void Loader::visit(Triangle& triangle) noexcept {
}

void Loader::visit(Group& group) noexcept {
}