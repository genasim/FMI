#include "CriteriaFunction.h"

#include <exception>
#include <iostream>
#include <sstream>

CriteriaFunction::CriteriaFunction(Criteria* const& func) {
    predicate = func->clone();
}

PartialFunction* CriteriaFunction::clone() const {
    return new CriteriaFunction(*this);
}

void CriteriaFunction::serialize(std::ostream& out) const noexcept {
    predicate->serialize(out);
}

void CriteriaFunction::deserialize(std::istream& in) noexcept {
    predicate->deserialize(in);
}

CriteriaFunction::CriteriaFunction(const CriteriaFunction& other) {
    _copy(other);
}

CriteriaFunction& CriteriaFunction::operator=(const CriteriaFunction& other) {
    if (this != &other) {
        _copy(other);
    }
    return *this;
}

void CriteriaFunction::_copy(const CriteriaFunction& other) {
    predicate.reset(other.predicate->clone());
}

bool CriteriaFunction::defined(int x) const noexcept {
    return (*predicate)(x).first;
}

int CriteriaFunction::compute(int x) const {
    if (!defined(x)) {
        std::ostringstream oss;
        oss << "CriteriaFunction::compute(int x) -> function is undefined for "
            << x;
        throw std::runtime_error(oss.str().c_str());
    }
    return (*predicate)(x).second;
}
