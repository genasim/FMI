#include "ExclusiveIdCriteria.h"

ExclusiveIdCriteria::ExclusiveIdCriteria() : undefined() {}

ExclusiveIdCriteria::ExclusiveIdCriteria(const Vector<int>& points)
    : undefined(points) {}

Criteria* ExclusiveIdCriteria::clone() const noexcept {
    return new ExclusiveIdCriteria(*this);
}

Pair<bool, int> ExclusiveIdCriteria::operator()(int arg) const noexcept {
    return undefined.contains(arg) ? Pair<bool, int>(false, 0)
                                   : Pair<bool, int>(true, arg);
}

void ExclusiveIdCriteria::serialize(std::ostream& out) const noexcept {
    int N = undefined.size();
    int T = 1;
    out.write((const char*)&T, sizeof(T));
    out.write((const char*)&N, sizeof(N));
    for (auto&& point : undefined) {
        out.write((const char*)&point, sizeof(point));
    }
}

void ExclusiveIdCriteria::deserialize(std::istream& in) noexcept {
    int N;
    in.read((char*)&N, sizeof(N));
    undefined.reserve(N);

    for (int i = 0; i < N; i++) {
        int a;
        in.read((char*)&a, sizeof(a));
        undefined.push_back(a);
    }
}
