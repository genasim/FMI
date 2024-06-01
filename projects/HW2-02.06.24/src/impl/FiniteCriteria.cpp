#include "FiniteCriteria.h"

FiniteCriteria::FiniteCriteria() : points() {}

FiniteCriteria::FiniteCriteria(const Vector<int32_t>& args,
                               const Vector<int32_t>& results) {
    size_t min = std::min(args.size(), results.size());
    for (size_t i = 0; i < min; i++) {
        points.push_back({args[i], results[i]});
    }
}

Criteria* FiniteCriteria::clone() const noexcept {
    return new FiniteCriteria(*this);
}

Pair<bool, int> FiniteCriteria::operator()(int arg) const noexcept {
    for (auto&& point : points) {
        if (point.first == arg) {
            return {true, point.second};
        }
    }
    return {false, 0};
}

void FiniteCriteria::serialize(std::ostream& out) const noexcept {
    int N = points.size();
    int T = 0;
    out.write((const char*)&T, sizeof(T));
    out.write((const char*)&N, sizeof(N));
    for (auto&& point : points) {
        out.write((const char*)&point.first, sizeof(point.first));
    }
    for (auto&& point : points) {
        out.write((const char*)&point.second, sizeof(point.second));
    }
}

void FiniteCriteria::deserialize(std::istream& in) noexcept {
    int N;
    in.read((char*)&N, sizeof(N));

    Vector<int> args, results;
    points.reserve(N);
    args.reserve(N);
    results.reserve(N);

    for (int i = 0; i < N; i++) {
        int a;
        in.read((char*)&a, sizeof(a));
        args.push_back(a);
    }
    for (int i = 0; i < N; i++) {
        int r;
        in.read((char*)&r, sizeof(r));
        results.push_back(r);
    }

    for (int i = 0; i < N; i++) {
        points.push_back({args[i], results[i]});
    }
}
