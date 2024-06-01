#include "BinaryCriteria.h"

BinaryCriteria::BinaryCriteria() : ones() {}

BinaryCriteria::BinaryCriteria(const Vector<int> args) : ones(args) {}

Criteria* BinaryCriteria::clone() const noexcept {
    return new BinaryCriteria(*this);
}

Pair<bool, int> BinaryCriteria::operator()(int arg) const noexcept {
    bool result = ones.contains(arg);
    return {true, result};
}

void BinaryCriteria::serialize(std::ostream& out) const noexcept {
    int T = 2;
    int N = ones.size();
    out.write((const char*)&T, sizeof(T));
    out.write((const char*)&N, sizeof(N));
    for (auto&& arg : ones) {
        out.write((const char*)&arg, sizeof(arg));
    }
}

void BinaryCriteria::deserialize(std::istream& in) noexcept {
    int N;
    in.read((char*)&N, sizeof(N));
    ones.reserve(N);

    for (int i = 0; i < N; i++) {
        int a;
        in.read((char*)&a, sizeof(a));
        ones.push_back(a);
    }
}
