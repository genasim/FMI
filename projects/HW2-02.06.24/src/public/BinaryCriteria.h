#pragma once

#include "Criteria.h"
#include "utils/Vector.h"

class BinaryCriteria : public Criteria {
   public:
    BinaryCriteria();
    BinaryCriteria(const Vector<int> ones);

    Criteria* clone() const noexcept override;
    Pair<bool, int> operator()(int) const noexcept override;

    void serialize(std::ostream& out) const noexcept override;
    void deserialize(std::istream& in) noexcept override;

   private:
    Vector<int> ones;
};