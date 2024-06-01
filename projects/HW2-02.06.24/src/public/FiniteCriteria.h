#pragma once

#include "Criteria.h"
#include "utils/Pair.h"
#include "utils/Vector.h"

class FiniteCriteria : public Criteria {
   public:
    FiniteCriteria();
    FiniteCriteria(const Vector<int32_t>& args, const Vector<int32_t>& results);

    Criteria* clone() const noexcept override;
    Pair<bool, int> operator()(int) const noexcept override;

    void serialize(std::ostream& out) const noexcept override;
    void deserialize(std::istream& in) noexcept override;

   private:
    Vector<Pair<int, int>> points;
};
