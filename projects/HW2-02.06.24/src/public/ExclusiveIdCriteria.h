#pragma once

#include "Criteria.h"
#include "utils/Vector.h"

class ExclusiveIdCriteria : public Criteria {
   public:
    ExclusiveIdCriteria();
    ExclusiveIdCriteria(const Vector<int>& points);

    Criteria* clone() const noexcept override;
    Pair<bool, int> operator()(int arg) const noexcept override;

    void serialize(std::ostream& out) const noexcept override;
    void deserialize(std::istream& in) noexcept override;

   private:
    Vector<int> undefined;
};