#pragma once

#include "utils/Pair.h"
#include "utils/String.h"

class Criteria {
   public:
    virtual Criteria* clone() const noexcept = 0;
    virtual Pair<bool, int> operator()(int) const noexcept = 0;
    
    virtual void serialize(std::ostream& out) const noexcept = 0;
    virtual void deserialize(std::istream& in) noexcept = 0;

    virtual ~Criteria() = default;
};