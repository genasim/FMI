#pragma once

#include "Criteria.h"
#include "PartialFunction.h"
#include "utils/UniquePtr.h"

class CriteriaFunction final : public PartialFunction {
   public:
    bool defined(int x) const noexcept override;
    int compute(int x) const override;

    CriteriaFunction(Criteria* const& func);

    PartialFunction* clone() const override;

    void serialize(std::ostream& out) const noexcept override;
    void deserialize(std::istream& in) noexcept override;

   private:
    UniquePtr<Criteria> predicate;

   public:
    CriteriaFunction(const CriteriaFunction& other);
    CriteriaFunction& operator=(const CriteriaFunction& other);

    CriteriaFunction(CriteriaFunction&& other) noexcept = default;
    CriteriaFunction& operator=(CriteriaFunction&& other) noexcept = default;

   private:
    void _copy(const CriteriaFunction& other);
};