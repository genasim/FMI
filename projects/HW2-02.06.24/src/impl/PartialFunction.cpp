#include "PartialFunction.h"

#include <exception>

#include "CriteriaFunction.h"
#include "MaximumFunction.h"
#include "MinimumFunction.h"

#include "Criteria.h"
#include "ExclusiveIdCriteria.h"
#include "FiniteCriteria.h"
#include "BinaryCriteria.h"

PartialFunction* PartialFunction::factory(int code) {
    switch (code) {
        case 0:
            return new CriteriaFunction(new FiniteCriteria());
            break;

        case 1:
            return new CriteriaFunction(new ExclusiveIdCriteria());
            break;

        case 2:
            return new CriteriaFunction(new BinaryCriteria());
            break;

        case 3:
            return new MaximumFunction();
            break;

        case 4:
            return new MinimumFunction();
            break;

        default:
            throw std::runtime_error("Criteria::factory -> Invalid code");
            break;
    }
}
