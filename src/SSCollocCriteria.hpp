#ifndef _SS_COLLOC_CRITERIA_HPP_
#define _SS_COLLOC_CRITERIA_HPP_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSEvent.hpp"
#include "SSUtilities.hpp"
#include "SSIdCriteria.hpp"
#include <memory>
#include <limits>

namespace StrideSearch {

class CollocationCriterion {
    public:
    CollocationCriterion(const std::shared_ptr<IDCriterion> crit_1, const std::shared_ptr<IDCriterion> crit_2,
        const Real dist) : crit1(crit_1), crit2(crit_2), distance_threshold(dist) {}
    
    std::shared_ptr<IDCriterion> crit1;
    std::shared_ptr<IDCriterion> crit2;
    Real distance_threshold;
};

}
#endif