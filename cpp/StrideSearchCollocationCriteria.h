#ifndef _STRIDE_SEARCH_COLLOCATION_CRITERIA_H_
#define _STRIDE_SEARCH_COLLOCATION_CRITERIA_H_

#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchEvent.h"
#include <string>

namespace StrideSearch {

class CollocationCriterion {
    protected:
        std::string eventDesc1;
        std::string eventDesc2;
        scalar_type distanceThreshold;
        
    public:
        CollocationCriterion(const std::string& desc1, const std::string& desc2, const scalar_type thresh);
        
        bool evaluate(const Event& event) const;
};

}
#endif