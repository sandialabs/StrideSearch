#ifndef _STRIDE_SEARCH_COLLOCATION_CRITERIA_H_
#define _STRIDE_SEARCH_COLLOCATION_CRITERIA_H_

#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchIDCriteria_Base.h"

class CollocationCriterion : public IDCriterion {
    bool evaluate(const Sector* sec, const Workspace* wspc) const;
    Event returnEvent(const Sector* sec, const Workspace*, const DateTime& dt, const StrideSearchData* sdata);
    std::string returnEventType() const;
};


#endif