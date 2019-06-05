#ifndef _STRIDE_SEARCH_EXTRA_CRITERIA_H_
#define _STRIDE_SEARCH_EXTRA_CRITERIA_H_

#include "StrideSearchConfig.h"
#include "StrideSearchTypeDefs.h"
#include "StrideSearchIDCriterionBase.h"
#include "StrideSearchWorkspaceDict.h"

namespace StrideSearch {

class ExtraCriterion : public IDCriterion {
    public:
        ExtraCriterion(const std::string varname) : IDCriterion(varname, 0.0, Event::NO_COMPARE) {};
        
        bool evaluate(const WorkspaceDict& wspc) {return true;}
        std::string description() const {return "value(" + varnames[0] + ")";}
};

}

#endif