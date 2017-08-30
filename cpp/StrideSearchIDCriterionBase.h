#ifndef _STRIDE_SEARCH_ID_CRITERION_BASE_H_
#define _STRIDE_SEARCH_ID_CRITERION_BASE_H_

#include "StrideSearchConfig.h"
#include "StrideSearchTypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchEvent.h"
#include "StrideSearchWorkspaceDict.h"
#include <string>
#include <vector>

namespace StrideSearch {

/// Base class and interface for all identification criteria.
class IDCriterion {
    protected:
        IDCriterion(const std::vector<std::string>& vars, const scalar_type thresh, const Event::IntensityComparison cmprKind);
        IDCriterion(const std::string& var, const scalar_type thresh, const Event::IntensityComparison cmprKind);
        IDCriterion(const std::string& var1, const std::string& var2, const scalar_type thresh, 
            const Event::IntensityComparison cmprKind);

    public:
        Event::IntensityComparison compareKind;
        std::vector<std::string> varnames;
        scalar_type threshold;
        index_type wspcIndex;
        scalar_type val;
        
        virtual ~IDCriterion(){};
           
        virtual bool evaluate(const WorkspaceDict& wspc) = 0;
        virtual std::string description() const = 0;
};


}
#endif
