#ifndef _STRIDE_SEARCH_ID_CRITERIA_BASE_H_
#define _STRIDE_SEARCH_ID_CRITERIA_BASE_H_

#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchEvent.h"
#include <string>
#include <vector>

namespace StrideSearch {

class Sector;

class IDCriterion {
    protected:
        std::vector<std::string> varnames;
        scalar_type threshold;
    
    public:
        IDCriterion(std::vector<std::string> varnames, const scalar_type threshold);
        virtual ~IDCriterion(){};
           
        virtual bool evaluate(const Sector* sec, const Workspace* wspc) const = 0;
        virtual Event returnEvent(const Sector* sec, const Workspace* wspc, const DateTime& dt, 
            const StrideSearchData* sdata) const = 0;
        virtual std::string returnEventType() const = 0;
};

class MaxCriterion : public IDCriterion {
    bool evaluate(const Sector* sec, const Workspace* wspc) const;
    Event returnEvent(const Sector* sec, const Workspace*, const DateTime& dt, const StrideSearchData* sdata);
    std::string returnEventType() const;
};

class MinCriterion : public IDCriterion {
    bool evaluate(const Sector* sec, const Workspace* wspc) const;
    Event returnEvent(const Sector* sec, const Workspace*, const DateTime& dt, const StrideSearchData* sdata);
    std::string returnEventType() const;
};

class CollocationCriterion : public IDCriterion {
    bool evaluate(const Sector* sec, const Workspace* wspc) const;
    Event returnEvent(const Sector* sec, const Workspace*, const DateTime& dt, const StrideSearchData* sdata);
    std::string returnEventType() const;
};

}
#endif
