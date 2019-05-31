#ifndef _SS_SEARCH_PARAMS_
#define _SS_SEARCH_PARAMS_

#include "SSDefs.hpp"
#include "SSConsts.hpp"
#include "SSDateTime.hpp"

namespace StrideSearch {

struct SearchParams {
    DTUnits time_units;
    Int timestep_stride;
    
    SearchParams(const DTUnits& dtu = DTUnits::DAYS, const Int ts=1) : 
        time_units(dtu), timestep_stride(ts) {}
};

}
#endif