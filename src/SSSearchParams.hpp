#ifndef _SS_SEARCH_PARAMS_
#define _SS_SEARCH_PARAMS_

#include "SSDefs.hpp"
#include "SSConsts.hpp"
#include "SSDateTime.hpp"

namespace StrideSearch {

struct SearchParams {
    DateTime::DTUnits time_units;
    Int timestep_stride;
    
    SearchParams(const DateTime::DTUnits& dtu = DateTime::DTUnits::DAYS, const Int ts=1) : 
        time_units(dtu), timestep_stride(ts) {}
};

}
#endif