#ifndef _SS_SEARCH_PARAMS_
#define _SS_SEARCH_PARAMS_

#include "SSDefs.hpp"
#include "SSConsts.hpp"
#include "SSDateTime.hpp"

namespace StrideSearch {

/// Parameter class to alter the behavior of StrideSearch::SearchManager instances
struct SearchParams {
    /// Units of time used the data files.  Required if the data's time variable is not defined in units of days.
    DTUnits time_units;
    /// Number of timesteps to skip between spatial searches.  Used when the data's temporal resolution is more than necessary for a particular application.  
    Int timestep_stride;
    
    /// Constructor
    /**
        @param dtu : units of time from data set
        @param ts : timestep_stride
    */
    SearchParams(const DTUnits& dtu = DTUnits::DAYS, const Int ts=1) : 
        time_units(dtu), timestep_stride(ts) {}
};

}
#endif