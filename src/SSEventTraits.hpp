#ifndef _SS_EVENT_TRAITS_HPP_
#define _SS_EVENT_TRAITS_HPP_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSConsts.hpp"
#include "SSUtilities.hpp"

namespace StrideSearch {

    enum IntensityComparison {
        /// Used when an Event with a lower value is more intense (e.g., pressure, height).
        LESS_THAN,
        /// Used when an Event with higher value is more intense (e.g., wind speed).
        GREATER_THAN};
    
    enum SpatialDependence {
        /// Used when an Event's value depends on the location it is computed (e.g., a spatial average).
        DEPENDENT, 
        /// Used when an Event's value is independent of the location it's computed (e.g., a maximum).
        INDEPENDENT};

}
#endif
