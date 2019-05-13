#ifndef _SS_SECTOR_SET_HPP_
#define _SS_SECTOR_SET_HPP_

#include "SSSector.hpp"


namespace StrideSearch {

/// A SectorSet is the data structure used by driver programs to conduct searches of data sets.
/**
    SectorSet is responsible for 
    1. Creating Sectors within a given search region defined as a rectangle
    in latitude-longitude space.
    2. Linking each sector to the data points and indices within the file associated with an StrideSearchData_Base subclass.
    3. Providing a control structure (loop) for the per-timestep search.
*/
template <typename DataLayout=UnstructuredLayout>
class SectorSet {
    public:
        /// Constructor.
        /**
            @param sb : southern boundary of search domain (degrees latitude)
            @param nb : northern boundary of search domain (degrees latitude)
            @param wb : western boundary of search domain (degrees longitude)
            @param eb : eastern boundary of search domain (degrees longitude)
        */
        SectorSet(const Real sb=-90.0, const Real nb=90.0, const Real wb=0.0, const Real eb=360.0,
        const Real rad=2000.0);
        
    
        /// Constructor for predefined sector locations
        SectorSet(const RealArray& center_lats, const ReayArray& center_lons, const Real rad);
        
        
        
    
    protected:

};

}
#endif
