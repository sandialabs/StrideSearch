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
            @param rad : sector radius
        */
        SectorSet(const Real sb=-90.0, const Real nb=90.0, const Real wb=0.0, const Real eb=360.0,
        const Real rad=2000.0);
        
    
        /// Constructor for predefined sector locations
        SectorSet(const RealArray& center_lats, const RealArray& center_lons, const Real rad);
        
        
        /// Constructor for Sectors centered on Event locations from an EventSet
        SectorSet(const EventSet<DataLayout>& evs, const Real rad);
        
        RealArray centerLats() const;
        RealArray centerLons() const;
        
        Index nSectors() const {return sectors.size();}
        
        std::vector<std::unique_ptr<Sector<DataLayout>>> sectors;
        
        Int maxPointsPerSector() const;
        
        Int minPointsPerSector() const;
    protected:
        /// Southern boundary of search domain, in [-90, 90)
        Real southern_boundary;
        /// Northern boundary of search domain, in (-90,90]
        Real northern_boundary;
        /// Western boundary of search domain, in [0, 360)
        Real western_boundary;
        /// Eastern boundary of search domain, in (0, 360]
        Real eastern_boundary;
        
        /// Latitude Stride: distance in latitude between strips
        Real lat_stride_degrees;
        /// Number of latitude strips
        Int nstrips;
        /// Longitude strides along each latitude strip
        RealArray lon_strides_degrees;

};

}
#endif
