#ifndef _SS_SECTOR_SET_HPP_
#define _SS_SECTOR_SET_HPP_

#include "SSSector.hpp"
#include "SSEventSet.hpp"
#include "SSKdTree.hpp"
#include "SSNCReader.hpp"

namespace StrideSearch {

/// A SectorSet is the data structure used by driver programs to conduct searches of data sets.
/**
    __SectorSet responsibilities__ @n
    1. Create Sectors within a search region defined by a rectangle in latitude-longitude space.
    2. Link each sector to the data points and indices within the file associated with an StrideSearchData_Base subclass.
    3. Providing a control structure (loop) for the per-timestep search.
    4. Has unique ownership of a vector of Sector instances
*/
template <typename DataLayout=UnstructuredLayout>
class SectorSet {
    public:
        /// Main Constructor.
        /**
            __Responsibilities:__@n
            1. Computes latitude stride and longitude strides for each latitude line
            2. Creates a SectorSet covering the search region using the strides
            
            
            @post SectorSet is initialized, but not yet linked to data.
        
            @param sb : southern boundary of search domain (degrees latitude, in [-90, 90))
            @param nb : northern boundary of search domain (degrees latitude, in (-90, 90])
            @param wb : western boundary of search domain (degrees longitude, in [0,360))
            @param eb : eastern boundary of search domain (degrees longitude, in (0,360])
            @param rad : sector radius, in kilometers
            
            @throws if boundaries lie outside above ranges or if the search region is degenerate
        */
        SectorSet(const Real sb=-90.0, const Real nb=90.0, const Real wb=0.0, const Real eb=360.0,
        const Real rad=2000.0);
        
        /// Constructor for predefined sector locations
        /**
            This constructor is primarily used for testing.
            Strides are left uninitialized, search region undefined.
            
            @post SectorSet is initialized but not yet linked to data.
        */
        SectorSet(const RealArray& center_lats, const RealArray& center_lons, const Real rad);
        
        /// Constructor for Sectors centered on Event locations from an EventSet
        /**
            __Responsibilities:__@n
            1. Create a new sector centered on every event in evs.
            
            @post SectorSet is initialized but not linked to data. Strides are not initialized, 
            search region not initialized.
        */
        SectorSet(const EventSet<DataLayout>& evs, const Real rad);
        
        /// Return an array containing the latitudes of all sectors in this set
        RealArray centerLats() const;
        
        /// Return an array containing the longitudes of all sectors in this set
        RealArray centerLons() const;
        
        /// Return the number of sectors in this set
        Index nSectors() const {return sectors.size();}
        
        /// Array of sectors
        std::vector<std::unique_ptr<Sector<DataLayout>>> sectors;
        
        /// Return the maximum number of data points contained by any one sector
        Int maxPointsPerSector() const;
        
        /// Return the minimum number of data points contained by any one sector
        Int minPointsPerSector() const;
        
        
        /// Links each sector to data
        /**
            @todo thread paralellize over sectors
        */
        void linkToData(const KDTree& tree, const std::shared_ptr<NCReader> ncr);
        
        std::string infoString(const bool printall=false) const;
        
    protected:
        /// Southern boundary of search domain, in [-90, 90)
        Real southern_boundary;
        /// Northern boundary of search domain, in (-90,90]
        Real northern_boundary;
        /// Western boundary of search domain, in [0, 360)
        Real western_boundary;
        /// Eastern boundary of search domain, in (0, 360]
        Real eastern_boundary;
        /// sector radius
        Real radius;
        /// Latitude Stride: distance in latitude between strips
        Real lat_stride_degrees;
        /// Number of latitude strips
        Int nstrips;
        /// Longitude strides along each latitude strip
        RealArray lon_strides_degrees;

};

}
#endif
