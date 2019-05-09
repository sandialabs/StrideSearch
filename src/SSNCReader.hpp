#ifndef _SS_NC_READER_HPP_
#define _SS_NC_READER_HPP_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSConsts.hpp"
#include "SSUtilities.hpp"
#include <netcdf>
#include <memory>
#include <map>

namespace StrideSearch {

/// Defines a common data structure for all data sets to use with kd tree utilities.
/**
    Each pt (x[i], y[i], z[i]) has magnitude = EARTH_RADIUS_KM
*/
struct Points {
    typedef Real coord_t;
    RealArray x;
    RealArray y;
    RealArray z;
    Int n;
    Points() {}
    Points(const Int nn) : x(nn), y(nn), z(nn), n(nn) {}
};

/// Abstract base class for netcdf interface.  Subclassed for each type of data layout.
/**    
    Properties:
        An NCReader has unique ownership of an NcFile.  
    
    Responsibilities:
        The format of netcdf data is not standardized. 
        Some grids are unstructured, some are structured; some are given in angular variables (e.g., lat-lon),
        others are given in Cartesian variables (x,y,z).  
        For the Cartesian variables, some are given on the unit sphere, others have physical dimensions.
        
        The NCReader class creates Points data that the rest of Stride Search can rely on to be
            1. In Cartesian (x,y,z) coordinates.
            2. On an Earth-sized sphere with units of kilometers.

*/
class NCReader {
    public:
        virtual ~NCReader()  {}
        virtual Points makePoints() const = 0;
        virtual Int nPoints() const = 0;
        virtual Real getLat(const Index ptInd) const = 0;
        virtual Real getLon(const Index ptInd) const = 0;
        
        /// returns the time values contained in the NcFile
        RealArray getTime() const;  
        
        /// Updates reader to use a new source file in the same data set (the same horizontal grid is assumed).
        void updateFile(const std::string& filename);
        
        //// Returns the curent source filename
        std::string filename() const {return src_file;}
        
        void printLats() const;
        void printLons() const;
        
    protected:
        NCReader() {}
        NCReader(const std::string& filename); 
        
        /// Ptr to netCDF::NcFile
        std::unique_ptr<const netCDF::NcFile> ncfile; 
        
        /// full filename (including path)
        std::string src_file; 
        
        virtual void initCoordinates() = 0;
        
        RealArray lats;
        RealArray lons;

};


///    Instantiation of NCReader for lat-lon structured grids
/**
*/
class LatLonNCReader : public NCReader {
    public: 
        typedef typename LatLonLayout::data_index_type data_index_type;
        
        /// Returns a collection of x, y, z points for use with nanoflann
        Points makePoints() const;    
    
        LatLonNCReader(const std::string& filename) : NCReader(filename) {
            initCoordinates();
        }
        
        ~LatLonNCReader() {}
    
        void llIndices(Int& lat_ind, Int& lon_ind, const Int& pt_ind) const {
            lat_ind = pt_ind/n_lon;
            lon_ind = pt_ind%n_lon;
        }
    
        /// Returns the number of horizontal grid points
        inline Int nPoints() const {return n_lat*n_lon;}
        
        Real getLat(const Index ptInd) const {return lats[ptInd/n_lon];}
        Real getLon(const Index ptInd) const {return lons[ptInd%n_lon];}
    protected:
        Int n_lat;
        Int n_lon;
    
        /// Initializes coordinate data by reading latitude and longitude data from file.
        void initCoordinates();
};



///    Instantiation of NCReader for unstructured grids
/**
*/
class UnstructuredNCReader : public NCReader {
    public: 
        typedef typename UnstructuredLayout::data_index_type data_index_type;
        
        /// Returns a collection of x, y, z points for use with nanoflann
        Points makePoints() const;
    
        UnstructuredNCReader(const std::string& filename) : NCReader(filename) {
            initCoordinates();
        }
        
        ~UnstructuredNCReader() {}
    
        /// Returns the number of horizontal grid points.
        inline Int nPoints() const {return n_nodes;}
        
        Real getLat(const Index ptInd) const {return lats[ptInd];}
        Real getLon(const Index ptInd) const {return lons[ptInd];}
    protected:
    
        /// Initializes x, y, z, arrays by reading coord* data from file.
        void initCoordinates();
        
        Int n_nodes;
        RealArray lats;
        RealArray lons;
};

}
#endif