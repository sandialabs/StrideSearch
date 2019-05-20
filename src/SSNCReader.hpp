#ifndef _SS_NC_READER_HPP_
#define _SS_NC_READER_HPP_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSDataLayoutTraits.hpp"
#include "SSConsts.hpp"
#include "SSUtilities.hpp"
#include "SSWorkspace.hpp"
#include <netcdf>
#include <memory>
#include <map>
#include <vector>

namespace StrideSearch {

// fwd declaration
template <typename RT> class SSData;

/// Defines a common data structure for all data sets to use with kd tree utilities and SSData.
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

    Required subclass methods:
        - makePoints() const : return a Points object of Cartesian coordinates, each with magnitude = EARTH_RADIUS_KM
        - nPoints() const : return the number of horizontal grid points in the data set
        - getLat(const Index ind) const: Return the latitude of the data point in the Points object
        - getLon(const Index ind) const: Return the latitude of the data point in the Points object
        - loadVariableValue(const Index ind, Real& val) const : Load one variable value 
        - loadVariableValue(const Index ind, const Index lev_ind, Real& val) const : Load one variable value

*/
class NCReader {
    public:
        virtual ~NCReader()  {}
        virtual Points makePoints() const = 0;
        virtual Int nPoints() const = 0;
        virtual Real getLat(const Index ptInd) const = 0;
        virtual Real getLon(const Index ptInd) const = 0;

        virtual void fillWorkspaceData(Workspace& wspc, const std::vector<typename UnstructuredLayout::horiz_index_type>& inds, const Int t_ind, const Int l_ind=-1) const {}
        virtual void fillWorkspaceData(Workspace& wspc, const std::vector<typename LatLonLayout::horiz_index_type>& inds, const Int t_ind, const Int l_ind=-1) const {}
        
        /// returns the time values contained in the NcFile
        /**
            @throws std::runtime_error if *this cannot locate a time coordinate variable in ncfile.
        */
        RealArray getTime() const;  
        
        /// Updates reader to use a new source file in the same data set (the same horizontal grid is assumed).
        void updateFile(const std::string& filename);
        
        /// Returns the curent source filename
        std::string filename() const {return src_file;}
        
        /// Print coordinates to console
        void printLats() const;
        /// Print coordinates to console
        void printLons() const;
        
    protected:
        NCReader() {}
        NCReader(const std::string& filename); 
        
        /// Ptr to netCDF::NcFile
        std::unique_ptr<const netCDF::NcFile> ncfile; 
        
        /// full filename (including path)
        std::string src_file; 
        
        /// Initializes data point coordinates
        /**
            Standardizes all data points to lat-lon coordinates to minimize storage and
            to facilitate normalizing all coordinates to an Earth-sized sphere with radius = EARTH_RADIUS_KM.
            
            @throws std::runtime_error if *this cannot locate coordinate variables in ncfile.
        */
        virtual void initCoordinates() = 0;
        
        /// Latitude coordinates of data points
        RealArray lats;
        /// Longitude coordinates of data points
        RealArray lons;

};


///    Instantiation of NCReader for lat-lon structured grids
/**
    This class's makePoints method packs the 2d data into a 1d array of points;
    the inverse of this packing is computed as part of the getLat and getLon methods.
*/
class LatLonNCReader : public NCReader {
    public: 
        typedef typename LatLonLayout::horiz_index_type data_index_type;
        typedef LatLonLayout Layout;
        template <typename RT> friend class SSData;
        
        /// Returns a collection of x, y, z points for use with nanoflann
        Points makePoints() const override;    
    
        LatLonNCReader(const std::string& filename) : NCReader(filename) {
            initCoordinates();
        }
        
        ~LatLonNCReader() {}
    
        void llIndices(Int& lat_ind, Int& lon_ind, const Int& pt_ind) const {
            lat_ind = pt_ind/n_lon;
            lon_ind = pt_ind%n_lon;
        }
    
        /// Returns the number of horizontal grid points
        inline Int nPoints() const override {return n_lat*n_lon;}
        
        data_index_type getDataIndFromTreeInd(const Index ptInd) const {
            data_index_type result;
            result[0] = ptInd/n_lon;
            result[1] = ptInd%n_lon;
            return result;
        }
        
        Real getLat(const Index ptInd) const override {return lats[ptInd/n_lon];}
        Real getLon(const Index ptInd) const override {return lons[ptInd%n_lon];}
        
        void fillWorkspaceData(Workspace& wspc,
            const std::vector<typename LatLonLayout::horiz_index_type>& inds,
            const Int t_ind, const Int l_ind=-1) const override;
        
    protected:
        Int n_lat;
        Int n_lon;
    
        /// Initializes coordinate data by reading latitude and longitude data from file.
        void initCoordinates() override;
};



///    Instantiation of NCReader for unstructured grids
/**
    This class assumes a 1-1 mapping between data points and Points points.
*/
class UnstructuredNCReader : public NCReader {
    public: 
        typedef typename UnstructuredLayout::horiz_index_type data_index_type;
        typedef UnstructuredLayout Layout;
        template <typename RT> friend class SSData;
        
        /// Returns a collection of x, y, z points for use with nanoflann
        Points makePoints() const override;
    
        UnstructuredNCReader(const std::string& filename) : NCReader(filename) {
            initCoordinates();
        }
        
        ~UnstructuredNCReader() {}
    
        /// Returns the number of horizontal grid points.
        inline Int nPoints() const override {return n_nodes;}
        
        data_index_type getDataIndFromTreeInd(const Index ptInd) const {
            data_index_type result;
            result[0] = ptInd;
            return result;
        }
        
        Real getLat(const Index ptInd) const override {return lats[ptInd];}
        Real getLon(const Index ptInd) const override {return lons[ptInd];}
        
        void fillWorkspaceData(Workspace& wspc, 
            const std::vector<typename UnstructuredLayout::horiz_index_type>& inds, 
            const Int t_ind, const Int l_ind=-1) const override;
    protected:
    
        /// Initializes x, y, z, arrays by reading coord* data from file.
        void initCoordinates() override;
        
        Int n_nodes;
        RealArray lats;
        RealArray lons;
};

}
#endif
