#ifndef _SS_NC_READER_HPP_
#define _SS_NC_READER_HPP_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSDataLayoutTraits.hpp"
#include "SSConsts.hpp"
#include "SSUtilities.hpp"
#include "SSWorkspace.hpp"
#include "SSDateTime.hpp"
#include <netcdf>
#include <memory>
#include <map>
#include <vector>
#include <cmath>

namespace StrideSearch {

/// Defines a common data structure for all data sets to use with KDTree interface to nanoflann TPL.
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
    __Properties:__ @n
        An NCReader has unique ownership of a `netCDF::NcFile`.  
    
    __Responsibilities:__ @n
        The format of netcdf data is not standardized. 
        Some grids are unstructured, some are structured; some are given in angular variables (e.g., lat-lon),
        others are given in Cartesian variables (x,y,z).  
        For the Cartesian variables, some are given on the unit sphere, others have physical dimensions.
        
        The NCReader class creates Points data that the rest of Stride Search can rely on to be
            1. In Cartesian (x,y,z) coordinates.
            2. On an Earth-sized sphere with units of kilometers.

    __Example subclasses:__ @n
        - LatLonNCReader
        - UnstructuredNCReader

    __Required subclass methods:__
        - makePoints() const : return a Points object of Cartesian coordinates, each with magnitude = EARTH_RADIUS_KM
        - nPoints() const : return the number of horizontal grid points in the data set
        - getLat(const Index ind) const: Return the latitude of the data point in the Points object
        - getLon(const Index ind) const: Return the latitude of the data point in the Points object
        - fillWorkspaceData() : Fill the workspace's required data arrays with values read from NcFile
        - resolutionEstimate() : Return an estimate of the dataset's average horizontal resolution, in km.

*/
class NCReader {
    public:
        virtual ~NCReader()  {}
        
        /// Returns a collection of x, y, z points for use with KDTree and nanoflann
        virtual Points makePoints() const = 0;
        
        /// Returns the number of horizontal grid points
        virtual Int nPoints() const = 0;
        
        /// Return the latitude of the point with index ptInd (returned from KDTree::search)
        virtual Real getLat(const Index ptInd) const = 0;
        
        /// Return the longitude of the point with index ptInd (returned from KDTree::search)
        virtual Real getLon(const Index ptInd) const = 0;
        
        /// Fill workspace data with values read from file
        /**
            The Workspace defines the variables it needs to complete its computations.
            The data indices corresponding to the Workspace's Sector are the second argument.
            
            Procedure:@n
            For each every variable name in Workspace.data, load datum for each horizontal index.
            
            @param wspc : Workspace with memory already allocated.
            @param inds : Horizontal grid point indices
            @param t_ind : time index in current source data file.
            @param l_ind : level index (if applicable)
        */
        virtual void fillWorkspaceData(Workspace& wspc, const std::vector<typename UnstructuredLayout::horiz_index_type>& inds, const Int t_ind, const Int l_ind=-1) const {}
        
        /// Fill workspace data with values read from file
        /**
            The Workspace defines the variables it needs to complete its computations.
            The data indices corresponding to the Workspace's Sector are the second argument.
            
            Procedure:@n
            For each every variable name in Workspace.data, load datum for each horizontal index.
            
            @param wspc : Workspace with memory already allocated.
            @param inds : Horizontal grid point indices
            @param t_ind : time index in current source data file.
            @param l_ind : level index (if applicable)
        */
        virtual void fillWorkspaceData(Workspace& wspc, const std::vector<typename LatLonLayout::horiz_index_type>& inds, const Int t_ind, const Int l_ind=-1) const {}
        
        /// returns the time values contained in the NcFile
        /**
            @warning Throughout Stride Search, time values read from netCDF files are assumed to be defined 
            in units of days since start; they are therefore floating point (not integral) type. For 
            data defined with time in other units, use StrideSearch::DTUnits.

            @throws std::runtime_error if *this cannot locate a time coordinate variable in ncfile.
        */
        RealArray getTime(const DTUnits& dtu=DTUnits::DAYS) const;  
        
        /// Updates reader to use a new source file in the same data set (the same horizontal grid is assumed).
        void updateFile(const std::string& filename);
        
        /// Returns the curent source filename
        std::string filename() const {return src_file;}
        
        /// Print coordinates to console
        void printLats() const;
        /// Print coordinates to console
        void printLons() const;
        
        /// Average horizontal resolution (in kilometers) of the data set.
        Real avgResKm;
        
        /// Returns a string with the NCReader's basic info.s
        std::string infoString(const Int tab_lev=0) const;

    protected:
        NCReader() {}
        NCReader(const std::string& filename); 
        
        /// Ptr to `netCDF::NcFile`
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
        
        virtual Real resolutionEstimate() const = 0;
        
        /// Latitude coordinates of data points
        RealArray lats;
        /// Longitude coordinates of data points
        RealArray lons;

};


///    Instantiation of NCReader for lat-lon structured grids
/**
    This class's makePoints method packs the 2d data into a 1d array of points;
    the inverse of this packing is computed as part of the LatLonNCReader::getLat and LatLonNCReader::getLon methods.
*/
class LatLonNCReader : public NCReader {
    public: 
        typedef typename LatLonLayout::horiz_index_type data_index_type;
        typedef LatLonLayout Layout;
        
        /// Returns a collection of x, y, z points for use with nanoflann
        Points makePoints() const override;    
    
        LatLonNCReader(const std::string& filename) : NCReader(filename) {
            initCoordinates();
            avgResKm = resolutionEstimate();
        }
        
        ~LatLonNCReader() {}
    
        /// Get data indices from tree indices
        /**
            @deprecated Use DataLayoutTraits::getDataIndexFromTreeIndex instead.
        */
        void llIndices(Int& lat_ind, Int& lon_ind, const Int& pt_ind) const {
            lat_ind = pt_ind/n_lon;
            lon_ind = pt_ind%n_lon;
        }
    
        /// Returns the number of horizontal grid points
        inline Int nPoints() const override {return n_lat*n_lon;}
        
        /// Get data indices from tree indices
        /**
        */
        data_index_type getDataIndexFromTreeIndex(const Index ptInd) const {
            return LatLonLayout::getDataIndexFromTreeIndex(ptInd, n_lon);
        }
        
        /// Return the latitude of the point with index ptInd (returned from KDTree::search)
        /**
            @param ptInd : index of a point output from a nanoflann search, implemented by the KDTree class.
            @return latitude of the corresponding point
        */
        Real getLat(const Index ptInd) const override {return lats[ptInd/n_lon];}
        
        /// Return the longitude of the point with index ptInd (returned from KDTree::search)
        /**
            @param ptInd : index of a point output from a nanoflann search, implemented by the KDTree class.
            @return longitude of the corresponding point
        */
        Real getLon(const Index ptInd) const override {return lons[ptInd%n_lon];}
        
        /// Fill workspace data with values read from file
        /**
            The Workspace defines the variables it needs to complete its computations.
            The data indices corresponding to the Workspace's Sector are the second argument.
            
            Procedure:@n
            For each every variable name in Workspace.data, load datum for each horizontal index.
            
            @param wspc : Workspace with memory already allocated.
            @param inds : Horizontal grid point indices
            @param t_ind : time index in current source data file.
            @param l_ind : level index (if applicable)
        */
        void fillWorkspaceData(Workspace& wspc,
            const std::vector<typename LatLonLayout::horiz_index_type>& inds,
            const Int t_ind, const Int l_ind=-1) const override;
        
        
        
    protected:
        /// number of latitude points in data set (frequently, `n_lat = n_lon/2 + 1`)
        Int n_lat;
        /// number of longitude points in data set
        Int n_lon;
    
        /// Initializes coordinate data by reading latitude and longitude data from file.
        void initCoordinates() override;
        
        /// Estimate the data set's average horizontal resolution, in kilometers
        /**
            @return @f$\Delta \lambda = \frac{2\pi R}{n_{lon}}@f$
        */
        Real resolutionEstimate() const override {return 2*PI*EARTH_RADIUS_KM/n_lon;}
};



///    Instantiation of NCReader for unstructured grids
/**
    This class assumes a 1-1 mapping between data points and Points points.
*/
class UnstructuredNCReader : public NCReader {
    public: 
        typedef typename UnstructuredLayout::horiz_index_type data_index_type;
        typedef UnstructuredLayout Layout;
        
        /// Returns a collection of x, y, z points for use with nanoflann
        Points makePoints() const override;
    
        UnstructuredNCReader(const std::string& filename) : NCReader(filename) {
            initCoordinates();
            avgResKm = resolutionEstimate();
        }
        
        ~UnstructuredNCReader() {}
    
        /// Returns the number of horizontal grid points.
        inline Int nPoints() const override {return n_nodes;}
        
        /// Get data indices from tree indices
        /**
            
        */
        data_index_type getDataIndexFromTreeIndex(const Index ptInd) const {
            return UnstructuredLayout::getDataIndexFromTreeIndex(ptInd);
        }
        
        /// Return the latitude of the point with index ptInd (returned from KDTree::search)
        /**
            @param ptInd : index of a point output from a nanoflann search, implemented by the KDTree class.
            @return latitude of the corresponding point
        */
        Real getLat(const Index ptInd) const override {return lats[ptInd];}
        /// Return the longitude of the point with index ptInd (returned from KDTree::search)
        /**
            @param ptInd : index of a point output from a nanoflann search, implemented by the KDTree class.
            @return longitude of the corresponding point
        */
        Real getLon(const Index ptInd) const override {return lons[ptInd];}
        
        /// Fill workspace data with values read from file
        /**
            The Workspace defines the variables it needs to complete its computations.
            The data indices corresponding to the Workspace's Sector are the second argument.
            
            Procedure:@n
            For each every variable name in Workspace.data, load datum for each horizontal index at time_index t_ind
            
            @param wspc : Workspace with memory already allocated.
            @param inds : Horizontal grid point indices
            @param t_ind : time index in current source data file.
            @param l_ind : level index (if applicable)
        */
        void fillWorkspaceData(Workspace& wspc, 
            const std::vector<typename UnstructuredLayout::horiz_index_type>& inds, 
            const Int t_ind, const Int l_ind=-1) const override;

    protected:
    
        /// Estimate the data set's average horizontal resolution, in kilometers
        /**
            @return @f$\Delta \lambda = \sqrt{\frac{4\pi R^2}{n_{nodes}}}@f$
        */
        Real resolutionEstimate() const override {
            return std::sqrt(4*PI*SQ_EARTH_RADIUS_KM/n_nodes);}
    
        /// Initializes x, y, z, arrays by reading coord* data from file.
        void initCoordinates() override;
        
        /// Number of nodes in data set
        Int n_nodes;

};

}
#endif
