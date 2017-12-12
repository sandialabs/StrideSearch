#ifndef _STRIDE_SEARCH_DATA_BASE_H_
#define _STRIDE_SEARCH_DATA_BASE_H_

#include "StrideSearchConfig.h"
#include "StrideSearchTypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchDateTime.h"
#include "StrideSearchWorkspaceDict.h"
#include "StrideSearchSector.h"
#include "StrideSearchIDCriterionBase.h"
#include <string>
#include <vector>
#include <map>

namespace StrideSearch {

/// Base class for data access. Must be subclassed for particular grid types.
/**
Base class for defining StrideSearch's data access.  Hides interfaces to netCDF c++ API.
*/
class StrideSearchData {
    public:
        /// Constructor.
        /**
            @param fname Input file with source dataIndices
        */
        StrideSearchData(const std::string fname) : filename(fname), fileNTimesteps(0),  totalNTimesteps(0) {};
        
        /// Destructor.
        virtual ~StrideSearchData(){};

        /// Array of integers that describe a grid.  
        /**
            Read a descriptive array of integers from a data file's grid variables.  For a uniform lat-lon grid, for
            example, the array would be [nLat, nLon].  
            For unstructured grids, the array could be [nNodes, nElem], etc.
            
            @todo This is not used -- remove it.
        */
        virtual void getGridDescription(index_type* gridDescInts) const {};
        
        /// Update the source file for this data object. Used for advancing to the next file in a data set.
        void updateSourceFile(const std::string fname);
        
        /// Initialize the time variable, read values into memory.
        /**
            Must be called once per file.
        */
        void initTime();
        
        /// Returns the current filename.
        inline std::string getFilename() const {return filename;}

        /// Returns the number of timesteps in the current file.  
        /**
            Requires initTime() to have finished previously.
        */
        inline index_type nTimesteps() const {return fileNTimesteps;}
        
        /// Returns a time variable value.
        /** 
            Requires initTime() to have finished previously.
        */
        inline scalar_type getTime(const index_type i) const {return time[i];}
        
        /// Read data from file into a Sector's local memory.
        /**
            This method may be specialized for structured grid types using sub-classes.
        */
        virtual void loadSectorWorkingData(Sector* sec, const index_type& tInd, const index_type& levInd = -1);

        /// Longitude values of each grid point.  
        /**
            Read from file, defined by initDimensions().
        */
        std::vector<scalar_type> lons;
        
        /// Latitude values of each grid point.
        /**
            Read from file, defined by initDimensions().
        */
        std::vector<scalar_type> lats;
        
        /// Basic info about this StrideSearchData instance, output to a string.
        std::string infoString() const;
        
        /// True if the *horizontal* grid associated with the data set has a one-dimensional layout in the data file.
        /**
            One dimensional layouts are common for unstructured grid types.  For example, a common 1d horizontal layout
            for surface variables has NetCDF dimensions (time, nodeID), while 3d variables with 1d horizontal layouts
            have NetCDF dimensions (time, level, nodeID).
        */
        bool layout1d() const {return oneD;}
        
        /// True if the *horizontal* grid associated with the data set has a two-dimensional layout in the data file.
        /**
            2d layouts are common for uniform latitude-longitude grids.  For example, a usual 2d horizontal layout for
            surface variables has NetCDF dimensions (time, lat, lon), while a 3d variable will have NetCDF dimensions
            (time, level, lat, lon).
        */
        bool layout2d() const {return twoD;}
        
        /// Initialize the grid description variables, read values into memory. 
        /** 
            Must be called once per data set.
            Populates the lats and lons member variables.
        */
        virtual void initDimensions();
        
        inline index_type nPoints() const {return _nPoints;}
        
        index_type get1dIndex(const index_type latI, const index_type lonJ) const;
        std::pair<index_type, index_type> get2dIndex(const index_type ind) const;
        
    protected:
        /// filename of current data file
        std::string filename;
        
        /// True for grids whose horizontal memory layouts have rank = 1. 
        /**
            See layout1d() for more information.
        */
        bool oneD;
        
        /// True for grids whose horizontal memory layouts have rank = 2.
        /**
            See layout2d() for more information.
        */
        bool twoD;
        
        /// time variable of the current file
        std::vector<scalar_type> time;
        
        /// number of timesteps in the current file       
        index_type fileNTimesteps;
        
        /// total number of timesteps in the data set (so far)
        index_type totalNTimesteps;
        
        /// number of horizontal grid points
        index_type _nPoints;
 
};

}
#endif
