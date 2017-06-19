#ifndef _STRIDE_SEARCH_DATA_BASE_H_
#define _STRIDE_SEARCH_DATA_BASE_H_

#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchDateTime.h"
#include "StrideSearchWorkspaceDict.h"
#include "StrideSearchSector.h"
#include "StrideSearchIDCriteria_Base.h"
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
            @param varnames names to be used in search
        */
        StrideSearchData(const std::string fname) : filename(fname), fileNTimesteps(0),  totalNTimesteps(0) {
            initTime();
        };
        
        /// Destructor.
        virtual ~StrideSearchData(){};

        /// Array of integers that describe a grid.  
        /**
            Read a descriptive array of integers from a data file's grid variables.  For a uniform lat-lon grid, for
            example, the array would be [nLat, nLon].  
            For unstructured grids, the array could be [nNodes, nElem], etc.
        */
        virtual void getGridDescription(index_type* gridDescInts) const = 0;
        
        /// Update the source file for this data object. Used for advancing to the next file in a data set.
        void updateSourceFile(std::string fname);
        
        /// Initialize the time variable, read values into memory.
        /**
            Must be called once per file.
        */
        void initTime();
        
        std::string getFilename() const; 
        
        virtual void loadSectorWorkingData(Sector* sec, const index_type& tInd, const index_type& levInd = -1) = 0;

        std::vector<scalar_type> lons;
        std::vector<scalar_type> lats;
        
        std::string infoString() const;
        
    protected:
        /// Initialize the grid description variables, read values into memory. 
        /** 
            Must be called once per data set.
        */
        virtual void initDimensions() = 0;
    
        /// filename of current data file
        std::string filename;
        
        /// time variable of the current file
        std::vector<scalar_type> time;
        
        /// number of timesteps in the current file       
        int fileNTimesteps;
        
        /// total number of timesteps in the data set (so far)
        int totalNTimesteps;
};

}
#endif
