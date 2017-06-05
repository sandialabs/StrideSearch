#ifndef _STRIDE_SEARCH_DATA_BASE_H_
#define _STRIDE_SEARCH_DATA_BASE_H_

#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchDateTime.h"
#include "StrideSearchWorkspace.h"
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
        StrideSearchData(const std::string fname, const std::vector<std::string>& varnames) : 
            filename(fname), variables(varnames), fileNTimesteps(0),  totalNTimesteps(0) {
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
        
        /// Return a local workspace filled with search data for a particular search region.
        virtual Workspace1D getSectorWorkingData(const std::vector<std::string>& varnames, 
            const std::vector<ll_index_type>& dataIndices) = 0;
        
        /// Update the source file for this data object. Used for advancing to the next file in a data set.
        void updateSourceFile(std::string fname);
        
        /// Initialize the time variable, read values into memory.
        /**
            Must be called once per file.
        */
        void initTime();
        
        std::string getFilename() const; 
        
        /// Read a set of 2D data at a particular time index and model level.
        virtual void read2DDataFromTimestep(const index_type time_index, const index_type level_index = 0) = 0;

    protected:
        /// Initialize the grid description variables, read values into memory. 
        /** 
            Must be called once per data set.
        */
        virtual void initDimensions() = 0;
    
        std::string filename;
        std::vector<std::string> variables;
        
        std::vector<scalar_type> time;
       
        int fileNTimesteps;
        int totalNTimesteps;
};

}
#endif
