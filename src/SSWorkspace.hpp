#ifndef _SS_WORKSPACE_HPP_
#define _SS_WORKSPACE_HPP_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include <map>
#include <vector>
#include <string>
#include <iostream>

namespace StrideSearch {
/// Local memory (to each Sector) for the variables associated with a set of IDCriterion subclasses.
/** A Workspace is a set of dictionary whose keys are variable names and whose values arrays of data read from 
    a netCDF file.  
    
    
    Each IDCriterion has its own workspace, but the workspace may contain multiple data variables if the IDCriterion
    requires.    
    For example, the MaxMagnitudeCriterion may require data from separate components of a vector (e.g., wind).@n
    Each of these components may have its own variable name in the netCDF data (e.g., 'U850', 'V850', etc.)
    
*/
struct Workspace {
    /// A workspace simply holds scalar data values
    /**
        The container is a std::map; key = variable name, value = vector of data values
    */
    std::map<std::string, RealArray> data;
    
    /// Default constructor, creates null workspace.
    Workspace() : data() {}
    
    /// Constructor with memory allocation
    Workspace(const std::string& var, const Index nn);
    
    /// Constructor with memory allocation
    Workspace(const std::vector<std::string>& vars, const Index nn);
    
    /// Fill workspace with data from a predefined array.  
    /**
     @note Used for testing.
    */
    void fillData(const std::string& var, const RealArray& vals);
    
    /// Return a constant (read only) reference to the data stored in one workspace variable.
    const RealArray& getConstDataRef(const std::string& var) const;
    
    /// Return a non-const reference to workspace data 
    RealArray& getDataRef(const std::string& var);
    
    /// Return the number of variables used by *this
    inline Int nVars() const {return data.size();}

    Int n;
};

std::ostream& operator << (std::ostream& os, const Workspace& wspc);

}
#endif
