#ifndef _STRIDE_SEARCH_WORKSPACE_DICT_H_
#define _STRIDE_SEARCH_WORKSPACE_DICT_H_

#include "StrideSearchConfig.h"
#include "StrideSearchTypeDefs.h"
#include <map>
#include <vector>
#include <string>
#include <iostream>

namespace StrideSearch {

/// Space for each sector to have its own local memory for the variables associated with a set of IDCriterion subclasses.
/** A Workspace is a set of dictionaries whose keys are variable names and whose values arrays of data read from 
    a netCDF file.  
    
    Each IDCriterion has its own workspace, but the workspace may contain multiple data variables if the IDCriterion
    requires.
    
    @todo It's possible that this class could be sped up with a templated implementation (code commented below)
*/
struct WorkspaceDict {
    /// A workspace simply holds scalar data values
    /**
        The container is a std::map key = variable name, value = vector of data values
    */
    std::map<std::string, std::vector<scalar_type>> dict;

    /// Default constructor, creates empty workspace.
    WorkspaceDict() : dict() {};
    /// Constructor with memory allocation
    WorkspaceDict(const std::string& varname, const index_type nDataInds);
    /// Constructor with memory allocation
    WorkspaceDict(const std::vector<std::string>& varnames, const index_type nDataInds);
    
    /// Fill workspace with data from a std::vector. Used for testing.
    void fillData(const std::string varname, const std::vector<scalar_type> vals);
    
    /// Return a constant (read only) reference to the data stored in one workspace variable.
    std::vector<scalar_type>& getDataReference(const std::string& varname);
    
    /// Return the number of variables used by *this
    inline index_type nVariables() const {return dict.size();}
};

}

std::ostream& operator << (std::ostream& os, const StrideSearch::WorkspaceDict& wspc);

#endif
