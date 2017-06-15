#ifndef _STRIDE_SEARCH_WORKSPACE_DICT_H_
#define _STRIDE_SEARCH_WORKSPACE_DICT_H_

#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include <map>
#include <vector>
#include <string>
#include <iostream>

namespace StrideSearch {


/** A Workspace is a set of dictionaries whose keys are variable names and whose values arrays of data read from 
    a netCDF file.  Each IDCriterion has its own workspace.
    
*/
struct WorkspaceDict {
    std::map<std::string, std::vector<scalar_type>> dict;

    WorkspaceDict() : dict() {};
    WorkspaceDict(const std::string& varname, const index_type nDataInds);
    WorkspaceDict(const std::vector<std::string>& varnames, const index_type nDataInds);
    
    void fillData(const std::string varname, const std::vector<scalar_type> vals);
    
    std::vector<scalar_type>& getConstDataReference(const std::string& varname);
};

// template <index_type n> struct WorkspaceDict {
//     std::map<std::string, std::unique_ptr<scalar_type[n]>> dict;
// }

}

std::ostream& operator << (std::ostream& os, const StrideSearch::WorkspaceDict& wspc);

#endif
