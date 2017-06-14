#include "StrideSearchWorkspaceDict.h"

namespace StrideSearch {

WorkspaceDict::WorkspaceDict(const std::vector<std::string>& varnames, const index_type nDataInds) {
    for (auto& var : varnames) {
        dict.emplace(var, std::vector<scalar_type>(nDataInds, 0.0));
    }
}

WorkspaceDict::WorkspaceDict(const std::string& varname, const index_type nDataInds) {
    dict.emplace(varname, std::vector<scalar_type>(nDataInds, 0.0));
}

}

std::ostream& operator << (std::ostream& os, const StrideSearch::WorkspaceDict& wspc) {
    for (auto& elem : wspc.dict) {
        os << elem.first << ": ";
        for (StrideSearch::index_type i = 0; i < elem.second.size(); ++i) {
            os << elem.second[i] << "  ";
        }
        os << std::endl;
    }
    return os;
}



