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

std::vector<scalar_type>& WorkspaceDict::getConstDataReference(const std::string& varname) {
    return dict.at(varname);
}

void WorkspaceDict::fillData(const std::string varname, const std::vector<scalar_type> vals) {
    for (index_type i = 0; i < std::min( dict.at(varname).size(), vals.size()); ++i) {
        dict.at(varname)[i] = vals[i];
    }
}

}// namespace

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



