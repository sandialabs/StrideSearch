#include "SSWorkspace.hpp"

namespace StrideSearch {

Workspace::Workspace(const std::string& var, const Index nn) : n(nn) {
    data.emplace(var, RealArray(n,0.0));
}

Workspace::Workspace(const std::vector<std::string>& vars, const Index nn) : n(nn) {
    for (auto& var : vars)
        data.emplace(var, RealArray(n,0.0));
}

const RealArray& Workspace::getConstDataRef(const std::string& var) const {
    return const_cast<RealArray&>(data.at(var));
}

RealArray& Workspace::getDataRef(const std::string& var) {
    return data.at(var);
}

std::ostream& operator << (std::ostream& os, const Workspace& wspc) {
    for (auto& elem : wspc.data) {
        os << elem.first << ": ";
        for (Index i=0; i<elem.second.size(); ++i) {
            os << elem.second[i] << " ";
        }
        os << std::endl;
    }
    return os;
}

}
