#include "SSMPIManager.hpp"
#ifdef HAVE_MPI

#include <sstream>
#include <algorithm>

namespace StrideSearch {

void MPIManager::distribute() {
    const Index chunk_size = nItems/numProcs;
    for (Int i=0; i<numProcs; ++i) {
        procStartIndex[i] = i*chunk_size;
        procWorkLength[i] = chunk_size;
    }
    procWorkLength[numProcs-1] = nItems - procStartIndex[numProcs-1];
}

std::string MPIManager::infoString() const {
    std::ostringstream ss;
    ss << "MPIManager record:\n";
    ss << "\tdistributed " << nItems << (strat == DISTRIBUTE_FILES ? " files" : 
        (strat==DISTRIBUTE_TIMESTEPS ? " timesteps" : "null")) << " over " << numProcs << " ranks.\n";
    auto minmaxlocs = std::minmax_element(procWorkLength.begin(), procWorkLength.end());
    ss << "\tmin work size = " << *minmaxlocs.first << " max work size = " << *minmaxlocs.second << '\n';
    for (int i=0; i<numProcs; ++i) {
        ss << "\t\trank " << i << " (startInd, workSize) = (" 
           << procStartIndex[i] << ", " << procWorkLength[i] << ")\n";
    }
    return ss.str();
}

}
#endif