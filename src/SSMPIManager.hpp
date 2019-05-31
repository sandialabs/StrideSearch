#ifndef _SS_MPI_MANAGER_HPP_
#define _SS_MPI_MANAGER_HPP_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include <string>

namespace StrideSearch {

enum MPIDistribute {FILES, TIMESTEPS};

class MPIManager {
    public:
        
        MPIManager(const Index nitems_, const Int rank_=0, const Int nprocs=1, 
            const MPIDistribute& st=FILES) : 
            nItems(nitems_), numProcs(nprocs), rank(rank_), 
            procStartIndex(nprocs,-1), procWorkLength(nprocs,-1), strat(st) {
            distribute();
        }
        
        inline Index startInd(const Int procrank) const {return procStartIndex[procrank];}
        inline Index workSize(const Int procrank) const {return procWorkLength[procrank];}    
        
        std::string infoString() const;
        
        void distribute();
        
        inline Int getRank() const {return rank;}
        inline Int getNumProcs() const {return numProcs;}
        inline MPIDistribute parallelStrategy() const {return strat;}
        
    protected:
        std::vector<Index> procStartIndex;
        std::vector<Index> procWorkLength;
        MPIDistribute strat;
    
        Index nItems;
        Int numProcs;
        Int rank;
};

}
#endif