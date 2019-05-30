#ifndef _SS_MPI_MANAGER_HPP_
#define _SS_MPI_MANAGER_HPP_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include <string>

namespace StrideSearch {

class MPIManager {
    public:
        enum Strategy {DISTRIBUTE_FILES, DISTRIBUTE_TIMESTEPS};
        
        MPIManager(const Index nitems_, const Int rank_=0, const Int nprocs=1, 
            const Strategy& st=DISTRIBUTE_FILES) : 
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
        
    protected:
        std::vector<Index> procStartIndex;
        std::vector<Index> procWorkLength;
        Strategy strat;
    
        Index nItems;
        Int numProcs;
        Int rank;
};

}
#endif