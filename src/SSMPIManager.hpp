#ifndef _SS_MPI_MANAGER_HPP_
#define _SS_MPI_MANAGER_HPP_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include <string>

namespace StrideSearch {

/// MPI Parallel strategy.  
enum MPIDistribute {
                     /// Distribute files over MPI ranks.
                    FILES,
                    /// Distribute time steps over MPI ranks.
                    TIMESTEPS};

/// Maps nItems to numProcs ranks to distribute work.
/**
    
*/
class MPIManager {
    public:
        /// Constructor
        /**
            @param nitems : total number of items (e.g., files or time steps)
            @param rank : rank, output from `MPI_Comm_rank`
            @param nproc : number of processors, output from `MPI_Comm_size`
        */    
        MPIManager(const Index nitems_, const Int rank_=0, const Int nprocs=1, 
            const MPIDistribute& st=FILES) : 
            nItems(nitems_), numProcs(nprocs), rank(rank_), 
            procStartIndex(nprocs,-1), procWorkLength(nprocs,-1), strat(st) {
            distribute();
        }
        
        /// Return the starting index for an MPI rank
        /**
            @param procrank : MPI rank
            @return starting index in [0, nItems)
        */
        inline Index startInd(const Int procrank) const {return procStartIndex[procrank];}
        /// Return the number of items process procrank will work on
        /**
            @param procrank : MPI rank
            @return number of items to work on (nItems/numProcs, typically).
        */
        inline Index workSize(const Int procrank) const {return procWorkLength[procrank];}    
        
        /// Return a string containing data about the parallel work distribution
        std::string infoString() const;
        
        /// Compute the item-to-rank mapping
        void distribute();
        
        /// Get the rank of *this process
        inline Int getRank() const {return rank;}
        /// Get the size of the communicator
        inline Int getNumProcs() const {return numProcs;}
        /// Get the parallel strategy
        inline MPIDistribute parallelStrategy() const {return strat;}
        
    protected:
        /// Starting indices for each process
        std::vector<Index> procStartIndex;
        /// Workset sizes for each process
        std::vector<Index> procWorkLength;
        /// Parallel strategy
        MPIDistribute strat;
        /// Total number of items distributed
        Index nItems;
        /// Total number of processes, output from `MPI_Comm_size`
        Int numProcs;
        /// Rank of this process, output from `MPI_Comm_rank`
        Int rank;
};

}
#endif