#ifndef _STRIDE_SEARCH_SEARCH_MANAGER_HPP_
#define _STRIDE_SEARCH_SEARCH_MANAGER_HPP_

#include "SSDefs.hpp"
#include "StrideSearchConfig.h"
#include "SSUtilities.hpp"
#include "SSDataLayoutTraits.hpp"
#include "SSNCReader.hpp"
#include "SSWorkspace.hpp"
#include "SSIdCriteria.hpp"
#include "SSSector.hpp"
#include "SSSectorSet.hpp"
#include "SSEvent.hpp"
#include "SSEventSet.hpp"
#include "SSCollocCriteria.hpp"
#include <string>
#include <vector>
#include <array>
#include "SSMPIManager.hpp"
#include "SSSearchParams.hpp"
// #ifdef HAVE_MPI
// #include <mpi.h>
// #endif

namespace StrideSearch {




/// Spatial search driver.
/**
    The user must provide a search region (a rectangle in lat-lon space, possibly the whole globe) and a spatial scale.  The spatial scale defines the StrideSearch::Sector radius; any two StrideSearch::Event s detected within this radius may be consider part of the same storm.
    
    Users provide a definition of what a ``storm'' is through the use of StrideSearch::IDCriterion subclasses and StrideSearch::CollocationCriterion instances. 
    For example, a simple barotropic low could be defined as a pressure minimum (StrideSearch::MinCriterion) collocated (StrideSearch::CollocationCriterion) with a cyclonic vorticity maximum (StrideSearch::MaxSignedCriterion).
    
    Users provide the filenames of their data set as well as its start date, formatted as a StrideSearch::DateTime.
    
    Upon completion, search results are stored in a StrideSearch::EventSet, the StrideSearch::SearchManager::main_event_set member variable.
    
    Examples
    ----------
    - SSSearchManagerUnitTests.cpp
    - SSTropicalCycloneTest.cpp
    - SSTropicalCyclone.cpp
    - SSSouthernExtraTrop.cpp
    
*/
template <typename DataLayout=UnstructuredLayout>
class SearchManager {
    public:
        typedef std::shared_ptr<NCReader> reader_ptr;
        typedef std::shared_ptr<IDCriterion> crit_ptr;
        typedef std::shared_ptr<Event<DataLayout>> event_ptr;
        typedef std::shared_ptr<CollocationCriterion> colloc_ptr;
    
        /// Constructor.  Main user entry point to Stride Search.
        /** 
            @param sreg : search region, rectangle in lat-lon space (south, north, west, east)
            @param srad : sector radius
        */
        SearchManager(const region_type& sreg, const Real srad) : 
            region(sreg), sector_radius(srad), main_sector_set(sreg[0], sreg[1], sreg[2], sreg[3], srad), 
            main_event_set(), reader(), tree(), filenames(), start_date(), criteria(), locator_crit(), 
            colloc_criteria() {} 
        
        /// Set data set start date.
        /**
            @warning Some netCDF data sets have start year = 0; this can cause problems.  
                See DateTime for additional discussion.
        */
        void setStartDate(const DateTime& sd) {start_date = sd;}
        
        /// Define the input files that contain the netCDF data.
        /**
            @note This function will run setup a serial search.
        */
        void setInputFiles(const std::vector<std::string>& fnames, const SearchParams& params = SearchParams()); 
        
        /// Define the input files that contain the netCDF data for this rank
        /**
            @note This function will set up a parallel search, dividing either files or timesteps between MPI ranks, depending on StrideSearch::MPIDistribute value.
        */
        void setInputFiles(const MPIManager& mpi, const std::vector<std::string>& allfiles);
        
        /// Set the definition of a "storm" as a set of identification criteria.
        /**
        */
        void defineCriteria(const std::vector<crit_ptr>& cs, const std::vector<colloc_ptr>& cpairs = 
            std::vector<colloc_ptr>());
        
        /// Output SearchManager state info to string (e.g., for console output)
        std::string infoString() const;
        
        /// run spatial search on complete data set
        /**
            @param stop_timestep : the timestep loop will only search this many timesteps. 
                Values > 0 are used for testing.
        */
        void runSpatialSearch(const Int stop_timestep=-1);
        
        /// run spatial search on complete data set
        /**
            @param params : SearchParameters
            @param stop_timestep : the timestep loop will only search this many timesteps. 
                Values > 0 are used for testing.
        */
        void runSpatialSearch(const SearchParams& params, const Int stop_timestep=-1);
        
        /// run spatial search on complete data set
        /**
            @note For MPI, use only one output stream per rank.
            @param mpi : MPI rank-to-file or rank-to-timestep map.
            @param params : SearchParameters
            @param stop_timestep : the timestep loop will only search this many timesteps. 
                Values > 0 are used for testing.
        */
        void runSpatialSearch(const MPIManager& mpi, const SearchParams& params, const Int stop_timestep=-1);
    
        /// Outputs a delimited file in order of ascending DateTime.
        /**
            The delimiter is a semicolon ';'.
            If the output is saved to a file, say, "output.txt" it may be read into a Pandas DataFrame in Python with@n
                @code{.py}
                import pandas as pd 
                df = pd.read_csv("output.txt", sep=';')
                @endcode
        */
        void outputCSV(std::ostream& os) const;
        
        void printTime() const;
    
    protected:
        /// Search domain (rectangle in lat-lon space, in degrees; latitudes between -90 and 90, longitudes between 0 and 360
        region_type region;
        /// Sector radius in kilometers
        Real sector_radius;
        /// Base search SectorSet.  Used by StrideSearch::SearchManager::runLocatorAtTimestep.
        SectorSet<DataLayout> main_sector_set;
        /// Primary result from StrideSearch::runSpatialSearch.
        EventSet<DataLayout> main_event_set;
        /// Array of time values read from the current file.
        RealArray file_time;
        
        /// NCReader for current file.
        reader_ptr reader;
        /// KDTree for whole data set
        std::unique_ptr<KDTree> tree;
        /// List of files for this rank
        std::vector<std::string> filenames;
        /// Start date of data set
        DateTime start_date;
        /// Identification criteria
        std::vector<std::shared_ptr<IDCriterion>> criteria;
        /// Locator criterion
        std::shared_ptr<IDCriterion> locator_crit;
        /// Collocation criterion
        std::vector<colloc_ptr> colloc_criteria;
        
        /// Locates possible events using only the LocatorCriterion.  
        /** 
            This is step 1 of the per-timestep search algorithm.
            Its results are the input to StrideSearch::SearchManager::investigatePossibles.
        */
        std::vector<event_ptr> runLocatorAtTimestep(const Index time_ind);
        
        /// Creates a new SectorSet centered on each possible Event, evaluates all criteria in each Sector.
        /**
            This is step 2 of the per-timestep search algorithm.
            Its results are the input to StrideSearch::SearchManager::processCollocations.
        */
        EventSet<DataLayout> investigatePossibles(const Index time_ind, 
            const std::vector<event_ptr>& poss);
            
        /// Removes duplicates and collates related events.
        /** This is step 3 of the per-timestep search algorithm.
            Its results are appended to StrideSearch::SearchManager::main_event_set.
        */
        void processCollocations(EventSet<DataLayout>& events) const;
        
        /// Run spatial search on a file.
        void runfile(const Int f_ind, const Int stop_timestep=-1);
        
        /// Run spatial search on a file
        void runfile(const Int f_ind, const SearchParams& params, const Int stop_timestep=-1);
        
        /// Run spatial search on one timestep
        void runTimestepSearch(const Int t_ind);
                
    private:
        template <typename DL> typename
        std::enable_if<std::is_same<DL,UnstructuredLayout>::value, reader_ptr>::type
        readerHelper() const;
        
        template <typename DL> typename
        std::enable_if<std::is_same<DL,LatLonLayout>::value, reader_ptr>::type
        readerHelper() const;
};

}

#endif
