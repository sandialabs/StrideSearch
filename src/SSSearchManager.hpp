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
#ifdef HAVE_MPI
#include <mpi.h>
#endif

namespace StrideSearch {

/// Rectangle in lat-lon space (south, north, west, east)
typedef std::array<Real,4> region_type;


/// Spatial search driver.
/**
    
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
            main_event_set(), reader(), tree(), filenames(), start_date(), criteria(), locator_crit() {} 
        
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
            @note This function will set up a parallel search, dividing files between MPI ranks.
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
            @note If using MPI, use only one output stream per rank.
            @param stop_timestep : the timestep loop will only search this many timesteps. 
                Values > 0 are used for testing.
        */
        void runSpatialSearch(const Int stop_timestep=-1);
        
        void runSpatialSearch(const SearchParams& params, const Int stop_timestep=-1);
        
        void runSpatialSearch(const MPIManager& mpi, const Int stop_timestep=-1);
    
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
        region_type region;
        Real sector_radius;
        SectorSet<DataLayout> main_sector_set;
        EventSet<DataLayout> main_event_set;
        
        RealArray file_time;
        
        reader_ptr reader;
        std::unique_ptr<KDTree> tree;
        std::vector<std::string> filenames;
        DateTime start_date;
        std::vector<std::shared_ptr<IDCriterion>> criteria;
        std::shared_ptr<IDCriterion> locator_crit;
        std::vector<colloc_ptr> colloc_criteria;
        
        EventSet<DataLayout> investigatePossibles(const Index time_ind, 
            const std::vector<event_ptr>& poss);
            
        std::vector<event_ptr> runLocatorAtTimestep(const Index time_ind);
        
        void processCollocations(EventSet<DataLayout>& events) const;
        
        /// Run spatial search on a file.
        void runfile(const Int f_ind, const Int stop_timestep=-1);
        
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
