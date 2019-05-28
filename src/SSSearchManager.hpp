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
#include <string>
#include <vector>
#include <array>
#ifdef HAVE_MPI
#include <mpi.h>
#include "SSMpiManager.hpp"
#endif
#ifdef HAVE_OPENMP
#include <omp.h>
#endif

namespace StrideSearch {

/// Rectangle in lat-lon space (south, north, west, east)
typedef std::array<Real,4> region_type;

template <typename DataLayout=UnstructuredLayout>
class SearchManager {
    public:
        typedef std::shared_ptr<NCReader> reader_ptr;
        typedef std::shared_ptr<IDCriterion> crit_ptr;
        typedef std::pair<crit_ptr, crit_ptr> colloc_pair;
        typedef std::shared_ptr<Event<DataLayout>> event_ptr;
    
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
        */
        void setInputFiles(const std::vector<std::string>& fnames); // build ncreader, tree, link to data
        
        /// Set the definition of a "storm" as a set of identification criteria.
        /**
        */
        void defineCriteria(const std::vector<crit_ptr>& cs, const std::vector<colloc_pair>& cpairs = 
            std::vector<colloc_pair>());
        
        /// Output SearchManager state info to string (e.g., for console output)
        std::string infoString() const;
        
        /// run spatial search on complete data set
        /**
            @param os : output stream (most of the time, `std::ofstream`; can also be `std::cout`.)
            @note If using MPI, use only one output stream per rank.
        */
        void runSpatialSearch(std::ostream& os);
    
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
        std::vector<colloc_pair> colloc_criteria;
        
        EventSet<DataLayout> investigatePossibles(const Index time_ind, 
            const std::vector<event_ptr>& poss);
            
        std::vector<event_ptr> runLocatorAtTimestep(const Index time_ind);
        
        void processCollocations(EventSet<DataLayout>& events) const;
        
        /// Run spatial search on a file.
        void runfile(const Int f_ind);
        
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
