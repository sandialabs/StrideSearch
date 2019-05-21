#ifndef _STRIDE_SEARCH_SEARCH_MANAGER_HPP_
#define _STRIDE_SEARCH_SEARCH_MANAGER_HPP_

#include "SSDefs.hpp"
#include "StrideSearchConfig.h"
#include "SSUtilities.h"
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

namespace StrideSearch {

/// Rectangle in lat-lon space (south, north, west, east)
typedef std::array<Real,4> region_type;

template <typename DataLayout=UnstructuredLayout>
class SearchManager {
    public:
        SearchManager(const region_type& sreg, const Real srad,) : region(sreg), sector_radius(srad),
            main_sector_set(sreg[0], sreg[1], sreg[2], sreg[3], srad), 
            main_event_set(), reader(), tree(), filenames(), start_date(), criteria(), locator_crit() {} 
        
        void setStartDate(const DateTime& sd) {start_date = sd;}
        
        void setInputFiles(const std::vector<std::string> fnames); // build ncreader, tree, link to data
        
        void defineCriteria(const std::vector<std::shared_ptr<IDCriterion> cs);
        
        void run();
    
    protected:
        region_type region;
        Real sector_radius;
        SectorSet<DataLayout> main_sector_set;
        EventSet<DataLayout> main_event_set;
        
        std::shared_ptr<NCReader> reader;
        KDTree tree;
        std::vector<std::string> filenames;
        DateTime start_date;
        std::vector<std::shared_ptr<IDCriterion>> criteria;
        std::shared_ptr<IDCriterion> locator_crit;
        
        
        
};

}

#endif
