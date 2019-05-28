#include "SSSearchManager.hpp"
#include <sstream>
#include <iomanip>

namespace StrideSearch {

template <typename DataLayout>
void SearchManager<DataLayout>::setInputFiles(const std::vector<std::string>& fnames) {
    filenames = fnames;
    reader = readerHelper<DataLayout>();
    tree = std::unique_ptr<KDTree>(new KDTree(reader.get()));
    main_sector_set.linkToData(*tree, reader);
    file_time = reader->getTime();
}

template <> template <>
std::shared_ptr<NCReader> SearchManager<UnstructuredLayout>::readerHelper<UnstructuredLayout>() const {
    return std::shared_ptr<NCReader>(new UnstructuredNCReader(filenames[0]));
}

template <> template <>
std::shared_ptr<NCReader> SearchManager<LatLonLayout>::readerHelper<LatLonLayout>() const {
    return std::shared_ptr<NCReader>(new LatLonNCReader(filenames[0]));
}

template <typename DataLayout>
void SearchManager<DataLayout>::defineCriteria(const std::vector<std::shared_ptr<IDCriterion>>& cs, 
    const std::vector<colloc_pair>& cpairs) {
    criteria = cs;
    colloc_criteria = cpairs;
    if (cs[0]->locationKind == DEPENDENT) {
        throw std::runtime_error("SearchManager::defineCriteria error: first criterion must be location independent.");
    }
    locator_crit = cs[0];
    for (Index i=0; i<main_sector_set.nSectors(); ++i) {
        main_sector_set.sectors[i]->allocWorkspace(locator_crit);
    }
}

template <typename DataLayout>
std::string SearchManager<DataLayout>::infoString() const {
    std::ostringstream ss;
    ss << "SearchManager record:" << std::endl;
    ss << "\tregion (sb, nb, wb, eb) = (" << region[0] << "," << region[1] << "," 
       << region[2] << "," <<region[3] << ")" << std::endl;
    ss << "\tsector radius = " << sector_radius << std::endl;
    ss << main_sector_set.infoString(1);
    ss << main_event_set.infoString(1);
    ss << reader->infoString(1);
    ss << "\tstartdate = " << start_date.DTGString() << std::endl;
    ss << "\tcriteria = ";
    if (criteria.size() == 0) {
        ss << "null";
    }
    else {
        for (Int i=0; i<criteria.size(); ++i) {
            ss << criteria[i]->description() << " ";
        }
    }
    ss << std::endl;
    ss << "\tlocator_crit = " << (locator_crit ? locator_crit->description() : "null") << std::endl;
    ss << "--------------------------------------" << std::endl;
    return ss.str();
}

template <typename DataLayout>
void SearchManager<DataLayout>::runfile(const Int f_ind) {
    reader->updateFile(filenames[f_ind]);
    file_time = reader->getTime();
    for (Int k=0; k<file_time.size(); ++k) {
        runTimestepSearch(k);
    }
}

template <typename DataLayout>
void SearchManager<DataLayout>::runTimestepSearch(const Index t_ind) {
    const std::vector<event_ptr> loc_results = runLocatorAtTimestep(t_ind);
    EventSet<DataLayout> found_results = investigatePossibles(t_ind, loc_results);
    processCollocations(found_results);
    main_event_set.extend(found_results);
}


template <typename DataLayout> 
std::vector<std::shared_ptr<Event<DataLayout>>> 
SearchManager<DataLayout>::runLocatorAtTimestep(const Index time_ind) {
    std::vector<std::shared_ptr<Event<DataLayout>>> possible_events;
    /// Step 1: Loop over each sector, collect result from locator_crit only
    const std::string fn = reader->filename();
    const DateTime current_date = DateTime(file_time[time_ind], start_date);
    // load & evaluate data
    for (Index i=0; i<main_sector_set.nSectors(); ++i) {
        reader->fillWorkspaceData(main_sector_set.sectors[i]->workspaces[0], main_sector_set.sectors[i]->indices, time_ind);
        std::shared_ptr<Event<DataLayout>> ev = main_sector_set.sectors[i]->evaluateLocatorCriterionAtTimestep(locator_crit, 
            current_date, fn, time_ind);
        if (ev) {
            possible_events.push_back(ev);
        }
    }
    return possible_events;
}

template <typename DataLayout>
EventSet<DataLayout> SearchManager<DataLayout>::investigatePossibles(const Index time_ind, 
    const std::vector<std::shared_ptr<Event<DataLayout>>>& poss) {
    /// Step 2: Center a Sector on each possibility, investigate all criteria
    EventSet<DataLayout> possible_events(poss);
    possible_events.removeDuplicates(sector_radius);
    SectorSet<DataLayout> timestep_sectors(possible_events, sector_radius);
    timestep_sectors.linkToData(*tree, reader);
    timestep_sectors.allocWorkspaces(criteria);
    
    const std::string fn = reader->filename();
    const DateTime current_date = DateTime(file_time[time_ind], start_date);
    
    std::vector<std::vector<std::shared_ptr<Event<DataLayout>>>> found_events;

    for (Index i=0; i<timestep_sectors.nSectors(); ++i) {
        // load workspace data
        for (Int j=0; j<timestep_sectors.sectors[i]->workspaces.size(); ++j) {
            reader->fillWorkspaceData(timestep_sectors.sectors[i]->workspaces[j], 
                timestep_sectors.sectors[i]->indices, time_ind);
        }
        // evaluate all criteria
        found_events.push_back(timestep_sectors.sectors[i]->evaluateCriteriaAtTimestep(criteria, current_date, fn, time_ind));
    }
    // flatten found events
    EventSet<DataLayout> found_set(found_events);
    found_set.removeDuplicates(sector_radius);
    found_set.consolidateRelated(sector_radius);
    return found_set;
}

template <typename DataLayout>
void SearchManager<DataLayout>::processCollocations(EventSet<DataLayout>& events) const {
    /// Step 3: require events to be collocated (if applicable)
    for (Int i=0; i<colloc_criteria.size(); ++i) {
        events.requireCollocation(colloc_criteria[i].first, colloc_criteria[i].second, sector_radius);
    }
}

template <typename DataLayout>
void SearchManager<DataLayout>::runSpatialSearch(std::ostream& os) {
#ifdef HAVE_MPI
    throw std::runtime_error("SearchManager::runSpatialSearch error: MPI not implemented.");
#else
    const Int start_findex = 0;
    const Int end_findex = filenames.size();
#endif
    for (Int i=start_findex; i<=end_findex; ++i) {
        runfile(i);        
    }
}

// ETI
template class SearchManager<UnstructuredLayout>;
template class SearchManager<LatLonLayout>;

}
