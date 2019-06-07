#include "SSSearchManager.hpp"
#include "SSUtilities.hpp"
#include <sstream>
#include <iomanip>
#include <limits>

namespace StrideSearch {

template <typename DataLayout>
void SearchManager<DataLayout>::setInputFiles(const std::vector<std::string>& fnames, const SearchParams& params) {
    filenames = fnames;
    reader = readerHelper<DataLayout>();
    tree = std::unique_ptr<KDTree>(new KDTree(reader.get()));
    main_sector_set.linkToData(*tree, reader);
    file_time = reader->getTime(params.time_units);
}

template <typename DataLayout>
void SearchManager<DataLayout>::setInputFiles(const MPIManager& mpi, const std::vector<std::string>& allfiles) {
    const Int rank = mpi.getRank();
    const Int wsize = mpi.workSize(rank);
    const Index start = mpi.startInd(rank);
    filenames = std::vector<std::string>(wsize);
    for (Int i=start; i<start + wsize; ++i) {
        filenames[i] = allfiles[i];
    }
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
    const std::vector<colloc_ptr>& cpairs) {
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
    ss << main_event_set.infoString(1,true);
    ss << reader->infoString(1);
    ss << "\tstartdate = " << start_date.DTGString() << std::endl;
    ss << "\tcriteria = ";
    if (criteria.size() == 0) {
        ss << "null";
    }
    else {
        for (Int i=0; i<criteria.size(); ++i) {
            ss << criteria[i]->description() << (i == criteria.size()-1 ? "" : ", ");
        }
    }
    ss << std::endl;
    ss << "\tlocator_crit = " << (locator_crit ? locator_crit->description() : "null") << std::endl;
    ss << "--------------------------------------" << std::endl;
    return ss.str();
}

template <typename DataLayout>
void SearchManager<DataLayout>::runfile(const Int f_ind, const Int stop_timestep) {
    std::cout << "... file " << f_ind + 1 << " of " << filenames.size() << std::endl;
    reader->updateFile(filenames[f_ind]);
    file_time = reader->getTime();
    ProgressBar file_prog("\t% done", (stop_timestep==-1 ? file_time.size() : stop_timestep), 10);
    for (Int k=0; k<(stop_timestep != -1 ? stop_timestep : file_time.size()); ++k) {
        runTimestepSearch(k);
        file_prog.update();
    }
}

template <typename DataLayout>
void SearchManager<DataLayout>::runfile(const Int f_ind, const SearchParams& params, const Int stop_timestep) {
    std::cout << "... file " << f_ind +1 << " of " << filenames.size() << '\n';
    reader->updateFile(filenames[f_ind]);
    file_time = reader->getTime(params.time_units);
    ProgressBar prog("\t% done", (stop_timestep == -1 ? file_time.size() : stop_timestep)/params.timestep_stride, 10);
    for (Int k=0; k<(stop_timestep != -1 ? stop_timestep : file_time.size()); k+=params.timestep_stride) {
        runTimestepSearch(k);
        prog.update();
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
void SearchManager<DataLayout>::printTime() const {
    std::cout << "file_time = (";
    for (Int i=0; i<file_time.size(); ++i) {
        std::cout << file_time[i] << (i<file_time.size()-1 ? " " : ")\n");
    }
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
        events.requireCollocation(colloc_criteria[i]->crit1, colloc_criteria[i]->crit2,
             colloc_criteria[i]->distance_threshold);
    }
}

template <typename DataLayout>
void SearchManager<DataLayout>::runSpatialSearch(const Int stop_timestep) {
    const Int start_findex = 0;
    const Int end_findex = filenames.size();
    for (Int i=start_findex; i<end_findex; ++i) {
        runfile(i, stop_timestep);        
    }
}

template <typename DataLayout>
void SearchManager<DataLayout>::runSpatialSearch(const SearchParams& params, const Int stop_timestep) {
    const Int start_findex = 0;
    const Int end_findex = filenames.size();
    for (Int i=start_findex; i<end_findex; ++i) {
        runfile(i, params, stop_timestep);
    }
}

template <typename DataLayout>
void SearchManager<DataLayout>::runSpatialSearch(const MPIManager& mpi, const SearchParams& params, const Int stop_timestep) {
    switch (mpi.parallelStrategy()) {
        case (MPIDistribute::FILES) : {
            const Int start_findex = 0;
            const Int end_findex = filenames.size();
            for (Int i=start_findex; i<end_findex; ++i) {
                runfile(i,params,stop_timestep);
            }
            break;
        }
        case (MPIDistribute::TIMESTEPS) : {
            throw std::runtime_error("SearchManager::runSpatialSearch error: MPIDistribute::TIMESTEPS not implemented.");
            break;
        }
    }
}

template <typename DataLayout>
void SearchManager<DataLayout>::buildTracks(const Real storm_spd_mps, const Real dt_hours, const Int mintracklen) {
    main_track_set = TrackSet<DataLayout>(storm_spd_mps, dt_hours, mintracklen);
    main_track_set.build(main_event_set);
}

template <typename DataLayout>
void SearchManager<DataLayout>::outputCSV(std::ostream& os) const {
    const Real fillvalue = std::numeric_limits<Real>::max();
    const std::map<DateTime,std::vector<event_ptr>> dtmap = main_event_set.separateByDateTime();
    // write header
    os << "datetime;lat;lon;loc_ind;loc_ind3d;time_ind;filename";
    for (Int i=0; i<criteria.size(); ++i) {
        os << ";" << criteria[i]->description();
    }
    os << '\n';
    const char dl = ';';
    for (auto& elem : dtmap) {
        for (Int i=0; i<elem.second.size(); ++i) {
            os << elem.first.isoFullStr() << dl;
            os << elem.second[i]->lat << dl << elem.second[i]->lon << dl;
            os << elem.second[i]->loc_ind << dl << elem.second[i]->loc_ind_3d << dl;
            os << elem.second[i]->time_ind << dl << elem.second[i]->filename;
            std::map<std::string, Real> event_values;
            for (Int j=0; j<criteria.size(); ++j) {
                event_values.emplace(criteria[j]->description(), fillvalue);
            }
            event_values[elem.second[i]->description()] = elem.second[i]->value;
            for (Int j=0; j<elem.second[i]->relatedEvents.size(); ++j) {
                event_values[elem.second[i]->relatedEvents[j]->description()] = 
                    elem.second[i]->relatedEvents[j]->value;
            }
            for (Int j=0; j<criteria.size(); ++j) {
                os << dl << event_values[criteria[j]->description()];
            }
            os << '\n';
        }
    }
}

// ETI
template class SearchManager<UnstructuredLayout>;
template class SearchManager<LatLonLayout>;

}
