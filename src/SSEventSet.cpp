#include "SSEventSet.hpp"
#include <sstream>
#include <set>

namespace StrideSearch {

template <typename DataLayout>
EventSet<DataLayout>::EventSet(const std::vector<std::vector<event_ptr_type>>& events_) {
    for (Index i=0; i<events_.size(); ++i) {
        for (Index j=0; j<events_[i].size(); ++j) {
            events.push_back(events_[i][j]);
        }
    }
}

template <typename DataLayout>
void EventSet<DataLayout>::extend(const EventSet<DataLayout>& other) {
    for (Index i=0; i<other.size(); ++i) {
        events.push_back(other.events[i]);
    }
}

template <typename DataLayout>
Real EventSet<DataLayout>::minSeparationDistance() const {
    Real result = 2*PI*EARTH_RADIUS_KM;
    for (Index i=0; i<events.size(); ++i) {
        for (Index j=i+1; j<events.size(); ++j) {
            const Real dist = sphereDistance(events[i]->lat, events[i]->lon, events[j]->lat, events[j]->lon);
            if (dist < result) result = dist;
        }
    }
    return result;
}

template <typename DataLayout>
Real EventSet<DataLayout>::maxSeparationDistance() const {
    Real result = 0;
    for (Index i=0; i<events.size(); ++i) {
        for (Index j=i+1; j<events.size(); ++j) {
            const Real dist = sphereDistance(events[i]->lat,events[i]->lon, events[j]->lat, events[j]->lon);
            if (dist > result) result = dist;
        }
    }
    return result;
}

template <typename DataLayout>
void EventSet<DataLayout>::removeDuplicates(const Real dist_tol) {
    std::vector<bool> duplicates(events.size(), false);
    Index dup_ct = 0;
    /// Step 1: Mark and count duplicates
    for (Index i=0; i<events.size(); ++i) {
        if (!duplicates[i]) {
            for (Index j=i+1; j<events.size(); ++j) {
                if (!duplicates[j]) {
                    if (events[i]->isDuplicate(*events[j])) {
                        duplicates[j] = true;
                        dup_ct += 1;
                    }
                }
            }
        }
    }
    /// Step 2: Mark and count redundant events
    for (Index i=0; i<events.size(); ++i) {
        for (Index j=i+1; j<events.size(); ++j) {
            if (!duplicates[i] && !duplicates[j]) {
                if (events[i]->isRedundant(*events[j], dist_tol)) {
                    dup_ct += 1;
                    if ( events[i]->lowerIntensity(*events[j])) {
                        duplicates[i] = true;
                    }
                    else {
                        duplicates[j] = true;
                    }
                }
            }
        }
    }
    if (dup_ct > 0) {
        /// Step 3: remove duplicate and redundant events
        std::vector<std::shared_ptr<Event<DataLayout>>> new_events;
        new_events.reserve(events.size()-dup_ct);
        for (Index i=0; i<events.size(); ++i) {
            if (!duplicates[i]) {
                new_events.push_back(events[i]);
            }
        }
        events = new_events;
    }
}

template <typename DataLayout> 
std::string EventSet<DataLayout>::infoString(const int tab_lev, const bool printAll) const {
    std::string tabstr("");
    for (int i=0; i<tab_lev; ++i) {
        tabstr += "\t";
    }
    std::ostringstream ss;
    ss << tabstr << "EventSet record:" << std::endl;
    ss << tabstr << "\tsize = " << events.size() << std::endl;
    if (printAll) {
        for (Index i=0; i<events.size(); ++i) {
            ss << events[i]->infoString(tab_lev+1);
        }
    }
    ss << "--------------------------------------" << std::endl;
    return ss.str();
}

template <typename DataLayout>
std::map<DateTime, std::vector<std::shared_ptr<Event<DataLayout>>>> 
EventSet<DataLayout>::separateByDateTime() const{
    std::map<DateTime, std::vector<std::shared_ptr<Event<DataLayout>>>> result;
    for (Index i=0; i<events.size(); ++i) {
        result[events[i]->datetime].push_back(events[i]);
    }
    return result;
}

template <typename DataLayout>
void EventSet<DataLayout>::consolidateRelated(const Real dist_tol) {
    std::vector<bool> already_used(events.size(), false);
    /// Step 1: process flat list for related events, nest related with first found
    for (Index i=0; i<events.size(); ++i) {
        if (!already_used[i]) {
            std::set<std::string> descs;
            descs.insert(events[i]->desc);
            for (Index j=i+1; j<events.size(); ++j) {
                if (!already_used[j]) {
                    if (events[i]->isNear(*events[j], dist_tol) && events[i]->datetime == events[j]->datetime) {
                        /*
                           Events i and j have the same datetime and are close enough to be related
                        */
                        if (descs.count(events[j]->desc) == 0) {
                            /*
                                Event i is not related to an event of type j yet
                            */
                            events[i]->addRelated(events[j]);
                            descs.insert(events[j]->desc);
                        }
                        else {
                            /*
                                Event i is related to an event of type j already; 
                                pick the most intense one, discard the other
                            */
                            if (events[i]->desc == events[j]->desc) {
                                if (events[i]->lowerIntensity(*events[j])) {
                                    events[i]->lat = events[j]->lat;
                                    events[i]->lon = events[j]->lon;
                                    events[i]->value = events[j]->value;
                                    events[i]->loc_ind = events[j]->loc_ind;
                                    events[i]->loc_ind_3d = events[j]->loc_ind_3d;
                                }
                            }
                            else {
                                for (Index k=0; k<events[i]->relatedEvents.size(); ++k) {
                                    if(events[i]->relatedEvents[k]->desc == events[j]->desc) {
                                        if (events[i]->relatedEvents[k]->lowerIntensity(*events[j])) {
                                            events[i]->relatedEvents[k]->lat = events[j]->lat;
                                            events[i]->relatedEvents[k]->lon = events[j]->lon;
                                            events[i]->relatedEvents[k]->value = events[j]->value;
                                            events[i]->relatedEvents[k]->loc_ind = events[j]->loc_ind;
                                            events[i]->relatedEvents[k]->loc_ind_3d = events[j]->loc_ind_3d;
                                        }
                                    }
                                }
                            }
                        }
                        already_used[j] = true;
                    }
                }
            }
        }
    }
    /// Step 2: replace flat list with nested list
    std::vector<std::shared_ptr<Event<DataLayout>>> new_events;
    for (Index i=0; i<events.size(); ++i) {
        if (!already_used[i]) {
            new_events.push_back(events[i]);
        }
    }
    events = new_events;
}

// template <typename DataLayout>
// void EventSet<DataLayout>::writeData(std::ostream& os) {
//     const char sep = ';';
// }

template <typename DataLayout>
void EventSet<DataLayout>::requireCollocation(const std::shared_ptr<IDCriterion> crit1, 
    const std::shared_ptr<IDCriterion> crit2, const Real dist_tol) {
    std::vector<bool> is_collocated(events.size(), false);
    /// Step 1: mark collocated events
    for (Index i=0; i<events.size(); ++i) {
        is_collocated[i] = events[i]->isCollocated(crit1.get(), crit2.get(), dist_tol);
    }
    /// Step 2: remove unmarked events
    std::vector<std::shared_ptr<Event<DataLayout>>> new_events;
    for (Index i=0; i<events.size(); ++i) {
        if (is_collocated[i]) {
            new_events.push_back(events[i]);
        }
    }
    events = new_events;
}

/// ETI
template class EventSet<UnstructuredLayout>;
template class EventSet<LatLonLayout>;

}
