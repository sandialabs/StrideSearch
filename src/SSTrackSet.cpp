#include "SSTrackSet.hpp"
#include "SSTrack.hpp"
#include "SSDateTime.hpp"
#include "SSUtilities.hpp"
#include <map>
#include <exception>
#include <sstream>
#include <fstream>

namespace StrideSearch {

template <typename DataLayout>
void TrackSet<DataLayout>::build(const EventSet<DataLayout>& spatial_results) {
    std::map<DateTime, std::vector<event_ptr>> separated_results = 
        spatial_results.separateByDateTime();
//         std::cout << "DEBUG: events separated.\n";
    std::map<DateTime, std::vector<bool>> already_used;
    for (auto& entry : separated_results) {
        already_used.emplace(entry.first, std::vector<bool>(entry.second.size(), false));
    }
//     std::cout << "DEBUG: already_used allocated; size = " << already_used.size() << "\n";
    const DateTime end_date = separated_results.rbegin()->first;
//     std::cout << "DEBUG: end_date = " << end_date.isoFullStr() << '\n';
    // For each datetime recorded by the spatial search
    for (auto& entry : separated_results) {
        /*  
            Each entry in separated_results is a 
            std::pair<DateTime, std::vector<event_ptr>>;
            
            Hence,
                entry.first = DateTime
                entry.second = Vector of events found at key's DateTime value
        */
//         std::cout << "DEBUG: entry = (" << entry.first.isoFullStr() << ", " << entry.second.size() << ")\n";
        for (Int i=0; i<entry.second.size(); ++i) {
            // For events at this datetime
//             std::cout << "DEBUG: i = " << i << '\n';
//             std::cout << "DEBUG: already_used[" << entry.first.isoFullStr() << "][" << i << "] = ";
//             std::cout << std::boolalpha <<  already_used[entry.first][i] << '\n';
            if (!already_used[entry.first][i]) {
                
                // if this event is not already used, it may be the beginning of a new track
                std::shared_ptr<Track<DataLayout>> possible_track(new Track<DataLayout>(entry.second[i]));
                already_used[entry.first][i] = true;
                event_ptr current_ev = entry.second[i];
                DateTime current_dt = entry.first;
//                 std::cout << "DEBUG: current ev = " << current_ev->infoString();
//                 std::cout << "DEBUG: current dt = " << current_dt.isoFullStr() << '\n';
                bool keepGoing = true;
                while(keepGoing) {
//                     std::cout << "DEBUG: poss_track.size() = " << possible_track->size() << '\n';
                    if (current_dt == end_date) {
                        // End of data
                        keepGoing = false;
                        break;
                    }
                    else {
                        auto next_dt = DateTime(timestep_hours*HOURS2DAYS, current_dt);
                        event_ptr next_ev = current_ev;
                        std::vector<event_ptr> candidates;
                        std::vector<Int> candidate_inds;
                        std::vector<event_ptr> next_events;
                        try {
                             // get all events at next datetime
                             next_events = separated_results.at(next_dt);
                        }
                        catch (const std::out_of_range& e) {
                            // No events at next date time; end track.
                            keepGoing = false;
                            break;
                        }
                        if (keepGoing) {
                            // find candidate successor events from next time step
                            for (int j=0; j<next_events.size(); ++j) {
                                if (!already_used[next_dt][j]) {
                                    if (current_ev->isNear(*separated_results[next_dt][j], maxKmPerTimestep)) {
                                        candidate_inds.push_back(j);
                                        candidates.push_back(separated_results[next_dt][j]);
                                    }
                                }
                            }
                            if (candidates.size() == 0) {
                                // no successor found. end track.
                                keepGoing = false;
                            }
                            else if (candidates.size() == 1) {
                                // successor found. continue track.
                                already_used[next_dt][candidate_inds[0]] = true;
                                next_ev = separated_results[next_dt][candidate_inds[0]];
                                possible_track->addEventToTrack(next_ev);
                                current_ev = next_ev;
                                current_dt = next_dt;
                            }
                            else {
                                // multiple candidates found; use closest one, continue.
                                Int keep_ind = 0;
                                Real dist = current_ev->distance(*candidates[0]);
                                for (int j=1; j<candidates.size(); ++j) {
                                    const Real tdist = current_ev->distance(*candidates[j]);
                                    if (tdist < dist) {
                                        dist = tdist;
                                        keep_ind = j;
                                    }
                                }
                                already_used[next_dt][candidate_inds[keep_ind]] = true;
                                next_ev = separated_results[next_dt][candidate_inds[keep_ind]];
                                possible_track->addEventToTrack(next_ev);
                                current_ev = next_ev;
                                current_dt = next_dt;
                            }
                        }
                    }
                }
                if (possible_track->size() >= minTrackLength) {
                    this->addTrack(possible_track);
                }
            }
        }
    }
}

template <typename DataLayout>
void TrackSet<DataLayout>::writeData(const std::string& base_name, const std::vector<std::string>& descs) const {
    for (int i=0; i<size(); ++i) {
        std::ostringstream fns;
        fns << base_name << "_track" << i << ".txt";
        const std::string fname = fns.str();
        std::ofstream file(fname);
        if (!file.is_open()) {
            std::ostringstream oss;
            oss << "TrackSet::writeData error: cannot open file " << fname;
            throw std::ios_base::failure(oss.str());
        }
        tracks[i]->writeTrackData(file,descs);
    }
}

template <typename DataLayout>
std::string TrackSet<DataLayout>::infoString() const {
    std::ostringstream ss;
    ss << "TrackSet record:\n";
    ss << "\tsize = " << size() << '\n';
    return ss.str();
}

// ETI
template class TrackSet<LatLonLayout>;
template class TrackSet<UnstructuredLayout>;

}
