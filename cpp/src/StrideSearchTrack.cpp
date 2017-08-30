#include "StrideSearchTrack.h"
#include <sstream>
#include <iostream>
#include <iomanip>

namespace StrideSearch {

Track::Track(const std::vector<std::shared_ptr<Event>>& evs) {
    for (index_type i = 0; i < evs.size(); ++i)
        events.emplace(evs[i]->datetime, evs[i]);
}

std::map<DateTime, ll_coord_type> Track::getTrackPositions() const {
    std::map<DateTime, ll_coord_type> result;
    for (auto& ev : events) {
        result.emplace(ev.first, ev.second->latLon);
    }
    return result;
}

std::string Track::infoString() const {
    std::ostringstream ss;
    ss << "Track record:\n";
    ss << "\tsize() = " << size() << std::endl;
//     ss << "\tstart date = " << startDate() << std::endl;
//     ss << "\tend date = " << endDate() << std::endl;
    return ss.str();
}

DateTime Track::startDate() const {
    DateTime result(0,0,0,0);
    event_map_type::const_iterator pos;
    if (!events.empty()) {
        pos = events.begin();
        result = pos->first;
    }
    return result;
}

DateTime Track::endDate() const {
    DateTime result(0,0,0,0);
    if (!events.empty()) {
        const event_map_type::const_iterator pos = events.cend();
        result = pos->first;
    }
    return result;
}

}