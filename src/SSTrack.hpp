#ifndef _SS_TRACK_HPP_
#define _SS_TRACK_HPP_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSConsts.hpp"
#include "SSEvent.hpp"
#include "SSEventSet.hpp"

namespace StrideSearch {

template <typename DataLayout=UnstructuredLayout>
class Track {
    public : 
        typedef std::shared_ptr<Event<DataLayout>> event_ptr;
        
        inline void addEventToTrack(const event_ptr ev) {events.append(ev);}
        
        inline bool empty() const {return size() == 0;}
        
        inline Int size() const {return events.size();}
    
        Track() : events() {}
        
        Track(const std::vector<event_ptr>& evs) : events(evs) {}
        
        Track(const event_ptr ev) : events({ev}) {}

        std::string infoString() const;
        
        void writeTrackData(std::ostream& os, const std::vector<std::string>& descs = std::vector<std::string>()) const;
    protected:
        EventSet<DataLayout> events;
};

}
#endif