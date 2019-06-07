#ifndef _SS_TRACK_SET_HPP_
#define _SS_TRACK_SET_HPP_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSDateTime.hpp"
#include "SSConsts.hpp"
#include "SSTrack.hpp"
#include "SSEventSet.hpp"
#include "SSEvent.hpp"

namespace StrideSearch {

template <typename DataLayout=UnstructuredLayout>
class TrackSet {
    public:
        typedef std::shared_ptr<Track<DataLayout>> track_ptr;
        typedef std::shared_ptr<Event<DataLayout>> event_ptr;
        
        std::string infoString() const;
        
        inline void addTrack(const track_ptr tr) {tracks.push_back(tr);}
        
        TrackSet() : maxKmPerTimestep(0), minTrackLength(0), tracks(), timestep_hours(0) {}
        
        TrackSet(const Real storm_spd_mps, const Real dt_hours, const Int mintracklen=1) :
            maxKmPerTimestep(MPS2KPH*storm_spd_mps*dt_hours), minTrackLength(mintracklen),
            timestep_hours(dt_hours), tracks() {}
        
        void build(const EventSet<DataLayout>& spatial_results);
        
        inline Int size() const {return tracks.size();}
        
        void writeData(const std::string& base_name, 
            const std::vector<std::string>& descs=std::vector<std::string>()) const;
    
    protected:
        Real maxKmPerTimestep;
        Int minTrackLength;
        std::vector<track_ptr> tracks;
        Real timestep_hours;
};

}
#endif