#ifndef _STRIDE_SEARCH_TRACK_H_
#define _STRIDE_SEARCH_TRACK_H_

#include "StrideSearchConfig.h"
#include "StrideSearchTypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchDateTime.h"
#include "StrideSearchEvent.h"
#include "StrideSearchEventList.h"
#include <memory>
#include <vector>
#include <map>
#include <string>

namespace StrideSearch {

class Track {
    public:
        typedef std::map<DateTime, std::shared_ptr<Event>> event_map_type;
    
        Track();
        Track(const std::vector<std::shared_ptr<Event>>& evs);
        
        std::map<DateTime, ll_coord_type> getTrackPositions() const;
        
        std::string infoString() const;
        
        inline index_type size() const {return events.size();}
        
        DateTime startDate() const;
        
        DateTime endDate() const;
        
        
    protected:
        event_map_type events;
};

}

#endif
