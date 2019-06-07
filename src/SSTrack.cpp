#include "SSTrack.hpp"
#include <map>

namespace StrideSearch {

template <typename DataLayout>
void Track<DataLayout>::writeTrackData(std::ostream& os, const std::vector<std::string>& descs) const {
    os << "datetime;lat;lon";
    if (!descs.empty()) {
        os << ";";
        for (int i=0; i<descs.size(); ++i) {
            os << descs[i] << (i==descs.size()-1?"":";");
        }
    }
    os << '\n';
    for (int i=0; i<events.size(); ++i) {
        const event_ptr ev = events.events[i];
        os << ev->datetime.isoFullStr() << ';' << ev->lat << ';' << ev->lon;
        if (!descs.empty()) {
            os << ";";
            for (int j=0; j<descs.size(); ++j) {
                if (ev->desc == descs[j]) {
                    os << ev->value << (j==descs.size()-1? "" : ";");
                }
                else {
                    for (int k=0; k<ev->nRelated(); ++k) {
                        if (ev->relatedEvents[k]->desc == descs[j]) {
                            os << ev->relatedEvents[k]->value << (j==descs.size()-1? "": ";");
                        }
                    }
                }
            }
        }
        os << '\n';
    }
}
    
// ETI
template class Track<LatLonLayout>;
template class Track<UnstructuredLayout>;    
}
