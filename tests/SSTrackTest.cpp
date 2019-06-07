#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSDataLayoutTraits.hpp"
#include "SSTrack.hpp"
#include "SSTrackSet.hpp"
#include "SSEventSet.hpp"
#include <string>
#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <iterator>
#include <iomanip>
#include <memory>

using namespace StrideSearch;

typedef std::vector<std::string> str_vec;
typedef std::shared_ptr<Event<LatLonLayout>> event_ptr;

Real getLatitudeFromBestTrackString(const std::string& str);
Real getLongitudeFromBestTrackString(const std::string& str);
str_vec test_data_filenames();
class CSVRow {
    public :
        inline std::string const& operator [] (std::size_t index) const {
            return m_data[index];
        }
        
        inline std::size_t size() const {return m_data.size();}
        
        void readNextRow(std::istream& is) {
            std::string line;
            std::getline(is, line);
            std::stringstream line_stream(line);
            std::string cell;
            m_data.clear();
            while (std::getline(line_stream, cell, ',')) {
                m_data.push_back(cell);
            }
            if (!line_stream && cell.empty()) {
                m_data.push_back("");
            }
        }
        std::vector<std::string> m_data;
};

event_ptr makeEventFromRow(const CSVRow& row, const Int rowInd, const std::string& fn) {
//     const DateTime dt(row[2].substr(1));
//     std::cout << "datetime = " << dt.isoFullStr() << '\n';
//     std::cout << "(lat, lon) = (" << getLatitudeFromBestTrackString(row[6])
//                 << "," <<  getLongitudeFromBestTrackString(row[7]) << ")\n";
//     std::cout << "wind = " << std::stod(row[8]) << '\n';
//     std::cout << "psl = " << std::stod(row[9]) << '\n';
//     event_ptr result;
    event_ptr ev1 = event_ptr(new Event<LatLonLayout>("min(psl)", std::stod(row[9]), 
        getLatitudeFromBestTrackString(row[6]), getLongitudeFromBestTrackString(row[7]),
        DateTime(row[2].substr(1)), LatLonLayout::horiz_index_type(), rowInd,
        fn, IntensityComparison::LESS_THAN, SpatialDependence::INDEPENDENT));
    event_ptr ev2 = event_ptr(new Event<LatLonLayout>("max(windspd)", std::stod(row[8]), 
        getLatitudeFromBestTrackString(row[6]), getLongitudeFromBestTrackString(row[7]),
        DateTime(row[2].substr(1)), LatLonLayout::horiz_index_type(), rowInd,
        fn, IntensityComparison::GREATER_THAN, SpatialDependence::INDEPENDENT));
    ev1->addRelated(ev2);
    return ev1;
}


std::istream& operator >> (std::istream& is, CSVRow& row) {
    row.readNextRow(is);
    return is;
}

std::ostream& operator << (std::ostream& os, const str_vec& ss) {
    for (int i=0; i<ss.size(); ++i) {
        os << ss[i] << " | ";
    }
    return os;
}

std::ostream& operator << (std::ostream& os, const CSVRow& row) {
    os << row.m_data;   
    return os;
}
class CSVIterator {
    public:
        typedef std::input_iterator_tag iterator_category;
        typedef CSVRow value_type;
        typedef std::size_t difference_type;
        typedef CSVRow* pointer;
        typedef CSVRow& reference;
        
        CSVIterator(std::istream& is) : m_str(is.good() ? &is : NULL) {++(*this);}
        CSVIterator() : m_str(NULL) {}
        
        CSVIterator& operator++() {
            if (m_str) {
                if (!((*m_str) >> m_row)) {
                    m_str = NULL;
                }
            }
            return *this;
        }
        
//         CSVIterator& operator++(int) {
//             CSVIterator tmp(*this);
//             ++(*this);
//             return tmp;
//         }
        
        CSVRow const& operator*() const {return m_row;}
        CSVRow const* operator->() const {return &m_row;}
        
        bool operator == (const CSVIterator& rhs) {
            return (this == &rhs || (this->m_str == NULL && rhs.m_str == NULL));
        }
        bool operator != (const CSVIterator& rhs) {
            return !((*this) == rhs);
        }
    protected:
        std::istream* m_str;
        CSVRow m_row;
};

int main(int argc, char* argv[]) {
    std::cout << "using test data from files:\n";
    const str_vec files = test_data_filenames();
    std::cout << files;
    std::cout << "reading " << files.size() << " csv files...\n";
    std::vector<event_ptr> events;
    for (int i=0; i<files.size(); ++i) {
        std::ifstream file(files[i]);
        int row = 0;
        for (CSVIterator loop(file); loop != CSVIterator(); ++loop) {
            event_ptr ev = makeEventFromRow(*loop, row++, files[i]);
//             std::cout << ev->infoString();
            events.push_back(ev);
        }
    }
    EventSet<LatLonLayout> evset(events);
    std::cout << evset.infoString();
    
    const Real max_storm_speed_mps = 30.0;
    const Real dt_hours = 6;
    const Int min_steps_per_track = 2;
    TrackSet<LatLonLayout> tracks(max_storm_speed_mps, dt_hours, min_steps_per_track);
    std::cout << "building tracks.\n";
    tracks.build(evset);
    std::cout << tracks.infoString();
    const std::vector<std::string> descs = {"min(psl)", "max(windspd)"};
    tracks.writeData("track_test",descs);
    
return 0;
}

std::vector<std::string> test_data_filenames() {
    std::vector<std::string> result;
    const std::string dir_str = StrideSearch_TEST_DATA_DIR;
    const std::string end_str = "2014.dat";
    for (int i=1;i<=2; ++i) {
        std::ostringstream ss;
        ss << dir_str << "/bwp" << std::setw(2) << std::setfill('0')
           << i << end_str;
        result.push_back(ss.str());
    }
    return result;
}

Real getLatitudeFromBestTrackString(const std::string& str) {
    std::size_t letter_ind;
    Real result = std::stod(str, &letter_ind);
    if (str[letter_ind] == 'N') {
        result *= 0.1;
    }
    else if (str[letter_ind] == 'S') {
        result *= -0.1;
    }
    return result;
}

Real getLongitudeFromBestTrackString(const std::string& str) {
    std::size_t letter_ind;
    Real result = std::stod(str, &letter_ind);
    if (str[letter_ind] == 'E') {
        result *= 0.1;
    }
    else if (str[letter_ind] == 'W') {
        result = 360 - 0.1*result;
    }
    return result;
}
