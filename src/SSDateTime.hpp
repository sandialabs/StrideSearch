#ifndef _STRIDE_SEARCH_DATE_TIME_H_
#define _STRIDE_SEARCH_DATE_TIME_H_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include <string>
#include <map>
#include <chrono>
#include <ctime>

namespace StrideSearch {

typedef std::map<int, int> calendar_map_type;
typedef std::map<int, std::string> month_string_map_type;

static calendar_map_type monthDayMap;
static month_string_map_type monthStringMap;

/// Bare-bones date-time structure and methods.
class DateTime {
    public:
        /// Year
        int year;
        /// Month
        int month;
        /// Day
        int day;
        /// Hour
        int hour;
        
        /// Basic Constructor.
        /**
            @note The default year = 1850 is an arbitrary choice related to the commonly used 1850 climate test cases.
            @warning Some netCDF data sets have start year = 0, which can cause problems later. 
            If that is true for your data set, choosing a value > 0 is recommended.
        */
        DateTime(const int yr = 1850, const int mo = 1, const int dy = 1, const int hr = 0);
        
        /// Constructor using YYYY-MM-DD or YYYY-MM-DD-HH formatted string 
        DateTime(const std::string ymd_string);
        
        /// Return a formatted Date-Time-Group string.   
        std::string DTGString(std::string time_zone_string = "Z") const;
        
        /// Constructor from ctime::tm
        DateTime(const std::tm& ctm);
        
        /// Constructor using time values in units of days since a particular Day 0.
        /**
            This constructor converts dataset time to c++ "localtime" relative to a Day 0 defined by start.
            @param daysSinceStart time (in days) since Day 0 of a simulation.  May have fractional values.
            @param start Day 0.
        */
        DateTime(const Real daysSinceStart, const DateTime& start);
    
        /// Return a string formatted YYYYMMDDHH00.
        std::string intString() const;
    
        /// Return a string corresponding to a month's integer.
        std::string monthString(const int mInt) const;
        
        /// Return the month string corresponding to *this.
        std::string monthString() const;
        
        
    protected:
        /// Builds the maps used to convert from integers to strings, and from months to days-in-month.
        void buildMonthDayMap();
};

/// Equivalence operator.
inline bool operator==(const DateTime& left, const DateTime& right){
    return left.year == right.year && left.month == right.month && left.day == right.day && left.hour == right.hour;
}

inline bool operator != (const DateTime& left, const DateTime& right) {
    return !(left==right);
}

/// Less than implies that the left operand occurs prior to the right operand.
bool operator < (const DateTime& left, const DateTime& right); 

/// Greater than implies that the right operand occurs prior to the left operand.
bool operator > (const DateTime& left, const DateTime& right); 

}
#endif
