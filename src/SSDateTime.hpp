#ifndef _STRIDE_SEARCH_DATE_TIME_H_
#define _STRIDE_SEARCH_DATE_TIME_H_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include <string>
#include <map>
#include <ctime>

namespace StrideSearch {

typedef std::map<int, int> calendar_map_type;
typedef std::map<int, std::string> month_string_map_type;

/// Map whose keys are month integers and values are the days in that month.
static calendar_map_type monthDayMap;
/// Map whos keys are month integers values are 3-letter capitalized string abbreviations of the month.
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
        /// Minute
        int minute;
        
        enum DTUnits {MINUTES, HOURS, DAYS};
        
        /// Basic Constructor.
        /**
            @note The default year = 1850 is an arbitrary choice related to the commonly used 1850 climate test cases.
            @warning Some netCDF data sets have start year = 0, which can cause problems later. 
            If that is true for your data set, choosing a value > 0 is recommended.
        */
        DateTime(const int yr = 1850, const int mo = 1, const int dy = 1, const int hr = 0, const int mn = 0) : 
            year(yr), month(mo), day(dy), hour(hr), minute(mn) {
            if (monthDayMap.empty()) buildMonthDayMap();
        }
        
        /// Constructor using YYYY-MM-DD or YYYY-MM-DD-HH formatted string 
        DateTime(const std::string ymd_string);
        
        /// Return a string formatted in the U.S. military's DTG style.   
        std::string DTGString(std::string time_zone_string = "Z") const;
        
        /// Constructor from ctime::tm
        DateTime(const std::tm& ctm);
        
        /// Constructor using time values in units of days since a particular Day 0.
        /**
            This constructor converts dataset time to c++ "localtime" relative to a Day 0 defined by start.
            @warning `std::tm` uses years in units of years since 1900; on the Mac at least, it gives bad output when start.year < 1900.
            @param daysSinceStart time (in days) since Day 0 of a simulation.  May have fractional values.
            @param start Day 0.
        */
        DateTime(const Real timeSinceStart, const DateTime& start, const DTUnits& units=DAYS);
    
        /// Return a string formatted YYYYMMDDHH00.
        std::string intString() const;
    
        /// Return a string corresponding to a month's integer.
        std::string monthString(const int mInt) const;
        
        /// Return the month string corresponding to *this.
        std::string monthString() const;
        
        /// Return a `std::tm` struct corresponding to *this
        std::tm dt2tm() const;
        
        /// Return an easy-to-read string
        std::string easyReadStr() const;
        
        /// Return the date formatted by the ISO 8601 standard.
        std::string isoDateStr() const;
        
        /// Return the time formatted by the ISO 8601 standard.
        std::string isoTimeStr() const;
        
        /// Return the date and time formatted by the ISO 8601 standard with no white space.
        std::string isoDatetimeStr() const;
        
        /// Return the date and time formatted by the ISO 8601 standard.
        /**
            @note This format can be automatically parsed by the python `dateutil.parser` module's `parse` method
            to return a `datetime.datetime` instance.
        */
        std::string isoFullStr() const;
        
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
