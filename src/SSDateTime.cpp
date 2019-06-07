#include "SSDateTime.hpp"
#include "SSConsts.hpp"
#include <string>
#include <iostream>
#include <sstream>
#include <iomanip>
#include <exception>


namespace StrideSearch {

std::tm DateTime::dt2tm() const {
    std::tm result = {0};
    result.tm_sec = 0;
    result.tm_min = minute;
    result.tm_hour = hour;
    result.tm_mday = day;
    result.tm_mon = month-1;
    result.tm_year = year - 1900;
    result.tm_isdst = -1;
    return result;
}

std::string DateTime::easyReadStr() const {
    std::ostringstream ss;
    std::string mstr = monthString();
    ss << mstr[0] << char(std::tolower(mstr[1])) << char(std::tolower(mstr[2])) << '-';
    ss << day << '-' << std::setw(4) << std::setfill('0') << year << ',';
    ss << std::setw(2) << std::setfill('0') << hour << ":" << std::setw(2) << std::setfill('0') <<  minute;
    return ss.str();
}

std::string DateTime::isoDateStr() const {
    std::ostringstream ss;
    ss << std::setw(4) << std::setfill('0') << year << '-';
    ss << std::setw(2) << std::setfill('0') << month << '-';
    ss << std::setw(2) << std::setfill('0') << day;
    return ss.str();
}

std::string DateTime::isoFullStr() const {
    std::ostringstream ss;
    ss << isoDateStr() << ' ' << isoTimeStr();
    return ss.str();
}

std::string DateTime::isoTimeStr() const {
    std::ostringstream ss;
    ss << std::setw(2) << std::setfill('0') << hour << ':' << std::setw(2) << std::setfill('0') << minute;
    return ss.str();
}

std::string DateTime::isoFullStrNoSpace() const {
    std::ostringstream ss;
    ss << std::setw(4) << std::setfill('0') << year;
    ss << std::setw(2) << std::setfill('0') << month;
    ss << std::setw(2) << std::setfill('0') << day;
    ss << 'T' << std::setw(2) << std::setfill('0') << hour << std::setw(2) << std::setfill('0') << minute << "00";
    return ss.str();
}

DateTime::DateTime(const std::string ymd_string) {
    year = std::stoi(ymd_string.substr(0,4));
    month = std::stoi(ymd_string.substr(4,2));
    day = std::stoi(ymd_string.substr(6,2));
    std::string hr_str = "00";
    try {
         hr_str = ymd_string.substr(8,2);
    }
    catch (const std::out_of_range& e) {}
    hour = std::stoi(hr_str);
    minute = 0;
    if (monthDayMap.empty())
        buildMonthDayMap();
}

DateTime::DateTime(const std::tm& ctm) : year(ctm.tm_year), month(ctm.tm_mon), day(ctm.tm_mday), hour(ctm.tm_hour),
    minute(ctm.tm_min) {
    if (monthDayMap.empty()) 
        buildMonthDayMap();
};

DateTime::DateTime(const Real timeSinceStart, const DateTime& start, const DTUnits& units) {
    if (monthDayMap.empty()) 
        buildMonthDayMap();
    int year_conv = 0;
    if (start.year < 1900) {
        year_conv = start.year - 1900;
    }
    const DateTime sd(start.year - year_conv, start.month, start.day, start.hour, start.minute);
    std::tm stm = sd.dt2tm();
    const Real conv_fac = (units == DAYS ? 1.0 : (units == MINUTES ? MINUTES2DAYS : HOURS2DAYS));
    std::time_t date_seconds = std::mktime(&stm) + SIDEREAL_DAY_SEC * timeSinceStart * conv_fac;
    std::tm date = *std::localtime(&date_seconds);
    year = date.tm_year+1900 + year_conv;
    month = date.tm_mon+1;
    day = date.tm_mday;
    hour = date.tm_hour;
    minute = date.tm_min;
}

void DateTime::buildMonthDayMap() {
    monthDayMap.emplace(1, 31);
    monthDayMap.emplace(2, 28);
    monthDayMap.emplace(3, 31);
    monthDayMap.emplace(4, 30);
    monthDayMap.emplace(5, 31);
    monthDayMap.emplace(6, 30);
    monthDayMap.emplace(7, 31);
    monthDayMap.emplace(8, 31);
    monthDayMap.emplace(9, 30);
    monthDayMap.emplace(10, 31);
    monthDayMap.emplace(11, 30);
    monthDayMap.emplace(12, 31);
    
    monthStringMap.emplace(1, "JAN");
    monthStringMap.emplace(2, "FEB");
    monthStringMap.emplace(3, "MAR");
    monthStringMap.emplace(4, "APR");
    monthStringMap.emplace(5, "MAY");
    monthStringMap.emplace(6, "JUN");
    monthStringMap.emplace(7, "JUL");
    monthStringMap.emplace(8, "AUG");
    monthStringMap.emplace(9, "SEP");
    monthStringMap.emplace(10, "OCT");
    monthStringMap.emplace(11, "NOV");
    monthStringMap.emplace(12, "DEC");
}

std::string DateTime::DTGString(std::string time_zone_string) const {
    std::ostringstream ss4;
    ss4 << std::setw(4) << std::setfill('0') << year;
    std::string yrStr(ss4.str());
    
    std::ostringstream ss2;
    ss2 << std::setw(2) << std::setfill('0') << day;
    std::string dyStr(ss2.str());
    
    ss2.str(std::string());
    ss2 << std::setw(2) << std::setfill('0') << hour << "00";
    std::string hrStr(ss2.str());
    
    std::string moStr;
    try {
     moStr = monthString(month);
    }
    catch (std::exception& e) {
    std::cout << "DateTime::monthString error: caught std::exception " << e.what() << " month = " << month << '\n';
    }

    return dyStr + hrStr + time_zone_string + moStr + yrStr;
}

std::string DateTime::monthString() const {
    return monthStringMap.at(month);
}

std::string DateTime::monthString(const int mInt) const {
    std::string result;
    result = monthStringMap.at(mInt);
    return result;
}


bool operator < (const DateTime& left, const DateTime& right) {
    bool result = false;
    if (left.year < right.year)
        result = true;
    else if (left.year == right.year) {
        if (left.month < right.month) {
            result = true;
        }
        else if (left.month == right.month) {
            if (left.day < right.day) {
                result = true;
            }
            else if (left.day == right.day) {
                if (left.hour < right.hour)
                    result = true;
                else if (left.hour == right.hour) {
                    if (left.minute < right.minute)
                        result = true;
                }
            }
        }
    }
    return result;
}

bool operator > (const DateTime& left, const DateTime& right) {
    return right < left;
}

std::string DateTime::intString() const{
    std::ostringstream ss;
    ss << std::setw(4) << std::setfill('0') << year;
    ss << std::setw(2) << std::setfill('0') << month;
    ss << std::setw(2) << std::setfill('0') << day;
    ss << std::setw(2) << std::setfill('0') << hour;
    ss << std::setw(2) << std::setfill('0') << minute;
    return ss.str();
}

}
