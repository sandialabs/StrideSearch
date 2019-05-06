#ifndef _STRIDE_SEARCH_TIMER_H_
#define _STRIDE_SEARCH_TIMER_H_

#include "StrideSearchConfig.h"
#include "StrideSearchTypeDefs.h"
#include <chrono>
#include <string>
#include <unistd.h>
#include <sys/time.h>

namespace StrideSearch {

namespace time {

static timeval tic() {
    timeval t;
    gettimeofday(&t,0);
    return t;
}

static double calc_et(const timeval& t1, const timeval& t2) {
    static constexpr double us = 1.0e6; // microseconds
    return (t2.tv_sec * us + t2.tv_usec - t1.tv_sec*us - t1.tv_usec)/us;
}

static double toc(const timeval& t1) {
    timeval t;
    gettimeofday(&t,0);
    return calc_et(t1,t);
}

}

/// Basic wall-clock timer.
class Timer {
    public :
        /// Constructor
        Timer(const std::string& name="") : _name(name) {};
        
        /// Start timer
        inline void start() {_startTime = std::chrono::system_clock::now();}
        /// Stop timer
        inline void end() {_endTime = std::chrono::system_clock::now();}
        
        /// Print name, elapsed time to string
        std::string infoString() const;
        
        /// Compute elapsed time
        inline std::chrono::duration<double> elapsed() const {return _endTime - _startTime;}
        
    protected:
        std::chrono::time_point<std::chrono::system_clock> _startTime;
        std::chrono::time_point<std::chrono::system_clock> _endTime;
        std::string _name;
};

}

#endif
