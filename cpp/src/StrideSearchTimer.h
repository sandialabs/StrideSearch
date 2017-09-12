#ifndef _STRIDE_SEARCH_TIMER_H_
#define _STRIDE_SEARCH_TIMER_H_

#include "StrideSearchConfig.h"
#include "StrideSearchTypeDefs.h"
#include <chrono>
#include <string>

namespace StrideSearch {

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
