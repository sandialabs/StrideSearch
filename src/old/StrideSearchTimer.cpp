#include "StrideSearchTimer.h"
#include <sstream>

namespace StrideSearch {

std::string Timer::infoString() const {
    std::stringstream ss;
    ss << "Timer(" << _name << ").elapsed() = " << elapsed().count() << "s" << std::endl;
    return ss.str();
}

}
