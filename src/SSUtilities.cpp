// #include "Python.h"
#include "SSUtilities.hpp"
#include "SSConsts.hpp"
#include <cmath>
#include <string>
#include <iostream>
#include <sstream>
#include <vector>
#include <iomanip>
#include <fstream>


namespace StrideSearch {

void llToXYZ(Real& x, Real& y, Real& z, const Real& lat, const Real& lon){
    x = EARTH_RADIUS_KM*std::cos(DEG2RAD * lat) * std::cos(DEG2RAD * lon);
    y = EARTH_RADIUS_KM*std::cos(DEG2RAD * lat) * std::sin(DEG2RAD * lon);
    z = EARTH_RADIUS_KM*std::sin(DEG2RAD * lat);
}

Real atan4( const Real y, const Real x)
{
    Real result = 0.0;
    if ( x == 0.0)
    {
        if ( y > 0.0)
            result = 0.5 * PI;
        else if ( y < 0.0 )
            result = 1.5 * PI;
        else if ( y == 0.0 )
            result = 0.0;
    }
    else if ( y == 0 )
    {
        if ( x > 0.0 )
            result = 0.0;
        else if ( x < 0.0 )
            result = PI;
    }
    else
    {
        Real theta = std::atan2( std::abs(y), std::abs(x));
        if ( x > 0.0 && y > 0.0 )
            result = theta;
        else if ( x < 0.0 && y > 0.0 )
            result = PI - theta;
        else if ( x < 0.0 && y < 0.0 )
            result = PI + theta;
        else if ( x > 0.0 && y < 0.0 )
            result = 2.0 * PI - theta;
    }
    return result;
}

void XyzToLL(Real& lat, Real& lon, const Real& x, const Real& y, const Real& z) {
    lat = std::atan2(z, std::sqrt(x*x + y*y)) * RAD2DEG;
    lon = atan4(y, x) * RAD2DEG;
}

Real sphereDistance(const Real latA, const Real lonA, const Real latB, const Real lonB){
    if (std::abs(latB - latA) < ZERO_TOL && std::abs(lonB - lonA) < ZERO_TOL)
        return 0.0;
    else {
        Real xA, yA, zA;
        Real xB, yB, zB;
        llToXYZ(xA, yA, zA, latA, lonA);
        llToXYZ(xB, yB, zB, latB, lonB);
        
        const Real cp1 = yA * zB - yB * zA;
        const Real cp2 = xB * zA - xA * zB;
        const Real cp3 = xA * yB - xB * yA;
        const Real cpnorm = std::sqrt(cp1 * cp1 + cp2 * cp2 + cp3 * cp3);
        
        const Real dotProd = xA * xB + yA * yB + zA * zB;
        
        return EARTH_RADIUS_KM * std::atan2(cpnorm, dotProd);
    }
}

std::string copyright_str() {
    std::ostringstream ss;
    ss <<  "------------------------------------------------------------------------------------------------\n";
    ss <<  "Stride Search version " << StrideSearch_VERSION_MAJOR << "." << StrideSearch_VERSION_MINOR <<"\n";
    ss <<  "Copyright 2016 Sandia Corporation. Under the terms of contract DE-AC04-94AL85000,\n"; 
    ss <<  "there is a non-exclusive license for use of this work by or on behalf of the U.S. Government. \n";
    ss <<  "Export of this program may require a license from the United States Government.\n";
    ss <<  "------------------------------------------------------------------------------------------------\n";
    return ss.str();
}

void print_copyright() {
    std::cout << copyright_str();
}

std::ostream& operator << (std::ostream& os, const std::array<Index,2>& arr) {
    os << "(";
    for (int i=0; i<2; ++i) {
        os << arr[i] << (i==1 ? ")" : ",");
    }
    return os;
}

std::ostream& operator << (std::ostream& os, const std::array<Index,1>& arr) {
    os << "(" << arr[0] << ")";
    return os;
}


std::ostream& operator << (std::ostream& os, const std::array<Index,3>& arr) {
    os << "(";
    for (int i=0; i<3; ++i) {
        os << arr[i] << (i==2 ? ")" : ",");
    }
    return os;
}

std::ostream& operator << (std::ostream& os, const std::array<Real,4>& arr) {
    os << "(";
    for (int i=0; i<4; ++i) {
        os << arr[i] << (i==3 ? ")" : ",");
    }
    return os;
}

std::vector<std::string> getLinesFromFile(const std::string& fname) {
    std::vector<std::string> result;
    
    std::ifstream file(fname);
    if (!file.is_open()) {
        std::ostringstream ss;
        ss << "getLinesFromFile error: cannot open file " << fname;
        throw std::ios_base::failure(ss.str());
    }
    std::string line;
    while (std::getline(file, line)) {
        result.push_back(line);
    }
    return result;
}


}

