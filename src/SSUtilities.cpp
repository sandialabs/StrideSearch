// #include "Python.h"
#include "SSUtilities.hpp"
#include "SSConsts.hpp"
#include <cmath>
#include <string>
#include <iostream>
#include <sstream>
#include <vector>
#include <iomanip>


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
};

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

Real chordDistanceFromRadius(const Real radius) {
    const Real theta = radius / (2*PI*EARTH_RADIUS_KM);
    return std::sqrt(EARTH_RADIUS_KM*EARTH_RADIUS_KM*(1-2*std::cos(theta)));
}

// Real sphereDistance(const ll_coord_type& posA, const ll_coord_type& posB) {
//     return sphereDistance(posA.first, posA.second, posB.first, posB.second);
// }

void print_copyright(){
    std::cout <<  "------------------------------------------------------------------------------------------------\n";
    std::cout <<  "Stride Search. Copyright 2016 Sandia Corporation. Under the terms of contract DE-AC04-94AL85000,\n"; 
    std::cout <<  "there is a non-exclusive license for use of this work by or on behalf of the U.S. Government. \n";
    std::cout <<  "Export of this program may require a license from the United States Government.\n";
    std::cout <<  "------------------------------------------------------------------------------------------------\n";
}


}
