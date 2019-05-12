#ifndef _STRIDE_SEARCH_UTILITIES_H_
#define _STRIDE_SEARCH_UTILITIES_H_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include <iostream>
#include <array>

namespace StrideSearch {

/// Prints the required copyright information to the console.
void print_copyright();

/// Four-quadrant arc tangent
/** Similar to std::atan2 except that the branch cut is moved so the output of atan4
    lies with [0, 2*pi), rather than (-pi, pi).
    
    Output is given in radians.
*/
Real atan4( const Real y, const Real x);

/// Converts latitude-longitude coordinates to Cartesian coordinates
/** 
    Assumes input values are given in degrees.
*/
void llToXYZ(Real& x, Real& y, Real& z, const Real& lat, const Real& lon);

/// Converts Cartesian coordinates of a point on the sphere to latitude-longitude coordinates.
/**
    Output values are given in degrees.
*/
void XyzToLL(Real& lat, Real& lon, const Real& x, const Real& y, const Real& z);

/// Computes the geodesic length in kilometers between two points on an Earth-sized sphere (const radius = EARTH_RADIUS_KM).
/**
    Assumes input values are given in degrees.
*/
Real sphereDistance(Real latA, Real lonA, Real latB, Real lonB);

Real chordDistanceFromRadius(const Real radius);

std::ostream& operator << (std::ostream& os, const std::array<Index,1>& arr);
std::ostream& operator << (std::ostream& os, const std::array<Index,2>& arr);
std::ostream& operator << (std::ostream& os, const std::array<Index,3>& arr);

}
#endif
