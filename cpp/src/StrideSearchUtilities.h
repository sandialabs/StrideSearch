#ifndef _STRIDE_SEARCH_UTILITIES_H_
#define _STRIDE_SEARCH_UTILITIES_H_

#include "StrideSearchConfig.h"
#include "StrideSearchTypeDefs.h"

namespace StrideSearch {

/// Mean sea level radius of the Earth, in kilometers.
#define EARTH_RADIUS_KM 6371.220

/// Sphere radius. Global variable, determined by data.
static scalar_type sphereRadius=EARTH_RADIUS_KM;

/// Tolerance for floating point zero.
#define ZERO_TOL 2.0e-9

/// Constant Pi.
static constexpr scalar_type PI = 3.1415926535897932384626433832795027975;

/// Radians to degrees conversion factor
static constexpr scalar_type RAD2DEG = 180.0 / PI;

/// Degrees to radians conversion factor
static constexpr scalar_type DEG2RAD = PI / 180.0;

/// Meters per second to Kilometers per hour conversion factor
static constexpr scalar_type MPS2KPH = 3.6;

/// Prints the required copyright information to the console.
void print_copyright();

/// Four-quadrant arc tangent
/** Similar to std::atan2 except that the branch cut is moved so the output of atan4
    lies with [0, 2*pi), rather than (-pi, pi).
    
    Output is given in radians.
    
    @param y y-coordinate in R2
    @param x x-coordinate in R2
    @return angle theta between positive x-axis and (x,y)
*/
scalar_type atan4(const scalar_type y, const scalar_type x);

/// Converts latitude-longitude coordinates to Cartesian coordinates
/** 
    Assumes input values are given in degrees.
    
    @param x Cartesian x coordinate
    @param y Cartesian y coordinate
    @param z Cartesian z coordinate
*/
void llToXYZ(scalar_type& x, scalar_type& y, scalar_type& z, 
    const scalar_type& lat, const scalar_type& lon, const scalar_type& sphRad=EARTH_RADIUS_KM);

/// Converts Cartesian coordinates of a point on the sphere to latitude-longitude coordinates.
/**
    Output values are given in degrees.
*/
void XyzToLL(scalar_type& lat, scalar_type& lon, const scalar_type& x, const scalar_type& y, const scalar_type& z);

/// Computes the geodesic length in kilometers between two points on an Earth-sized sphere (const radius = EARTH_RADIUS_KM).
/**
    Assumes input values are given in degrees.
*/
scalar_type sphereDistance(const scalar_type latA, const scalar_type lonA, const scalar_type latB, const scalar_type lonB, 
    const scalar_type& sphRad=EARTH_RADIUS_KM);

/// Computes the geodesic length in kilometers between two points on an Earth-sized sphere (const radius = EARTH_RADIUS_KM).
/**
    Assumes input values are given in degrees.
*/
scalar_type sphereDistance(const ll_coord_type& posA, const ll_coord_type& posB, const scalar_type& sphereRadius=EARTH_RADIUS_KM);

scalar_type sphDist(const scalar_type ax, const scalar_type ay, const scalar_type az, 
    const scalar_type bx, const scalar_type by, const scalar_type bz, const scalar_type& sphRad=EARTH_RADIUS_KM);

}
#endif
