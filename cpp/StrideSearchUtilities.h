#ifndef _STRIDE_SEARCH_UTILITIES_H_
#define _STRIDE_SEARCH_UTILITIES_H_

/// Mean sea level radius of the Earth, in kilometers.
#define EARTH_RADIUS_KM 6371.220

/// Tolerance for floating point zero.
#define ZERO_TOL 2.0e-9

#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include <cmath>
#include <string>
#include <map>
#include <vector>

namespace StrideSearch {

/// Degrees to radians conversion factor.
static const scalar_type deg2rad = std::atan(1.0) / 45.0;

/// Constant Pi.
static const scalar_type PI = 4.0 * std::atan(1.0);

/// Prints the required copyright information to the console.
void print_copyright();

/// Four-quadrant arc tangent
/** Similar to std::atan2 except that the branch cut is moved so the output of atan4
    lies with [0, 2*pi), rather than (-pi, pi).
*/
scalar_type atan4( const scalar_type y, const scalar_type x);

/// Converts latitude-longitude coordinates to Cartesian coordinates
void llToXYZ(scalar_type& x, scalar_type& y, scalar_type& z, const scalar_type& lat, const scalar_type& lon);

/// Converts Cartesian coordinates of a point on the sphere to latitude-longitude coordinates.
void XyzToLL(scalar_type& lat, scalar_type& lon, const scalar_type& x, const scalar_type& y, const scalar_type& z);

/// Computes the geodesic length in kilometers between two points on an Earth-sized sphere (const radius = EARTH_RADIUS_KM).
scalar_type sphereDistance(scalar_type latA, scalar_type lonA, scalar_type latB, scalar_type lonB);


}
#endif
