#ifndef _STRIDE_SEARCH_UTILITIES_H_
#define _STRIDE_SEARCH_UTILITIES_H_

#define EARTH_RADIUS_KM 6371.220
#define ZERO_TOL 2.0e-9

#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include <cmath>
#include <string>
#include <map>
#include <vector>

namespace StrideSearch {

static const scalar_type deg2rad = std::atan(1.0) / 45.0;
static const scalar_type PI = 4.0 * std::atan(1.0);

void print_copyright();

void llToXYZ(scalar_type& x, scalar_type& y, scalar_type& z, const scalar_type& lat, const scalar_type& lon);
scalar_type atan4( const scalar_type y, const scalar_type x);
void XyzToLL(scalar_type& lat, scalar_type& lon, const scalar_type& x, const scalar_type& y, const scalar_type& z);
scalar_type sphereDistance(scalar_type latA, scalar_type lonA, scalar_type latB, scalar_type lonB);


}
#endif
