#ifndef _STRIDE_SEARCH_UTILITIES_H_
#define _STRIDE_SEARCH_UTILITIES_H_

#define EARTH_RADIUS_KM 6371.220
#define ZERO_TOL 2.0e-9

#include <cmath>
#include <string>
#include <map>
#include <vector>

static const double deg2rad = std::atan(1.0) / 45.0;
static const double PI = 4.0 * std::atan(1.0);

void print_copyright();

void llToXYZ(double& x, double& y, double& z, const double& lat, const double& lon);
double atan4( const double y, const double x);
void XyzToLL(double& lat, double& lon, const double& x, const double& y, const double& z);
double sphereDistance(double latA, double lonA, double latB, double lonB);





#endif
