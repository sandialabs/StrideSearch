#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchUtilities.h"
#include <iostream>
#include <vector> 
#include <string>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
    print_copyright();
    
    double x, y, z;
    double lat, lon;
    
    x = 1.0;
    y = 0.0;
    z = 0.0;
    XyzToLL(lat, lon, x, y, z);
    std::cout << "x = " << x << ", y = " << y << ", z = " << z << std::endl;
    std::cout << "\tlat = " << lat << ", lon = " << lon << std::endl << std::endl;

    lat = 45.0;
    lon = 180.0;
    llToXYZ(x, y, z, lat, lon);
    std::cout << "lat = " << lat << ", lon = " << lon << std::endl;
    std::cout << "\tx = " << x << ", y = " << y << ", z = " << z << std::endl << std::endl;
    
    double lat2 = 90.0;
    double lon2 = 180.0;
    double gcd = sphereDistance(lat, lon, lat2, lon2);
    double gcd2 = 0.25 * PI * EARTH_RADIUS_KM;
    std::cout << "great circle distance from (" << lat << ", " << lon << ") to (" << lat2 << ", " << lon2 << ") = " 
              << gcd << " km by function." << std::endl;
    std::cout << "great circle distance from (" << lat << ", " << lon << ") to (" << lat2 << ", " << lon2 << ") = " 
              << gcd2 << " km by geometry." << std::endl << std::endl;
    
    
return 0;
}