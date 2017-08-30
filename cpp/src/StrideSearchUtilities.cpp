// #include "Python.h"
#include "StrideSearchUtilities.h"
#include <cmath>
#include <string>
#include <iostream>
#include <sstream>
#include <vector>
#include <iomanip>


namespace StrideSearch {

// static const double deg2rad = std::atan(1.0) / 45.0;

void llToXYZ(scalar_type& x, scalar_type& y, scalar_type& z, const scalar_type& lat, const scalar_type& lon){
    x = std::cos(deg2rad * lat) * std::cos(deg2rad * lon);
    y = std::cos(deg2rad * lat) * std::sin(deg2rad * lon);
    z = std::sin(deg2rad * lat);
}

scalar_type atan4( const scalar_type y, const scalar_type x)
{
    scalar_type result = 0.0;
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
        scalar_type theta = std::atan2( std::abs(y), std::abs(x));
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

void XyzToLL(scalar_type& lat, scalar_type& lon, const scalar_type& x, const scalar_type& y, const scalar_type& z) {
    lat = std::atan2(z, std::sqrt(x*x + y*y)) / deg2rad;
    lon = atan4(y, x) / deg2rad;
};

scalar_type sphereDistance(const scalar_type latA, const scalar_type lonA, const scalar_type latB, const scalar_type lonB){
    if (std::abs(latB - latA) < ZERO_TOL && std::abs(lonB - lonA) < ZERO_TOL)
        return 0.0;
    else {
        scalar_type xA, yA, zA;
        scalar_type xB, yB, zB;
        llToXYZ(xA, yA, zA, latA, lonA);
        llToXYZ(xB, yB, zB, latB, lonB);
        
        const scalar_type cp1 = yA * zB - yB * zA;
        const scalar_type cp2 = xB * zA - xA * zB;
        const scalar_type cp3 = xA * yB - xB * yA;
        const scalar_type cpnorm = std::sqrt(cp1 * cp1 + cp2 * cp2 + cp3 * cp3);
        
        const scalar_type dotProd = xA * xB + yA * yB + zA * zB;
        
        return EARTH_RADIUS_KM * std::atan2(cpnorm, dotProd);
    }
}

scalar_type sphereDistance(const ll_coord_type& posA, const ll_coord_type& posB) {
    return sphereDistance(posA.first, posA.second, posB.first, posB.second);
}

void print_copyright(){
    std::cout <<  "------------------------------------------------------------------------------------------------\n";
    std::cout <<  "Stride Search. Copyright 2016 Sandia Corporation. Under the terms of contract DE-AC04-94AL85000,\n"; 
    std::cout <<  "there is a non-exclusive license for use of this work by or on behalf of the U.S. Government. \n";
    std::cout <<  "Export of this program may require a license from the United States Government.\n";
    std::cout <<  "------------------------------------------------------------------------------------------------\n";
}


// static PyObject* wrap_sphereDistance(PyObject* self, PyObject* args) {
//     double latA, lonA, latB, lonB;
//     double result;
//     /* Python -> C++ conversion */
//     if (!PyArg_ParseTuple(args, "dddd", &latA, &lonA, &latB, &lonB))
//         return NULL;
//         
//     /* call c++ function */
//     result = sphereDistance(latA, lonA, latB, lonB);
//     
//     /* c++ -> Python conversion */
//     return Py_BuildValue("d", result);
// }
// 
// static PyMethodDef UtilMethods[] = { {"sphereDistance", wrap_sphereDistance, METH_VARARGS, 
//     "Calculate the great-circle distance between two lat-lon locations (in degrees)"}, {NULL, NULL, 0, NULL} };
// 
// PyMODINIT_FUNC initStrideSearchUtilities(void) {
//     (void) Py_InitModule("StrideSearchUtilities", UtilMethods);
// }


}
