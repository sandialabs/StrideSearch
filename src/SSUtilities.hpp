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

std::ostream& operator << (std::ostream& os, const std::array<Index,1>& arr);
std::ostream& operator << (std::ostream& os, const std::array<Index,2>& arr);
std::ostream& operator << (std::ostream& os, const std::array<Index,3>& arr);

/// Class to show output progress in console
/**
*/
class ProgressBar {
    /// Name of process being tracked
    std::string name;
    /// Write frequency in percentage points
    Real write_freq;
    /// Number of total iterations
    Int n_iter;
    /// Number of current iteration
    Int iter;
    /// Next console update in percentage points
    Real next;
    /// Output stream
    std::ostream& os;
    
    public:
        ProgressBar(const std::string& nm, const Int nIter, const Real wf=1.0, std::ostream& ss = std::cout) :
            name(nm), n_iter(nIter), write_freq(wf), os(ss), iter(0), next(wf) {
            os << name << ": 0";
            os.flush();
        }
        
        void update() {
            ++iter;
            const Real p = 100*iter/n_iter;
            if (p>=next || iter==n_iter) {
                os << " " << p;
                if (iter == n_iter) 
                    os << std::endl;
                os.flush();
                next += write_freq;
            }
        }
        
};

}
#endif
