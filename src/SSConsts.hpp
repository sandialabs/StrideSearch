#ifndef _SS_CONSTS_HPP_
#define _SS_CONSTS_HPP_

#include "StrideSearchConfig.h"

namespace StrideSearch {
    /// Meters per second to Kilometers per hour conversion factor
    static const Real MPS2KPH = 3.6;
    /// Knots to meters per second conversion factor
    static const Real KTS2MPS = 0.5144444;
    /// Nautical miles to kilometers conversion factor
    static const Real NM2KM = 1.852;
    /// Pi
    static constexpr Real PI = 3.1415926535897932384626433832795027975;
    /// Radians to degrees conversion factor
    static constexpr Real RAD2DEG = 180.0 / PI;
    /// Degrees to radians conversion factor
    static constexpr Real DEG2RAD = PI / 180.0;
    /// Hours to days conversion factor
    static constexpr Real HOURS2DAYS = 1.0/24.0;
    /// Minutes to days conversion factor
    static constexpr Real MINUTES2DAYS = 1.0/24.0/60.0;
    /// Gravitational acceleration
    static constexpr Real G = 9.80616;

    /// Mean sea level radius of the Earth (meters)
    static constexpr Real EARTH_RADIUS_KM = 6371.220;
    
    static constexpr Real SQ_EARTH_RADIUS_KM = EARTH_RADIUS_KM*EARTH_RADIUS_KM;

    /// One sidereal day, in units of seconds
    static constexpr Real SIDEREAL_DAY_SEC = 24.0 * 3600.0;

    /// Rotational rate of Earth about its z-axis
    static constexpr Real EARTH_OMEGA_HZ = 2.0 * PI / SIDEREAL_DAY_SEC;

    /// Floating point zero
    static constexpr Real ZERO_TOL = 1.0e-11;

}
#endif