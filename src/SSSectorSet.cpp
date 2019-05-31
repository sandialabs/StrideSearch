#include "SSSectorSet.hpp"
#include "SSConsts.hpp"
#include <cmath>
#include <exception>
#include <limits>
#include <sstream>

namespace StrideSearch {

template <typename DataLayout>
SectorSet<DataLayout>::SectorSet(const Real sb, const Real nb, const Real wb, const Real eb, const Real rad) : 
    southern_boundary(sb), northern_boundary(nb), eastern_boundary(eb), western_boundary(wb),
    radius(rad) {
    
    // check inputs
    if (sb < -90.0 || sb >= 90.0) {
        throw std::runtime_error("SectorSet error: invalid southern boundary.");
    }
    if (nb <= -90.0 || nb > 90.0) {
        throw std::runtime_error("SectorSet error: invalid northern boundary.");
    }
    if (nb - sb <= 0.0) {
        throw std::runtime_error("SectorSet error: invalid south-north extent.");
    }
    if (wb < 0.0 || wb >= 360.0) {
        throw std::runtime_error("SectorSet error: invalid western boundary.");
    }
    if (eb <= 0.0 || eb > 360.0) {
        throw std::runtime_error("SectorSet error: invalid eastern boundary.");
    }
    if (eb-wb <= 0.0) {
        throw std::runtime_error("SectorSet error: invalid west-east extent.");
    }
    
    const Real sector_arc_length = radius/EARTH_RADIUS_KM;
    nstrips = Int(std::floor((DEG2RAD*northern_boundary - DEG2RAD*southern_boundary)/sector_arc_length+1));
    lat_stride_degrees = (northern_boundary - southern_boundary)/(nstrips-1);
    lon_strides_degrees = std::vector<Real>(nstrips);
    for (Int i=0; i<nstrips; ++i) {
        const Real clat = southern_boundary + i*lat_stride_degrees;
        if (std::abs(std::abs(clat)-90.0) > ZERO_TOL) {
            lon_strides_degrees[i] = radius / (EARTH_RADIUS_KM * std::cos(DEG2RAD*clat)) * RAD2DEG;
        }
        else {
            lon_strides_degrees[i] = 360.0;
        }
    }
    
    for (Int i=0; i<nstrips; ++i) {
        const Real latI = southern_boundary + i*lat_stride_degrees;
        const Int nLonsOnStrip = std::ceil((eastern_boundary - western_boundary)/lon_strides_degrees[i]);
        for (Int j=0; j<nLonsOnStrip; ++j) {
            const Real lonJ = western_boundary + j*lon_strides_degrees[i];
            sectors.push_back(std::unique_ptr<Sector<DataLayout>>(new Sector<DataLayout>(latI, lonJ, radius, i)));
        }
    }
}

template <typename DataLayout>
SectorSet<DataLayout>::SectorSet(const RealArray& clats, const RealArray& clons, const Real rad) : radius(rad) {
    for (Int i=0; i<clats.size(); ++i) {
        sectors.push_back(std::unique_ptr<Sector<DataLayout>>(new Sector<DataLayout>(clats[i], clons[i], rad,-1)));
    }
}

template <typename DataLayout>
SectorSet<DataLayout>::SectorSet(const EventSet<DataLayout>& evs, const Real rad) : radius(rad) {
    for (Index i=0; i<evs.size(); ++i) {
        std::array<Real,2> ll = evs.getEventConst(i)->getCoords();
        sectors.push_back(std::unique_ptr<Sector<DataLayout>>(new Sector<DataLayout>(ll[0], ll[1], rad, -1)));
    }
}

template <typename DataLayout>
RealArray SectorSet<DataLayout>::centerLats() const {
    RealArray lats(sectors.size());
    for (Index i=0; i<sectors.size(); ++i) {
        lats[i] = sectors[i]->lat;
    }
    return lats;
}

template <typename DataLayout>
RealArray SectorSet<DataLayout>::centerLons() const {
    RealArray lons(sectors.size());
    for (Index i=0; i<sectors.size(); ++i) {
        lons[i] = sectors[i]->lon;
    }
    return lons;
}

template <typename DataLayout>
Int SectorSet<DataLayout>::maxPointsPerSector() const {
    Int result = 0;
    for (Index i=0; i<sectors.size(); ++i) {
        const Int ndp = sectors[i]->nDataPoints();
        if ( ndp > result)
            result = ndp;
    }
    return result;
}

template <typename DataLayout>
Int SectorSet<DataLayout>::minPointsPerSector() const {
    Int result = std::numeric_limits<Int>::max();
    for (Index i=0; i<sectors.size(); ++i) {
        const Int ndp = sectors[i]->nDataPoints();
        if (ndp < result)
            result = ndp;
    }
    return result;
}

template <typename DataLayout>
void SectorSet<DataLayout>::linkToData(const KDTree& tree, const std::shared_ptr<NCReader> ncr) {
    for (Index i=0; i<sectors.size(); ++i) {
        sectors[i]->linkToData(tree, ncr);
    }
}

template <typename DataLayout>
void SectorSet<DataLayout>::allocWorkspaces(const std::vector<std::shared_ptr<IDCriterion>>& criteria) {
    for (Index i=0; i<sectors.size(); ++i) {
        sectors[i]->allocWorkspaces(criteria);
    }
}

template <typename DataLayout>
std::string SectorSet<DataLayout>::infoString(const Int tab_lev, const bool printall) const {
    std::ostringstream ss;
    std::string tabstr("");
    for (int i=0; i<tab_lev; ++i) {
        tabstr += "\t";
    }
    ss << tabstr << "SectorSet record:" << std::endl;
    ss << tabstr << "\t(sb, nb, wb, eb) = (" << southern_boundary << ", " << northern_boundary << ", " 
       << western_boundary << "," << eastern_boundary << ")" << std::endl;
    ss << tabstr << "\tsize = " << sectors.size() << std::endl;
    ss << tabstr << "\tnstrips = " << nstrips << std::endl;
    ss << tabstr << "\tlat_stride_degrees = " << lat_stride_degrees << std::endl;
    ss << tabstr << "\tlon_strides_degrees = (";
    for (Int i=0; i<nstrips; ++i) {
        ss << lon_strides_degrees[i] << (i==nstrips-1 ? ")" : " ");
    }
    ss << std::endl;
    ss << tabstr << "\tmin. points per sector = " << minPointsPerSector() << std::endl;
    ss << tabstr << "\tmax. points per sector = " << maxPointsPerSector() << std::endl;
    ss << std::endl;
    if (printall) {
        for (Index i=0; i<sectors.size(); ++i) {
            ss << sectors[i]->infoString(tab_lev+1);
        }
    }
    ss << "--------------------------------------" << std::endl;
    return ss.str();
}

/// ETI
template class SectorSet<LatLonLayout>;
template class SectorSet<UnstructuredLayout>;
}
