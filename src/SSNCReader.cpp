#include "SSNCReader.hpp"
#include <cmath>
#include <exception>
#include <sstream>

namespace StrideSearch {

NCReader::NCReader(const std::string& filename) {
    src_file = filename;
    ncfile = std::unique_ptr<const netCDF::NcFile>(new netCDF::NcFile(filename, netCDF::NcFile::read));
}

void NCReader::updateFile(const std::string& filename) {
    src_file = filename;
    ncfile = std::unique_ptr<const netCDF::NcFile>(new netCDF::NcFile(filename, netCDF::NcFile::read));
}

std::string NCReader::infoString(const Int tab_lev) const {
    std::ostringstream ss;
    std::string tabstr("");
    for (int i=0; i<tab_lev; ++i) {
        tabstr += "\t";
    }
    ss << tabstr << "NCReader record:" << std::endl;
    ss << tabstr << "\tsrc_file = " << src_file << std::endl;
    ss << tabstr << "\tlats.size() = " << lats.size() << std::endl;
    ss << tabstr << "\tlons.size() = " << lons.size() << std::endl;
    ss << tabstr << "\tavgResKm = " << avgResKm << std::endl;
    ss << tabstr << "\tnpoints = " << this->nPoints() << std::endl;
    ss << tabstr << "--------------------------------------" << std::endl;
    return ss.str();
}

void NCReader::printLats() const {
    std::cout << "lats = [";
    for (Index i=0; i<lats.size(); ++i) {
        std::cout << lats[i] << (i<lats.size()-1 ? "," : "];");
    }
    std::cout << std::endl;
}

void NCReader::printLons() const {
    std::cout << "lons = [";
    for (Index j=0; j<lons.size(); ++j) {
        std::cout << lons[j] << (j<lons.size()-1 ? "," : "];");
    }
    std::cout << std::endl;
}

RealArray NCReader::getTime(const DTUnits& dtu) const {
    const std::vector<std::string> time_vars = {"time", "time_whole"};
    netCDF::NcVar time_var;
    bool time_found = false;
    int name_index = -1;
    for (int i=0; i<time_vars.size(); ++i) {
        time_var = ncfile->getVar(time_vars[i]);
        if (!time_var.isNull()) {
            name_index = i;
            time_found = true;
            break;
        }
    }
    if (!time_found) {
        throw std::runtime_error("NCReader::getTime error: time variable not found.");
    }
    const Int nsteps = time_var.getDim(0).getSize();
    Real time_vals[nsteps];
    if (nsteps>0) {
        time_var.getVar(time_vals);
    }
    if (dtu != DTUnits::DAYS) {
        const Real conv_fac = (dtu == DTUnits::HOURS ? HOURS2DAYS : MINUTES2DAYS);
        for (Int i=0; i<nsteps; ++i) {
            time_vals[i] *= conv_fac;
        }
    }
    return RealArray(&time_vals[0], &time_vals[0]+nsteps);
}

void LatLonNCReader::initCoordinates() {
    netCDF::NcVar lat_var = ncfile->getVar("lat");
    netCDF::NcVar lon_var = ncfile->getVar("lon");
    
    if (!lat_var.isNull() && !lon_var.isNull()) {
        n_lat = lat_var.getDim(0).getSize();
        n_lon = lon_var.getDim(0).getSize();
        Real lats_arr[n_lat];
        Real lons_arr[n_lon];
        lat_var.getVar(lats_arr);
        lon_var.getVar(lons_arr);
        lats = RealArray(&lats_arr[0], &lats_arr[0] + n_lat);
        lons = RealArray(&lons_arr[0], &lons_arr[0] + n_lon);
    }
    else {
        throw std::runtime_error("LatLonNCReader::initData error: coordinate variables not found.");
    }
}

void LatLonNCReader::fillWorkspaceData(Workspace& wspc,
    const std::vector<typename LatLonLayout::horiz_index_type>& inds,
    const Int t_ind, const Int l_ind) const {
    /// @throws if Workspace size != inds.size()
    if (inds.size() != wspc.n) {
        throw std::runtime_error("LatLonNCReader::fillWorkspaceData error: workspace/indices size mismatch.");
    }
    for (auto& var : wspc.data) {
        if (var.first == "lat" || var.first == "latitude") {
            for (Int i=0; i<inds.size(); ++i) {
                var.second[i] = lats[inds[i][0]];
            }
        }
        else if (var.first == "area_weight" || var.first == "weight") {
            const Real dlam = this->resolutionEstimate()/EARTH_RADIUS_KM;
            for (Int i=0; i<inds.size(); ++i) {
                var.second[i] = SQ_EARTH_RADIUS_KM*dlam*dlam*std::cos(DEG2RAD*lats[inds[i][0]]);
            }
        }
        else {
            netCDF::NcVar ncv(ncfile->getVar(var.first));
            const Int ndim = ncv.getDimCount();
            if (ndim == 3) { // (time, lat, lon)
                auto getind = LatLonLayout::get_index_2d();
                getind[0] = t_ind;
                for (Int i=0; i<inds.size(); ++i) {
                    getind[1] = inds[i][0];
                    getind[2] = inds[i][1];
                    ncv.getVar(getind, &var.second[i]);
                }
            }
            else if (ndim == 4) { // (time, level, lat, lon)
                auto getind = LatLonLayout::get_index_3d();
                getind[0] = t_ind;
                getind[1] = l_ind;
                for (Int i=0; i<inds.size(); ++i) {
                    getind[2] = inds[i][0];
                    getind[3] = inds[i][1];
                    ncv.getVar(getind, &var.second[i]);
                }
            
            }
            else {
                /// @throws if getDimCount() result is unexpected.
                std::ostringstream ss;
                ss << "LatLonNCReader::fillWorkspaceData error: unsupported ndim value: " 
                   << var.first << " dimCount = " << ndim;
                throw std::runtime_error(ss.str());
            }
        }
    }
}

void UnstructuredNCReader::fillWorkspaceData(Workspace& wspc, 
    const std::vector<typename UnstructuredLayout::horiz_index_type>& inds,
    const Int t_ind, const Int l_ind) const {
    /// @throws if workspace size != inds.size
    if (inds.size() != wspc.n) {
        throw std::runtime_error("UnstructuredNCReader::fillWorkspaceData error: workspace/inds size mismatch.");
    }
    for (auto& var : wspc.data) {
    	if (var.first == "lat" || var.first == "latitude") {
    	    for (Int i=0; i<inds.size(); ++i) {
                var.second[i] = lats[inds[i][0]];
    	    }
    	}
    	else if (var.first == "area_weight" || var.first == "weight") {
            const Real dlam = this->resolutionEstimate()/EARTH_RADIUS_KM;
            for (Int i=0; i<inds.size(); ++i) {
                var.second[i] = SQ_EARTH_RADIUS_KM * dlam*dlam * std::cos(lats[inds[i][0]]);
            }
        }
    	else {
            netCDF::NcVar ncv(ncfile->getVar(var.first));
            const Int ndim = ncv.getDimCount();
            if (ndim == 2) { // (time, node)
                auto getind = UnstructuredLayout::get_index_2d();
                getind[0] = t_ind;
                for (Int i=0; i<inds.size(); ++i) {
                    getind[1] = inds[i][0];
                    ncv.getVar(getind, &var.second[i]);
                }
            }
            else if (ndim == 3) { // (time, level, node)
                auto getind = UnstructuredLayout::get_index_2d();
                getind[0] = t_ind;
                getind[1] = l_ind;
                for (Int i=0; i<inds.size(); ++i) {
                    getind[2] = inds[i][0];
                    ncv.getVar(getind, &var.second[i]);
                }
            }
            else {
                /// @throws if getDimCount() result is unexpected.
                throw std::runtime_error("UnstructuredNCReader::fillWorkspaceData error: unsupported ndim value.");
            }
        }
    }
}

Points LatLonNCReader::makePoints() const {
    Points result(n_lat*n_lon);
    for (Int i=0; i<n_lat; ++i) {
        const Real z = EARTH_RADIUS_KM * std::sin(lats[i]*DEG2RAD);
        for (Int j=0; j<n_lon; ++j) {
            const Real x = EARTH_RADIUS_KM * std::cos(lats[i]*DEG2RAD) * std::cos(lons[j]*DEG2RAD);
            const Real y = EARTH_RADIUS_KM * std::cos(lats[i]*DEG2RAD) * std::sin(lons[j]*DEG2RAD);
            const Int insert_ind = i*n_lon + j;
            result.x[insert_ind] = x;
            result.y[insert_ind] = y;
            result.z[insert_ind] = z;
        }
    }
    return result;
}

void UnstructuredNCReader::initCoordinates() {
    netCDF::NcVar x_var = ncfile->getVar("coordx");
    netCDF::NcVar y_var = ncfile->getVar("coordy");
    netCDF::NcVar z_var = ncfile->getVar("coordz");
    netCDF::NcVar coord_var = ncfile->getVar("coord");
    
    if (!x_var.isNull() && !y_var.isNull() && !z_var.isNull()) {
        n_nodes = x_var.getDim(0).getSize();
        
        Real xarr[n_nodes];
        Real yarr[n_nodes];
        Real zarr[n_nodes];
        x_var.getVar(xarr);
        y_var.getVar(yarr);
        z_var.getVar(zarr);
        
        lats = RealArray(n_nodes);
        lons = RealArray(n_nodes);
        for (Index i=0; i< n_nodes; ++i) {
            Real lat, lon;
            XyzToLL(lat, lon, xarr[i], yarr[i], zarr[i]);
            lats[i] = lat;
            lons[i] = lon;
        }
    }
    else if (!coord_var.isNull()) {
        n_nodes = coord_var.getDim(1).getSize();
        Real xyz_arr[3][n_nodes];
        coord_var.getVar(xyz_arr);
        
        lats = RealArray(n_nodes);
        lons = RealArray(n_nodes);
        for (Index i=0; i< n_nodes; ++i) {
            Real lat, lon;
            XyzToLL(lat, lon, xyz_arr[0][i], xyz_arr[1][i], xyz_arr[2][i]);
            lats[i] = lat;
            lons[i] = lon;
        }
    }
    else {
        throw std::runtime_error("UnstructuredNCReader::initDimensions error: coordinate variables not found.");
    }
}

Points UnstructuredNCReader::makePoints() const {
    Points result(n_nodes);
    for (Index i=0; i<n_nodes; ++i) {
        result.x[i] = EARTH_RADIUS_KM*std::cos(lats[i]*DEG2RAD)*std::cos(lons[i]*DEG2RAD);
        result.y[i] = EARTH_RADIUS_KM*std::cos(lats[i]*DEG2RAD)*std::sin(lons[i]*DEG2RAD);
        result.z[i] = EARTH_RADIUS_KM*std::sin(lats[i]*DEG2RAD);
    }
    return result;
}
    
}
