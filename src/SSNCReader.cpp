#include "SSNCReader.hpp"
#include <cmath>
#include <exception>

namespace StrideSearch {

NCReader::NCReader(const std::string& filename) {
    src_file = filename;
    ncfile = std::unique_ptr<const netCDF::NcFile>(new netCDF::NcFile(filename, netCDF::NcFile::read));
}

void NCReader::updateFile(const std::string& filename) {
    src_file = filename;
    ncfile = std::unique_ptr<const netCDF::NcFile>(new netCDF::NcFile(filename, netCDF::NcFile::read));
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

RealArray NCReader::getTime() const {
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
    time_var.getVar(time_vals);
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