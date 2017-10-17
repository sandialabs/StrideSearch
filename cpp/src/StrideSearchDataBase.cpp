#include "StrideSearchDataBase.h"
#include <vector>
#include <string> 
#include <netcdf>
#include <map>
#include <iostream>
#include <sstream>
#include <iomanip>
#include <cmath>

namespace StrideSearch {

void StrideSearchData::initTime(){
    std::vector<std::string> tstrings = {"time", "time_whole"};
    
    netCDF::NcFile file(filename, netCDF::NcFile::read);
    
    netCDF::NcVar time_var;
    bool timeFound = false;
    index_type nameInd = -1;
    for (int i = 0; i < tstrings.size(); ++i){
        time_var = file.getVar(tstrings[i]);
        if (!time_var.isNull()) {
            nameInd = i;
            timeFound = true;
            break;           
        }
    }
    if (!timeFound) 
        std::cerr << "StrideSearchData ERROR: could not find time variable in file " << filename << std::endl;
    fileNTimesteps = time_var.getDim(0).getSize();
    totalNTimesteps += fileNTimesteps;   
    
    scalar_type timevals[fileNTimesteps];
    time_var.getVar(timevals);
    time = std::vector<scalar_type>(&timevals[0], &timevals[0] + fileNTimesteps);
}

std::string StrideSearchData::infoString() const {
    std::ostringstream ss;
    ss << "StrideSearchData:\n";
    ss << "\tfile = " << filename << std::endl;
    ss << "\tlats.size() = " << lats.size() << std::endl;
    ss << "\tlons.size() = " << lons.size() << std::endl;
    return ss.str();
}

void StrideSearchData::updateSourceFile(const std::string fname){
    filename = fname;
    initTime();
}

index_type StrideSearchData::get1dIndex(const index_type latI, const index_type lonJ) const {
    index_type result = -1;
    if (twoD) {
        const index_type nLat = lats.size();
        const index_type nLon = lons.size();
        result = lonJ * nLat + latI;
    }
    return result;
}

std::pair<index_type, index_type> StrideSearchData::get2dIndex(const index_type ind) const {
    std::pair<index_type, index_type> result(-1,-1);
    if (twoD) {
        const index_type nLat = lats.size();
        const index_type nLon = lons.size();
        result.second = ind / nLat;
        result.first = ind - result.second * nLat;
    }
    return result;
}

void StrideSearchData::initDimensions() {
    
    netCDF::NcFile file(filename, netCDF::NcFile::read);
    
    netCDF::NcVar lat_var = file.getVar("lat");
    netCDF::NcVar lon_var = file.getVar("lon");
    netCDF::NcVar x_var = file.getVar("coordx");
    netCDF::NcVar y_var = file.getVar("coordy");
    netCDF::NcVar z_var = file.getVar("coordz");
    netCDF::NcVar coord_var = file.getVar("coord");

    if (!lat_var.isNull() && !lon_var.isNull()) {
        //
        //  lat, lon
        //
        const index_type nLat = lat_var.getDim(0).getSize();
        const index_type nLon = lon_var.getDim(0).getSize();
        
        _nPoints = nLat * nLon;
        
        std::cout << "File : " << filename << std::endl;
        std::cout << "\tFound lat-lon coordinates: nLat = " << nLat << ", nLon = " << nLon << "; nNodes = " << 
            nLat * nLon << std::endl;   
    
        scalar_type latArr[nLat];
        scalar_type lonArr[nLon];
        lat_var.getVar(latArr);
        lon_var.getVar(lonArr);
      
        lats = std::vector<scalar_type>(&latArr[0], &latArr[0] + nLat);
        lons = std::vector<scalar_type>(&lonArr[0], &lonArr[0] + nLon);
        
        twoD = true;
        oneD = false;
        
        sphereRadius = 1.0;
        
    }
    else if (!x_var.isNull() && !y_var.isNull() && !z_var.isNull()) {
        //
        //  coordx, coordy, coordz
        //
        const index_type nNodes = x_var.getDim(0).getSize();
        
        std::cout << "File : " << filename << std::endl;
        std::cout << "\tFound x-y-z coordinates (1 x n): nNodes = " << nNodes << std::endl;  
        
        scalar_type xArr[nNodes];
        scalar_type yArr[nNodes];
        scalar_type zArr[nNodes];
        
        x_var.getVar(xArr);
        y_var.getVar(yArr);
        z_var.getVar(zArr);
        
        lats.reserve(nNodes);
        lons.reserve(nNodes);
        for (index_type i = 0; i < nNodes; ++i) {
            scalar_type lat;
            scalar_type lon;
            XyzToLL(lat, lon, xArr[i], yArr[i], zArr[i]);
            
            lats.push_back(lat);
            lons.push_back(lon);
        }
     
        oneD = true;
        twoD = false;
        
        scalar_type avg_norm = 0.0;
        for (int i=0; i<3; ++i) {
            avg_norm += std::sqrt(xArr[i]*xArr[i] + yArr[i]*yArr[i] + zArr[i]*zArr[i]);
        }
        avg_norm /= 3.0;
        
        sphereRadius = avg_norm;
        
        _nPoints = nNodes;    
    }
    else if (!coord_var.isNull()) {       
        //
        //  coord 
        //
        const index_type nNodes = coord_var.getDim(1).getSize();
        
        std::cout << "File : " << filename << std::endl;
        std::cout << "\tFound x-y-z coordinates (3xn): nNodes = " << nNodes << std::endl;
        
        scalar_type xyzArr[3][nNodes];
        
        coord_var.getVar(xyzArr);
        
        lats.reserve(nNodes);
        lons.reserve(nNodes);
        
        for (index_type i = 0; i < nNodes; ++i) {
            scalar_type lat;
            scalar_type lon;
            XyzToLL(lat, lon, xyzArr[0][i], xyzArr[1][i], xyzArr[2][i]);
            
            lats.push_back(lat);
            lons.push_back(lon);
        }
        
        oneD = true;
        twoD = false;
        
        scalar_type avg_norm = 0.0;
        for (int i=0; i<3; ++i) {
            avg_norm += std::sqrt(xyzArr[0][i]*xyzArr[0][i] + xyzArr[1][i]*xyzArr[1][i] + xyzArr[2][i]*xyzArr[2][i]);
        }
        avg_norm /= 3.0;
        
        sphereRadius = avg_norm;
        
        _nPoints = nNodes;
    }
    else {
        std::cerr << "Cannot find coordinate variables in file " << filename << std::endl;
    }
}

void StrideSearchData::loadSectorWorkingData(Sector* sec, const index_type& tInd, const index_type& levInd) {
    netCDF::NcFile file(filename, netCDF::NcFile::read);
    if (oneD) {
        //
        //  loop over every variable in sector's WorkspaceDict
        //
        for (index_type wspcInd = 0; wspcInd < sec->workspace.size(); ++wspcInd) {
            //
            //  loop over every variable in WorkspaceDict
            //
            for (auto& wvar : sec->workspace[wspcInd].dict) {
                netCDF::NcVar ncv(file.getVar(wvar.first));
                const index_type nDims = ncv.getDimCount();
                if (nDims == 2) { // e.g., (time, node)
                    std::vector<size_t> getIndex(2,0);
                    getIndex[0] = tInd;
                    for (index_type i = 0; i < sec->data_indices.size(); ++i) {
                        getIndex[1] = sec->data_indices[i][0];
                        ncv.getVar(getIndex, &wvar.second[i]);
                    }
                }
                else if (nDims == 3) { // e.g., (time, level, node)
                    std::vector<size_t> getIndex(3,0);
                    getIndex[0] = tInd;
                    getIndex[1] = levInd;
                    for (index_type i = 0; i < sec->data_indices.size(); ++i) {
                        getIndex[2] = sec->data_indices[i][0];
                        ncv.getVar(getIndex, &wvar.second[i]);
                    }
                }
                else {
                    std::cerr << "StrideSearchDataBase::loadSectorWorkingData ERROR: dimension number" << std::endl;
                }
            }
        }
    }
    else if (twoD) {
        //
        //  loop over every variable in sector's WorkspaceDict
        //
        for (index_type wspcInd = 0; wspcInd < sec->workspace.size(); ++wspcInd) {
            //
            //  loop over every variable in workspacedict
            //
            for (auto& wvar : sec->workspace[wspcInd].dict) {
                netCDF::NcVar ncv(file.getVar(wvar.first));
                const index_type nDims = ncv.getDimCount();
                if (nDims == 3) { // e.g., (time, lat, lon)
                    std::vector<size_t> getIndex(3,0);
                    getIndex[0] = tInd;
                    for (index_type i = 0; i < sec->data_indices.size(); ++i) {
                        getIndex[1] = sec->data_indices[i][0];
                        getIndex[2] = sec->data_indices[i][1];
                        ncv.getVar(getIndex, &wvar.second[i]);
                    }
                }
                else if (nDims == 4) { // e.g., (time, level, lat, lon)
                    std::vector<size_t> getIndex(4,0);
                    getIndex[0] = tInd;
                    getIndex[1] = levInd;
                    for (index_type i = 0; i < sec->data_indices.size(); ++i) {
                        getIndex[2] = sec->data_indices[i][0];
                        getIndex[3] = sec->data_indices[i][1];
                        ncv.getVar(getIndex, &wvar.second[i]);
                    }
                }
                else {
                    std::cerr << "StrideSearchDataBase::loadSectorWorkingData ERROR: dimension number" << std::endl;
                }
            }
        }
    }
}

}