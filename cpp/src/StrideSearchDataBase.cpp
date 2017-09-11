#include "StrideSearchDataBase.h"
#include <vector>
#include <string> 
#include <netcdf>
#include <map>
#include <iostream>
#include <sstream>
#include <iomanip>

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
    time = std::vector<scalar_type>(fileNTimesteps, -1.0);
    time_var.getVar(timevals);
    
    std::vector<size_t> index(1,0);
    for (index_type k = 0; k < fileNTimesteps; ++k) {
        index[0] = k;
        time_var.getVar(index, &time[k]);
    }    
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
    }
    else {
        std::cerr << "Cannot find coordinate variables in file " << filename << std::endl;
    }
}


}