#include "StrideSearchDataLatLon.h"
#include <netcdf>
#include <iostream>
#include <sstream>
#include <cassert>

namespace StrideSearch {

void StrideSearchDataLatLon::initDimensions(){
    netCDF::NcFile file(filename, netCDF::NcFile::read);
    
    netCDF::NcVar latVar(file.getVar("lat"));
    netCDF::NcVar lonVar(file.getVar("lon"));
    
    nLat = latVar.getDim(0).getSize();
    nLon = lonVar.getDim(0).getSize();
    
    scalar_type latArr[nLat];
    scalar_type lonArr[nLon];
    latVar.getVar(latArr);
    lonVar.getVar(lonArr);
      
    lats = std::vector<scalar_type>(&latArr[0], &latArr[0] + nLat);
    lons = std::vector<scalar_type>(&lonArr[0], &lonArr[0] + nLon);
}

std::string StrideSearchDataLatLon::infoString() const {
    std::ostringstream ss;
    ss << StrideSearchData::infoString() << std::endl;
    ss << "StrideSearchDataLatLon:\n";
    ss << "\tnLat = " << nLat;
    ss << "\tnLon = " << nLon;
    ss << std::endl;
    return ss.str();
}

void StrideSearchDataLatLon::getGridDescription(int* gridDescInts) const {
    gridDescInts[0] = nLat;
    gridDescInts[1] = nLon;
}

std::string StrideSearchDataLatLon::basicInfo() const {
    std::stringstream ss;
    ss << "filename = " << filename << std::endl;
    ss << "nTimesteps = " << fileNTimesteps << ", start time = " << time[0] <<
        ", end time = " << time[time.size() - 1] << std::endl;
    ss << "nLat = " << nLat << " == " << lats.size() << ", nLon = " << nLon << " == " << lons.size() << std::endl;
    return ss.str();
}

///
/**
    Assumes variables have dimensions (time, level, lat, lon) or (time, lat, lon)
*/
 void StrideSearchDataLatLon::loadSectorWorkingData(Sector* sec, const index_type& tInd, const index_type& levInd) {
    netCDF::NcFile file(filename, netCDF::NcFile::read);
    //
    //  Loop over every WorkspaceDict in Sector
    //
    for (index_type wspcI = 0; wspcI < sec->workspace.size(); ++wspcI) {
        //
        //  Loop over every variable in WorkspaceDict
        //
        for (auto& wvar : sec->workspace[wspcI].dict) {
            netCDF::NcVar ncv(file.getVar(wvar.first));
            const index_type nDims = ncv.getDimCount();
            if (nDims == 4) {
                std::vector<size_t> getIndex(4,0);
                getIndex[0] = tInd;
                getIndex[1] = levInd;
                for (index_type i = 0; i < sec->data_indices.size(); ++i) {
                    getIndex[2] = sec->data_indices[i][0];
                    getIndex[3] = sec->data_indices[i][1];
                    ncv.getVar(getIndex, &wvar.second[i]);
                }
            }
            else if (nDims == 3) {
                std::vector<size_t> getIndex(3,0);
                getIndex[0] = tInd;
                for (index_type i = 0; i < sec->data_indices.size(); ++i) {
                    getIndex[1] = sec->data_indices[i][0];
                    getIndex[2] = sec->data_indices[i][1];
                    ncv.getVar(getIndex, &wvar.second[i]);
                }
            }
            else {
                std::cerr << "StrideSearchDataLatLon::loadSectorWorkingData ERROR: dimension number" << std::endl;
            }   
        }
    }
}

// void StrideSearchDataLatLon::readFullFile(const std::string var)
// {
//   for(int i = 0; i < nLat; i++){
//     read2DDataFromSingle(var,i);
//   }
// };
// 
// void StrideSearchDataLatLon::read2DDataFromSingle(const std::string var, const int latIndex)
// {
//   for(int i = 0; i < nLon; ++i){
//     getDatumValue(var,latIndex,i);
//   }
// }; 
// 
// void StrideSearchDataLatLon::readFullWChunks(const int time_index)
// {
//   for(int i = 0; i < nLon; ++i){
//     read2DDataFromTimestep(time_index,0);
//   }
// };
// 
// void StrideSearchDataLatLon::read2DDataFromTimestep(const int time_index, const int level_index){
//     netCDF::NcFile file(filename, netCDF::NcFile::read);    
//     for (auto& elem : nc_data.data2d) {
//         netCDF::NcVar ncv(file.getVar(elem.first));
//         std::vector<netCDF::NcDim> dims(ncv.getDims());
//         if (dims.size() == 3) {
//             std::vector<size_t> readStart;
//             readStart.push_back(time_index);
//             readStart.push_back(0);
//             readStart.push_back(0);
//             std::vector<size_t> readCount;
//             readCount.push_back(1);
//             readCount.push_back(1);
//             readCount.push_back(nLon);
//         
//             for (int i = 0; i < nLat; ++i) {
//                 readStart[1] = i;
//                 ncv.getVar(readStart, readCount, elem.second[i]);        
//             }
//         }
//         else if (dims.size() == 4) {
//             std::vector<size_t> readStart;
//             readStart.push_back(time_index);
//             readStart.push_back(level_index);
//             readStart.push_back(0);
//             readStart.push_back(0);
//             std::vector<size_t> readCount;
//             readCount.push_back(1);
//             readCount.push_back(1);
//             readCount.push_back(1);
//             readCount.push_back(nLon);
//             
//             for (int i = 0; i < nLat; ++i) {
//                 readStart[2] = i;
//                 ncv.getVar(readStart, readCount, elem.second[i]);
//             }
//         }
//     }
// };
// 
// double StrideSearchDataLatLon::getDatumValue(const std::string var, const int latInd, const int lonInd) {
//     return nc_data[var][latInd][lonInd];
// }
// 
// Workspace1D StrideSearchDataLatLon::getSectorWorkingData(const std::vector<std::string>& crit_vars, 
//         const std::vector<ll_index_type>& dataIndices) {
//     Workspace1D result(crit_vars, dataIndices.size());
//     for (int key_num = 0; key_num < crit_vars.size(); ++key_num) {
//         for (int i = 0; i < dataIndices.size(); ++i) {
//             result[crit_vars[key_num]][i] = 
//                 nc_data[crit_vars[key_num]][dataIndices[i].first][dataIndices[i].second];
//         }
//     }
//     return result;
// }

std::vector<ll_coord_type > StrideSearchDataLatLon::getLLCoordsFromIndices(
    const std::vector<vec_indices_type>& dataIndices) const {
    
    std::vector<ll_coord_type > result;
    for (int i = 0; i < dataIndices.size(); ++i) {
        result.push_back(ll_coord_type(lats[dataIndices[i][0]], lons[dataIndices[i][1]]));
    }
    return result;
}

}
