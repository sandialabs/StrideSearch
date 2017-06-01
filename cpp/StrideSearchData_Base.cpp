#include "StrideSearchData_Base.h"
#include <vector>
#include <string> 
#include <netcdf>
#include <map>

void StrideSearchData::initTime(){
    std::vector<std::string> tstrings = {"time", "time_whole"};
    
    netCDF::NcFile file(filename, netCDF::NcFile::read);
    
    netCDF::NcVar time_var;
    bool timeFound = false;
    int nameInd = -1;
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
    
    double timevals[fileNTimesteps];
    time = std::vector<double>(fileNTimesteps, -1.0);
    time_var.getVar(timevals);
    
    std::vector<size_t> index(1,0);
    for (int k = 0; k < fileNTimesteps; ++k) {
        index[0] = k;
        time_var.getVar(index, &time[k]);
    }    
}

void StrideSearchData::updateSourceFile(std::string fname){
    filename = fname;
    initTime();
}

