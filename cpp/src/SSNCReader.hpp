#ifndef _SS_NC_READER_HPP_
#define _SS_NC_READER_HPP_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSConsts.hpp"
#include "SSUtilities.hpp"
#include <netcdf>
#include <memory>
#include <map>

namespace StrideSearch {

struct Points {
    typedef Real coord_t;
    RealArray x;
    RealArray y;
    RealArray z;
    Int n;
    Points(const Int nn) : x(nn), y(nn), z(nn), n(nn) {}
};

class NCReader {
    public:
        virtual ~NCReader()  {}
        virtual Points makePoints() const;
        virtual Int nPoints() const = 0;
        
        RealArray getTime() const;
        
        void updateFile(const std::string& filename);
        
        std::string filename() const {return src_file;}
        
    protected:
        NCReader() {}
        NCReader(const std::string& filename); 
        
        std::shared_ptr<const netCDF::NcFile> ncfile;
        std::string src_file;
        
        virtual void initCoordinates() = 0;

};


class LatLonNCReader : public NCReader {
    public: 
        Points makePoints() const;    
    
        LatLonNCReader(const std::string& filename) : NCReader(filename) {
            initCoordinates();
        }
    
        void llIndices(Int& lat_ind, Int& lon_ind, const Int& pt_ind) const {
            lat_ind = pt_ind/n_lon;
            lon_ind = pt_ind%n_lon;
        }
    
    protected:
        Int n_lat;
        Int n_lon;
    
        void initCoordinates();
        
        RealArray lats;
        RealArray lons;
};


class UnstructuredNCReader : public NCReader {
    public: 
        Points makePoints() const;
    
        UnstructuredNCReader(const std::string& filename) : NCReader(filename) {
            initCoordinates();
        }
    
    protected:
        void initCoordinates();
        
        Int n_nodes;
        RealArray x;
        RealArray y;
        RealArray z;
};

}
#endif