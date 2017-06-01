#ifndef _STRIDE_SEARCH_DATA_BASE_H_
#define _STRIDE_SEARCH_DATA_BASE_H_

#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchDateTime.h"
#include "StrideSearchWorkspace.h"
#include <string>
#include <vector>
#include <map>

namespace StrideSearch {

class StrideSearchData {
    public:
        StrideSearchData(const std::string fname, const std::vector<std::string>& varnames) : 
            filename(fname), variables(varnames), fileNTimesteps(0),  totalNTimesteps(0) {
            initTime();
        };
        virtual ~StrideSearchData(){};

        virtual void getGridDescription(index_type* gridDescInts) const = 0;
        virtual Workspace getSectorWorkingData(const std::vector<std::string>& varnames, 
            const std::vector<std::vector<index_type> >& dataIndices) = 0;
//         virtual void loadTimestepData(std::vector<IDCriterion*> criteria, const int time_index);       
        
        void updateSourceFile(std::string fname);
        void initTime();
        
        std::string getFilename() const {return filename;}
        
        virtual void read2DDataFromTimestep(const index_type time_index, const index_type level_index = 0) = 0;

    protected:
        virtual void initDimensions() = 0;
    
        std::string filename;
        std::vector<std::string> variables;
        
        std::vector<scalar_type> time;
       
        int fileNTimesteps;
        int totalNTimesteps;
};

}
#endif
