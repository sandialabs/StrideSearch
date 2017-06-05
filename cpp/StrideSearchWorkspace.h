#ifndef _STRIDE_SEARCH_WORKSPACE_H_
#define _STRIDE_SEARCH_WORKSPACE_H_

#include <vector>
#include <string>
#include <map>
#include <iostream>

namespace StrideSearch {

class Workspace1D {
    public:
    std::map<std::string, std::vector<scalar_type> > var_work;
    
    Workspace1D(std::vector<std::string> keys, const index_type nValsPerKey);
    
    std::vector<scalar_type>& operator[](std::string key) {return var_work[key];}
};
std::ostream& operator<<(std::ostream& os, const Workspace1D& wspc);

class Workspace2D {
    public :
        typedef scalar_type** ptr_type;
    
        index_type dim0_size;
        index_type dim1_size;
        bool allocated;
    
        std::map<std::string, ptr_type> data2d;
    
        ptr_type operator[](const std::string key) {return data2d[key];}

        Workspace2D(){dim0_size = 0; dim1_size = 0; allocated = false;}
        Workspace2D(const std::vector<std::string> var_names, const index_type size_dim0, const index_type size_dim1);

        Workspace2D& operator=(const Workspace2D& other); // copy assignment
    
        ~Workspace2D(){deleteMemory();}
        
        std::string basicInfo() const;
    private :
        std::vector<std::string> varnames;
        void allocMemory();
        void deleteMemory();
};
std::ostream& operator<<(std::ostream& os, const Workspace2D& wspc);

}
#endif
