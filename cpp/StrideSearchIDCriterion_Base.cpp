#include "StrideSearchIDCriteria_Base.h"

namespace StrideSearch {

IDCriterion::IDCriterion(const std::vector<std::string>& vars, const scalar_type thresh, const Event::IntensityComparison cmpr) : 
    varnames(vars), threshold(thresh), wspcIndex(-1), compareKind(cmpr) {};
    
IDCriterion::IDCriterion(const std::string& var, const scalar_type thresh, const Event::IntensityComparison cmpr) {
    varnames = std::vector<std::string>(1,var);
    threshold = thresh;
    wspcIndex = -1;
    compareKind = cmpr;
}

IDCriterion::IDCriterion(const std::string& var1, const std::string& var2, const scalar_type thresh, 
            const Event::IntensityComparison cmpr) {
    varnames = {var1, var2};
    threshold = thresh;
    wspcIndex = -1;
    compareKind = cmpr; 
            
}

}