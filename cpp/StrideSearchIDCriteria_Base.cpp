#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchIDCriteria_Base.h"

namespace StrideSearch {

IDCriterion::IDCriterion(const std::vector<std::string>& vars, const scalar_type thresh) : 
    varnames(vars), threshold(thresh) {};

IDCriterion::IDCriterion(const std::string& var, const scalar_type thresh) : threshold(thresh) {
    std::vector<std::string> varvec;
    varvec.push_back(var);
    varnames = varvec;
}


}
