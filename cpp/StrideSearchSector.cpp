#include "StrideSearchSector.h"
#include "StrideSearchWorkspaceDict.h"
#include "StrideSearchIDCriteria_Base.h"

namespace StrideSearch {

void Sector::BuildWorkspace(const std::vector<IDCriterion*>& criteria) {
    for (int i = 0; i < criteria.size(); ++i) {
        workspace[i] = WorkspaceDict(criteria[i]->varnames, data_indices.size());
    }
}
    
};