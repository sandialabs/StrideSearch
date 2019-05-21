ADD_LIBRARY(strideSearch SSUtilities.cpp  SSDateTime.cpp SSNCReader.cpp SSKdTree.cpp SSWorkspace.cpp
            SSIdCriteria.cpp SSEvent.cpp SSSector.cpp SSEventSet.cpp SSSectorSet.cpp)
# SSData.cpp) # StrideSearchEvent.cpp
#            StrideSearchSector.cpp StrideSearchWorkspaceDict.cpp StrideSearchIDCriterionBase.cpp
#            StrideSearchMinMaxCriteria.cpp StrideSearchSectorListBase.cpp StrideSearchSectorListLatLon.cpp
#            StrideSearchDataBase.cpp StrideSearchDataLatLon.cpp StrideSearchEventList.cpp
#            StrideSearchTrack.cpp StrideSearchTimer.cpp
#            )

TARGET_LINK_LIBRARIES(strideSearch ${netcdf} ${netcdf_c++})
