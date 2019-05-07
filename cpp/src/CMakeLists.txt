ADD_LIBRARY(strideSearch StrideSearchUtilities.cpp StrideSearchDateTime.cpp StrideSearchEvent.cpp
            StrideSearchSector.cpp StrideSearchWorkspaceDict.cpp StrideSearchIDCriterionBase.cpp
            StrideSearchMinMaxCriteria.cpp #StrideSearchSectorListBase.cpp #StrideSearchSectorListLatLon.cpp
            StrideSearchDataBase.cpp StrideSearchEventList.cpp
            StrideSearchTrack.cpp StrideSearchTimer.cpp #StrideSearchNanoflannTree.cpp
            )

#FILE(GLOB STRIDE_SEARCH_CPP_FILES ${PROJECT_SOURCE_DIR}/*.cpp)
#ADD_LIBRARY(strideSearch ${STRIDE_SEARCH_CPP_FILES})
##TARGET_INCLUDE_DIRECTORIES(strideSearch PUBLIC ${})
TARGET_LINK_LIBRARIES(strideSearch ${netcdf} ${netcdf_c++})
