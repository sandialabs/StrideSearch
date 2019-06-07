ADD_LIBRARY(strideSearch SSUtilities.cpp  SSDateTime.cpp SSNCReader.cpp SSKdTree.cpp SSWorkspace.cpp
            SSIdCriteria.cpp SSEvent.cpp SSSector.cpp SSEventSet.cpp SSSectorSet.cpp SSSearchManager.cpp
            SSMPIManager.cpp SSInput.cpp SSTrack.cpp SSTrackSet.cpp)

TARGET_LINK_LIBRARIES(strideSearch ${netcdf} ${netcdf_c++})
