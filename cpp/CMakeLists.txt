cmake_minimum_required(VERSION 2.8)

PROJECT(StrideSearch)

IF (CMAKE_VERSION VERSION_LESS "3.1")
  IF (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    SET (CMAKE_CXX_FLAGS "--std=gnu++11 ${CMAKE_CXX_FLAGS}")
  ENDIF ()
  IF (CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
    SET (CMAKE_CXX_FLAGS "--std=c++11 ${CMAKE_CXX_FLAGS}")
  ENDIF ()
ELSE ()
  SET (CMAKE_CXX_STANDARD 11)
ENDIF ()

FIND_LIBRARY(netcdf  netcdf PATHS $ENV{NETCDF_ROOT}/lib REQUIRED NO_DEFAULT_PATH)
IF (netcdf)
    message("**** Found NetCDF: " ${netcdf})
ELSE (netcdf)
    message("CMake ERROR: Cannot find the NetCDF Library" \n)
ENDIF (netcdf)
FIND_LIBRARY(netcdf_c++ netcdf_c++4.1 PATHS $ENV{NETCDF_ROOT}/lib REQUIRED NO_DEFAULT_PATH)
IF (netcdf_c++)
    message("**** Found NetCDF C++ : " ${netcdf_c++})
ELSE (netcdf_c++)
    message("CMake ERROR: Cannot find the NetCDF C++ Library")
ENDIF (netcdf_c++)
INCLUDE_DIRECTORIES($ENV{NETCDF_ROOT}/include)

#
# set default build type to release
#
if ( NOT CMAKE_BUILD_TYPE)
	set (CMAKE_BUILD_TYPE RELEASE CACHE STRING "Choose type of build. Options are : NONE, DEBUG, RELEASE." FORCE)
endif ( NOT CMAKE_BUILD_TYPE)
#
# default installation
#
set (CMAKE_INSTALL_PREFIX ${PROJECT_SOURCE_DIR}/install CACHE STRING
      "Choose the installation directory; by default StrideSearch installs in the install directory." )

ADD_LIBRARY(stridesearch StrideSearchUtilities.cpp StrideSearchEvent.cpp 
    StrideSearchDateTime.cpp StrideSearchWorkspace.cpp
    StrideSearchData_Base.cpp StrideSearchData_LatLon.cpp
    StrideSearchSectorList_Base.cpp StrideSearchSectorList_LatLon.cpp)
TARGET_LINK_LIBRARIES(stridesearch ${netcdf} ${netcdf_c++})

#
#   Testing
#
ENABLE_TESTING()
ADD_EXECUTABLE(ssUtilTest.exe SSUtilitiesTests.cpp)
TARGET_LINK_LIBRARIES(ssUtilTest.exe stridesearch)
ADD_TEST(NAME SSUtil COMMAND ${PROJECT_BINARY_DIR}/ssUtilTest.exe)

ADD_EXECUTABLE(ssEventTest.exe SSEventTests.cpp)
TARGET_LINK_LIBRARIES(ssEventTest.exe stridesearch)
ADD_TEST(NAME SSEvent COMMAND ${PROJECT_BINARY_DIR}/ssEventTest.exe)

ADD_EXECUTABLE(ssDateTimeTest.exe SSDateTimeTests.cpp)
TARGET_LINK_LIBRARIES(ssDateTimeTest.exe stridesearch)
ADD_TEST(NAME SSDatetime COMMAND ${PROJECT_BINARY_DIR}/ssDateTimeTest.exe)

ADD_EXECUTABLE(ssWorkspaceTest.exe SSWorkspaceTests.cpp)
TARGET_LINK_LIBRARIES(ssWorkspaceTest.exe stridesearch)
ADD_TEST(NAME SSWorkspace COMMAND ${PROJECT_BINARY_DIR}/ssWorkspaceTest.exe)

ADD_EXECUTABLE(ssLLDataTest.exe SSDataLatLonTests.cpp)
TARGET_LINK_LIBRARIES(ssLLDataTest.exe stridesearch)
ADD_TEST(NAME SSLLData COMMAND ${PROJECT_BINARY_DIR}/ssLLDataTest.exe)

ADD_EXECUTABLE(ssLLSectorTest.exe SSLLSectorListTests.cpp)
TARGET_LINK_LIBRARIES(ssLLSectorTest.exe stridesearch)
ADD_TEST(NAME SSLLSectors COMMAND ${PROJECT_BINARY_DIR}/ssLLSectorTest.exe)

#ADD_EXECUTABLE(ssSectorListTest.exe SSSectorListTests.cpp)
#TARGET_LINK_LIBRARIES(ssSectorListTest.exe stridesearch)
#ADD_TEST(NAME SSSectorList COMMAND ${PROJECT_BINARY_DIR}/ssSectorListTest.exe)