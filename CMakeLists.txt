CMAKE_MINIMUM_REQUIRED(VERSION 2.6)

project (StormSearch)
enable_language(Fortran)
enable_testing()

#
# Find NetCDF and NetCDF Fortran
#
FIND_LIBRARY(netcdf netcdf PATHS $ENV{NETCDF}/lib REQUIRED NO_DEFAULT_PATH  )
FIND_LIBRARY(netcdff netcdff PATHS $ENV{NETCDF}/lib REQUIRED NO_DEFAULT_PATH )
if ( netcdf )
	message("CMake found NetCDF! \n" ${netcdf} \n)
	message( "netcdf = " ${netcdf})
else ( netcdf)
	message("ERROR : CMake cannot find the NetCDF library.")
	message( "netcdf = " ${netcdf})
endif ( netcdf )
#
if ( netcdff )
	message("CMake found NetCDF Fortran! \n" ${netcdff} \n)
	message( "netcdff = " ${netcdff})
else (netcdff )
	message("ERROR : CMake cannot find the NetCDF Fortran library.")
	message( "netcdff = " ${netcdff})
endif ( netcdff)
INCLUDE_DIRECTORIES( $ENV{NETCDF}/include )

#
# set default build type to release
#
if ( NOT CMAKE_BUILD_TYPE)
	set (CMAKE_BUILD_TYPE RELEASE CACHE STRING "Choose type of build. Options are : NONE, DEBUG, RELEASE." FORCE)
endif ( NOT CMAKE_BUILD_TYPE)
#
# default installation
#
get_filename_component (default_prefix "." ABSOLUTE)
set (CMAKE_INSTALL_PREFIX ./install CACHE STRING
      "Choose the installation directory; by default it installs in the install directory."
      )
#set( CMAKE_VERBOSE_MAKEFILE true )      

#      
# FFLAGS depend on the compiler
#
if ( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
  # gfortran
  message( "** ** ** Setting gfortran compiler options ** ** **" )
#  set (CMAKE_Fortran_FLAGS_RELEASE "-O2 -fopenmp ")
#  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -fopenmp")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2 ")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
elseif ( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
  # ifort
  message( "** ** ** Setting ifort compiler options ** ** **" )
#  set (CMAKE_Fortran_FLAGS_RELEASE "-O2 -openmp ")
#  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -openmp")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2 ")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g ")
else ( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif ( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")

#
#	Add subdirectories
#
add_subdirectory(gfdlUtilities)      

#
#	StrideSearch library
#
ADD_LIBRARY( strideSearchLib StormListNode.f90 StrideSearchData.f90 StrideSearch.f90 
							TropicalStormListNode.f90 TropicalData.f90 TropicalStrideSearch.f90
							StormTrackData.f90 StormTracks.f90 
							PolarLowListNode.f90 PolarData.f90 PolarStrideSearch.f90
							)
TARGET_INCLUDE_DIRECTORIES( strideSearchLib PUBLIC ${PROJECT_BINARY_DIR}/gfdlUtilities  )
TARGET_LINK_LIBRARIES(strideSearchLib  gfdlUtilitiesLib ${netcdf} ${netcdff} )

#
#	Examples
#
ADD_EXECUTABLE( strideSearchDriver.exe StrideSearchDriver.f90 )
TARGET_INCLUDE_DIRECTORIES( strideSearchDriver.exe PUBLIC ${PROJECT_BINARY_DIR}/gfdlUtilities ${PROJECT_BINARY_DIR} )
TARGET_LINK_LIBRARIES( strideSearchDriver.exe strideSearchLib gfdlUtilitiesLib ${netcdf} ${netcdff} )

ADD_EXECUTABLE( polarSearchDriver.exe PolarSearchDriver.f90 )
TARGET_INCLUDE_DIRECTORIES( polarSearchDriver.exe PUBLIC ${PROJECT_BINARY_DIR}/gfdlUtilities ${PROJECT_BINARY_DIR} )
TARGET_LINK_LIBRARIES(polarSearchDriver.exe strideSearchLib gfdlUtilitiesLib ${netcdf} ${netcdff} )

ADD_EXECUTABLE( tropicalSearch.exe TropicalSearchDriver.f90 )
TARGET_INCLUDE_DIRECTORIES( tropicalSearch.exe PUBLIC ${PROJECT_BINARY_DIR}/gfdlUtilities ${PROJECT_BINARY_DIR} )
TARGET_LINK_LIBRARIES(tropicalSearch.exe strideSearchLib gfdlUtilitiesLib ${netcdf} ${netcdff} )

ADD_EXECUTABLE( trackDriver.exe trackDriver.f90 )
TARGET_INCLUDE_DIRECTORIES( trackDriver.exe PUBLIC ${PROJECT_BINARY_DIR}/gfdlUtilities ${PROJECT_BINARY_DIR} )
TARGET_LINK_LIBRARIES(trackDriver.exe strideSearchLib gfdlUtilitiesLib ${netcdf} ${netcdff} )


#
#	Tests
#
ADD_EXECUTABLE( stormListTest.exe StormListTester.f90 )
TARGET_LINK_LIBRARIES( stormListTest.exe strideSearchLib gfdlUtilitiesLib ${netcdf} ${netcdff} )
TARGET_INCLUDE_DIRECTORIES( stormListTest.exe PUBLIC ${PROJECT_BINARY_DIR}/gfdlUtilities ${PROJECT_BINARY_DIR} )
ADD_TEST( stormList_unitTest ${PROJECT_BINARY_DIR}/stormListTest.exe )

ADD_EXECUTABLE( searchDataInput.exe ReadDataTest.f90 )
TARGET_INCLUDE_DIRECTORIES( searchDataInput.exe PUBLIC ${PROJECT_BINARY_DIR}/gfdlUtilities ${PROJECT_BINARY_DIR} )
TARGET_LINK_LIBRARIES( searchDataInput.exe strideSearchLib gfdlUtilitiesLib ${netcdf} ${netcdff} )
ADD_TEST( dataInput_unitTest ${PROJECT_BINARY_DIR}/searchDataInput.exe)

ADD_TEST( strideSearchDriver_unitTest ${PROJECT_BINARY_DIR}/strideSearchDriver.exe)

ADD_EXECUTABLE( stormTrackDataTest.exe TrackDataTest.f90 )
TARGET_INCLUDE_DIRECTORIES( stormTrackDataTest.exe PUBLIC ${PROJECT_BINARY_DIR}/gfdlUtilities ${PROJECT_BINARY_DIR} )
TARGET_LINK_LIBRARIES( stormTrackDataTest.exe strideSearchLib gfdlUtilitiesLib ${netcdf} ${netcdff} )
ADD_TEST( stormTrackData_unitTest ${PROJECT_BINARY_DIR}/searchDataInput.exe)

install( TARGETS strideSearchLib trackDriver.exe strideSearchDriver.exe tropicalSearch.exe  polarSearchDriver.exe 
		 RUNTIME DESTINATION bin
		 LIBRARY DESTINATION lib
		 ARCHIVE DESTINATION lib )
