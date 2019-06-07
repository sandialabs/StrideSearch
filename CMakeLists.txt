cmake_minimum_required(VERSION 3.1)
#
#   Project details
#
PROJECT(StrideSearch)
INCLUDE_DIRECTORIES(.)
SET( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin )
SET( CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib )
SET( CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib )
SET( StrideSearch_VERSION_MAJOR 2)
SET( StrideSearch_VERSION_MINOR 0)

cmake_policy(SET CMP0074 NEW)

INCLUDE_DIRECTORIES("${PROJECT_BINARY_DIR}")

SET (CMAKE_CXX_STANDARD 11)

#
#   Find NetCDF (required)
#
FIND_LIBRARY(netcdf netcdf PATHS $ENV{NETCDF_ROOT}/lib HINTS ${NETCDF_DIR}/lib REQUIRED NO_DEFAULT_PATH)
IF (netcdf)
    message("**** Found NetCDF: " ${netcdf})
ELSE (netcdf)
    message("CMake ERROR: Cannot find the NetCDF Library" \n)
ENDIF (netcdf)
FIND_LIBRARY(netcdf_c++ netcdf_c++4 PATHS $ENV{NETCDF_ROOT}/lib $ENV{NETCDF_CXX_ROOT}/lib HINTS ${NETCDF_CXX_DIR}/lib REQUIRED NO_DEFAULT_PATH)

IF (netcdf_c++)
    message("**** Found NetCDF C++ : " ${netcdf_c++})
ELSE (netcdf_c++)
    message("CMake ERROR: Cannot find the NetCDF C++ Library")
ENDIF (netcdf_c++)
INCLUDE_DIRECTORIES($ENV{NETCDF_ROOT}/include)

INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR}/tpl/include)

#
#   Optional packages for GPU 
#   
#   *not implemented yet*
#
set(CUDA_BUILD FALSE)
if (Kokkos_ROOT)   
    option(HAVE_KOKKOS "Located Kokkos library." ON)
    include(${Kokkos_ROOT}/kokkos_generated_settings.cmake)
    set(Kokkos_INCLUDE_DIRS ${Kokkos_ROOT}/include)
    set(Kokkos_LIBRARIES ${Kokkos_ROOT}/lib/libkokkos.a)
    string(FIND "${KOKKOS_GMAKE_DEVICES}" "Cuda" cuda_str_pos)
    if (${cuda_str_pos} GREATER -1)
        option(HAVE_CUDA "If true, indicates a Kokkos Cuda build." ON)
        set(CUDA_BUILD TRUE)
        execute_process(COMMAND ${CMAKE_CXX_COMPILER} "--nvcc-wrapper-show" 
	        RESULT_VARIABLE WRAPS_NVCC OUTPUT_VARIABLE WRAPS_NVCC_OUT)
        string (FIND ${WRAPS_NVCC_OUT} "nvcc" pos)
    endif()
else()
    MESSAGE(STATUS "Kokkos not found.  Stride Search will run without thread-parallelism.")
endif()

#
#   Optional parallel packages
#
FIND_PACKAGE(MPI)
If (MPI_FOUND)
    Option(HAVE_MPI "Use MPI for parallel search over files." ON)
endif()

#
#   Optional packages for documentation
#
FIND_PACKAGE(Doxygen)
IF (DOXYGEN_FOUND)
    #    Message("Doxygen found.")
    #
    #   Doxygen setup
    #

    #   dot
    EXECUTE_PROCESS(COMMAND which dot OUTPUT_VARIABLE DOT_PATH)
    IF (DOT_PATH) 
        SET(HAVE_DOT YES)
    #        Message("Graphviz found.")
    ELSE ()
        SET(HAVE_DOT NO)
    #        Message("Graphviz not found; this will affect the documentation output.")
    ENDIF()

    #   input/output files
    SET(DOXYGEN_IN ${CMAKE_CURRENT_SOURCE_DIR}/doc/Doxyfile.in)
    SET(DOXYGEN_OUT ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)

    #   configure files
    CONFIGURE_FILE(${DOXYGEN_IN} ${DOXYGEN_OUT} @ONLY)
    MESSAGE("Doxygen build enabled.  Use 'make docs' to build html documentation.")

    #   build docs
    ADD_CUSTOM_TARGET(docs COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYGEN_OUT}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} COMMENT "Generating API docs..." VERBATIM)
ELSE()
    Message("Doxygen not found -- documentation html will not be generated.")
ENDIF()


include_directories("${PROJECT_SOURCE_DIR}" "${PROJECT_SOURCE_DIR}/src")

#
#   Write configure file *** ALL CMAKE OPTIONS MUST BE SET BEFORE THIS POINT ***
#
CONFIGURE_FILE("${CMAKE_SOURCE_DIR}/StrideSearchConfig.h.in" "${PROJECT_BINARY_DIR}/StrideSearchConfig.h")
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

#
# Add subdirectories
#
add_subdirectory( src )
add_subdirectory( tests )
add_subdirectory( examples )

ENABLE_TESTING()
