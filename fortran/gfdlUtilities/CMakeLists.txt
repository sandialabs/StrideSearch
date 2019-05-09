ADD_LIBRARY( gfdlUtilitiesLib get_date.f90 time_manager.f90 utilities.f90 netcdfUtilities.f90 )

INCLUDE_DIRECTORIES( ${PROJECT_BINARY_DIR}/gfdlUtilities)

set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)

install( TARGETS gfdlUtilitiesLib 
		 LIBRARY DESTINATION lib
		 ARCHIVE DESTINATION lib )