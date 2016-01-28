module StrideSearchDataModule
!> @file StrideSearchData.f90 Reads .nc file input for storm detection algorithm using procedures from TSTORMS.
!! Provides a container class for data read from a netcdf file.
!> @author Peter Bosler SNL
!!

!> @defgroup StrideSearchData StrideSearchData
!! @brief Reads .nc data for storm detection.
!!
!! @{
use netcdf
use get_date_mod, only : CURRENT_DATE
use netcdfUtilitiesModule

implicit none
public

!> @class StrideSearchData 
!> @brief Container class for data read from a netcdf file.
type StrideSearchData
	character(len=256) :: filename !< filename of netcdf file to be read
	integer :: ncFileID !< netcdf ID, output from @ref netcdfutilitiesmodule::getdimensions
	! coordinate definitions
	real, pointer :: lats(:) !< latitude vector from netcdf file
	real, pointer :: lons(:) !< longitude vector from netcdf file
	real, pointer :: time(:) !< time vector from netcdf file
	integer :: nLat !< number of longitudinal grid points in netcdf file
	integer :: nLon !< number of latitudinal grid points in netcdf file
	integer :: nTimesteps !< number of time steps in netcdf file
	! variables
	real, pointer :: vorticity(:,:) !< variables from netcdf file, will be overwritten at each time step
	real, pointer :: psl(:,:) !< variables from netcdf file, will be overwritten at each time step
	real, pointer :: wind(:,:) !< variables from netcdf file, will be overwritten at each time step
end type

contains

!> @brief Reads coordinate dimensions and allocates memory to prepare a StrideSearchData object.
!> Does not read variables, only coordinates.
!> @param searchData StrideSearchData to be initialized
!> @param filename filename of netcdf file to be read
subroutine InitializeData( searchData, filename )
	type(StrideSearchData), intent(out) :: searchData
	character(len=*), intent(in) :: filename
	!
	integer :: status
	
	searchData%filename = trim(filename)
	
	print *, "READING DATA FROM FILE : ", trim(filename)
	
	call GetDimensions(filename, searchData%ncFileID, searchData%nLon, searchData%nLat, searchData%nTimesteps)
	
	allocate(searchData%time(searchData%nTimesteps))
	allocate(searchData%lats(searchData%nLat))
	allocate(searchData%lons(searchData%nLon))
	
	call read_variable_1d(searchData%ncFileID, "lon", searchData%lons )
	call read_variable_1d(searchData%ncFileID, "lat", searchData%lats )
	call read_variable_1d(searchData%ncFileID, "time", searchData%time )
	
	allocate(searchData%vorticity(searchData%nLon, searchData%nLat))
	allocate(searchData%psl(searchData%nLon, searchData%nLat))
	allocate(searchData%wind(searchData%nLon, searchData%nLat))
end subroutine

!> @brief Frees memory associated with a StrideSearchData object.  
!> This memory was allocated by InitializeData.
!> @param searchData object to be deleted.
subroutine DeleteData(searchData)
	type(StrideSearchData), intent(inout) :: searchData
	deallocate(searchData%time)
	deallocate(searchData%lons)
	deallocate(searchData%lats)
	deallocate(searchData%vorticity)
	deallocate(searchData%psl)
	deallocate(searchData%wind)
end subroutine	

!> @brief Prints coordinate / dimension info about netcdf data to console
!> @param searchData
subroutine PrintCoordinateInfo(searchData)
	class(StrideSearchData), intent(in) :: searchData
	print *, "nLon = ", searchData%nLon
	print *, "nLat = ", searchData%nLat
	print *, "nTimesteps = ", searchData%nTimesteps
end subroutine

!> @brief Prints variable info about netcdf data. Must be called after ReadVariablesAtTimestep.
!> @param searchData
subroutine PrintVariableInfo(searchData)
	type(StrideSearchData), intent(in) :: searchData
	print *, "max vort. = ", maxval(searchData%vorticity)
	print *, "min vort. = ", minval(searchData%vorticity)
	print *, "max psl = ", maxval(searchData%psl)
	print *, "min psl = ", minval(searchData%psl)
	print *, "max wind = ", maxval(searchData%wind)
	print *, "min wind = ", maxval(searchData%wind)
end subroutine

!> @brief Reads storm identification data at a particular timestep, to be used by StrideSearch objects.
!> @param searchData data container, initialized by InitializeData.
!> @param year (output)
!> @param month (output)
!> @param day (output)
!> @param hour (output)
!> @param timeIndex index of timestep in netcdf file, from which variables should be read.
subroutine ReadVariablesAtTimestep( searchData, year, month, day, hour, timeIndex)
	type(StrideSearchData), intent(inout) :: searchData
	integer, intent(out) :: year, month, day, hour
	integer, intent(in) :: timeIndex
	!
	integer, dimension(3) :: start
	integer, dimension(6) :: date
	real :: rtime
	real, dimension(searchData%nLon, searchData%nLat) :: work1, work2
	
	start(1) = 1
	start(2) = 1
	start(3) = timeIndex
	
	rtime = searchData%time(timeIndex)
	call CURRENT_DATE( searchData%ncFileID, rtime, date)
	year = date(1)
	month = date(2)
	day = date(3)
	hour = date(4)
	
	call read_variable_2d( searchData%ncFileID, 'PSL', start, searchData%psl )
	
	call read_variable_2d( searchData%ncFileID, 'UBOT', start, work1)
	call read_variable_2d( searchData%ncFileID, 'VBOT', start, work2)
	searchData%wind = sqrt( work1*work1 + work2*work2)
	
	call read_variable_2d( searchData%ncFileID, 'VORBOT', start, searchData%vorticity)
end subroutine

!> @}
end module