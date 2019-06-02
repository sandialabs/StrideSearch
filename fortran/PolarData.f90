module PolarDataModule
!> @file PolarData.f90 Reads .nc files for polar low detection.
!! Provides a container class for data read from a netcdf file.
!> @author Peter Bosler SNL
!!
!> @defgroup PolarData PolarData
!> @brief Extends StrideSearchData to the application of polar low detection.
!!
!> @{
use StrideSearchDataModule
use netcdf
use netcdfUtilitiesModule
use get_date_mod, only : CURRENT_DATE

implicit none
public 

!> @class PolarData
!> @brief Container for netcdf data read from file.  
type, extends(StrideSearchData) :: PolarData
	real, pointer :: sst(:,:) !< Sea surface temperature (deg K)
	real, pointer :: theta700(:,:) !< potential temperature (deg K)
	real, pointer :: iceFrac(:,:) !< ice fraction
	character(len=256) :: ocnFilename !< filename for sst data
	integer :: sstFileID !< netcdf ID for sst file, output from netcdfutilitiesmodule::getdimensions
end type

contains

!> @brief Reads data required for polar low detection initializes two files, one ocean and one atmosphere.
!> Does not read data yet, only coordinates.  The ocean and atmosphere files are assumed to have the same grid.
!> @param pData PolarData output 
!> @param atmFilename filename of netcdf data file for atmosphere
!> @param ocnFilename filename of netcdf data for ocean
subroutine InitializePolarData( pData, atmFilename, ocnFilename )
	type(PolarData), intent(out) :: pData
	character(len=*), intent(in) :: atmFilename, ocnFilename
	!
	integer :: ocnNlon, ocnNLat, nSSTTimesteps
	
	call InitializeData( pData%StrideSearchData, atmFilename)
	
	pData%ocnFilename = trim(ocnFilename)
	
	call GetDimensions( ocnFilename, pData%sstFileID, ocnNlon, ocnNLat, nSSTTimesteps )
	
	allocate( pData%sst( pData%nLon, pData%nLat ) )
	allocate( pData%theta700( pData%nLon, pData%nLat) )
	allocate( pData%iceFrac( pData%nLon, pData%nLat) )
end subroutine

!> @brief frees memory associated with a PolarData object.
!> This memory was allocated by InitializePolarData.
!> @param pData object to be deleted.
subroutine DeletePolarData( pData )
	type(PolarData), intent(inout) :: pData
	deallocate(pData%sst)
	deallocate(pData%theta700)
	deallocate(pData%iceFrac)
	call DeleteData(pData%StrideSearchData)
end subroutine

!> @brief Reads meteorological and ocean data at a timestep.  Outputs the date.
!> Converts SST from C to K and computes potential temperature.
!> @param pData data container, initialized by InitializePolarData
!> @param year (output)
!> @param month (output)
!> @param day (output)
!> @param hour (output)
!> @param timeIndex index of timestep in netcdf file (input)
subroutine ReadPolarDataAtTimestep( pData, year, month, day, hour, timeIndex )
	type(PolarData), intent(inout) :: pData
	integer, intent(out) :: year, month, day, hour
	integer, intent(in) :: timeIndex
	!
	integer, dimension(3) :: start, sstStart
	integer, dimension(6) :: date
	real :: rtime
	real, dimension(pData%nLon, pData%nLat) :: work1, work2
	
	start(1) = 1
	start(2) = 1
	start(3) = timeIndex
	
	rtime = pData%time(timeIndex)
	call CURRENT_DATE( pData%ncFileID, rtime, date)
	year = date(1)
	month = date(2)
	day = date(3)
	hour = date(4)
	
	sstStart(1) = 1
	sstStart(2) = 1
	sstStart(3) = month
	
	call read_variable_2d( pData%ncFileID, 'PSL', start, pData%psl )
	
	call read_variable_2d( pData%ncFileID, 'UBOT', start, work1 )
	call read_variable_2d( pData%ncFileID, 'VBOT', start, work2 )
	pData%wind = sqrt( work1*work1 + work2*work2 )
	
	call read_variable_2d( pData%ncFileID, 'VOR850', start, pData%vorticity )
	
	call read_variable_2d( pData%ncFileID, 'T850', start, work1 )
	call read_variable_2d( pData%ncFileID, 'T500', start, work2 )
	work1 = work1 * (10.0 / 8.5 ) ** 0.286
	work2 = work2 * (2.0) ** 0.286
	pData%theta700 = 0.65 *  work1 + 0.35 * work2 
	
	call read_variable_2d( pData%sstFileID, "SST_cpl", sstStart, work1 )
	pData%sst = work1 + 273.0
	
	call read_variable_2d( pData%sstFileID, "ice_cov", sstStart, pData%iceFrac )
end subroutine

!> @brief Prints variable info about PolarData.  Must be called after ReadPolarDataAtTimestep.
!> @param pData
subroutine PrintPolarDataInfo( pData )
	type(PolarData), intent(in) :: pData
	call PrintVariableInfo(pData%StrideSearchData)
	print *, "max sst = ", maxval(pData%sst)
	print *, "min sst = ", minval(pData%sst)
	print *, "max \theta_{700} = ", maxval(pData%theta700)
	print *, "min \theta_{700} = ", minval(pData%theta700)
end subroutine

!> @}
end module