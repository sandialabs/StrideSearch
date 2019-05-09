module TropicalDataModule
!> @file TropicalData.f90 Reads .nc file input for storm detection algorithm using procedures from TSTORMS.
!! Extends StrideSearchData to tropical cyclone applications.
!> @author Peter Bosler SNL
!!

!> @defgroup TropicalData TropicalData
!! @brief Reads .nc data for tropical cyclone detection.
!!
!! @{
use StrideSearchDataModule
use netcdf
use netcdfUtilitiesModule
use get_date_mod, only : CURRENT_DATE

implicit none
public

!> @class TropicalData
!> @brief Extends StrideSearchData to tropical cylcone applications
type, extends(StrideSearchData) :: TropicalData
	real, pointer :: vertAvgT(:,:) !< vertically averaged temperature (500 hPa and 200 hPa)
	real, pointer :: thickness(:,:) !< thickness between 1000 hPa and 200 hPa surfaces
end type

contains

!> @brief initializes tropical cylcone detection data
!> @param tData container to be initialized
!> @param filename netcdf filename
subroutine InitializeTropicalData( tData, filename)
	type(TropicalData), intent(out) :: tData
	character(len=*), intent(in) :: filename
	
	call InitializeData( tData%StrideSearchData, filename )
	
	allocate(tData%vertAvgT( tData%nLon, tData%nLat ))
	allocate(tData%thickness( tData%nLon, tData%nLat))
end subroutine

!> @brief deletes and frees a tropical cylcone netcdf data container
!> @param tData
subroutine DeleteTropicalData( tData )
	type(TropicalData), intent(inout) :: tData
	deallocate(tData%vertAvgT)
	deallocate(tData%thickness)
	call DeleteData(tData%StrideSearchData)
end subroutine

!> @brief Prints basic variable info to console
!> @param tData
subroutine PrintTropicalVariableInfo( tData )
	type(TropicalData), intent(in) :: tData
	call PrintVariableInfo(tData%StrideSearchData)
	print *, "max vertAvgT = ", maxval(tData%vertAvgT)
	print *, "min vertAvgT = ", minval(tData%vertAvgT)
	print *, "max thickness = ", maxval(tData%thickness)
	print *, "min thickness = ", minval(tData%thickness)
end subroutine

!> @brief Reads netcdf data from file at a given timeIndex.
!> Computes vertically averaged temperature @f$ = \frac{1}{2}\left( T_{500} + T_{200} \right) @f$.
!> Computes thicknes @f$ = Z_{200} - Z_{1000}@f$.
!> Outputs the date in ymdh format.
!> @param tData 
!> @param year
!> @param month
!> @param day
!> @param hour
!> @param timeIndex
subroutine ReadTropicalVariablesAtTimestep( tData, year, month, day, hour, timeIndex )
	type(TropicalData), intent(inout) :: tData
	integer, intent(out) :: year, month, day, hour
	integer, intent(in) :: timeIndex
	!
	integer, dimension(3) :: start
	integer, dimension(6) :: date
	real :: rtime
	real, dimension(tData%nLon, tData%nLat) :: work1, work2
	
	start(1) = 1
	start(2) = 1
	start(3) = timeIndex
	
	rtime = tData%time(timeIndex)
	call CURRENT_DATE( tData%ncFileID, rtime, date)
	year = date(1)
	month = date(2)
	day = date(3)
	hour = date(4)
	
	call read_variable_2d( tData%ncFileID, 'PSL', start, tData%psl)
	
	call read_variable_2d( tData%ncFileID, 'U850', start, work1)
	call read_variable_2d( tData%ncFileId, 'V850', start, work2)
	tData%wind = sqrt( work1*work1 + work2*work2)
	
	call read_variable_2d( tData%ncFileID, 'VOR850', start, tData%vorticity)
	
	call read_variable_2d( tData%ncFileID, 'T200', start, work1)
	call read_variable_2d( tData%ncFileID, 'T500', start, work2)
	tData%vertAvgT = 0.5 * ( work1 + work2 )
	
	call read_variable_2d( tData%ncFileID, 'Z1000', start, work1)
	call read_variable_2d( tData%ncFileID, 'Z200', start, work2)
	tData%thickness = work2 - work1
end subroutine

!> @}
end module