program ReadDataTest
!> @file ReadDataTest.f90
!> @brief Unit test for StrideSearchData
!> @author Peter Bosler, SNL
use StrideSearchDataModule

implicit none

character(len=256) :: sampleFile = "/Volumes/Storage/polarStorms/ne30/"&
									//"f1850c5_ne30.cam.h2.0002-01-26-00000.nc"

type(StrideSearchData) :: ncData
integer :: k
integer :: year, month, day, hour

call initializeData(ncData, sampleFile)
call PrintCoordinateInfo(ncData)

do k = 1, 10
	call ReadVariablesAtTimestep( ncData, year, month, day, hour, k )
	print *, "year = ", year, ", month = ", month, ", day = ", day, ", hour = ", hour
	call PrintVariableInfo(ncData)
enddo

call DeleteData(ncData)

end program
