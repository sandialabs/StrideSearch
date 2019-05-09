program StormTrackDataTest
!> @file TrackDataTest.f90
!> @brief Unit test for StormTrackData
!> @author Peter Bosler, SNL
use StormTrackDataModule

implicit none

character(len=256) :: detectionOutputFile = "detectionsTestFile.txt"

type(datedStormListNode), pointer :: stormPtr

integer :: i, readStat

type(StormTrackData) :: sData

open(unit=14,file=detectionOutputFile,status='OLD',action='READ',iostat = readStat)
if ( readStat /= 0 ) stop "ERROR opening detected storms file."

call InitializeTrackData( sData, 14)

call ReadStormTrackDataFile( sData, 14)

call PrintTrackDataInfo( sData )



close(12)

end program 