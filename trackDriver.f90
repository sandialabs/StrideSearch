program TrajectoryDriver
!> @file trackDriver.f90
!> @brief Constructs storm tracks from spatial search driver output.
!> Demonstrates @ref StormTrackData and @ref TrackListNode
!> @author Peter Bosler, SNL
use StormTrackDataModule
use TrackModule

implicit none

character(len=256) :: detectionOutputFile , outputDir, outputRoot, outputFileRoot, outputFile, namelistfile
type(StormTrackData) :: stData
type(TrackListNode), pointer :: trackList

integer :: readStat
real :: programTimerStart, programTimerEnd

integer :: minDuration, hoursPerTimestep
real :: maxTravelSpeed
logical :: land_mask

namelist /input/ detectionOutputFile, minDuration, hoursPerTimestep, maxTravelSpeed, outputDir, outputRoot, land_mask

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	start program : read input from user
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
call cpu_time(programTimerStart)

print *, "Stride Search. Copyright 2015 Sandia Corporation. Under the terms of contract DE-AC04-94AL85000, ", &
		 "there is a non-exclusive license for use of this work by or on behalf of the U.S. Government. ", &
		 " Export of this program may require a license from the United States Government."

call GET_COMMAND_ARGUMENT(1, namelistfile)
open(unit=12, file=trim(namelistfile), status='OLD', action='READ', iostat=readStat)
	if ( readStat /= 0 ) stop "TrackDriver ERROR: cannot open namelist file"
	read(12, nml=input)
close(12)	

print *, "namelist reading done."

write(outputFileRoot,'(A,A,A)') trim(outputDir), '/', trim(outputRoot)
write(outputFile,'(A,A)') trim(outputFileRoot), '.txt'



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	initialize tracks, read detected storms data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

print *, "Reading data from file : ", trim(detectionOutputFile)

open(unit=12, file=trim(detectionOutputFile), status='OLD', action='READ', iostat=readStat)
	if ( readStat /= 0 ) stop "TrackDriver ERROR: cannot open storms data file"
	call InitializeTrackData( stData, 12 )
	call ReadStormTrackDataFile( stData, 12)
	call PrintTrackDataInfo(stData)
close(12)

print *, "reading data done."
!call PrintTrackDataInfo( stData )

allocate(trackList)

call GenerateStormTracks( trackList, stData, minDuration, maxTravelSpeed, hoursPerTimestep )

if ( land_mask ) call MarkStormsOverLand( trackList )

print *, "tracks generated"
open(unit=14, file=outputFile, status='REPLACE', action='WRITE')
	call PrintTrajectories(trackList, 14)
close(14)
print *, "outputfile done."

call DeleteStormTracks(trackList)
print *, "tracklist deleted."

!call DeleteStormTrackData(stData)
print *, "storm track data deleted."



end program