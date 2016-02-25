program PolarSearch
!> @file PolarSearch.f90
!> @brief Driver program for a StrideSearch of polar latitudes.
!!
!> @author Peter Bosler, SNL


use PolarDataModule
use PolarLowListNodeModule
use PolarStrideSearchModule

implicit none

character(len = 256) :: atmfilename, ocnfilename
character(len = 256) :: namelistfile
character(len = 256) :: outputDir, outputRoot, outputFile, outputFileRoot

real :: southernBoundary, northernBoundary
real :: sectorRadius
real :: pslThreshold, windThreshold, vortThreshold, airSeaTempThreshold, vortDistThreshold, iceThreshold

integer :: argc
real :: programTimerStart, programTimerEnd, OMP_GET_WTIME
integer :: readstat

type(PolarData) :: ncData
integer :: year, month, day, hour, timeIndex
type(PolarStrideSearchSector) :: sSearch

type(PolarLowListNode), pointer :: storms

namelist /input/ atmfilename, ocnfilename, &
				 southernBoundary, northernBoundary, sectorRadius, &
				 pslThreshold, windThreshold, vortThreshold, airSeaTempThreshold, vortDistThreshold, iceThreshold, &
				 outputDir, outputRoot

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	read input from user
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

call cpu_time(programTimerStart)

print *, "Stride Search. Copyright 2016 Sandia Corporation. Under the terms of contract DE-AC04-94AL85000, ", &
		 "there is a non-exclusive license for use of this work by or on behalf of the U.S. Government. ", &
		 " Export of this program may require a license from the United States Government."

call GET_COMMAND_ARGUMENT(1, namelistfile)
open(unit = 12, file=trim(namelistfile), status='OLD', action='READ', iostat = readstat)
	if ( readstat /= 0 ) stop "ERROR : cannot open namelist file."
	rewind(12)
	read(12, nml=input)
	print *, "southernBoundary = ", southernBoundary
	print *, "northernBoundary = ", northernBoundary
	print *, "sectorRadius = ", sectorRadius
	print *, "pslThreshold = ", pslThreshold
	print *, "vortThreshold = ", vortThreshold
	print *, "windThreshold = ", windThreshold
	print *, "\theta_{700} - SST threshold = ", airSeaTempThreshold
	print *, "vortDistThreshold = ", vortDistThreshold
	print *, "ice fraction threshold = ", iceThreshold
close(12)

write(outputFileRoot,'(A,A,A)') trim(outputDir), '/', trim(outputRoot)
write(outputFile,'(A,A)') trim(outputFileRoot), '.txt'

open(unit = 14, file=outputFile, status='REPLACE', action='WRITE')

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	initialize search
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

call initializePolarData( ncData, atmfilename, ocnfilename )
call PrintCoordinateInfo( ncData )

call PolarSearchSetup( sSearch, southernBoundary, northernBoundary, sectorRadius, &
				  pslThreshold, vortThreshold, windThreshold, airSeaTempThreshold, vortDistThreshold, iceThreshold, ncData )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	search each timestep
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do timeIndex = 1, ncData%nTimesteps
	allocate(storms)
	
	write(6,'(A,I4,A,I4,A)',advance='no') "processing time step ", timeIndex, " of ", ncData%nTimesteps, "..."
	
	call ReadPolarDataAtTimestep( ncData, year, month, day, hour, timeIndex )
	!call PrintPolarDataInfo( ncData )

	call DoPolarSearch( storms, sSearch, ncData )
	
	call MarkPolarNodesForRemoval(storms, sSearch, southernBoundary, northernBoundary )
	
	call RemoveMarkedPolarNodes(storms)
	
	write(6,'(A,I4,A)') " found ", storms%listSize, " storms that match the per-timestep spatial criteria."
	
	call PrintPolarOutputForTracker( storms, 14, year, month, day, hour )
	
	call deletePolarList(storms)	
enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	clean up
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

close(14)

!programTimerEnd = OMP_GET_WTIME()
call cpu_time(programTimerEnd)
write(6,*) "**** PROGRAM COMPLETE ***** elapsed time = ", (programTimerEnd - programTimerStart), " seconds."
end program