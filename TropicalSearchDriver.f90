program TropicalSearchDriver
!> @file TropicalSearchDriver.f90
!> @brief Driver program for spatial searches for tropical storms.
!> Demonstrates TropicalStormListNode, TropicalData, and TropicalStrideSearch.
!> @author Peter Bosler, SNL
use TropicalDataModule
use TropicalStormListNodeModule
use TropicalStrideSearchModule

implicit none

character(len = 256) :: ncfilename
character(len = 256) :: namelistfile
character(len = 256) :: outputDir, outputRoot, outputFile, outputFileRoot

real :: southernBoundary, northernBoundary
real :: sectorRadius
real :: pslThreshold, windThreshold, vortThreshold, vortPslDistThreshold, tempPslDistThreshold, tempExcessThreshold

integer :: argc
real :: programTimerStart, programTimerEnd
integer :: readStat

type(TropicalData) :: ncData
integer :: year, month, day, hour, timeIndex
type(TropicalStrideSearchSector) :: tSearch

type(TropicalStormListNode), pointer :: tstormList

namelist /input/ ncfilename, southernBoundary, northernBoundary, sectorRadius, &
				 pslThreshold, windThreshold, vortThreshold, vortPslDistThreshold, tempPslDistThreshold, &
				 tempExcessThreshold, outputDir, outputRoot

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	start program : read input from user
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
call cpu_time(programTimerStart)

print *, "Stride Search. Copyright 2016 Sandia Corporation. Under the terms of contract DE-AC04-94AL85000, ", &
		 "there is a non-exclusive license for use of this work by or on behalf of the U.S. Government. ", &
		 " Export of this program may require a license from the United States Government."


call GET_COMMAND_ARGUMENT(1, namelistfile)
open(unit=12, file=trim(namelistfile), status='OLD', action='READ', iostat=readStat)
	if ( readStat /= 0 ) stop "ERROR: cannot open namelist file"
	read(12, nml=input)
	print *, "---- reading data from file : ", trim(ncfilename), " -----"
	print *, "southernBoundary = ", southernBoundary
	print *, "northernBoundary = ", northernBoundary
	print *, "sectorRadius = ", sectorRadius
	print *, "pslThreshold = ", pslThreshold
	print *, "vortThreshold = ", vortThreshold
	print *, "windThreshold = ", windThreshold
	print *, "vortPslDistThreshold = ", vortPslDistThreshold
	print *, "tempExcessThreshold = ", tempExcessThreshold
	print *, "tempPslDistThreshold = ", tempPslDistThreshold
close(12)

write(outputFileRoot,'(A,A,A)') trim(outputDir), '/', trim(outputRoot)
write(outputFile,'(A,A)') trim(outputFileRoot), '.txt'

open(unit=14, file=outputFile, status='REPLACE', action='WRITE')

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	initialize search
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

call initializeTropicalData(ncData, ncfilename)
call PrintCoordinateInfo(ncData)

call TropicalSearchSetup( tSearch, southernBoundary, northernBoundary, sectorRadius, &
						  pslThreshold, vortThreshold, windThreshold, &
						  vortPslDistThreshold, tempExcessThreshold, tempPslDistThreshold, ncData)

call PrintTropicalSearchInfo( tSearch )						  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	search each timestep
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do timeIndex = 1, ncData%nTimesteps
	allocate(tstormList)
	if ( mod(timeIndex, 40) == 0 ) &
	write(6,'(A,I4,A,I4,A)') "processing time step ", timeIndex, " of ", ncData%nTimesteps, "..."
	
	call ReadTropicalVariablesAtTimestep( ncData, year, month, day, hour, timeIndex )
		
	call DoTropicalSearch( tstormList, tSearch, ncData )
		
	call MarkTropicalNodesForRemoval( tstormList, tSearch, southernBoundary, northernBoundary)
		
	!call ApplyTropicalLandMask( tstormList )
		
	call RemoveMarkedTropicalNodes(tstormList)
		
	!write(6,'(A,I4,A)') " found ", tstormList%listSize, " storms that match the per-timestep spatial criteria."
	
	call PrintTropicalTSTORMSOutput( tstormList, 14, year, month, day, hour )
	
	call deleteTropicalList(tstormList)
enddo


call FinalizeTropicalSearch(tSearch)
close(14)
call cpu_time(programTimerEnd)
write(6,*) "**** PROGRAM COMPLETE ***** elapsed time = ", (programTimerEnd - programTimerStart), " seconds."
end program