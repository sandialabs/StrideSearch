program StrideSearchDriver

use StrideSearchDataModule
use StrideSearchModule
use StormListNodeModule

implicit none

character(len = 256) :: ncfilename
character(len = 256) :: namelistfile
character(len = 256) :: outputDir, outputRoot, outputFile, outputFileRoot

character(len=256) :: sampleFile = "/Volumes/Storage/stormSearchData/latlon/ll181x360/"&
									//"f1850c5_ne240_rel06.cam.h2.0004-07-18-00000-181x360.nc"

real :: southernBoundary, northernBoundary
real :: sectorRadius
real :: pslThreshold, windThreshold, vortThreshold
logical :: runAsUnitTest

integer :: argc
real :: programTimerStart, programTimerEnd
integer :: readStat

type(StrideSearchData) :: ncData
integer :: year, month, day, hour, timeIndex

type(StrideSearchSector) :: sSearch

type(StormListNode), pointer :: stormList

namelist /input/ ncfilename, southernBoundary, northernBoundary, sectorRadius, &
				 pslThreshold, windThreshold, vortThreshold, outputDir, outputRoot

integer :: i, j, k, ii, jj, kk, nLon, nLat
integer :: nstrips, nsectors
integer, allocatable :: sectorPlot(:,:)
			 
				 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	start program : read input from user
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
call cpu_time(programTimerStart)

print *, "Stride Search. Copyright 2016 Sandia Corporation. Under the terms of contract DE-AC04-94AL85000, ", &
		 "there is a non-exclusive license for use of this work by or on behalf of the U.S. Government. ", &
		 " Export of this program may require a license from the United States Government."

runAsUnitTest = .FALSE.
if ( IARGC() < 1 ) then 
	runAsUnitTest = .TRUE.
	ncfilename = sampleFile
	southernBoundary = -90.0
	northernBoundary = 90.0
	sectorRadius = 3000.0
	pslThreshold = 99000.0
	vortThreshold = 0.0
	windThreshold = 20.0
	write(outputFileRoot,'(A)') './searchDriverUnitTest'
else
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
	close(12)		
	write(outputFileRoot,'(A,A,A)') trim(outputDir), '/', trim(outputRoot)
endif

call initializeData(ncData, ncfilename)
call PrintCoordinateInfo(ncData)
nLon = ncData%nlon
nLat = ncdata%nLat


call SearchSetup( sSearch, southernBoundary, northernBoundary, sectorRadius, &
				  pslThreshold, vortThreshold, windThreshold, ncData )
				  
if ( runAsUnitTest ) then	
	nstrips = size(sectorCenterLats) - 1
	k = 0
	do i = 1, nstrips + 1
		print *, "along latitude line Lat = ", sectorCenterLats(i), ": longitude stride = ", lonStrideReals(i), &
			" lonStrideInt = ", lonStrideInts(i)
		write(6,'(A)', advance='no') "longitudes = "
		do j = 1, nLonsPerLatLine(i)-1
			k = k + 1
			write(6,'(F8.3,A)', advance='no') sectorCenterLons(k), ','
			
		enddo
		k = k + 1
		write(6,'(F8.3)') sectorCenterLons(k)
	enddo
	print *, "counted nSectors = ", k
	nsectors = k
	
	do i = 1, nStrips + 1
		write(6,*) "nLonsPerLatLine = ", nLonsPerLatLine(i)
	enddo
			  
	write(outputFile, '(A,A)') trim(outputFileRoot), '.m'
	open(unit=12, file=trim(outputFile), status='REPLACE', action='WRITE' )
		
		write(12,'(A)',advance='no') 'centerLats = ['
		do i = 1, nstrips
			write(12,'(F8.3,A)', advance='no') sectorCenterLats(i), ', '
		enddo
		write(12,'(F8.3,A)') sectorCenterLats(nstrips + 1), '];'
		
		write(12,'(A)',advance='no') 'nLonsPerLatLine = ['
		do i = 1, nstrips
			write(12,'(I6,A)', advance='no') nLonsPerLatLine(i), ', '
		enddo
		write(12,'(I6,A)') nLonsPerLatLine(nstrips + 1), '];'
		
		k = 0
		write(12,'(A,I6,A,I6,A)') 'centerLons = -1 * ones(', nstrips, ',', maxval(nLonsPerLatLine), ');'
		do i = 1, nstrips + 1
			do j = 1, nLonsPerLatLine(i)
				k = k + 1
				write(12,'(A,I6,A,I6,A,F8.3,A)') 'centerLons(', i, ',', j, ') = ', sectorCenterLons(k), ';'
			enddo
		enddo
		write(12,'(A)') 'centerLons( centerLons < 0 ) = nan;'
		close(12)

	allocate(sectorPlot(nLat,nLon))		
		k = 0
		do i = 1, nstrips + 1
			call AllocateSectorMemory( ssearch, ncData, i )
			do j = 1, nLonsPerLatLine(i)
				k = k + 1
				call DefineSectorInData( sSearch, ncData, i, k )
				sectorPlot = 0
				do ii = 1, size(sSearch%myLatIs)
					do jj = 1, size(sSearch%myLonJs)
						if ( sSearch%neighborhood(ii,jj) )then
							sectorPlot( sSearch%myLatIs(ii), sSearch%myLonJs(jj) ) = 1
						endif
					enddo
				enddo
				print *, "k = ", k
				call PlotSector(sectorPlot, k)
			enddo
			call FinalizeSector(sSearch)
		enddo
	deallocate(sectorPlot)
	print *, "END OF UNIT TEST ONLY SECTION"
endif 

write(outputFile, '(A,A)') trim(outputFileRoot), '.txt'
open(unit=14, file=outputFile, status='REPLACE',action='WRITE')

do timeIndex = 1, ncData%nTimesteps
	write(6,'(A,I4,A,I4,A)') "processing time step ", timeIndex, " of ", ncData%nTimesteps, "..."
	allocate(stormList)
	
	call ReadVariablesAtTimestep( ncData, year, month, day, hour, timeIndex )
		
	call DoStrideSearch( stormList, sSearch, ncData )
	
	call MarkNodesForRemoval( stormList, sectorRadius, southernBoundary, northernBoundary )
	
	call RemoveMarkedNodes(stormList)
	
	call PrintTSTORMSOutput( stormList, 14, year, month, day, hour )
	
	call deleteList(stormList)
enddo

close(14)


contains

subroutine PlotSector( sectorPlot, sectorIndex )
	integer, intent(in) :: sectorPlot(:,:)
	integer, intent(in) :: sectorIndex
	character(len=56) :: filename
	integer :: i, j 
	integer :: nLat, nLon
	
	nLat = size(sectorPlot,1)
	nLon = size(sectorPlot,2)
	
	write(filename, '(A,I0.3,A)') 'sectorPlot', sectorIndex, '.m'
	
	open(unit=13, file=filename, status='REPLACE',action='WRITE')
		write(13,'(A)',advance='NO') 'secPlot = ['
		do i = 1, nLat-1
			do j = 1, nLon-1
				write(13, '(I3,A)', advance='NO') sectorPlot(i,j), ','
			enddo
			write(13,'(I3,A)') sectorPlot(i,nLon), ';'
		enddo
		do j = 1, nLon-1
			write(13, '(I3,A)', advance='NO') sectorPlot(nLat,j), ','
		enddo
		write(13,'(I3,A)') sectorPlot(nLat,nLon), '];'
	close(13)
end subroutine

end program 