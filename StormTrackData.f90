module StormTrackDataModule
!> @file StormTrackData.f90 
!> @brief Extends @ref StormListNode to datedStormList node. 
!> Provides a container class for output from spatial search driver programs, to be used to construct storm tracks and
!> apply temporal storm identification criteria.
!!
!> @author Peter Bosler SNL
!!

!> @defgroup StormTrackData StormTrackData
!! @brief Reads output file from Stride Search (in TSTORMS format) into linked lists used for temporal filtering and 
!! building storm tracks.
!!
!! @{
use StormListNodeModule

implicit none
private
public datedStormListNode, initializeDatedNode
public AddDatedNodeToList, CopyDated, CopyDatedList, deleteDatedList
public StormTrackData
public InitializeTrackData, ReadStormTrackDataFile, DeleteStormTrackData, PrintTrackDataInfo

!> @class datedStormListNode
!> @brief Extends @ref StormListNode to include date information used for constructing storm tracks and applying
!> temporal storm identification criteria.
!> @extends StormListNode
type, extends(StormListNode) :: datedStormListNode
	integer, dimension(4) :: date = 0
	type(datedStormListNode), pointer :: nextDatedNode => null()
	contains
		procedure, public :: initializeDatedNode
end type

!> @class StormTrackData
!> @brief Container for output of many per-timestep spatial searches.
!> Each stormList pointer corresponds to the output of dostridesearch for one timestep.
type StormTrackData
	integer, dimension(4) :: startDate !< starting date of data set
	integer, dimension(4) :: endDate !< end date of data set
	integer :: nTimesteps !< number of timesteps within data set
	integer :: hoursPerTimestep = 6 !< timestep length (hours)
	type(datedStormListNode), pointer :: stormLists(:) => null() !< 1d array of per-timestep spatial search output
end type

contains

!> @brief Copies a linked list of datedStormListNode objects
!> @param dest destination list
!> @param source source list
subroutine CopyDatedList( dest, source )
	type(datedStormListNode), pointer, intent(inout) :: dest
	type(datedStormListNode), pointer, intent(in) :: source
	type(datedStormListNode), pointer :: currentSource
	currentSource => source
	do while ( associated(currentSource) )
		call AddDatedNodeToList( dest, currentSource)
		currentSource => currentSource%nextDatedNode
	enddo	
end subroutine

!> @brief initializes a datedStormListNode
!> @param listRoot node to be initialized (its data will be overwritten)
!> @param lon longitude of new storm
!> @param lat latitude of new storm
!> @param lonIndex index of storm's longitude in the data file longitude vector
!> @param latIndex index of storm's latitude in the data file latitude vector
!> @param vort vorticity of storm
!> @param psl minimum sea level pressure of storm
!> @param wind maximum wind of storm
!> @param date date of storm detection
subroutine initializeDatedNode( listRoot, lon, lat, lonIndex, latIndex, vort, psl, wind, date )
	class(datedStormListNode), intent(out) :: listRoot
	real, intent(in) :: lon, lat
	integer, intent(in) :: lonIndex, latIndex
	real, intent(in) :: vort, psl, wind
	integer, dimension(4), intent(in) :: date
	
	call initialize(listRoot%stormListNode, lon, lat, lonIndex, latIndex, vort, psl, wind )
	listRoot%date = date
	nullify(listRoot%nextDatedNode)
end subroutine

!> @brief Copies a datedStormListNode object
!> @param dest destination node
!> @param source source node
subroutine CopyDated( dest, source )
	class(datedStormListNode), intent(out) :: dest
	class(datedStormListNode), intent(in) :: source
	
	call Copy(dest%stormListNode, source%stormListNode)
	dest%date = source%date
	dest%nextDatedNode => source%nextDatedNode
end subroutine

!> @brief Adds an initialized (initializeDatedNode must be called on newNode prior to this subroutine) to a linked list.
!> @param listRoot first node in linked list
!> @param newNode node to be added
subroutine AddDatedNodeToList( listRoot, newNode)
	type(datedStormListNode), pointer, intent(inout) :: listRoot
	type(datedStormListNode), pointer, intent(in) :: newNode
	type(datedStormListNode), pointer :: current, next
	current => listRoot
	next => current%nextDatedNode
	if ( listRoot%listSize == 0 ) then
		call CopyDated(listRoot, newNode)
		listRoot%listSize = 1
		nullify(listRoot%nextDatedNode)
	else
		do while ( associated(next) )
			current => next
			next => current%nextDatedNode
		enddo
		
		allocate(next)
		call CopyDated(next, newNode )
		listRoot%listSize = listRoot%listSize + 1
		current%nextDatedNode => next
		nullify(next%nextDatedNode)		
	endif
end subroutine

!> @brief Removes a datedStormListNode from a linked list
!> @brief listRoot first node in list
!> @brief nodeToRemove pointer to node that will be deleted
subroutine RemoveDatedNodeFromList( listRoot, nodeToRemove )
	type(datedStormListNode), pointer, intent(inout) :: listRoot, nodeToRemove
	type(datedStormListNode), pointer :: foundNode, current, next, previous
	logical :: keepGoing
	
	nullify(current)
	nullify(foundNode)
	nullify(next)
	nullify(previous)
	
	if ( listRoot%listSize == 0 ) then
	
	elseif ( listRoot%listSize == 1 ) then
		listRoot%listSize = 0
	else
		current => listRoot
		keepGoing = .TRUE.
		do while ( associated(current) .AND. keepGoing )
			next => current%nextDatedNode
			if ( associated(current, nodeToRemove ) ) then
				foundNode => current
				keepGoing = .FALSE.
			else
				previous => current
				current => next
				next => current%nextDatedNode
			endif
		enddo
		
		if ( associated(foundNode, listRoot ) ) then
			next => listRoot%nextDatedNode
			next%listSize = listRoot%listSize - 1
			listRoot => next
			deallocate(foundNode)
		else
			listRoot%listSize = listRoot%listSize - 1
			next => current%nextDatedNode
			previous%nextDatedNode => next
			deallocate(foundNode)
		endif
	endif
end subroutine

!> @brief Frees memory used by a linked list
!> @param list list to be deleted
recursive subroutine deleteDatedList( list )
	type(datedStormListNode), pointer, intent(inout) :: list
	type(datedStormListNode), pointer :: next
	next => list%nextDatedNode
	deallocate(list)
	if ( associated(next) ) call deleteDatedList( next )
end subroutine

!> @brief Allocates memore for a StormTrackData object, based on an output file from a spatial search driver program.
!> @param trackData object to be initialized
!> @param fileunit integer unit of file containing spatial search output data
subroutine InitializeTrackData( trackData, fileunit )
	type(StormTrackData), intent(out) :: trackData
	integer, intent(in) :: fileunit
	trackData%nTimesteps = CountTimestepsInFile(fileunit)
	print *, "StormTrackData::InitializeTrackData : found ", trackData%nTimesteps, " timesteps in file."
	allocate(trackData%stormLists(trackData%nTimesteps))
end subroutine

!> @brief Reads spatial search output from a file, stores data in memory.  
!> InitializeTrackData must be called before this subroutine.
!> @param trackData data container
!> @param fileunit integer unit of file containing spatial search output data
subroutine ReadStormTrackDataFile( trackData, fileunit )
	class(StormTrackData), intent(inout) :: trackData
	integer, intent(in) :: fileunit
	!
	integer :: year, month, day, hour, i, j, readStat, lineNumber
	type(datedStormListNode), pointer :: tempNodePtr, timestepList
	integer :: lonIndex, latIndex, nStorms, date(4)
	real :: lon, lat, wind, psl, vort, temp, thick
	logical :: hasWarmCore, hasThickness
	
	allocate(tempNodePtr)
	rewind(fileunit)
	lineNumber = 0
	
	do i = 1, trackData%nTimesteps
		!
		! 	read time step header
		!
		read(fileunit,*,iostat=readStat) day, month, year, nStorms, hour
		lineNumber = lineNumber + 1
		
		if ( readStat > 0 ) then
			print *, "ReadStormTrackDataFile ERROR at input file line ", lineNumber
			return
		elseif ( readStat < 0 ) then
			print *, "ReadStormTrackDataFile WARNING : unexpected end of file at line ", lineNumber
			return
		endif	
		
		date(1) = year
		date(2) = month
		date(3) = day
		date(4) = hour
		if ( i == 1 ) then
			trackData%startDate = date
		elseif ( i == trackData%nTimesteps ) then
			trackData%endDate = date
		endif
				
		!
		!	read time step data
		!
		do j = 1, nStorms
			!
			!	read individual storm data
			!
			read(fileunit,*,iostat=readStat) lonIndex, latIndex, lon, lat, wind, vort, psl, hasWarmCore, hasThickness, temp, thick
			lineNumber = lineNumber + 1
			if ( readStat > 0 ) then
				print *, "ReadStormTrackDataFile ERROR at input file line ", lineNumber
				return
			elseif ( readStat < 0 ) then
				print *, "ReadStormTrackDataFile WARNING : unexpected end of file at line ", lineNumber
				return
			endif	
			
			
			call initializeDatedNode(tempNodePtr, lon, lat, lonIndex, latIndex, vort, psl, wind, date)
			timeStepList => trackData%stormLists(i)
			call AddDatedNodeToList( timeStepList, tempNodePtr )			
		enddo
	enddo
	
	deallocate(tempNodePtr)
	rewind(fileunit)
end subroutine

!> @brief Prints StormTrackData to console.  Should match the input file.
!> @param trackData
subroutine PrintTrackDataInfo( trackData )
	type(StormTrackData), intent(in) :: trackData
	!
	integer :: i, date(4)
	type(datedStormListNode), pointer :: timestepList, current, next
	
	do i = 1, trackData%nTimesteps
		timestepList => trackData%stormLists(i)
		if ( trackData%stormLists(i)%listSize > 0 ) then
			date = timestepList%date
			print *, date(3), date(2), date(1), timestepList%listSize, date(4)
			current => timestepList
			do while (associated(current))
				next => current%nextDatedNode
				print *, current%lonIndex, current%latIndex, current%lon, current%lat, current%wind, current%psl
				current => next
			enddo
		endif
	enddo
end subroutine

!> @brief Frees memory associated with a StormTrackData object
!> @param trackData
subroutine DeleteStormTrackData( trackData )
	type(StormTrackData), intent(inout) :: trackData
	!
	integer :: i
	type(datedStormListNode), pointer :: timestepList
	do i = 1, trackData%nTimesteps
		timestepList => trackData%stormLists(i)
		call deleteDatedList( timestepList )
	enddo
end subroutine

!> @brief Counts the number of timesteps in an output file from a spatial search driver program.
!> Used to allocate memory for a StormTrackData object.
!> @param fileunit integer unit of file containing spatial search output data
!> @return CountTimestepsInFile number of timesteps contained in spatial search output file
function CountTimestepsInFile( fileunit )
	integer :: CountTimestepsInFile
	integer, intent(in) :: fileunit
	integer :: day, month, year, number, hour, i, readStat, lineNumber
	character(len=256) :: lineIn
	logical :: keepGoing
	
	CountTimestepsInFile = 0
	lineNumber = 0
	keepGoing = .TRUE.
	do while (keepGoing)
		read(fileunit,*, iostat = readStat) day, month, year, number, hour
		lineNumber = lineNumber + 1
		if ( readStat > 0 ) then
			print *, "CountTimestepsInFile ERROR reading line ", lineNumber, " of detected storms file."
			keepGoing = .FALSE.			
		elseif ( readStat < 0 ) then
			!print *, "CountTimestepsInFile End of file found at line ", lineNumber
			keepGoing = .FALSE.
			exit
		endif
		do i = 1, number
			read(fileunit,'(A)',iostat=readStat) lineIn
			lineNumber = lineNumber + 1
			if ( readStat > 0 ) then
				print *, "CountTimestepsInFile ERROR reading line ", lineNumber, " of detected storms file."
				keepGoing = .FALSE.			
			elseif ( readStat < 0 ) then
				print *, "CountTimestepsInFile End of file found at line ", lineNumber
				keepGoing = .FALSE.
			endif
		enddo
		CountTimestepsInFile = CountTimestepsInFile + 1
	enddo 	
	rewind(fileunit)
end function

!> @}
end module
