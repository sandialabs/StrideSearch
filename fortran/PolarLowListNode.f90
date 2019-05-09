module PolarLowListNodeModule
!> @file PolarLowListNode.f90 
!> @brief Provides a linked-list structure for polar low detection.
!> @author Peter Bosler SNL

!> @defgroup PolarLowListNode PolarLowListNode
!> @brief Extends StormListNode to the application of polar low detection.
!> @{
use StormListNodeModule

implicit none

private
public PolarLowListNode, initializePolar, addPolarNodeToList, removePolarNodeFromList, deletePolarList
public PrintPolarOutputForTracker

!> @class PolarLowListNode
!> @brief Extends the basic StormListNode to include data relevant to polar low identification.
type, extends(StormListNode) :: PolarLowListNode
	real :: airSeaTempDiff = 0.0 !< @f$ \theta_{700} - SST @f$ (K)
	real :: vortLon = 0.0	!< longitude of cyclonic vorticity max (degress east)
	real :: vortLat = 0.0 !< latitude of cyclonic vorticity max (degrees north)
	type(PolarLowListNode), pointer :: nextPolar => null()
	
	contains
		procedure, public :: initializePolar
end type

contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!	node functions (not pointers)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!> @brief Initializes a PolarLowListNode.
!> @param listRoot list to be initialized
!> @param lon longitude of storm
!> @param lat latitude of storm
!> @param lonIndex index of storm's longitude in grid point array
!> @param latIndex index of storm's latitude in grid point array
!> @param pslMin sea level pressure minimum 
!> @param vortMax cyclonic vorticity max
!> @param windMax windspeed maximum
!> @param vortLon longitude of vorticity maximum
!> @param vortLat latitude of vorticity maximum
!> @param airSeaDT air-sea temp difference
subroutine initializePolar( listRoot, lon, lat, lonIndex, latIndex, pslMin, vortMax, windMax, vortLon, vortLat, airSeaDT )
	class(PolarLowListNode), intent(inout) :: listRoot
	real, intent(in) :: lon, lat
	integer, intent(in) :: lonIndex, latIndex
	real, intent(in) :: pslMin, vortMax, windMax, vortLon, vortLat, airSeaDT
	
	call initialize(listRoot%stormListNode, lon, lat, lonIndex, latIndex, vortMax, pslMin, windMax )
	
	listRoot%airSeaTempDiff = airSeaDT
	listRoot%vortLon = vortLon
	listRoot%vortLat = vortLat
end subroutine

!> @brief copies data from one PolarLowListNode to another.
!> @param dest node with data to be overwritten
!> @param source node with source data
subroutine CopyPolar( dest, source )
	class(PolarLowListNode), intent(inout) :: dest
	class(PolarLowListNode), intent(in) :: source
	
	call Copy(dest%stormListNode, source%stormListNode)
	dest%vortLon = source%vortLon
	dest%vortLat = source%vortLat
	dest%airSeaTempDiff = source%airSeaTempDiff
	dest%nextPolar => source%nextPolar
end subroutine

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!	list functions (pointers)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!> @brief Adds a PolarLowListNode to a list
!> @brief listRoot list to which newNode should be added
!> @brief newNode node to add
subroutine addPolarNodeToList( listRoot, newNode ) 
	type(PolarLowListNode), pointer, intent(inout) :: listRoot
	type(PolarLowListNode), pointer, intent(in) :: newNode
	type(PolarLowListNode), pointer :: current, next
	
	current => listRoot
	next => listRoot%nextPolar
	if ( listRoot%listSize == 0 ) then
		call CopyPolar( listRoot, newNode )
		listRoot%listSize = 1
	else
		do while ( associated(next) )
			current => next
			next => current%nextPolar
		enddo
		listRoot%listSize = listRoot%listSize + 1
		allocate(next)
		call CopyPolar( next, newNode )
		current%nextPolar => next
		nullify(next%nextPolar)
	endif
end subroutine

!> @brief removes a PolarLowListNode from a list, frees that node's memory
!> @param listRoot head of list
!> @param nodeToRemove
subroutine removePolarNodeFromList( listRoot, nodeToRemove )
	type(PolarLowListNode), pointer, intent(inout) :: listRoot, nodeToRemove
	type(PolarLowListNode), pointer :: foundNode, current, next, previous
	logical :: keepGoing
	
	nullify(current)
	nullify(next)
	nullify(foundNode)
	nullify(previous)
	keepGoing = .FALSE.
	if ( .NOT. associated(nodeToRemove) ) then
		print *, "removePolarNodeFromList ERROR : nodeToRemove not associated"
		return
	endif
	
	if ( .NOT. associated(listRoot) ) then
		print *, "removePolarNodeFromList ERROR : listRoot not associated"
		return
	endif
	
	if ( listRoot%listSize == 0 ) then
		!print *, "removePolarNodeFromList WARNING : list is empty "
		return
	elseif ( listRoot%listSize == 1 ) then
		!print *, "removePolarNodeFromList WARNING : list has only one element"
		listRoot%listSize = 0
	else
		current => listRoot
		keepGoing = .TRUE.
		do while ( associated(current) .AND. keepGoing )
			next => current%nextPolar
			if ( associated(current, nodeToRemove) ) then 
				foundNode => current
				keepGoing = .FALSE.
			else
				previous => current
				current => next
				next => current%nextPolar
			endif
		enddo
		
		if ( .NOT. associated(foundNode) ) then
			print *, "removePolarNodeFromList ERROR : nodeToRemove not found in list."
			return
		else
			if ( associated(foundNode, listRoot) ) then
				next => listRoot%nextPolar
				next%listSize = listRoot%listSize - 1
				listRoot => next
				deallocate(foundNode)
			else
				listRoot%listSize = listRoot%listSize - 1
				next => current%nextPolar
				previous%nextPolar => next
				deallocate(foundNode)
			endif
		endif
	endif
end subroutine

!> @brief Deletes and nullifies an entire list of PolarLowListNodes
!> @param list
recursive subroutine deletePolarList( list ) 
	type(PolarLowListNode), pointer, intent(inout) :: list
	type(PolarLowListNode), pointer :: next
	next => list%nextPolar
	deallocate(list)
	if ( associated(next)) call deletePolarList(next)
end subroutine

!> @brief Prints PolarLowListNode lists in output suitable for use by either @ref TrackDriver.f90 or TSTORMS trajectory program.
!> @todo rewrite this to output polar low data -- this will require rewriting the StormTrack module routines.  
!!
!> @param listRoot list to output to file
!> @param fileunit fileunit where data should be written
!> @param year
!> @param month
!> @param day
!> @param hour
subroutine PrintPolarOutputForTracker( listRoot, fileunit, year, month, day, hour )
	type(PolarLowListNode), pointer, intent(in) :: listRoot
	integer, intent(in) :: fileunit, year, month, day, hour
	type(PolarLowListNode), pointer :: current
	
	write(fileunit,*) day, month, year, listRoot%listSize, hour
	if ( listRoot%listSize == 0 ) then
		return
	else
		current => listRoot
		do while ( associated(current) )
			write(fileunit, *) current%lonIndex, current%latIndex, current%lon, current%lat, &
				current%wind, current%vort, current%psl, ' F  F ', current%airSeaTempDiff, 0.0
			
			current => current%nextPolar
		enddo
	endif
end subroutine

!> @}
end module