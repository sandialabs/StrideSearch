module TropicalStormListNodeModule
!> @file TropicalStormListNode.f90 
!! @brief Extends StormListNode to tropical cyclone detection applications.
!> @author Peter Bosler SNL
!!

!> @defgroup TropicalStormListNode TropicalStormListNode
!! @brief Extends the @ref StormListNode type for tropical cyclone detections
!! @extends StormListNode
!!
!!	Like its parent @ref StormListNode, TropicalStormListNode procedures are divided into 2 categories: 
!!		Node procedures that work on one or two nodes directly, and list procedures that work with node pointers.
!!
!! @{
use StormListNodeModule

implicit none

private
public TropicalStormListNode, initializeTropical, addTropicalNodeToList, removeTropicalNodeFromList, deleteTropicalList
public PrintTropicalTSTORMSOutput

!> @class TropicalStormListNode
!> @brief Linked list node for tropical storm detection. 
type, extends(StormListNode) :: TropicalStormListNode
	real :: vortLon = 0.0	!< Longitude of vorticity max
	real :: vortLat = 0.0	!< Latitude of vorticity max
	real :: vertAvgT = 0.0	!< Max of 200 hPa and 500 hPa average - sector avg of 200-500 hPa vertical average
	real :: tempLon = 0.0	!< Location of vert. avg. temp maximum
	real :: tempLat = 0.0	!< Location ov vert. avg. temp maximum
	logical :: hasWarmCore = .FALSE. !< True if temperature excess criterion is met __and__ temperature/psl collocation criterion is met
	real :: thickness = 10000.0000 !< 1000 hPa to 200 hPa geopotential thickness
	real :: thickLon = 0.0	!< location of thickness maximum
	real :: thickLat = 0.0	!< location of thickness maximum
	logical :: hasThickness = .FALSE.
	type(TropicalStormListNode), pointer :: nextTropical => null()
	
	contains
		procedure, public :: initializeTropical
end type

contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!	node functions (not pointers)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!> @brief Initializes a TropicalStormListNode.
!> @param listRoot First member of a linked list of TropicalStormListNode objects.
!> @param lon longitude of new storm
!> @param lat latitude of new storm 
!> @param lonIndex index of storm's longitude in the data file longitude vector
!> @param latIndex index of storm's latitude in the data file latitude vector
!> @param pslMin minimum sea level pressure of storm
!> @param vortMax vorticity of storm
!> @param windMax maximum wind of storm
!> @param vortLon longitude of vorticity max
!> @param vortLat latitude of vorticity max
!> @param vertAvgT vertically averaged temperature, or vertically averaged temperatue excess
!> @param tempLon longitude of maximum vertically averaged temperature
!> @param tempLat latitude of maximum vertically averaged temperature
!> @param hasWarmCore true if temperature excess and temperature collocation criteria are met
!> @param thickness difference between 200 hPa and 1000 hPa geopotential heights
!> @param thickLon longitude of maximum thickness
!> @param thickLat latitude of maximum thickness
!> @param hasThickness true if thickness maximum exceeds a threshold and thickness/psl collocation criterion is met
subroutine initializeTropical( listRoot, lon, lat, lonIndex, latIndex, pslMin, vortMax, windMax, vortLon, vortLat, &
						vertAvgT, tempLon, tempLat, hasWarmCore, thickness, thickLon, thickLat, hasThickness )
	class(TropicalStormListNode), intent(inout) :: listRoot
	real, intent(in) :: lon, lat
	integer, intent(in) :: lonIndex, latIndex
	real, intent(in) :: pslMin, vortMax, windMax, vortLon, vortLat
	real, intent(in) :: vertAvgT, tempLon, tempLat
	logical, intent(in) :: hasWarmCore
	real, intent(in) :: thickness, thickLon, thickLat
	logical, intent(in) :: hasThickness
	
	call initialize(listRoot%StormListNode, lon, lat, lonIndex, latIndex, vortMax, pslMin, windMax)
	
	listRoot%vortLon = vortLon
	listRoot%vortLat = vortLat
	listRoot%vertAvgT = vertAvgT
	listRoot%tempLon = tempLon
	listRoot%tempLat = tempLat
	listRoot%hasWarmCore = hasWarmCore
	listRoot%thickness = thickness
	listRoot%thickLon = thickLon
	listRoot%thickLat = thickLat
	listRoot%hasThickness = hasThickness
	
	listRoot%nextTropical => null()
end subroutine

!> @brief Copies a TropicalStormListNode
!> @param dest destination node
!> @param source source node
subroutine CopyTropical( dest, source )
	class(TropicalStormListNode), intent(inout) :: dest
	class(TropicalStormListNode), intent(in) :: source
	
	call Copy( dest%StormListNode, source%stormListNode)
	dest%vortLon = source%vortLon
	dest%vortLat = source%vortLat
	dest%vertAvgT = source%vertAvgT
	dest%tempLon = source%tempLon
	dest%tempLat = source%tempLat
	dest%hasWarmCore = source%hasWarmCore
	dest%thickness = source%thickness
	dest%thickLon = source%thickLon
	dest%thickLat = source%thickLat
	dest%hasThickness = source%hasThickness
	dest%nextTropical => source%nextTropical
end subroutine

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!	list functions (pointers)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!> @brief Adds a node to a linked list.  
!> The new node must be initialized with initializeTropical subroutine
!> @param listRoot first element of list, where new node will be added
!> @param newNode new TropicalStormListNode to be added
subroutine AddTropicalNodeToList( listRoot, newNode )
	type(TropicalStormListNode), pointer, intent(inout) :: listRoot
	type(TropicalStormListNode), pointer, intent(in) :: newNode
	type(TropicalStormListNode), pointer :: current, next
	
	current => listRoot
	next => listRoot%nextTropical
	if ( listRoot%listSize == 0 ) then
		call CopyTropical( listRoot, newNode)
		listRoot%listSize = 1
	else
		do while ( associated(next) )
			current => next
			next => current%nextTropical
		enddo
		listRoot%listSize = listRoot%listSize + 1
		allocate(next)
		call CopyTropical( next, newNode)
		current%nextTropical => next
		nullify(next%nextTropical)
	endif
end subroutine

!> @brief Removes a node from a list, and deletes that node from memory.
!!
!> @param listRoot pointer to list
!> @param nodeToRemove pointer to node that will be deleted
subroutine removeTropicalNodeFromList( listRoot, nodeToRemove )
	type(TropicalStormListNode), pointer, intent(inout) :: listRoot, nodeToRemove
	type(TropicalStormListNode), pointer :: foundNode, current, next, previous
	logical :: keepGoing
	
	nullify(current)
	nullify(foundNode)
	nullify(next)
	nullify(previous)
	if ( .NOT. associated(nodeToRemove) ) then
		print *, "removeNodeTropicalFromList ERROR : nodeToRemove not associated"
		return
	endif
	
	if ( .NOT. associated(listRoot) ) then
		print *, "removeTropicalNodeFromList ERROR : listRoot not associated"
		return
	endif
	
	if ( listRoot%listSize == 0 ) then
		!print *, "removeTropicalNodeFromList WARNING : list is empty "
		return
	elseif ( listRoot%listSize == 1 ) then
		!print *, "removeTropicalNodeFromList WARNING : list has only one element"
		listRoot%listSize = 0
	else
		current => listRoot
		keepGoing = .TRUE.
		do while ( associated(current) .AND. keepGoing )
			next => current%nextTropical
			if ( associated(current, nodeToRemove) ) then
				foundNode => current
				keepGoing = .FALSE.
			else
				previous => current
				current => next
				next => current%nextTropical
			endif
		enddo
		
		if ( .NOT. associated(foundNode) ) then
			print *, "removeTropicalNodeFromList ERROR : nodeToRemove not found in list."
			return
		else
			if ( associated(foundNode, listRoot) ) then
				next => listRoot%nextTropical
				next%listSize = listRoot%listSize - 1
				listRoot => next
				deallocate(foundNode)
			else
				listRoot%listSize = listRoot%listSize - 1 
				next => current%nextTropical
				previous%nextTropical => next
				deallocate(foundNode)
			endif
		endif
	endif
end subroutine

!> @brief Deletes an entire list from memory.
!!
!> @param list to be deleted
recursive subroutine deleteTropicalList( list )
	type(TropicalStormListNode), pointer, intent(inout) :: list
	type(TropicalStormListNode), pointer :: next
	next => list%nextTropical
	deallocate(list)
	if ( associated(next)) call deleteTropicalList(next)
end subroutine

!> @brief Prints storm info in the same format as TSTORMS uses.  
!! Enables use of the TSTORMS trajectory program.
!!
!> @param listRoot list containing storms
!> @param fileunit output fileunit
!> @param year
!> @param month
!> @param day
!> @param hour
subroutine PrintTropicalTSTORMSOutput( listRoot, fileunit, year, month, day, hour )
	type(TropicalStormListNode), pointer, intent(in) :: listRoot
	integer, intent(in) :: fileunit, year, month, day, hour
	type(TropicalStormListNode), pointer :: current
	
	write(fileunit, *) day, month, year, listRoot%listSize, hour
	if ( listRoot%listSize == 0 ) return
	current => listRoot
	do while ( associated(current) )
		write(fileunit, *) current%lonIndex, current%latIndex, current%lon, current%lat, &
						   current%wind, current%vort, current%psl, &
						   current%hasWarmCore, current%hasThickness, current%vertAvgT, current%thickness
		current => current%nextTropical
	enddo
end subroutine

!> @}
end module