module StormListNodeModule
!> @file StormListNode.f90 
!! @brief Provides a linked-list data structure for spatial searches.
!> @author Peter Bosler SNL
!!

!> @defgroup StormListNode StormListNode
!! @brief Defines the linked-list data structure used by StrideSearch to keep track of found storms.
!! This type may be modified (or subclassed, with Fortran 2003) to carry different data for use with different 
!! meteorological applications.
!!
!! StormListNode procedures are divided into 2 categories: 
!!		Node procedures that work on one or two nodes directly, and list procedures that work with node pointers.
!! @{
implicit none

private
public StormListNode, initialize, deleteList, addNodeToList, removeNodeFromList
public PrintStormsInfo, PrintStormsToTable, PrintTSTORMSOutput, Copy
public SameLocation

real, public, parameter :: EARTH_RADIUS = 6371.220 !< Earth's mean radius (km)
real, public, parameter :: PI = 4.0*atan(1.0) !< pi
real, public, parameter :: RAD_2_DEG = 180.0/PI !< convert radians to degrees
real, public, parameter :: DEG_2_RAD = PI/180.0 !< convert degrees to radians
real, public, parameter :: ZERO_TOL = 1e-8

!> @class StormListNode
!> @brief Node data type for linked list. 
!> Provides a basic extensible class for tracking storms using linked lists.
!!
!> This type may be modified (or subclassed, with Fortran 2003) to carry different data for use with different 
!! meteorological applications.
type StormListNode
	real :: lon = 0.0 !< degrees east
	real :: lat = 0.0 !< degrees north
	integer :: lonIndex = 0 !< index of longitude in data file longitude vector
	integer :: latIndex = 0 !< index of latitude in data file latitude vector
	real :: vort = 0.0 !< cyclonic vorticity max (1/s)
	real :: psl = 0.0 !< sea level pressure min (hPa or Pa)
	real :: wind = 0.0 !< surface or 850 hPa wind speed max (m/s)
	integer :: listSize = 0
	logical :: removeThisNode = .FALSE.
	class(StormListNode), pointer :: next => null()
	
	contains
	
		procedure, public :: initialize 
end type

contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!	node functions (not pointers)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!> @brief Initializes a StormListNode.  This subroutine must be called prior to using a list.
!!
!> @param listRoot list to be initialized
!> @param lon longitude of new storm
!> @param lat latitude of new storm
!> @param lonIndex index of storm's longitude in the data file longitude vector
!> @param latIndex index of storm's latitude in the data file latitude vector
!> @param vort vorticity of storm
!> @param psl minimum sea level pressure of storm
!> @param wind maximum wind of storm
subroutine initialize( listRoot, lon, lat, lonIndex, latIndex, vort, psl, wind )
	class(StormListNode), intent(out) :: listRoot
	real, intent(in), optional :: lon, lat, vort, psl, wind
	integer, intent(in), optional :: lonIndex, latIndex
	
	if ( present(lon) .AND. present(lat) ) then
		listRoot%lon = lon
		listRoot%lat = lat
		listRoot%lonIndex = lonIndex
		listRoot%latIndex = latIndex
		listRoot%vort = vort
		listRoot%psl = psl
		listRoot%wind = wind
		listRoot%listSize = 1
	else	
		listRoot%listSize = 0
	endif
	listRoot%removeThisNode = .FALSE.
	listRoot%next => null()
end subroutine

!> @brief Copies a StormListNode
!!
!> @param dest destination of data to be copied
!> @param source source data to be copied
subroutine Copy( dest, source )
	class(StormListNode), intent(inout) :: dest
	class(StormListNode), intent(in) :: source
	dest%lon = source%lon
	dest%lat = source%lat
	dest%lonIndex = source%lonIndex
	dest%latIndex = source%latIndex
	dest%vort = source%vort
	dest%psl = source%psl
	dest%wind = source%wind
	dest%listSize = source%listSize
	dest%next => source%next
	dest%removeThisNode = source%removeThisNode
end subroutine

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!	list functions (pointers)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!> @brief Adds a node (that must be predefined) to an existing list
!!
!> @param listRoot list to which newNode should be added
!> @param newNode node to add to list
subroutine AddNodeToList( listRoot, newNode )
	type(StormListNode), intent(inout), pointer :: listRoot
	type(StormListNode), pointer, intent(in) :: newNode
	type(StormListNode), pointer :: current, next
	
	current => listRoot
	next => listRoot%next
	if ( listRoot%listSize == 0 ) then
		call Copy( listRoot, newNode )
		listRoot%listSize = 1	
	else
		do while ( associated(next) )
			current => next
			next => current%next
		end do
		listRoot%listSize = listRoot%listSize + 1
		allocate(next)
		call Copy( next, newNode )
		current%next => next
	endif
end subroutine

!> @brief Removes a node from a list, and deletes that node from memory.
!!
!> @param listRoot pointer to list
!> @param nodeToRemove pointer to node that will be deleted
subroutine removeNodeFromList( listRoot, nodeToRemove )
	type(StormListNode), pointer, intent(inout) :: listRoot 
	type(StormListNode), pointer, intent(inout) :: nodeToRemove 
	type(StormListNode), pointer :: foundNode, current, next, previous
	logical :: keepGoing
	
	nullify(current)
	nullify(foundNode)
	nullify(next)
	nullify(previous)
!	print *, "entering removeStormFromList"
	
	if ( .NOT. associated(nodeToRemove) ) then
		print *, "removeNodeFromList ERROR : nodeToRemove not associated"
		return
	endif
	
	if ( .NOT. associated(listRoot) ) then
		print *, "removeNodeFromList ERROR : listRoot not associated"
		return
	endif
	
	if ( listRoot%listSize == 0 ) then
		!print *, "removeNodeFromList WARNING : list is empty "
		return
	elseif ( listRoot%listSize == 1 ) then
		!print *, "removeNodeFromList WARNING : list has only one element"
		listRoot%listSize = 0
	else
		current => listRoot
		keepGoing = .TRUE.
		do while ( associated(current) .AND. keepGoing )
			next => current%next
			if ( associated(current, nodeToRemove) ) then
				foundNode => current
				keepGoing = .FALSE.
			else
				previous => current
				current => next
				next => current%next
			endif
		enddo
		
		if ( .NOT. associated(foundNode) ) then
			print *, "removeStormFromList ERROR : nodeToRemove not found."
			return
		else
			if ( associated(foundNode, listRoot ) ) then
				next => listRoot%next
				next%listSize = listRoot%listSize - 1
				listRoot => next
				deallocate(foundNode)
			else
				listRoot%listSize = listRoot%listSize - 1
				next => current%next
				previous%next => next
				deallocate(foundNode)
			endif
		endif
	endif
!	print *, "exiting removeNodeFromList"
end subroutine

!> @brief Deletes an entire list from memory.
!!
!> @param list to be deleted
recursive subroutine deleteList( list )
	type(StormListNode), pointer, intent(inout) :: list
	type(StormListNode), pointer :: next
	next => list%next
	deallocate(list)
	if ( associated(next) ) call deleteList( next)
end subroutine

!> @brief Prints storm info to a fileunit in a human-readable format.
!!
!> @param listRoot list containing storms
!> @param fileunit output fileunit
subroutine PrintStormsInfo( listRoot, fileunit )
	type(StormListNode), pointer, intent(in) :: listRoot
	integer, intent(in) :: fileunit
	type(StormListNode), pointer :: current
	integer :: k
	
	if ( listRoot%listSize == 0 ) then
		print *, "PrintStormsInfo WARNING : list is empty."
		return
	endif
	
	write(fileunit,'(8A24)') '#','lon', 'lat', 'lonIndex', 'latIndex', 'psl', 'vort', 'wind'
	current => listRoot
	k = 0
	do while ( associated(current) )
		k = k + 1
		write(fileunit,'(I24, 2F24.3, 2I24, F24.3, F24.12, F24.3)') k, current%lon, current%lat, &
			current%lonIndex, current%latIndex, current%psl/100.0, current%vort, current%wind
		current => current%next
	enddo
end subroutine

!> @brief Prints storm info to a table suitable for reading with NCL's readAsciiTable command.
!!
!> @param listRoot list containing storms
!> @param fileunit output fileunit
subroutine PrintStormsToTable( listRoot, fileunit )
	type(StormListNode), pointer, intent(in) :: listRoot
	integer, intent(in) :: fileunit
	type(StormListNode), pointer :: current
	
	if ( listRoot%listSize == 0 ) then
		print *, "PrintStormsInfo WARNING : list is empty."
		return
	endif
	
	!
	! helpful NCL info
	!
	write(6,'(A)') "For use with NCL : "
	write(6,'(A,I4)') ".... nRow = ", listRoot%listSize
	write(6,'(A,I4)') ".... nCol = ", 5
	
	current => listRoot
	do while ( associated(current) )
		write(fileunit,'(3(F6.2,8X),F15.12,6X,F6.2)') current%lon, current%lat, current%psl/100.0, current%vort, current%wind
		current => current%next
	enddo
end subroutine

!> @brief Prints storm info in the same format as TSTORMS uses.  Does not include warm core or thickness data.
!! Enables use of the TSTORMS trajectory program.
!!
!> @param listRoot list containing storms
!> @param fileunit output fileunit
!> @param year
!> @param month
!> @param day
!> @param hour
subroutine PrintTSTORMSOutput( listRoot, fileunit, year, month, day, hour )
	type(StormListNode), pointer, intent(in) :: listRoot
	integer, intent(in) :: fileunit, year, month, day, hour
	type(StormListNode), pointer :: current
			
	!
	! helpful tstorms formatting info
	!
!	write(6,'(A)') "*** TSTORMS Output **** "
!	write(6,'(A)') '* First row = day, month, year, number of storms, hour'
!	write(6,'(A)') '* each row >= 2 represents 1 storm:'
!	write(6,'(A,A)') '* storm entry = lonIndex, latIndex, longitude, latitude, surface wind max, surface vorticity max,', &
!					 ' surface pressure min, warmCore T/F, thickness T/F'	
	write(fileunit, *) day, month, year, listRoot%listSize, hour				 
	if ( listRoot%listSize == 0 ) then
		print *, "PrintTSTORMSOutput WARNING : list is empty."
		return
	endif
	current => listRoot
	do while ( associated(current) )
		write(fileunit,*) current%lonIndex, current%latIndex, current%lon, current%lat, &
								 current%wind, current%vort, current%psl, &
								 ' T ', ' T ', 0.0, 0.0 
		current=>current%next
	enddo					 
end subroutine

pure function SameLocation( node1, node2 )
	logical :: SameLocation
	class(StormListNode), intent(in) :: node1, node2
	SameLocation = (( node1%lonIndex == node2%lonIndex ) .AND. ( node1%latIndex == node2%latIndex) )
end function

!> @}
end module