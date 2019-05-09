program StormListTester
!> @file StormListTester.f90 
!> @brief Unit test for the StormListNode module.  
!! Demonstrates and tests the use of the basic linked-list data structure
!!
!> @author Peter Bosler SNL
!!
!! Linked-lists work through pointers, so the fundamental object in this file is type(StormListNode), pointer :: stormList.
!! This points to the list root, as soon as it is allocated and initialized.
!! From that point on, all access to the list begins with this pointer.
!!
!! Another pointer, tempNode, is used to hold only one storm.  
!! This pointer never grows a list of its own; it is only used to temporarily hold data that will be added to stormList.
use StormListNodeModule

implicit none

type(StormListNode), pointer :: stormList !< listRoot
type(StormListNode), pointer :: tempNode  !< temporary pointers used to hold data on one storm
type(StormListNode), pointer :: current !<  pointer used to traverse stormList
integer, parameter :: nSTorms = 14
integer, parameter :: stdOut = 6
integer, parameter :: fileOut = 12

real :: lons(nStorms), lats(nStorms), vort(nStorms), psl(nStorms), wind(nStorms)
integer :: lonJ(nStorms), latI(nStorms)

integer :: i, j
real :: r

!
! set up some fake storm data
!
call random_seed()
do i = 1, nStorms
	call random_number(r)
	lons(i) = 360.0*r
	lats(i) = -90.0 + 180.0*r
	vort(i) = -1.0e-4 + 2.0e-4*r
	psl(i) = 95000.0 + 10000.0*r
	wind(i) = 40.0*r
enddo

write(stdOut,'(A)') "Fake storm data ready."
write(stdOut,'(A)', advance='NO') " lons = ["
do i = 1, nStorms-1
	write(stdOut,'(F6.2,A)',advance='NO') lons(i), ",    "
enddo
write(stdOut,'(F6.2,A)', advance='YES') lons(nStorms), "]; "

write(stdOut,'(A)', advance='NO') " lats = ["
do i = 1, nStorms-1
	write(stdOut,'(F6.2,A)',advance='NO') lats(i), ",    "
enddo
write(stdOut,'(F6.2,A)', advance='YES') lats(nStorms), "]; "

!
! initialize the list with the first storm
!
allocate(stormList)
i = 1
call stormList%initialize( lons(1), lats(1), i, i, vort(1), psl(1), wind(1))
write(6,'(A)') "StormList initialized."

!
! Add the rest of the storms to the list
!
allocate(tempNode)
do i = 2, nStorms
	call initialize(tempNode, lons(i), lats(i), i, i, vort(i), psl(i), wind(i) )
	call AddNodeToList( stormList, tempNode)
enddo

write(6,'(A)') "StormList ready."

!
! print the list
!
call PrintStormsInfo( stormList, stdOut)

!
! remove storm 5
!
current => stormList
do i = 1, 4
	current => current%next
enddo
call removeNodeFromList( stormList, current)

write(6,'(A)') "removed node 5."

!
! print the list
!
call PrintStormsInfo( stormLIst, stdOut)

!
! remove storm 1
!
call removeNodeFromList( stormList, stormList)
write(6,'(A)') "removed node 1."

!
! print the list
!
call PrintStormsInfo( stormLIst, stdOut)


deallocate(tempNode)
call deleteList(stormList)

write(6,'(A)') "PROGRAM COMPLETE"

end program