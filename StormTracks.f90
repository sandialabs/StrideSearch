module TrackModule
!> @file StormTracks.f90 
!! @brief Provides a container class for holding storm track information, and methods for constructing tracks and 
!! applying temporal storm identification criteria.
!> @author Peter Bosler SNL
!!

!> @defgroup TrackListNode TrackListNode
!! @brief Linked-list data structure for storm tracks.
!!
!! @{
use StormListNodeModule, only : DEG_2_RAD, SameLocation
use StrideSearchModule, only : SphereDistance
use TropicalStrideSearchModule, only : GetLandMask
use StormTrackDataModule

implicit none
private
public TrackListNode
public GenerateStormTracks, PrintTrajectories, DeleteStormTracks, MarkStormsOverLand

integer, parameter :: maxPointsPerTrack = 200 ! 200 6-hourly timesteps = 50 days

!> @class TrackListNode
!> @brief Linked-list node for storm track.
!> Statically allocates memory for each track, so no track can be longer than maxPointsPerTrack timesteps.
type TrackListNode
	real, dimension(maxPointsPerTrack) :: trackLons = 0.0 !< longitudes of a storm along its track
	real, dimension(maxPointsPerTrack) :: trackLats = 0.0 !< latitudes of a storm along its track
	real, dimension(maxPointsPerTrack) :: trackWind = 0.0 !< max windspeed of a storm along its track
	real, dimension(maxPointsPerTrack) :: trackPsl = 0.0 !< min sea level pressure of a storm along its track
	real, dimension(maxPointsPerTrack) :: trackVort = 0.0 !< max cyclonic vorticity of a storm along its track
	integer, dimension(4, maxPointsPerTrack) :: trackDate = 0 !< date along a storm track
	real :: maxWind = 0.0	!< maximum wind speed achieved by a storm over its lifetime
	real :: minPsl = 0.0 !< minimum pressure over a storm's lifetime
	real :: maxVort = 0.0 !< maximum cyclonic vorticity over a storm's lifetime
	integer, dimension(4) :: startDate = 0 !< storm's starting date
	integer, dimension(4) :: endDate = 0 !< storm's ending date
	integer :: startTimeIndex = 0 !< time index in netcdf file corresponding to startDate
	integer :: trackLength = 0 !< number of points in storm track (@f$ \le @f$ maxPointsPerTrack)
	integer :: nTracks = 0	!< total number of tracks in this list
	logical :: originOverWater = .True. !< true if the first point in a storm track is not over land
	type(TrackListNode), pointer :: nextTrack => null()
	contains
		procedure, public :: initializeTrackNode
end type

contains

!> @brief Prints storm tracks to an output file
!> @param trackList linked-list of storm tracks
!> @param fileunit integer unit of output file
subroutine PrintTrajectories( trackList, fileunit )
	type(TrackListNode), pointer, intent(in) :: trackList
	integer, intent(in) :: fileUnit
	!
	type(TrackListNode), pointer :: current
	integer :: i
	
	print *, "writing ", trackList%nTracks, " tracks to file."
	
	current => trackList
	do while ( associated( current ) )
		if ( current%originOverWater .AND. current%trackLength > 0) then		

			write(fileunit, *) "start ", current%trackLength, current%startDate
		
			do i = 1, current%trackLength
				write(fileunit, *) current%trackLons(i), current%trackLats(i), current%trackWind(i), current%trackPsl(i)*0.01, &
								   current%trackDate(:,i)
			enddo
		endif
		current => current%nextTrack
	enddo
end subroutine

!> @brief Builds storm tracks from output of a spatial search driver program.
!> @param trackList list to be constructed. On output, contains storm tracks.
!> @param trackData Container output from stormtrackdatamodule::readstormtrackdatafile.
!> @param minimumDuration tracks that are not at least this many of timesteps long will be ignored.
!> @param maxTravelSpeed maximum storm travel speed in m/s (not maximum wind speed)
!> @param hoursPerTimestep integer number of hours for each timestep
subroutine GenerateStormTracks( trackList, trackData, minimumDuration, maxTravelSpeed, hoursPerTimestep )
	type(TrackListNode), pointer, intent(inout) :: trackList
	type(StormTrackData), intent(inout) :: trackData
	integer, intent(in) :: minimumDuration
	real, intent(in) :: maxTravelSpeed
	integer, intent(in) :: hoursPerTimestep
	!
	type(datedStormListNode), pointer :: possTrack, candidateList, current, tempDatedNode, query, currentLoc
	integer :: i, startTimeIndex, timeIndex, j
	logical :: keepGoing
	real :: maxDistPerTimestep, testDist
	type(TrackListNode), pointer :: tempTrackPtr
	
	nullify(possTrack)
	nullify(candidateList)
	nullify(tempTrackPtr)
	nullify(tempDatedNode)
		
	maxDistPerTimestep = maxTravelSpeed * 3.6 ! convert m/s to kph
	maxDistPerTimestep = maxDistPerTimestep * hoursPerTimestep ! convert kph to km per timestep
	
	do i = 1, trackData%nTimesteps - 1
		
		current => trackData%stormLists(i)
!		print *, i, ", listSize = ", current%listSize
		j = 0
		if ( current%listSize > 0 ) then
			do while ( associated(current) )
				!
				!	search storms at this timestep for possible track origins
				!
				j = j + 1
				if ( .NOT. current%removeThisNode ) then 
					!
					! possible storm track origin
					!
					allocate(possTrack)
					startTimeIndex = i
					timeIndex = startTimeIndex
					call initializeDatedNode( possTrack, current%lon, current%lat, current%lonIndex, current%latIndex, &
														 current%vort, current%psl, current%wind, current%date )
					!
					!  build possible track
					!
					keepGoing = .TRUE.
					currentLoc => possTrack
					!print *, "possible track initialized"
					do while ( keepGoing )
						!
						! search subsequent timestep for successor candidates
						!
						allocate(candidateList)
						timeIndex = timeIndex + 1
					
						if ( timeIndex > trackData%nTimesteps ) then
							!
							! end of data
							!
							keepGoing = .FALSE.
							!print *, "end of data."
							exit
						endif
					
						query => trackData%stormLists(timeIndex)
						do while ( associated(query) )
							testDist = SphereDistance( currentLoc%lon * DEG_2_RAD, currentLoc%lat * DEG_2_RAD, &
												 query%lon * DEG_2_RAD, query%lat * DEG_2_RAD )
							if (  testDist < maxDistPerTimestep .AND. (.NOT. query%removeThisNode ) ) then
								!
								!	candidate found
								!
								allocate(tempDatedNode)				 
								call initializeDatedNode( tempDatedNode, query%lon, query%lat, query%lonIndex, query%latIndex, &
																		 query%vort, query%psl, query%wind, query%date)
								call AddDatedNodeToList( candidateList, tempDatedNode )
								deallocate(tempDatedNode)
							endif
							query => query%nextDatedNode
						enddo
					
						if ( candidateList%listSize == 0 ) then 
							!
							! no successors found, end track
							!
							keepGoing = .FALSE.
							!print *, "no candidates found"
						elseif ( candidateList%listSize == 1 ) then 
							!
							! one successor found, add to track
							!
							!print *, "one candidate found"
							!print *, "PossTrack Info : "
							!call PrintTrackLocations(possTrack)
							!print *, "candidateList Info : "
							!call PrintTrackLocations(candidateList)
							call AddDatedNodeToList( possTrack, candidateList )
							call UpdateCurrentLocation( currentLoc, possTrack)
						else
							! 
							! multiple possible successors found; choose closest one, add to track
							!
							!print *, "multiple candidates found"
							call FindClosestCandidate( tempDatedNode, currentLoc, candidateList )
							call AddDatedNodeToList( possTrack, tempDatedNode )
							call UpdateCurrentLocation( currentLoc, possTrack)
						endif
						call deleteDatedList(candidateList)
					enddo
			
					!
					! check temporal criteria along possible track
					!
					if ( possTrack%listSize >= minimumDuration ) then
						!print *, "adding track"
						!
						!	storm track found: record to track list, remove entries from data to prevent duplication
						!
						call MarkTrackNodesForRemoval( trackData, possTrack, startTimeIndex )
						!print *, "used nodes marked for removal"
						!print *, "calling initialize for track with ", possTrack%listSize, " entries."
						allocate(tempTrackPtr)
						call initializeTrackNode( tempTrackPtr, possTrack, startTimeIndex )
						call AddTrackNodeToList( trackList, tempTrackPtr )
						deallocate(tempTrackPtr)
					endif
			
					call deleteDatedList(possTrack)
				endif
	!			write(6,'(I4,A)',advance='no') j, "   "
				current => current%nextDatedNode
			enddo
		endif
	enddo	
end subroutine

!> @brief Prints basic storm track info to console
!> @param track Track list
subroutine PrintTrackLocations( track ) 
	type(datedStormListNode), pointer, intent(in) :: track
	type(datedStormListNode), pointer :: current
	current => track
	do while ( associated(current) )
		print *, current%lon, current%lat, current%wind, current%psl, current%date
		current => current%nextDatedNode
	enddo
end subroutine

!> @brief Updates a storm's location within a track to the previously searched timestep.
!> @param currLoc
!> @param track
subroutine UpdateCurrentLocation( currLoc, track )
	type(datedStormListNode), pointer, intent(inout) :: currLoc
	type(datedStormListNode), pointer, intent(in) :: track
	type(datedStormListNode), pointer :: current
	!print *, "updating current location"
	current => track
	do while ( associated(current%nextDatedNode) )
		current => current%nextDatedNode
	enddo
	currLoc => current
end subroutine

!> @brief When storm tracks get close together, a storm may have more than one possible candidate location at the next timestep.
!> This subroutine eliminates all but the closest one to a storm's current location.
!> @param closestCandidate (output)
!> @param currentLoc location of storm at current time step
!> @param candidateList list of possible storm locations at next time step
subroutine FindClosestCandidate( closestCandidate, currentLoc, candidateList )
	type(datedStormListNode), pointer, intent(inout) :: closestCandidate
	type(datedStormListNode), pointer, intent(in) :: currentLoc, candidateList
	!
	real :: minDist, testDist
	type(datedStormListNode), pointer :: query
	!print *,"finding Closest Candidate"
	query => candidateList
	minDist = 1.0e20
	do while ( associated(query) )
		testDist = SphereDistance( currentLoc%lon * DEG_2_RAD, currentLoc%lat * DEG_2_RAD, &
							 query%lon * DEG_2_RAD, query%lat * DEG_2_RAD )
		if ( testDist < minDist ) then
			minDist = testDist
			closestCandidate => query
    	endif
		query => query%nextDatedNode
	enddo
end subroutine

!> @brief Once a datedStormListNode has been used to construct a storm track, it should not be used for other storms' 
!> possible tracks. 
!> This subroutine marks used nodes to prevent duplicate uses of the same storm.
!> @param trackData
!> @param trackToRemove new track to be added to track list, so it should be deleted from trackData
!> @param startTimeIndex starting time index of trackToRemove
subroutine MarkTrackNodesForRemoval( trackData, trackToRemove, startTimeIndex )
	type(StormTrackData), intent(inout) :: trackData
	type(datedStormListNode), pointer, intent(in) :: trackToRemove
	integer, intent(in) :: startTimeIndex
	!
	type(datedStormListNode), pointer :: query, querynext, trackDataPtr, nextTrackDataPtr
	integer :: i
	!print *, "Marking used nodes for removal..."
	query => trackToRemove
	i = startTimeIndex
	do while ( associated(query) )
		querynext => query%nextDatedNode
		trackDataPtr => trackData%stormLists(i)
		do while ( associated(trackDataPtr) )
			nextTrackDataPtr => trackDataPtr%nextDatedNode
			if ( SameLocation(query, trackDataPtr ) ) then
				trackDataPtr%removeThisNode = .TRUE.
				i = i + 1
				exit
			endif
			trackDataPtr => nextTrackDataPtr
		enddo
		query => querynext
	enddo
end subroutine

!> @brief Initializes a TrackListNode.
!> @param trackList list to be initialized
!> @param stormTrack linked list of datedStormListNode objects that defines a storm track
!> @param startIndex starting time index in netCDF file associated with stormTrack's origin
subroutine initializeTrackNode( trackList, stormtrack, startIndex )
	class(TrackListNode), intent(inout) :: trackList
	type(datedStormListNode), pointer, intent(in) :: stormtrack
	integer, intent(in) :: startIndex
	!
	type(datedStormListNode), pointer :: current
	integer :: i

	!print *, "initializing a new track..."
	if ( stormTrack%listSize > 0 .AND. stormTrack%listSize <= maxPointsPerTrack ) then
		trackList%trackLength = stormTrack%listSize
		trackList%startTimeIndex = startIndex

		i = 0
		current => stormtrack
		do while ( associated(current) )
			i = i + 1
			trackList%trackLons(i) = current%lon
			trackList%trackLats(i) = current%lat
			trackList%trackWind(i) = current%wind
			trackList%trackPsl(i) = current%psl
			trackList%trackVort(i) = current%vort
			trackList%trackDate(:,i) = current%date
			if ( i == 1 ) then
				trackList%startDate = current%date
			elseif ( i == trackList%trackLength) then
				trackList%endDate = current%date
			endif
			current => current%nextDatedNode
		enddo
		trackList%maxWind = maxval(trackList%trackWind)
		trackList%maxVort = maxval(trackList%trackVort)
		trackList%minPSl = minval(trackList%trackPsl)
		trackList%nTracks = 1
		trackList%originOverWater = .TRUE.
		nullify(trackList%nextTrack)
	endif
end subroutine

!> @brief Copies a TrackListNode object
!> @param dest (output) destination node
!> @param source source node
subroutine CopyTrack( dest, source )
	type(TrackListNode), intent(out) :: dest
	type(TrackListNode), intent(in) :: source
	dest%trackLons = source%trackLons
	dest%trackLats = source%trackLats
	dest%trackWind = source%trackWind
	dest%trackPsl = source%trackPsl
	dest%trackVort = source%trackVort
	dest%trackDate = source%trackDate
	dest%maxWind = source%maxWind
	dest%minPsl = source%minPSl
	dest%maxVort = source%maxVort
	dest%startDate = source%startDate
	dest%endDate = source%endDate
	dest%startTimeIndex = source%startTimeIndex
	dest%nTracks = source%nTracks
	dest%trackLength = source%trackLength
	dest%originOverWater = source%originOverWater
	dest%nextTrack => source%nextTrack
end subroutine

!> @brief Adds an intialized TrackListNode to a linked list.
!> @param trackList list root
!> @param newNode node to add to list 
subroutine AddTrackNodeToList( trackList, newNode )
	type(TrackListNode), pointer, intent(inout) :: trackList
	type(TrackListNode), pointer, intent(in) :: newNode
	type(TrackListNode), pointer :: current, next
	
	current => trackList
	if ( current%nTracks == 0 ) then
		call CopyTrack( trackList, newNode )
		trackList%nTracks = 1
		nullify(trackList%nextTrack)
	else
		next => current%nextTrack
		do while ( associated(next) )
			current => next
			next => current%nextTrack
		enddo
		
		allocate(next)
		call CopyTrack(next, newNode)
		trackList%nTracks = trackList%nTracks + 1
		current%nextTrack => next
		nullify(next%nextTrack)
	endif
end subroutine

!> @brief Frees memory used by a list of TrackListNode objects.
!> @param trackList list root
recursive subroutine DeleteStormTracks( trackList )
	type(TrackListNode), pointer, intent(inout) :: trackList
	type(TrackListNode), pointer :: next
	next => trackList%nextTrack
	deallocate(trackList)
	if ( associated(next) ) call DeleteStormTracks( next )
end subroutine

!> @brief Marks storms whose origins are over land.
!> This subroutine will not remove tracks that originate over water and then move over land.
!> @param trackList
subroutine MarkStormsOverLand( trackList ) 
	type(TrackListNode), pointer, intent(inout) :: trackList
	type(TrackListNode), pointer :: current
	integer, dimension(360,180) :: mask
	real :: dLon, dLat, lon0, lat0, origLon, origLat
	integer :: i, j, counter
	mask = GetLandMask()

	lon0 = 0.0
	dLon = 1.0
	
	lat0 = -89.5	
	dLat = 1.0
	
	current => trackList
	counter = 0
	do while ( associated(current) ) 
		origLon = current%trackLons(1)
		origLat = current%trackLats(1)
		j = ( origLat - lat0 ) + 1.5
		i = ( origLon - lon0 ) + 1.5
		if ( i == 0 ) i = 360
		if ( i > 360 ) i = i - 360
		if ( mask(i,j) /= 0 ) then 
			current%originOverWater = .FALSE.
			counter = counter + 1
		endif
		current => current%nextTrack
	enddo
	
	print *, counter, " of ", trackList%nTracks, " storms originate over land."
		
end subroutine

!> @brief Given a storm's maximum wind speed, returns the hurricane category.
!> @param maxWind
!> @return hurricaneCat, according to the Saffir-Simpson scale.
pure function hurricaneCat( maxWind )
	integer :: hurricaneCat
	real, intent(in) :: maxWind
	if ( maxWind < 33.0 ) then
		hurricaneCat = 0
	elseif ( maxWind >= 33.0 .and. maxWind < 43.0 ) then
		hurricaneCat = 1
	elseif ( maxWind >= 43.0 .and. maxWind < 50.0 ) then
		hurricaneCat = 2
	elseif ( maxWind >= 50.0 .and. maxWind < 58.0 ) then
		hurricaneCat = 3
	elseif ( maxWind >= 58.0 .and. maxWind < 70.0 ) then
		hurricaneCat = 4
	elseif ( maxWind >= 70.0 ) then
		hurricaneCat = 5
	endif
end function 

!> @}
end module