module StrideSearchModule
!> @file StrideSearch.f90 
!! @brief Defines a data type for StrideSearch and implements the Stride Search algorithm for spatial searches of climate data sets.
!> @author Peter Bosler SNL
!!

!> @defgroup StrideSearch StrideSearch
!! @brief Implements the Stride Search algorithm for spatial data search
!!
!! @{
use StrideSearchDataModule
use StormListNodeModule

implicit none
private
public StrideSearchSector, SearchSetup, DoStrideSearch, FinalizeSearch, PrintSearchInfo
public MarkNodesForRemoval, RemoveMarkedNodes, ApplyLandMask, SphereDistance

!	
!	sector definition variables
real, allocatable, public :: sectorCenterLats(:)
real, allocatable, public :: sectorCenterLons(:)
integer, allocatable, public :: nLonsPerLatLine(:)
real, public :: latStrideReal
real, public, allocatable :: lonStrideReals(:)

!	
!	sector-to-data linking variables
integer, allocatable, public :: lonStrideInts(:)
integer, public :: latMinIndex, latMaxIndex, latStrideInt


!> @class StrideSearchSector
!> @brief Container for small sets of working data (enough to contain the largest search sector), used to identify storms.
!> Provides a workspace for spatial search.
type StrideSearchSector
	real :: centerLon = 0.0
	real :: centerLat = 0.0
	integer :: centerLatIndex = 0
	integer :: centerLonIndex = 0
	logical, allocatable, dimension(:,:) :: neighborhood  !< neighborhood(j,i) = true if grid point at (lon(j),lat(i)) is 
													  !< within a sectorRadius of the sector center
	real, allocatable, dimension(:,:) :: pslWork, & !< array for sector's sea level pressure data
									 vortWork, & !< array for sector's vorticity data
									 windWork !< array for sector's wind speed data
	real, allocatable, dimension(:) :: myLats, & !< latitudes associated with one sector
								   myLons !< longitudes associated with one sector
	integer, allocatable, dimension(:) :: myLatIs,& !< latitude indices associated with one sector
									  myLonJs !< longitude indices associated with one sector
	real :: sectorRadius, & !< radius (in km) of search sectors
			pslThreshold, & !< sectors whose minimum pressures exceed this value will be ignored
			vortThreshold, & !< sectors whose maximum vorticity falls below this value will be ignored
			windThreshold !< sectors whose maximum windspeed falls below this value will be ignored
end type

contains

!> @brief Defines sectors for a StrideSearch. 
!> Allocates working memory for each sector.
!> @param sSearch object to be initialized
!> @param southernBoundary latitude of southernmost extent of search domain @f$ (\ge -90.0 ) @f$
!> @param northernBoundary latitude of northernmost extent of search domain @f$ (\le 90.0 ) @f$
!> @param sectorRadius radius of each circular search sector (km)
!> @param pslThreshold sectors whose minimum pressures exceed this value will be ignored
!> @param vortThreshold sectors whose maximum vorticity falls below this value will be ignored
!> @param windThreshold sectors whose maximum windspeed falls below this value will be ignored
!> @param searchData container for netcdf data
subroutine SearchSetup( sSearch, southernBoundary, northernBoundary, sectorRadius, &
					 	pslThreshold, vortThreshold, windThreshold, searchData)
	type(StrideSearchSector), intent(out) :: sSearch
	real, intent(in) :: southernBoundary, northernBoundary, sectorRadius, pslThreshold, vortThreshold, windThreshold
	type(StrideSearchData), intent(in) :: searchData
	!
	integer :: i, j, k, nSectors, nStrips, latPtsPerSector, lonPtsPerSector
	real :: eqGridSpacing, latCircSpacing 
	real :: stormArcLength, latStripWidth
	real, allocatable :: latLines(:)
	
	sSearch%sectorRadius = sectorRadius
	sSearch%pslThreshold = pslThreshold
	sSearch%vortThreshold = vortThreshold
	sSearch%windThreshold = windThreshold
	
	!
	!	Define sector centers
	!	
	stormArcLength = sectorRadius / EARTH_RADIUS 	
	nStrips = floor( (northernBoundary * DEG_2_RAD - southernBoundary * DEG_2_RAD ) / stormArcLength ) + 1	
	latStripWidth = (northernBoundary - southernBoundary) / real(nStrips)
	latStrideReal = latStripWidth
	
	allocate(latLines( nStrips + 1))
	allocate(sectorCenterLats( nStrips + 1 ) )
	do i = 1, nStrips + 1
		latLines(i) = southernBoundary + (i - 1) * latStripWidth
	enddo
	sectorCenterLats = latLines
		
	allocate(nLonsPerLatLine( nStrips + 1) )
	allocate(lonStrideReals( nStrips + 1) )
	allocate(lonStrideInts( nStrips + 1) )
	do i = 1, nStrips + 1
		lonStrideReals(i) = min( sectorRadius / ( EARTH_RADIUS * cos( sectorCenterLats(i) * DEG_2_RAD ) ), 2.0 * PI )
		nLonsPerLatLine(i) = floor( 2.0 * PI / lonStrideReals(i) ) 
	enddo
	lonStrideReals = RAD_2_DEG * lonStrideReals
	nSectors = sum(nLonsPerLatLine)
	
	allocate(sectorCenterLons( nSectors ) )

	k = 0
	do i = 1, nStrips + 1
		do j = 1, nLonsPerLatLine(i)
			k = k + 1
			sectorCenterLons(k) = (j - 1) * lonStrideReals(i)
		enddo
	enddo
	deallocate(latLines)

	!
	!	Find boundaries of search domain in data arrays
	!
	if ( southernBoundary > -90.0 ) then
		do i = 1, searchData%nLat
			if ( searchData%lats(i) >= southernBoundary ) then
				latMinIndex = i
				exit
			endif
		enddo
	else
		latMinIndex = 1
	endif
	
	if ( northernBoundary < 90.0 ) then
		do i = latMinIndex, searchData%nLat
			if ( searchData%lats(i) >= northernBoundary ) then
				latMaxIndex = i
				exit
			endif
		enddo
	else
		latMaxIndex = searchData%nLat
	endif

	!
	!	Allocate sufficient space for each sector to search the data
	!
	latStrideInt = floor( latStrideReal * searchData%nLon / ( 2.0 * PI ) ) + 1
	latPtsPerSector = 2 * latStrideInt + 1
	
	do i = 1, nStrips + 1
		if ( abs(sectorCenterLats(i) - 90.0) < ZERO_TOL ) then ! sector center is a pole
			lonStrideInts(i) = searchData%nLon 
		else
			lonStrideInts(i) = floor( lonStrideReals(i) * searchData%nLon / (2.0 * PI ) ) + 1
		endif
	enddo
	lonPtsPerSector = 2 * maxval(lonStrideInts) + 1
	
	allocate(sSearch%neighborhood( lonPtsPerSector, latPtsPerSector ))
	allocate(sSearch%pslWork( lonPtsPerSector , latPtsPerSector))
	allocate(sSearch%windWork(lonPtsPerSector, latPtsPerSector))
	allocate(sSearch%vortWork( lonPtsPerSector, latPtsPerSector))
	allocate(sSearch%myLats(latPtsPerSector))
	allocate(sSearch%myLatIs(latPtsPerSector))
	allocate(sSearch%myLons( lonPtsPerSector ) )
	allocate(sSearch%myLonJs(lonPtsPerSector ) )
	
	
	print *, "StrideSearch::SearchSetup complete : ", nSectors, " search sectors defined."
end subroutine

!> @brief Deletes memory used for stride search.
!> This memory was allocated by SearchSetup.
!> @param sSearch object to be deleted.
subroutine FinalizeSearch( sSearch )
	type(StrideSearchSector), intent(inout) :: sSearch
	deallocate(sSearch%neighborhood)
	deallocate(sSearch%pslWork)
	deallocate(sSearch%windWork)
	deallocate(sSearch%vortWork)
	deallocate(sSearch%myLats)
	deallocate(sSearch%myLatIs)
	deallocate(sSearch%myLons)
	deallocate(sSearch%myLonJs)
end subroutine

!> @brief Prints basic info about a StrideSearch object to the console.
!> @param sSearch
subroutine PrintSearchInfo( sSearch )
	type(StrideSearchSector), intent(in) :: sSearch
	print *, "sector radius = ", sSearch%sectorRadius, " km."
	print *, "pslThreshold = ", sSearch%pslThreshold, " Pa."
	print *, "vortThreshold = ", sSearch%vortThreshold, " 1/s"
	print *, "windThreshold = ", sSearch%windThreshold, " m/s"
end subroutine

!> @brief Performs the StrideSearch algorithm.  
!> Writes sectors whose data exceed storm identification thresholds to an output list of StormListNode objects.
!> @param stormList (output) list of storms that meet or exceed identification criteria.  
!> 			Will contain duplicate detections of the same storm by different sectors.
!> @param sSearch StrideSearch object initialized by SearchSetup
!> @param searchData data container for netcdf input data
subroutine DoStrideSearch( stormList, sSearch, searchData )
	type(StormListNode), pointer, intent(inout) :: stormList
	type(StrideSearchSector), intent(inout) :: sSearch
	type(StrideSearchData), intent(in) :: searchData
	!
	integer :: i, j, k, ii, jj, kk, lonIndex, latIndex, stormI, stormJ
	real :: stormLon, stormLat, stormPsl, stormVort, stormWind
	type(StormListNode), pointer :: tempNodePtr
	logical :: foundStorm
	integer :: nStrips
	real :: dLambda, dataRes
	
	allocate(tempNodePtr)
	nStrips = size(sectorCenterLats)
	
	dLambda = 2.0 * PI / searchData%nLon 
	dataRes = 360.0 / searchData%nLon

	!
	! loop over search sector centers
	!
	kk = 0	
	do ii = 1, nStrips + 1
		do jj = 1, nLonsPerLatLine(ii) 
			kk = kk + 1
			!
			!	locate sector center in data
			!
			sSearch%centerLat = sectorCenterLats(ii)
			sSearch%centerLatIndex = floor( (sectorCenterLats(ii) + 90.0) / dataRes )
			sSearch%centerLon = sectorCenterLons( kk )
			sSearch%centerLonIndex = floor( sSearch%centerLon / dataRes )
			sSearch%myLons = 0.0
			sSearch%myLonJs = 0
			sSearch%myLats = 0.0
			sSearch%myLatIs = 0
			
			!
			!	define sector's neighborhood
			!
			sSearch%neighborhood = .FALSE.
			latIndex = 0
			do i = max(latMinIndex, sSearch%centerLatIndex - latStrideInt), min( latMaxIndex, sSearch%centerLatIndex + latStrideInt )
				latIndex = latIndex + 1
				sSearch%myLats(latIndex) = searchData%lats(i)
				sSearch%myLatIs(latIndex) = i
				
				lonIndex = 0
				if ( sSearch%centerLonIndex - lonStrideInts(ii) < 1 ) then ! sector crosses longitude = 0
					do j = 1, sSearch%centerLonIndex + lonStrideInts(ii)
						lonIndex = lonIndex + 1
						sSearch%myLons(lonIndex) = searchData%lons(j)
						sSearch%myLonJs(lonIndex)= j
					enddo
					do j = searchData%nLon + sSearch%centerLonIndex - lonStrideInts(ii), searchData%nLon
						lonIndex = lonIndex + 1
						sSearch%myLons(lonIndex) = searchData%lons(j)
						sSearch%myLonJs(lonIndex)= j
					enddo
				elseif ( sSearch%centerLonIndex + lonStrideInts(ii) > searchData%nLon ) then ! sector crosses longitude = 360
					do j = sSearch%centerLonIndex - lonStrideInts(ii), searchData%nLon
						lonIndex = lonIndex + 1
						sSearch%myLons(lonIndex) = searchData%lons(j)
						sSearch%myLonJs(lonIndex)= j
					enddo
					do j = 1, lonStrideInts(ii) + searchData%nLon - sSearch%centerLonIndex
						lonIndex = lonIndex + 1
						sSearch%myLons(lonIndex) = searchData%lons(j)
						sSearch%myLonJs(lonIndex)= j
					enddo
				else 
					do j = sSearch%centerLonIndex - lonStrideInts(ii), sSearch%centerLonIndex + lonStrideInts(ii)
						lonIndex = lonIndex + 1
						sSearch%myLons(lonIndex) = searchData%lons(j)
						sSearch%myLonJs(lonIndex)= j
					enddo
				endif	
			enddo
			
			do j = 1, lonIndex
				do i = 1, latIndex				
					if ( SphereDistance( sSearch%centerLon * DEG_2_RAD, sSearch%centerLat * DEG_2_RAD , &
										 sSearch%myLons(j) * DEG_2_RAD, sSearch%myLats(i) * DEG_2_RAD ) < sSearch%sectorRadius ) &
							sSearch%neighborhood(i,j) = .TRUE.
				enddo
			enddo
			
			!
			!	collect data from sector's neighborhood
			!
			sSearch%pslWork = 5.0e20
			sSearch%windWork = 0.0
			sSearch%vortWork = 0.0
			do j = 1, lonIndex
				do i = 1, latIndex
					if ( sSearch%neighborhood(i,j) ) then
						sSearch%pslWork(i,j) = searchData%psl( sSearch%myLonJs(j), sSearch%myLatIs(i) )
						sSearch%windWork(i,j) = searchData%wind( sSearch%myLonJs(j), sSearch%myLatIs(i) )
						sSearch%vortWork(i,j) = sign(1.0, sSearch%myLats(i)) * &
											searchData%vorticity( sSearch%myLonJs(j), sSearch%myLatIs(i) )
					endif
				enddo
			enddo
			
			!
			!	apply storm identification criteria
			!
			foundStorm = .FALSE.
			if ( minval(sSearch%pslWork) < sSearch%pslThreshold )  then
				if ( maxval(sSearch%vortWork) > sSearch%vortThreshold ) then
					if ( maxval(sSearch%windWork) > sSearch%windThreshold ) then
						foundStorm = .TRUE.
					endif				
				endif			
			endif			
			
			if ( foundStorm ) then
				stormPsl = minval(sSearch%pslWork)
				stormVort = maxval(sSearch%vortWork)
				stormWind = maxval(sSearch%windWork)
				do j = 1, lonIndex
					do i = 1, latIndex
						if ( stormPsl == sSearch%pslWork(i,j) ) then
							stormLon = sSearch%myLons(j)
							stormLat = sSearch%myLats(i)
							stormJ = sSearch%myLonJs(j)
							stormI = sSearch%myLatIs(i)
						endif
					enddo
				enddo
				
				call initialize(tempNodePtr, stormLon, stormLat, stormJ, stormI, stormVort, stormPsl, stormWind)

				call AddNodeToList( stormList, tempNodePtr)
			endif
			
		enddo
	enddo

	deallocate(tempNodePtr)
end subroutine

!> @brief Marks duplicate detections and storms at or outside the search domain boundaries for removal.
!> @param stormList output from DoStrideSearch
!> @param distanceTol Storms separated by a distance less than this value are considered duplicates
!> @param southernBoundary
!> @param northernBoundary
subroutine MarkNodesForRemoval(stormList, distanceTol, southernBoundary, northernBoundary )
	type(StormListNode), pointer, intent(inout) :: stormList
	real, intent(in) :: distanceTol, southernBoundary, northernBoundary
	!
	type(StormListNode), pointer :: current, next, query, querynext
	
	current => stormList
	do while (associated(current))
		next => current%next
		if ( current%lat < southernBoundary .OR. current%lat > northernBoundary ) then
			current%removeThisNode = .TRUE.
		else
			query => next
			do while (associated(query) ) 
				querynext => query%next
				if ( SphereDistance( current%lon * DEG_2_RAD, current%lat * DEG_2_RAD, &
								     query%lon * DEG_2_RAD, query%lat * DEG_2_RAD ) < distanceTol ) then
					if ( query%psl < current%psl ) then
						current%removeThisNode = .TRUE.
					else
						query%removeThisNode = .TRUE.
					endif			     
				endif
				query => querynext
			enddo
		endif
		current=>next
	enddo
end subroutine

!> @brief Removes marked nodes from a linked list
!> @param stormList
subroutine RemoveMarkedNodes(stormList)
	type(StormListNode), pointer, intent(inout) :: stormList
	type(StormListNode), pointer :: current, next
	
	current => stormList
	do while ( associated(current) )
		next => current%next
		if ( current%removeThisNode ) then
			call RemoveNodeFromList(stormList, current)
		endif
		current => next
	enddo
end subroutine

!> @brief Marks nodes over land for removal.
!> This procedure does not account for temporal variations, and is applied per timestep.
!> Storms that originate over water but move over land will still be removed.
!> @param stormList
subroutine ApplyLandMask( stormList ) 
	type(StormListNode), pointer, intent(inout) :: stormList
	character(len=256), parameter :: maskfile = "/Users/pabosle/Desktop/stormSearch2/gfdlUtilities/landsea.map"
	integer, parameter :: maskNLat = 180, &
						  maskNLon = 360, &
						  maskFileRows = 1620, &
						  maskFileColumns = 40
	integer :: mask0(maskFileColumns,maskFileRows), mask(maskNLon,maskNLat)
	integer :: i, j
	real :: dLon, dLat, lon0, lat0
	type(StormListNode), pointer :: current, next
	
	open(unit=12, file=trim(maskfile), status='OLD', action='READ')
		do j= 1, maskFileRows
			read(12,'(40I2)') mask0(:,j)
		enddo
	close(12)
	
	mask = RESHAPE(mask0, (/ maskNLon, maskNLat /) )
	mask = CSHIFT( mask, maskNLon/2, 1 )
	
	lon0 = 0.0
	lat0 = -90.0 + 90.0 / float( maskNLat )
	
	dlon = 360.0 / float(maskNLon)
	dLat = -2.0 * lat0 / float(maskNLat - 1)
	
	current => stormList
	do while ( associated(current) ) 
		next => current%next
		j = (current%lat - lat0)/dLat + 1.5
		i = (current%lon - lon0)/dLon + 1.5
		if ( i == 0 ) i = maskNLon
		if ( i > maskNLon ) i = i - maskNLon
		if ( mask(i,j) /= 0 ) then
			current%removeThisNode = .TRUE.
		endif
		current=>next
	enddo
end subroutine


!> @brief Determines the great-circle distance between points on an Earth-sized sphere.
!!
!> @param lon1 longitude of point 1
!> @param lat1 latitude of point 1
!> @param lon2 longitude of point 2
!> @param lat2 latitude of point 2
!> @return SphereDistance great-circle distance separating point 1 from point 2, in km.
pure function SphereDistance(lon1, lat1, lon2, lat2)
	real :: SphereDistance
	real, intent(in) :: lon1, lat1, lon2, lat2
	SphereDistance = EARTH_RADIUS * acos( sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2)*cos(lon2-lon1))
	!SphereDistance = ( (lon2-lon1)*(lon2-lon1) + (lat2-lat1)*(lat2-lat1) )
end function

!> @}
end module