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

integer, allocatable, public :: lonStrides(:)
integer, public :: latMinIndex, latMaxIndex, latStride

!> @class StrideSearchSector
!> @brief Container for small sets of working data (enough to contain the largest search sector), used to identify storms.
!> Provides a workspace for spatial search.
type StrideSearchSector
	real :: centerLon
	real :: centerLat
	logical, pointer, dimension(:,:) :: neighborhood  !< neighborhood(j,i) = true if grid point at (lon(j),lat(i)) is 
													  !< within a sectorRadius of the sector center
	real, pointer, dimension(:,:) :: pslWork, & !< array for sector's sea level pressure data
									 vortWork, & !< array for sector's vorticity data
									 windWork !< array for sector's wind speed data
	real, pointer, dimension(:) :: myLats, & !< latitudes associated with one sector
								   myLons !< longitudes associated with one sector
	integer, pointer, dimension(:) :: myLatIs,& !< latitude indices associated with one sector
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
	integer :: i, nSectors
	real :: eqGridSpacing, latPtsPerStorm, latCircSpacing, lonPtsPerStorm
	
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
	
	eqGridSpacing = 2.0 * PI * EARTH_RADIUS / searchData%nLon
	latPtsPerStorm = sectorRadius / eqGridSpacing
	latStride = floor( latPtsPerStorm )
	
	allocate(lonStrides( (latMaxIndex - latMinIndex)/latStride + 1 ) )
	do i = 1, (latMaxIndex - latMinIndex)/latStride + 1
		latCircSpacing = eqGridSpacing * cos( searchData%lats(latMinIndex + (i-1)*latStride) * DEG_2_RAD )
		lonPtsPerStorm = sectorRadius / latCircSpacing
		lonStrides(i) = floor(lonPtsPerStorm)
	enddo
	
	allocate(sSearch%neighborhood( 2 * maxval(lonStrides) + 1, 2 * latStride + 1 ))
	allocate(sSearch%pslWork( 2 * maxval(lonStrides) + 1, 2 * latStride + 1 ))
	allocate(sSearch%windWork(2 * maxval(lonStrides) + 1, 2 * latStride + 1 ))
	allocate(sSearch%vortWork(2 * maxval(lonStrides) + 1, 2 * latStride + 1 ))
	allocate(sSearch%myLats(2 * latStride + 1))
	allocate(sSearch%myLatIs(2*latStride + 1))
	allocate(sSearch%myLons(2 * maxval(lonStrides) + 1 ))
	allocate(sSearch%myLonJs(2 * maxval(lonStrides) + 1 ))
	
	sSearch%sectorRadius = sectorRadius
	sSearch%pslThreshold = pslThreshold
	sSearch%vortThreshold = vortThreshold
	sSearch%windThreshold = windThreshold
	
	nSectors = 0
	do i = 1, (latMaxIndex - latMinIndex)/latStride + 1
		nSectors = nSectors + searchData%nLon / lonStrides(i)
	enddo
	
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
	integer :: i, j, k, ii, jj, lonIndex, latIndex, stormI, stormJ
	real :: stormLon, stormLat, stormPsl, stormVort, stormWind
	type(StormListNode), pointer :: tempNodePtr
	logical :: foundStorm
	
	allocate(tempNodePtr)
	
	!
	! loop over search sector centers
	!
	k = 0 ! latitude strip counter
	do i = latMinIndex, latMaxIndex, latStride
		k = k + 1
		
		do j = 1, searchData%nLon, lonStrides(k)
			!
			!	define sector
			!
		
			sSearch%neighborhood = .FALSE.
			
			latIndex = 0
			do ii = max(latMinIndex, i - latStride), min(latMaxIndex, i + latStride)
				latIndex = latIndex + 1
				sSearch%myLats(latIndex) = searchData%lats(ii)
				sSearch%myLatIs(latIndex) = ii
			enddo
			lonIndex = 0
			if ( j - lonStrides(k) < 1 ) then ! sector crosses longitude = 0
				do jj = 1, j + lonStrides(k)
					lonIndex = lonIndex + 1
					sSearch%myLons(lonIndex) = searchData%lons(jj)
					sSearch%myLonJs(lonIndex) = jj
				enddo
				do jj = searchData%nLon - lonStrides(k), searchData%nLon
					lonIndex = lonIndex + 1
					sSearch%myLons(lonIndex) = searchData%lons(jj)
					sSearch%myLonJs(lonIndex) = jj
				enddo
			elseif ( j + lonStrides(k) > searchData%nLon ) then ! sector crosses longitude = 360
				do jj = 1, j + lonStrides(k) - searchData%nLon
					lonIndex = lonIndex + 1
					sSearch%myLons(lonIndex) = searchData%lons(jj)
					sSearch%myLonJs(lonIndex) = jj
				enddo
				do jj = j - lonStrides(k), searchData%nLon
					lonIndex = lonIndex + 1
					sSearch%myLons(lonIndex) = searchData%lons(jj)
					sSearch%myLonJs(lonIndex) = jj
				enddo
			else
				do jj = j - lonStrides(k), j + lonStrides(k)
					lonIndex = lonIndex + 1
					sSearch%myLons(lonIndex) = searchData%lons(jj)
					sSearch%myLonJs(lonIndex) = jj
				enddo
			endif
			
			do ii = 1, latIndex
				do jj = 1, lonIndex
					if ( SphereDistance( searchData%lons(j) * DEG_2_RAD, searchData%lats(i) * DEG_2_RAD , &
										 sSearch%myLons(jj) * DEG_2_RAD, sSearch%myLats(ii) * DEG_2_RAD ) &
										 < sSearcH%sectorRadius ) then
						sSearch%neighborhood(jj,ii) = .TRUE.
					endif
				enddo
			enddo
						
			!
			!	collect data from sector
			!
			sSearch%pslWork = 5.0e20
			sSearch%windWork = 0.0
			sSearch%vortWork = 0.0
			do ii = 1, latIndex
				do jj = 1, lonIndex
					if ( sSearch%neighborhood(jj,ii) ) then
						sSearch%pslWork(jj,ii) = searchData%psl( sSearch%myLonJs(jj), sSearch%myLatIs(ii) )
						sSearch%windWork(jj,ii) = searchData%wind(sSearch%myLonJs(jj), sSearch%myLatIs(ii) )
						sSearch%vortWork(jj,ii) = sign(1.0, sSearch%myLats(ii))  * &
												  searchData%vorticity(sSearch%myLonJs(jj), sSearch%myLatIs(ii) )
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
				do ii = 1, latIndex
					do jj = 1, lonIndex
						if ( stormPsl == sSearch%pslWork(jj,ii) ) then
							stormLon = sSearch%myLons(jj)
							stormJ = sSearch%myLonJs(jj)
							stormLat = sSearch%myLats(ii)
							stormI = sSearch%myLatIs(ii)
						endif
					enddo
				enddo
				
				call initialize(tempNodePtr, stormLon, stormLat, stormJ, stormI, stormVort, stormPsl, stormWind)

				call AddNodeToList( stormList, tempNodePtr)
			endif
		enddo !j 
	enddo ! i
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