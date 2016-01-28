module TropicalStrideSearchModule
!> @file TropicalStrideSearch.f90 
!! @brief Extends StrideSearch to the application of tropical cyclone detection.
!> @author Peter Bosler SNL
!!

!> @defgroup TropicalStrideSearch TropicalStrideSearch
!! @brief Extends the Stride Search data structure for tropical cyclone detection
!!
!! @{
use StrideSearchModule
use TropicalDataModule
use StormListNodeModule, only : DEG_2_RAD
use TropicalStormListNodeModule

implicit none
private 
public TropicalStrideSearch, TropicalSearchSetup, DoTropicalSearch, FinalizeTropicalSearch, PrintTropicalSearchInfo
public RemoveMarkedTropicalNodes, MarkTropicalNodesForRemoval, ApplyTropicalLandMask, GetLandMask

type, extends(StrideSearch) :: TropicalStrideSearch
	real, pointer, dimension(:,:) :: tempWork
	real :: vortPslDistThreshold
	real :: tempExcessThreshold
	real :: tempPslDistThreshold
end type

contains

!> @brief Defines search sectors based on sectorRadius, links sectors to data.
!!
!> @param tSearch StrideSearch object defined by this subroutine
!> @param southernBoundary southernmost latitude (in degrees_north) of search domain
!> @param northernBoundary northernmost latitude (in degrees_north) of search domain
!> @param sectorRadius spatial scale of a typical storm (in km); a maximum of 1 storm will be found in any 
!>	 spherical circle with geodesic radius = sectorRadius
!> @param pslThreshold storms with minimum pressures greater than this value (in Pa) will be ignored
!> @param vortThreshold storms with cyclonic vorticity (1/s) less than this value will be ignored
!> @param windThreshold storms with maximum winds (m/s) less than this value will be ignored
!> @param vortPslDistThreshold maximum allowable distance (in km) separating a storm's pressure minimum from its
!>	vorticity maximum
!> @param tempExcessThreshold storms whose maximum vertically averaged temperature does not exceed the sector average of 
!>  the vertically average temperature by at least this amount will be ignored
!> @param tempPslDistThreshold maximum allowable distance (in km) separating a storms' pressure minimum from the location 
!>  of its maximum vertically averaged temperature
!> @param tData data read from netcdf file by @ref tropicaldatamodule::readtropicalvariablesattimestep
subroutine TropicalSearchSetup( tSearch, southernBoundary, northernBoundary, sectorRadius, &
								pslThreshold, vortThreshold, windThreshold, &
								vortPslDistThreshold, tempExcessThreshold, tempPslDistThreshold, &
								tData )	
	type(TropicalStrideSearch), intent(out) :: tSearch
	real, intent(in) :: southernBoundary, northernBoundary, sectorRadius
	real, intent(in) :: pslThreshold, vortThreshold, windThreshold
	real, intent(in) :: vortPslDistThreshold, tempExcessThreshold, tempPslDistThreshold
	type(TropicalData), intent(in) :: tData
	
	call SearchSetup(tSearch%StrideSearch, southernBoundary, northernBoundary, sectorRadius, &
					 pslThreshold, vortThreshold, windThreshold, tData%StrideSearchData)
	allocate(tSearch%tempWork( 2*maxval(lonStrides) + 1, 2*latStride+1))
	tSearch%vortPslDistThreshold = vortPslDistThreshold
	tSearch%tempExcessThreshold = tempExcessThreshold
	tSearch%tempPslDistThreshold = tempPslDistThreshold
end subroutine

!> @brief Frees memory used by a TropicalStrideSearch object which is allocated in TropicalSearchSetup.  
!> @param tSearch object to be deleted
subroutine FinalizeTropicalSearch( tSearch )
	type(TropicalStrideSearch), intent(inout) :: tSearch
	deallocate(tSearch%tempWork)
	call FinalizeSearch(tSearch%StrideSearch)
end subroutine

!> @brief Prints basic info about a TropicalStrideSearch object to the console
!> @param tSearch
subroutine PrintTropicalSearchInfo( tSearch )
	type(TropicalStrideSearch), intent(in) :: tSearch
	call PrintSearchInfo( tSearch%StrideSearch )
	print *, "vortPslDistThreshold = ", tSearch%vortPslDistThreshold
	print *, "tempExcessThreshold = ", tSearch%tempExcessThreshold
	print *, "tempPslDistThreshold = ", tSearch%tempPslDistThreshold
end subroutine

!> @brief Performs a stride search of a data set, identifies storms according to the criteria set in TropicalSearchSetup.
!> @param tstormList list of storms found during this Stride Search
!> @param tSearch Stride Search object defined by TropicalSearchSetup
!> @param tData netcdf data output by ReadTropicalVariablesAtTimestep
subroutine DoTropicalSearch( tstormList, tSearch, tData )
	type(TropicalStormListNode), pointer, intent(inout) :: tstormList
	type(TropicalStrideSearch), intent(inout) :: tSearch
	type(TropicalData), intent(in) :: tData
	!
	integer :: i, j, k, ii, jj, lonIndex, latIndex, stormI, stormJ
	real :: stormLon, stormLat, stormPsl, stormVort, stormWind
	real :: stormTemp, tempLon, tempLat, stormThick, thickLon, thickLat
	real :: vortLon, vortLat
	logical :: hasWarmCore, hasThickness
	type(TropicalStormListNode), pointer :: tempNode
	logical :: foundStorm, crit1, crit2, crit3, crit4
	real :: avgT
	integer :: maxPtsPerSector, minPtsPerSector
	
	allocate(tempNode)
	
	maxPtsPerSector = 0
	minPtsPerSector = tdata%nLon * tData%nLat
	
	!
	!	loop over search sector centers (defined in TropicalSearchSetup)
	!
	k = 0 ! latitude strip counter
	do i = latMinIndex, latMaxIndex, latStride
		k = k + 1
		
		do j = 1, tData%nLon, lonStrides(k)
			
			!
			!	define search sector
			!
			
			tSearch%neighborhood = .FALSE.
			!
			!		collect latitudes and longitudes near sector center
			!
			latIndex = 0
			do ii = max(latMinIndex, i - latStride ), min(latMaxIndex, i + latStride)
				latIndex = latIndex + 1
				tSearch%myLats(latIndex) = tData%lats(ii)
				tSearch%myLatIs(latIndex) = ii
			enddo
			lonIndex = 0
			if ( j - lonStrides(k) < 1 ) then ! sector crosses longitude = 0.0
				do jj = 1, j + lonStrides(k)
					lonIndex = lonIndex + 1
					tSearch%myLons(lonIndex) = tData%lons(jj)
					tSearch%myLonJs(lonIndex) = jj
				enddo
				do jj = tData%nLon - lonStrides(k), tData%nLon
					lonIndex = lonIndex + 1
					tSearch%myLons(lonIndex) = tData%lons(jj)
					tSearch%myLonJs(lonIndex) = jj
				enddo
			elseif( j + lonStrides(k) > tData%nLon ) then ! sector crosses longitude = 360.0
				do jj = 1, j + lonStrides(k) - tData%nLon
					lonIndex = lonIndex + 1
					tSearch%myLons(lonIndex) = tData%lons(jj)
					tSearch%myLonJs(lonIndex) = jj
				enddo
				do jj = j - lonStrides(k), tData%nLon
					lonIndex = lonIndex + 1
					tSearch%myLons(lonIndex) = tData%lons(jj)
					tSearch%myLonJs(lonIndex) = jj
				enddo
			else
				do jj = j - lonStrides(k), j + lonStrides(k)
					lonIndex = lonIndex + 1
					tSearch%myLons(lonIndex) = tData%lons(jj)
					tSearch%myLonJs(lonIndex) = jj
				enddo
			endif ! sector crosses longitude = 0,360
			
			!
			!		define sector by geodesic radius
			!
			do ii = 1, latIndex
				do jj = 1, lonIndex
					if ( SphereDistance( tData%lons(j) * DEG_2_RAD, tData%lats(i) * DEG_2_RAD, &
										 tSearch%myLons(jj)*DEG_2_RAD, tSearch%myLats(ii)*DEG_2_RAD ) &
										 < tSearch%sectorRadius ) then
						tSearch%neighborhood(jj,ii) = .TRUE.
					endif
				enddo
			enddo
			
			if ( count(tSearch%neighborhood) > maxPtsPerSector ) then
				maxPtsPerSector = count(tSearch%neighborhood)
			endif
			if ( count(tSearch%neighborhood) < minPtsPerSector ) then
				minPtsPerSector = count(tSearch%neighborhood)
			endif
			
			!
			!	collect data from points in sector
			!
			tSearch%pslWork = 1.0e20
			tSearch%windWork = 0.0
			tSearch%vortWork = 0.0
			tSearch%tempWork = 0.0
			do ii = 1, latIndex
				do jj = 1, lonIndex
					if ( tSearch%neighborhood(jj,ii) ) then
						tSearch%pslWork(jj,ii) = tData%psl( tSearch%myLonJs(jj), tSearch%myLatIs(ii))
						tSearch%windWork(jj,ii)= tData%wind(tSearch%myLonJs(jj), tSearch%myLatIs(ii))
						tSearch%vortWork(jj,ii)= sign(1.0, tSearch%myLats(ii)) * &
												 tData%vorticity( tSearch%myLonJs(jj), tSearch%myLatIs(ii))
						tSearch%tempWork(jj,ii)= tData%vertAvgT( tSearch%myLonJs(jj), tSearch%myLatIs(ii))
					endif
				enddo
			enddo
			
			
			!
			!	apply storm identification criteria
			!
			foundStorm = .FALSE.
			crit1 = .FALSE.
			crit2 = .FALSE.
			crit3 = .FALSE.
			crit4 = .FALSE.
			if ( maxval(tSearch%vortWork) > tSearch%vortThreshold ) then ! criteria 1
				
				crit1 = .TRUE.
				
				stormPsl = minval(tSearch%pslWork)
				stormVort = maxval(tSearch%vortWork)
				stormWind = maxval(tSearch%windWork)
				
				avgT = ArithmeticAverageTemp(tSearch)
				tSearch%tempWork = tSearch%tempWork - avgT
				stormTemp = maxval(tSearch%tempWork)
				
				do ii = 1, latIndex
					do jj = 1, lonIndex
						if ( tSearch%pslWork(jj,ii) == stormPsl ) then
							stormLon = tSearch%myLons(jj)
							stormJ = tSearch%myLonJs(jj)
							stormLat = tSearch%myLats(ii)
							stormI = tSearch%myLatIs(ii)
						endif
						if ( tSearch%vortWork(jj,ii) == stormVort ) then
							vortLon = tSearch%myLons(jj)
							vortLat = tSearch%myLats(ii)
						endif
						if ( tSearch%tempWork(jj,ii) == stormTemp ) then
							tempLon = tSearch%myLons(jj)
							tempLat = tSearch%myLats(ii)
						endif		
					enddo
				enddo
				
				if ( SphereDistance( stormLon*DEG_2_RAD, stormLat*DEG_2_RAD, vortLon*DEG_2_RAD, vortLat*DEG_2_RAD ) &
					 < tSearch%vortPslDistThreshold ) crit2 = .TRUE.
				if ( stormTemp > tSearch%tempExcessThreshold ) crit3 = .TRUE.
				if ( SphereDistance( stormLon*DEG_2_RAD, stormLat*DEG_2_RAD, tempLon*DEG_2_RAD, tempLat*DEG_2_RAD ) &
					 < tSearch%tempPslDistThreshold ) crit4 = .TRUE.
			endif ! criteria 1
			
			foundStorm = ( ( crit1 .AND. crit2) .AND. ( crit3 .AND. crit4) )
			hasWarmCore = ( crit3 .AND. crit4 )
			! thickness criteria not used currently
			hasThickness = .TRUE.
			stormThick = 10000.0
			thickLon = 0.0
			thickLat = 0.0
			if ( foundStorm ) then
				call initializeTropical( tempNode, stormLon, stormLat, stormJ, stormI, stormPsl, stormVort, stormWind, &
										vortLon, vortLat, stormTemp, tempLon, tempLat, hasWarmCore, stormThick, &
										thickLon, thickLat, hasThickness )
				call AddTropicalNodeToList( tstormList, tempNode )
			endif! foundStorm
		enddo!j
	enddo! i
	print *, "maxPtsPerSector = ", maxPtsPerSector, ", minPtsPerSector = ", minPtsPerSector
	deallocate(tempNode)
end subroutine

!> @brief Marks duplicate detections of the same storm, and storms on our outside the search domain boundaries for removal.
!> @param stormList list of possible storms output by DoTropicalSearch
!> @param tSearch TropicalStrideSearch object output by TropicalSearchSetup
!> @param southernBoundary southernmost latitude (in degrees_north) of search domain
!> @param northernBoundary northernmost latitude (in degrees_north) of search domain
subroutine MarkTropicalNodesForRemoval( stormList, tSearch, southernBoundary, northernBoundary )
	type(TropicalStormListNode), pointer, intent(inout) :: stormList
	type(TropicalStrideSearch), intent(in) :: tSearch
	real, intent(in) :: southernBoundary, northernBoundary
	type(TropicalStormListNode), pointer :: current, next, query, querynext
	
	current => stormList
	do while (associated(current) )
		next => current%nextTropical
		if ( current%lat <= southernBoundary .OR. current%lat >= northernBoundary ) then
			current%removeThisNode = .TRUE.
		else
			query => next
			do while ( associated( query ) ) 
				querynext => query%nextTropical
				if ( SphereDistance( current%lon*DEG_2_RAD, current%lat*DEG_2_RAD, &
									 query%lon*DEG_2_RAD, query%lat*DEG_2_RAD ) < tSearch%sectorRadius ) then
					if ( query%psl < current%psl ) then
						current%removeThisNode = .TRUE.
					else
						query%removeThisNode = .TRUE.				 
					endif
				endif
				query => querynext
			enddo
		endif
		current => next
	enddo
end subroutine

!> @brief Deletes any TropicalStormListNode marked for removal by another subroutine.
!> @param stormList 
subroutine RemoveMarkedTropicalNodes(stormList)
	type(TropicalStormListNode), pointer, intent(inout) :: stormList
	type(TropicalStormListNode), pointer :: current, next
	
	current => stormList
	do while ( associated(current) )
		next => current%nextTropical
		if ( current%removeThisNode ) then
			call RemoveTropicalNodeFromList(stormList, current)
		endif
		current => next
	enddo
end subroutine

!> @brief Marks any storms over land for removal
!> @warning This routine applies to all storms, even those that may have originated over water and moved over land.
!> @param stormlist
subroutine ApplyTropicalLandMask( stormList )
	type(TropicalStormListNode), pointer, intent(inout) :: stormList
	character(len=256), parameter :: maskfile = "/Users/pabosle/Desktop/stormSearch2/gfdlUtilities/landsea.map"
	integer, parameter :: maskNLat = 180, &
						  maskNLon = 360, &
						  maskFileRows = 1620, &
						  maskFileColumns = 40
	integer :: mask0(maskFileColumns,maskFileRows), mask(maskNLon,maskNLat)
	integer :: i, j
	real :: dLon, dLat, lon0, lat0
	type(TropicalStormListNode), pointer :: current, next
	
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
		next => current%nextTropical
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

!> @brief Computes an arithemetic average of a sector's vertically averaged temperature
!> @param tSearch contains data to be averaged
!> @return ArithmeticAverageTemp
pure function ArithmeticAverageTemp( tSearch )
	real :: ArithmeticAverageTemp
	type(TropicalStrideSearch), intent(in) :: tSearch
	!
	integer :: ii, jj
	real :: sum
	sum = 0.0
	do ii = 1, size(tSearch%myLatIs)
		do jj = 1, size(tSearch%myLonJs)
			if ( tSearch%neighborhood(jj,ii) ) sum = sum + tSearch%tempWork(jj,ii)
		enddo
	enddo
	ArithmeticAverageTemp = sum / count(tSearch%neighborhood)
end function

!> @brief Returns an integer array used to define land masks
function GetLandMask()
	real :: GetLandMask(360,180)
	character(len=256), parameter :: maskfile = "/Users/pabosle/Desktop/stormSearch2/gfdlUtilities/landsea.map"
	integer :: mask0(40,1620)
	integer :: j
	open(unit=12, file=maskfile, status='OLD', action='READ')
		do j = 1, 1620
			read(12, '(40I2)') mask0(:,j)
		enddo
	close(12)
	GetLandMask = RESHAPE(mask0, (/ 360, 180 /) )
	GetLandMask = CSHIFT( GetLandMask, 180, 1 )
end function

!> @}
end module