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

!use OMP_LIB

implicit none
private 
public TropicalStrideSearchSector, TropicalSearchSetup, DoTropicalSearch, FinalizeTropicalSector, PrintTropicalSearchInfo
public RemoveMarkedTropicalNodes, MarkTropicalNodesForRemoval, ApplyTropicalLandMask, GetLandMask

type, extends(StrideSearchSector) :: TropicalStrideSearchSector
	real, allocatable, dimension(:,:) :: tempWork
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
	type(TropicalStrideSearchSector), intent(out) :: tSearch
	real, intent(in) :: southernBoundary, northernBoundary, sectorRadius
	real, intent(in) :: pslThreshold, vortThreshold, windThreshold
	real, intent(in) :: vortPslDistThreshold, tempExcessThreshold, tempPslDistThreshold
	type(TropicalData), intent(in) :: tData
	
	call SearchSetup(tSearch%StrideSearchSector, southernBoundary, northernBoundary, sectorRadius, &
					 pslThreshold, vortThreshold, windThreshold, tData%StrideSearchData)
	tSearch%vortPslDistThreshold = vortPslDistThreshold
	tSearch%tempExcessThreshold = tempExcessThreshold
	tSearch%tempPslDistThreshold = tempPslDistThreshold
end subroutine

subroutine AllocateTropicalSectorMemory( tSearch, tData, stripIndex )
	type(TropicalStrideSearchSector), intent(inout) :: tSearch
	type(TropicalData), intent(in) :: tData
	integer, intent(in) :: stripIndex
	
	call AllocateSectorMemory(tSearch%StrideSearchSector, tData%StrideSearchData, stripIndex)
	allocate(tSearch%tempWork( size(tSearch%myLatIs), size(tSearch%myLonJs) ) )
end subroutine

!> @brief Frees memory used by a TropicalStrideSearch object which is allocated in TropicalSearchSetup.  
!> @param tSearch object to be deleted
subroutine FinalizeTropicalSector( tSearch )
	type(TropicalStrideSearchSector), intent(inout) :: tSearch
	deallocate(tSearch%tempWork)
	call FinalizeSector(tSearch%StrideSearchSector)
end subroutine

!> @brief Prints basic info about a TropicalStrideSearch object to the console
!> @param tSearch
subroutine PrintTropicalSearchInfo( tSearch )
	type(TropicalStrideSearchSector), intent(in) :: tSearch
	call PrintSearchInfo( tSearch%StrideSearchSector )
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
	type(TropicalStrideSearchSector), intent(inout) :: tSearch
	type(TropicalData), intent(in) :: tData
	!
	integer :: i, j, k, ii, jj, kk, stormI, stormJ
	real :: stormLon, stormLat, stormPsl, stormVort, stormWind
	real :: stormTemp, tempLon, tempLat, stormThick, thickLon, thickLat
	real :: vortLon, vortLat
	logical :: hasWarmCore, hasThickness
	type(TropicalStormListNode), pointer :: tempNode
	logical :: foundStorm, crit1, crit2, crit3, crit4
	real :: avgT
	integer :: nStrips, tid

	allocate(tempNode)
	!
	!	loop over search sector centers 
	!
	kk = 0
	do ii = 1, size(sectorCenterLats)
		call AllocateTropicalSectorMemory(tSearch, tData, ii)
		
		do jj = 1, nLonsPerLatLine(ii)
			kk = kk + 1
			call DefineSectorInData(tSearch%StrideSearchSector, tData%StrideSearchData, ii, kk)
			!
			!	collect data from sector's neighborhood
			!
			tSearch%pslWork = 1.0e20
			tSearch%windWork = 0.0
			tSearch%vortWork = 0.0
			tSearch%tempWork = 0.0
			do j = 1, size(tSearch%myLonJs)
				do i = 1, size(tSearch%myLatIs)
					if ( tSearch%neighborhood(i,j) ) then
						tSearch%pslWork(i,j) = tData%psl(tSearch%myLonJs(j), tSearch%myLatIs(i))
						tSearch%windWork(i,j) = tData%wind(tSearch%myLonJs(j), tSearch%myLatIs(i))
						tSearch%vortWork(i,j) = sign(1.0, tSearch%myLats(i)) * &
												tData%vorticity(tSearch%myLonJs(j), tSearch%myLatIs(i))
						tSearch%tempWork(i,j) = tData%vertAvgT(tSearch%myLonJs(j), tSearch%myLatIs(i))
					endif
				enddo
			enddo
			
			!
			!	apply storm identification criteria
			!
			crit1 = .FALSE.
			crit2 = .FALSE.
			crit3 = .FALSE.
			crit4 = .FALSE.
		
			if ( maxval(tSearch%vortWork) > tSearch%vortThreshold ) then
				crit1 = .TRUE.
				
				stormPsl = minval(tSearch%pslWork)
				stormVort = maxval(tSearch%vortWork)
				stormWind = maxval(tSearch%windWork)
				
				avgT = ArithmeticAverageTemp(tSearch)
				tSearch%tempWork = tSearch%tempWork - avgT
				stormTemp = maxval(tSearch%tempWork)
				
				do j = 1, size(tSearch%myLonJs)
					do i = 1, size(tSearch%myLatIs)
						if (stormPsl == tSearch%pslWork(i,j) ) then
							stormLon = tSearch%myLons(j)
							stormJ = tSearch%myLonJs(j)
							stormLat = tSearch%myLats(i)
							stormI = tSearch%myLatIs(i)
						endif
						if ( stormVort == tSearch%vortWork(i,j) ) then
							vortLon = tSearch%myLons(j)
							vortLat = tSearch%myLats(i)
						endif
						if ( stormTemp == tSearch%tempWork(i,j) ) then
							tempLon = tSearch%myLons(j)
							tempLat = tSearch%myLats(i)
						endif
					enddo
				enddo
				
				if ( SphereDistance( stormLon * DEG_2_RAD, stormLat * DEG_2_RAD, &
								     vortLon * DEG_2_RAD, vortLat * DEG_2_RAD ) < tSearch%vortPslDistThreshold ) &
					crit2 = .TRUE.
				if ( stormTemp > tSearch%tempExcessThreshold ) crit3 = .TRUE.
				if ( SphereDistance( stormLon * DEG_2_RAD, stormLat * DEG_2_RAD, &
									 tempLon * DEG_2_RAD, tempLat * DEG_2_RAD ) < tSearch%tempPslDistThreshold ) &
					crit4 = .TRUE.
			endif			
			
			foundStorm = ( ( crit1 .AND. crit2) .AND. ( crit3 .AND. crit4) )
			hasWarmCore = ( crit3 .AND. crit4 )
			! thickness criteria not used currently, but is required by output format
			hasThickness = .TRUE.
			stormThick = 10000.0
			thickLon = 0.0
			thickLat = 0.0
			
			if (kk == 1303 ) then
				print *, "SECTOR 1303"
				print *, "foundstorm = ", foundStorm
				print *, "crit1 = ", crit1
				print *, "crit2 = ", crit2
				print *, "crit3 = ", crit3
				print *, "crit4 = ", crit4
				print *, "stormPSL = ", stormPsl
				print *, "stormVort = ", stormVort
				print *, "stormWind = ", stormWind
				print *, "stormTemp = ", stormTemp
				if ( (crit1 .and. crit4) .and. .not. crit2 ) then
					print *, "Sector has valid vortMax with collocated tempMax."
					print *, "stormLon = ", stormLon, ", stormLat = ", stormLat
					print *, " vortLon = ", vortLon,  ",  vortLat = ", vortLat
					print *, "SphereDistance output = ", SphereDistance( stormLon * DEG_2_RAD, stormLat * DEG_2_RAD, &
								     vortLon * DEG_2_RAD, vortLat * DEG_2_RAD )
				endif
				if ( (crit1 .and. crit3) .and. .not. crit2 ) then
					print *, "Sector has valid vortMax and valid tempExcess"
					print *, "stormLon = ", stormLon, ", stormLat = ", stormLat
					print *, " vortLon = ", vortLon,  ",  vortLat = ", vortLat
					print *, "SphereDistance(psl,vort) output = ", SphereDistance( stormLon * DEG_2_RAD, stormLat * DEG_2_RAD, &
								     vortLon * DEG_2_RAD, vortLat * DEG_2_RAD )
					print *, "SphereDistance(psl,temp) output = ", SphereDistance( stormLon * DEG_2_RAD, stormLat * DEG_2_RAD, &
									 tempLon * DEG_2_RAD, tempLat * DEG_2_RAD )
				endif
			endif
			
			if ( foundStorm ) then
				call initializeTropical( tempNode, stormLon, stormLat, stormJ, stormI, stormPsl, stormVort, stormWind, &
										vortLon, vortLat, stormTemp, tempLon, tempLat, hasWarmCore, stormThick, &
										thickLon, thickLat, hasThickness )
				call AddTropicalNodeToList( tstormList, tempNode )
			endif
		enddo
		
		call FinalizeTropicalSector(tSearch)
	enddo
	deallocate(tempNode)
end subroutine

!> @brief Marks duplicate detections of the same storm, and storms on our outside the search domain boundaries for removal.
!> @param stormList list of possible storms output by DoTropicalSearch
!> @param tSearch TropicalStrideSearch object output by TropicalSearchSetup
!> @param southernBoundary southernmost latitude (in degrees_north) of search domain
!> @param northernBoundary northernmost latitude (in degrees_north) of search domain
subroutine MarkTropicalNodesForRemoval( stormList, tSearch, southernBoundary, northernBoundary )
	type(TropicalStormListNode), pointer, intent(inout) :: stormList
	type(TropicalStrideSearchSector), intent(in) :: tSearch
	real, intent(in) :: southernBoundary, northernBoundary
	type(TropicalStormListNode), pointer :: current, next, query, querynext
	
	current => stormList
	do while (associated(current) )
		next => current%nextTropical
		if ( .NOT. current%removeThisNode ) then
			if ( current%lat <= southernBoundary .OR. current%lat >= northernBoundary ) then
				current%removeThisNode = .TRUE.
			else
				query => next
				do while ( associated( query ) ) 
					querynext => query%nextTropical
					if ( SphereDistance( current%lon*DEG_2_RAD, current%lat*DEG_2_RAD, &
										 query%lon*DEG_2_RAD, query%lat*DEG_2_RAD ) < tSearch%sectorRadius ) then
						if ( query%wind > current%wind ) then
							current%removeThisNode = .TRUE.
							query%psl = min( current%psl, query%psl)
							query%vort = max( current%vort, query%vort)
							query%vertAvgT = max(current%vertAvgT, query%vertAvgT)
						else
							query%removeThisNode = .TRUE.				 
							current%psl = min( current%psl, query%psl)
							current%vort = max( current%vort, query%vort)
							current%vertAvgT = max(current%vertAvgT, query%vertAvgT)
						endif
					endif
					query => querynext
				enddo
			endif
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
	character(len=256), parameter :: maskfile = "/Users/pabosle/Desktop/StrideSearch/gfdlUtilities/landsea.map"
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
	lat0 = -90.0 + 0.5
	
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
	type(TropicalStrideSearchSector), intent(in) :: tSearch
	!
	real :: tsum
	tsum = sum( tSearch%tempWork, MASK=tSearch%neighborhood)	
	ArithmeticAverageTemp = tsum / count(tSearch%neighborhood)
end function

!> @brief Returns an integer array used to define land masks
function GetLandMask()
	real :: GetLandMask(360,180)
	character(len=256), parameter :: maskfile = "/Users/pabosle/Desktop/StrideSearch/gfdlUtilities/landsea.map"
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