module PolarStrideSearchModule
!> @file PolarStrideSearch.f90
!> @brief Extends StrideSearch to the application of polar low detection.
!> @author Peter Bosler SNL

!> @defgroup PolarStrideSearch PolarStrideSearch
!> @brief Extends the StrideSearch data type to polar low detection.
!> @{
use PolarDataModule
use PolarLowListNodeModule
use StormListNodeModule, only : DEG_2_RAD
use StrideSearchModule

implicit none

private
public PolarStrideSearchSector, PolarSearchSetup, DoPolarSearch, FinalizePolarSearchSector
public RemoveMarkedPolarNodes, MarkPolarNodesForRemoval

!> @class PolarStrideSearch
!> @brief Container for local data sets (one sector).  Provides a workspace for spatial search.
type, extends(StrideSearchSector) :: PolarStrideSearchSector
	real, pointer, dimension(:,:) :: tempWork
	real, pointer, dimension(:,:) :: iceWork
	real :: coldAirThreshold
	real :: vortPslDistThreshold
	real :: iceFracThreshold
end type

contains

!> @brief Defines the sectors for a stride search of polar latitudes. Allocates memory for each sector.
!> @param pSearch search object to be initialized
!> @param southernBoundary latitude of southernmost extent of search domain @f$ (\ge -90.0 ) @f$
!> @param northernBoundary latitude of northernmost extent of search domain @f$ (\le 90.0 ) @f$
!> @param sectorRadius radius of each circular search sector (km)
!> @param pslThreshold sectors whose minimum pressures exceed this value will be ignored
!> @param vortThreshold sectors whose maximum vorticity falls below this value will be ignored
!> @param windThreshold sectors whose maximum windspeed falls below this value will be ignored
!> @param coldAirThreshold air-sea temp differential threshold; values above this value will be ignored
!> @param vortPslDistThreshold collocation threshold for location of vorticity max relative to location of psl min
!> @param iceThreshold minimum allowable ice fraction per sector
!> @param pData data container for netcdf input
subroutine PolarSearchSetup( pSearch, southernBoundary, northernBoundary, sectorRadius, &
						 	 pslThreshold, vortThreshold, windThreshold, coldAirThreshold, vortPslDistThreshold, iceThreshold, &
						 	 pData )
	type(PolarStrideSearchSector), intent(out) :: pSearch
	real, intent(in) :: southernBoundary, northernBoundary, sectorRadius
	real, intent(in) :: pslThreshold, vortThreshold, windThreshold, coldAirThreshold, vortPslDistThreshold, iceThreshold
	type(PolarData), intent(in) :: pData
	
	call SearchSetup( pSearch%StrideSearchSector, southernBoundary, northernBoundary, sectorRadius, &
					  pslThreshold, vortThreshold, windThreshold, pData%StrideSearchData )
	pSearch%coldAirThreshold = coldAirThreshold
	pSearch%vortPslDistThreshold = vortPslDistThreshold
	pSearch%iceFracThreshold = iceThreshold
end subroutine

subroutine AllocatePolarSectorMemory( pSearch, pData, stripIndex )
	type(PolarStrideSearchSector), intent(inout) :: pSearch
	type(PolarData), intent(in) :: pData
	integer, intent(in) :: stripIndex
	
	call AllocateSectorMemory(pSearch%StrideSearchSector, pData%StrideSearchData, stripIndex)
	allocate(pSearch%tempWork( size(pSearch%myLatIs), size(pSearch%myLonJs)) )
	allocate(pSearch%iceWork( size(pSearch%myLatIs), size(pSearch%myLonJs)) )
end subroutine

!> @brief Frees memory used by a PolarStrideSearch object. This memory was initialized by PolarSearchSetup
!> @param pSearch
subroutine FinalizePolarSearchSector( pSearch )
	type(PolarStrideSearchSector), intent(inout) :: pSearch
	deallocate(pSearch%tempWork)
	deallocate(pSearch%iceWork)
	call FinalizeSector(pSearch%StrideSearchSector)
end subroutine

!> @brief Performs a Stride Search of polar domains.
!> @param pstormList linked list of detected storms (output)
!> @param pSearch workspace for stride SearchSetup
!> @param pData data container
subroutine DoPolarSearch( pstormList, pSearch, pData )
	type(PolarLowListNode), pointer, intent(inout) :: pstormList
	type(PolarStrideSearchSector), intent(inout) :: pSearch
	type(PolarData), intent(in) :: pData
	!
	integer :: i, j, k, ii, jj, kk, lonIndex, latINdex, stormI, stormJ
	real :: stormLon, stormLat, stormPsl, stormVort, stormWind
	real :: stormSSTMax, stormTempDiff, vortLon, vortLat
	type(PolarLowListNode), pointer :: tempNode
	logical :: foundStorm, crit1, crit2, crit3, crit4, crit5
	
	allocate(tempNode)
	!
	!	loop over search sectors
	!
	kk = 0
	do ii = 1, size(sectorCenterLats)
		call AllocatePolarSectorMemory(pSearch, pData, ii)
		do jj = 1, nLonsPerLatLine(ii)
			kk = kk + 1
			call DefineSectorInData(pSearch%StrideSearchSector, pData%StrideSearchData, ii, kk)
			!
			!	collect data from sector's neighborhood
			!
			pSearch%pslWork = 1.0e20
			pSearch%windWork = 0.0
			pSearch%vortWork = 0.0
			pSearch%tempWork = 1.0e20
			pSearch%iceWork = 100.0
			do j = 1, size(pSearch%myLonJs)
				do i = 1, size(pSearch%myLatIs)
					pSearch%pslWork(i,j)  = pData%psl( pSearch%myLonJs(j), pSearch%myLatIs(i))
					pSearch%windWork(i,j) = pData%wind( pSearch%myLonJs(j), pSearch%myLatIs(i))
					pSearch%vortWork(i,j) = sign(1.0, pSearch%myLats(i)) * &
											pData%vorticity( pSearch%myLonJs(j), pSearch%myLatIs(i))
					pSearch%tempWork(i,j) = pData%theta700( pSearch%myLonJs(j), pSearch%myLatIs(i)) - &
											pData%sst( pSearch%myLonJs(j), pSearch%myLatIs(i))
					pSearch%iceWork(i,j)  = pData%iceFrac( pSearch%myLonJs(j), pSearch%myLatIs(i))
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
			crit5 = .FALSE.
			if ( minval(pSearch%pslWork) < pSearch%pslThreshold ) then
				crit1 = .TRUE.
				stormPsl = minval(pSearch%pslWork)
				do j = 1, size(pSearch%myLonJs)
					do i = 1, size(pSearch%myLatIs)
						if ( pSearch%pslWork(i,j) == stormPSL ) then
							stormLon = pSearch%myLons(j)
							stormLat = pSearch%myLats(i)
							stormJ = pSearch%myLonJs(j)
							stormI = pSearcH%myLatIs(i)
						endif
					enddo
				enddo
				
				if ( minval(pSearch%tempWork) < pSearch%coldAirThreshold ) then
					crit2 = .TRUE.
					stormTempDiff = minval(pSearch%tempWork)
				endif	
				
				if ( maxval(pSearch%vortWork) > pSearch%vortThreshold ) then
					crit3 = .TRUE.
					stormVort = maxval(pSearch%vortWork)
					do j = 1, size(pSearch%myLonJs)
						do i = 1, size(pSearch%myLatIs)
							if ( pSearch%vortWork(i,j) == stormVort ) then
								vortLon = pSearch%myLons(j)
								vortLat = pSearch%myLats(i)
							endif
						enddo
					enddo
					
					if ( SphereDistance( stormLon * DEG_2_RAD, stormLat * DEG_2_RAD, &
										 vortLon * DEG_2_RAD, vortLat * DEG_2_RAD ) < pSearch%vortPslDistThreshold ) then
						crit4 = .TRUE.
					endif
				endif
				
				if ( minval(pSearch%iceWork) <= pSearch%iceFracThreshold ) then
					crit5 = .TRUE.
				endif
				
				foundStorm = ( (crit1 .and. crit2) .and. (crit3 .and. crit4)) .and. crit5
				if ( foundStorm ) then
					stormWind = maxval( pSearch%windWork )
					call InitializePolar( tempNode, stormLon, stormLat, stormJ, stormI, stormPsl, stormVort, stormWind, &
										  vortLon, vortLat, stormTempDiff )
					call AddPolarNodeToList( pstormList, tempNode )
				endif
				
			endif
		enddo
		call FinalizePolarSearchSector(pSearch)
	enddo
	deallocate(tempNode)
end subroutine

!> @brief Marks duplicate detections for removal. Also marks storms on search domain boundaries for removal
!> @param pstorms list output from DoPolarSearch
!> @param pSearch search workspace object
!> @param southernBoundary
!> @param northernBoundary
subroutine MarkPolarNodesForRemoval( pstorms, pSearch, southernBoundary, northernBoundary )
	type(PolarLowListNode), pointer, intent(inout) :: pstorms
	type(PolarStrideSearchSector), intent(in) :: pSearch
	real, intent(in) :: southernBoundary, northernBoundary
	type(PolarLowListNode), pointer :: current, next, query, querynext
	
	current => pstorms
	do while ( associated(current) )
		next => current%nextPolar
		if ( .NOT. current%removeThisNode ) then
			if ( current%lat <= southernBoundary .OR. current%lat >= northernBoundary ) then	
				current%removeThisNode = .TRUE.
			else
				query => next
				do while ( associated( query ) )
					querynext => query%nextPolar
					if ( SphereDistance( current%lon * DEG_2_RAD, current%lat * DEG_2_RAD, &
								query%lon * DEG_2_RAD, query%lat * DEG_2_RAD ) < pSearch%sectorRadius ) then
						if ( query%psl < current%psl ) then
							current%removeThisNode = .TRUE.
						else
							query%removeThisNode = .TRUE.
						endif
					endif
					query => querynext
				enddo
			endif
		endif
		current => next
	enddo
end subroutine

!> @brief Deletes nodes marked by MarkPolarNodesForRemoval from the linked list
!> @param pstorms
subroutine RemoveMarkedPolarNodes( pstorms )
	type(PolarLowListNode), pointer, intent(inout) :: pstorms
	type(PolarLowListNode), pointer :: current, next
	
	current => pstorms
	do while ( associated(current) )
		next => current%nextPolar
		if ( current%removeThisNode ) then
			call RemovePolarNodeFromList( pstorms, current )
		endif
		current => next
	enddo
end subroutine

!> @}
end module