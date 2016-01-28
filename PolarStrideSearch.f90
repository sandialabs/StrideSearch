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
public PolarStrideSearch, PolarSearchSetup, DoPolarSearch, FinalizePolarSearch
public RemoveMarkedPolarNodes, MarkPolarNodesForRemoval

!> @class PolarStrideSearch
!> @brief Container for local data sets (one sector).  Provides a workspace for spatial search.
type, extends(StrideSearch) :: PolarStrideSearch
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
	type(PolarStrideSearch), intent(out) :: pSearch
	real, intent(in) :: southernBoundary, northernBoundary, sectorRadius
	real, intent(in) :: pslThreshold, vortThreshold, windThreshold, coldAirThreshold, vortPslDistThreshold, iceThreshold
	type(PolarData), intent(in) :: pData
	
	call SearchSetup( pSearch%StrideSearch, southernBoundary, northernBoundary, sectorRadius, &
					  pslThreshold, vortThreshold, windThreshold, pData%StrideSearchData )
	allocate( pSearch%tempWork( 2*maxval(lonStrides) + 1, 2*latStride + 1) )
	allocate( pSearch%iceWork( 2*maxval(lonStrides) + 1, 2*latStride + 1) )
	pSearch%coldAirThreshold = coldAirThreshold
	pSearch%vortPslDistThreshold = vortPslDistThreshold
	pSearch%iceFracThreshold = iceThreshold
end subroutine

!> @brief Frees memory used by a PolarStrideSearch object. This memory was initialized by PolarSearchSetup
!> @param pSearch
subroutine FinalizePolarSearch( pSearch )
	type(PolarStrideSearch), intent(inout) :: pSearch
	deallocate(pSearch%tempWork)
	deallocate(pSearch%iceWork)
	call FinalizeSearch(pSearch%StrideSearch)
end subroutine

!> @brief Performs a Stride Search of polar domains.
!> @param pstormList linked list of detected storms (output)
!> @param pSearch workspace for stride SearchSetup
!> @param pData data container
subroutine DoPolarSearch( pstormList, pSearch, pData )
	type(PolarLowListNode), pointer, intent(inout) :: pstormList
	type(PolarStrideSearch), intent(inout) :: pSearch
	type(PolarData), intent(in) :: pData
	!
	integer :: i, j, k, ii, jj, lonIndex, latINdex, stormI, stormJ
	real :: stormLon, stormLat, stormPsl, stormVort, stormWind
	real :: stormSSTMax, stormTempDiff, vortLon, vortLat
	type(PolarLowListNode), pointer :: tempNode
	logical :: foundStorm, crit1, crit2, crit3, crit4, crit5
	
	allocate(tempNode)
	!
	!	loop over search sectors
	!
	k = 0 ! latitude strip counter
	do i = latMinIndex, latMaxIndex, latStride
		k = k + 1
		do j = 1, pData%nLon, lonStrides(k)
			!
			! define search sector
			!
			pSearch%neighborhood = .FALSE.
			
			latINdex = 0
			do ii = max( latMinIndex, i - latStride), min(latMaxIndex, i + latStride )
				latINdex = latINdex + 1
				pSearch%mylats(latINdex) = pData%lats(ii)
				pSearch%myLatIs(latIndex) = ii
			enddo
			
			lonIndex = 0
			if ( j - lonStrides(k) < 1 ) then ! sector crosses longitude = 0.0
				do jj = 1, j + lonStrides(k)
					lonIndex = lonIndex + 1
					pSearch%myLons(lonIndex) = pData%lons(jj)
					pSearch%myLonJs(lonIndex) = jj
				enddo
				do jj = pData%nLon - lonStrides(k), pData%nLon
					lonIndex = lonIndex + 1
					pSearch%myLons(lonIndex) = pData%lons(jj)
					pSearch%myLonJs(lonIndex) = jj
				enddo
			elseif ( j + lonStrides(k) > pData%nLon ) then ! sector crosses longitude = 360.0
				do jj = 1, j + lonStrides(k) - pData%nLon
					lonIndex = lonIndex + 1
					pSearch%myLons(lonIndex) = pData%lons(jj)
					pSearch%myLonJs(lonIndex) = jj
				enddo
				do jj = j - lonStrides(k), pData%nLon
					lonIndex = lonIndex + 1
					pSearch%myLons(lonIndex) = pData%lons(jj)
					pSearch%myLonJs(lonIndex) = jj
				enddo
			else ! interior sector
				do jj = j - lonStrides(k), j + lonStrides(k)
					lonIndex = lonIndex + 1
					pSearch%myLons(lonIndex) = pData%lons(jj)
					pSearch%myLonJs(lonIndex) = jj
				enddo
			endif ! sector crosses longitude branch cut
			
			!
			!	define sector by geodesic radius
			!
			do ii = 1, latIndex
				do jj = 1, lonIndex
					if ( SphereDistance( pData%lons(j) * DEG_2_RAD, pData%lats(i) * DEG_2_RAD, &
							pSearch%myLons(jj) * DEG_2_RAD, pSearch%myLats(ii) * DEG_2_RAD ) < pSearch%sectorRadius ) then
						pSearch%neighborhood(jj,ii) = .TRUE.
					endif
				enddo
			enddo
			
			!
			!	collect data from points in sector
			!
			pSearch%pslWork = 1.0e20
			pSearch%windWork = 0.0
			pSearch%vortWork = 0.0
			pSearch%tempWork = 1.0e20
			pSearch%iceWork = -1.0
			do ii = 1, latIndex
				do jj = 1, lonIndex
					pSearch%pslWork(jj,ii) = pData%psl( pSearch%myLonJs(jj), pSearch%myLatIs(ii) )
					pSearch%windWork(jj,ii)= pData%wind( pSearch%myLonJs(jj), pSearch%myLatIs(ii) )
					pSearch%vortWork(jj,ii)= sign(1.0, pSearch%myLats(ii) ) * pData%vorticity( pSearch%myLonJs(jj), pSearch%myLatIs(ii) )
					pSearch%tempWork(jj,ii)= pData%theta700( pSearch%myLonJs(jj), pSearch%myLatIs(ii) ) - &
											 pData%sst( pSearch%myLonJs(jj), pSearch%myLatIs(ii) )
					pSearch%iceWork(jj,ii) = pData%iceFrac( pSearch%myLonJs(jj), pSearch%myLatIs(ii) )
				enddo
			enddo
			
			!
			! 	apply storm identification criteria
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
				
				do ii = 1, latIndex
					do jj = 1, lonIndex
						if ( pSearch%pslWork(jj,ii) == stormPsl ) then
							stormLon = pSearch%myLons(jj)
							stormLat = pSearch%myLats(ii)
							stormJ = pSearch%myLonJs(jj)
							stormI = pSearch%myLatIs(ii)
						endif
					enddo
				enddo
				
				if ( minval( pSearch%tempWork) < pSearch%coldAirThreshold ) then
					crit2 = .TRUE.
					stormTempDiff = minval(pSearch%tempWork)
				endif! crit2
				
				if ( maxval(pSearch%vortWork) > pSearch%vortThreshold ) then
					crit3 = .TRUE.
					stormVort = maxval(pSearcH%vortWork) 
					do ii = 1, latIndex
						do jj = 1, lonIndex
							if ( pSearch%vortWork(jj,ii) == stormVort ) then
								vortLon = pSearch%myLons(jj)
								vortLat = pSearch%myLats(ii)
							endif
						enddo
					enddo
					
					if ( SphereDistance( stormLon * DEG_2_RAD, stormLat * DEG_2_RAD, vortLon * DEG_2_RAD, vortLat * DEG_2_RAD ) &
						< pSearch%vortPslDistThreshold ) then
						crit4 = .TRUE.
					endif ! crit4
				endif! crit3
				
				if ( minval(pSearch%iceWork) <= pSearch%iceFracThreshold ) then
					crit5 = .TRUE.
				endif! crit5
				
			endif ! crit1
			
			foundStorm =  ( ( crit1 .and. crit2) .and. (crit3 .and. crit4 )) .and. crit5
			stormWind = maxval(pSearch%windWork)
			if ( foundStorm ) then
				call initializePolar( tempNode, stormLon, stormLat, stormJ, stormI, stormPsl, stormVort, stormWind, &
					 vortLon, vortLat, stormTempDiff )
				
				call AddPolarNodeToList( pstormList, tempNode)
			endif!foundStorm
		enddo ! j
	enddo!i
	deallocate(tempNode)
end subroutine

!> @brief Marks duplicate detections for removal. Also marks storms on search domain boundaries for removal
!> @param pstorms list output from DoPolarSearch
!> @param pSearch search workspace object
!> @param southernBoundary
!> @param northernBoundary
subroutine MarkPolarNodesForRemoval( pstorms, pSearch, southernBoundary, northernBoundary )
	type(PolarLowListNode), pointer, intent(inout) :: pstorms
	type(PolarStrideSearch), intent(in) :: pSearch
	real, intent(in) :: southernBoundary, northernBoundary
	type(PolarLowListNode), pointer :: current, next, query, querynext
	
	current => pstorms
	do while ( associated(current) )
		next => current%nextPolar
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