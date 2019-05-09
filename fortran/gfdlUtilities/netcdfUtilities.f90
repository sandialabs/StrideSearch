module netcdfUtilitiesModule
!> @defgroup netcdfUtilities netcdfUtilities
!! @brief Reads .nc data 
!!
!!
!!
!! @{
use netcdf

implicit none
public

contains  

!> @brief Get netCDF metadata from file header
!!
!> @param filename
!> @param ncFileID (output)
!> @param nLon
!> @param nLat
!> @param nTimesteps
subroutine GetDimensions( filename, ncFileID, nLon, nLat, nTimesteps )
	character(len=*), intent(in) :: filename
	integer, intent(out) :: ncFileID, nLon, nLat, nTimesteps
	!
	integer :: status
	
	status = nf90_open(path=trim(filename), mode=nf90_nowrite, ncid = ncFileID )
	if ( status /= nf90_noerr) call handle_err(status)
	
	nLon = AXIS_LENGTH( ncFileID, 'lon')
	nLat = AXIS_LENGTH( ncFileID, 'lat')
	nTimesteps = AXIS_LENGTH( ncFileID, 'time')
	
!	write(6,*) "nLon = ", nLon
!	write(6,*) "nLat = ", nLat
!	write(6,*) "nTimesteps = ", nTimesteps
	
end subroutine


!subroutine GetCoordinates( rlon, rlat )
!	real, intent(out), dimension(:) :: rlon, rlat
!	
!	call read_variable_1d( ncFileID, "lon", rlon)
!	call read_variable_1d( ncFileID, "lat", rlat)
!		
!	allocate(time(nTimesteps))
!	
!	call read_variable_1d( ncFileID, 'time', time)
!end subroutine

!> @brief reads a 1d variable from a netcdf file into a buffer
!!
!> @param ncid integer ID of netcdf file
!> @param varname string matching variable name in netcdf file
!> @param buffer preallocated array to hold data
subroutine read_variable_1d (ncid, varname, buffer)
	integer, intent(in)  :: ncid
	character(len=*) , intent(in)  :: varname
	real             , intent(out) :: buffer(:)
	integer              :: varid, status

	status = nf90_inq_varid(ncid, varname, varid)
	if(status /= nf90_NoErr) call handle_err(status)
	status = nf90_get_var(ncid, varid, buffer)
	if(status /= nf90_NoERr) call handle_err(status)
end subroutine read_variable_1d


!> @brief reads a 2d variable from a netcdf file into a buffer
!> Output will have size (/ nLon, nLat /). 
!> 
!> @param ncid integer ID of netcdf file
!> @param varname variable name (must match a variable in the netcdf file)
!> @param start integer array defining start indices and size of buffer
!> @param buffer preallocated array to hold data
subroutine read_variable_2d (ncid, varname, start, buffer)
	integer, intent(in)  :: ncid
	character(len=*) , intent(in)  :: varname
	integer          , intent(in)  :: start(:)
	real             , intent(out) :: buffer(:,:)
	integer              :: varid, status

	status = nf90_inq_varid(ncid, varname, varid)
	if(status /= nf90_NoErr) call handle_err(status)
	status = nf90_get_var(ncid, varid, buffer, start)
	if(status /= nf90_NoERr) call handle_err(status)
end subroutine read_variable_2d

!> @brief determines the length of a netcdf file coordinate axis array.
!> @param ncid integer id of netcdf file
!> @param dimname name of coordinate dimension
function axis_length (ncid, dimname) result (length)
	integer, intent(in)  :: ncid
	character(len=*) , intent(in)  :: dimname
	integer              :: length, dimid, status

	status = nf90_inq_dimid(ncid, dimname, dimid)
	if(status /= nf90_NoErr) call handle_err(status)
	status = nf90_inquire_dimension(ncid, dimid, len=length)
	if(status /= nf90_NoErr) call handle_err(status)
end function axis_length

subroutine handle_err (status)
	integer, intent(in) :: status
	character(len=80) :: errstrg
	errstrg = NF90_STRERROR (status)
	write (*,*) "NETCDF ERROR: "//trim(errstrg)
	stop 111
end subroutine handle_err

!> @}
end module