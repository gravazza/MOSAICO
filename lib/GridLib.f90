!! This file is part of 
!!
!!   MOSAICO -- MOdular library for raSter bAsed hydrologIcal appliCatiOn.
!! 
!!   Copyright (C) 2011 Giovanni Ravazzani
!!
!!   This program is free software: you can redistribute it and/or modify
!!   it under the terms of the GNU General Public License as published by
!!   the Free Software Foundation, either version 3 of the License, or
!!   (at your option) any later version.
!!
!!   This program is distributed in the hope that it will be useful,
!!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!   GNU General Public License for more details.
!!
!!   You should have received a copy of the GNU General Public License
!!   along with this program.  If not, see <http://www.gnu.org/licenses/>.
!! Module Description: 
!!   set of fortran routines to manage input and output of grids.
!!   The module supports different file formats:    
!!   NetCDF raster layer CF 1.0 compliant (Climate and Forecast metadata standard)
!!   ESRI ASCII grid
!!   ESRI BINARY grid
!!
!!   The module introduces two new variable types:
!!   grid_real to store floating point data, and
!!   grid_integer to store integer data.
!!   To initialize a new grid variable, either integer or float, you can:
!!
!!   read grid from NetCdf file passing the standard name:
!!>   TYPE(grid_real) :: new_grid
!!<   CALL NewGrid (new_grid, NetCdfFileName, stdName='air_temperature')
!!
!!   read grid from NetCdf file passing name of variable:
!!>   TYPE(grid_real) :: new_grid
!!<   CALL NewGrid (new_grid, NetCdfFileName, variable = 'T2m')
!!   
!!   If NetCdf archive is multitemporal, that is contains more grids,
!!   one for each time, you can retrieve the grid for a given time:
!!>   TYPE(grid_real) :: new_grid
!!    TYPE (DateTime) :: ncTime
!!    ncTime = '2007-03-05T11:00:00+00:00'
!!<   CALL NewGrid (new_grid, NetCdfFileName, stdName='air_temperature', time = ncTime) 
!!
!!   Read grid from ESRI ASCII file:
!!>    TYPE(grid_real) :: new_grid
!!     fileName = 'file.asc'
!!<    CALL NewGrid (new_grid, fileName, ESRI_ASCII)
!!
!!   Read grid from ESRI BINARY file:
!!>    TYPE(grid_real) :: new_grid
!!     fileName = 'file'
!!<    CALL NewGrid (new_grid, fileName, ESRI_BINARY)
!!
!!   Initialize grid using an existing one as template. The new grid
!!   is initialized with value 0:
!!>    TYPE(grid_real) :: template
!!     TYPE(grid_integer) :: new_Grid
!!<    CALL NewGrid (newGrid, template) 
!!   If you want assign an initial value different than 0:
!!>    CALL NewGrid (new_Grid, template, 100)
!!   The initial value must be of the same type as values stored in grid
!! 
!!   To export a grid to a file you can:
!!
!!   write the grid to a netcdf file as variable termed VariableName:
!!>   TYPE(grid_real) :: grid
!!      ... some statements that assign data to grid
!!<   CALL ExportGrid (grid, NetCdfFileName, VariableName, 'add')
!!   If the netcdf file is multitemporal, you can add grid to already existing ones in file:
!!>   TYPE(grid_real) :: grid
!!      ... some statements that assign data to grid
!!<   CALL ExportGrid (grid, NetCdfFileName, VariableName, 'append')
!!   If NetCdfFileName does not exist, it is created.
!!
!!   Write the grid to ESRI ASCII or BINARY file:
!!>   TYPE(grid_real) :: grid
!!      ... some statements that assign data to grid
!!    fileName = 'file.asc'
!!    CALL ExportGrid (grid, fileName, ESRI_ASCII)
!!    fileName = 'file' 
!!<   CALL ExportGrid (grid, fileName, ESRI_BINARY)
!!
!!   If a grid is no more used you can free memory:
!!>   TYPE(grid_real) :: grid
!!      ... some statements that assign data to grid
!!<   CALL GridDestroy (grid)
!!
!!
!! References:
!!   NetCDF: http://www.unidata.ucar.edu/software/netcdf/
!!   CF 1.0: http://cf-pcmdi.llnl.gov/
!!   UDUNITS: http://www.unidata.ucar.edu/software/udunits/
!! @author <a href="mailto:giovanni.ravazzani@polimi.it">Giovanni Ravazzani</a> 
!! @version 0.9 - 17th Jun 2011  
MODULE GridLib         
			
! History:  
!  
! Version   Date                Comment 
! -------       ----                    ------- 
!  0.1        02/Nov/2007   Original code. giovanni ravazzani
!  0.2        30/Nov/2008   extract grid of specified date
!  0.3        10/Dec/2008   export grid to netcdf dataset
!  0.4        13/Dec/2008   Add support to read ESRI GRID
!  0.5        18/May/2009   Add support to grid_mapping (makes use of GeoLib)
!  0.6        03/Jul/2009   export to file (ESRI GRID)
!  0.7        07/Apr/2010   added GetDtGrid
!  0.8        19/May/2011   increased decimal digits in header of exported esri grid
!  0.9        17/Jun/2011   add support to read netcdf file from WRF
! 
! Modules used: 
! 
USE netcdf , ONLY : & 
! Imported Parameters: 
NF90_NOWRITE, nf90_noerr, NF90_CLOBBER, &
NF90_FLOAT, NF90_GLOBAL, NF90_WRITE,  &
NF90_UNLIMITED, NF90_INT, NF90_MAX_VAR_DIMS, &
! Imported Routines: 
nf90_open, nf90_close, nf90_inq_dimid, nf90_inq_varid, &
nf90_inquire_dimension, nf90_get_var, nf90_strerror, &
nf90_inquire, nf90_get_att, nf90_inquire_variable, &
nf90_create, nf90_def_dim, nf90_def_var, nf90_put_att, &
nf90_enddef, nf90_put_var, nf90_redef
         
USE DataTypeSizes ,ONLY: & 
! Imported Parameters:  
short,long,float,double

USE LogLib, ONLY : &          
! Imported Routines:
Catch

USE ErrorCodes, ONLY : &
! Imported parameters:
ncIOError, memAllocError, openFileError, genericIOError, &
unknownOption

USE Chronos, ONLY : &
! Imported type definitions:
DateTime , &
! Imported routines:
UtcNow, ToUtc, DateTimeIsDefault, &
ASSIGNMENT(=), OPERATOR(-), &
OPERATOR(+), &
!Imported parameters:
timeDefault, timeString

USE GeoLib, ONLY : &
! Imported type definitions:
CRS, &
! Imported parameters:
GEODETIC, UTM, GAUSS_BOAGA, TM, &
WGS84, ED50, ROME40, &
! Imported routines:
SetCRS, SetTransverseMercatorParameters, SetGeodeticParameters
         
! Declarations must be of the form: 
! [type]   [VariableName]      ! Description/ purpose of variable 
     
IMPLICIT NONE 
! Global (i.e. public) Declarations: 

! Global Type Definitions: 

!!spatial conventions in netCDF file:
!!
!!>      y
!!  yDim ^     N
!!       |
!!       |  W     E
!!       |
!!       |     S
!!     1 |---------> x
!!<       1        xDim
!!
!!index conventions: GRID(x,y)

!!spatial conventions used in GridLib:
!!
!!>      1        jdim
!!     1 |---------> j
!!       |     N
!!       |
!!       |  W     E
!!       |
!!       |     S
!! idim  v
!!<      i
!!
!!index conventions: GRID(i,j)

TYPE grid_real
  INTEGER (KIND = short) :: jdim            !!number of columns
  INTEGER (KIND = short) :: idim            !!number of rows
  CHARACTER (LEN = 300)  :: standard_name   !! CF 1.0 compliant standard name
  CHARACTER (LEN = 300)  :: var_name        !! name of the variable
  CHARACTER (LEN = 300)  :: long_name       !! long descriptive name
  CHARACTER (LEN = 300)  :: file_name       !! name of the file from which grid was read
  CHARACTER (LEN = 30)   :: units           !!UDUNITS compliant measure units
  CHARACTER (LEN = 20)   :: varying_mode    !!mode to vary: sequence, linear
  REAL (KIND = float)    :: nodata          !!scalar identifying missing value
  REAL (KIND = float)    :: valid_min       !!minimum valid value
  REAL (KIND = float)    :: valid_max       !!maximum valid value
  TYPE (DateTime)        :: reference_time  !!reference time from which calculate current
  TYPE (DateTime)        :: current_time    !!current date and time of the grid in memory
  TYPE (DateTime)        :: next_time       !!time when next update is required
  INTEGER (KIND = short) :: time_index      !!position of grid in time dimension in netcdf file
  CHARACTER (LEN = 7)    :: time_unit       !!define time unit. Accepted values are: 
                                            !!seconds, second, sec, s
                                            !!minutes, minute, min
                                            !!hours, hour, hr, h
                                            !!days, day, d
  REAL (KIND = float), ALLOCATABLE :: mat(:,:) !!data variable contained in grid
  !!georeferencing informations
  REAL (KIND = float)    :: cellsize  
  REAL (KIND = float)    :: xllcorner
  REAL (KIND = float)    :: yllcorner 
  CHARACTER (LEN = 1000) :: esri_pe_string !!used by ArcMap 9.2
  TYPE (CRS)             :: grid_mapping !!Coordinate reference System
END TYPE grid_real

TYPE grid_integer
  INTEGER (KIND = short) :: jdim            !!number of columns
  INTEGER (KIND = short) :: idim            !!number of rows
  CHARACTER (LEN = 300)  :: standard_name   !! CF 1.0 compliant standard name
  CHARACTER (LEN = 300)  :: var_name        !! name of the variable
  CHARACTER (LEN = 300)  :: long_name       !! long descriptive name
  CHARACTER (LEN = 300)  :: file_name       !! name of the file from which grid was read
  CHARACTER (LEN = 30)   :: units           !!UDUNITS compliant measure units
  CHARACTER (LEN = 20)   :: varying_mode    !!mode to vary: steady, sequence, linear
  INTEGER (KIND = long)  :: nodata   !!scalar identifying missing value
  INTEGER (KIND = long)  :: valid_min       !!minimum valid value
  INTEGER (KIND = long)  :: valid_max       !!maximum valid value
  TYPE (DateTime)        :: reference_time  !!reference time from which calculate current
  TYPE (DateTime)        :: current_time    !!current date and time of the grid in memory
  TYPE (DateTime)        :: next_time       !!time when next update is required
  INTEGER (KIND = short) :: time_index      !!position of grid in time dimension in netcdf file
  CHARACTER (LEN = 7)    :: time_unit       !!define time unit. Accepted values are: 
                                            !!seconds, second, sec, s
                                            !!minutes, minute, min
                                            !!hours, hour, hr, h
                                            !!days, day, d
  INTEGER (KIND = long), ALLOCATABLE   :: mat(:,:)!!data contained in grid
  !!georeferencing informations
  REAL (KIND = float)    :: cellsize  
  REAL (KIND = float)    :: xllcorner
  REAL (KIND = float)    :: yllcorner 
  CHARACTER (LEN = 1000) :: esri_pe_string !!used by ArcMap 9.2
  TYPE (CRS)             :: grid_mapping !!Coordinate reference System
END TYPE grid_integer

! Global Parameters: 
INTEGER (KIND = short), PARAMETER :: ESRI_ASCII = 1
INTEGER (KIND = short), PARAMETER :: ESRI_BINARY = 2
INTEGER (KIND = short), PARAMETER :: NET_CDF = 3
         
! Global Scalars: 
         
! Global Arrays: 

!Global Procedures:
PUBLIC :: Newgrid
PUBLIC :: ExportGrid
PUBLIC :: GridDestroy
PUBLIC :: SetStandardName
PUBLIC :: SetLongName
PUBLIC :: SetUnits
PUBLIC :: SetVaryingMode
PUBLIC :: SetReferenceTime
PUBLIC :: SetCurrentTime
PUBLIC :: SetEsriPeString
PUBLIC :: GetGridMapping
PUBLIC :: GetDtGrid
         
! Local (i.e. private) Declarations: 
! Local Procedures:
PRIVATE :: ParseTime
PRIVATE :: TimeIndex
PRIVATE :: NewGridFloatFromNetCDF
PRIVATE :: NewGridIntegerFromNetCDF
PRIVATE :: NewGridFloatFromFile
PRIVATE :: NewGridFloatFromESRI_ASCII
PRIVATE :: NewGridFloatFromESRI_BINARY
PRIVATE :: NewGridIntegerFromFile
PRIVATE :: NewGridIntegerFromESRI_ASCII
PRIVATE :: NewGridIntegerFromESRI_BINARY
PRIVATE :: NewGridFloatAsGridFloat
PRIVATE :: NewGridFloatAsGridInteger
PRIVATE :: NewGridIntegerAsGridInteger
PRIVATE :: NewGridIntegerAsGridFloat
PRIVATE :: GridDestroyFloat
PRIVATE :: GridDestroyInteger
PRIVATE :: GetXYSizesFromFile
PRIVATE :: GetGeoreferenceFromNCdataSet
PRIVATE :: ExportGridFloatToNetCDF
PRIVATE :: ExportGridFloatToFile
PRIVATE :: ExportGridFloatToESRI_ASCII
PRIVATE :: ExportGridFloatToESRI_BINARY
PRIVATE :: ExportGridIntegerToFile
PRIVATE :: ExportGridIntegerToESRI_ASCII
PRIVATE :: ExportGridIntegerToESRI_BINARY
PRIVATE :: SetStandardNameFloat
PRIVATE :: SetStandardNameInteger
PRIVATE :: SetLongNameFloat
PRIVATE :: SetLongNameInteger
PRIVATE :: SetUnitsFloat
PRIVATE :: SetUnitsInteger
PRIVATE :: SetVaryingModeFloat
PRIVATE :: SetVaryingModeInteger
PRIVATE :: SetReferenceTimeFloat
PRIVATE :: SetReferenceTimeInteger
PRIVATE :: SetCurrentTimeFloat
PRIVATE :: SetCurrentTimeInteger
PRIVATE :: SetEsriPeStringFloat
PRIVATE :: SetEsriPeStringInteger
PRIVATE :: SwapGridRealForward
PRIVATE :: SwapGridIntegerForward
PRIVATE :: SwapGridRealBack
PRIVATE :: SwapGridIntegerBack
PRIVATE :: NextTime



! Local Type Definitions: 
         
! Local Parameters: 

INTEGER (KIND = short), PRIVATE, PARAMETER :: MISSING_DEF_INT = -9999
REAL (KIND = float), PRIVATE, PARAMETER :: MISSING_DEF_REAL = -9999.9
         
! Local Scalars: 
         
! Local Arrays: 
         
! Operator definitions:
! Define new operators or overload existing ones.
INTERFACE NewGrid
   !MODULE PROCEDURE NewGridFloatFromNetCDF
   !MODULE PROCEDURE NewGridIntegerFromNetCDF
   MODULE PROCEDURE NewGridFloatFromFile
   MODULE PROCEDURE NewGridIntegerFromFile
   MODULE PROCEDURE NewGridFloatAsGridFloat
   MODULE PROCEDURE NewGridFloatAsGridInteger
   MODULE PROCEDURE NewGridIntegerAsGridInteger
   MODULE PROCEDURE NewGridIntegerAsGridFloat
END INTERFACE

INTERFACE ExportGrid
  MODULE PROCEDURE ExportGridFloatToNetCDF
  MODULE PROCEDURE ExportGridIntegerToNetCDF
  MODULE PROCEDURE ExportGridFloatToFile
  MODULE PROCEDURE ExportGridIntegerToFile
END INTERFACE

INTERFACE GridDestroy
  MODULE PROCEDURE GridDestroyFloat
  MODULE PROCEDURE GridDestroyInteger
END INTERFACE

INTERFACE SetStandardName
  MODULE PROCEDURE SetStandardNameFloat
  MODULE PROCEDURE SetStandardNameInteger
END INTERFACE

INTERFACE SetLongName
  MODULE PROCEDURE SetLongNameFloat
  MODULE PROCEDURE SetLongNameInteger
END INTERFACE

INTERFACE SetUnits
  MODULE PROCEDURE SetUnitsFloat
  MODULE PROCEDURE SetUnitsInteger
END INTERFACE

INTERFACE SetVaryingMode
  MODULE PROCEDURE SetVaryingModeFloat
  MODULE PROCEDURE SetVaryingModeInteger
END INTERFACE

INTERFACE SetReferenceTime
  MODULE PROCEDURE SetReferenceTimeFloat
  MODULE PROCEDURE SetReferenceTimeInteger
END INTERFACE

INTERFACE SetCurrentTime
  MODULE PROCEDURE SetCurrentTimeFloat
  MODULE PROCEDURE SetCurrentTimeInteger
END INTERFACE

INTERFACE SetEsriPeString
  MODULE PROCEDURE SetEsriPeStringFloat
  MODULE PROCEDURE SetEsriPeStringInteger
END INTERFACE

INTERFACE GetGridMapping
  MODULE PROCEDURE GetGridMappingFloat
  MODULE PROCEDURE GetGridMappingInteger
END INTERFACE


!=======         
CONTAINS
!======= 
! Define procedures contained in this module. 

!==============================================================================
!! Description:
!!  create a new raster_real reading data from NetCDF file
!!  The variable to read can be defined by its current name or 
!!  the standard_name. The dimensions x and y of the variable
!!  is calculated searching from the dimensions of the couple of 
!!  variables with 'standard_name' equal to 'projection_x_coordinate'
!!  and 'projection_y_coordinate' for projected reference systems
!!  or 'longitude' and 'latitude' for geographic reference systems
!!  or 'grid_longitude' and 'grid_latitude' for rotated pole systems
!!  If a comprehensible reference systems is not found a geodetic
!!  reference system is supposed.
!!  Once the variable is retrieved, offset and scale factor are applied
!!  and a check on minimum and maximum valid value is performed.
SUBROUTINE NewGridFloatFromNetCDF &
!
(layer, fileName, variable, stdName, time)

USE StringManipulation, ONLY: &
!Imported routines:
StringCompact


IMPLICIT NONE

! Arguments with intent(in):
CHARACTER (LEN = *), INTENT(in) :: fileName  !!NetCDF file to be read
CHARACTER (LEN = *), OPTIONAL, INTENT(in) :: variable  !!variable  to read
CHARACTER (LEN = *), OPTIONAL, INTENT(in) :: stdName  !!standard name of 
                                                      !!the variable  to read
TYPE (DateTime), OPTIONAL, INTENT(in) :: time  !!time of the grid to read

! Arguments with intent(out):
TYPE (grid_real), INTENT (out)            :: layer  !!gridreal to return


! Local scalars:
INTEGER (KIND = short)          :: ios !!error return code
INTEGER (KIND = short)          :: ncStatus !!error code returned by NetCDF routines
INTEGER (KIND = short)          :: ncId  !!NetCdf Id for the file
INTEGER (KIND = short)          :: varId !!variable Id
INTEGER (KIND = short)          :: nVars !!number of variables
CHARACTER (LEN = 80)            :: attribute
INTEGER (KIND = short)          :: nDimsVar !!number of dimensions of a variable
INTEGER (KIND = short)          :: i, j   !!loop index
CHARACTER (LEN = 100)           :: variableName
CHARACTER (LEN = 1)             :: shp
REAL (KIND = float)             :: scale_factor
REAL (KIND = float)             :: offset

! Local arrays:
INTEGER, ALLOCATABLE            :: slice (:)
REAL (KIND = float), ALLOCATABLE :: tempGrid (:,:) 

!------------end of declaration------------------------------------------------

!------------------------------------------------------------------------------
![1.0] open NetCDF dataset with read-only access
!------------------------------------------------------------------------------
ncStatus = nf90_open (fileName, NF90_NOWRITE, ncId)
IF (ncStatus /= nf90_noerr) THEN
  CALL Catch ('error', 'GridLib',        &
  TRIM (nf90_strerror (ncStatus) )//': ',  &
  code = ncIOError, argument = fileName )
ENDIF

!------------------------------------------------------------------------------
![2.0] get x and y size and allocate array
!------------------------------------------------------------------------------

CALL GetXYSizesFromFile (ncId, layer % jdim, layer % idim, shape = shp)

!allocate grid
ALLOCATE ( layer % mat (layer % idim, layer % jdim), STAT = ios )
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',  &
  'memory allocation ',  &
  code = memAllocError,argument = variable )
ENDIF

!allocate temporary grid
ALLOCATE ( tempGrid (layer % jdim, layer % idim), STAT = ios )
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',  &
  'memory allocation ',  &
  code = memAllocError,argument = variable )
ENDIF

!------------------------------------------------------------------------------
![3.0] get values
!------------------------------------------------------------------------------
IF ( PRESENT (variable) ) THEN
  ncStatus = nf90_inq_varid (ncId, variable, varId)
  CALL ncErrorHandler (ncStatus)
ELSE IF (PRESENT (stdName) ) THEN !search variable corresponding to standard name

  !inquire dataset to retrieve number of dimensions, variables 
  !and global attributes
  ncStatus = nf90_inquire(ncId,  nVariables = nVars )
                          
  CALL ncErrorHandler (ncStatus)
  
  DO i = 1, nVars
    attribute = ''
    ncStatus = nf90_get_att (ncId, varid = i, name = 'standard_name', &
                             values = attribute)
    IF (ncStatus == nf90_noerr) THEN !'standard_name' is found
      IF ( StringCompact(attribute(1:LEN_TRIM(attribute))) == stdName(1:LEN_TRIM(stdName)) )THEN
         varId = i
      END IF
    END IF
  END DO
END IF

!verify number of dimensions of variable to read
ncstatus = nf90_inquire_variable(ncId, varId, ndims = nDimsVar) 
CALL ncErrorHandler (ncStatus)

!If number of dimensions = 3 read info about time
IF (nDimsVar == 3) THEN
  ALLOCATE ( slice(3) )
  !Read time information
  CALL ParseTime (ncId, layer % reference_time, layer % time_unit)


  !Define current time slice (if present)
  IF (PRESENT (time) ) THEN
    slice = (/ 1, 1, 1 /)
     slice(3) = TimeIndex (ncId, layer % reference_time, &
                          layer % time_unit, time)

    layer % current_time = time
    layer % time_index = slice(3)
  ELSE !read the first grid
    slice = (/ 1, 1, 1 /)
    layer % time_index = 1
  ENDIF
  !define time of the next grid
  layer % next_time = NextTime (ncId, layer % reference_time, &
                                layer % time_unit, layer % time_index)                 
ELSE
   ALLOCATE ( slice(2) )
   slice = (/ 1, 1/)
   layer % current_time = timeDefault
   layer % next_time = timeDefault
END IF 
ncStatus = nf90_get_var (ncId, varId, tempGrid , start = slice)
CALL ncErrorHandler (ncStatus)

!transpose temporary matrix to grid specification
CALL SwapGridRealForward (tempGrid, layer % mat)

!deallocate temporary grid
DEALLOCATE (tempGrid)

!------------------------------------------------------------------------------
![4.0] get attributes
!------------------------------------------------------------------------------
ncStatus = nf90_get_att (ncId, varId, name = 'standard_name', &
                             values = layer % standard_name)
                            
ncStatus = nf90_get_att (ncId, varId, name = 'long_name', &
                             values = layer % long_name)                              
                                                       
ncStatus = nf90_get_att (ncId, varId, name = 'units', &
                             values = layer % units) 
                            
ncStatus = nf90_get_att (ncId, varId, name = 'varying_mode', &
                             values = layer % varying_mode)
!If attribute is not defined set to default                             
IF (ncStatus /= nf90_noerr) layer % varying_mode = 'sequence'                            
                               
ncStatus = nf90_get_att (ncId, varId, name = '_FillValue', &
                             values = layer % nodata)

!if _FillValue is not defined search for missing_value   
IF (ncStatus /= nf90_noerr)  THEN                         
  ncStatus = nf90_get_att (ncId, varId, name = 'missing_value', &
                             values = layer % nodata)
END IF   

 !if missing_value is not defined set to default
IF (ncStatus /= nf90_noerr)  THEN                         
  layer % nodata = MISSING_DEF_REAL
END IF   
   

                         
ncStatus = nf90_get_att (ncId, varId, name = 'valid_min', &
                             values = layer % valid_min) 
!If attribute is not defined set to default                             
IF (ncStatus /= nf90_noerr) layer % valid_min = layer % nodata 
                                                         
ncStatus = nf90_get_att (ncId, varId, name = 'valid_max', &
                             values = layer % valid_max) 
!If attribute is not defined set to default                             
IF (ncStatus /= nf90_noerr) layer % valid_max = layer % nodata
                                                        
ncStatus = nf90_get_att (ncId, varId, name = 'scale_factor', &
                             values = scale_factor)
!If attribute is not defined set to default                             
IF (ncStatus /= nf90_noerr) scale_factor = 1.0
                             
ncStatus = nf90_get_att (ncId, varId, name = 'add_offset', &
                             values = offset)
!If attribute is not defined set to default                             
IF (ncStatus /= nf90_noerr)  offset = 0.  

ncStatus = nf90_get_att (ncId, varId, name = 'esri_pe_string', &
                             values = layer % esri_pe_string)
!If attribute is not defined set to default                             
IF (ncStatus /= nf90_noerr) layer % esri_pe_string = ''                         

!------------------------------------------------------------------------------
![5.0] check values and apply scale factor and offset
!------------------------------------------------------------------------------
!retrieve name of variable
ncstatus = nf90_inquire_variable(ncId, varId, name = variableName)   
layer % var_name = variableName
                          
DO i = 1, layer % idim
  DO j = 1, layer % jdim 
    IF ( layer % mat (i,j) /= layer % nodata ) THEN
    
      !apply scale factor
      layer % mat (i,j) = layer % mat (i,j) * scale_factor
      
      !add offset
      layer % mat (i,j) = layer % mat (i,j) + offset
      
      !check lower bound
      IF ( layer % valid_min /= layer % nodata .AND. &
           layer % mat (i,j) < layer % valid_min ) THEN
        layer % mat (i,j) = layer % valid_min
        CALL Catch ('info', 'GridLib', 'corrected value exceeding &
         lower bound in variable: ', argument = variableName )
      END IF  
    
      !check upper bound
      IF ( layer % valid_max /= layer % nodata .AND. &
           layer % mat (i,j) > layer % valid_max ) THEN
        layer % mat (i,j) = layer % valid_max
        CALL Catch ('info', 'GridLib', 'corrected value exceeding &
         upper bound in variable: ', argument = variableName )
      END IF  
    END IF
  END DO
END DO 

!------------------------------------------------------------------------------
![6.0] set file name
!------------------------------------------------------------------------------
layer % file_name = fileName  

!------------------------------------------------------------------------------
![7.0] read georeferencing informations from netCDF file
!------------------------------------------------------------------------------

CALL GetGeoreferenceFromNCdataSet (ncId, varId, layer % cellsize, &
                    layer % xllcorner, layer % yllcorner, layer % grid_mapping)
!------------------------------------------------------------------------------
![8.0] close NetCDF dataset
!------------------------------------------------------------------------------
ncStatus = nf90_close (ncid)
CALL ncErrorHandler (ncStatus)

END SUBROUTINE NewGridFloatFromNetCDF

!==============================================================================
!! Description:
!!  create a new grid_integer reading data from NetCDF file
!!  The variable to read can be defined by its current name or 
!!  the standard_name. The dimensions x and j of the variable
!!  is calculated searching from the dimensions of the couple of 
!!  variables with 'standard_name' equal to 'projection_x_coordinate'
!!  and 'projection_y_coordinate' for projected reference systems
!!  or 'longitude' and 'latitude' for geographic reference systems
!!  or 'grid_longitude' and 'grid_latitude' for rotated pole systems
!!  If a comprehensible reference systems is not found a geodetic
!!  reference system is supposed.
!!  Once the variable is retrieved, offset and scale factor are applied
!!  and a check on minimum and maximum valid value is performed.
SUBROUTINE NewGridIntegerFromNetCDF &
!
(layer, fileName, variable, stdName, time)

USE StringManipulation, ONLY: &
!Imported routines:
StringCompact


IMPLICIT NONE

! Scalar arguments with intent(in):
CHARACTER (LEN = *), INTENT(in) :: fileName  !!NetCDF file to read
CHARACTER (LEN = *), OPTIONAL, INTENT(in) :: variable  !!variable  to read
CHARACTER (LEN = *), OPTIONAL, INTENT(in) :: stdName  !!standard name of 
                                                      !!the variable  to read
TYPE (DateTime), OPTIONAL, INTENT(in) :: time  !!time of the grid to read

! Arguments with intent(out):
TYPE (grid_integer), INTENT (out)          :: layer  !!gridreal to return

! Local scalars:
INTEGER (KIND = short)          :: ios !!error return code
INTEGER (KIND = short)          :: ncStatus !!error code return by NetCDF routines
INTEGER (KIND = short)          :: ncId  !!NetCdf Id for the file
INTEGER (KIND = short)          :: varId !!variable Id
INTEGER (KIND = short)          :: nVars !!number of variables
CHARACTER (LEN = 80)            :: attribute
INTEGER (KIND = short)          :: nDimsVar !!number of dimensions of a variable
INTEGER (KIND = short)          :: i, j   !!loop index
CHARACTER (LEN = 100)           :: variableName
CHARACTER (LEN = 1)             :: shp
INTEGER (KIND = long)           :: scale_factor
INTEGER (KIND = long)           :: offset


! Local arrays:
INTEGER, ALLOCATABLE            :: slice (:)
INTEGER (KIND = long), ALLOCATABLE  :: tempGrid (:,:) 

!------------end of declaration------------------------------------------------

!------------------------------------------------------------------------------
![1.0] open NetCDF dataset with read-only access
!------------------------------------------------------------------------------
ncStatus = nf90_open (fileName, NF90_NOWRITE, ncId)
IF (ncStatus /= nf90_noerr) THEN
  CALL Catch ('error', 'GridLib',        &
  TRIM (nf90_strerror (ncStatus) )//': ',  &
  code = ncIOError, argument = fileName )
ENDIF

!------------------------------------------------------------------------------
![2.0] get x and y size and allocate array
!------------------------------------------------------------------------------

CALL GetXYSizesFromFile (ncId, layer % jdim, layer % idim, shape = shp)

!allocate grid
ALLOCATE ( layer % mat (layer % idim, layer % jdim), STAT = ios )
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',  &
  'memory allocation ',  &
  code = memAllocError,argument = variable )
ENDIF

!allocate temporary grid
ALLOCATE ( tempGrid (layer % jdim, layer % idim), STAT = ios )
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',  &
  'memory allocation ',  &
  code = memAllocError,argument = variable )
ENDIF

!------------------------------------------------------------------------------
![3.0] get values
!------------------------------------------------------------------------------
IF ( PRESENT (variable) ) THEN
  ncStatus = nf90_inq_varid (ncId, variable, varId)
  CALL ncErrorHandler (ncStatus)
ELSE IF (PRESENT (stdName) ) THEN !search variable corresponding to standard name

  !inquire dataset to retrieve number of dimensions, variables 
  !and global attributes
  ncStatus = nf90_inquire(ncId,  nVariables = nVars )
                          
  CALL ncErrorHandler (ncStatus)
  
  DO i = 1, nVars
    attribute = ''
    ncStatus = nf90_get_att (ncId, varid = i, name = 'standard_name', &
                             values = attribute)
    IF (ncStatus == nf90_noerr) THEN !error is raised if 'standard_name' is not found
      IF ( StringCompact(attribute(1:LEN_TRIM(attribute))) == stdName(1:LEN_TRIM(stdName)) )THEN
         varId = i
      END IF
    END IF
  END DO
END IF

!verify number of dimensions of variable to read
ncstatus = nf90_inquire_variable(ncId, varId, ndims = nDimsVar) 
CALL ncErrorHandler (ncStatus)

!If number of dimensions = 3 read info about time
IF (nDimsVar == 3) THEN
  ALLOCATE ( slice(3) )
  !Read time informations
  CALL ParseTime (ncId, layer % reference_time, layer % time_unit)

  !Define current time slice (if present)
  IF (PRESENT (time) ) THEN
    slice = (/ 1, 1, 1 /)
    slice(3) = TimeIndex (ncId, layer % reference_time, &
                          layer % time_unit, time)
    layer % current_time = time
    layer % time_index = slice(3)
  ELSE
    slice = (/ 1, 1, 1 /)
  ENDIF
ELSE
   ALLOCATE ( slice(2) )
   slice = (/ 1, 1/)
END IF 
ncStatus = nf90_get_var (ncId, varId, tempGrid , start = slice)
CALL ncErrorHandler (ncStatus)

!transpose temporary matrix to grid specification
CALL SwapGridIntegerForward (tempGrid, layer % mat)

!deallocate temporary grid
DEALLOCATE (tempGrid)

!------------------------------------------------------------------------------
![4.0] get attributes
!------------------------------------------------------------------------------

ncStatus = nf90_get_att (ncId, varId, name = 'standard_name', &
                             values = layer % standard_name)
                            
ncStatus = nf90_get_att (ncId, varId, name = 'long_name', &
                             values = layer % long_name)                              
                                                       
ncStatus = nf90_get_att (ncId, varId, name = 'units', &
                             values = layer % units) 
                            
ncStatus = nf90_get_att (ncId, varId, name = 'varying_mode', &
                             values = layer % varying_mode)
!If attribute is not defined set to default                             
IF (ncStatus /= nf90_noerr) layer % varying_mode = 'sequence'                            
                               
ncStatus = nf90_get_att (ncId, varId, name = '_FillValue', &
                             values = layer % nodata)

!if _FillValue is not defined search for missing_value   
IF (ncStatus /= nf90_noerr)  THEN                         
  ncStatus = nf90_get_att (ncId, varId, name = 'missing_value', &
                             values = layer % nodata)
END IF   

 !if missing_value is not defined set to default
IF (ncStatus /= nf90_noerr)  THEN                         
  layer % nodata = MISSING_DEF_REAL
END IF   
                             
ncStatus = nf90_get_att (ncId, varId, name = 'valid_min', &
                             values = layer % valid_min) 
!If attribute is not defined set to default                             
IF (ncStatus /= nf90_noerr) layer % valid_min = layer % nodata 
                                                         
ncStatus = nf90_get_att (ncId, varId, name = 'valid_max', &
                             values = layer % valid_max) 
!If attribute is not defined set to default                             
IF (ncStatus /= nf90_noerr) layer % valid_max = layer % nodata
                                                        
ncStatus = nf90_get_att (ncId, varId, name = 'scale_factor', &
                             values = scale_factor)
!If attribute is not defined set to default                             
IF (ncStatus /= nf90_noerr) scale_factor = 1
                             
ncStatus = nf90_get_att (ncId, varId, name = 'add_offset', &
                             values = offset)
!If attribute is not defined set to default                             
IF (ncStatus /= nf90_noerr) offset = 0  

ncStatus = nf90_get_att (ncId, varId, name = 'esri_pe_string', &
                             values = layer % esri_pe_string)
!If attribute is not defined set to default                             
IF (ncStatus /= nf90_noerr) layer % esri_pe_string = ''                         

!------------------------------------------------------------------------------
![5.0] check values and apply scale factor and offset
!------------------------------------------------------------------------------
!retrieve name of variable
ncstatus = nf90_inquire_variable(ncId, varId, name = variableName) 
layer % var_name = variableName
                              
DO i = 1, layer % jdim
  DO j = 1, layer % idim 
    IF ( layer % mat (i,j) /= layer % nodata ) THEN
    
      !apply scale factor
      layer % mat (i,j) = layer % mat (i,j) * scale_factor
      
      !add offset
      layer % mat (i,j) = layer % mat (i,j) + offset
      
      !check lower bound
      IF ( layer % valid_min /= layer % nodata .AND. &
           layer % mat (i,j) < layer % valid_min ) THEN
        layer % mat (i,j) = layer % valid_min
        CALL Catch ('info', 'GridLib', 'corrected value exceeding &
         lower bound in variable: ', argument = variableName )
      END IF  
    
      !check upper bound
      IF ( layer % valid_max /= layer % nodata .AND. &
           layer % mat (i,j) > layer % valid_max ) THEN
        layer % mat (i,j) = layer % valid_max
        CALL Catch ('info', 'GridLib', 'corrected value exceeding &
         upper bound in variable: ', argument = variableName )
      END IF  
    END IF
  END DO
END DO   

!------------------------------------------------------------------------------
![6.0] set file name
!------------------------------------------------------------------------------
layer % file_name = fileName

!------------------------------------------------------------------------------
![7.0] read georeferencing informations from netCDF file
!------------------------------------------------------------------------------

CALL GetGeoreferenceFromNCdataSet (ncId, varId, layer % cellsize, &
                    layer % xllcorner, layer % yllcorner, layer % grid_mapping)
                                                                                                                             
!------------------------------------------------------------------------------
![8.0] close NetCDF dataset
!------------------------------------------------------------------------------
ncStatus = nf90_close (ncid)
CALL ncErrorHandler (ncStatus)

END SUBROUTINE NewGridIntegerFromNetCDF


!==============================================================================
!! Description:
!!  read a grid from a file.
!!  List of supported format:
!!  ESRI_ASCII: ESRI ASCII GRID
!!  ESRI_BINARY: ESRI BINARY GRID 
!!  NET_CDF: NetCDF CF compliant
SUBROUTINE NewGridFloatFromFile &
!
(layer, fileName, fileFormat, variable, stdName, time)

IMPLICIT NONE

!Arguments with intent(in):
CHARACTER (LEN = *), INTENT(IN) :: fileName  !! file to read
INTEGER (KIND = short), INTENT(IN) :: fileFormat  !! format of the file to read
CHARACTER (LEN = *), OPTIONAL, INTENT(in) :: variable  !!variable  to read
CHARACTER (LEN = *), OPTIONAL, INTENT(in) :: stdName  !!standard name of 
                                                      !!the variable  to read
TYPE (DateTime), OPTIONAL, INTENT(in) :: time  !!time of the grid to read

!Arguments with intent(out):
TYPE (grid_real), INTENT(OUT)     :: layer  !!gridreal to return

!Local variables:

!------------end of declaration------------------------------------------------

IF ( fileformat == ESRI_ASCII ) THEN
  CALL NewGridFloatFromESRI_ASCII (fileName, layer)
ELSE IF ( fileformat == ESRI_BINARY ) THEN
  CALL NewGridFloatFromESRI_BINARY (fileName, layer)
ELSE IF ( fileformat == NET_CDF ) THEN
  IF (PRESENT(stdName)) THEN
    IF (PRESENT (time)) THEN
      CALL NewGridFloatFromNetCDF (layer, fileName, stdName = stdName, time = time)
    ELSE
      CALL NewGridFloatFromNetCDF (layer, fileName, stdName= stdName)
    END IF
  ELSE IF (PRESENT(variable)) THEN
    IF (PRESENT (time)) THEN
      CALL NewGridFloatFromNetCDF (layer, fileName, variable = variable, time = time)
    ELSE
      CALL NewGridFloatFromNetCDF (layer, fileName, variable = variable)
    END IF
  END IF
ELSE
  CALL Catch ('error', 'GridLib',  &
               'unknown option in reading file grid: ',  &
               code = unknownOption, argument = fileName )
END IF

END SUBROUTINE NewGridFloatFromFile

!==============================================================================
!! Description:
!!  read a float grid from a ESRI ASCII file.
SUBROUTINE NewGridFloatFromESRI_ASCII &
!
(fileName, layer)

USE Utilities, ONLY: &
!Imported routines:
GetUnit

IMPLICIT NONE

!Arguments with intent(in):
CHARACTER (LEN = *), INTENT(in) :: fileName  !! file to read

!Arguments with intent(out)
TYPE (grid_real), INTENT (OUT) :: layer !!returned grid float

!Local variables:
INTEGER (KIND = short)          :: fileUnit
INTEGER (KIND = short)          :: ios
CHARACTER (LEN = 50)            :: dummy
INTEGER (KIND = short)          :: i, j
!------------end of declaration------------------------------------------------

!open file
fileUnit = GetUnit ()
OPEN (UNIT = fileUnit, file = fileName, STATUS = 'OLD', IOSTAT = ios)
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error opening file: ',    &
              code = openFileError, argument = fileName )
END IF

!read number of columns
READ (fileUnit,*,IOSTAT = ios) dummy, layer % jdim
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading columns in file: ',    &
              code = genericIOError, argument = fileName )
END IF

!read number of rows
READ (fileUnit,*,IOSTAT = ios) dummy, layer % idim
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading rows in file: ',    &
              code = genericIOError, argument = fileName )
END IF

!read xll corner
READ (fileUnit,*,IOSTAT = ios) dummy, layer % xllcorner
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading xll in file: ',    &
              code = genericIOError, argument = fileName )
END IF

!read yll corner
READ (fileUnit,*,IOSTAT = ios) dummy, layer % yllcorner
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading yll in file: ',    &
              code = genericIOError, argument = fileName )
END IF

!read cellsize
READ (fileUnit,*,IOSTAT = ios) dummy, layer % cellsize
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading cellsize in file: ',    &
              code = genericIOError, argument = fileName )
END IF

!read nodata value
READ (fileUnit,*,IOSTAT = ios) dummy, layer % nodata
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading missing value in file: ',    &
              code = genericIOError, argument = fileName )
END IF

!allocate grid
ALLOCATE ( layer % mat (layer % idim, layer % jdim), STAT = ios )
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',  &
             'memory allocation ',  &
             code = memAllocError,argument = fileName )
ENDIF

!read data
DO i = 1,layer % idim
  READ (fileUnit,*) ( layer % mat (i,j) , j = 1,layer % jdim )
END DO

CLOSE (fileUnit)

!Set to default other fields
layer % standard_name = ''
layer % long_name = ''
layer % units = ''
layer % varying_mode = 'sequence' 
layer % valid_min = layer % nodata
layer % valid_max = layer % nodata
layer % esri_pe_string = ''

END SUBROUTINE NewGridFloatFromESRI_ASCII


!==============================================================================
!! Description:
!!  read a float grid from a ESRI BINARY file.
SUBROUTINE NewGridFloatFromESRI_BINARY &
!
(fileName, layer)

USE Utilities, ONLY: &
!Imported routines:
GetUnit

IMPLICIT NONE

!Arguments with intent(in):
CHARACTER (LEN = *), INTENT(in) :: fileName  !! file to read

!Arguments with intent(out)
TYPE (grid_real), INTENT (OUT) :: layer !!returned grid float

!Local variables:
INTEGER (KIND = short)          :: fileUnit
INTEGER (KIND = short)          :: ios
CHARACTER (LEN = 50)            :: dummy
INTEGER (KIND = short)          :: recordLength 
INTEGER (KIND = short)          :: recordNumber
INTEGER (KIND = short)          :: i, j
!------------end of declaration------------------------------------------------

!open file
fileUnit = GetUnit ()

OPEN (UNIT = fileUnit, file = TRIM(fileName) // '.hdr', &
      STATUS = 'OLD', IOSTAT = ios)
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error opening file: ',    &
              code = openFileError, argument = TRIM(fileName) // '.hdr' )
END IF

!read number of columns
READ (fileUnit,*,IOSTAT = ios) dummy, layer % jdim
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading columns in file: ',    &
              code = genericIOError, argument = TRIM(fileName) // '.hdr' )
END IF

!read number of rows
READ (fileUnit,*,IOSTAT = ios) dummy, layer % idim
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading rows in file: ',    &
              code = genericIOError, argument = TRIM(fileName) // '.hdr' )
END IF

!read xll corner
READ (fileUnit,*,IOSTAT = ios) dummy, layer % xllcorner
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading xll in file: ',    &
              code = genericIOError, argument = TRIM(fileName) // '.hdr' )
END IF

!read yll corner
READ (fileUnit,*,IOSTAT = ios) dummy, layer % yllcorner
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading yll in file: ',    &
              code = genericIOError, argument = TRIM(fileName) // '.hdr' )
END IF

!read cellsize
READ (fileUnit,*,IOSTAT = ios) dummy, layer % cellsize
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading cellsize in file: ',    &
              code = genericIOError, argument = TRIM(fileName) // '.hdr' )
END IF

!read nodata value
READ (fileUnit,*,IOSTAT = ios) dummy, layer % nodata
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading missing value in file: ',    &
              code = genericIOError, argument = TRIM(fileName) // '.hdr' )
END IF

!allocate grid
ALLOCATE ( layer % mat (layer % idim, layer % jdim), STAT = ios )
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',  &
             'memory allocation ',  &
             code = memAllocError,argument = fileName )
ENDIF

CLOSE (fileUnit)

fileUnit = GetUnit ()
INQUIRE (IOLENGTH = recordLength) 100_float

OPEN (UNIT = fileUnit, file = TRIM(fileName) // '.flt', &
      FORM = 'UNFORMATTED', ACCESS = 'DIRECT', RECL = recordLength, &
      STATUS = 'OLD', IOSTAT = ios)
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error opening file: ',    &
              code = openFileError, argument = TRIM(fileName) // '.flt' )
END IF

!read data
recordNumber = 0
DO i = 1,layer % idim
  DO j = 1, layer % jdim
    recordNumber = recordNumber + 1
    READ (fileUnit,REC = recordNumber) layer % mat (i,j)
  END DO
END DO

CLOSE (fileUnit)

!Set other fields to default
layer % standard_name = ''
layer % long_name = ''
layer % units = ''
layer % varying_mode = 'sequence' 
layer % valid_min = layer % nodata
layer % valid_max = layer % nodata
layer % esri_pe_string = ''

END SUBROUTINE NewGridFloatFromESRI_BINARY

!==============================================================================
!! Description:
!!  read a grid from a file.
!!  List of supported format:
!!  ESRI_ASCII: ESRI ASCII GRID
!!  ESRI_BINARY: ESRI BINARY GRID 
SUBROUTINE NewGridIntegerFromFile &
!
(layer, fileName, fileFormat, variable, stdName, time)

IMPLICIT NONE

!Arguments with intent(in):
CHARACTER (LEN = *), INTENT(IN) :: fileName  !! file to read
INTEGER (KIND = short), INTENT(IN) :: fileFormat  !! format of the file to read
CHARACTER (LEN = *), OPTIONAL, INTENT(in) :: variable  !!variable  to read
CHARACTER (LEN = *), OPTIONAL, INTENT(in) :: stdName  !!standard name of 
                                                      !!the variable  to read
TYPE (DateTime), OPTIONAL, INTENT(in) :: time  !!time of the grid to read


!Arguments with intent(out):
TYPE (grid_integer), INTENT(OUT)    :: layer  !!grid to be returned
!------------end of declaration------------------------------------------------

IF ( fileformat == ESRI_ASCII ) THEN
  CALL NewGridIntegerFromESRI_ASCII (fileName, layer)
ELSE IF ( fileformat == ESRI_BINARY ) THEN
  CALL NewGridIntegerFromESRI_BINARY (fileName, layer)
ELSE IF ( fileformat == NET_CDF ) THEN
  IF (PRESENT(stdName)) THEN
    IF (PRESENT (time)) THEN
      CALL NewGridIntegerFromNetCDF (layer, fileName, stdName = stdName, time = time)
    ELSE
      CALL NewGridIntegerFromNetCDF (layer, fileName, stdName= stdName)
    END IF
  ELSE IF (PRESENT(variable)) THEN
    IF (PRESENT (time)) THEN
      CALL NewGridIntegerFromNetCDF (layer, fileName, variable = variable, time = time)
    ELSE
      CALL NewGridIntegerFromNetCDF (layer, fileName, variable = variable)
    END IF
  END IF
ELSE

  CALL Catch ('error', 'GridLib',  &
               'unknown option in reading file grid: ',  &
               code = unknownOption, argument = fileName )
END IF

END SUBROUTINE NewGridIntegerFromFile

!==============================================================================
!! Description:
!!  read a integer grid from a ESRI ASCII file.
SUBROUTINE NewGridIntegerFromESRI_ASCII &
!
(fileName, layer)

USE Utilities, ONLY: &
!Imported routines:
GetUnit

IMPLICIT NONE

!Arguments with intent(in):
CHARACTER (LEN = *), INTENT(in) :: fileName  !! file to read

!Arguments with intent(out)
TYPE (grid_integer), INTENT (OUT) :: layer !!returned grid float

!Local variables:
INTEGER (KIND = short)          :: fileUnit
INTEGER (KIND = short)          :: ios
CHARACTER (LEN = 50)            :: dummy
INTEGER (KIND = short)          :: i, j
!------------end of declaration------------------------------------------------

!open file
fileUnit = GetUnit ()
OPEN (UNIT = fileUnit, file = fileName, STATUS = 'OLD', IOSTAT = ios)
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error opening file: ',    &
              code = openFileError, argument = fileName )
END IF

!read number of columns
READ (fileUnit,*,IOSTAT = ios) dummy, layer % jdim
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading columns in file: ',    &
              code = genericIOError, argument = fileName )
END IF

!read number of rows
READ (fileUnit,*,IOSTAT = ios) dummy, layer % idim
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading rows in file: ',    &
              code = genericIOError, argument = fileName )
END IF

!read xll corner
READ (fileUnit,*,IOSTAT = ios) dummy, layer % xllcorner
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading xll in file: ',    &
              code = genericIOError, argument = fileName )
END IF

!read yll corner
READ (fileUnit,*,IOSTAT = ios) dummy, layer % yllcorner
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading yll in file: ',    &
              code = genericIOError, argument = fileName )
END IF

!read cellsize
READ (fileUnit,*,IOSTAT = ios) dummy, layer % cellsize
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading cellsize in file: ',    &
              code = genericIOError, argument = fileName )
END IF

!read nodata value
READ (fileUnit,*,IOSTAT = ios) dummy, layer % nodata
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading missing value in file: ',    &
              code = genericIOError, argument = fileName )
END IF

!allocate grid
ALLOCATE ( layer % mat (layer % idim, layer % jdim), STAT = ios )
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',  &
             'memory allocation ',  &
             code = memAllocError,argument = fileName )
ENDIF

!read data
DO i = 1,layer%idim
  READ (fileUnit,*) ( layer % mat (i,j) , j=1,layer % jdim )
END DO

CLOSE (fileUnit)

!Set to default other fields
layer % standard_name = ''
layer % long_name = ''
layer % units = ''
layer % varying_mode = 'sequence' 
layer % valid_min = layer % nodata
layer % valid_max = layer % nodata
layer % esri_pe_string = ''

END SUBROUTINE NewGridIntegerFromESRI_ASCII


!==============================================================================
!! Description:
!!  read a integer grid from a ESRI BINARY file.
SUBROUTINE NewGridIntegerFromESRI_BINARY &
!
(fileName, layer)

USE Utilities, ONLY: &
!Imported routines:
GetUnit

IMPLICIT NONE

!Arguments with intent(in):
CHARACTER (LEN = *), INTENT(in) :: fileName  !! file to read

!Arguments with intent(out)
TYPE (grid_integer), INTENT (OUT) :: layer !!returned grid float

!Local variables:
INTEGER (KIND = short)          :: fileUnit
INTEGER (KIND = short)          :: ios
CHARACTER (LEN = 50)            :: dummy
INTEGER (KIND = short)          :: recordLength 
INTEGER (KIND = short)          :: recordNumber
INTEGER (KIND = short)          :: i, j
!------------end of declaration------------------------------------------------

!open file
fileUnit = GetUnit ()

OPEN (UNIT = fileUnit, file = TRIM(fileName) // '.hdr', &
      STATUS = 'OLD', IOSTAT = ios)
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error opening file: ',    &
              code = openFileError, argument = TRIM(fileName) // '.hdr' )
END IF

!read number of columns
READ (fileUnit,*,IOSTAT = ios) dummy, layer % jdim
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading columns in file: ',    &
              code = genericIOError, argument = TRIM(fileName) // '.hdr' )
END IF

!read number of rows
READ (fileUnit,*,IOSTAT = ios) dummy, layer % idim
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading rows in file: ',    &
              code = genericIOError, argument = TRIM(fileName) // '.hdr' )
END IF

!read xll corner
READ (fileUnit,*,IOSTAT = ios) dummy, layer % xllcorner
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading xll in file: ',    &
              code = genericIOError, argument = TRIM(fileName) // '.hdr' )
END IF

!read yll corner
READ (fileUnit,*,IOSTAT = ios) dummy, layer % yllcorner
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading yll in file: ',    &
              code = genericIOError, argument = TRIM(fileName) // '.hdr' )
END IF

!read cellsize
READ (fileUnit,*,IOSTAT = ios) dummy, layer % cellsize
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading cellsize in file: ',    &
              code = genericIOError, argument = TRIM(fileName) // '.hdr' )
END IF

!read nodata value
READ (fileUnit,*,IOSTAT = ios) dummy, layer % nodata
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error reading missing value in file: ',    &
              code = genericIOError, argument = TRIM(fileName) // '.hdr' )
END IF

!allocate grid
ALLOCATE ( layer % mat (layer % idim, layer % jdim), STAT = ios )
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',  &
             'memory allocation ',  &
             code = memAllocError,argument = fileName )
ENDIF

CLOSE (fileUnit)

fileUnit = GetUnit ()
INQUIRE (IOLENGTH = recordLength) 100_long

OPEN (UNIT = fileUnit, file = TRIM(fileName) // '.flt', &
      FORM = 'UNFORMATTED', ACCESS = 'DIRECT', RECL = recordLength, &
      STATUS = 'OLD', IOSTAT = ios)
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error opening file: ',    &
              code = openFileError, argument = TRIM(fileName) // '.flt' )
END IF

!read data
recordNumber = 0
DO i = 1,layer % idim
  DO j = 1, layer % jdim
    recordNumber = recordNumber + 1
    READ (fileUnit,REC = recordNumber) layer % mat (i,j)
  END DO
END DO

CLOSE (fileUnit)

!Set other fields to default
layer % standard_name = ''
layer % long_name = ''
layer % units = ''
layer % varying_mode = 'sequence' 
layer % valid_min = layer % nodata
layer % valid_max = layer % nodata
layer % esri_pe_string = ''

END SUBROUTINE NewGridIntegerFromESRI_BINARY


!==============================================================================
!! Description:
!!  create a new grid_real using an existing grid_real as template
SUBROUTINE NewGridFloatAsGridFloat &
!
(layer, grid, initial)

IMPLICIT NONE

!Arguments with intent(in):
TYPE (grid_real), INTENT(in) :: grid 
REAL (KIND = float), OPTIONAL, INTENT(in) :: initial

!Arguments with intent(out):
TYPE (grid_real), INTENT(OUT)              :: layer  !!grid to be returned

!Local variables:
INTEGER (KIND = short)          :: ios 
INTEGER (KIND = short)          :: i, j 
!------------end of declaration------------------------------------------------

layer % jdim = grid % jdim
layer % idim = grid % idim
layer % varying_mode =  'sequence' !default
layer % nodata = grid % nodata
layer % valid_min = layer % nodata
layer % valid_max = layer % nodata
layer % cellsize =  grid % cellsize
layer % xllcorner = grid % xllcorner
layer % yllcorner = grid % yllcorner
layer % esri_pe_string = grid % esri_pe_string 
layer % grid_mapping = grid % grid_mapping

ALLOCATE ( layer % mat ( layer % idim, layer % jdim ), STAT = ios )
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',  &
  'memory allocation ',  &
  code = memAllocError, argument = 'new grid as' )
ENDIF   

DO i = 1, layer % idim
  DO j = 1, layer % jdim
    IF ( grid % mat (i,j) == grid % nodata ) THEN
       layer % mat (i,j) = layer % nodata
    ELSE
       IF (PRESENT(initial)) THEN 
         layer % mat (i,j) = initial
       ELSE
         layer % mat (i,j) = 0.
       END IF
    END IF
  END DO
END DO              

END SUBROUTINE NewGridFloatAsGridFloat

!==============================================================================
!! Description:
!!  create a new grid_real using an existing grid_integer as template
SUBROUTINE NewGridFloatAsGridInteger &
!
(layer, grid, initial)

IMPLICIT NONE

!Arguments with intent(in):
TYPE (grid_integer), INTENT(in) :: grid 
REAL (KIND = float), OPTIONAL, INTENT(in) :: initial

!Arguments with intent(out):
TYPE (grid_real), INTENT(OUT)             :: layer  !!gridreal to return

!Local variables:
INTEGER (KIND = short)          :: ios 
INTEGER (KIND = short)          :: i, j 
!------------end of declaration------------------------------------------------

layer % jdim = grid % jdim
layer % idim = grid % idim
layer % varying_mode =  'sequence' !default
layer % nodata = MISSING_DEF_REAL
layer % valid_min = layer % nodata
layer % valid_max = layer % nodata
layer % cellsize =  grid % cellsize
layer % xllcorner = grid % xllcorner
layer % yllcorner = grid % yllcorner
layer % esri_pe_string = grid % esri_pe_string 
layer % grid_mapping = grid % grid_mapping

ALLOCATE ( layer % mat ( layer % idim, layer % jdim ), STAT = ios )
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',  &
  'memory allocation ',  &
  code = memAllocError, argument = 'new grid as' )
ENDIF   

DO i = 1, layer % idim
  DO j = 1, layer % jdim
    IF ( grid % mat (i,j) == grid % nodata ) THEN
       layer % mat (i,j) = layer % nodata
    ELSE
       IF (PRESENT(initial)) THEN 
         layer % mat (i,j) = initial
       ELSE
         layer % mat (i,j) = 0.
       END IF
    END IF
  END DO
END DO              

END SUBROUTINE NewGridFloatAsGridInteger


!==============================================================================
!! Description:
!!  create a new grid_integer using an existing grid_integer as template
SUBROUTINE NewGridIntegerAsGridInteger &
!
(layer, grid, initial)

IMPLICIT NONE

!Arguments with intent(in):
TYPE (grid_integer), INTENT(IN)    :: grid 
INTEGER, OPTIONAL, INTENT(IN)     :: initial

!Arguments with intent(out):
TYPE (grid_integer), INTENT(OUT)   :: layer  !!grid to be returned

!Local variables:
INTEGER (KIND = short)          :: ios 
INTEGER (KIND = short)          :: i, j 
!------------end of declaration------------------------------------------------

layer % jdim = grid % jdim
layer % idim = grid % idim
layer % varying_mode =  'sequence' !default
layer % nodata = grid % nodata
layer % valid_min = layer % nodata
layer % valid_max = layer % nodata
layer % cellsize =  grid % cellsize
layer % xllcorner = grid % xllcorner
layer % yllcorner = grid % yllcorner
layer % esri_pe_string = grid % esri_pe_string 
layer % grid_mapping = grid % grid_mapping

ALLOCATE ( layer % mat ( layer % idim, layer % jdim ), STAT = ios )
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',  &
  'memory allocation ',  &
  code = memAllocError, argument = 'new grid as' )
ENDIF   

DO i = 1, layer % idim
  DO j = 1, layer % jdim
    IF ( grid % mat (i,j) == grid % nodata ) THEN
       layer % mat (i,j) = layer % nodata
    ELSE
       IF (PRESENT(initial)) THEN 
         layer % mat (i,j) = initial
       ELSE
         layer % mat (i,j) = 0
       END IF
    END IF
  END DO
END DO              

END SUBROUTINE NewGridIntegerAsGridInteger


!==============================================================================
!! Description:
!!  create a new grid_integer using an existing grid_real as template
SUBROUTINE NewGridIntegerAsGridFloat &
!
(layer, grid, initial)

IMPLICIT NONE

!Arguments with intent(in):
TYPE (grid_real), INTENT(in)     :: grid 
INTEGER, OPTIONAL, INTENT(in)    :: initial

!Arguments with intent (out):
TYPE (grid_integer), INTENT(OUT)  :: layer  !!grid to be returned

!Local variables:
INTEGER (KIND = short)           :: ios 
INTEGER (KIND = short)           :: i, j 
!------------end of declaration------------------------------------------------

layer % jdim = grid % jdim
layer % idim = grid % idim
layer % varying_mode =  'sequence' !default
layer % nodata = MISSING_DEF_INT
layer % valid_min = layer % nodata
layer % valid_max = layer % nodata
layer % cellsize =  grid % cellsize
layer % xllcorner = grid % xllcorner
layer % yllcorner = grid % yllcorner
layer % esri_pe_string = grid % esri_pe_string 
layer % grid_mapping = grid % grid_mapping

ALLOCATE ( layer % mat ( layer % idim, layer % jdim ), STAT = ios )
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',  &
  'memory allocation ',  &
  code = memAllocError, argument = 'new grid as' )
ENDIF   

DO i = 1, layer % idim
  DO j = 1, layer % jdim
    IF ( grid % mat (i,j) == grid % nodata ) THEN
       layer % mat (i,j) = layer % nodata
    ELSE
       IF (PRESENT(initial)) THEN 
         layer % mat (i,j) = initial
       ELSE
         layer % mat (i,j) = 0
       END IF
    END IF
  END DO
END DO              

END SUBROUTINE NewGridIntegerAsGridFloat


!==============================================================================
!! Description:
!!  set the standard name of a float grid
SUBROUTINE SetStandardNameFloat &
!
(name, layer)

IMPLICIT NONE

!Arguments with intent(in):
CHARACTER (LEN = *), INTENT(IN) :: name

!Arguments with intent(out):
TYPE(grid_real), INTENT(INOUT) :: layer

!------------end of declaration------------------------------------------------

layer % standard_name = name

END SUBROUTINE SetStandardNameFloat

!==============================================================================
!! Description:
!!  set the standard name of a integer grid
SUBROUTINE SetStandardNameInteger &
!
(name, layer)

IMPLICIT NONE

!Arguments with intent(in):
CHARACTER (LEN = *), INTENT(IN) :: name

!Arguments with intent(out):
TYPE(grid_integer), INTENT(INOUT) :: layer

!------------end of declaration------------------------------------------------

layer % standard_name = name

END SUBROUTINE SetStandardNameInteger

!==============================================================================
!! Description:
!!  set the long name of a float grid
SUBROUTINE SetLongNameFloat &
!
(name, layer)

IMPLICIT NONE

!Arguments with intent(in):
CHARACTER (LEN = *), INTENT(IN) :: name

!Arguments with intent(out):
TYPE(grid_real), INTENT(INOUT) :: layer

!------------end of declaration------------------------------------------------

layer % long_name = name

END SUBROUTINE SetLongNameFloat

!==============================================================================
!! Description:
!!  set the long name of a integer grid
SUBROUTINE SetLongNameInteger &
!
(name, layer)

IMPLICIT NONE

!Arguments with intent(in):
CHARACTER (LEN = *), INTENT(IN) :: name

!Arguments with intent(out):
TYPE(grid_integer), INTENT(INOUT) :: layer

!------------end of declaration------------------------------------------------

layer % long_name = name

END SUBROUTINE SetLongNameInteger

!==============================================================================
!! Description:
!!  set the units of a float grid
SUBROUTINE SetUnitsFloat &
!
(units, layer)

IMPLICIT NONE

!Arguments with intent(in):
CHARACTER (LEN = *), INTENT(IN) :: units

!Arguments with intent(out):
TYPE(grid_real), INTENT(INOUT) :: layer

!------------end of declaration------------------------------------------------

layer % units = units

END SUBROUTINE SetUnitsFloat

!==============================================================================
!! Description:
!!  set the units of a integer grid
SUBROUTINE SetUnitsInteger &
!
(units, layer)

IMPLICIT NONE

!Arguments with intent(in):
CHARACTER (LEN = *), INTENT(IN) :: units

!Arguments with intent(out):
TYPE(grid_integer), INTENT(INOUT) :: layer

!------------end of declaration------------------------------------------------

layer % units = units

END SUBROUTINE SetUnitsInteger

!==============================================================================
!! Description:
!!  set the varying mode of a float grid
!!  Possible values:
!!  linear: linear variation between two dates
!!  sequence: new grid substitute the previous as frames in a movie
SUBROUTINE SetVaryingModeFloat &
!
(varMod, layer)

USE StringManipulation, ONLY: &
!Imported routines:
StringToLower, StringCompact

IMPLICIT NONE

!Arguments with intent(in):
CHARACTER (LEN = *), INTENT(IN) :: varMod

!Arguments with intent(out):
TYPE(grid_real), INTENT(INOUT) :: layer

!Local variables
CHARACTER (LEN = 20) :: string

!------------end of declaration------------------------------------------------

string = StringCompact (StringToLower  (varMod) )
IF ( String == 'sequence' .OR. String == 'linear' ) THEN  
  layer % varying_mode = string
ELSE
  CALL Catch ('error', 'GridLib', 'unsupported varying mode: ', &
                code = unknownOption, argument = string )
END IF

END SUBROUTINE SetVaryingModeFloat

!==============================================================================
!! Description:
!!  set the varying mode of a integer grid
SUBROUTINE SetVaryingModeInteger &
!
(varMod, layer)

USE StringManipulation, ONLY: &
!Imported routines:
StringToLower, StringCompact

IMPLICIT NONE

!Arguments with intent(in):
CHARACTER (LEN = *), INTENT(IN) :: varMod

!Arguments with intent(out):
TYPE(grid_integer), INTENT(INOUT) :: layer

!Local variables
CHARACTER (LEN = 20) :: string

!------------end of declaration------------------------------------------------

string = StringCompact (StringToLower  (varMod) )
IF ( String == 'sequence' .OR. String == 'linear' ) THEN  
  layer % varying_mode = string
ELSE
  CALL Catch ('error', 'GridLib', 'unsupported varying mode: ', &
                code = unknownOption, argument = string )
END IF

END SUBROUTINE SetVaryingModeInteger

!==============================================================================
!! Description:
!!  set the reference time of a float grid
SUBROUTINE SetReferenceTimeFloat &
!
(time, layer)

IMPLICIT NONE

!Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time

!Arguments with intent(out):
TYPE(grid_real), INTENT(INOUT) :: layer

!------------end of declaration------------------------------------------------

layer % reference_time = time

END SUBROUTINE SetReferenceTimeFloat

!==============================================================================
!! Description:
!!  set the reference time of a integer grid
SUBROUTINE SetReferenceTimeInteger &
!
(time, layer)

IMPLICIT NONE

!Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time

!Arguments with intent(out):
TYPE(grid_integer), INTENT(INOUT) :: layer

!------------end of declaration------------------------------------------------

layer % reference_time = time

END SUBROUTINE SetReferenceTimeInteger

!==============================================================================
!! Description:
!!  set the current time of a float grid
SUBROUTINE SetCurrentTimeFloat &
!
(time, layer)

IMPLICIT NONE

!Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time

!Arguments with intent(out):
TYPE(grid_real), INTENT(INOUT) :: layer

!------------end of declaration------------------------------------------------

layer % current_time = time

END SUBROUTINE SetCurrentTimeFloat

!==============================================================================
!! Description:
!!  set the current time of a integer grid
SUBROUTINE SetCurrentTimeInteger &
!
(time, layer)

IMPLICIT NONE

!Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time

!Arguments with intent(out):
TYPE(grid_integer), INTENT(INOUT) :: layer

!------------end of declaration------------------------------------------------

layer % current_time = time

END SUBROUTINE SetCurrentTimeInteger

!==============================================================================
!! Description:
!!  set the esri pe string of a float grid
SUBROUTINE SetEsriPeStringFloat &
!
(string, layer)

IMPLICIT NONE

!Arguments with intent(in):
CHARACTER (LEN = *), INTENT(IN) :: string

!Arguments with intent(out):
TYPE(grid_real), INTENT(INOUT) :: layer

!------------end of declaration------------------------------------------------

layer % esri_pe_string = string

END SUBROUTINE SetEsriPeStringFloat

!==============================================================================
!! Description:
!!  set the esri pe string of a integer grid
SUBROUTINE SetEsriPeStringInteger &
!
(string, layer)

IMPLICIT NONE

!Arguments with intent(in):
CHARACTER (LEN = *), INTENT(IN) :: string

!Arguments with intent(out):
TYPE(grid_integer), INTENT(INOUT) :: layer

!------------end of declaration------------------------------------------------

layer % esri_pe_string = string

END SUBROUTINE SetEsriPeStringInteger

!==============================================================================
!! Description:
!!  export grid into netcdf file. Two actions are possible:
!!  add: add a non-record variable to a netCDF dataset. If file does not exist
!!       it is created new. The added variable
!!       must have the same dimensions of already present variables.
!!       If you try to add a variable already present in netCDF dataset
!!       an error is returned.
!!  append: append more data to record variables along the unlimited dimension.
!!          If netCDF dataset is created new, dimensions and attributes
!!          are set, otherwise only new data are written. If current time
!!          is already present in netcdf dataset an error is returned
SUBROUTINE ExportGridFloatToNetCDF &
!
(grid, file, name, action)

USE StringManipulation, ONLY: &
! imported routines:
StringToUpper, ToString

IMPLICIT NONE

! Arguments with intent(in):
TYPE (grid_real), INTENT(IN) :: grid !! grid to be exported
CHARACTER (LEN = *), INTENT(IN) :: file !!netcdf file to export to
CHARACTER (LEN = *), INTENT(IN) :: name !!name of variable in netcdf
CHARACTER (LEN = *), INTENT(IN) :: action !!add or append

! Local variables:
INTEGER (KIND = short)  :: ncStatus !!error code return by NetCDF routines
INTEGER (KIND = short)  :: ncId  !!NetCdf Id for the file
INTEGER (KIND = short)  :: dimXid  !!id of x dimension
INTEGER (KIND = short)  :: dimYid  !!id of y dimension
INTEGER (KIND = short)  :: dimTid  !!id of time dimension
INTEGER (KIND = short)  :: varXid  !!id of x variable
INTEGER (KIND = short)  :: varYid  !!id of y variable
INTEGER (KIND = short)  :: varTid  !!id of time variable
INTEGER (KIND = short)  :: mapId   !!id of grid mapping variable
INTEGER (KIND = short), ALLOCATABLE  :: varDim (:)  !!define dimension of variable
INTEGER (KIND = short)  :: varId  !!id of grid variable
CHARACTER (LEN = 25)    :: string
REAL (KIND = float), ALLOCATABLE :: lats(:), lons(:) !!array containing coordinate
INTEGER (KIND = short)  :: i !!loop index
LOGICAL :: fileExist    !!true if netcdf exists
LOGICAL :: varExist     !!true if variable exists in dataset 
TYPE (dateTime) :: ref_time !!reference time to calculate time span
CHARACTER (LEN = 7) :: dummyString 
REAL (KIND = float) :: timeSpan !!time between current time and reference time
INTEGER (KIND = short) :: records !!number of records stored in netcdf dataset
INTEGER (KIND = short) :: slice (3) !!used to write new record in right position
INTEGER (KIND = short) :: timeRecord (1) !!to write new time record
REAL (KIND = float), ALLOCATABLE :: times (:) !!values stored in time variable
LOGICAL :: timeExists
REAL (KIND = float), ALLOCATABLE :: tempGrid (:,:)

!------------end of declaration------------------------------------------------

!verify if file exists
ncStatus = nf90_open (file, NF90_WRITE, ncId)
IF (ncStatus == nf90_noerr) THEN
  fileExist = .TRUE.
ELSE
  fileExist = .FALSE.
END IF 

!if file does not exist create the file
IF (.NOT. fileExist) THEN
  ncStatus = nf90_create ( file, NF90_CLOBBER, ncId )
  CALL ncErrorHandler (ncStatus)
  
  !define dimensions
  ncStatus = nf90_def_dim ( ncId, 'x', grid % jdim, dimXid )
  CALL ncErrorHandler (ncStatus)
  ncStatus = nf90_def_dim ( ncId, 'y', grid % idim, dimYid )
  CALL ncErrorHandler (ncStatus)
  !if record (unlimited) variable define time dimension
  IF ( StringToUpper(action) ==  'APPEND' ) THEN
    ncStatus = nf90_def_dim ( ncId, 'time', NF90_UNLIMITED, dimTid )
    CALL ncErrorHandler (ncStatus) 
  END IF
  
  !define coordinate variables
  ncStatus = nf90_def_var (ncId, 'x', NF90_FLOAT, dimXid, varXid)
  CALL ncErrorHandler (ncStatus)
  ncStatus = nf90_def_var (ncId, 'y', NF90_FLOAT, dimYid, varYid)
  CALL ncErrorHandler (ncStatus)
  !if record (unlimited) variable define time variable
  IF ( StringToUpper(action) ==  'APPEND' ) THEN
    ncStatus = nf90_def_var ( ncId, 'time', NF90_FLOAT, dimTid, varTid )
    CALL ncErrorHandler (ncStatus) 
  END IF
  
  !assign attributes to coordinate variables
  IF (grid % grid_mapping % system == GEODETIC) THEN
      ncStatus = nf90_put_att (ncId, varXid, 'long_name', 'longitude (centre of grid cell)')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varXid, 'standard_name', 'longitude')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varXid, 'units', 'deg')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varXid, 'axis', 'X')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varYid, 'long_name', 'latitude (centre of grid cell)')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varYid, 'standard_name', 'latitude')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varYid, 'units', 'deg')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varYid, 'axis', 'Y')
      CALL ncErrorHandler (ncStatus)
  ELSE 
      ncStatus = nf90_put_att (ncId, varXid, 'long_name', 'x coordinate of projection')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varXid, 'standard_name', 'projection_x_coordinate')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varXid, 'units', 'm')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varXid, 'axis', 'X')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varYid, 'long_name', 'y coordinate of projection')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varYid, 'standard_name', 'projection_y_coordinate')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varYid, 'units', 'm')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varYid, 'axis', 'Y')
      CALL ncErrorHandler (ncStatus)
END IF

  !if record (unlimited) variable define attributes of time variable
  IF ( StringToUpper(action) ==  'APPEND' ) THEN
    ncStatus = nf90_put_att ( ncId, varTid, 'long_name', 'time' )
    CALL ncErrorHandler (ncStatus)
    ncStatus = nf90_put_att ( ncId, varTid, 'standard_name', 'time' )
    CALL ncErrorHandler (ncStatus)
    string = grid % current_time
    ncStatus = nf90_put_att ( ncId, varTid, 'units', 'seconds since ' // string )
    CALL ncErrorHandler (ncStatus) 
  END IF
  
  !define dummy grid mapping variable
  ncStatus = nf90_def_var (ncId, 'crs', NF90_INT, mapId)
  CALL ncErrorHandler (ncStatus)
  
  !assign attributes to grid mapping variable
  ncStatus = nf90_put_att (ncId, mapId, 'grid_mapping_name', &
                           grid % grid_mapping % name)
  CALL ncErrorHandler (ncStatus)
  DO i = 1, grid % grid_mapping % basic
    ncStatus = nf90_put_att (ncId, mapId, grid % grid_mapping % &
               description (i), grid % grid_mapping % param (i))
    CALL ncErrorHandler (ncStatus)
  END DO
  !datum
  ncStatus = nf90_put_att (ncId, mapId, 'datum_code', &
                           grid % grid_mapping % datum % code)
  CALL ncErrorHandler (ncStatus)
  
  
  !assign global attributes
  ncStatus = nf90_put_att (ncId, NF90_GLOBAL, 'Conventions', 'CF-1.0')
  CALL ncErrorHandler (ncStatus)
  string = UtcNow ()
  ncStatus = nf90_put_att (ncId, NF90_GLOBAL, 'history', 'creation date: ' // string )
  CALL ncErrorHandler (ncStatus)
  ncStatus = nf90_put_att (ncId, NF90_GLOBAL, 'institution', 'Politecnico di Milano' )
  CALL ncErrorHandler (ncStatus)

  !end define mode
  ncStatus = nf90_enddef (ncId)
  CALL ncErrorHandler (ncStatus)

END IF

!enter define mode
ncStatus = nf90_redef (ncId)
CALL ncErrorHandler (ncStatus)
  
!define  variable for the grid
 !If file already exists, have to read dimXd and dimYd
 !and dimTid if necessary
IF ( fileExist) THEN
  ncStatus = nf90_inq_dimid (ncId, "x", dimXid)
  CALL ncErrorHandler (ncStatus)
  ncStatus = nf90_inq_dimid (ncId, "y", dimYid)
  CALL ncErrorHandler (ncStatus)
  IF (StringToUpper (action) == 'APPEND') THEN
    ncStatus = nf90_inq_dimid (ncId, "time", dimTid)
    CALL ncErrorHandler (ncStatus)
    ncStatus = nf90_inq_varid (ncId, 'time', varTid)
    CALL ncErrorHandler (ncStatus)
  END IF
END IF

!verify if variable already exists 
varId = 0
ncStatus = nf90_inq_varid (ncId, name, varId)
IF (ncStatus == nf90_noerr) THEN
  varExist = .TRUE.
  !if variable already exists in non record dataset: error
  IF (StringToUpper (action) == 'ADD') THEN
    CALL Catch ('error', 'GridLib', 'variable ' // TRIM(name) // &
                ' already exists in netCDf dataset: ', &
                code = ncIOError, argument = file )
  END IF
ELSE
  varExist = .FALSE.

  !define new variable 
  IF (StringToUpper (action) == 'ADD' ) THEN
    ALLOCATE ( varDim (2) )  
  ELSE IF (StringToUpper (action) == 'APPEND' ) THEN
    ALLOCATE ( varDim (3) )
    varDim(3) = dimTid
  END IF
  varDim(1) = dimXid
  varDim(2) = dimYid
  ncStatus = nf90_def_var (ncId, name, NF90_FLOAT, varDim, varId)
  CALL ncErrorHandler (ncStatus)
  DEALLOCATE (varDim)

  !assign attributes to variable
  ncStatus = nf90_put_att (ncId, varId, 'long_name', grid % long_name)
  CALL ncErrorHandler (ncStatus)
  ncStatus = nf90_put_att (ncId, varId, 'standard_name', grid % standard_name)
  CALL ncErrorHandler (ncStatus)
  ncStatus = nf90_put_att (ncId, varId, 'units', grid % units)
  CALL ncErrorHandler (ncStatus)
  IF (grid % valid_min /= grid % nodata) THEN
    ncStatus = nf90_put_att (ncId, varId, 'valid_min', grid % valid_min)
    CALL ncErrorHandler (ncStatus)
  END IF
  IF (grid % valid_max /= grid % nodata) THEN
    ncStatus = nf90_put_att (ncId, varId, 'valid_max', grid % valid_max)
    CALL ncErrorHandler (ncStatus)
  END IF
  ncStatus = nf90_put_att (ncId, varId, '_FillValue', grid % nodata)
  CALL ncErrorHandler (ncStatus)
  IF (grid % esri_pe_string /= '') THEN
    ncStatus = nf90_put_att (ncId, varId, 'esri_pe_string', grid % esri_pe_string)
    CALL ncErrorHandler (ncStatus)
  END IF
  ncStatus = nf90_put_att (ncId, varId, 'varying_mode', grid % varying_mode)
  CALL ncErrorHandler (ncStatus)
  ncStatus = nf90_put_att (ncId, varId, 'grid_mapping', 'crs')
  CALL ncErrorHandler (ncStatus)
  
END IF

!end define mode
  ncStatus = nf90_enddef (ncId)
  CALL ncErrorHandler (ncStatus)

IF ( .NOT. fileExist) THEN !if file was created new, add coordinate variables
  !put coordinate
  ALLOCATE ( lons ( grid % jdim) )
  DO i = 1, grid % jdim
    lons (i) = grid % xllcorner + i * grid % cellsize - grid % cellsize / 2.
  END DO
  ncStatus = nf90_put_var (ncId, varXid, lons )
  CALL ncErrorHandler (ncStatus)
  DEALLOCATE ( lons )

  ALLOCATE ( lats ( grid % idim) )
  DO i = 1, grid % idim
    lats (i) = grid % yllcorner + i * grid % cellsize - grid % cellsize / 2.
  END DO
  ncStatus = nf90_put_var (ncId, varYid, lats )
  CALL ncErrorHandler (ncStatus)
  DEALLOCATE ( lats )

END IF

!if unlimited variable put time and grid
IF ( StringToUpper (action) == 'APPEND' ) THEN
  !time
  CALL ParseTime (ncId, ref_time, dummyString)
  timeSpan = grid % current_time - ref_time
  !retrieve number of records already stored in dataset
  ncStatus = nf90_inquire_dimension (ncId, dimTid, len = records)
  CALL ncErrorHandler (ncStatus) 
  !check if time span already exists 
  ALLOCATE ( times (records) )
  ncStatus = nf90_get_var (ncId, varTid, times)
  timeExists = .FALSE.
  DO i = 1, records
    IF ( timeSpan == times (i) ) THEN
      timeExists = .TRUE.
      timeRecord = i
      !(ADD SUPPORT FOR MULTIPLE TIME AXIS
      !CALL Catch ('error', 'GridLib',        &
      !     'time is already present in netCDF dataset: ',  &
      !     code = ncIOError, argument = ToString(timeSpan) )
    END IF
  END DO
  DEALLOCATE (times)
  
  !put time span
  IF ( .NOT. timeExists ) THEN
    timeRecord = records + 1
    ncStatus = nf90_put_var (ncId, varTid, timeSpan, start = timeRecord )
  END IF
  
  !put grid variable
  slice = (/ 1, 1, timeRecord /) 
  !swap grid before write to netcdf
  ALLOCATE (tempGrid (grid % jdim, grid % idim) )
  CALL SwapGridRealBack (grid % mat, tempGrid)
  ncStatus = nf90_put_var (ncId, varId, tempGrid, start = slice )
  CALL ncErrorHandler (ncStatus) 
  DEALLOCATE (tempGrid)
ELSE IF (StringToUpper (action) == 'ADD' ) THEN
  !swap grid before write to netcdf
  ALLOCATE (tempGrid (grid % jdim, grid % idim) )
  CALL SwapGridRealBack (grid % mat, tempGrid)
  !put variable
  ncStatus = nf90_put_var (ncId, varId, tempGrid )
  CALL ncErrorHandler (ncStatus)
  DEALLOCATE (tempGrid)
END IF

! close file
ncStatus = nf90_close (ncid)
CALL ncErrorHandler (ncStatus)


END SUBROUTINE ExportGridFloatToNetCDF


!==============================================================================
!! Description:
!!  export grid into netcdf file. Two actions are possible:
!!  add: add a non-record variable to a netCDF dataset. If file does not exist
!!       it is created new. The added variable
!!       must have the same dimensions of already present variables.
!!       If you try to add a variable already present in netCDF dataset
!!       an error is returned.
!!  append: append more data to record variables along the unlimited dimension.
!!          If netCDF dataset is created new, dimensions and attributes
!!          are set, otherwise only new data are written. If current time
!!          is already present in netcdf dataset an error is returned
SUBROUTINE ExportGridIntegerToNetCDF &
!
(grid, file, name, action)

USE StringManipulation, ONLY: &
! imported routines:
StringToUpper, ToString

IMPLICIT NONE

! Arguments with intent(in):
TYPE (grid_integer), INTENT(IN) :: grid !! grid to be exported
CHARACTER (LEN = *), INTENT(IN) :: file !!netcdf file to export to
CHARACTER (LEN = *), INTENT(IN) :: name !!name of variable in netcdf
CHARACTER (LEN = *), INTENT(IN) :: action !!add or append

! Local variables:
INTEGER (KIND = short)  :: ncStatus !!error code return by NetCDF routines
INTEGER (KIND = short)  :: ncId  !!NetCdf Id for the file
INTEGER (KIND = short)  :: dimXid  !!id of x dimension
INTEGER (KIND = short)  :: dimYid  !!id of y dimension
INTEGER (KIND = short)  :: dimTid  !!id of time dimension
INTEGER (KIND = short)  :: varXid  !!id of x variable
INTEGER (KIND = short)  :: varYid  !!id of y variable
INTEGER (KIND = short)  :: varTid  !!id of time variable
INTEGER (KIND = short)  :: mapId   !!id of grid mapping variable
INTEGER (KIND = short), ALLOCATABLE  :: varDim (:)  !!define dimension of variable
INTEGER (KIND = short)  :: varId  !!id of grid variable
CHARACTER (LEN = 25)    :: string
REAL (KIND = float), ALLOCATABLE :: lats(:), lons(:) !!array containing coordinate
INTEGER (KIND = short)  :: i !!loop index
LOGICAL :: fileExist    !!true if netcdf exists
LOGICAL :: varExist     !!true if variable exists in dataset 
TYPE (dateTime) :: ref_time !!reference time to calculate time span
CHARACTER (LEN = 7) :: dummyString 
REAL (KIND = float) :: timeSpan !!time between current time and reference time
INTEGER (KIND = short) :: records !!number of records stored in netcdf dataset
INTEGER (KIND = short) :: slice (3) !!used to write new record in right position
INTEGER (KIND = short) :: timeRecord (1) !!to write new time record
REAL (KIND = float), ALLOCATABLE :: times (:) !!values stored in time variable
LOGICAL :: timeExists
INTEGER (KIND = long), ALLOCATABLE :: tempGrid (:,:)

!------------end of declaration------------------------------------------------

!verify if file exists
ncStatus = nf90_open (file, NF90_WRITE, ncId)
IF (ncStatus == nf90_noerr) THEN
  fileExist = .TRUE.
ELSE
  fileExist = .FALSE.
END IF 

!if file does not exist create the file
IF (.NOT. fileExist) THEN
  ncStatus = nf90_create ( file, NF90_CLOBBER, ncId )
  CALL ncErrorHandler (ncStatus)
  
  !define dimensions
  ncStatus = nf90_def_dim ( ncId, 'x', grid % jdim, dimXid )
  CALL ncErrorHandler (ncStatus)
  ncStatus = nf90_def_dim ( ncId, 'y', grid % idim, dimYid )
  CALL ncErrorHandler (ncStatus)
  !if record (unlimited) variable define time dimension
  IF ( StringToUpper(action) ==  'APPEND' ) THEN
    ncStatus = nf90_def_dim ( ncId, 'time', NF90_UNLIMITED, dimTid )
    CALL ncErrorHandler (ncStatus) 
  END IF
  
  !define coordinate variables
  ncStatus = nf90_def_var (ncId, 'x', NF90_FLOAT, dimXid, varXid)
  CALL ncErrorHandler (ncStatus)
  ncStatus = nf90_def_var (ncId, 'y', NF90_FLOAT, dimYid, varYid)
  CALL ncErrorHandler (ncStatus)
  !if record (unlimited) variable define time variable
  IF ( StringToUpper(action) ==  'APPEND' ) THEN
    ncStatus = nf90_def_var ( ncId, 'time', NF90_FLOAT, dimTid, varTid )
    CALL ncErrorHandler (ncStatus) 
  END IF
  
  !assign attributes to coordinate variables
  IF (grid % grid_mapping % system == GEODETIC) THEN
      ncStatus = nf90_put_att (ncId, varXid, 'long_name', 'longitude (centre of grid cell)')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varXid, 'standard_name', 'longitude')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varXid, 'units', 'deg')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varXid, 'axis', 'X')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varYid, 'long_name', 'latitude (centre of grid cell)')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varYid, 'standard_name', 'latitude')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varYid, 'units', 'deg')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varYid, 'axis', 'Y')
      CALL ncErrorHandler (ncStatus)
  ELSE 
      ncStatus = nf90_put_att (ncId, varXid, 'long_name', 'x coordinate of projection')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varXid, 'standard_name', 'projection_x_coordinate')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varXid, 'units', 'm')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varXid, 'axis', 'X')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varYid, 'long_name', 'y coordinate of projection')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varYid, 'standard_name', 'projection_y_coordinate')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varYid, 'units', 'm')
      CALL ncErrorHandler (ncStatus)
      ncStatus = nf90_put_att (ncId, varYid, 'axis', 'Y')
      CALL ncErrorHandler (ncStatus)
END IF

  !if record (unlimited) variable define attributes of time variable
  IF ( StringToUpper(action) ==  'APPEND' ) THEN
    ncStatus = nf90_put_att ( ncId, varTid, 'long_name', 'time' )
    CALL ncErrorHandler (ncStatus)
    ncStatus = nf90_put_att ( ncId, varTid, 'standard_name', 'time' )
    CALL ncErrorHandler (ncStatus)
    string = grid % current_time
    ncStatus = nf90_put_att ( ncId, varTid, 'units', 'seconds since ' // string )
    CALL ncErrorHandler (ncStatus) 
  END IF
  
  !define dummy grid mapping variable
  ncStatus = nf90_def_var (ncId, 'crs', NF90_INT, mapId)
  CALL ncErrorHandler (ncStatus)
  
  !assign attributes to grid mapping variable
  ncStatus = nf90_put_att (ncId, mapId, 'grid_mapping_name', &
                           grid % grid_mapping % name)
  CALL ncErrorHandler (ncStatus)
  DO i = 1, grid % grid_mapping % basic
    ncStatus = nf90_put_att (ncId, mapId, grid % grid_mapping % description (i), grid % grid_mapping % param (i))
    CALL ncErrorHandler (ncStatus)
  END DO
  !datum
  ncStatus = nf90_put_att (ncId, mapId, 'datum_code', &
                           grid % grid_mapping % datum % code)
  CALL ncErrorHandler (ncStatus)
  
  
  !assign global attributes
  ncStatus = nf90_put_att (ncId, NF90_GLOBAL, 'Conventions', 'CF-1.0')
  CALL ncErrorHandler (ncStatus)
  string = UtcNow ()
  ncStatus = nf90_put_att (ncId, NF90_GLOBAL, 'history', 'creation date: ' // string )
  CALL ncErrorHandler (ncStatus)
  ncStatus = nf90_put_att (ncId, NF90_GLOBAL, 'institution', 'Politecnico di Milano' )
  CALL ncErrorHandler (ncStatus)

  !end define mode
  ncStatus = nf90_enddef (ncId)
  CALL ncErrorHandler (ncStatus)

END IF

!enter define mode
ncStatus = nf90_redef (ncId)
CALL ncErrorHandler (ncStatus)
  
!define  variable for the grid
 !If file already exists, have to read dimXd and dimYd
 !and dimTid if necessary
IF ( fileExist) THEN
  ncStatus = nf90_inq_dimid (ncId, "x", dimXid)
  CALL ncErrorHandler (ncStatus)
  ncStatus = nf90_inq_dimid (ncId, "y", dimYid)
  CALL ncErrorHandler (ncStatus)
  IF (StringToUpper (action) == 'APPEND') THEN
    ncStatus = nf90_inq_dimid (ncId, "time", dimTid)
    CALL ncErrorHandler (ncStatus)
    ncStatus = nf90_inq_varid (ncId, 'time', varTid)
    CALL ncErrorHandler (ncStatus)
  END IF
END IF

!verify if variable already exists 
varId = 0
ncStatus = nf90_inq_varid (ncId, name, varId)
IF (ncStatus == nf90_noerr) THEN
  varExist = .TRUE.
  !if variable already exists in non record dataset: error
  IF (StringToUpper (action) == 'ADD') THEN
    CALL Catch ('error', 'GridLib', 'variable ' // TRIM(name) // &
                ' already exists in netCDf dataset: ', &
                code = ncIOError, argument = file )
  END IF
ELSE
  varExist = .FALSE.

  !define new variable 
  IF (StringToUpper (action) == 'ADD' ) THEN
    ALLOCATE ( varDim (2) )  
  ELSE IF (StringToUpper (action) == 'APPEND' ) THEN
    ALLOCATE ( varDim (3) )
    varDim(3) = dimTid
  END IF
  varDim(1) = dimXid
  varDim(2) = dimYid
  ncStatus = nf90_def_var (ncId, name, NF90_FLOAT, varDim, varId)
  CALL ncErrorHandler (ncStatus)
  DEALLOCATE (varDim)

  !assign attributes to variable
  ncStatus = nf90_put_att (ncId, varId, 'long_name', grid % long_name)
  CALL ncErrorHandler (ncStatus)
  ncStatus = nf90_put_att (ncId, varId, 'standard_name', grid % standard_name)
  CALL ncErrorHandler (ncStatus)
  ncStatus = nf90_put_att (ncId, varId, 'units', grid % units)
  CALL ncErrorHandler (ncStatus)
  IF (grid % valid_min /= grid % nodata) THEN
    ncStatus = nf90_put_att (ncId, varId, 'valid_min', grid % valid_min)
    CALL ncErrorHandler (ncStatus)
  END IF
  IF (grid % valid_max /= grid % nodata) THEN
    ncStatus = nf90_put_att (ncId, varId, 'valid_max', grid % valid_max)
    CALL ncErrorHandler (ncStatus)
  END IF
  ncStatus = nf90_put_att (ncId, varId, '_FillValue', grid % nodata)
  CALL ncErrorHandler (ncStatus)
  IF (grid % esri_pe_string /= '') THEN
    ncStatus = nf90_put_att (ncId, varId, 'esri_pe_string', grid % esri_pe_string)
    CALL ncErrorHandler (ncStatus)
  END IF
  ncStatus = nf90_put_att (ncId, varId, 'varying_mode', grid % varying_mode)
  CALL ncErrorHandler (ncStatus)
  ncStatus = nf90_put_att (ncId, varId, 'grid_mapping', 'crs')
  CALL ncErrorHandler (ncStatus)
  
END IF

!end define mode
  ncStatus = nf90_enddef (ncId)
  CALL ncErrorHandler (ncStatus)

IF ( .NOT. fileExist) THEN !if file was created new, add coordinate variables
  !put coordinate
  ALLOCATE ( lons ( grid % jdim) )
  DO i = 1, grid % jdim
    lons (i) = grid % xllcorner + i * grid % cellsize - grid % cellsize / 2.
  END DO
  ncStatus = nf90_put_var (ncId, varXid, lons )
  CALL ncErrorHandler (ncStatus)
  DEALLOCATE ( lons )

  ALLOCATE ( lats ( grid % idim) )
  DO i = 1, grid % idim
    lats (i) = grid % yllcorner + i * grid % cellsize - grid % cellsize / 2.
  END DO
  ncStatus = nf90_put_var (ncId, varYid, lats )
  CALL ncErrorHandler (ncStatus)
  DEALLOCATE ( lats )

END IF

!if unlimited variable put time and grid
IF ( StringToUpper (action) == 'APPEND' ) THEN
  !time
  CALL ParseTime (ncId, ref_time, dummyString)
  timeSpan = grid % current_time - ref_time
  !retrieve number of records already stored in dataset
  ncStatus = nf90_inquire_dimension (ncId, dimTid, len = records)
  CALL ncErrorHandler (ncStatus) 
  !check if time span already exists 
  ALLOCATE ( times (records) )
  ncStatus = nf90_get_var (ncId, varTid, times)
  timeExists = .FALSE.
  DO i = 1, records
    IF ( timeSpan == times (i) ) THEN
      timeExists = .TRUE.
      timeRecord = i
      !(ADD SUPPORT FOR MULTIPLE TIME AXIS
      !CALL Catch ('error', 'GridLib',        &
      !     'time is already present in netCDF dataset: ',  &
      !     code = ncIOError, argument = ToString(timeSpan) )
    END IF
  END DO
  DEALLOCATE (times)
  
  !put time span
  IF ( .NOT. timeExists ) THEN
    timeRecord = records + 1
    ncStatus = nf90_put_var (ncId, varTid, timeSpan, start = timeRecord )
  END IF
  
  !put grid variable
  slice = (/ 1, 1, timeRecord /)
  !swap grid before write to netcdf
  ALLOCATE (tempGrid (grid % jdim, grid % idim) )
  CALL SwapGridIntegerBack (grid % mat, tempGrid) 
  ncStatus = nf90_put_var (ncId, varId, tempGrid, start = slice )
  CALL ncErrorHandler (ncStatus) 
  DEALLOCATE (tempGrid)
ELSE IF (StringToUpper (action) == 'ADD' ) THEN
  !swap grid before write to netcdf
  ALLOCATE (tempGrid (grid % jdim, grid % idim) )
  CALL SwapGridIntegerBack (grid % mat, tempGrid)
  !put variable
  ncStatus = nf90_put_var (ncId, varId, tempGrid )
  CALL ncErrorHandler (ncStatus)
  DEALLOCATE (tempGrid)
END IF

! close file
ncStatus = nf90_close (ncid)
CALL ncErrorHandler (ncStatus)

END SUBROUTINE ExportGridIntegerToNetCDF

!==============================================================================
!! Description:
!!   export grid_real to file. 
!!   List of supported format:
!!   ESRI_ASCII: ESRI ASCII GRID
!!   ESRI_BINARY: ESRI BINARY GRID 
SUBROUTINE ExportGridFloatToFile &
!
(layer, fileName, fileFormat) 

IMPLICIT NONE

!Arguments with intent(in):
TYPE (grid_real), INTENT (IN) :: layer
CHARACTER (LEN = *), INTENT (IN) :: fileName
INTEGER (KIND = short), INTENT(IN) :: fileFormat 
!------------end of declaration------------------------------------------------

IF ( fileformat == ESRI_ASCII ) THEN
  CALL ExportGridFloatToESRI_ASCII (layer, fileName)
ELSE IF ( fileformat == ESRI_BINARY ) THEN
  CALL ExportGridFloatToESRI_BINARY (layer, fileName)
ELSE
  CALL Catch ('error', 'GridLib',  &
               'unknown option on exporting file: ',  &
               code = unknownOption, argument = fileName )
END IF
 
END SUBROUTINE ExportGridFloatToFile

!==============================================================================
!! Description:
!!   export grid_real to a ESRI ASCII file. 
SUBROUTINE ExportGridFloatToESRI_ASCII &
!
(layer, fileName) 

USE Utilities, ONLY: &
!Imported routines:
GetUnit

IMPLICIT NONE

!Arguments with intent(in):
TYPE (grid_real), INTENT (IN) :: layer
CHARACTER (LEN = *), INTENT (IN) :: fileName

!Local variables:
INTEGER (KIND = short)          :: fileUnit
INTEGER (KIND = short)          :: ios
INTEGER (KIND = short)          :: i,j
!------------end of declaration------------------------------------------------

!open file
fileUnit = GetUnit ()
OPEN (UNIT = fileUnit, file = fileName, IOSTAT = ios)
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error opening file: ',    &
              code = openFileError, argument = fileName )
END IF

!write header
WRITE(fileUnit,'(a14,i10)')    "ncols         ", layer % jdim
WRITE(fileUnit,'(a14,i10)')    "nrows         ", layer % idim
WRITE(fileUnit,'(a14,f15.5)') "xllcorner     ", layer % xllcorner
WRITE(fileUnit,'(a14,f15.5)') "yllcorner     ", layer % yllcorner
WRITE(fileUnit,'(a14,f15.5)') "cellsize      ", layer % cellsize
WRITE(fileUnit,'(a14,E14.7)') "NODATA_value  ", layer % nodata

!write data
DO i = 1, layer % idim 
   DO j = 1, layer % jdim - 1
      WRITE(fileUnit,'(E14.7," ")', ADVANCE = 'no') layer % mat(i,j)
   END DO
   WRITE(fileUnit,'(E14.7," ")') layer % mat(i,layer % jdim)
END DO 

CLOSE (fileUnit)

END SUBROUTINE ExportGridFloatToESRI_ASCII

!==============================================================================
!! Description:
!!   export grid_real to a ESRI BINARY file. 
SUBROUTINE ExportGridFloatToESRI_BINARY &
!
(layer, fileName) 

USE Utilities, ONLY: &
!Imported routines:
GetUnit

IMPLICIT NONE

!Arguments with intent(in):
TYPE (grid_real), INTENT (IN) :: layer
CHARACTER (LEN = *), INTENT (IN) :: fileName

!Local variables:
INTEGER (KIND = short)          :: fileUnit
INTEGER (KIND = short)          :: ios
INTEGER (KIND = short)          :: recordLength 
INTEGER (KIND = short)          :: recordNumber
INTEGER (KIND = short)          :: i,j
!------------end of declaration------------------------------------------------

!open file
fileUnit = GetUnit ()

OPEN (UNIT = fileUnit, file = TRIM(fileName) // '.hdr', IOSTAT = ios)
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error opening file: ',    &
              code = openFileError, argument = TRIM(fileName) // '.hdr' )
END IF

!write header
WRITE(fileUnit,'(a14,i10)')    "ncols         ", layer % jdim
WRITE(fileUnit,'(a14,i10)')    "nrows         ", layer % idim
WRITE(fileUnit,'(a14,f15.5)') "xllcorner     ", layer % xllcorner
WRITE(fileUnit,'(a14,f15.5)') "yllcorner     ", layer % yllcorner
WRITE(fileUnit,'(a14,f15.5)') "cellsize      ", layer % cellsize
WRITE(fileUnit,'(a14,E14.7)') "NODATA_value  ", layer % nodata
WRITE(fileUnit,'(a18)')       "BYTEORDER LSBFIRST"
CLOSE (fileUnit)

!write data
fileUnit = GetUnit ()
INQUIRE (IOLENGTH = recordLength) 100_float
OPEN (UNIT = fileUnit, file = TRIM(fileName) // '.flt', &
      FORM = 'UNFORMATTED', ACCESS = 'DIRECT', RECL = recordLength, &
      IOSTAT = ios)
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error opening file: ',    &
              code = openFileError, argument = TRIM(fileName) // '.flt' )
END IF

recordNumber = 0
DO i = 1,layer % idim
  DO j = 1, layer % jdim
    recordNumber = recordNumber + 1
    WRITE (fileUnit,REC = recordNumber) layer % mat (i,j)
  END DO
END DO

CLOSE (fileUnit)

END SUBROUTINE ExportGridFloatToESRI_BINARY

!==============================================================================
!! Description:
!!   export grid_integer to file. 
!!   List of supported format:
!!   ESRI_ASCII: ESRI ASCII GRID
!!   ESRI_BINARY: ESRI BINARY GRID 
SUBROUTINE ExportGridIntegerToFile &
!
(layer, fileName, fileFormat) 

IMPLICIT NONE

!Arguments with intent(in):
TYPE (grid_integer), INTENT (IN) :: layer
CHARACTER (LEN = *), INTENT (IN) :: fileName
INTEGER (KIND = short), INTENT(IN) :: fileFormat 
!------------end of declaration------------------------------------------------

IF ( fileformat == ESRI_ASCII ) THEN
  CALL ExportGridIntegerToESRI_ASCII (layer, fileName)
ELSE IF ( fileformat == ESRI_BINARY ) THEN
  CALL ExportGridIntegerToESRI_BINARY (layer, fileName)
ELSE
  CALL Catch ('error', 'GridLib',  &
               'unknown option on exporting file: ',  &
               code = unknownOption, argument = fileName )
END IF
 
END SUBROUTINE ExportGridIntegerToFile

!==============================================================================
!! Description:
!!   export grid_integer to a ESRI ASCII file. 
SUBROUTINE ExportGridIntegerToESRI_ASCII &
!
(layer, fileName) 

USE Utilities, ONLY: &
!Imported routines:
GetUnit

IMPLICIT NONE

!Arguments with intent(in):
TYPE (grid_integer), INTENT (IN) :: layer
CHARACTER (LEN = *), INTENT (IN) :: fileName

!Local variables:
INTEGER (KIND = short)          :: fileUnit
INTEGER (KIND = short)          :: ios
INTEGER (KIND = short)          :: i,j
!------------end of declaration------------------------------------------------

!open file
fileUnit = GetUnit ()
OPEN (UNIT = fileUnit, file = fileName, IOSTAT = ios)
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error opening file: ',    &
              code = openFileError, argument = fileName )
END IF

!write header
WRITE(fileUnit,'(a14,i4)')    "ncols         ", layer % jdim
WRITE(fileUnit,'(a14,i4)')    "nrows         ", layer % idim
WRITE(fileUnit,'(a14,f10.2)') "xllcorner     ", layer % xllcorner
WRITE(fileUnit,'(a14,f10.2)') "yllcorner     ", layer % yllcorner
WRITE(fileUnit,'(a14,f10.1)') "cellsize      ", layer % cellsize
WRITE(fileUnit,'(a14,i8)')    "NODATA_value  ", layer % nodata

!write data
DO i = 1,layer % idim 
   DO j = 1, layer % jdim - 1
      WRITE(fileUnit,'(i8," ")', ADVANCE = 'no') layer % mat(i,j)
   END DO
   WRITE(fileUnit,'(i8," ")') layer % mat(i,layer % jdim)
END DO 

CLOSE (fileUnit)

END SUBROUTINE ExportGridIntegerToESRI_ASCII

!==============================================================================
!! Description:
!!   export grid_integer to a ESRI BINARY file. 
SUBROUTINE ExportGridIntegerToESRI_BINARY &
!
(layer, fileName) 

USE Utilities, ONLY: &
!Imported routines:
GetUnit

IMPLICIT NONE

!Arguments with intent(in):
TYPE (grid_integer), INTENT (IN) :: layer
CHARACTER (LEN = *), INTENT (IN) :: fileName

!Local variables:
INTEGER (KIND = short)          :: fileUnit
INTEGER (KIND = short)          :: ios
INTEGER (KIND = short)          :: recordLength 
INTEGER (KIND = short)          :: recordNumber
INTEGER (KIND = short)          :: i,j
!------------end of declaration------------------------------------------------

!open file
fileUnit = GetUnit ()

OPEN (UNIT = fileUnit, file = TRIM(fileName) // '.hdr', IOSTAT = ios)
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error opening file: ',    &
              code = openFileError, argument = TRIM(fileName) // '.hdr' )
END IF

!write header
WRITE(fileUnit,'(a14,i4)')    "ncols         ", layer % jdim
WRITE(fileUnit,'(a14,i4)')    "nrows         ", layer % idim
WRITE(fileUnit,'(a14,f10.2)') "xllcorner     ", layer % xllcorner
WRITE(fileUnit,'(a14,f10.2)') "yllcorner     ", layer % yllcorner
WRITE(fileUnit,'(a14,f10.1)') "cellsize      ", layer % cellsize
WRITE(fileUnit,'(a14,i8)') "NODATA_value  ", layer % nodata
WRITE(fileUnit,'(a18)')       "BYTEORDER LSBFIRST"
CLOSE (fileUnit)

!write data
fileUnit = GetUnit ()
INQUIRE (IOLENGTH = recordLength) 100_long
OPEN (UNIT = fileUnit, file = TRIM(fileName) // '.flt', &
      FORM = 'UNFORMATTED', ACCESS = 'DIRECT', RECL = recordLength, &
      IOSTAT = ios)
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridLib',        &
              'error opening file: ',    &
              code = openFileError, argument = TRIM(fileName) // '.flt' )
END IF

recordNumber = 0
DO i = 1,layer % idim
  DO j = 1, layer % jdim
    recordNumber = recordNumber + 1
    WRITE (fileUnit,REC = recordNumber) layer % mat (i,j)
  END DO
END DO

CLOSE (fileUnit)

END SUBROUTINE ExportGridIntegerToESRI_BINARY

!==============================================================================
!! Description:
!!   read and calculate georeferencing informations from netCDF file.
SUBROUTINE GetGeoreferenceFromNCdataSet &
!
(ncId, varId, cellsize, xll, yll, grid_mapping)

USE StringManipulation, ONLY: &
!Imported routines:
StringCompact


IMPLICIT NONE

! Arguments with intent (in):
INTEGER (KIND = short), INTENT(IN) :: ncId  !!NetCdf Id for the file
INTEGER (KIND = short), INTENT(IN) :: varId !!variable Id

! Arguments with intent(out):
REAL (KIND = float), INTENT(OUT) :: cellsize !!cell resolution
REAL (KIND = float), INTENT(OUT) :: xll !!x  coordinate of lower left corner of map
REAL (KIND = float), INTENT(OUT) :: yll !!y  coordinate of lower left corner of map
TYPE (CRS),          INTENT(OUT) :: grid_mapping !!coordinate reference system        

! Local variables:
INTEGER (KIND = short) :: x, y !!number of columns and rows
INTEGER (KIND = short) :: idx, idy
REAL (KIND = float), ALLOCATABLE :: xArray (:), yArray (:)
REAL (KIND = float) :: jdim, idim !! x and y resolution
INTEGER (KIND = short)          :: ncStatus !!error code return by NetCDF routines
INTEGER (KIND = short) :: i
CHARACTER (LEN = 80) :: attribute
CHARACTER (LEN = 100)         :: variableName
CHARACTER (LEN = 1) :: shp !!define if x and y are vectors or matrix
INTEGER (KIND = short) :: mappingId !!grid mapping variable Id
INTEGER (KIND = short) :: refSystem !!reference system
INTEGER (KIND = short) :: datum !!datum code
REAL (KIND = float) :: lat0 !!latitude of projection origin
REAL (KIND = float) :: centM !!longitude of central meridian
REAL (KIND = float) :: primeM !!longitude of prime meridian
REAL (KIND = float) :: falseE
REAL (KIND = float) :: falseN
REAL (KIND = float) :: scale_factor
REAL (KIND = float) :: tol = 0.01!!tolerance for checking regularly spaced grid
!------------end of declaration------------------------------------------------

!get length of x and y sizes and related id
CALL GetXYSizesFromFile (ncId, x, y, idx = idx, idy = idy, shape = shp)

!allocate space to read coordinates
ALLOCATE ( xArray (x) )
ALLOCATE ( yArray (y) )

IF (shp == 'v' ) THEN !x and y values are stored in vectors
    !read x coordinates
    ncStatus = nf90_get_var (ncId, idx, xArray)
    CALL ncErrorHandler (ncStatus)

    !read y coordinates
    ncStatus = nf90_get_var (ncId, idy, yArray)
    CALL ncErrorHandler (ncStatus)
ELSE IF (shp == 'm') THEN !x and y values are stored in matrix
    !read x coordinates: read only first row
    ncStatus = nf90_get_var (ncId, idx, xArray, count = (/ x, 1 /))
    CALL ncErrorHandler (ncStatus)
    
    !read y coordinates: read only first column
    ncStatus = nf90_get_var (ncId, idy, yArray, count = (/ 1, y /))
    CALL ncErrorHandler (ncStatus)
  
END IF

!calculate and check x spatial resolution
jdim = xArray (2) - xArray (1)
DO i = 3, x
  IF ( ABS((xArray (i) - xArray (i-1) - jdim ) / jdim) > tol) THEN
    CALL Catch ('error', 'GridLib', 'x dimension not regularly spaced'  )
  END IF
END DO

!calculate and check y spatial resolution
idim = yArray (2) - yArray (1) 
DO i = 3, y
  IF ( ABS((yArray (i) - yArray (i-1) - idim) / idim) > tol) THEN
    CALL Catch ('error', 'GridLib', 'y dimension not regularly spaced'  )
  END IF
END DO

!set cellsize
IF (ABS((jdim - idim)/jdim) < tol) THEN
  cellsize = jdim
ELSE
  CALL Catch ('error', 'GridLib', 'x resolution different from y resolution' )
END IF
 

!calculate xll and yll
xll = MINVAL (xArray) - cellsize / 2.
yll = MINVAL (yArray) - cellsize / 2.

!deallocate arrays
DEALLOCATE ( xArray )
DEALLOCATE ( yArray )

!read coordinate reference system
attribute = ''
ncStatus = nf90_get_att (ncId, varid = varId, name = 'grid_mapping', &
                           values = attribute)

IF (ncStatus == nf90_noerr) THEN !check if grid_mapping exists
  !retrieve varId of grid_mapping variable
  ncStatus = nf90_inq_varid (ncId, attribute, mappingId)
  CALL ncErrorHandler (ncStatus)
  !retrieve grid mapping name
  ncStatus = nf90_get_att (ncId, varid = mappingId, name = 'grid_mapping_name', &
                           values = attribute)
  CALL ncErrorHandler (ncStatus)
  !retrieve datum EPSG code (extension to CF conventions)
  ncStatus = nf90_get_att (ncId, varid = mappingId, name = 'datum_code', &
                           values = datum)
  !if datum is not specified, datum_code does not exist, set default to WGS84 
  IF (ncStatus /= nf90_noerr) THEN
    datum = WGS84
  END IF    
  !retrieve reference system parameters                    
  IF ( StringCompact (attribute) == 'transverse_mercator' ) THEN
    CALL SetCRS (TM, datum, grid_mapping )
     ncStatus = nf90_get_att (ncId, varid = mappingId, &
                              name = 'longitude_of_central_meridian', &
                              values = centM)
     ncStatus = nf90_get_att (ncId, varid = mappingId, &
                              name = 'latitude_of_projection_origin', &
                              values = lat0)
     ncStatus = nf90_get_att (ncId, varid = mappingId, &
                              name = 'scale_factor_at_central_meridian', &
                              values = scale_factor)
     ncStatus = nf90_get_att (ncId, varid = mappingId, &
                              name = 'false_easting', &
                              values = falseE)
     ncStatus = nf90_get_att (ncId, varid = mappingId, &
                              name = 'false_northing', &
                              values = falseN)
     CALL SetTransverseMercatorParameters (grid_mapping, lat0, centM, &
                                           falseE, falseN, scale_factor)
  ELSE IF ( StringCompact (attribute) == 'latitude_longitude' ) THEN
     CALL SetCRS (GEODETIC, datum, grid_mapping )
     ncStatus = nf90_get_att (ncId, varid = mappingId, &
                              name = 'longitude_of_prime_meridian', &
                              values = primeM)
     
     CALL SetGeodeticParameters (grid_mapping, primeM)
  ELSE
  !other reference systems not yet supported
  END IF
ELSE !CRS not specified: set default to geodetic WGS84
  !retrieve name of variable
   ncStatus = nf90_inquire_variable(ncId, varId, name = variableName)
   CALL ncErrorHandler (ncStatus)
   CALL Catch ('warning', 'GridLib',        &
    'grid_mapping not found in variable: ',  &
    argument = variableName ) 
    
    CALL Catch ('info', 'GridLib',        &
    'set default coordinate reference system to geodetic wgs84') 
    
    CALL SetCRS (GEODETIC, WGS84, grid_mapping)
    CALL SetGeodeticParameters (grid_mapping, prime_meridian = 0.0)

END IF                           
  
END SUBROUTINE GetGeoreferenceFromNCdataSet


!==============================================================================
!! Description:
!!   extraxt the number of columns (x) and rows (y) from netCDF file.
SUBROUTINE GetXYSizesFromFile &
!
(ncId, x, y, idx, idy, shape)

IMPLICIT NONE

!Arguments with intent(in):
INTEGER (KIND = short), INTENT(IN) :: ncId  !!NetCdf Id for the file

!Arguments with intent(out):
INTEGER (KIND = short), INTENT(OUT) :: x, y !!number of columns and rows
CHARACTER (LEN = *), INTENT(OUT) :: shape
INTEGER (KIND = short), OPTIONAL, INTENT(OUT) :: idx !! id of x variable
INTEGER (KIND = short), OPTIONAL, INTENT(OUT) :: idy !! id of y variable

! Local variables:
INTEGER (KIND = short)          :: ncStatus !!error code return by NetCDF routines
INTEGER (KIND = short)          :: nVars !!number of variables
INTEGER (KIND = short)          :: IdXCoordinate !!Id of the variable containing 
                                                 !!information on x ccordinate
INTEGER (KIND = short)          :: IdYCoordinate !!Id of the variable containing 
                                                 !!information on y ccordinate
INTEGER (KIND = short)          :: IdXDim !!Id of the X dimension 
INTEGER (KIND = short)          :: IdYDim !!Id of the Y dimension 
INTEGER (KIND = short)          :: nDimsVar !!number of dimensions of a variable
INTEGER (KIND = short), ALLOCATABLE :: dimIds (:) !!dimension Ids of a variable
CHARACTER (LEN = 80)            :: attribute
INTEGER (KIND = short)          :: i

!------------end of declaration------------------------------------------------


!inquire dataset to retrieve number of dimensions, variables 
!and global attributes
ncStatus = nf90_inquire(ncId, nVariables = nVars )
                  
CALL ncErrorHandler (ncStatus)

!scan dataset searching for variable referring to grid mapping
DO i = 1, nVars
  attribute = ''
  ncStatus = nf90_get_att (ncId, varid = i, name = 'standard_name', &
                           values = attribute)
  
  IF (ncStatus == nf90_noerr) THEN !'standard_name' is  found
    IF ( attribute(1:23) == 'projection_x_coordinate' .OR. & !projected reference system
         attribute(1:9) == 'longitude' .OR. & !geographic reference system
         attribute(1:14) == 'grid_longitude')THEN !rotated pole system
       IdXCoordinate = i
    ELSE IF ( attribute(1:23) == 'projection_y_coordinate' .OR. &
              attribute(1:8) == 'latitude' .OR. & !geographic reference system
              attribute(1:13) == 'grid_latitude')THEN !rotated pole system
       IdYCoordinate = i
    END IF
  END IF

  ncStatus = nf90_get_att (ncId, varid = i, name = 'long_name', &
                           values = attribute)

  IF (ncStatus == nf90_noerr) THEN !'long_name' is  found
    IF ( attribute(1:23) == 'projection_x_coordinate' .OR. & !projected reference system
         attribute(1:9) == 'longitude' .OR. & !geographic reference system
         attribute(1:14) == 'grid_longitude')THEN !rotated pole system
       IdXCoordinate = i
    ELSE IF ( attribute(1:23) == 'projection_y_coordinate' .OR. &
              attribute(1:8) == 'latitude' .OR. & !geographic reference system
              attribute(1:13) == 'grid_latitude')THEN !rotated pole system
       IdYCoordinate = i
    END IF
  END IF
 
END DO

IF (PRESENT (idx) ) THEN
  idx = IdXCoordinate
END IF

IF (PRESENT (idy) ) THEN
  idy = IdYCoordinate
END IF

!retrieve Id of X  dimension
ncStatus = nf90_inquire_variable (ncId, IdXCoordinate, ndims = nDimsVar)
CALL ncErrorHandler (ncStatus)
ALLOCATE (dimIds (nDimsVar))
ncStatus = nf90_inquire_variable (ncId, IdXCoordinate, dimids = dimIds)
CALL ncErrorHandler (ncStatus)
IF ( nDimsVar == 1) THEN !x is a vector
  IdXDim = dimIds (1)
  shape = 'v'
ELSE IF ( nDimsVar == 2) THEN 
  !x is a matrix. Used for not regular grids.
  !Suppose that x is the first dimension
  IdXDim = dimIds (1)
  shape = 'm'
END IF
DEALLOCATE (dimIds)

!retrieve Id of Y dimension
ncStatus = nf90_inquire_variable (ncId, IdYCoordinate, ndims = nDimsVar)
CALL ncErrorHandler (ncStatus)
ALLOCATE (dimIds (nDimsVar))
ncStatus = nf90_inquire_variable (ncId, IdYCoordinate, dimids = dimIds)
CALL ncErrorHandler (ncStatus)
IF ( nDimsVar == 1) THEN !y is a vector
  IdYDim = dimIds (1)
ELSE IF ( nDimsVar == 2) THEN 
  !y is a matrix. Used for not regular grids.
  !Suppose that y is the second dimension
  IdYDim = dimIds (2)
END IF
DEALLOCATE (dimIds)

!retrieve length
ncStatus = nf90_inquire_dimension (ncId, IdXDim, len = x)
CALL ncErrorHandler (ncStatus)

ncStatus = nf90_inquire_dimension (ncId, IdYDim, len = y)
CALL ncErrorHandler (ncStatus)

END SUBROUTINE GetXYSizesFromFile


!==============================================================================
!! Description:
!!   Calculate the index to extract the corresponding slice from netcdf file.
FUNCTION TimeIndex &
!
(ncId, refTime, timeUnit, time) &
RESULT (index)

USE Units, ONLY: &
! Imported parameters:
minute, hour, day, month

USE StringManipulation, ONLY: &
!Imported routines:
StringtOsHORT

IMPLICIT NONE

! Arguments with intent(in):
INTEGER (KIND = short), INTENT(IN)   :: ncId  !!NetCdf Id for the file
TYPE (DateTime), INTENT(IN)  :: refTime  !!reference time to calculate time index
CHARACTER (LEN = *) :: timeUnit
TYPE (DateTime), INTENT (IN)  :: time !!time to calculate index from reference time

! Local variables:
INTEGER (KIND = short) :: index
INTEGER (KIND = long)  :: difference
INTEGER (KIND = short) :: ncStatus !!error code return by NetCDF routines
INTEGER (KIND = short) :: nDims !!number of dimensions
INTEGER (KIND = short) :: nVars !!number of variables
INTEGER (KIND = short) :: nAtts !!number of global attributes
INTEGER (KIND = short) :: length !!length of time dimension
INTEGER (KIND = short) :: idTime !!Id of the variable containing 
                                 !!information on time ccordinate   
INTEGER (KIND = short) :: timeNumber   
INTEGER (KIND = short) :: CurrentTimeNumber     
CHARACTER (LEN = 80)   :: attribute
CHARACTER (LEN = 100)  :: variableName
INTEGER (KIND = short) :: i !!loop index
INTEGER                :: slice (1)
INTEGER                :: current
LOGICAL                :: found
CHARACTER (LEN = 25)   :: string
INTEGER, DIMENSION(NF90_MAX_VAR_DIMS) :: dimIDs
!------------end of declaration------------------------------------------------

!inquire dataset to retrieve number of dimensions, variables 
!and global attributes
ncStatus = nf90_inquire(ncId, nDimensions = nDims, &
                        nVariables = nVars,        &
                        nAttributes = nAtts        )
                  
CALL ncErrorHandler (ncStatus)

!search for time variable
DO i = 1, nVars
  attribute = ''
  ncStatus = nf90_get_att (ncId, varid = i, name = 'standard_name', &
                           values = attribute)
  
  IF (ncStatus == nf90_noerr) THEN 
    IF ( attribute(1:4) == 'time' ) THEN
      idTime = i 
      EXIT   
    END IF
  ELSE !standard_name is not defined: search for variable named 'time'
     !ncStatus = nf90_inq_varid (ncId, 'time', varid = i )
     ncstatus = nf90_inquire_variable(ncId, varId = i, name = variableName)
     IF (LEN_TRIM(variableName) == 4 .AND. &
         variableName(1:4) == 'time') THEN !variable 'time' found
       idTime = i 
       EXIT 
     END IF
  END IF
END DO

!inquire time length
ncStatus = nf90_inquire_variable(ncid, idTime, dimids = dimIDs)
CALL ncErrorHandler (ncStatus)

ncStatus = nf90_inquire_dimension (ncId, dimid = dimIDs(1), len = length)
CALL ncErrorHandler (ncStatus)

!search for current time
found = .FALSE.
IF (DateTimeIsDefault(refTime)) THEN
  !build datetime string as used in netcdf file
  timeString = time
  timeString = timeString(1:4) // timeString(6:7) // &
               timeString(9:10) // timeString(12:13)
   CurrentTimeNumber  = StringToShort (timeString)      
  DO i = 1, length
     slice(1) = i
     ncStatus = nf90_get_var (ncId, idTime, timeNumber , start = slice)
     CALL ncErrorHandler (ncStatus)
     IF (CurrentTimeNumber == timeNumber) THEN
       found = .TRUE.
       index = i
       EXIT
     END IF
    END DO
ELSE
    !calculate time span in appropriate unit
    difference = time - refTime
    SELECT CASE (timeUnit)
      CASE ('minutes')
        difference = difference / INT(minute)
      CASE ('hours')
        difference = difference / INT(hour)
      CASE ('days')
        difference = difference / INT(day)
      CASE ('months')
        difference = difference / INT(month)
    END SELECT  

    DO i = 1, length
     slice(1) = i
     ncStatus = nf90_get_var (ncId, idTime, current , start = slice)
     CALL ncErrorHandler (ncStatus)
     IF (current == difference) THEN
       found = .TRUE.
       index = i
       EXIT
     END IF
    END DO
END IF

IF ( .NOT. found ) THEN
  string = time
  CALL Catch ('error', 'GridLib',        &
    'time not found in netcdf file: ',  &
    argument = string )
END IF

END FUNCTION TimeIndex

!==============================================================================
!! Description
!!   returns the time of the next grid in netcdf dataset
FUNCTION NextTime &
!
(ncId, refTime, timeUnit, current) &
!
RESULT (time)

USE Units, ONLY: &
! Imported parameters:
minute, hour, day, month

USE StringManipulation, ONLY: &
!Imported routines:
ToString

IMPLICIT NONE

! Arguments with intent(in):
INTEGER (KIND = short), INTENT(IN)   :: ncId  !!NetCdf Id for the file
TYPE (DateTime), INTENT(IN)  :: refTime  !!reference time to calculate time index
CHARACTER (LEN = *), INTENT(IN) :: timeUnit
INTEGER (KIND = short), INTENT(IN) :: current !!current time step

!Local variables:
TYPE (DateTime)  :: time !!returned time of the next grid
INTEGER (KIND = short) :: next !!index of next time step
INTEGER (KIND = short) :: ncStatus !!error code return by NetCDF routines
INTEGER (KIND = short) :: nDims !!number of dimensions
INTEGER (KIND = short) :: nVars !!number of variables
INTEGER (KIND = short) :: nAtts !!number of global attributes
INTEGER (KIND = short) :: length !!length of time dimension
INTEGER (KIND = short) :: idTime !!Id of the variable containing 
                                 !!information on time ccordinate
CHARACTER (LEN = 80)   :: attribute
CHARACTER (LEN = 100)  :: variableName
INTEGER (KIND = short) :: i !!loop index
INTEGER                :: slice (1)
INTEGER                :: timeSpan
CHARACTER (LEN = 80)   :: string
INTEGER, DIMENSION(NF90_MAX_VAR_DIMS) :: dimIDs

!------------end of declaration------------------------------------------------

!inquire dataset to retrieve number of dimensions, variables 
!and global attributes
ncStatus = nf90_inquire(ncId, nDimensions = nDims, &
                        nVariables = nVars,        &
                        nAttributes = nAtts        )
                  
CALL ncErrorHandler (ncStatus)

!search for time variable
DO i = 1, nVars
  attribute = ''
  ncStatus = nf90_get_att (ncId, varid = i, name = 'standard_name', &
                           values = attribute)
  
  IF (ncStatus == nf90_noerr) THEN 
    IF ( attribute(1:4) == 'time' ) THEN
      idTime = i 
      EXIT   
    END IF
  ELSE !standard_name is not defined: search for variable named 'time'
     !ncStatus = nf90_inq_varid (ncId, 'time', varid = i )
     ncstatus = nf90_inquire_variable(ncId, varId = i, name = variableName)
     IF (LEN_TRIM(variableName) == 4 .AND. &
         variableName(1:4) == 'time') THEN !variable 'time' found
       idTime = i 
       EXIT 
     END IF
  END IF
END DO

!inquire time length
ncStatus = nf90_inquire_variable(ncid, idTime, dimids = dimIDs)
CALL ncErrorHandler (ncStatus)

ncStatus = nf90_inquire_dimension (ncId, dimid = dimIDs(1), len = length)
CALL ncErrorHandler (ncStatus)

!set next time step
IF (current < length) THEN
  next = current + 1
ELSE
  next = length
END IF

!compute date corresponding to next time step
slice(1) = next
ncStatus = nf90_get_var (ncId, idTime, timeSpan , start = slice)
CALL ncErrorHandler (ncStatus)

IF (DateTimeIsDefault(refTime)) THEN
  !timespan contains date and time information
  string = ToString (timeSpan)
  string = string(1:4) // '-' // &
           string(5:6) // '-' // &
           string(7:8) // 'T' // &
           string(9:10) // ':00:00+00:00'
  time = string  
ELSE
    SELECT CASE (timeUnit)
      CASE ('minutes')
        timeSpan = timeSpan * minute
      CASE ('hours')
        timeSpan = timeSpan * hour
      CASE ('days')
        timeSpan = timeSpan * day
      CASE ('months')
        timeSpan = timeSpan * month
    END SELECT

    time = refTime + timeSpan

END IF


END FUNCTION NextTime

!==============================================================================
!! Description:
!!   Parse units attribute of time variable
!! Limits:
!!   string representing date and time must not contain blanks
SUBROUTINE ParseTime &
!
(ncId, ref_time, time_unit)

USE StringManipulation, ONLY: &
!Imported routines:
StringCompact


IMPLICIT NONE

! Arguments with intent(in):
INTEGER (KIND = short), INTENT(IN)   :: ncId  !!NetCdf Id for the file

! Arguments with intent(out):
TYPE(DateTime), INTENT(OUT) :: ref_time
CHARACTER (LEN = 7), INTENT(OUT) :: time_unit

! local variables:
INTEGER (KIND = short)    :: ncStatus !!error code return by NetCDF routines
INTEGER (KIND = short)    :: nDims !!number of dimensions
INTEGER (KIND = short)    :: nVars !!number of variables
INTEGER (KIND = short)    :: nAtts !!number of global attributes
INTEGER (KIND = short)    :: idTime !!Id of the variable containing 
                                    !!information on time ccordinate
CHARACTER (LEN = 80)      :: attribute
CHARACTER (LEN = 25)      :: timeString
CHARACTER (LEN = 80)      :: unit
CHARACTER (LEN = 10)      :: since
CHARACTER (LEN = 100)     :: variableName
INTEGER (KIND = short)    :: i !!loop index

!------------end of declaration------------------------------------------------
 
!inquire dataset to retrieve number of dimensions, variables 
!and global attributes
ncStatus = nf90_inquire(ncId, nDimensions = nDims, &
                        nVariables = nVars,        &
                        nAttributes = nAtts        )
                  
CALL ncErrorHandler (ncStatus)

DO i = 1, nVars
  attribute = ''
  ncStatus = nf90_get_att (ncId, varid = i, name = 'standard_name', &
                           values = attribute)
  
  IF (ncStatus == nf90_noerr) THEN !standard_name is defined
    IF ( attribute(1:4) == 'time' ) THEN
      idTime = i 
      EXIT   
    END IF
  ELSE !standard_name is not defined: search for variable named 'time'
     !ncStatus = nf90_inq_varid (ncId, 'time', varid = i )
     ncstatus = nf90_inquire_variable(ncId, varId = i, name = variableName)
     IF (LEN_TRIM(variableName) == 4 .AND. &
         variableName(1:4) == 'time') THEN !variable 'time' found
       idTime = i 
       EXIT 
     END IF
  END IF
END DO


!get units attribute
ncStatus = nf90_get_att (ncId, varid = idTime, name = 'units', &
                           values = attribute)


READ(attribute,*) unit

i = INDEX (attribute, 'since', BACK = .TRUE.)


IF (i > 0 ) THEN !attribute contains 'since'
                       
    attribute = attribute ( i+6:LEN_TRIM(attribute) )

     timeString = ''

    IF ( SCAN (attribute, '+') /= 0 ) THEN !.OR. SCAN (attribute, '-') /= 0 ) THEN
         !string probably is in the form: 
         !1961-01-01T00:00:00+00:00 or
         !1961-01-01 00:00:00+00:00
         timeString = attribute (1:25)
         timeString(11:11) = 'T'
    ELSE
         !string probably is in the form: 
         !1961-01-01T00:00:00 or
         !1961-01-01 00:00:00
         timeString(1:19) = attribute (1:19)
         !suppose time zone is UTC
         timeString (20:25) = '+00:00'
         timeString(11:11) = 'T'
         IF (timeString(8:8) /= '-') THEN
           !only year and month are defined
           timeString(8:25) = '-01T00:00:00+00:00'
         ELSE IF (timeString(17:19) == '   ') THEN
           !second is missing: set to 0
           timeString(17:19) = ':00'
         END IF
    END IF

    !set reference time. Convert to UTC
    ref_time = timeString
    ref_time = ToUTC (ref_time)
ELSE !attribute is in the form like "day as %Y%m%d%h"
    ref_time = timeDefault
END IF

!set time_unit
SELECT CASE (unit)
  CASE ('seconds', 'second', 'sec', 's')
    time_unit = 'seconds'
  CASE ('minutes', 'minute', 'min')
    time_unit = 'minutes'
  CASE ('hours', 'hour', 'hr', 'h')
    time_unit = 'hours'
  CASE ('days', 'day', 'd')
    time_unit = 'days'
   CASE ('months', 'month')
    time_unit = 'months'
END SELECT

RETURN

END SUBROUTINE


!==============================================================================
!! Description:
!!   get grid mapping for a floating point grid
FUNCTION GetGridMappingFloat &
!
( layer ) &
!
RESULT (grid_mapping)

IMPLICIT NONE
!Arguments with intent(in):
TYPE (grid_real), INTENT(INOUT) :: layer

!Local variables
TYPE (CRS) :: grid_mapping

!------------end of declaration------------------------------------------------

grid_mapping = layer % grid_mapping

END FUNCTION GetGridMappingFloat

!==============================================================================
!! Description:
!!   get grid mapping for a floating point grid
FUNCTION GetGridMappingInteger &
!
( layer ) &
!
RESULT (grid_mapping)

IMPLICIT NONE
!Arguments with intent(in):
TYPE (grid_integer), INTENT(INOUT) :: layer

!Local variables
TYPE (CRS) :: grid_mapping

!------------end of declaration------------------------------------------------

grid_mapping = layer % grid_mapping

END FUNCTION GetGridMappingInteger


!==============================================================================
!! Description:
!!   deallocate float grid
SUBROUTINE GridDestroyFloat &
!
(layer)

IMPLICIT NONE

! Arguments with intent(inout):
TYPE (grid_real), INTENT(INOUT) :: layer

! Local variables:
INTEGER (KIND = short)          :: ios 
!------------end of declaration------------------------------------------------

!deallocate data
IF ( ALLOCATED (layer % mat) ) THEN
  DEALLOCATE ( layer % mat, STAT = ios )
  IF (ios /= 0) THEN
    CALL Catch ('error', 'GridLib',      &
    'memory deallocating grid float ' ,  &
    code = memAllocError                 )
  END IF
END IF

!deallocate coordinate reference system
IF ( ALLOCATED ( layer % grid_mapping % param) ) THEN
  DEALLOCATE ( layer % grid_mapping % param, STAT = ios )
  IF (ios /= 0) THEN
    CALL Catch ('error', 'GridLib',      &
    'memory deallocating grid float ' ,  &
    code = memAllocError                 )
  END IF
END IF

IF ( ALLOCATED ( layer % grid_mapping % description) ) THEN
  DEALLOCATE ( layer % grid_mapping % description, STAT = ios )
  IF (ios /= 0) THEN
    CALL Catch ('error', 'GridLib',      &
    'memory deallocating grid float ' ,  &
    code = memAllocError                 )
  END IF
END IF

END SUBROUTINE GridDestroyFloat

!==============================================================================
!! Description:
!!   deallocate integer grid
SUBROUTINE GridDestroyInteger &
!
(layer)

IMPLICIT NONE

! Arguments with intent(inout):
TYPE (grid_integer), INTENT(INOUT) :: layer

! Local variables:
INTEGER (KIND = short)          :: ios 
!------------end of declaration------------------------------------------------

IF ( ALLOCATED (layer % mat) ) THEN
  DEALLOCATE ( layer % mat, STAT = ios )
  IF (ios /= 0) THEN
    CALL Catch ('error', 'GridLib',      &
    'memory deallocating grid integer ' ,  &
    code = memAllocError                 )
  END IF
END IF

!deallocate coordinate reference system
IF ( ALLOCATED ( layer % grid_mapping % param) ) THEN
  DEALLOCATE ( layer % grid_mapping % param, STAT = ios )
  IF (ios /= 0) THEN
    CALL Catch ('error', 'GridLib',      &
    'memory deallocating grid float ' ,  &
    code = memAllocError                 )
  END IF
END IF

IF ( ALLOCATED ( layer % grid_mapping % description) ) THEN
  DEALLOCATE ( layer % grid_mapping % description, STAT = ios )
  IF (ios /= 0) THEN
    CALL Catch ('error', 'GridLib',      &
    'memory deallocating grid float ' ,  &
    code = memAllocError                 )
  END IF
END IF

END SUBROUTINE GridDestroyInteger

!==============================================================================
!! Description:
!!   transport matrix from netcdf format to grid_real
SUBROUTINE SwapGridRealForward &
!
(matIn, matOut)

IMPLICIT NONE

!arguments with intent in:
REAL (KIND = float), INTENT (IN) :: matIn(:,:)

!arguments with intent out:
REAL (KIND = float), INTENT (OUT) :: matOut(:,:)

!local variables:
INTEGER :: i,j,idim,jdim
!----------------------end of declaration--------------------------------------
idim = SIZE (matOut,1)
jdim = SIZE (matOut,2)
DO i = 1, idim
  DO j = 1, jdim
    matOut (i,j) = matIn (j,idim - i + 1)
  END DO
END DO

END SUBROUTINE SwapGridRealForward


!==============================================================================
!! Description:
!!   transport matrix from netcdf format to grid_integer
SUBROUTINE SwapGridIntegerForward &
!
(matIn, matOut)

IMPLICIT NONE

!arguments with intent in:
INTEGER (KIND = long), INTENT (IN) :: matIn(:,:)

!arguments with intent out:
INTEGER (KIND = long), INTENT (OUT) :: matOut(:,:)

!local variables:
INTEGER :: i,j,idim,jdim
!----------------------end of declaration--------------------------------------
idim = SIZE (matOut,1)
jdim = SIZE (matOut,2)
DO i = 1, idim
  DO j = 1, jdim
    matOut (i,j) = matIn (j,idim - i + 1)
  END DO
END DO

END SUBROUTINE SwapGridIntegerForward


!==============================================================================
!! Description:
!!   transport matrix from grid_real to netcdf format
SUBROUTINE SwapGridRealBack &
!
(matIn, matOut)

IMPLICIT NONE

!arguments with intent in:
REAL (KIND = float), INTENT (IN) :: matIn(:,:)

!arguments with intent out:
REAL (KIND = float), INTENT (OUT) :: matOut(:,:)

!local variables:
INTEGER :: i,j,idim,jdim
!----------------------end of declaration--------------------------------------
idim = SIZE (matOut,1)
jdim = SIZE (matOut,2)
DO i = 1, idim
  DO j = 1, jdim
    matOut (i,j) = matIn (jdim - j + 1,i)
  END DO
END DO

END SUBROUTINE SwapGridRealBack


!==============================================================================
!! Description:
!!   transport matrix from grid_integer to netcdf format
SUBROUTINE SwapGridIntegerBack &
!
(matIn, matOut)

IMPLICIT NONE

!arguments with intent in:
INTEGER (KIND = long), INTENT (IN) :: matIn(:,:)

!arguments with intent out:
INTEGER (KIND = long), INTENT (OUT) :: matOut(:,:)

!local variables:
INTEGER :: i,j,idim,jdim
!----------------------end of declaration--------------------------------------
idim = SIZE (matOut,1)
jdim = SIZE (matOut,2)
DO i = 1, idim
  DO j = 1, jdim
    matOut (i,j) = matIn (jdim - j + 1,i)
  END DO
END DO

END SUBROUTINE SwapGridIntegerBack


!==============================================================================
!! Description:
!!   Handle errors from netcdf related operation
SUBROUTINE ncErrorHandler &
!
(errcode)

IMPLICIT NONE

! Local scalars:
INTEGER (KIND = short), INTENT (IN) :: errcode
!------------end of declaration------------------------------------------------

IF (errcode /= nf90_noerr) THEN
  CALL Catch ('error', 'GridLib',  &
  TRIM (nf90_strerror (errcode) ),  &
  code = ncIOError                   )
ENDIF

END SUBROUTINE ncErrorHandler

!! Description:
!!  given filename of a multidimensional net-cdf file
!!  the GetDtGrid function returns the temporal resolution (seconds)
!!  assuming that it is regular
!!  If option checkRegular is true the function check that
!!  temporal resolution is regular
FUNCTION GetDtGrid &
!
(filename, checkRegular) &
!
RESULT (dt)

USE Units, ONLY: &
! Imported parameters:
minute, hour, day, month

USE StringManipulation, ONLY: &
! imported routines:
ToString

IMPLICIT NONE

!Arguments with intent(in):
CHARACTER (LEN = *), INTENT(IN) :: filename
LOGICAL, OPTIONAL, INTENT(IN)   :: checkRegular

!Local declarations:
INTEGER (KIND = long)  :: dt
INTEGER (KIND = short) :: ncStatus !!error code returned by NetCDF routines
INTEGER (KIND = short) :: ncId  !!NetCdf Id for the file
INTEGER (KIND = short) :: nDims !!number of dimensions
INTEGER (KIND = short) :: nVars !!number of variables
INTEGER (KIND = short) :: nAtts !!number of global attributes
INTEGER (KIND = short) :: timeDimId !!id of time dimension
INTEGER (KIND = short) :: length !!length of time dimension
INTEGER (KIND = short) :: idTime !!Id of the variable containing 
                                 !!information on time coordinate         
CHARACTER (LEN = 80)   :: attribute
CHARACTER (LEN = 25)   :: timeString
CHARACTER (LEN = 100)  :: variableName
TYPE(DateTime)         :: ref_time
TYPE(DateTime)         :: date1, date2
CHARACTER (LEN = 7)    :: time_unit
INTEGER                :: slice (1)
INTEGER                :: time1, time2
INTEGER                :: timeSpan
INTEGER (KIND = short) :: i
INTEGER, DIMENSION(NF90_MAX_VAR_DIMS) :: dimIDs

!------------end of declaration------------------------------------------------

!open net-cdf file with read-only access
ncStatus = nf90_open (fileName, NF90_NOWRITE, ncId)
IF (ncStatus /= nf90_noerr) THEN
  CALL Catch ('error', 'GridLib',        &
  TRIM (nf90_strerror (ncStatus) )//': ',  &
  code = ncIOError, argument = fileName )
ENDIF

!retrieve time unit
CALL ParseTime (ncId, ref_time, time_unit)

!inquire dataset to retrieve number of dimensions, variables 
!and global attributes
ncStatus = nf90_inquire(ncId, nDimensions = nDims, &
                        nVariables = nVars,        &
                        nAttributes = nAtts        )
                  
CALL ncErrorHandler (ncStatus)

!search for time variable
DO i = 1, nVars
  attribute = ''
  ncStatus = nf90_get_att (ncId, varid = i, name = 'standard_name', &
                           values = attribute)
  
  IF (ncStatus == nf90_noerr) THEN !standard_name is defined
    IF ( attribute(1:4) == 'time' ) THEN
      idTime = i 
      EXIT   
    END IF
  ELSE !standard_name is not defined: search for variable named 'time'
     !ncStatus = nf90_inq_varid (ncId, 'time', varid = i )
     ncstatus = nf90_inquire_variable(ncId, varId = i, name = variableName)
     IF (LEN_TRIM(variableName) == 4 .AND. &
         variableName(1:4) == 'time') THEN !variable 'time' found
       idTime = i 
       EXIT 
     END IF
  END IF
END DO

!inquire time length
ncStatus = nf90_inquire_variable(ncid, idTime, dimids = dimIDs)
CALL ncErrorHandler (ncStatus)

ncStatus = nf90_inquire_dimension (ncId, dimid = dimIDs(1), len = length)
CALL ncErrorHandler (ncStatus)

!compute dt
slice(1) = 1
ncStatus = nf90_get_var (ncId, idTime, time1 , start = slice)
CALL ncErrorHandler (ncStatus)
 
slice(1) = 2
ncStatus = nf90_get_var (ncId, idTime, time2 , start = slice)
CALL ncErrorHandler (ncStatus)

IF (DateTimeIsDefault(ref_time)) THEN
  timeString = ToString (time1)
  timeString = timeString (1:4) // '-' // &
               timeString (5:6) // '-' // &
               timeString (7:8) // 'T' // &
               timeString (9:10) // ':00:00+00:00'
  date1 = timeString
  
  timeString = ToString (time2)
  timeString = timeString (1:4) // '-' // &
               timeString (5:6) // '-' // &
               timeString (7:8) // 'T' // &
               timeString (9:10) // ':00:00+00:00'
  date2 = timeString
  dt = date2 - date1
  
ELSE
  dt = time2 - time1
  !convert in seconds
    SELECT CASE (time_unit)
      CASE ('minutes')
        dt = dt * minute
      CASE ('hours')
        dt = dt * hour
      CASE ('days')
        dt = dt * day
      CASE ('months')
        dt = dt * month
    END SELECT
END IF

!Check if dt is regular
IF (PRESENT (checkRegular) ) THEN
  IF (checkRegular) THEN
    DO i = 1, length - 1
      slice(1) = i
      ncStatus = nf90_get_var (ncId, idTime, time1 , start = slice)
      CALL ncErrorHandler (ncStatus)
      
      slice(1) = i + 1
      ncStatus = nf90_get_var (ncId, idTime, time2 , start = slice)
      CALL ncErrorHandler (ncStatus)
      
      IF (DateTimeIsDefault(ref_time)) THEN
          timeString = ToString (time1)
          timeString = timeString (1:4) // '-' // &
                       timeString (5:6) // '-' // &
                       timeString (7:8) // 'T' // &
                       timeString (9:10) // ':00:00+00:00'
          date1 = timeString
          
          timeString = ToString (time2)
          timeString = timeString (1:4) // '-' // &
                       timeString (5:6) // '-' // &
                       timeString (7:8) // 'T' // &
                       timeString (9:10) // ':00:00+00:00'
          date2 = timeString
          
          timeSpan = date2 - date1
          
      ELSE
          timeSpan = time2 - time1
          !convert in seconds
            SELECT CASE (time_unit)
              CASE ('minutes')
                timeSpan = timeSpan * minute
              CASE ('hours')
                timeSpan = timeSpan * hour
              CASE ('days')
                timeSpan = timeSpan * day
              CASE ('months')
                timeSpan = timeSpan * month
            END SELECT
      END IF
       
      IF (timeSpan /= dt ) THEN
        CALL Catch ('error', 'GridLib',        &
            'time not regular in multidimensional grid')
      END IF
    
    END DO
  END IF
END IF


END FUNCTION GetDtGrid


END MODULE GridLib
