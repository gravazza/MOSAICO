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
!!   library to operate on grids
!! @author <a href="mailto:giovanni.ravazzani@polimi.it">Giovanni Ravazzani</a> 
!! @version 0.7 - 19th October 2011 
MODULE GridOperations         
			
! History: 
!  
! Version   Date                Comment 
! -------       ----                    ------- 
!  0.1        29/Jun/2009   Original code. giovanni ravazzani
!  0.2        09/Aug/2009   Added GridByIni
!  0.3        07/Apr/2010   Added GridResample
!  0.4        16/Oct/2010   function to compute cell area and distance
!  0.5        03/Feb/2011   Distance moved to GeoLib
!  0.6        01/Aug/2011   optional argument for cellsize output from GridConvert
!  0.7        19/Oct/2011   added getSum routines

! Modules used: 

USE DataTypeSizes, ONLY: &
! Imported type definitions:
short, long, float 

USE LogLib, ONLY: &
! Imported routines:
Catch

USE ErrorCodes, ONLY: &
!Imported parameters:
memAllocError, unknownOption
 
USE GridLib, ONLY: &
!Imported type definitions:
grid_integer, grid_real, &
! Imported routines:
NewGrid, &
!Imported parameters:
ESRI_ASCII, ESRI_BINARY, NET_CDF

USE GeoLib, ONLY: &
!Imported operators:
OPERATOR (==)


IMPLICIT NONE 

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE  AssignGridReal
  MODULE PROCEDURE  AssignGridInteger
  MODULE PROCEDURE  AssignReal
  MODULE PROCEDURE  AssignInteger
END INTERFACE

INTERFACE GridConvert
   MODULE PROCEDURE GridConvertFloat
   MODULE PROCEDURE GridConvertInteger
END INTERFACE

INTERFACE GetXY
   MODULE PROCEDURE GetXYFloat
   MODULE PROCEDURE GetXYInteger
END INTERFACE

INTERFACE GetIJ
   MODULE PROCEDURE GetIJFloat
   MODULE PROCEDURE GetIJInteger
END INTERFACE

INTERFACE CRSisEqual
   MODULE PROCEDURE CRSisEqualIntInt
   MODULE PROCEDURE CRSisEqualFloatFloat
   MODULE PROCEDURE CRSisEqualFloatInt
   MODULE PROCEDURE CRSisEqualIntFloat
END INTERFACE

INTERFACE GridByIni
   MODULE PROCEDURE GridByIniFloat
   MODULE PROCEDURE GridByIniInteger
   MODULE PROCEDURE GridByIniFloatSubSection
   MODULE PROCEDURE GridByIniIntegerSubSection
END INTERFACE

INTERFACE IsOutOfGrid
   MODULE PROCEDURE IsOutOfGridFloat
   MODULE PROCEDURE IsOutOfGridInteger
END INTERFACE

INTERFACE ExtractBorder
   MODULE PROCEDURE ExtractBorderFloat
   MODULE PROCEDURE ExtractBorderInteger
END INTERFACE

INTERFACE GridResample
   MODULE PROCEDURE ResampleFloatCell
   MODULE PROCEDURE ResampleIntegerCell
   MODULE PROCEDURE ResampleFloat
   MODULE PROCEDURE ResampleInteger
END INTERFACE

INTERFACE CellArea
  MODULE PROCEDURE CellAreaFloat
  MODULE PROCEDURE CellAreaInteger
END INTERFACE

INTERFACE GetMean
   MODULE PROCEDURE GetMeanOfGridFloat
   MODULE PROCEDURE GetMeanOfGridInteger
END INTERFACE

INTERFACE GetSum
   MODULE PROCEDURE GetSumOfGridFloat
   MODULE PROCEDURE GetSumOfGridInteger
END INTERFACE


! Local (i.e. private) Declarations: 
! Local Procedures:
PRIVATE :: GridConvertFloat
PRIVATE :: GridConvertInteger
PRIVATE :: CRSisEqualFloatFloat
PRIVATE :: CRSisEqualIntInt
PRIVATE :: CRSisEqualIntFloat
PRIVATE :: CRSisEqualFloatInt
PRIVATE :: GetXYinteger
PRIVATE :: GetXYFloat
PRIVATE :: GetIJinteger
PRIVATE :: GetIJFloat
PRIVATE :: GridByIniFloat
PRIVATE :: GridByIniInteger
PRIVATE :: IsOutOfGridInteger
PRIVATE :: IsOutOfGridFloat
PRIVATE :: ExtractBorderFloat
PRIVATE :: ExtractBorderInteger
PRIVATE :: ResampleFloatCell
PRIVATE :: ResampleIntegerCell
PRIVATE :: ResampleFloat
PRIVATE :: ResampleInteger
PRIVATE :: AssignGridReal
PRIVATE :: AssignGridInteger
PRIVATE :: AssignReal
PRIVATE :: AssignInteger
PRIVATE :: CellAreaFloat
PRIVATE :: CellAreaInteger
PRIVATE :: GetMeanOfGridFloat
PRIVATE :: GetMeanOfGridInteger
PRIVATE :: GetSumOfGridFloat
PRIVATE :: GetSumOfGridInteger

!=======         
CONTAINS
!======= 
! Define procedures contained in this module. 

!------------------------------------------------------------------------------
!! Description
!!   compute area [m2] of a cell of a grid as a function of latitude defined by
!!   the position of cell in local coordinate system (row, column).
!!    Input grid of type grid_real
!!   Reference: Sivakholundu, K. M., Prabaharan, N. (1998). A program to
!!              compute the area of an irregular polygon on a spheroidal surface,
!!              Computers & Geosciences, 24(8), 823-826.
FUNCTION CellAreaFloat &
!
(gridIn, i, j) &
!
RESULT(cellarea)

USE GeoLib , ONLY: &
!Imported parameters:
GEODETIC

USE Units, ONLY: &
!Imported parameters:
degToRad, squareKilometer

!arguments with intent(in):
TYPE(grid_real), INTENT (IN)   :: gridIn  
INTEGER, INTENT (IN) :: i,j !!row and column of cell 

!Local variables
REAL (KIND = float) :: midLatitude !mid-latitude of cell
REAL (KIND = float) :: cellWidht !cell width in angular terms
REAL (KIND = float) :: re !!semi-major axis of spheroid
REAL (KIND = float) :: ecc !!eccentricity of spheroid
REAL (KIND = float) :: cellarea
REAL (KIND = float) :: x, y
LOGICAL             :: check


SELECT CASE (gridIn % grid_mapping  % system)
  CASE (GEODETIC)
    
    CALL GetXY (i, j, gridIn, x, y, check)
    IF (.NOT. check) THEN
       CALL Catch ('error', 'GridOperations',  &
       'cell out of grid calculating cell area' )
    ENDIF   
    
    midLatitude = y * degToRad
    cellWidht = gridIn % cellsize * degToRad
    re = gridIn % grid_mapping % ellipsoid % a
    ecc = gridIn % grid_mapping % ellipsoid % e 
    cellarea = re**2. * (1.-ecc**2.) * SIN(cellWidht)**2. * COS(midLatitude) / &
                    (1.-ecc**2.*SIN(midLatitude)**2.)**2.
  CASE DEFAULT
    cellarea = gridIn % cellsize ** 2.
  END SELECT

END FUNCTION CellAreaFloat


!------------------------------------------------------------------------------
!! Description
!!   compute area [m2] of a cell of a grid as a function of latitude defined by
!!   the position of cell in local coordinate system (row, column).
!!    Input grid of type grid_integer
!!   Reference: Sivakholundu, K. M., Prabaharan, N. (1998). A program to
!!              compute the area of an irregular polygon on a spheroidal surface,
!!              Computers & Geosciences, 24(8), 823-826.
FUNCTION CellAreaInteger &
!
(gridIn, i, j) &
!
RESULT(cellarea)

USE GeoLib , ONLY: &
!Imported parameters:
GEODETIC

USE Units, ONLY: &
!Imported parameters:
degToRad, squareKilometer

!arguments with intent(in):
TYPE(grid_integer), INTENT (IN)   :: gridIn  
INTEGER, INTENT (IN) :: i,j !!row and column of cell 

!Local variables
REAL (KIND = float) :: midLatitude !mid-latitude of cell
REAL (KIND = float) :: cellWidht !cell width in angular terms
REAL (KIND = float) :: re !!semi-major axis of spheroid
REAL (KIND = float) :: ecc !!eccentricity of spheroid
REAL (KIND = float) :: cellarea
REAL (KIND = float) :: x, y
LOGICAL             :: check


SELECT CASE (gridIn % grid_mapping  % system)
  CASE (GEODETIC)
    
    CALL GetXY (i, j, gridIn, x, y, check)
    IF (.NOT. check) THEN
       CALL Catch ('error', 'GridOperations',  &
       'cell out of grid calculating cell area' )
    ENDIF   
    
    midLatitude = y * degToRad
    cellWidht = gridIn % cellsize * degToRad
    re = gridIn % grid_mapping % ellipsoid % a
    ecc = gridIn % grid_mapping % ellipsoid % e 
    cellarea = re**2. * (1.-ecc**2.) * SIN(cellWidht)**2. * COS(midLatitude) / &
                    (1.-ecc**2.*SIN(midLatitude)**2.)**2.
  CASE DEFAULT
    cellarea = gridIn % cellsize ** 2.
  END SELECT

END FUNCTION CellAreaInteger


!==============================================================================
!! Description:
!!  coordinate conversion of a grid_real
!!  definition of corner points:
!!
!!    A---------B
!!    |         |
!!    |         |
!!    |         |
!!    C---------D
SUBROUTINE GridConvertFloat &
!
(GridIn, GridOut, cellsize)

USE GeoLib, ONLY: &
!Imported type definitions:
Coordinate, &
! Imported routines:
SetCoord, Convert, ASSIGNMENT(=)


!arguments with intent(in):
TYPE (grid_real), INTENT (IN) :: GridIn
REAL (KIND = float), OPTIONAL, INTENT (IN) :: cellsize

!arguments with intent (inout):
TYPE (grid_real), INTENT (INOUT) :: GridOut

!local variables:
TYPE (Coordinate) :: Ain, Bin, Cin, Din !!corner points of input grid
TYPE (Coordinate) :: Aout, Bout, Cout, Dout !!corner points converted to new CRS
TYPE (Coordinate) :: Arec, Brec, Crec, Drec !!corner points of rectangle
TYPE (Coordinate) :: pointNew !!generic point in the new CRS
TYPE (Coordinate) :: pointOld !!generic point in the old CRS
REAL (KIND = float) :: Xdim, Ydim
INTEGER :: newXsize, newYsize
REAL (KIND = float) :: CellSizeX, CellSizeY, newCellSize
INTEGER (KIND = short)  :: ios !!error return code
INTEGER (KIND = short)  :: i, j, iOld, jOld
REAL (KIND = float) :: X, Y
LOGICAL :: checkBound

!------------end of declaration------------------------------------------------

!------------------------------------------------------------------------------
!            calculate corner points
!------------------------------------------------------------------------------
!Set coordinate of corner Ain
CALL SetCoord (GridIn % xllcorner , GridIn % yllcorner + &
               GridIn % idim * GridIn % cellsize, Ain)
!copy coordinate reference system parameters
Ain % system = GridIn % grid_mapping
Aout %system = GridOut % grid_mapping
!calculate coordinate in the new reference system
CALL Convert (Ain, Aout)

!Set coordinate of corner Bin
CALL SetCoord (GridIn % xllcorner + GridIn % jdim * GridIn % cellsize , &
               GridIn % yllcorner + GridIn % idim * GridIn % cellsize, Bin)
!copy corrdinate reference system parameters
Bin % system = GridIn % grid_mapping
Bout % system = GridOut % grid_mapping
!calculate coordinate in the new reference system
CALL Convert (Bin, Bout)

!Set coordinate of corner Cin
CALL SetCoord (GridIn % xllcorner, GridIn % yllcorner, Cin)
!copy coordinate reference system parameters
Cin % system = GridIn % grid_mapping
Cout % system = GridOut % grid_mapping
!calculate coordinate in the new reference system
CALL Convert (Cin, Cout)

!Set coordinate of corner Din
CALL SetCoord (GridIn % xllcorner + GridIn % jdim * GridIn % cellsize, &
                GridIn % yllcorner, Din)
!copy corrdinate reference system parameters
Din % system = GridIn % grid_mapping
Dout % system = GridOut % grid_mapping
!calculate coordinate in the new reference system
CALL Convert (Din, Dout)

!------------------------------------------------------------------------------
!        define minimum rectangular view that contains the 4 converted points
!------------------------------------------------------------------------------
!calculate Arec
CALL SetCoord ( MIN (Aout % easting, Cout % easting),  &
                  MAX (Aout % northing, Bout % northing), Arec )
!copy corrdinate reference system parameters
Arec % system = Aout % system

!calculate Brec
CALL SetCoord ( MAX (Bout % easting, Dout % easting),  &
                  MAX (Aout % northing, Bout % northing), Brec )
!copy corrdinate reference system parameters
Brec % system = Bout % system

!calculate Crec
CALL SetCoord ( MIN (Aout % easting, Cout % easting),  &
                  MIN (Cout % northing, Dout % northing), Crec )
!copy corrdinate reference system parameters
Crec % system = Cout % system

!calculate Drec
CALL SetCoord ( MAX (Bout % easting, Dout % easting),  &
                  MIN (Cout % northing, Dout % northing), Drec )
!copy corrdinate reference system parameters
Drec % system = Dout % system


!------------------------------------------------------------------------------
!            calculate cellsize of gridout, number of rows and columns
!------------------------------------------------------------------------------
Xdim = Brec % easting - Arec % easting
Ydim = Brec % northing - Drec % northing
IF (PRESENT (cellsize) ) THEN
  !set cellsize of new grid to cellsize
    newCellSize = cellsize
  !calculate number of columns of new grid
    newXsize = INT ( Xdim / newCellSize ) + 1
  !calculate number of rows of new grid
    newYsize = INT ( Ydim / newCellSize ) + 1
ELSE   
    
    CellSizeX = Xdim / GridIn % jdim

    CellSizeY = Ydim / GridIn % idim

    !set cellsize of new grid to CellSizeX
    newCellSize = CellSizeX 

    !set number of columns of new grid equals to GridIn
    newXsize = GridIn % jdim

    !calculate number of rows of new grid
    newYsize = INT ( Ydim / newCellSize ) + 1
END IF

!------------------------------------------------------------------------------
!                 define new grid
!------------------------------------------------------------------------------
GridOut % jdim = newXsize
GridOut % idim = newYsize
GridOut % standard_name = GridIn % standard_name
GridOut % long_name = GridIn % long_name
GridOut % units = GridIn % units
GridOut % varying_mode = GridIn % varying_mode
GridOut % nodata = GridIn % nodata
GridOut % valid_min = GridIn % valid_min
GridOut % valid_max = GridIn % valid_max
GridOut % reference_time = GridIn % reference_time
GridOut % current_time = GridIn % current_time
GridOut % time_index = GridIn % time_index
GridOut % time_unit = GridIn % time_unit
GridOut % cellsize =  newCellSize
GridOut % xllcorner = Crec % easting
GridOut % yllcorner = Crec % northing
!esri_pe_string !!used by ArcMap 9.2 
IF (ALLOCATED(GridOut % mat)) THEN
  DEALLOCATE (GridOut % mat)
END IF
ALLOCATE ( GridOut % mat ( newYsize, newXsize ), STAT = ios )
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridOperations',  &
  'memory allocation: ',  &
  code = memAllocError,argument = 'converted grid' )
ENDIF   

DO i = 1, GridOut % idim
  DO j = 1, GridOut % jdim
    GridOut % mat (i,j) = GridOut % nodata
  END DO
END DO                                 

!------------------------------------------------------------------------------
!                 fill in new grid with values
!------------------------------------------------------------------------------
!set CRS of pointOld and pointNew
pointOld % system = GridIn % grid_mapping
pointNew % system = GridOut % grid_mapping
!loop
DO i = 1, GridOut % idim
  DO j = 1, GridOut % jdim
    !set pointNew
    CALL GetXY (i, j, GridOut, X, Y, checkBound)
    CALL SetCoord (X, Y, pointNew)
    !calculate pointOld
    CALL Convert (pointNew, pointOld)
    CALL GetIJ (pointOld % easting, pointOld % northing, GridIn, iOld, jOld, checkBound)
    IF (checkBound) THEN 
      IF (GridIn % mat (iOld, jOld) /= GridIn % nodata) THEN
        GridOut % mat (i,j) = GridIn % mat (iOld, jOld) 
      END IF    
    END IF
  END DO
END DO     

END SUBROUTINE GridConvertFloat

!==============================================================================
!! Description:
!!  coordinate conversion of a grid_integer
!!  definition of corner points:
!!
!!    A---------B
!!    |         |
!!    |         |
!!    |         |
!!    C---------D
SUBROUTINE GridConvertInteger &
!
(GridIn, GridOut, cellsize)

USE GeoLib, ONLY: &
!Imported type definitions:
Coordinate, &
! Imported routines:
SetCoord, Convert, ASSIGNMENT(=)


!arguments with intent(in):
TYPE (grid_integer), INTENT (IN) :: GridIn
REAL (KIND = float), OPTIONAL, INTENT (IN) :: cellsize

!arguments with intent (inout):
TYPE (grid_integer), INTENT (INOUT) :: GridOut

!local variables:
TYPE (Coordinate) :: Ain, Bin, Cin, Din !!corner points of input grid
TYPE (Coordinate) :: Aout, Bout, Cout, Dout !!corner points converted to new CRS
TYPE (Coordinate) :: Arec, Brec, Crec, Drec !!corner points of rectangle
TYPE (Coordinate) :: pointNew !!generic point in the new CRS
TYPE (Coordinate) :: pointOld !!generic point in the old CRS
REAL (KIND = float) :: Xdim, Ydim
INTEGER :: newXsize, newYsize
REAL (KIND = float) :: CellSizeX, CellSizeY, newCellSize
INTEGER (KIND = short)  :: ios !!error return code
INTEGER (KIND = short)  :: i, j, iOld, jOld
REAL (KIND = float) :: X, Y
LOGICAL :: checkBound

!------------end of declaration------------------------------------------------

!------------------------------------------------------------------------------
!            calculate corner points
!------------------------------------------------------------------------------
!Set coordinate of corner Ain
CALL SetCoord (GridIn % xllcorner , GridIn % yllcorner + &
               GridIn % idim * GridIn % cellsize, Ain)
!copy corrdinate reference system parameters
Ain % system = GridIn % grid_mapping
Aout %system = GridOut % grid_mapping
!calculate coordinate in the new reference system
CALL Convert (Ain, Aout)

!Set coordinate of corner Bin
CALL SetCoord (GridIn % xllcorner + GridIn % jdim * GridIn % cellsize , &
               GridIn % yllcorner + GridIn % idim * GridIn % cellsize, Bin)
!copy corrdinate reference system parameters
Bin % system = GridIn % grid_mapping
Bout %system = GridOut % grid_mapping
!calculate coordinate in the new reference system
CALL Convert (Bin, Bout)

!Set coordinate of corner Cin
CALL SetCoord (GridIn % xllcorner, GridIn % yllcorner, Cin)
!copy corrdinate reference system parameters
Cin % system = GridIn % grid_mapping
Cout %system = GridOut % grid_mapping
!calculate coordinate in the new reference system
CALL Convert (Cin, Cout)

!Set coordinate of corner Din
CALL SetCoord (GridIn % xllcorner + GridIn % jdim * GridIn % cellsize, &
                GridIn % yllcorner, Din)
!copy corrdinate reference system parameters
Din % system = GridIn % grid_mapping
Dout % system = GridOut % grid_mapping
!calculate coordinate in the new reference system
CALL Convert (Din, Dout)

!------------------------------------------------------------------------------
!        define minimum rectangular view that contains the 4 converted points
!------------------------------------------------------------------------------
!calculate Arec
CALL SetCoord ( MIN (Aout % easting, Cout % easting),  &
                  MAX (Aout % northing, Bout % northing), Arec )
!copy corrdinate reference system parameters
Arec % system = Aout % system

!calculate Brec
CALL SetCoord ( MAX (Bout % easting, Dout % easting),  &
                  MAX (Aout % northing, Bout % northing), Brec )
!copy corrdinate reference system parameters
Brec % system = Bout % system

!calculate Crec
CALL SetCoord ( MIN (Aout % easting, Cout % easting),  &
                  MIN (Cout % northing, Dout % northing), Crec )
!copy corrdinate reference system parameters
Crec % system = Cout % system

!calculate Drec
CALL SetCoord ( MAX (Bout % easting, Dout % easting),  &
                  MIN (Cout % northing, Dout % northing), Drec )
!copy corrdinate reference system parameters
Drec % system = Dout % system


!------------------------------------------------------------------------------
!            calculate cellsize of gridout, number of rows and columns
!------------------------------------------------------------------------------
Xdim = Brec % easting - Arec % easting
Ydim = Brec % northing - Drec % northing
IF (PRESENT (cellsize) ) THEN
  !set cellsize of new grid to cellsize
    newCellSize = cellsize
  !calculate number of columns of new grid
    newXsize = INT ( Xdim / newCellSize ) + 1
  !calculate number of rows of new grid
    newYsize = INT ( Ydim / newCellSize ) + 1
ELSE   
    
    CellSizeX = Xdim / GridIn % jdim

    CellSizeY = Ydim / GridIn % idim

    !set cellsize of new grid to CellSizeX
    newCellSize = CellSizeX 

    !set number of columns of new grid equals to GridIn
    newXsize = GridIn % jdim

    !calculate number of rows of new grid
    newYsize = INT ( Ydim / newCellSize ) + 1
END IF

!------------------------------------------------------------------------------
!                 define new grid
!------------------------------------------------------------------------------
GridOut % jdim = newXsize
GridOut % idim = newYsize
GridOut % standard_name = GridIn % standard_name
GridOut % long_name = GridIn % long_name
GridOut % units = GridIn % units
GridOut % varying_mode = GridIn % varying_mode
GridOut % nodata = GridIn % nodata
GridOut % valid_min = GridIn % valid_min
GridOut % valid_max = GridIn % valid_max
GridOut % reference_time = GridIn % reference_time
GridOut % current_time = GridIn % current_time
GridOut % time_index = GridIn % time_index
GridOut % time_unit = GridIn % time_unit
GridOut % cellsize =  newCellSize
GridOut % xllcorner = Crec % easting
GridOut % yllcorner = Crec % northing
!esri_pe_string !!used by ArcMap 9.2 
IF (ALLOCATED(GridOut % mat)) THEN
  DEALLOCATE (GridOut % mat)
END IF
ALLOCATE ( GridOut % mat ( newYsize, newXsize ), STAT = ios )
IF (ios /= 0) THEN
  CALL Catch ('error', 'GridOperations',  &
  'memory allocation ',  &
  code = memAllocError,argument = 'converted grid' )
ENDIF   

DO i = 1, GridOut % idim
  DO j = 1, GridOut % jdim
    GridOut % mat (i,j) = GridOut % nodata
  END DO
END DO                                 

!------------------------------------------------------------------------------
!                 fill in new grid with values
!------------------------------------------------------------------------------
!set CRS of pointOld and pointNew
pointOld % system = GridIn % grid_mapping
pointNew % system = GridOut % grid_mapping
!loop
DO i = 1, GridOut % idim
  DO j = 1, GridOut % jdim
    !set pointNew
    CALL GetXY (i, j, GridOut, X, Y, checkBound)
    CALL SetCoord (X, Y, pointNew)
    !calculate pointOld
    CALL Convert (pointNew, pointOld)
    CALL GetIJ (pointOld % easting, pointOld % northing, GridIn, iOld, jOld, checkBound)
    IF (checkBound) THEN 
      IF (GridIn % mat (iOld, jOld) /= GridIn % nodata) THEN
        GridOut % mat (i,j) = GridIn % mat (iOld, jOld) 
      END IF    
    END IF
  END DO
END DO     

END SUBROUTINE GridConvertInteger

!==============================================================================
!! Description:
!!  returns X and Y coordinate given i and j position in grid(i,j)
SUBROUTINE GetIJfloat &
!
(X, Y, grid, i, j, check)

IMPLICIT NONE

!Arguments with intent(in):
REAL (KIND = float), INTENT(IN) :: X,Y
TYPE (grid_real), INTENT(IN) :: grid


!Arguments with intent(out):
INTEGER, INTENT(OUT) :: i, j
LOGICAL, OPTIONAL, INTENT(OUT) :: check !!return false if i and j are outside grid definition
!------------end of declaration------------------------------------------------

j = INT ( (X - grid % xllcorner) / grid %cellsize ) + 1 
i = INT ( (grid % yllcorner + grid % idim * grid % cellsize - y) &
          / grid%cellsize ) + 1

IF (PRESENT (check)) THEN
  IF (i < 1 .OR. i > grid % idim .OR. j < 1 .OR. j > grid % jdim ) THEN
    check = .FALSE.
  ELSE
    check = .TRUE.
  END IF
END IF

END SUBROUTINE GetIJfloat


!==============================================================================
!! Description:
!!  returns X and Y coordinate given i and j position in grid(i,j)
SUBROUTINE GetIJinteger &
!
(X, Y, grid, i, j, check)

IMPLICIT NONE

!Arguments with intent(in):
REAL (KIND = float), INTENT(IN) :: X,Y
TYPE (grid_integer), INTENT(IN) :: grid


!Arguments with intent(out):
INTEGER, INTENT(OUT) :: i, j
LOGICAL, OPTIONAL, INTENT(OUT) :: check !!return false if i and j are outside grid definition
!------------end of declaration------------------------------------------------

j = INT ( (X - grid % xllcorner) / grid %cellsize ) + 1 
i = INT ( (grid % yllcorner + grid % idim * grid % cellsize - y) &
          / grid%cellsize ) + 1

IF (PRESENT (check)) THEN
  IF (i < 1 .OR. i > grid % idim .OR. j < 1 .OR. j > grid % jdim ) THEN
    check = .FALSE.
  ELSE
    check = .TRUE.
  END IF
END IF

END SUBROUTINE GetIJinteger


!==============================================================================
!! Description:
!!  returns X and Y coordinate given i and j position in grid(i,j)
SUBROUTINE GetXYfloat &
!
(i, j, grid, X, Y, check)

IMPLICIT NONE

!Arguments with intent(in):
TYPE (grid_real), INTENT(IN) :: grid
INTEGER, INTENT(IN) :: i, j

!Arguments with intent(out):
REAL (KIND = float), INTENT(OUT) :: X,Y
LOGICAL, OPTIONAL, INTENT(OUT) :: check !!return false if i and j are outside grid definition

!------------end of declaration------------------------------------------------


  IF (i < 1 .OR. i > grid % idim .OR. j < 1 .OR. j > grid % jdim ) THEN
    X = 0.
    Y = 0.
    IF ( PRESENT(check) ) check = .FALSE.
  ELSE
    X = grid % xllcorner + (j - 0.5) * grid % cellsize
    Y = grid % yllcorner + (grid % idim - i + 0.5) * grid % cellsize
     
    IF ( PRESENT(check) ) check = .TRUE.
  END IF

END SUBROUTINE GetXYfloat


!==============================================================================
!! Description:
!!  returns X and Y coordinate given i and j position in grid(i,j)
SUBROUTINE GetXYinteger &
!
(i, j, grid, X, Y, check)

IMPLICIT NONE

!Arguments with intent(in):
TYPE (grid_integer), INTENT(IN) :: grid
INTEGER, INTENT(IN) :: i, j

!Arguments with intent(out):
REAL (KIND = float), INTENT(OUT) :: X,Y
LOGICAL, OPTIONAL, INTENT(OUT) :: check !!return false if i and j are outside grid definition

!------------end of declaration------------------------------------------------


  IF (i < 1 .OR. i > grid % idim .OR. j < 1 .OR. j > grid % jdim ) THEN
    X = 0.
    Y = 0.
    IF ( PRESENT(check) ) check = .FALSE.
  ELSE
    X = grid % xllcorner + (j - 0.5) * grid % cellsize
    Y = grid % yllcorner + (grid % idim - i + 0.5) * grid % cellsize
    
    IF ( PRESENT(check) ) check = .TRUE.
  END IF

END SUBROUTINE GetXYinteger

!==============================================================================
!! Description:
!!   return .TRUE. if the two grids have the same Coordinate Reference System,
!!   and the same spatial reference (cellsize, xllxorner, yllcorner, idim, jdim)
!!   If checkCells is given the function checks that grid has
!!   the same active cells of mask.
FUNCTION CRSisEqualFloatFloat &
!
(mask, grid, checkCells) &
!
RESULT (isEqual)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (grid_real), INTENT(IN) :: mask, grid
LOGICAL, OPTIONAL, INTENT(IN) :: checkCells

!Local declarations:
LOGICAL :: isEqual
INTEGER :: i,j
!------------------------end of declaration------------------------------------

IF ( mask % grid_mapping == grid % grid_mapping .AND. &
     mask % cellsize     == grid % cellsize     .AND. &
     mask % xllcorner    == grid % xllcorner    .AND. &
     mask % yllcorner    == grid % yllcorner    .AND. &
     mask % idim         == grid % idim         .AND. &
     mask % jdim         == grid % jdim               ) THEN
   
   isEqual = .TRUE.
   
ELSE
   
   isEqual = .FALSE.
   
END IF

IF ( PRESENT (checkCells) ) THEN
  IF (checkCells) THEN
    DO i = 1, mask % idim
      DO j = 1, mask % jdim
        IF ( mask % mat (i,j) /= mask % nodata .AND. &
             grid % mat (i,j) == grid % nodata  ) THEN
             isEqual = .FALSE.
             EXIT
        END IF
      END DO
    END DO
  END IF
END IF

END FUNCTION CRSisEqualFloatFloat


!==============================================================================
!! Description:
!!   return .TRUE. if the two grids have the same Coordinate Reference System,
!!   and the same spatial reference (cellsize, xllxorner, yllcorner, idim, jdim)
!!   If checkCells is given the function checks that grid has
!!   the same active cells of mask.
FUNCTION CRSisEqualIntInt &
!
(mask, grid, checkCells) &
!
RESULT (isEqual)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (grid_integer), INTENT(IN) :: mask, grid
LOGICAL, OPTIONAL, INTENT(IN) :: checkCells

!Local declarations:
LOGICAL :: isEqual
INTEGER :: i,j
!------------------------end of declaration------------------------------------

IF ( mask % grid_mapping == grid % grid_mapping .AND. &
     mask % cellsize     == grid % cellsize     .AND. &
     mask % xllcorner    == grid % xllcorner    .AND. &
     mask % yllcorner    == grid % yllcorner    .AND. &
     mask % idim         == grid % idim         .AND. &
     mask % jdim         == grid % jdim               ) THEN
   
   isEqual = .TRUE.
   
ELSE
   
   isEqual = .FALSE.
   
END IF

IF ( PRESENT (checkCells) ) THEN
  IF (checkCells) THEN
    DO i = 1, mask % idim
      DO j = 1, mask % jdim
        IF ( mask % mat (i,j) /= mask % nodata .AND. &
             grid % mat (i,j) == grid % nodata  ) THEN
             isEqual = .FALSE.
             EXIT
        END IF
      END DO
    END DO
  END IF
END IF

END FUNCTION CRSisEqualIntInt


!==============================================================================
!! Description:
!!   return .TRUE. if the two grids have the same Coordinate Reference System,
!!   and the same spatial reference (cellsize, xllxorner, yllcorner, idim, jdim)
!!   If checkCells is given the function checks that grid has
!!   the same active cells of mask.
FUNCTION CRSisEqualFloatInt &
!
(mask, grid, checkCells) &
!
RESULT (isEqual)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (grid_real), INTENT(IN) :: mask
TYPE (grid_integer), INTENT(IN) :: grid
LOGICAL, OPTIONAL, INTENT(IN) :: checkCells

!Local declarations:
LOGICAL :: isEqual
INTEGER :: i,j
!------------------------end of declaration------------------------------------

IF ( mask % grid_mapping == grid % grid_mapping .AND. &
     mask % cellsize     == grid % cellsize     .AND. &
     mask % xllcorner    == grid % xllcorner    .AND. &
     mask % yllcorner    == grid % yllcorner    .AND. &
     mask % idim         == grid % idim         .AND. &
     mask % jdim         == grid % jdim               ) THEN
   
   isEqual = .TRUE.
   
ELSE
   
   isEqual = .FALSE.
   
END IF

IF ( PRESENT (checkCells) ) THEN
  IF (checkCells) THEN
    DO i = 1, mask % idim
      DO j = 1, mask % jdim
        IF ( mask % mat (i,j) /= mask % nodata .AND. &
             grid % mat (i,j) == grid % nodata  ) THEN
             isEqual = .FALSE.
             EXIT
        END IF
      END DO
    END DO
  END IF
END IF

END FUNCTION CRSisEqualFloatInt

!==============================================================================
!! Description:
!!   return .TRUE. if the two grids have the same Coordinate Reference System,
!!   and the same spatial reference (cellsize, xllxorner, yllcorner, idim, jdim)
!!   If checkCells is given the function checks that grid has
!!   the same active cells of mask.
FUNCTION CRSisEqualIntFloat &
!
(mask, grid, checkCells) &
!
RESULT (isEqual)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (grid_real), INTENT(IN) :: grid
TYPE (grid_integer), INTENT(IN) :: mask
LOGICAL, OPTIONAL, INTENT(IN) :: checkCells

!Local declarations:
LOGICAL :: isEqual
INTEGER :: i,j
!------------------------end of declaration------------------------------------

IF ( mask % grid_mapping == grid % grid_mapping .AND. &
     mask % cellsize     == grid % cellsize     .AND. &
     mask % xllcorner    == grid % xllcorner    .AND. &
     mask % yllcorner    == grid % yllcorner    .AND. &
     mask % idim         == grid % idim         .AND. &
     mask % jdim         == grid % jdim               ) THEN
   
   isEqual = .TRUE.
   
ELSE
   
   isEqual = .FALSE.
   
END IF

IF ( PRESENT (checkCells) ) THEN
  IF (checkCells) THEN
    DO i = 1, mask % idim
      DO j = 1, mask % jdim
        IF ( mask % mat (i,j) /= mask % nodata .AND. &
             grid % mat (i,j) == grid % nodata  ) THEN
             isEqual = .FALSE.
             EXIT
        END IF
      END DO
    END DO
  END IF
END IF

END FUNCTION CRSisEqualIntFloat


!==============================================================================
!! Description:
!!   read a grid_real using information stored in ini configuration file
SUBROUTINE GridByIniFloat &
!
(ini, grid, section) 

USE Inilib, ONLY: &
!Imported type definitions:
IniList, &
!imported routines:
IniReadString, IniReadReal, KeyIsPresent, IniReadReal, IniReadInt

USE StringManipulation, ONLY: &
!Imported routines:
StringToUpper, StringToLower, StringToShort

USE Chronos, ONLY: &
!Imported type definitions:
DateTime, &
!Imported operands:
ASSIGNMENT( = )

USE GeoLib , ONLY: &
!Imported routines:
SetCRS, ScanDatum, &
SetGeodeticParameters, SetTransverseMercatorParameters, &
SetSwissParameters, &
!Imported parameters:
GEODETIC, TM, SOC, &
EAST, WEST, NORTH, SOUTH, ROME40

USE Units, ONLY: &
!Imported parameters:
degToRad


IMPLICIT NONE

!arguments with intent in:
TYPE (IniList), INTENT(IN) :: ini
CHARACTER (LEN = *), INTENT (IN) :: section

!arguments with intent out:
TYPE (grid_real), INTENT (OUT) :: grid

!local variables:
CHARACTER (LEN = 100) :: fileFormat
CHARACTER (LEN = 300) :: file
CHARACTER (LEN = 100) :: variable  !!variable  to read
CHARACTER (LEN = 100) :: stdName  !!standard name of the variable  to read
CHARACTER (LEN = 100) :: grid_mapping
CHARACTER (LEN = 100) :: datum
TYPE (DateTime)       :: gridTime  !!time of the grid to read
REAL (KIND = float)   :: scale_factor
REAL (KIND = float)   :: offset
REAL (KIND = float)   :: valid_min
REAL (KIND = float)   :: valid_max
REAL (KIND = float)   :: centralMeridian
INTEGER               :: grid_datum
INTEGER (KIND = short) :: utm_zone
INTEGER :: i,j

!-----------------------------end of declaration-------------------------------

  file = IniReadString ('file', ini, section)
  
  IF (KeyIsPresent ('format', ini, section)) THEN
    fileFormat = StringToUpper ( IniReadString ('format', ini, section) )
  ELSE
    CALL Catch ('error', 'GridOperations',  &
    'format not specified for grid: ',  &
     argument = section )
  END IF
  
  !read grid
  IF ( fileFormat == 'ESRI-ASCII' ) THEN
    CALL NewGrid (grid, file, ESRI_ASCII)
  ELSE IF (fileFormat == 'ESRI-BINARY' ) THEN
    CALL NewGrid (grid, file, ESRI_BINARY)
  ELSE IF ( fileFormat == 'NET-CDF' ) THEN
    IF (KeyIsPresent('variable', ini, section)) THEN
      variable = IniReadString ('variable', ini, section) 
      IF (KeyIsPresent('time', ini, section)) THEN
        gridTime = IniReadString ('time', ini, section)
        CALL NewGrid (grid, file, NET_CDF, variable = variable, time = gridTime)
      ELSE
        CALL NewGrid (grid, file, NET_CDF, variable = variable)
      END IF
    ELSE IF (KeyIsPresent('standard_name', ini, section)) THEN
      stdName = IniReadString ('standard_name', ini, section) 
      IF (KeyIsPresent('time', ini, section)) THEN
        gridTime = IniReadString ('time', ini, section)
        CALL NewGrid (grid, file, NET_CDF, stdName = stdName, time = gridtime)
      ELSE
        CALL NewGrid (grid, file, NET_CDF, stdName = stdName)
      END IF
    ELSE
        CALL Catch ('error', 'GridOperations',  &
              'variable or standard name not defined while reading netcdf: ',  &
               argument = section )
    END IF
  ELSE
    CALL Catch ('error', 'GridOperations',  &
                'format not supported: ',  &
                argument = fileFormat )
  END IF
  
  !apply scale factor if given
  IF (KeyIsPresent ('scale_factor', ini, section) ) THEN
    scale_factor = IniReadReal ('scale_factor', ini, section)
    DO i = 1, grid % idim
      DO j = 1, grid % jdim 
        IF ( grid % mat (i,j) /= grid % nodata ) THEN   
          grid % mat (i,j) = grid % mat (i,j) * scale_factor
        END IF
      END DO
    END DO   
  END IF
  
  !add offset if given
  IF (KeyIsPresent ('offset', ini, section) ) THEN
    offset = IniReadReal ('offset', ini, section)
    DO i = 1, grid % idim
      DO j = 1, grid % jdim 
        IF ( grid % mat (i,j) /= grid % nodata ) THEN   
          grid % mat (i,j) = grid % mat (i,j) + offset
        END IF
      END DO
    END DO     
  END IF

  !check upper bound if given
  IF (KeyIsPresent ('valid_max', ini, section) ) THEN
    valid_max = IniReadInt ('valid_max', ini, section)
    DO i = 1, grid % idim
      DO j = 1, grid % jdim 
        IF ( grid % mat (i,j) /= grid % nodata ) THEN
          IF (grid % mat (i,j) > valid_max ) THEN   
            grid % mat (i,j) = valid_max
          END IF
        END IF
      END DO
    END DO     
  END IF

  !check lower bound if given
  IF (KeyIsPresent ('valid_min', ini, section) ) THEN
    valid_min = IniReadInt ('valid_min', ini, section)
    DO i = 1, grid % idim
      DO j = 1, grid % jdim 
        IF ( grid % mat (i,j) /= grid % nodata ) THEN
          IF (grid % mat (i,j) < valid_min ) THEN   
            grid % mat (i,j) = valid_min
          END IF
        END IF
      END DO
    END DO     
  END IF
  
  !read coordinate reference system if given
  IF (KeyIsPresent ('grid_mapping', ini, section) ) THEN
     grid_mapping = IniReadString ('grid_mapping', ini, section)
     IF (KeyIsPresent ('datum', ini, section) ) THEN
       datum = IniReadString ('datum', ini, section)
     ELSE
       datum = 'WGS84'
     END IF
     grid_datum = ScanDatum (datum)
     !set reference system
     IF (StringToUpper(grid_mapping) == 'GEODETIC') THEN
       CALL SetCRS (GEODETIC, grid_datum, grid % grid_mapping)
       !default prime_meridian = 0.
       CALL SetGeodeticParameters (grid % grid_mapping, prime_meridian = 0.0)
     ELSE IF (StringToUpper(grid_mapping(1:11)) == 'GAUSS-BOAGA') THEN
       !gauss boaga is a particular case of transverse-mercator
       CALL SetCRS (TM, ROME40, grid % grid_mapping)
       IF (StringToUpper(grid_mapping(13:16)) == 'EAST') THEN
         CALL SetTransverseMercatorParameters &
             (grid % grid_mapping, lat0 = 0., centM = 15. * degToRad, &
              falseE = 2520000., falseN = 0., k = 0.9996)
       ELSE 
         CALL SetTransverseMercatorParameters &
             (grid % grid_mapping, lat0 = 0., centM = 9. * degToRad, &
              falseE = 1500000., falseN = 0., k = 0.9996)
       END IF
     ELSE IF (StringToUpper(grid_mapping(1:3)) == 'UTM') THEN
         !UTM is a particular case of transverse-mercator
         CALL SetCRS (TM, grid_datum, grid % grid_mapping)
         utm_zone = StringToShort(grid_mapping(4:5))
         IF ( utm_zone >= 31) THEN
            centralMeridian = (6 * utm_zone - 183) * degToRad
         ELSE
            centralMeridian = (6 * utm_zone + 177) * degToRad
         END IF
         IF (StringToUpper(grid_mapping(6:6)) == 'N' ) THEN
           CALL SetTransverseMercatorParameters &
             (grid % grid_mapping, lat0 = 0., centM = centralMeridian, &
              falseE = 500000., falseN = 0., k = 0.9996)
         ELSE
           CALL SetTransverseMercatorParameters &
             (grid % grid_mapping, lat0 = 0., centM = centralMeridian, &
              falseE = 500000., falseN = 10000000., k = 0.9996)
         END IF
         
      ELSE IF (StringToUpper(grid_mapping(1:5)) == 'SWISS') THEN
         CALL SetCRS (SOC, grid_datum, grid % grid_mapping)
         CALL SetSwissParameters &
             (grid % grid_mapping, latc = 0.819474, lonc = 0.129845, &
              azimuth = 1.570796, falseE = 600000., falseN = 200000., k = 1.)
     END IF

  END IF
  
  !varying mode
   IF (KeyIsPresent ('varying_mode', ini, section) ) THEN
   
     grid % varying_mode = StringToLower(IniReadString ('varying_mode', ini, section))
     
     !check option is valid
     IF (grid % varying_mode (1:8) /= 'sequence' .AND. &
         grid % varying_mode (1:6) /= 'linear' ) THEN
         
          CALL Catch ('error', 'GridOperations',  &
           'invalid varying_mode option for grid: ',  &
           code = unknownOption, argument = section )
      
     END IF    
         
   ELSE !default to 'sequence'
   
     grid % varying_mode = 'sequence'
   
   END IF
  
END SUBROUTINE GridByIniFloat


!==============================================================================
!! Description:
!!   read a grid_integer using information stored in ini configuration file
SUBROUTINE GridByIniInteger &
!
(ini, grid, section) 

USE Inilib, ONLY: &
!Imported type definitions:
IniList, &
!imported routines:
IniReadString, IniReadReal, KeyIsPresent, IniReadReal, IniReadInt

USE StringManipulation, ONLY: &
!Imported routines:
StringToUpper, StringToLower, StringToShort

USE Chronos, ONLY: &
!Imported type definitions:
DateTime, &
!Imported operands:
ASSIGNMENT( = )

USE GeoLib , ONLY: &
!Imported routines:
SetCRS, ScanDatum, &
SetGeodeticParameters, SetTransverseMercatorParameters, &
SetSwissParameters, &
!Imported parameters:
GEODETIC, TM, SOC, &
EAST, WEST, NORTH, SOUTH, ROME40

USE Units, ONLY: &
!Imported parameters:
degToRad


IMPLICIT NONE

!arguments with intent in:
TYPE (IniList), INTENT(IN) :: ini
CHARACTER (LEN = *), INTENT (IN) :: section

!arguments with intent out:
TYPE (grid_integer), INTENT (OUT) :: grid

!local variables:
CHARACTER (LEN = 100) :: fileFormat
CHARACTER (LEN = 300) :: file
CHARACTER (LEN = 100) :: variable  !!variable  to read
CHARACTER (LEN = 100) :: stdName  !!standard name of the variable  to read
CHARACTER (LEN = 100) :: grid_mapping
CHARACTER (LEN = 100) :: datum
TYPE (DateTime)       :: gridTime  !!time of the grid to read
REAL (KIND = float)   :: scale_factor
REAL (KIND = float)   :: offset
INTEGER (KIND = long) :: valid_min
INTEGER (KIND = long) :: valid_max
REAL (KIND = float)   :: centralMeridian
INTEGER               :: grid_datum
INTEGER (KIND = short) :: utm_zone
INTEGER :: i,j

!-----------------------------end of declaration-------------------------------

  file = IniReadString ('file', ini, section)
  
  IF (KeyIsPresent ('format', ini, section)) THEN
    fileFormat = StringToUpper ( IniReadString ('format', ini, section) )
  ELSE
    CALL Catch ('error', 'GridOperations',  &
    'format not specified for grid: ',  &
     argument = section )
  END IF
  
  !read grid
  IF ( fileFormat == 'ESRI-ASCII' ) THEN
    CALL NewGrid (grid, file, ESRI_ASCII)
  ELSE IF (fileFormat == 'ESRI-BINARY' ) THEN
    CALL NewGrid (grid, file, ESRI_BINARY)
  ELSE IF ( fileFormat == 'NET-CDF' ) THEN
    IF (KeyIsPresent('variable', ini, section)) THEN
      variable = IniReadString ('variable', ini, section) 
      IF (KeyIsPresent('time', ini, section)) THEN
        gridTime = IniReadString ('time', ini, section)
        CALL NewGrid (grid, file, NET_CDF, variable = variable, time = gridTime)
      ELSE
        CALL NewGrid (grid, file, NET_CDF, variable = variable)
      END IF
    ELSE IF (KeyIsPresent('standard_name', ini, section)) THEN
      stdName = IniReadString ('standard_name', ini, section) 
      IF (KeyIsPresent('time', ini, section)) THEN
        gridTime = IniReadString ('time', ini, section)
        CALL NewGrid (grid, file, NET_CDF, stdName = stdName, time = gridtime)
      ELSE
        CALL NewGrid (grid, file, NET_CDF, stdName = stdName)
      END IF
    ELSE
        CALL Catch ('error', 'GridOperations',  &
              'variable or standard name not defined while reading netcdf: ',  &
               argument = section )
    END IF
  ELSE
    CALL Catch ('error', 'GridOperations',  &
                'format not supported: ',  &
                argument = fileFormat )
  END IF
  
  !apply scale factor if given
  IF (KeyIsPresent ('scale_factor', ini, section) ) THEN
    scale_factor = IniReadReal ('scale_factor', ini, section)
    DO i = 1, grid % idim
      DO j = 1, grid % jdim 
        IF ( grid % mat (i,j) /= grid % nodata ) THEN   
          grid % mat (i,j) = grid % mat (i,j) * scale_factor
        END IF
      END DO
    END DO   
  END IF
  
  !add offset if given
  IF (KeyIsPresent ('offset', ini, section) ) THEN
    offset = IniReadReal ('offset', ini, section)
    DO i = 1, grid % idim
      DO j = 1, grid % jdim 
        IF ( grid % mat (i,j) /= grid % nodata ) THEN   
          grid % mat (i,j) = grid % mat (i,j) + offset
        END IF
      END DO
    END DO     
  END IF

  !check upper bound if given
  IF (KeyIsPresent ('valid_max', ini, section) ) THEN
    valid_max = IniReadInt ('valid_max', ini, section)
    DO i = 1, grid % idim
      DO j = 1, grid % jdim 
        IF ( grid % mat (i,j) /= grid % nodata ) THEN
          IF (grid % mat (i,j) > valid_max ) THEN   
            grid % mat (i,j) = valid_max
          END IF
        END IF
      END DO
    END DO     
  END IF

  !check lower bound if given
  IF (KeyIsPresent ('valid_min', ini, section) ) THEN
    valid_min = IniReadInt ('valid_min', ini, section)
    DO i = 1, grid % idim
      DO j = 1, grid % jdim 
        IF ( grid % mat (i,j) /= grid % nodata ) THEN
          IF (grid % mat (i,j) < valid_min ) THEN   
            grid % mat (i,j) = valid_min
          END IF
        END IF
      END DO
    END DO     
  END IF
  
   !read coordinate reference system if given
  IF (KeyIsPresent ('grid_mapping', ini, section) ) THEN
     grid_mapping = IniReadString ('grid_mapping', ini, section)
     IF (KeyIsPresent ('datum', ini, section) ) THEN
       datum = IniReadString ('datum', ini, section)
     ELSE
       datum = 'WGS84'
     END IF
     grid_datum = ScanDatum (datum)
     !set reference system
     IF (StringToUpper(grid_mapping) == 'GEODETIC') THEN
       CALL SetCRS (GEODETIC, grid_datum, grid % grid_mapping)
       !default prime_meridian = 0.
       CALL SetGeodeticParameters (grid % grid_mapping, prime_meridian = 0.0)
     ELSE IF (StringToUpper(grid_mapping(1:11)) == 'GAUSS-BOAGA') THEN
       !gauss boaga is a particular case of transverse-mercator
       CALL SetCRS (TM, ROME40, grid % grid_mapping)
       IF (StringToUpper(grid_mapping(13:16)) == 'EAST') THEN
         CALL SetTransverseMercatorParameters &
             (grid % grid_mapping, lat0 = 0., centM = 15. * degToRad, &
              falseE = 2520000., falseN = 0., k = 0.9996)
       ELSE 
         CALL SetTransverseMercatorParameters &
             (grid % grid_mapping, lat0 = 0., centM = 9. * degToRad, &
              falseE = 1500000., falseN = 0., k = 0.9996)
       END IF
     ELSE IF (StringToUpper(grid_mapping(1:3)) == 'UTM') THEN
         !UTM is a particular case of transverse-mercator
         CALL SetCRS (TM, grid_datum, grid % grid_mapping)
         utm_zone = StringToShort(grid_mapping(4:5))
         IF ( utm_zone >= 31) THEN
            centralMeridian = (6 * utm_zone - 183) * degToRad
         ELSE
            centralMeridian = (6 * utm_zone + 177) * degToRad
         END IF
         IF (StringToUpper(grid_mapping(6:6)) == 'N' ) THEN
           CALL SetTransverseMercatorParameters &
             (grid % grid_mapping, lat0 = 0., centM = centralMeridian, &
              falseE = 500000., falseN = 0., k = 0.9996)
         ELSE
           CALL SetTransverseMercatorParameters &
             (grid % grid_mapping, lat0 = 0., centM = centralMeridian, &
              falseE = 500000., falseN = 10000000., k = 0.9996)
         END IF
         
     ELSE IF (StringToUpper(grid_mapping(1:5)) == 'SWISS') THEN
         CALL SetCRS (SOC, grid_datum, grid % grid_mapping)
         CALL SetSwissParameters &
             (grid % grid_mapping, latc = 0.819474, lonc = 0.129845, &
              azimuth = 1.570796, falseE = 600000., falseN = 200000., k = 1.)
     END IF

  END IF
  
   !varying mode
   IF (KeyIsPresent ('varying_mode', ini, section) ) THEN
   
     grid % varying_mode = StringToLower(IniReadString ('varying_mode', ini, section))
     
     !check option is valid
     IF (grid % varying_mode /= 'sequence' .OR. &
         grid % varying_mode /= 'linear' ) THEN
         
          CALL Catch ('error', 'GridOperations',  &
           'invalid varying_mode option for grid: ',  &
           code = unknownOption, argument = section )
      
     END IF    
         
   ELSE !default to 'sequence'
   
     grid % varying_mode = 'sequence'
   
   END IF
  
END SUBROUTINE GridByIniInteger


!==============================================================================
!! Description:
!!   read a grid_real using information stored in ini configuration file
!!   defined in subsection [[...]]
SUBROUTINE GridByIniFloatSubSection &
!
(ini, grid, section, subsection) 

USE Inilib, ONLY: &
!Imported type definitions:
IniList, &
!imported routines:
IniReadString, IniReadReal, KeyIsPresent, IniReadReal, IniReadInt

USE StringManipulation, ONLY: &
!Imported routines:
StringToUpper, StringToShort

USE Chronos, ONLY: &
!Imported type definitions:
DateTime, &
!Imported operands:
ASSIGNMENT( = )

USE GeoLib , ONLY: &
!Imported routines:
SetCRS, ScanDatum, &
SetGeodeticParameters, SetGaussBoagaParameters, &
SetUTMparameters, &
!Imported parameters:
GEODETIC, UTM, GAUSS_BOAGA, &
EAST, WEST, NORTH, SOUTH


IMPLICIT NONE

!arguments with intent in:
TYPE (IniList), INTENT(IN) :: ini
CHARACTER (LEN = *), INTENT (IN) :: section
CHARACTER (LEN = *), INTENT (IN) :: subsection

!arguments with intent out:
TYPE (grid_real), INTENT (OUT) :: grid

!local variables:
CHARACTER (LEN = 100) :: fileFormat
CHARACTER (LEN = 300) :: file
CHARACTER (LEN = 100) :: variable  !!variable  to read
CHARACTER (LEN = 100) :: stdName  !!standard name of the variable  to read
CHARACTER (LEN = 100) :: grid_mapping
CHARACTER (LEN = 100) :: datum
TYPE (DateTime)       :: gridTime  !!time of the grid to read
REAL (KIND = float)   :: scale_factor
REAL (KIND = float)   :: offset
REAL (KIND = float)   :: valid_min
REAL (KIND = float)   :: valid_max
INTEGER               :: grid_datum
INTEGER (KIND = short) :: utm_zone
INTEGER :: i,j

!-----------------------------end of declaration-------------------------------

  file = IniReadString ('file', ini, section, subsection)
  
  IF (KeyIsPresent ('format', ini, section, subsection)) THEN
    fileFormat = StringToUpper ( IniReadString ('format', ini, section, subsection) )
  ELSE
    CALL Catch ('error', 'GridOperations',  &
    'format not specified for grid: ',  &
     argument = subsection )
  END IF
  
  !read grid
  IF ( fileFormat == 'ESRI-ASCII' ) THEN
    CALL NewGrid (grid, file, ESRI_ASCII)
  ELSE IF (fileFormat == 'ESRI-BINARY' ) THEN
    CALL NewGrid (grid, file, ESRI_BINARY)
  ELSE IF ( fileFormat == 'NET-CDF' ) THEN
    IF (KeyIsPresent('variable', ini, section, subsection)) THEN
      variable = IniReadString ('variable', ini, section, subsection) 
      IF (KeyIsPresent('time', ini, section, subsection)) THEN
        gridTime = IniReadString ('time', ini, section, subsection)
        CALL NewGrid (grid, file, NET_CDF, variable = variable, time = gridTime)
      ELSE
        CALL NewGrid (grid, file, NET_CDF, variable = variable)
      END IF
    ELSE IF (KeyIsPresent('standard_name', ini, section, subsection)) THEN
      stdName = IniReadString ('standard_name', ini, section, subsection) 
      IF (KeyIsPresent('time', ini, section, subsection)) THEN
        gridTime = IniReadString ('time', ini, section, subsection)
        CALL NewGrid (grid, file, NET_CDF, stdName = stdName, time = gridtime)
      ELSE
        CALL NewGrid (grid, file, NET_CDF, stdName = stdName)
      END IF
    ELSE
        CALL Catch ('error', 'GridOperations',  &
              'variable or standard name not defined while reading netcdf: ',  &
               argument = subsection )
    END IF
  ELSE
    CALL Catch ('error', 'GridOperations',  &
                'format not supported: ',  &
                argument = fileFormat )
  END IF
  
  !apply scale factor if given
  IF (KeyIsPresent ('scale_factor', ini, section, subsection) ) THEN
    scale_factor = IniReadReal ('scale_factor', ini, section, subsection)
    DO i = 1, grid % idim
      DO j = 1, grid % jdim 
        IF ( grid % mat (i,j) /= grid % nodata ) THEN   
          grid % mat (i,j) = grid % mat (i,j) * scale_factor
        END IF
      END DO
    END DO   
  END IF
  
  !add offset if given
  IF (KeyIsPresent ('offset', ini, section, subsection) ) THEN
    offset = IniReadReal ('offset', ini, section, subsection)
    DO i = 1, grid % idim
      DO j = 1, grid % jdim 
        IF ( grid % mat (i,j) /= grid % nodata ) THEN   
          grid % mat (i,j) = grid % mat (i,j) + offset
        END IF
      END DO
    END DO     
  END IF

  !check upper bound if given
  IF (KeyIsPresent ('valid_max', ini, section, subsection) ) THEN
    valid_max = IniReadInt ('valid_max', ini, section, subsection)
    DO i = 1, grid % idim
      DO j = 1, grid % jdim 
        IF ( grid % mat (i,j) /= grid % nodata ) THEN
          IF (grid % mat (i,j) > valid_max ) THEN   
            grid % mat (i,j) = valid_max
          END IF
        END IF
      END DO
    END DO     
  END IF

  !check lower bound if given
  IF (KeyIsPresent ('valid_min', ini, section, subsection) ) THEN
    valid_min = IniReadInt ('valid_min', ini, section, subsection)
    DO i = 1, grid % idim
      DO j = 1, grid % jdim 
        IF ( grid % mat (i,j) /= grid % nodata ) THEN
          IF (grid % mat (i,j) < valid_min ) THEN   
            grid % mat (i,j) = valid_min
          END IF
        END IF
      END DO
    END DO     
  END IF
  
  !read coordinate reference system if given
  IF (KeyIsPresent ('grid_mapping', ini, section, subsection) ) THEN
     grid_mapping = IniReadString ('grid_mapping', ini, section, subsection)
     IF (KeyIsPresent ('datum', ini, section, subsection) ) THEN
       datum = IniReadString ('datum', ini, section, subsection)
     ELSE
       datum = 'WGS84'
     END IF
     grid_datum = ScanDatum (datum)
     !set reference system
     IF (StringToUpper(grid_mapping) == 'GEODETIC') THEN
       CALL SetCRS (GEODETIC, grid_datum, grid % grid_mapping)
       !default prime_meridian = 0.
       CALL SetGeodeticParameters (grid % grid_mapping, prime_meridian = 0.0)
     ELSE IF (StringToUpper(grid_mapping(1:11)) == 'GAUSS-BOAGA') THEN
       CALL SetCRS (GAUSS_BOAGA, grid_datum, grid % grid_mapping)
       IF (StringToUpper(grid_mapping(13:16)) == 'EAST') THEN
         CALL SetGaussBoagaParameters (grid % grid_mapping, fuse = EAST)
       ELSE 
         CALL SetGaussBoagaParameters (grid % grid_mapping, fuse = WEST)
       END IF
     ELSE IF (StringToUpper(grid_mapping(1:3)) == 'UTM') THEN
         CALL SetCRS (UTM, grid_datum, grid % grid_mapping)
         utm_zone = StringToShort(grid_mapping(4:5))
         IF (StringToUpper(grid_mapping(6:6)) == 'N' ) THEN
           CALL SetUTMparameters (grid % grid_mapping, utm_zone, NORTH)
         ELSE
           CALL SetUTMparameters (grid % grid_mapping, utm_zone, SOUTH)
         END IF
     END IF

  END IF
  
END SUBROUTINE GridByIniFloatSubSection


!==============================================================================
!! Description:
!!   read a grid_integer using information stored in ini configuration file
!!   defined in subsection [[.. ]]
SUBROUTINE GridByIniIntegerSubSection &
!
(ini, grid, section, subsection) 

USE Inilib, ONLY: &
!Imported type definitions:
IniList, &
!imported routines:
IniReadString, IniReadReal, KeyIsPresent, IniReadReal, IniReadInt

USE StringManipulation, ONLY: &
!Imported routines:
StringToUpper, StringToShort

USE Chronos, ONLY: &
!Imported type definitions:
DateTime, &
!Imported operands:
ASSIGNMENT( = )

USE GeoLib , ONLY: &
!Imported routines:
SetCRS, ScanDatum, &
SetGeodeticParameters, SetGaussBoagaParameters, &
SetUTMparameters, &
!Imported parameters:
GEODETIC, UTM, GAUSS_BOAGA, &
EAST, WEST, NORTH, SOUTH


IMPLICIT NONE

!arguments with intent in:
TYPE (IniList), INTENT(IN) :: ini
CHARACTER (LEN = *), INTENT (IN) :: section
CHARACTER (LEN = *), INTENT (IN) :: subsection


!arguments with intent out:
TYPE (grid_integer), INTENT (OUT) :: grid

!local variables:
CHARACTER (LEN = 100) :: fileFormat
CHARACTER (LEN = 300) :: file
CHARACTER (LEN = 100) :: variable  !!variable  to read
CHARACTER (LEN = 100) :: stdName  !!standard name of the variable  to read
CHARACTER (LEN = 100) :: grid_mapping
CHARACTER (LEN = 100) :: datum
TYPE (DateTime)       :: gridTime  !!time of the grid to read
REAL (KIND = float)   :: scale_factor
REAL (KIND = float)   :: offset
INTEGER (KIND = long) :: valid_min
INTEGER (KIND = long) :: valid_max
INTEGER               :: grid_datum
INTEGER (KIND = short) :: utm_zone
INTEGER :: i,j

!-----------------------------end of declaration-------------------------------

  file = IniReadString ('file', ini, section, subsection)
  
  IF (KeyIsPresent ('format', ini, section, subsection)) THEN
    fileFormat = StringToUpper ( IniReadString ('format', ini, section, subsection) )
  ELSE
    CALL Catch ('error', 'GridOperations',  &
    'format not specified for grid: ',  &
     argument = subsection )
  END IF
  
  !read grid
  IF ( fileFormat == 'ESRI-ASCII' ) THEN
    CALL NewGrid (grid, file, ESRI_ASCII)
  ELSE IF (fileFormat == 'ESRI-BINARY' ) THEN
    CALL NewGrid (grid, file, ESRI_BINARY)
  ELSE IF ( fileFormat == 'NET-CDF' ) THEN
    IF (KeyIsPresent('variable', ini, section, subsection)) THEN
      variable = IniReadString ('variable', ini, section, subsection) 
      IF (KeyIsPresent('time', ini, section, subsection)) THEN
        gridTime = IniReadString ('time', ini, section, subsection)
        CALL NewGrid (grid, file, NET_CDF, variable = variable, time = gridTime)
      ELSE
        CALL NewGrid (grid, file, NET_CDF, variable = variable)
      END IF
    ELSE IF (KeyIsPresent('standard_name', ini, section, subsection)) THEN
      stdName = IniReadString ('standard_name', ini, section, subsection) 
      IF (KeyIsPresent('time', ini, section, subsection)) THEN
        gridTime = IniReadString ('time', ini, section, subsection)
        CALL NewGrid (grid, file, NET_CDF, stdName = stdName, time = gridtime)
      ELSE
        CALL NewGrid (grid, file, NET_CDF, stdName = stdName)
      END IF
    ELSE
        CALL Catch ('error', 'GridOperations',  &
              'variable or standard name not defined while reading netcdf: ',  &
               argument = subsection )
    END IF
  ELSE
    CALL Catch ('error', 'GridOperations',  &
                'format not supported: ',  &
                argument = fileFormat )
  END IF
  
  !apply scale factor if given
  IF (KeyIsPresent ('scale_factor', ini, section, subsection) ) THEN
    scale_factor = IniReadReal ('scale_factor', ini, section, subsection)
    DO i = 1, grid % idim
      DO j = 1, grid % jdim 
        IF ( grid % mat (i,j) /= grid % nodata ) THEN   
          grid % mat (i,j) = grid % mat (i,j) * scale_factor
        END IF
      END DO
    END DO   
  END IF
  
  !add offset if given
  IF (KeyIsPresent ('offset', ini, section, subsection) ) THEN
    offset = IniReadReal ('offset', ini, section, subsection)
    DO i = 1, grid % idim
      DO j = 1, grid % jdim 
        IF ( grid % mat (i,j) /= grid % nodata ) THEN   
          grid % mat (i,j) = grid % mat (i,j) + offset
        END IF
      END DO
    END DO     
  END IF

  !check upper bound if given
  IF (KeyIsPresent ('valid_max', ini, section, subsection) ) THEN
    valid_max = IniReadInt ('valid_max', ini, section, subsection)
    DO i = 1, grid % idim
      DO j = 1, grid % jdim 
        IF ( grid % mat (i,j) /= grid % nodata ) THEN
          IF (grid % mat (i,j) > valid_max ) THEN   
            grid % mat (i,j) = valid_max
          END IF
        END IF
      END DO
    END DO     
  END IF

  !check lower bound if given
  IF (KeyIsPresent ('valid_min', ini, section, subsection) ) THEN
    valid_min = IniReadInt ('valid_min', ini, section, subsection)
    DO i = 1, grid % idim
      DO j = 1, grid % jdim 
        IF ( grid % mat (i,j) /= grid % nodata ) THEN
          IF (grid % mat (i,j) < valid_min ) THEN   
            grid % mat (i,j) = valid_min
          END IF
        END IF
      END DO
    END DO     
  END IF
  
  !read coordinate reference system if given
  IF (KeyIsPresent ('grid_mapping', ini, section, subsection) ) THEN
     grid_mapping = IniReadString ('grid_mapping', ini, section, subsection)
     IF (KeyIsPresent ('datum', ini, section, subsection) ) THEN
       datum = IniReadString ('datum', ini, section, subsection)
     ELSE
       datum = 'WGS84'
     END IF
     grid_datum = ScanDatum (datum)
     !set reference system
     IF (StringToUpper(grid_mapping) == 'GEODETIC') THEN
       CALL SetCRS (GEODETIC, grid_datum, grid % grid_mapping)
       !default prime_meridian = 0.
       CALL SetGeodeticParameters (grid % grid_mapping, prime_meridian = 0.0)
     ELSE IF (StringToUpper(grid_mapping(1:11)) == 'GAUSS-BOAGA') THEN
       CALL SetCRS (GAUSS_BOAGA, grid_datum, grid % grid_mapping)
       IF (StringToUpper(grid_mapping(13:16)) == 'EAST') THEN
         CALL SetGaussBoagaParameters (grid % grid_mapping, fuse = EAST)
       ELSE 
         CALL SetGaussBoagaParameters (grid % grid_mapping, fuse = WEST)
       END IF
     ELSE IF (StringToUpper(grid_mapping(1:3)) == 'UTM') THEN
         CALL SetCRS (UTM, grid_datum, grid % grid_mapping)
         utm_zone = StringToShort(grid_mapping(4:5))
         IF (StringToUpper(grid_mapping(6:6)) == 'N' ) THEN
           CALL SetUTMparameters (grid % grid_mapping, utm_zone, NORTH)
         ELSE
           CALL SetUTMparameters (grid % grid_mapping, utm_zone, SOUTH)
         END IF
     END IF

  END IF
  
END SUBROUTINE GridByIniIntegerSubSection

!==============================================================================
!! Description:
!!   calculates if cell is out of grid space limits
LOGICAL FUNCTION IsOutOfGridFloat &
!
(i, j, grid) 

IMPLICIT NONE

!Arguments with intent in:
INTEGER, INTENT(IN) :: i,j
TYPE (grid_real), INTENT(IN) :: grid

!----------------------end of declarations-------------------------------------

 IF (i > grid % idim .OR. j > grid % jdim .OR. i < 1 .OR. j < 1) THEN
      IsOutOfGridFloat = .TRUE.
 ELSE
      IsOutOfGridFloat = .FALSE.
 ENDIF

END FUNCTION IsOutOfGridFloat

!==============================================================================
!! Description:
!!   calculates if cell is out of grid space limits
LOGICAL FUNCTION IsOutOfGridInteger &
!
(i, j, grid) 

IMPLICIT NONE

!Arguments with intent in:
INTEGER, INTENT(IN) :: i,j
TYPE (grid_integer), INTENT(IN) :: grid

!----------------------end of declarations-------------------------------------

 IF (i > grid % idim .OR. j > grid % jdim .OR. i < 1 .OR. j < 1) THEN
      IsOutOfGridInteger = .TRUE.
 ELSE
      IsOutOfGridInteger = .FALSE.
 ENDIF

END FUNCTION IsOutOfGridInteger

!==============================================================================
!! Description:
!!   Extracts only the cells on the external border. Other cells are 
!!   assigned nodata. Border cell is the one that has at least a
!!   nodata value in the neighbouring 8 cells.
FUNCTION ExtractBorderFloat &
!
(grid, cardinal) &
!
RESULT (border)

IMPLICIT NONE

!Arguments with intent in:
TYPE (grid_real), INTENT(IN) :: grid
LOGICAL, INTENT(IN), OPTIONAL :: cardinal

!Local declaration:
TYPE (grid_real) :: border
INTEGER :: i,j
LOGICAL :: foundNodata
INTEGER :: row, col
LOGICAL :: fourCells !!true if to consider only four cells in cardinal directions

!---------------------------end of declarations--------------------------------

!Allocate space for grid containing values on the border
CALL NewGrid (border, grid)

IF (PRESENT(cardinal)) THEN
  IF (cardinal) THEN
    fourCells = .TRUE.
  ELSE
    fourCells = .FALSE.
  END IF
ELSE
  fourCells = .FALSE.
END IF

!scan grid
DO i = 1, border % idim
  DO j = 1, border % jdim
    IF (grid % mat (i,j) /= grid % nodata) THEN

          foundNodata = .FALSE.
          
          !check EAST cell
          row = i 
          col = j + 1
          IF ( .NOT. IsOutOfGrid(row,col,border) ) THEN
            IF (grid % mat (row,col) == grid % nodata) THEN
               foundNodata = .TRUE.
               border % mat (i,j) = grid % mat (i,j)
               CYCLE
            END IF
          ELSE
            foundNodata = .TRUE.
            border % mat (i,j) = grid % mat (i,j)
            CYCLE
          END IF
          
          !check SOUTH-EAST cell
          IF ( .NOT. fourCells) THEN
              row = i + 1
              col = j + 1
              IF ( .NOT. IsOutOfGrid(row,col,border) ) THEN
                IF (grid % mat (row,col) == grid % nodata) THEN
                   foundNodata = .TRUE.
                   border % mat (i,j) = grid % mat (i,j)
                   CYCLE
                END IF
              ELSE
                foundNodata = .TRUE.
                border % mat (i,j) = grid % mat (i,j)
                CYCLE
              END IF
          END IF
          !check SOUTH cell
          row = i + 1
          col = j
          IF ( .NOT. IsOutOfGrid(row,col,border) ) THEN
            IF (grid % mat (row,col) == grid % nodata) THEN
               foundNodata = .TRUE.
               border % mat (i,j) = grid % mat (i,j)
               CYCLE
            END IF
          ELSE
            foundNodata = .TRUE.
            border % mat (i,j) = grid % mat (i,j)
            CYCLE
          END IF
          
          !check SOUTH-WEST cell
          IF (.NOT. fourCells) THEN
              row = i + 1
              col = j - 1
              IF ( .NOT. IsOutOfGrid(row,col,border) ) THEN
                IF (grid % mat (row,col) == grid % nodata) THEN
                   foundNodata = .TRUE.
                   border % mat (i,j) = grid % mat (i,j)
                   CYCLE
                END IF
              ELSE
                foundNodata = .TRUE.
                border % mat (i,j) = grid % mat (i,j)
                CYCLE
              END IF
          END IF
          
          !check WEST cell
          row = i 
          col = j - 1
          IF ( .NOT. IsOutOfGrid(row,col,border) ) THEN
            IF (grid % mat (row,col) == grid % nodata) THEN
               foundNodata = .TRUE.
               border % mat (i,j) = grid % mat (i,j)
               CYCLE
            END IF
          ELSE
            foundNodata = .TRUE.
            border % mat (i,j) = grid % mat (i,j)
            CYCLE
          END IF
          
          !check NORTH-EAST cell
          IF (.NOT. fourCells) THEN
              row = i - 1
              col = j - 1
              IF ( .NOT. IsOutOfGrid(row,col,border) ) THEN
                IF (grid % mat (row,col) == grid % nodata) THEN
                   foundNodata = .TRUE.
                   border % mat (i,j) = grid % mat (i,j)
                   CYCLE
                END IF
              ELSE
                foundNodata = .TRUE.
                border % mat (i,j) = grid % mat (i,j)
                CYCLE
              END IF
          END IF
          
          !check NORTH cell
          row = i - 1
          col = j
          IF ( .NOT. IsOutOfGrid(row,col,border) ) THEN
            IF (grid % mat (row,col) == grid % nodata) THEN
               foundNodata = .TRUE.
               border % mat (i,j) = grid % mat (i,j)
               CYCLE
            END IF
          ELSE
            foundNodata = .TRUE.
            border % mat (i,j) = grid % mat (i,j)
            CYCLE
          END IF
          
          !check NORTH-EAST cell
          IF (.NOT. fourCells) THEN
              row = i - 1
              col = j + 1
              IF ( .NOT. IsOutOfGrid(row,col,border) ) THEN
                IF (grid % mat (row,col) == grid % nodata) THEN
                   foundNodata = .TRUE.
                   border % mat (i,j) = grid % mat (i,j)
                   CYCLE
                END IF
              ELSE
                foundNodata = .TRUE.
                border % mat (i,j) = grid % mat (i,j)
                CYCLE
              END IF
          END IF
          
          IF ( .NOT. foundNodata ) THEN
            border % mat (i,j) = border % nodata
          END IF
       
    END IF
  END DO
END DO


END FUNCTION ExtractBorderFloat

!==============================================================================
!! Description:
!!   Extracts only the cells on the external border. Other cells are 
!!   assigned nodata. Border cell is the one that has at least a
!!   nodata value in the neighbouring 8 cells. If cardinal is passed
!!   the routine checks only the four cells in the cardinal direction.
!!   This option is used to obtain border without duplicates. Default is
!!   check all the cells.
FUNCTION ExtractBorderInteger &
!
(grid, cardinal) &
!
RESULT (border)

IMPLICIT NONE

!Arguments with intent in:
TYPE (grid_integer), INTENT(IN) :: grid
LOGICAL, INTENT(IN), OPTIONAL :: cardinal

!Local declaration:
TYPE (grid_integer) :: border
INTEGER :: i,j
LOGICAL :: foundNodata
INTEGER :: row, col
LOGICAL :: fourCells !!true if to consider only four cells in cardinal directions

!---------------------------end of declarations--------------------------------

!Allocate space for grid containing values on the border
CALL NewGrid (border, grid)

IF (PRESENT(cardinal)) THEN
  IF (cardinal) THEN
    fourCells = .TRUE.
  ELSE
    fourCells = .FALSE.
  END IF
ELSE
  fourCells = .FALSE.
END IF

!scan grid
DO i = 1, border % idim
  DO j = 1, border % jdim
    IF (grid % mat (i,j) /= grid % nodata) THEN

          foundNodata = .FALSE.
          
          !check EAST cell
          row = i 
          col = j + 1
          IF ( .NOT. IsOutOfGrid(row,col,border) ) THEN
            IF (grid % mat (row,col) == grid % nodata) THEN
               foundNodata = .TRUE.
               border % mat (i,j) = grid % mat (i,j)
               CYCLE
            END IF
          ELSE
            foundNodata = .TRUE.
            border % mat (i,j) = grid % mat (i,j)
            CYCLE
          END IF
          
          !check SOUTH-EAST cell
          IF ( .NOT. fourCells) THEN
              row = i + 1
              col = j + 1
              IF ( .NOT. IsOutOfGrid(row,col,border) ) THEN
                IF (grid % mat (row,col) == grid % nodata) THEN
                   foundNodata = .TRUE.
                   border % mat (i,j) = grid % mat (i,j)
                   CYCLE
                END IF
              ELSE
                foundNodata = .TRUE.
                border % mat (i,j) = grid % mat (i,j)
                CYCLE
              END IF
          END IF
          !check SOUTH cell
          row = i + 1
          col = j
          IF ( .NOT. IsOutOfGrid(row,col,border) ) THEN
            IF (grid % mat (row,col) == grid % nodata) THEN
               foundNodata = .TRUE.
               border % mat (i,j) = grid % mat (i,j)
               CYCLE
            END IF
          ELSE
            foundNodata = .TRUE.
            border % mat (i,j) = grid % mat (i,j)
            CYCLE
          END IF
          
          !check SOUTH-WEST cell
          IF (.NOT. fourCells) THEN
              row = i + 1
              col = j - 1
              IF ( .NOT. IsOutOfGrid(row,col,border) ) THEN
                IF (grid % mat (row,col) == grid % nodata) THEN
                   foundNodata = .TRUE.
                   border % mat (i,j) = grid % mat (i,j)
                   CYCLE
                END IF
              ELSE
                foundNodata = .TRUE.
                border % mat (i,j) = grid % mat (i,j)
                CYCLE
              END IF
          END IF
          
          !check WEST cell
          row = i 
          col = j - 1
          IF ( .NOT. IsOutOfGrid(row,col,border) ) THEN
            IF (grid % mat (row,col) == grid % nodata) THEN
               foundNodata = .TRUE.
               border % mat (i,j) = grid % mat (i,j)
               CYCLE
            END IF
          ELSE
            foundNodata = .TRUE.
            border % mat (i,j) = grid % mat (i,j)
            CYCLE
          END IF
          
          !check NORTH-EAST cell
          IF (.NOT. fourCells) THEN
              row = i - 1
              col = j - 1
              IF ( .NOT. IsOutOfGrid(row,col,border) ) THEN
                IF (grid % mat (row,col) == grid % nodata) THEN
                   foundNodata = .TRUE.
                   border % mat (i,j) = grid % mat (i,j)
                   CYCLE
                END IF
              ELSE
                foundNodata = .TRUE.
                border % mat (i,j) = grid % mat (i,j)
                CYCLE
              END IF
          END IF
          
          !check NORTH cell
          row = i - 1
          col = j
          IF ( .NOT. IsOutOfGrid(row,col,border) ) THEN
            IF (grid % mat (row,col) == grid % nodata) THEN
               foundNodata = .TRUE.
               border % mat (i,j) = grid % mat (i,j)
               CYCLE
            END IF
          ELSE
            foundNodata = .TRUE.
            border % mat (i,j) = grid % mat (i,j)
            CYCLE
          END IF
          
          !check NORTH-EAST cell
          IF (.NOT. fourCells) THEN
              row = i - 1
              col = j + 1
              IF ( .NOT. IsOutOfGrid(row,col,border) ) THEN
                IF (grid % mat (row,col) == grid % nodata) THEN
                   foundNodata = .TRUE.
                   border % mat (i,j) = grid % mat (i,j)
                   CYCLE
                END IF
              ELSE
                foundNodata = .TRUE.
                border % mat (i,j) = grid % mat (i,j)
                CYCLE
              END IF
          END IF
          
          IF ( .NOT. foundNodata ) THEN
            border % mat (i,j) = border % nodata
          END IF
       
    END IF
  END DO
END DO


END FUNCTION ExtractBorderInteger


!==============================================================================
!! Description:
!!   Create a new grid_real with cellsize different from input grid
!!   The content of the created grid is filled in with nearest neighbor method
SUBROUTINE ResampleFloatCell &
!
(grid, resampledGrid, newCellsize)


IMPLICIT NONE

! Arguments with intent(in):
TYPE (grid_real), INTENT(IN) :: grid
REAL (KIND = float), INTENT(IN) :: newCellsize

!Arguments with intent(out):
TYPE (grid_real), INTENT(OUT) :: resampledGrid

!Local declarations:
REAL :: x, y
INTEGER :: i, j, iold, jold
!---------------------------end of declarations--------------------------------

!compute number of rows and columns of the resampled grid
resampledGrid % idim = INT(grid%cellsize * grid%idim / newCellsize) + 1
resampledGrid % jdim = INT(grid%cellsize * grid%jdim / newCellsize) + 1

!assign spatial information
resampledGrid % cellsize = newCellsize
resampledGrid % xllcorner = grid % xllcorner
resampledGrid % yllcorner = grid % yllcorner

!allocate resampled grid
ALLOCATE ( resampledGrid % mat (resampledGrid%idim, resampledGrid%jdim))

!copy information from grid to resampled grid
resampledGrid % standard_name = grid % standard_name
resampledGrid % long_name = grid % long_name
resampledGrid % units = grid % units
resampledGrid % varying_mode = grid % varying_mode
resampledGrid % nodata = grid % nodata
resampledGrid % valid_min = grid % valid_min
resampledGrid % valid_max = grid % valid_max
resampledGrid % reference_time = grid % reference_time
resampledGrid % current_time = grid % current_time
resampledGrid % time_index = grid % time_index
resampledGrid % time_unit = grid % time_unit
resampledGrid % esri_pe_string = grid % esri_pe_string
resampledGrid % grid_mapping = grid % grid_mapping

!fill in resampled grid
DO i = 1, resampledGrid % idim
  DO j = 1, resampledGrid % jdim
    CALL GetXY (i, j, resampledGrid, x, y)
    CALL GetIJ (x, y, grid, iold, jold)
    resampledGrid % mat (i,j) = grid % mat (iold, jold)
  END DO
END DO


END SUBROUTINE ResampleFloatCell


!==============================================================================
!! Description:
!!   Create a new grid_integer with cellsize different from input grid
!!   The content of the created grid is filled in with nearest neighbor method
SUBROUTINE ResampleIntegerCell &
!
(grid, resampledGrid, newCellsize)


IMPLICIT NONE

! Arguments with intent(in):
TYPE (grid_integer), INTENT(IN) :: grid
REAL (KIND = float), INTENT(IN) :: newCellsize

!Arguments with intent(out):
TYPE (grid_integer), INTENT(OUT) :: resampledGrid

!Local declarations:
REAL :: x, y
INTEGER :: i, j, iold, jold
!---------------------------end of declarations--------------------------------

!compute number of rows and columns of the resampled grid
resampledGrid % idim = INT(grid%cellsize * grid%idim / newCellsize) + 1
resampledGrid % jdim = INT(grid%cellsize * grid%jdim / newCellsize) + 1

!assign spatial information
resampledGrid % cellsize = newCellsize
resampledGrid % xllcorner = grid % xllcorner
resampledGrid % yllcorner = grid % yllcorner

!allocate resampled grid
ALLOCATE ( resampledGrid % mat (resampledGrid%idim, resampledGrid%jdim))

!copy information from grid to resampled grid
resampledGrid % standard_name = grid % standard_name
resampledGrid % long_name = grid % long_name
resampledGrid % units = grid % units
resampledGrid % varying_mode = grid % varying_mode
resampledGrid % nodata = grid % nodata
resampledGrid % valid_min = grid % valid_min
resampledGrid % valid_max = grid % valid_max
resampledGrid % reference_time = grid % reference_time
resampledGrid % current_time = grid % current_time
resampledGrid % time_index = grid % time_index
resampledGrid % time_unit = grid % time_unit
resampledGrid % esri_pe_string = grid % esri_pe_string
resampledGrid % grid_mapping = grid % grid_mapping

!fill in resampled grid
DO i = 1, resampledGrid % idim
  DO j = 1, resampledGrid % jdim
    CALL GetXY (i, j, resampledGrid, x, y)
    CALL GetIJ (x, y, grid, iold, jold)
    resampledGrid % mat (i,j) = grid % mat (iold, jold)
  END DO
END DO


END SUBROUTINE ResampleIntegerCell


!==============================================================================
!! Description:
!!    Fill in a grid with a different cellsize from input grid.
!!    Both input grid and output grid exist.   
SUBROUTINE ResampleFloat &
!
(grid, resampledGrid)


IMPLICIT NONE

! Arguments with intent(in):
TYPE (grid_real), INTENT(IN) :: grid

!Arguments with intent(inout):
TYPE (grid_real), INTENT(INOUT) :: resampledGrid

!Local declarations:
REAL :: x, y
INTEGER :: i, j, iold, jold
!---------------------------end of declarations--------------------------------

!check that input and output grid have the same coordinate reference system
IF ( .NOT. grid % grid_mapping == resampledGrid % grid_mapping) THEN
  CALL Catch ('error', 'GridOperations',  &
     'coordinate reference system of resampled grid differs from input grid' )
END IF 


!fill in resampled grid. Skip nodata
DO i = 1, resampledGrid % idim
  DO j = 1, resampledGrid % jdim
    IF (resampledGrid % mat (i,j) /= resampledGrid % nodata) THEN
      CALL GetXY (i, j, resampledGrid, x, y)
      CALL GetIJ (x, y, grid, iold, jold)
      resampledGrid % mat (i,j) = grid % mat (iold, jold)
    END IF
  END DO
END DO

END SUBROUTINE ResampleFloat

!==============================================================================
!! Description:
!!    Fill in a grid with a different cellsize from input grid.
!!    Both input grid and output grid exist.   
SUBROUTINE ResampleInteger &
!
(grid, resampledGrid)


IMPLICIT NONE

! Arguments with intent(in):
TYPE (grid_integer), INTENT(IN) :: grid

!Arguments with intent(inout):
TYPE (grid_integer), INTENT(INOUT) :: resampledGrid

!Local declarations:
REAL :: x, y
INTEGER :: i, j, iold, jold
!---------------------------end of declarations--------------------------------

!check that input and output grid have the same coordinate reference system
IF ( .NOT. grid % grid_mapping == resampledGrid % grid_mapping) THEN
  CALL Catch ('error', 'GridOperations',  &
     'coordinate reference system of resampled grid differs from input grid' )
END IF 

!fill in resampled grid. The content of resampledGird is totally overwritten
DO i = 1, resampledGrid % idim
  DO j = 1, resampledGrid % jdim
    IF (resampledGrid % mat (i,j) /= resampledGrid % nodata) THEN
      CALL GetXY (i, j, resampledGrid, x, y)
      CALL GetIJ (x, y, grid, iold, jold)
      resampledGrid % mat (i,j) = grid % mat (iold, jold)
    END IF
  END DO
END DO

END SUBROUTINE ResampleInteger

!==============================================================================
!! Description:
!!   assign value of mask to mat
SUBROUTINE AssignGridReal &
!
(mat, mask)

IMPLICIT NONE

!Arguments with intent(in):
TYPE(grid_real),INTENT(IN) :: mask

!Arguments with intent (inout)
TYPE(grid_real),INTENT(INOUT):: mat

!Local declarations:
INTEGER :: i,j

!---------------------------end of declarations--------------------------------

!check spatial reference
IF ( .NOT. CRSisEqual (mask, mat, .TRUE.) ) THEN
  CALL Catch ('error', 'GridOperations',  &
    'while assigning grid content from another grid: ',  &
     argument = 'spatial reference not equal' ) 
END IF

DO i = 1, mat % idim
  DO j = 1, mat % jdim
    IF (mat % mat (i,j) /= mat % nodata) THEN
      mat % mat(i,j) = mask % mat(i,j)
    END IF
  END DO
END DO

END SUBROUTINE AssignGridReal

!==============================================================================
!! Description:
!!   assign value of mask to mat
SUBROUTINE AssignGridInteger &
!
(mat, mask)

IMPLICIT NONE

!Arguments with intent(in):
TYPE(grid_integer),INTENT(IN) :: mask

!Arguments with intent (inout)
TYPE(grid_integer),INTENT(INOUT):: mat

!Local declarations:
INTEGER :: i,j

!---------------------------end of declarations--------------------------------

!check spatial reference
IF ( .NOT. CRSisEqual (mask, mat, .TRUE.) ) THEN
  CALL Catch ('error', 'GridOperations',  &
    'while assigning grid content from another grid: ',  &
     argument = 'spatial reference not equal' ) 
END IF

DO i = 1, mat % idim
  DO j = 1, mat % jdim
    IF (mat % mat (i,j) /= mat % nodata) THEN
      mat % mat(i,j) = mask % mat(i,j)
    END IF
  END DO
END DO

END SUBROUTINE AssignGridInteger


!==============================================================================
!! Description:
!!   assign value  to mat
SUBROUTINE AssignReal &
!
(mat, num)

IMPLICIT NONE

!Arguments with intent(in):
REAL (KIND = float),INTENT(IN) :: num

!Arguments with intent (inout)
TYPE(grid_real),INTENT(INOUT):: mat

!Local declarations:
INTEGER :: i,j

!---------------------------end of declarations--------------------------------


DO i = 1, mat % idim
  DO j = 1, mat % jdim
    IF (mat % mat (i,j) /= mat % nodata) THEN
      mat % mat(i,j) = num
    END IF
  END DO
END DO

END SUBROUTINE AssignReal


!==============================================================================
!! Description:
!!   assign value  to mat
SUBROUTINE AssignInteger &
!
(mat, num)

IMPLICIT NONE

!Arguments with intent(in):
INTEGER,INTENT(IN) :: num

!Arguments with intent (inout)
TYPE(grid_integer),INTENT(INOUT):: mat

!Local declarations:
INTEGER :: i,j

!---------------------------end of declarations--------------------------------


DO i = 1, mat % idim
  DO j = 1, mat % jdim
    IF (mat % mat (i,j) /= mat % nodata) THEN
      mat % mat(i,j) = num
    END IF
  END DO
END DO

END SUBROUTINE AssignInteger

!==============================================================================
!! Description:
!!   compute mean of grid_real eventually constrained to a mask
FUNCTION GetMeanOfGridFloat &
!
(grid, maskReal, maskInteger) &
!
RESULT (mean)

IMPLICIT NONE

!Arguments with intent(in):
TYPE (grid_real), INTENT(IN) :: grid
TYPE (grid_real), OPTIONAL,  INTENT(IN) :: maskReal
TYPE (grid_integer), OPTIONAL,  INTENT(IN) :: maskInteger


!Local declarations:
REAL (KIND = float) :: mean
REAL (KIND = float) :: countCells
INTEGER (KIND = long) :: i, j
!---------------------------end of declarations--------------------------------
mean = 0.
countCells = 0.
!check that grid and mask have the same coordinate reference system
IF (PRESENT (maskReal)) THEN
    IF ( .NOT. CRSisEqual(maskReal,grid) ) THEN
        CALL Catch ('error', 'GridOperations',  &
        'calculate mean: ', argument = &
        'coordinate reference system of mask differs from input grid' )
    END IF

    DO j = 1, maskReal % jdim
        DO i = 1, maskReal % idim
            IF (maskReal % mat(i,j) /= maskReal % nodata) THEN
                mean = mean + grid % mat (i,j)
                countCells = countCells + 1.
            END IF
        END DO
    END DO
ELSE IF (PRESENT (maskInteger)) THEN
    IF ( .NOT. CRSisEqual(maskInteger,grid) ) THEN
        CALL Catch ('error', 'GridOperations',  &
        'calculate mean: ', argument = &
        'coordinate reference system of mask differs from input grid' )
    END IF

    DO j = 1, maskInteger % jdim
        DO i = 1, maskInteger % idim
            IF (maskInteger % mat(i,j) /= maskInteger % nodata) THEN
                mean = mean + grid % mat (i,j)
                countCells = countCells + 1.
            END IF
        END DO
    END DO

ELSE
    DO j = 1, grid % jdim
        DO i = 1, grid % idim
            IF (grid % mat(i,j) /= grid % nodata) THEN
                mean = mean + grid % mat (i,j)
                countCells = countCells + 1.
            END IF
        END DO
    END DO
END IF

mean = mean / countCells

RETURN

END FUNCTION GetMeanOfGridFloat


!==============================================================================
!! Description:
!!   compute mean of grid_integer eventually constrained to a mask
FUNCTION GetMeanOfGridInteger &
!
(grid, maskReal, maskInteger) &
!
RESULT (mean)

IMPLICIT NONE

!Arguments with intent(in):
TYPE (grid_integer), INTENT(IN) :: grid
TYPE (grid_real), OPTIONAL,  INTENT(IN) :: maskReal
TYPE (grid_integer), OPTIONAL,  INTENT(IN) :: maskInteger

!Local declarations:
REAL (KIND = float) :: mean
REAL (KIND = float) :: countCells
INTEGER (KIND = long) :: i, j
!---------------------------end of declarations--------------------------------
mean = 0.
countCells = 0.
!check that grid and mask have the same coordinate reference system
IF (PRESENT (maskReal)) THEN
    IF ( .NOT. CRSisEqual(maskReal,grid) ) THEN
        CALL Catch ('error', 'GridOperations',  &
        'calculate mean: ', argument = &
        'coordinate reference system of mask differs from input grid' )
    END IF

    DO j = 1, maskReal % jdim
        DO i = 1, maskReal % idim
            IF (maskReal % mat(i,j) /= maskReal % nodata) THEN
                mean = mean + grid % mat (i,j)
                countCells = countCells + 1.
            END IF
        END DO
    END DO
ELSE IF (PRESENT (maskInteger)) THEN
    IF ( .NOT. CRSisEqual(maskInteger,grid) ) THEN
        CALL Catch ('error', 'GridOperations',  &
        'calculate mean: ', argument = &
        'coordinate reference system of mask differs from input grid' )
    END IF

    DO j = 1, maskInteger % jdim
        DO i = 1, maskInteger % idim
            IF (maskInteger % mat(i,j) /= maskInteger % nodata) THEN
                mean = mean + grid % mat (i,j)
                countCells = countCells + 1.
            END IF
        END DO
    END DO

ELSE
    DO j = 1, grid % jdim
        DO i = 1, grid % idim
            IF (grid % mat(i,j) /= grid % nodata) THEN
                mean = mean + grid % mat (i,j)
                countCells = countCells + 1.
            END IF
        END DO
    END DO
END IF

mean = mean / countCells

RETURN

END FUNCTION GetMeanOfGridInteger


!==============================================================================
!! Description:
!!   compute sum of grid_real eventually constrained to a mask
FUNCTION GetSumOfGridFloat &
!
(grid, maskReal, maskInteger) &
!
RESULT (sum)

IMPLICIT NONE

!Arguments with intent(in):
TYPE (grid_real), INTENT(IN) :: grid
TYPE (grid_real), OPTIONAL,  INTENT(IN) :: maskReal
TYPE (grid_integer), OPTIONAL,  INTENT(IN) :: maskInteger


!Local declarations:
REAL (KIND = float) :: sum
INTEGER (KIND = long) :: i, j
!---------------------------end of declarations--------------------------------
sum = 0.
!check that grid and mask have the same coordinate reference system
IF (PRESENT (maskReal)) THEN
    IF ( .NOT. CRSisEqual(maskReal,grid) ) THEN
        CALL Catch ('error', 'GridOperations',  &
        'calculate mean: ', argument = &
        'coordinate reference system of mask differs from input grid' )
    END IF

    DO j = 1, maskReal % jdim
        DO i = 1, maskReal % idim
            IF (maskReal % mat(i,j) /= maskReal % nodata) THEN
                sum = sum + grid % mat (i,j)
            END IF
        END DO
    END DO
ELSE IF (PRESENT (maskInteger)) THEN
    IF ( .NOT. CRSisEqual(maskInteger,grid) ) THEN
        CALL Catch ('error', 'GridOperations',  &
        'calculate mean: ', argument = &
        'coordinate reference system of mask differs from input grid' )
    END IF

    DO j = 1, maskInteger % jdim
        DO i = 1, maskInteger % idim
            IF (maskInteger % mat(i,j) /= maskInteger % nodata) THEN
                sum = sum + grid % mat (i,j)
            END IF
        END DO
    END DO

ELSE
    DO j = 1, grid % jdim
        DO i = 1, grid % idim
            IF (grid % mat(i,j) /= grid % nodata) THEN
                sum = sum + grid % mat (i,j)
            END IF
        END DO
    END DO
END IF

RETURN

END FUNCTION GetSumOfGridFloat



!==============================================================================
!! Description:
!!   compute sum of grid_integer eventually constrained to a mask
FUNCTION GetSumOfGridInteger &
!
(grid, maskReal, maskInteger) &
!
RESULT (sum)

IMPLICIT NONE

!Arguments with intent(in):
TYPE (grid_integer), INTENT(IN) :: grid
TYPE (grid_real), OPTIONAL,  INTENT(IN) :: maskReal
TYPE (grid_integer), OPTIONAL,  INTENT(IN) :: maskInteger


!Local declarations:
INTEGER (KIND = long) :: sum
INTEGER (KIND = long) :: i, j
!---------------------------end of declarations--------------------------------
sum = 0.
!check that grid and mask have the same coordinate reference system
IF (PRESENT (maskReal)) THEN
    IF ( .NOT. CRSisEqual(maskReal,grid) ) THEN
        CALL Catch ('error', 'GridOperations',  &
        'calculate mean: ', argument = &
        'coordinate reference system of mask differs from input grid' )
    END IF

    DO j = 1, maskReal % jdim
        DO i = 1, maskReal % idim
            IF (maskReal % mat(i,j) /= maskReal % nodata) THEN
                sum = sum + grid % mat (i,j)
            END IF
        END DO
    END DO
ELSE IF (PRESENT (maskInteger)) THEN
    IF ( .NOT. CRSisEqual(maskInteger,grid) ) THEN
        CALL Catch ('error', 'GridOperations',  &
        'calculate mean: ', argument = &
        'coordinate reference system of mask differs from input grid' )
    END IF

    DO j = 1, maskInteger % jdim
        DO i = 1, maskInteger % idim
            IF (maskInteger % mat(i,j) /= maskInteger % nodata) THEN
                sum = sum + grid % mat (i,j)
            END IF
        END DO
    END DO

ELSE
    DO j = 1, grid % jdim
        DO i = 1, grid % idim
            IF (grid % mat(i,j) /= grid % nodata) THEN
                sum = sum + grid % mat (i,j)
            END IF
        END DO
    END DO
END IF

RETURN

END FUNCTION GetSumOfGridInteger





END MODULE GridOperations

