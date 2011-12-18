!! This file is part of 
!!
!! MOSAICO -- MOdular library for raSter bAsed hydrologIcal appliCatiOn.
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
!!   library to deal with river and basin morphology
!! @author <a href="mailto:giovanni.ravazzani@polimi.it">Giovanni Ravazzani</a> 
!! @version 0.2 - 23th October 2010  
MODULE Morphology         
			
! History: 
!  
! Version   Date                Comment 
! -------       ----                    ------- 
!  0.1        17/Aug/2009   Original code. giovanni ravazzani
!  0.2        23/Oct/2010   Compute flow accumulation grid


! Modules used: 

USE DataTypeSizes, ONLY: &
!Imported parameters:
short, long, float

USE GridLib, ONLY: &
!Imported type definitions:
grid_integer, grid_real, &
! Imported routines:
NewGrid, &
!Imported parameters:
ESRI_ASCII, ESRI_BINARY, NET_CDF

USE GridOperations, ONLY: &
!Imported routines:
IsOutOfGrid, GetIJ, CellArea, GetXY

USE GeoLib, ONLY: &
!Imported definitions:
Coordinate, &
!Imported variables:
point1, point2, &
!Imported routines:
Distance


USE LogLib, ONLY: &
!Imported routines:
Catch



IMPLICIT NONE 

PUBLIC :: HortonOrders
PUBLIC :: CheckOutlet
PUBLIC :: DownstreamCell
PUBLIC :: FlowAccumulation
PUBLIC :: DeriveSlope
PUBLIC :: BasinDelineate
PUBLIC :: CellIsSpring

PRIVATE :: ConfluenceIsAround
PRIVATE :: BasinMask
PRIVATE :: BasinArea

!Local declarations:
!define ESRI convention to assign flow direction
INTEGER,PRIVATE,PARAMETER:: NW=32,N=64,NE=128,W=16,E=1,SW=8,S=4,SE=2

!=======         
CONTAINS
!======= 
! Define procedures contained in this module. 

!==============================================================================
!! Description:
!!   returns a grid_integer containing Horton orders. Horton orders are 
!!   computed on the entire space-filled basin.
SUBROUTINE HortonOrders &
!
(flowAccumulation,flowDirection,orders,basinOrder)

USE StringManipulation, ONLY: &
!Imported routines:
ToString


IMPLICIT NONE

!Arguments with intent in:
TYPE (grid_real), INTENT (IN) :: flowAccumulation
TYPE (grid_integer), INTENT (IN) :: flowDirection


!Arguments with intent out or inout
TYPE (grid_integer), INTENT (INOUT) :: orders
INTEGER, OPTIONAL, INTENT (OUT) :: basinOrder !!the maximum order of the basin

!local declarations:
LOGICAL  :: confluence !!true if confluence
LOGICAL  :: outlet  !!true if basin outlet

INTEGER  :: row, col !!current cell
INTEGER  :: iDown, jDown !!downstream cell
INTEGER  :: numConf !!number of confluences
INTEGER  :: order !! Horton order
INTEGER  :: cellsCount
INTEGER  :: i, j

!--------------------------------end of declaration----------------------------




order = 1
numConf = 1

DO WHILE (numConf > 0) ! se non trovo confluenze 
                                 ! di classe order
								 ! l'operazione è terminata

CALL Catch ('info', 'Morphology', 'Elaborating reaches of stream order: ', &
             argument = ToString(order))

numConf = 0

!-----follow the reach till a confluence or a basin outlet------

DO j = 1,orders % jdim
  DO i = 1,orders % idim

    IF(CellIsSpring(i,j,flowDirection)) THEN !found a spring
           row                = i
           col                = j
           outlet             = .FALSE.
           confluence         = .FALSE.
           cellsCount         = 0
           orders % mat(i,j)  = 1
          
       DO WHILE (.NOT. outlet) ! follow the reach till the basin outlet 
	                                                            
          IF (orders % mat(row,col) == order ) THEN
              cellsCount = cellsCount + 1 			  
          ENDIF

          CALL DownstreamCell(row, col, &
							  flowDirection%mat(row,col), &
                              iDown, jDown)                         
        
          IF (cellsCount >= 1 ) THEN  !I am in the reach of that order
          
          !check if downstream cell is a confluence to increment horton order 
          !Downstream the confluence, till the basin outlet, as temptative value,
          !order is increased by 1 (order + 1)
             IF ( .NOT. confluence  ) THEN
                CALL ConfluenceIsAround (iDown, jDown, row, col, &
								    flowDirection,confluence,orders,order)
                IF(confluence) numConf = numConf + 1 
             ENDIF

			 outlet = CheckOutlet (iDown,jDown,flowDirection)
			 
             IF (.NOT. outlet) THEN
                IF (.NOT. confluence) THEN
                   orders % mat(iDown,jDown) = order
                ELSE
                   orders % mat(iDown,jDown) = order + 1        
                ENDIF
             ENDIF

          ENDIF ! cellsCount >= 1 

          outlet = CheckOutlet(iDown,jDown,flowDirection)
          
          !loop
          row = iDown
          col = jDown

       END DO
                  
    ENDIF

  ENDDO
ENDDO  !ciclo sulla matrice ordini
!------------------------------------------------------------------------------
order = order + 1

ENDDO 

IF ( PRESENT (basinOrder) ) THEN
    basinOrder = order - 1
END IF

END SUBROUTINE HortonOrders


!==============================================================================
!! Description:
!!   Scan the eigth cells surrounding the center cell (row,col) (neglecting 
!!   the upstream cell (i,j) ) to find if a confluence is present.
!!   The confluence cell must be of the same order or not yet defined.
SUBROUTINE ConfluenceIsAround &
!
(row,col,i,j,flowDir,confluence,orders,Norder)


IMPLICIT NONE

TYPE (grid_integer), INTENT(IN):: flowDir
TYPE (grid_integer), INTENT(IN):: orders
INTEGER, INTENT(IN):: i,j,row,col,Norder
LOGICAL, INTENT(OUT):: confluence
!-----------------------------------------------------------


IF(.NOT. IsOutOfGrid(row,col+1,flowDir) ) THEN
    IF(flowDir%mat(row,col+1) == W .AND.&
       (row /= i .OR. col+1 /= j).AND. &
       (orders%mat(row,col+1) == orders%nodata.OR. &
       orders%mat(row,col+1) == Norder) ) THEN
       confluence = .TRUE.
    ENDIF
ENDIF

IF(.NOT. IsOutOfGrid(row+1,col+1,flowDir) ) THEN
    IF(flowDir%mat(row+1,col+1) == NW .AND.&
       (row+1 /=i .OR. col+1 /= j).AND. &
       (orders%mat(row+1,col+1) == orders%nodata.OR. &
       orders%mat(row+1,col+1) == Norder) ) THEN
       confluence = .TRUE.
    ENDIF
ENDIF

IF(.NOT. IsOutOfGrid(row+1,col,flowDir) ) THEN
    IF(flowDir%mat(row+1,col) == N .AND.&
       (row+1 /=i .OR. col /= j).AND. &
       (orders%mat(row+1,col) == orders%nodata.OR. &
       orders%mat(row+1,col) == Norder) ) THEN
       confluence = .TRUE.
    ENDIF
ENDIF

IF(.NOT. IsOutOfGrid(row+1,col-1,flowDir) ) THEN
    IF(flowDir%mat(row+1,col-1) == NE .AND.&
       (row+1 /= i .OR. col-1 /= j).AND. &
       (orders%mat(row+1,col-1) == orders%nodata.OR. &
       orders%mat(row+1,col-1) == Norder) ) THEN
       confluence = .TRUE.
    ENDIF
ENDIF

IF(.NOT. IsOutOfGrid(row,col-1,flowDir) ) THEN
    IF(flowDir%mat(row,col-1) == E .AND.&
       (row /= i .OR. col-1 /= j).AND. &
       (orders%mat(row,col-1) == orders%nodata.OR. &
       orders%mat(row,col-1) == Norder) ) THEN
       confluence = .TRUE.
    ENDIF
ENDIF

IF(.NOT. IsOutOfGrid(row-1,col-1,flowDir) ) THEN
    IF(flowDir%mat(row-1,col-1) == SE .AND.&
       (row-1 /= i .OR. col-1 /= j).AND. &
       (orders%mat(row-1,col-1) == orders%nodata.OR. &
       orders%mat(row-1,col-1) == Norder) ) THEN
       confluence = .TRUE.
    ENDIF
ENDIF

IF(.NOT. IsOutOfGrid(row-1,col,flowDir) ) THEN
    IF(flowDir%mat(row-1,col) == S .AND.&
       (row-1 /= i .OR. col /= j).AND. &
       (orders%mat(row-1,col) == orders%nodata.OR. &
       orders%mat(row-1,col) == Norder) ) THEN
       confluence = .TRUE.
    ENDIF
ENDIF

IF(.NOT. IsOutOfGrid(row-1,col+1,flowDir) ) THEN
    IF(flowDir%mat(row-1,col+1) == SW .AND.&
       (row-1 /= i .OR. col+1 /= j).AND. &
       (orders%mat(row-1,col+1) == orders%nodata.OR. &
       orders%mat(row-1,col+1) == Norder) ) THEN
       confluence = .TRUE.
    ENDIF
ENDIF


RETURN
END SUBROUTINE ConfluenceIsAround

!==============================================================================
!! Description:
!!   if the downstream cell is a nodata or out of grid space, the current cell
!!   is the basin outlet
LOGICAL FUNCTION CheckOutlet &
!
(iDown, jDown, flowDirection)


IMPLICIT NONE
INTEGER, INTENT(in)           :: iDown !riga cella di valle
INTEGER, INTENT(in)           :: jDown !colonna cella di valle
TYPE(grid_integer), INTENT(in):: flowDirection
!------------------------------end of declaration -----------------------------


!----------implementazione-----------------------------------------------------
IF ( IsOutOfGrid (iDown,jDown,flowDirection) ) THEN
	CheckOutlet = .TRUE.
ELSEIF ( flowDirection % mat(iDown,jDown) == flowDirection % nodata)  THEN
	CheckOutlet = .TRUE.
ELSE
    CheckOutlet = .FALSE.
ENDIF
RETURN
END FUNCTION CheckOutlet


!==============================================================================
!! Description:
!!   returns the position (is,js) of the downstream cell and, optionally,
!!   the flow path length, considering cardinal and diagonal direction
SUBROUTINE DownstreamCell &
!
(iin, jin, dir, is, js, dx, grid)

IMPLICIT NONE

!Arguments with intent in:
INTEGER (KIND = short), INTENT (IN) :: iin,jin !!current cell
INTEGER (KIND = long), INTENT (IN) :: dir !!flow direction 
TYPE(grid_integer), INTENT (IN),OPTIONAL:: grid !!used to define coordinate reference system

!Arguments with intent out:
INTEGER (KIND = short), INTENT (OUT) :: is,js !!downstream cell
REAL (KIND = float), INTENT (OUT) ,OPTIONAL :: dx !!flow path length [m]

!Local declarations
REAL (KIND = float) :: ddx,x,y,xs,ys

!--------------------end of declarations---------------------------------------

SELECT CASE(dir)
	CASE(E)
		js = jin + 1
		is = iin
	CASE(SE)
		js = jin + 1
		is = iin + 1
	CASE(S)
		js = jin
		is = iin + 1
	CASE(SW)
		js = jin - 1
		is = iin + 1
	CASE(W)
		js = jin - 1
		is = iin
	CASE(NW)
		js = jin - 1
		is = iin - 1
	CASE(N)
		js = jin
		is = iin - 1
	CASE(NE)
		js = jin + 1
		is = iin - 1
END SELECT

IF ((PRESENT(dx)).and.(PRESENT(grid))) THEN
    CALL GetXY (iin,jin,grid,x,y)
    CALL GetXY (is,js,grid,xs,ys)

    point1 % northing = y  
    point1 % easting = x
    point2 % northing = ys  
    point2 % easting = xs

    ddx = distance(point1,point2)
    dx = ddx
    RETURN

ELSE IF ((PRESENT(dx)).and. .not.(PRESENT(grid))) THEN
   CALL Catch ('error', 'Morphology', 'missing grid while calculating downstream distance')
END IF

END SUBROUTINE DownstreamCell



!==============================================================================
!! Description:
!!   compute local slope in radians. If flowDirection is not supplied, slope 
!!   is calculated along greater slope direction, otherwise, slope is 
!!   calculated using elevation of downstream cell.
SUBROUTINE DeriveSlope &
!
(dem, slope, flowDirection)


IMPLICIT NONE

!arguments with intent in
TYPE(grid_real),    INTENT(in):: dem
TYPE(grid_integer), OPTIONAL, INTENT(in):: flowDirection

!arguments with intent out
TYPE (grid_real), INTENT (out) :: slope

!local variables
INTEGER :: i,j
INTEGER :: id, jd
REAL (KIND = float) :: downElevation
REAL (KIND = float) :: dx
REAL (KIND = float) :: max
REAL (KIND = float) :: localSlope
REAL (KIND = float) :: length

!------------------------------end of declaration -----------------------------

!allocate slope grid
CALL NewGrid (slope,dem)

DO i = 1, dem % idim
  DO j = 1, dem % jdim
    IF (dem % mat(i,j) /= dem % nodata) THEN
      IF (PRESENT (flowDirection) ) THEN !use elevation of downstream cell
        CALL DownstreamCell (i, j, flowDirection % mat(i,j), id, jd, dx, flowDirection)
        IF ( CheckOutlet (id, jd, flowDirection) )THEN
          slope % mat (i,j) = ATAN (0.001) 
        ELSE 
          slope % mat (i,j) = ATAN  ( ( dem % mat (i,j) - dem % mat (id,jd) ) / dx)                                 
        END IF
      ELSE !search for maximum slope
        max = - 100.
        
        length = CellArea (dem,i,j) ** 0.5
        
        !north cell
        IF ( .NOT. IsOutOfGrid (i-1,j,dem) ) THEN
          IF ( dem % mat (i-1,j) /= dem % nodata) &
             localSlope = ATAN  ( ( dem % mat (i,j) - dem % mat (i-1,j) ) &
                                    / length )
          IF (localSlope > max) max =  localSlope
        END IF
        
        !north-east cell
        IF ( .NOT. IsOutOfGrid (i-1,j+1,dem) ) THEN
          IF ( dem % mat (i-1,j+1) /= dem % nodata) &
            localSlope = ATAN  ( ( dem % mat (i,j) - dem % mat (i-1,j+1) ) &
                                    / ( 1.4142 * length) )
          IF (localSlope > max) max =  localSlope
        END IF
        
        !east cell
        IF ( .NOT. IsOutOfGrid (i,j+1,dem) ) THEN
          IF ( dem % mat (i,j+1) /= dem % nodata) &
            localSlope = ATAN  ( ( dem % mat (i,j) - dem % mat (i,j+1) ) &
                                    / length )
          IF (localSlope > max) max =  localSlope
        END IF
        
        !south-east cell
        IF ( .NOT. IsOutOfGrid (i+1,j+1,dem) ) THEN
          IF ( dem % mat (i+1,j+1) /= dem % nodata) &
            localSlope = ATAN  ( ( dem % mat (i,j) - dem % mat (i+1,j+1) ) &
                                    / ( 1.4142 * length) )
          IF (localSlope > max) max =  localSlope
        END IF
        
        !south cell
        IF ( .NOT. IsOutOfGrid (i+1,j,dem) ) THEN
          IF ( dem % mat (i+1,j) /= dem % nodata) &
            localSlope = ATAN  ( ( dem % mat (i,j) - dem % mat (i+1,j) ) &
                                    / length )
          IF (localSlope > max) max =  localSlope
        END IF
        
        !south-west cell
        IF ( .NOT. IsOutOfGrid (i+1,j-1,dem) ) THEN
          IF ( dem % mat (i+1,j-1) /= dem % nodata) &
            localSlope = ATAN  ( ( dem % mat (i,j) - dem % mat (i+1,j-1) ) &
                                    / ( 1.4142 * length) )
          IF (localSlope > max) max =  localSlope
        END IF
        
        !west cell
        IF ( .NOT. IsOutOfGrid (i,j-1,dem) ) THEN
          IF ( dem % mat (i,j-1) /= dem % nodata) &
            localSlope = ATAN  ( ( dem % mat (i,j) - dem % mat (i,j-1) ) &
                                    / length )
          IF (localSlope > max) max =  localSlope
        END IF
        
        !north-west cell
        IF ( .NOT. IsOutOfGrid (i-1,j-1,dem) ) THEN
          IF ( dem % mat (i-1,j-1) /= dem % nodata) &
            localSlope = ATAN  ( ( dem % mat (i,j) - dem % mat (i-1,j-1) ) &
                                    / ( 1.4142 * length) )
          IF (localSlope > max) max =  localSlope
        END IF
        
        slope % mat (i,j) = max
        
      END IF
    END IF
  END DO
END DO


RETURN
END SUBROUTINE DeriveSlope


!==============================================================================
!! Description:
!!   compute map of flow accumulation [m2]
!!   Input grid: flow direction
SUBROUTINE FlowAccumulation &
!
(fdir, facc)

IMPLICIT NONE

!arguments with intent in
TYPE(grid_integer), INTENT(in):: fdir

!arguments with intent out
TYPE (grid_real), INTENT (out) :: facc

!local variables
INTEGER :: i,j

!------------------------------end of declaration -----------------------------

!allocate new flow accumulation grid using flow direction grid as template
CALL NewGrid (facc, fdir)

!compute grid containing area of each cell
DO i = 1, fdir % idim
  DO j = 1, fdir % jdim
    IF (fdir % mat (i,j) /= fdir % nodata) THEN
      facc % mat(i,j) = CellArea (facc, i, j)
    ELSE
      facc % mat(i,j) = facc % nodata
    END IF
   END DO
END DO

DO i = 1, fdir % idim
  DO j = 1, fdir % jdim
    IF (fdir % mat (i,j) /= fdir % nodata) THEN
      CALL BasinArea (i, j, fdir, facc % mat(i,j))
    ELSE
      facc % mat(i,j) = facc % nodata
    END IF
  END DO
END DO


END SUBROUTINE FlowAccumulation



!==============================================================================
!! Description:
!!   compute basin area [m2]
RECURSIVE SUBROUTINE BasinArea &
!
(r, c, fdir, area)

IMPLICIT NONE

TYPE(grid_integer),INTENT(IN) :: fdir
INTEGER, INTENt(in) :: r,c 
REAL (KIND = float), INTENT(inout) :: area
!------------------------------end of declaration -----------------------------

IF ( .NOT. IsOutOfGrid(r,c+1,fdir) ) THEN
    IF(fdir%mat(r,c+1) == W) THEN
       area = area + CellArea (fdir, r, c+1)
       CALL BasinArea (r,c+1,fdir,area)
    END IF
END IF

IF ( .NOT. IsOutOfGrid(r+1,c+1,fdir) ) THEN
    IF(fdir%mat(r+1,c+1) == NW ) THEN
       area = area + CellArea (fdir, r+1, c+1)
       CALL BasinArea (r+1,c+1,fdir,area)
    END IF
END IF

IF ( .NOT. IsOutOfGrid(r+1,c,fdir) ) THEN
    IF(fdir%mat(r+1,c) == N ) THEN
       area = area + CellArea (fdir, r+1, c)
       CALL BasinArea (r+1,c,fdir,area)
    END IF
END IF

IF ( .NOT. IsOutOfGrid(r+1,c-1,fdir) ) THEN
    IF(fdir%mat(r+1,c-1) == NE ) THEN
       area = area + CellArea (fdir, r+1, c-1)
       CALL BasinArea (r+1,c-1,fdir,area)
    END IF
END IF

IF ( .NOT. IsOutOfGrid(r,c-1,fdir) ) THEN
    IF(fdir%mat(r,c-1) == E) THEN
        area = area + CellArea (fdir, r, c-1)
        CALL BasinArea (r,c-1,fdir,area)
    END IF
END IF

IF ( .NOT. IsOutOfGrid(r-1,c-1,fdir) ) THEN
    IF(fdir%mat(r-1,c-1) == SE ) THEN
       area = area + CellArea (fdir, r-1, c-1)
       CALL BasinArea (r-1,c-1,fdir,area)
    END IF
END IF

IF ( .NOT. IsOutOfGrid(r-1,c,fdir) ) THEN
    IF(fdir%mat(r-1,c) == S ) THEN
       area = area + CellArea (fdir, r-1, c)
       CALL BasinArea (r-1,c,fdir,area)
    END IF
END IF

IF ( .NOT. IsOutOfGrid(r-1,c+1,fdir) ) THEN
    IF(fdir%mat(r-1,c+1) == SW ) THEN
       area = area + CellArea (fdir, r-1, c+1)
       CALL BasinArea (r-1,c+1,fdir,area) 
    END IF
END IF

END SUBROUTINE BasinArea



!==============================================================================
!! Description:
!!   find the cells that are springs, defined as those cells that have not
!!   any other cells upstream
FUNCTION CellIsSpring &
!
(row, col, flowDir) &
!
RESULT (spring)

IMPLICIT NONE

! Arguments with intent(in):
INTEGER, INTENT(in) :: row, col
TYPE (grid_integer), INTENT(in) :: flowDir

! Local variables:
LOGICAL :: spring

!------------end of declaration------------------------------------------------
spring = .TRUE.

IF (flowDir % mat(row,col) == flowDir % nodata) THEN
  spring = .FALSE.
  RETURN
END IF

IF(.NOT. IsOutOfGrid(row,col+1,flowDir) ) THEN
    IF(flowDir%mat(row,col+1) == W ) THEN
       spring = .FALSE.
       RETURN
    ENDIF
ENDIF

IF(.NOT. IsOutOfGrid(row+1,col+1,flowDir) ) THEN
    IF(flowDir%mat(row+1,col+1) == NW  ) THEN
       spring = .FALSE.
       RETURN
    ENDIF
ENDIF

IF(.NOT. IsOutOfGrid(row+1,col,flowDir) ) THEN
    IF(flowDir%mat(row+1,col) == N  ) THEN
      spring = .FALSE.
       RETURN
    ENDIF
ENDIF

IF(.NOT. IsOutOfGrid(row+1,col-1,flowDir) ) THEN
    IF(flowDir%mat(row+1,col-1) == NE  ) THEN
       spring = .FALSE.
       RETURN
    ENDIF
ENDIF

IF(.NOT. IsOutOfGrid(row,col-1,flowDir) ) THEN
    IF(flowDir%mat(row,col-1) == E  ) THEN
       spring = .FALSE.
       RETURN
    ENDIF
ENDIF

IF(.NOT. IsOutOfGrid(row-1,col-1,flowDir) ) THEN
    IF(flowDir%mat(row-1,col-1) == SE  ) THEN
       spring = .FALSE.
       RETURN
    ENDIF
ENDIF

IF(.NOT. IsOutOfGrid(row-1,col,flowDir) ) THEN
    IF(flowDir%mat(row-1,col) == S  ) THEN
       spring = .FALSE.
       RETURN
    ENDIF
ENDIF

IF(.NOT. IsOutOfGrid(row-1,col+1,flowDir) ) THEN
    IF(flowDir%mat(row-1,col+1) == SW  ) THEN
       spring = .FALSE.
       RETURN
    ENDIF
ENDIF


END FUNCTION CellIsSpring


!==============================================================================
!! Description:
!!   compute mask of river basin given map of flow direction and the coordinate
!!   of the outlet point
SUBROUTINE BasinDelineate &
!
(fdir,x,y, mask)

IMPLICIT NONE

!arguments with intent in
TYPE(grid_integer), INTENT(in):: fdir
REAL (KIND = float), INTENT(in) :: x, y !!coordinate of outlet

!arguments with intent inout:
TYPE(grid_integer), INTENT(inout) :: mask

!local variables
INTEGER :: i,j
LOGICAL :: check

!------------------------------end of declaration -----------------------------
                                         

!allocate new grid
!CALL NewGrid (mask, fdir)

mask % mat = mask % nodata

!compute row and column of outlet
CALL GetIJ (x, y, fdir, i, j, check)
	
!compute basin mask
CALL BasinMask(mask,fdir,i,j)

END SUBROUTINE BasinDelineate
	


!==============================================================================
!! Description:
!!   search for cells included in the river basin
RECURSIVE SUBROUTINE BasinMask &
!
(basin, fdir, r, c)

IMPLICIT NONE

TYPE(grid_integer),INTENT(IN) :: fdir
TYPE(grid_integer),INTENT(INOUT) :: basin
INTEGER, INTENt(in) :: r,c  

!------------------------------end of declaration -----------------------------

IF ( .NOT. IsOutOfGrid(r,c+1,fdir) ) THEN
    IF(fdir%mat(r,c+1)==W.AND. basin%mat(r,c+1)/=1) THEN
       basin%mat(r,c+1) = 1
       CALL BasinMask(basin,fdir,r,c+1) 
    END IF
END IF

IF ( .NOT. IsOutOfGrid(r+1,c+1,fdir) ) THEN
    IF(fdir%mat(r+1,c+1)==NW .AND. basin%mat(r+1,c+1)/=1) THEN
       basin%mat(r+1,c+1) = 1
       CALL BasinMask(basin,fdir,r+1,c+1) 
    END IF
END IF

IF ( .NOT. IsOutOfGrid(r+1,c,fdir) ) THEN
    IF(fdir%mat(r+1,c)==N .AND. basin%mat(r+1,c)/=1) THEN
       basin%mat(r+1,c) = 1
       CALL BasinMask(basin,fdir,r+1,c) 
    END IF
END IF

IF ( .NOT. IsOutOfGrid(r+1,c-1,fdir) ) THEN
    IF(fdir%mat(r+1,c-1)==NE .AND. basin%mat(r+1,c-1)/=1) THEN
       basin%mat(r+1,c-1) = 1
       CALL BasinMask(basin,fdir,r+1,c-1)
    END IF
END IF

IF ( .NOT. IsOutOfGrid(r,c-1,fdir) ) THEN
    IF(fdir%mat(r,c-1)==E .AND. basin%mat(r,c-1)/=1) THEN
       basin%mat(r,c-1) = 1
       CALL BasinMask(basin,fdir,r,c-1) 
    END IF
END IF

IF ( .NOT. IsOutOfGrid(r-1,c-1,fdir) ) THEN
    IF(fdir%mat(r-1,c-1)==SE .AND. basin%mat(r-1,c-1)/=1) THEN
       basin%mat(r-1,c-1) = 1
       CALL BasinMask(basin,fdir,r-1,c-1) 
    END IF
END IF

IF ( .NOT. IsOutOfGrid(r-1,c,fdir) ) THEN
    IF(fdir%mat(r-1,c)==S .AND. basin%mat(r-1,c)/=1) THEN
       basin%mat(r-1,c) = 1
       CALL BasinMask(basin,fdir,r-1,c) 
    END IF
END IF

IF ( .NOT. IsOutOfGrid(r-1,c+1,fdir) ) THEN
    IF(fdir%mat(r-1,c+1)==SW .AND. basin%mat(r-1,c+1)/=1) THEN
       basin%mat(r-1,c+1) = 1
       CALL BasinMask(basin,fdir,r-1,c+1) 
    END IF
END IF

END SUBROUTINE BasinMask











END MODULE Morphology

