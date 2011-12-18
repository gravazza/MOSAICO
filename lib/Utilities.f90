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
!!   Collection of general purpose utilities.
!! @author <a href="mailto:giovanni.ravazzani@polimi.it">Giovanni Ravazzani</a> 
!! @version 1.0 - 4th October 2008  
MODULE Utilities        
			
! History: 
!  
! Version   Date          Comment 
! -------   ----          ------- 
!  1.0      04/Oct/2008   Original code. giovanni ravazzani
! 
! 
! Modules used: 
! 
USE DataTypeSizes, ONLY : &
! Imported Type Definitions:
short, float, double      

IMPLICIT NONE 
! Global (i.e. public) Declarations: 
         
! Global Routines:
PUBLIC :: GetUnit
PUBLIC :: TimeStamp
PUBLIC :: LinearInterp

! Local (i.e. private) Declarations:
! Local Procedures:
PRIVATE :: LinearInterpFloatFloat
PRIVATE :: LinearInterpFloatDouble

! Operator definitions:
! Define new operators or overload existing ones.

INTERFACE LinearInterp
   MODULE PROCEDURE LinearInterpFloatFloat
   MODULE PROCEDURE LinearInterpFloatDouble
END INTERFACE
        
!=======
CONTAINS
!=======
! Define procedures contained in this module. 

!==============================================================================
!! Description:
!!   returns a free FORTRAN unit number
!!  Discussion:
!!   A "free" FORTRAN unit number is an integer between 1 and 99 which
!!   is not currently associated with an I/O device.  A free FORTRAN unit
!!   number is needed in order to open a file with the OPEN command.
!!   If IUNIT = 0, then no free FORTRAN unit could be found, although
!!   all 99 units were checked (except for units 5 and 6).
!!   Otherwise, IUNIT is an integer between 1 and 99, representing a
!!   free FORTRAN unit.  Note that GetUnit assumes that units 5 and 6
!!   are special, and will never return those values.
!!   Adapted from John Burkardt
FUNCTION GetUnit () &
RESULT (iunit)

IMPLICIT NONE

! Local scalars:
INTEGER (KIND = short)     :: iunit 
INTEGER (KIND = short)     :: i
INTEGER (KIND = short)     :: ios  
LOGICAL                    :: lopen
!------------end of declaration------------------------------------------------
  iunit = 0

  DO i = 1, 99
    IF ( i /= 5 .AND. i /= 6 ) THEN
      INQUIRE ( unit = i, opened = lopen, iostat = ios )
      IF ( ios == 0 ) THEN
        IF ( .NOT. lopen ) THEN
          iunit = i
          RETURN
        END IF
      END IF
    END IF
  END DO

  RETURN
END	FUNCTION GetUnit


!==============================================================================
!! Description:
!!prints the current YMDHMS date as a time stamp. 
!!Cursor remains on the same line.
!!Example:    2008-09-29T21:00:25.624+0200
!!   Adapted from John Burkardt
SUBROUTINE TimeStamp &
!
(unit)

IMPLICIT NONE
! Subroutine arguments:
! Scalar arguments with intent (in):
INTEGER (KIND = short), INTENT (IN) :: unit

! Local scalars:
INTEGER (KIND = short) :: d
CHARACTER ( LEN = 8 )  :: date
INTEGER (KIND = short) :: h
INTEGER (KIND = short) :: m
INTEGER (KIND = short) :: mm
INTEGER (KIND = short) :: n
INTEGER (KIND = short) :: s
CHARACTER ( LEN = 10 ) :: time
INTEGER (KIND = short) :: values(8)
INTEGER (KIND = short) :: y
CHARACTER ( LEN = 5 )  :: zone
!------------end of declaration------------------------------------------------
 
  CALL date_and_time ( date, time, zone, values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)
  
  WRITE (unit, '(i4,a1,i2.2,a1,i2.2,a1,i2,a1,i2.2,a1,i2.2,a1,i3.3,a5)', ADVANCE = 'no' ) &
         y, '-', m, '-', d, 'T', h, ':', n, ':', s, '.', mm, zone

  RETURN
END SUBROUTINE TimeStamp


!==============================================================================
!! Description:
!! calculates linear interpolation between real numbers. 
FUNCTION LinearInterpFloatFloat  &
  ( x1, x2, y1, y2, x )     &
RESULT (y)

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
REAL (KIND = float), INTENT (IN) :: x1, x2, y1, y2, x

! Scalar arguments with intent (out):
REAL (KIND = float) :: y

!------------end of declaration------------------------------------------------

y = y1 + ( y2 - y1 ) / ( x2 - x1 ) * ( x - x1 )

END FUNCTION LinearInterpFloatFloat

!==============================================================================
!! Description:
!! calculates linear interpolation between real numbers. Output is a double real
FUNCTION LinearInterpFloatDouble  &
  ( x1, x2, y1, y2, x )     &
RESULT (y)

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
REAL (KIND = float), INTENT (IN) :: x1, x2, x
REAL (KIND = double), INTENT (IN) :: y1, y2


! Scalar arguments with intent (out):
REAL (KIND = double) :: y

!------------end of declaration------------------------------------------------

y = y1 + ( y2 - y1 ) / ( x2 - x1 ) * ( x - x1 )

END FUNCTION LinearInterpFloatDouble


END MODULE Utilities
         