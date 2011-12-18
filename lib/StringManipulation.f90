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
!!   Module for simple string manipulations
!!
!! References and Credits:
!!   Adapted from flibs library http://flibs.sourceforge.net
!!   written by Arjen Markus
!!   and string utilities http://www.gbenthien.net/strings/index.html
!!   written by Dr. George Benthien
!!
!! @author <a href="mailto:giovanni.ravazzani@polimi.it">Giovanni Ravazzani</a> 
!! @version 1.0 - 15th September 2008  
MODULE StringManipulation        

! History: 
!  
! Version   Date          Comment 
! -------   ----          ------- 
! 1.0       15/Sep/2008    Original code. giovanni ravazzani
! 
! Code Description: 
!   Language:           Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!     Documenting Exchangeable Fortran 90 Code". 
! 
! Modules used:
!
USE DataTypeSizes, ONLY : &
! Imported Type Definitions:
short, long, float, double

USE LogLib, ONLY : &
! Imported Routines:
Catch

USE ErrorCodes, ONLY : &
! Imported parameters:
genericIOError


IMPLICIT NONE 

! Global (i.e. public) Declarations:
! Global Procedures:
PUBLIC :: StringReverse
PUBLIC :: StringToUpper
PUBLIC :: StringToLower
PUBLIC :: StringTokenize
PUBLIC :: StringCompact
PUBLIC :: StringToDouble
PUBLIC :: StringToFloat
PUBLIC :: StringToLong
PUBLIC :: StringToShort
PUBLIC :: ToString
PUBLIC :: StringSplit


! Local (i.e. private) Declarations:
! Local Procedures:
PRIVATE :: DoubleToString
PRIVATE :: FloatToString
PRIVATE :: LongToString
PRIVATE :: ShortToString

! Local Parameters:

CHARACTER(LEN=26), PARAMETER, PRIVATE :: upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
CHARACTER(LEN=26), PARAMETER, PRIVATE :: lower = 'abcdefghijklmnopqrstuvwxyz'

INTERFACE ToString
   MODULE PROCEDURE DoubleToString
   MODULE PROCEDURE FloatToString
   MODULE PROCEDURE LongToString
  MODULE PROCEDURE ShortToString
END INTERFACE


!=======
CONTAINS
!=======
! Define procedures contained in this module. 

!==============================================================================
!! Description:
!! Return a string that has all characters in reverse order 
!! Arguments:
!!     string     String to be reversed
!! Result:
!!     Reversed string 
FUNCTION StringReverse &
  ( string )           &
RESULT (rev)

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER(LEN=*)   :: string

! Local scalars:
CHARACTER(LEN=LEN(string)) :: rev
INTEGER (KIND = short)     :: i 
INTEGER (KIND = short)     :: length
!------------end of declaration------------------------------------------------
length = LEN(string)
DO i = 1,length
  rev(i:i) = string(length - i + 1 : length - i + 1)
END DO
END FUNCTION StringReverse

!==============================================================================
!! Description:
!! Return a string that has all _letters_ in upper case
!! Arguments:
!!     string     String to be treated
!! Result:
!!     String with letters turned into upper case 
FUNCTION StringToUpper &
  ( string )           &
RESULT (new)

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER(LEN=*)   :: string

! Local scalars:
CHARACTER(LEN=LEN(string)) :: new
INTEGER (KIND = short)     :: k 
INTEGER (KIND = short)     :: i
INTEGER (KIND = short)     :: length
!------------end of declaration------------------------------------------------

length = LEN(string)
new    = string
DO i = 1,length
   k = INDEX( lower, string(i:i) )
   IF ( k > 0 ) THEN
       new(i:i) = upper(k:k)
   END IF
END DO
END FUNCTION StringToUpper


!==============================================================================
!! Description:
!! Return a string that has all _letters_ in lower case
!! Arguments:
!!     string     String to be treated
!! Result:
!!     String with letters turned into lower case 
FUNCTION StringToLower &
  ( string )           &
RESULT (new)

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER(LEN=*)   :: string

! Local scalars:
CHARACTER(LEN=LEN(string)) :: new
INTEGER (KIND = short)     :: i 
INTEGER (KIND = short)     :: k 
INTEGER (KIND = short)     :: length
!------------end of declaration------------------------------------------------

length = LEN(string)
new    = string
DO i = 1,length
   k = INDEX( upper, string(i:i) )
   IF ( k > 0 ) THEN
       new(i:i) = lower(k:k)
   END IF
END DO
END FUNCTION StringToLower

!==============================================================================
!! Description:
!! It is often useful to represent a text as a list of tokens. 
!! The process of breaking a text up into its constituent tokens is known 
!! as tokenization. The subroutine parses the string in input into arguments 
!! args(1), ..., args(nargs) based on the delimiters contained in the string 
!! 'delims'. Preceding a delimiter in 'string' by a backslash (\) makes this 
!! particular instance not a delimiter.
!! The integer output variable nArgs contains the number of arguments found.
SUBROUTINE StringTokenize &
!
 (string, delims, args, nArgs)
 
! Declarations: 
  
IMPLICIT NONE 
     
! Subroutine arguments 
! Scalar arguments with intent(in): 
CHARACTER(LEN = *), INTENT(IN) :: string
CHARACTER(LEN = *), INTENT(IN) :: delims           
         
! Scalar arguments with intent(out): 
INTEGER (KIND = short), INTENT(OUT)  :: nArgs 
         
! Array  arguments with intent(out): 
CHARACTER (len=*), POINTER :: args(:)
                  
! Local scalars:
!! local copy of string to tokenize
CHARACTER (LEN = LEN_TRIM(string)) :: strSav
INTEGER (KIND = short)  :: na
INTEGER (KIND = short)  :: i
!------------end of declaration------------------------------------------------

strSav = StringCompact (string)
IF ( LEN_TRIM (strSav) == 0 ) RETURN      !string is empty

! Count number of tokens in string
nArgs = 0
DO i = 1, LEN_TRIM(strSav)
  IF ( INDEX ( delims,strSav(i:i) ) > 0 ) THEN !the character is a delimiter
    nArgs = nArgs + 1
  END IF
END DO

nArgs = nArgs + 1 !number of tokens are number of found delimiters + 1

!allocate space for tokens
ALLOCATE ( args(nArgs) )

!initialize tokens
DO i = 1,nArgs
  args(i) = ' '
END DO  

na = 0
DO
   IF (LEN_TRIM(strSav) == 0) EXIT
   na = na + 1
   CALL StringSplit(delims,strSav,args(na))
END DO   

END SUBROUTINE StringTokenize


!==============================================================================
!! Description:
!! Converts multiple spaces and tabs to single spaces; 
!! deletes control characters; removes initial spaces.
!! Arguments:
!!     string     String to be treated
!! Result:
!!     String compacted 
FUNCTION StringCompact &
  ( string )           &
RESULT (new)

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER(LEN=*)   :: string

! Local scalars:
CHARACTER (LEN=LEN(string)) :: new
CHARACTER (LEN = 1)         :: ch
INTEGER (KIND = short)      :: isp
INTEGER (KIND = short)      :: ich
INTEGER (KIND = short)      :: i,k 
INTEGER (KIND = short)      :: length
!------------end of declaration------------------------------------------------

string = ADJUSTL (string)
length = LEN_TRIM (string)
new = ' '
isp = 0
k = 0

DO i = 1,length
  ch = string(i:i)
  ich = IACHAR (ch)
  
  SELECT CASE (ich)
  
    CASE(9,32)     ! space or tab character
      IF ( isp == 0 ) THEN
        k = k + 1
        new (k:k) = ' '
      END IF
      isp = 1
      
    CASE(33:)      ! not a space, quote, or control character
      k = k + 1
      new (k:k) = ch
      isp = 0
      
  END SELECT
  
END DO

new = ADJUSTL (new)

END FUNCTION StringCompact


!==============================================================================
!! Description:
!! Finds the first instance of a character from 'delims' in the
!! the string 'string'. The characters before the found delimiter are
!! output in 'before'. The characters after the found delimiter are
!! output in 'string'. The optional output character 'sep' contains the 
!! found delimiter. 
!! Arguments:
!!     string     String to be treated
!! Result:
!!     The characters before the found delimiter, the remainder
!!     is output in string
SUBROUTINE StringSplit &
!
  ( delims, string, before, sep )

IMPLICIT NONE

! Subroutine arguments
! Scalar arguments with intent(in):
CHARACTER(LEN=*), INTENT (IN) :: delims

! Scalar arguments with intent(inout):
CHARACTER(LEN=*), INTENT (INOUT) :: string

! Scalar arguments with intent(out):
CHARACTER(LEN=*), INTENT (OUT) :: before
CHARACTER(LEN=*), OPTIONAL, INTENT (OUT) :: sep

! Local scalars:
CHARACTER (LEN = 1)         :: ch
CHARACTER (LEN = 1)         :: cha
LOGICAL                     :: pres
INTEGER (KIND = short)      :: iposa
INTEGER (KIND = short)      :: ipos
INTEGER (KIND = short)      :: i,k 
INTEGER (KIND = short)      :: length
!------------end of declaration------------------------------------------------

pres = PRESENT(sep)
string = ADJUSTL (string)
string = StringCompact (string)
length = LEN_TRIM (string)
IF (length == 0) RETURN        ! string is empty
k = 0
before = ' '
DO i = 1,length
   ch = string(i:i)
   ipos = INDEX (delims,ch)         
   IF (ipos == 0) THEN         ! character is not a delimiter
      k = k + 1
      before(k:k) = ch
      CYCLE
   END IF
   IF (ch /= ' ') THEN         ! character is a delimiter that is not a space
      string = string (i+1:)
      IF (pres) sep = ch
      EXIT
   END IF
   cha = string (i+1 : i+1)    ! character is a space delimiter
   iposa = INDEX (delims,cha)
   IF (iposa > 0) THEN         ! next character is a delimiter
      string = string (i+2:)
      IF (pres) sep = cha
      EXIT
   ELSE
      string = string (i+1:)
      IF (pres) sep = ch
      EXIT
   END IF
END DO
IF (i >= length) string = ''
string = ADJUSTL (string)      ! remove initial spaces
RETURN

END SUBROUTINE StringSplit

!==============================================================================
!! Description:
!! Converts number string to a double precision real number 
!! Arguments:
!!     string     String to be converted
!! Result:
!!     double precision real number 
FUNCTION StringToDouble &
  ( string )           &
RESULT (number)

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER(LEN = *), INTENT (IN)  :: string

! Local scalars:
REAL (KIND = double)    :: number
INTEGER (KIND = short)  :: ios 

!------------end of declaration------------------------------------------------

READ (string,*,iostat = ios) number

IF ( ios /= 0 ) THEN
  CALL Catch ('error', 'StringManipulation', 'converting string to double &
               precision number ', code = genericIOError, argument = string )
END IF

END FUNCTION StringToDouble


!==============================================================================
!! Description:
!! Converts number string to a  real number 
!! Arguments:
!!     string     String to be converted
!! Result:
!!     float number 
FUNCTION StringToFloat &
  ( string )           &
RESULT (number)

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER(LEN = *), INTENT (IN)  :: string

! Local scalars:
REAL (KIND = float)     :: number
INTEGER (KIND = short)  :: ios 

!------------end of declaration------------------------------------------------

READ (string,*,iostat = ios) number

IF ( ios /= 0 ) THEN
  CALL Catch ('error', 'StringManipulation', 'converting string to float ',&
               code = genericIOError, argument = string )
END IF

END FUNCTION StringToFloat


!==============================================================================
!! Description:
!! Converts number string to a  long integer 
!! Arguments:
!!     string     String to be converted
!! Result:
!!     long integer
FUNCTION StringToLong &
  ( string )          &
RESULT (number)

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER(LEN = *), INTENT (IN)    :: string

! Local scalars:
INTEGER (KIND = long)   :: number
INTEGER (KIND = short)  :: ios 

!------------end of declaration------------------------------------------------

READ (string,*,iostat = ios) number

IF ( ios /= 0 ) THEN
  CALL Catch ('error', 'StringManipulation', 'converting string to &
               long integer ', code = genericIOError, argument = string )
END IF

END FUNCTION StringToLong

!==============================================================================
!! Description:
!! Converts number string to a  short integer 
!! Arguments:
!!     string     String to be converted
!! Result:
!!     short integer
FUNCTION StringToShort &
  ( string )           &
RESULT (number)

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER(LEN = *), INTENT (IN)  :: string

! Local scalars:
INTEGER (KIND = short)  :: number
INTEGER (KIND = short)  :: ios 

!------------end of declaration------------------------------------------------

READ (string,*,iostat = ios) number

IF ( ios /= 0 ) THEN
  CALL Catch ('error', 'StringManipulation', 'converting string to &
               short integer ', code = genericIOError, argument = string )
END IF

END FUNCTION StringToShort

!==============================================================================
!! Description:
!! Converts a double precision number in a string 
!! Arguments:
!!     number     number to be converted
!! Result:
!!     string
FUNCTION DoubleToString &
  ( number, fmt )       &
RESULT (string)

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
REAL (KIND = double), INTENT (IN)  :: number
CHARACTER (LEN = *), INTENT (IN), OPTIONAL :: fmt

! Local scalars:
CHARACTER(LEN = 100)    :: string
INTEGER (KIND = short)  :: ios 

!------------end of declaration------------------------------------------------

IF (PRESENT (fmt)) THEN
  WRITE(string, fmt) number
ELSE
  WRITE(string,*) number
END IF
string = ADJUSTL (string)

END FUNCTION DoubleToString

!==============================================================================
!! Description:
!! Converts a real number in a string 
!! Arguments:
!!     number     number to be converted
!! Result:
!!     string
FUNCTION FloatToString &
  ( number, fmt )       &
RESULT (string)

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
REAL (KIND = float), INTENT (IN)  :: number
CHARACTER (LEN = *), INTENT (IN), OPTIONAL :: fmt

! Local scalars:
CHARACTER(LEN = 100)    :: string
INTEGER (KIND = short)  :: ios 

!------------end of declaration------------------------------------------------

IF (PRESENT (fmt)) THEN
  WRITE(string, fmt) number
ELSE
  WRITE(string,*) number
END IF
string = ADJUSTL (string)

END FUNCTION FloatToString


!==============================================================================
!! Description:
!! Converts a long integer number in a string 
!! Arguments:
!!     number     number to be converted
!! Result:
!!     string
FUNCTION LongToString &
  ( number, fmt )       &
RESULT (string)

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
INTEGER (KIND = long), INTENT (IN)  :: number
CHARACTER (LEN = *), INTENT (IN), OPTIONAL :: fmt

! Local scalars:
CHARACTER(LEN = 100)    :: string
INTEGER (KIND = short)  :: ios 

!------------end of declaration------------------------------------------------

IF (PRESENT (fmt)) THEN
  WRITE(string, fmt) number
ELSE
  WRITE(string,*) number
END IF
string = ADJUSTL (string)

END FUNCTION LongToString

!==============================================================================
!! Description:
!! Converts a short integer number in a string 
!! Arguments:
!!     number     number to be converted
!! Result:
!!     string
FUNCTION ShortToString &
  ( number, fmt )       &
RESULT (string)

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
INTEGER (KIND = short), INTENT (IN)  :: number
CHARACTER (LEN = *), INTENT (IN), OPTIONAL :: fmt

! Local scalars:
CHARACTER(LEN = 100)    :: string
INTEGER (KIND = short)  :: ios 

!------------end of declaration------------------------------------------------

IF (PRESENT (fmt)) THEN
  WRITE(string, fmt) number
ELSE
  WRITE(string,*) number
END IF
string = ADJUSTL (string)
END FUNCTION ShortToString




! TO DO

!PRIVATE NumberToString:
!PRIVATE DoubleToString
!PRIVATE FloatToString
!PRIVATE LongToString
!PRIVATE ShortToString

END MODULE StringManipulation


