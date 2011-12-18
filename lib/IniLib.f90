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
!!   routines for reading configuration files
!!   with each line of the form line 'name = value'
!!   with support for sections [] and subsections [[ ]].
!!   Comments are denoted by '#' and can occupy a an entire line
!!   or terminating one
!!   example file:
!!> #example ini file
!!  key1 = value1
!!  key2 = value2
!!    [section1]   # first section
!!      key1 = value3
!!      key2 = value4
!!    [section2]   # second section
!!      key1 = value5
!!      key2 = value6
!!      [[subsection]] # subsection in section1
!!        key1 = value7
!!<       key2 = value8
!!
!! Adapted from Antony Lewis (http://cosmologist.info/).
!! @author <a href="mailto:giovanni.ravazzani@polimi.it">Giovanni Ravazzani</a> 
!! @version 1.3 - 11th August 2009 
MODULE IniLib       
    
! History: 
!  
! Version   Date          Comment 
! -------   ----          ------- 
!  1.0      01/Feb/2007   Original code. giovanni ravazzani
!  1.1      09/Mar/2007   Added support for sections and subsections. giovanni ravazzani
!  1.2      25/Oct/2008   Added support for multiple ini files
!                         using type IniList
!  1.3      11/Aug/2009   Added SectionIsPresent
! 
! Modules used: 
! 
USE DataTypeSizes, ONLY : & 
! Imported Parameters: 
short, long, float, double         
         
USE Utilities, ONLY : &
! Imported Routines: 
GetUnit

USE LogLib, ONLY : &          
! Imported Routines:
Catch


USE ErrorCodes, ONLY : &
! Imported Parameters: 
iniIOError, openFileError


IMPLICIT NONE

! Local (i.e. private) Declarations: 
! Local Procedures
PRIVATE :: CheckClosure
PRIVATE :: IniAddLine

! Local Parameters: 
INTEGER (KIND = long), PRIVATE, PARAMETER :: stringLen = 500 
         
! Local Scalars:
INTEGER (KIND = long), PRIVATE    :: ios 
INTEGER (KIND = long), PRIVATE    :: numKeys = 0
LOGICAL, PRIVATE    :: inSection
LOGICAL, PRIVATE    :: inSubSection
         
! Global (i.e. public) Declarations:

!Global Type Definitions:

!! define a dynamic list to store elements in memory
TYPE IniList
INTEGER (KIND = long) :: numKeys
INTEGER (KIND = long) :: nOfSections
INTEGER (KIND = long) :: nOfSubSections 
CHARACTER (LEN = stringLen), POINTER :: keys (:) 
CHARACTER (LEN = stringLen), POINTER :: vals (:)
CHARACTER (LEN = stringLen), POINTER :: sectionName (:)
CHARACTER (LEN = stringLen), POINTER :: subSectionName (:)
INTEGER   (KIND = long), POINTER   :: sectionBegin (:)
INTEGER   (KIND = long), POINTER   :: sectionEnd (:)
INTEGER   (KIND = long), POINTER   :: subSectionBegin (:)
INTEGER   (KIND = long), POINTER   :: subSectionEnd (:)
END TYPE IniList


! Global Procedures
PUBLIC :: IniOpen
PUBLIC :: IniReadString
PUBLIC :: IniReadInt
PUBLIC :: IniReadReal
PUBLIC :: IniReadDouble
PUBLIC :: IniReadLogical
PUBLIC :: KeyIsPresent
PUBLIC :: SectionIsPresent
PUBLIC :: IniClose
         
                  
! Operator definitions: 
!   Define new operators or overload existing ones. 


!=======         
CONTAINS
!======= 
! Define procedures contained in this module. 


!==============================================================================
!! Description: 
!!   check if section and subsection still opened
SUBROUTINE CheckClosure &
!
(iniDB)

IMPLICIT NONE

! subroutine arguments
! Arguments with intent (inout):
TYPE (IniList), INTENT (INOUT) :: iniDB
! Local Scalars:
INTEGER (KIND = long) :: i

!------------end of declaration------------------------------------------------ 

IF (iniDB % nOfSections /= 0) THEN
  IF ( iniDB % sectionEnd(iniDB % nOfSections) == 0 ) THEN
	  iniDB % sectionEnd(iniDB % nOfSections) = iniDB % numKeys
  ENDIF
ENDIF

IF (iniDB % nOfSubSections /= 0) THEN
  IF ( iniDB % subSectionEnd(iniDB % nOfSubSections) == 0 ) THEN
	  iniDB % subSectionEnd(iniDB % nOfSubSections) = iniDB % numKeys
  ENDIF
ENDIF

RETURN
END SUBROUTINE CheckClosure


!==============================================================================
!! Description: 
!!   count Key-Val pair in a file
FUNCTION IniCountKeys &
  ( unit )            &
RESULT (count)

IMPLICIT NONE
! function arguments 
! Scalar arguments with intent(in):
INTEGER (KIND = short), INTENT(in) :: unit

!Local scalar:
INTEGER (KIND = long) :: eqPos
INTEGER (KIND = long) :: count
CHARACTER (LEN = stringLen ) :: inLine
INTEGER (KIND = short) :: ios

!------------end of declaration------------------------------------------------ 

REWIND (unit)
ios = 0
count = 0
DO 
  READ (unit,'(a)',IOSTAT = ios) inLine
  IF (ios < 0 ) THEN
    EXIT
  END IF
  inLine = TRIM ( ADJUSTL ( inLine ) )
  !search for key
  eqPos = SCAN ( inLine , '=' )
  IF ( eqPos /= 0 .AND. inLine(1:1) /= '#' ) THEN 
	count = count + 1      
  END IF
END DO

END FUNCTION IniCountKeys


!==============================================================================
!! Description: 
!!   add a new Key-Val pair 
SUBROUTINE IniAddLine &
!
( aInLine, iniDB )

IMPLICIT NONE
! subroutine arguments 
! Scalar arguments with intent(in):
CHARACTER (LEN=*), INTENT(in) :: aInLine

! Array arguments with intent(out):
TYPE (IniList), INTENT(OUT) :: iniDB

!Local scalar:
INTEGER (KIND = long) :: eqPos, commentPos, lastPos
CHARACTER (LEN = stringLen ) :: s, inLine

!------------end of declaration------------------------------------------------ 

inLine = TRIM ( ADJUSTL ( aInLine ) )

!search for section or subsection

IF ( inLine(1:1) == '[' .AND. inLine(2:2) /= '[' ) THEN
	IF (inSection) THEN
		iniDB % sectionEnd(iniDB % nOfSections) = numKeys
		inSection = .TRUE.
	ENDIF
	IF (inSubSection) THEN
		iniDB % subSectionEnd(iniDB % nOfSubSections) = numKeys
		inSubSection = .FALSE.
	ENDIF
	inSection = .TRUE.
	iniDB % nOfSections = iniDB % nOfSections + 1
	iniDB % sectionName(iniDB % nOfSections) = &
	      inLine (2 : SCAN (inLine, ']') - 1)
	iniDB % sectionBegin(iniDB % nOfSections) = numKeys + 1  
	
ENDIF

IF ( inLine(1:2) == '[[' ) THEN
	IF (inSubSection) THEN
		iniDB % subSectionEnd(iniDB % nOfSubSections) = numKeys
		inSubSection = .FALSE.
	ENDIF
	inSubSection = .TRUE.
	iniDB % nOfSubSections = iniDB % nOfSubSections + 1
	iniDB % subSectionName(iniDB % nOfSubSections) = &
	      inLine (3 : SCAN (inLine, ']') - 1)
	iniDB % subSectionBegin(iniDB % nOfSubSections) = numKeys + 1

ENDIF

eqPos = SCAN ( inLine , '=' )
IF ( eqPos /= 0 .AND. inLine(1:1) /= '#' ) THEN
   
	numKeys = numKeys + 1      
  
	iniDB % keys(numKeys) = TRIM(inLine(1 : eqPos - 1))
	s = ADJUSTL(inLine(eqPos + 1:)) 
	commentPos = SCAN(s,'#')
	IF (commentPos /= 0) THEN
		s  = s (1 : commentPos - 1)
	END IF
	lastPos = LEN_TRIM(s)
	IF (lastPos > 1) THEN
		IF ( s ( 1 : 1 ) == '''' .AND. s ( lastPos : lastPos ) == '''') THEN
			s = s ( 2 : lastPos - 1 )
		END IF
	END IF
	iniDB % vals(numKeys) = TRIM(s)  
END IF

END SUBROUTINE IniAddLine

!==============================================================================
!! Description: 
!!   open and read a ini file
SUBROUTINE IniOpen &
!
(fileName, iniDB)

IMPLICIT NONE
! subroutine arguments 
! Scalar arguments with intent(in):
CHARACTER (LEN = *), INTENT(IN)  :: fileName

! Array arguments with intent(out):
TYPE (IniList), INTENT(OUT)  :: iniDB 

! Local Scalars: 
CHARACTER (LEN = stringLen)     :: inLine
INTEGER (KIND = long)           :: i
INTEGER (KIND = short)          :: unit_id
!------------end of declaration------------------------------------------------

!------------------------------------------------------------------------------
![1.0] Inizialization:
!------------------------------------------------------------------------------

iniDB % nOfSections = 0
iniDB % nOfSubSections = 0
iniDB % numKeys = 0
numKeys = 0

unit_id = GetUnit ()
    
OPEN(UNIT = unit_id, FILE = fileName, FORM = 'formatted', &
     STATUS = 'old', IOSTAT = ios)
IF (ios > 0) THEN
   CALL Catch ('error', 'IniLib',   &
			   'error in opening file: ' ,  &
			   code = openFileError, argument = filename )
ENDIF

!count number of keys in  file
iniDB % numKeys = IniCountKeys (unit_id)

!allocate space
ALLOCATE ( iniDB % keys ( iniDB % numKeys ) )
ALLOCATE ( iniDB % vals ( iniDB % numKeys ) )
ALLOCATE ( iniDB % sectionName ( iniDB % numKeys ) )
ALLOCATE ( iniDB % subSectionName ( iniDB % numKeys ) )
ALLOCATE ( iniDB % sectionBegin ( iniDB % numKeys ) )
ALLOCATE ( iniDB % sectionEnd ( iniDB % numKeys ) )
ALLOCATE ( iniDB % subSectionBegin ( iniDB % numKeys ) )
ALLOCATE ( iniDB % subSectionEnd ( iniDB % numKeys ) )

iniDB % keys = ''
iniDB % vals = ''
iniDB % sectionName = ''
iniDB % subSectionName = ''
iniDB % sectionBegin = 0
iniDB % sectionEnd = 0
iniDB % subSectionBegin = 0
iniDB % subSectionEnd = 0
   
inSection = .FALSE.
inSubSection = .FALSE.

!------------------------------------------------------------------------------
![2.0] Parse ini file to the end of file:
!------------------------------------------------------------------------------
REWIND (unit_id)
DO 
  READ (unit_id,'(a)',IOSTAT = ios) inLine
  IF (ios < 0) THEN !end of file encountered
	CALL CheckClosure(iniDB)
    CLOSE (unit_id)
	EXIT    
  ENDIF
  IF (inLine /= '') CALL IniAddLine(inLine, iniDB) 
END DO

!------------------------------------------------------------------------------
![3.0] close ini file:
!------------------------------------------------------------------------------
CLOSE (unit_id)

RETURN

END SUBROUTINE iniOpen

!==============================================================================
!! Description: 
!!   close a ini file
SUBROUTINE IniClose &
!
 (iniDB)
 
IMPLICIT NONE
! subroutine arguments 

! Array arguments with intent(out):
TYPE (IniList), INTENT(OUT)  :: iniDB 
!------------end of declaration------------------------------------------------

    DEALLOCATE (iniDB % keys)
    DEALLOCATE (iniDB % vals)
	DEALLOCATE (iniDB % sectionName)
	DEALLOCATE (iniDB % subSectionName)
	DEALLOCATE (iniDB % sectionBegin)
	DEALLOCATE (iniDB % sectionEnd)
	DEALLOCATE (iniDB % subSectionBegin)
	DEALLOCATE (iniDB % subSectionEnd)

END SUBROUTINE IniClose

!==============================================================================
!! Description: 
!!   synchronize the window in which searching for the key
SUBROUTINE Sync &
!
(first,last,iniDB,sec,subSec)

IMPLICIT NONE

! subroutine arguments 
! Scalar arguments with intent(in):
CHARACTER (LEN = *), OPTIONAL, INTENT(IN) :: sec
CHARACTER (LEN = *), OPTIONAL, INTENT(IN) :: subSec
TYPE (IniList), INTENT (IN) :: iniDB

! Scalar arguments with intent(out): 
INTEGER (KIND = long), INTENT (OUT) :: first
INTEGER (KIND = long), INTENT (OUT) :: last

! Local Scalars:
INTEGER (KIND = long) :: i ! loop index 
INTEGER (KIND = long) :: j ! loop index 

!------------end of declaration------------------------------------------------


! if not present section and subsection key must to be serached in the root
IF ( .NOT.PRESENT (sec) .AND. .NOT.PRESENT (subSec) ) THEN
	first = 1
	IF ( iniDB % sectionBegin(1) == 0) THEN !there are not sections in ini file
		last = iniDB % numKeys
	ELSE  !root terminates one element before first section begin
		last = iniDB % sectionBegin(1) - 1  
	ENDIF	
ENDIF

! if present section limit window to that section
IF ( PRESENT (sec) .AND. .NOT.PRESENT (subSec) ) THEN

	DO i = 1, iniDB % nOfSections
		IF (iniDB % sectionName (i) == sec) THEN
			EXIT !found section
		ENDIF 
	ENDDO

	first = iniDB % sectionBegin (i)
	last  = iniDB % sectionEnd (i)
ENDIF

! if present subsection limit window to that subsection
IF ( PRESENT (sec) .AND. PRESENT (subSec) ) THEN

	DO i = 1, iniDB % nOfSections
		IF (iniDB % sectionName (i) == sec) THEN
			EXIT !found section
		ENDIF 
	ENDDO
	
	!search for subsection in the section
	DO j = 1, iniDB % nOfSubSections

		IF (iniDB % subSectionName (j) == subSec) THEN
			IF (iniDB % subSectionBegin (j) >= iniDB % sectionBegin (i) .AND. &
				iniDB % subSectionEnd (j) <= iniDB % sectionEnd (i) ) THEN
				EXIT !found subsection
			ELSE
				CYCLE
			ENDIF				
		ENDIF 

	ENDDO

	first = iniDB % subSectionBegin (j)
	last  = iniDB % subSectionEnd (j)
	
ENDIF

RETURN
END SUBROUTINE Sync


!==============================================================================
!! Description: 
!!   read a string corresponding to Key 
FUNCTION IniReadString &
!
(key, iniDB, section, subSection)

IMPLICIT NONE

! subroutine arguments 
! Scalar arguments with intent(in):
CHARACTER (LEN = *),           INTENT(IN) :: key
TYPE (IniList)     ,           INTENT(IN) :: iniDB
CHARACTER (LEN = *), OPTIONAL, INTENT(IN) :: section
CHARACTER (LEN = *), OPTIONAL, INTENT(IN) :: subSection

! Scalar arguments with intent(out):
CHARACTER (LEN = stringLen) :: IniReadString
! Local Scalars: 
INTEGER (KIND = long) :: elmBegin
INTEGER (KIND = long) :: elmEnd
INTEGER (KIND = long) :: i
!------------end of declaration------------------------------------------------

IF ( PRESENT (section) .AND. PRESENT (subSection) ) THEN
 CALL Sync(elmBegin, elmEnd, iniDB, sec = section, subSec = subSection)
ELSE IF ( PRESENT (section) .AND. .NOT.PRESENT (subSection)) THEN
	CALL Sync(elmBegin, elmEnd, iniDB, sec = section)
ELSE
	CALL Sync(elmBegin, elmEnd, iniDB)
ENDIF

IniReadString = ''

DO i = elmBegin, elmEnd
   IF (Key == iniDB % Keys(i) ) THEN
      IniReadString = iniDB % Vals(i)
      RETURN
   END IF
END DO

END FUNCTION IniReadString



!==============================================================================
!! Description: 
!!   read an integer corresponding to Key 
FUNCTION IniReadInt &
!
(key, iniDB, section, subSection, default)

IMPLICIT NONE

! subroutine arguments 
! Scalar arguments with intent(in):
CHARACTER (LEN = *),             INTENT(IN) :: key
TYPE (IniList)     ,             INTENT(IN) :: iniDB
CHARACTER (LEN = *),   OPTIONAL, INTENT(IN) :: section
CHARACTER (LEN = *),   OPTIONAL, INTENT(IN) :: subSection
INTEGER (KIND = long), OPTIONAL, INTENT(in) :: default
! Scalar arguments with intent(out):
INTEGER (KIND = long) :: IniReadInt
! Local Scalars:
CHARACTER(LEN = stringLen) :: s

!------------end of declaration------------------------------------------------

IF ( PRESENT (section) .AND. PRESENT (subSection) ) THEN
	s = IniReadString(key, iniDB, section = section, subSection = subSection)
ELSE IF ( PRESENT (section) .AND. .NOT.PRESENT (subSection)) THEN
	s = IniReadString(key, iniDB, section = section)
ELSE
	s = IniReadString(key, iniDB)
ENDIF
  
IF (s == '') THEN
	IF ( PRESENT (default) )THEN
		IniReadInt = default
	ELSE
		CALL Catch ('error', 'read ini file',      &
				    'key not found: ' , code = iniIOError, &
					 argument = key )
	ENDIF
ELSE
	READ (s,*, IOSTAT = ios) IniReadInt
	IF (ios > 0) THEN
		CALL Catch ('error', 'read ini file',   &
				  'error reading integer for key: ' ,  &
				  code = iniIOError, argument = key )
	ENDIF
END IF

RETURN

END FUNCTION IniReadInt

!==============================================================================
!! Description: 
!!   read an double precision number corresponding to Key 
FUNCTION IniReadDouble &
!
(key, iniDB, section, subSection, default)

IMPLICIT NONE

! subroutine arguments 
! Scalar arguments with intent(in):
CHARACTER (LEN = *),           INTENT(IN) :: key
TYPE (IniList),                INTENT(IN) :: iniDB
CHARACTER (LEN = *), OPTIONAL, INTENT(IN) :: section
CHARACTER (LEN = *), OPTIONAL, INTENT(IN) :: subSection
REAL(KIND = double), OPTIONAL, INTENT(IN) :: default
! Scalar arguments with intent(out):
REAL (KIND = double)                      :: IniReadDouble 
! Local Scalars:
CHARACTER(LEN = stringLen) :: s
!------------end of declaration------------------------------------------------
   
IF ( PRESENT (section) .AND. PRESENT (subSection) ) THEN
	s = IniReadString(key, iniDB, section = section, subSection = subSection)
ELSE IF ( PRESENT (section) .AND. .NOT.PRESENT (subSection)) THEN
	s = IniReadString(key, iniDB, section = section)
ELSE
	s = IniReadString(key, iniDB)
ENDIF
  
IF (s == '') THEN
   IF ( PRESENT (default) )THEN
     IniReadDouble = default
   ELSE
     CALL Catch ('error', 'read ini file',              &
				 'key not found: ' , code = iniIOError, &
				 argument = key )
   ENDIF
ELSE
   READ (s,*, IOSTAT = ios) IniReadDouble
   IF (ios > 0) THEN
     CALL Catch ('error', 'read ini file',           &
				 'error reading double for key: ' ,  &
				 code = iniIOError, argument = key )
   ENDIF
END IF

RETURN

END FUNCTION IniReadDouble

!==============================================================================
!! Description: 
!!   read a real number corresponding to Key
FUNCTION iniReadReal &
!
(key, iniDB, section, subSection, default)

IMPLICIT NONE

! subroutine arguments 
! Scalar arguments with intent(in):
CHARACTER (LEN = *),           INTENT(IN) :: key
TYPE (IniList )    ,           INTENT(IN) :: iniDB
CHARACTER (LEN = *), OPTIONAL, INTENT(IN) :: section
CHARACTER (LEN = *), OPTIONAL, INTENT(IN) :: subSection
REAL(KIND = float) , OPTIONAL, INTENT(IN) :: default
! Scalar arguments with intent(out):
REAL (KIND = float)                       :: IniReadReal 
!Local Scalars:
CHARACTER(LEN = stringLen)                :: s
!------------end of declaration------------------------------------------------
   
IF ( PRESENT (section) .AND. PRESENT (subSection) ) THEN
	s = IniReadString(key, iniDB, section = section, subSection = subSection)
ELSE IF ( PRESENT (section) .AND. .NOT.PRESENT (subSection)) THEN
	s = IniReadString(key, iniDB, section = section)
ELSE
	s = IniReadString(key, iniDB)
ENDIF
  
IF (s == '') THEN
   IF ( PRESENT (default) )THEN
     IniReadReal = default
   ELSE
     CALL Catch ('error', 'read ini file',      &
				 'key not found: ' , code = iniIOError, &
				 argument = key )
   ENDIF
ELSE
   READ (s,*, IOSTAT = ios) IniReadReal
   IF (ios > 0) THEN
     CALL Catch ('error', 'read ini file',   &
				 'error reading real for key: ' ,  &
				 code = iniIOError, argument = key )
   ENDIF
END IF

RETURN

END FUNCTION IniReadReal

!==============================================================================
!! Description: 
!!   read a logical value corresponding to Key
FUNCTION IniReadLogical &
!
(key, iniDB, section, subSection, default)

IMPLICIT NONE

! subroutine arguments 
! Scalar arguments with intent(in):
CHARACTER (LEN = *),           INTENT(IN) :: key
TYPE (IniList)     ,           INTENT(IN) :: iniDB
CHARACTER (LEN = *), OPTIONAL, INTENT(IN) :: section
CHARACTER (LEN = *), OPTIONAL, INTENT(IN) :: subSection
LOGICAL,             OPTIONAL, INTENT(IN) :: default
! Scalar arguments with intent(out):
LOGICAL                    :: IniReadLogical 
! Local Scalars:
CHARACTER(LEN = stringLen) :: s
!------------end of declaration------------------------------------------------
   
IF ( PRESENT (section) .AND. PRESENT (subSection) ) THEN
	s = IniReadString(key, iniDB, section = section, subSection = subSection)
ELSE IF ( PRESENT (section) .AND. .NOT.PRESENT (subSection)) THEN
	s = IniReadString(key, iniDB, section = section)
ELSE
	s = IniReadString(key, iniDB)
ENDIF
  
IF (s == '') THEN
   IF ( PRESENT (default) )THEN
     IniReadLogical = default
   ELSE
     CALL Catch ('error', 'read ini file',      &
				 'key not found: ' , code = iniIOError, &
				 argument = key )
   ENDIF
ELSE
   READ (s,*, IOSTAT = ios) IniReadLogical
   IF (ios > 0) THEN
     CALL Catch ('error', 'read ini file',   &
				 'error reading logical for key: ' ,  &
				 code = iniIOError, argument = key )
   ENDIF
END IF

RETURN

END FUNCTION IniReadLogical




!==============================================================================
!! Description: 
!!   return true if key is present, false otherwise 
FUNCTION KeyIsPresent &
!
(key, iniDB, section, subSection) &
!
RESULT (isHere)

IMPLICIT NONE

! subroutine arguments 
! Scalar arguments with intent(in):
CHARACTER (LEN = *),           INTENT(IN) :: key
TYPE (IniList)     ,           INTENT(IN) :: iniDB
CHARACTER (LEN = *), OPTIONAL, INTENT(IN) :: section
CHARACTER (LEN = *), OPTIONAL, INTENT(IN) :: subSection

! Local Scalars: 
LOGICAL :: isHere
INTEGER (KIND = long) :: elmBegin
INTEGER (KIND = long) :: elmEnd
INTEGER (KIND = long) :: i
!------------end of declaration------------------------------------------------

IF ( PRESENT (section) .AND. PRESENT (subSection) ) THEN
 CALL Sync(elmBegin, elmEnd, iniDB, sec = section, subSec = subSection)
ELSE IF ( PRESENT (section) .AND. .NOT.PRESENT (subSection)) THEN
	CALL Sync(elmBegin, elmEnd, iniDB, sec = section)
ELSE
	CALL Sync(elmBegin, elmEnd, iniDB)
ENDIF

isHere = .FALSE.

DO i = elmBegin, elmEnd
   IF (Key == iniDB % Keys(i) ) THEN
      isHere = .TRUE.
      RETURN
   END IF
END DO

END FUNCTION KeyIsPresent


!==============================================================================
!! Description: 
!!   return true if section is present, false otherwise 
FUNCTION SectionIsPresent &
!
(section, iniDB) &
!
RESULT (isHere)

IMPLICIT NONE

! subroutine arguments 
! Scalar arguments with intent(in):
TYPE (IniList)     ,           INTENT(IN) :: iniDB
CHARACTER (LEN = *), OPTIONAL, INTENT(IN) :: section

! Local Scalars: 
LOGICAL :: isHere
INTEGER (KIND = long) :: i
!------------end of declaration------------------------------------------------

isHere = .FALSE.

DO i = 1, iniDB % nOfSections
   IF (iniDB % sectionName (i) == section ) THEN
      isHere = .TRUE.
      RETURN
   END IF
END DO

END FUNCTION SectionIsPresent




END MODULE IniLib
         