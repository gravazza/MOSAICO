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
!!   Module for manage tables
!!   Tables are stored in plain text. 
!!   One single file may contain more than one table. 
!!   Example of a table:
!!
!!> Table Start
!!  Title: Stage discharge relatioship. #inline comment
!!  Id: Tab01 # mandatory
!!	
!!  # This is a sample comment.  You can put anything you want
!!  # on comment lines. Comment lines can be put everywhere
!!
!!  Columns:    [Stage]        [Discharge]      [Method]
!!  Units:     [m a.s.l.]         [m3/s]          [-]
!!              300.0             10.0          measure
!!              300.5             20.0          measure
!!              301.0             50.0          measure
!!              301.5             100.0         extrapolation
!!              302.0             200.0         extrapolation
!!              302.5             500.0         extrapolation
!!  # another comment
!!< Table End
!!
!! Example program:
!!>PROGRAM TestTableLib
!!
!!USE TableLib
!!USE DataTypeSizes
!!
!!TYPE (TableCollection) :: tables
!!TYPE (Table) :: tab, tab2
!!REAL (KIND = float) :: out
!!REAL (KIND = double) :: dOut
!!INTEGER :: i
!!
!!!initialize a new table reading from the file passed as an argument.
!!!At the end of initialization, file is closed.
!!CALL TableNew ( 'table.txt', tab )
!!
!!!initialize a new table reading from a file already open. 
!!!Unit of the file is passed as an argument.
!!!At the end of initialization, file is not closed.
!!OPEN (unit=10, file='table2.txt')
!!CALL TableNew ( 10, tab2 ) !mettere controllo che il file sia aperto.
!!
!!!initialize a collection of tables reading from a file whose name 
!!!is specified as argument.
!!CALL TableNew ( 'tables.txt', tables )
!!
!!!extract value from table with different methods.
!!!input value must be a float number. Output can be long integer, 
!!!float real or double real.
!!
!!!get float that corresponds exactly to input float. Bound is not 
!!!necessary (it does not make sense).
!!!If input value is not found, an error message is logged.
!!CALL TableGetValue ( 302., tab, 'stage', 'discharge', 'exact', out)
!!WRITE(*,*) 'The discharge corresponding to selected case is: ', out
!!
!!!get float calculating a linear interpolation between the two nearest values. 
!!!Option bound = 'fixed' limits the search inside the extreme values 
!!!of the table. If extreme values are exceeded, an error is thrown.
!!CALL TableGetValue ( 302.4, tab, 'stage', 'discharge', 'linear', out, &
!!                     bound = 'fixed' )
!!WRITE(*,*) 'The discharge corresponding to input value is: ', out
!!
!!!get float calculating a linear interpolation between the two nearest values. 
!!!Option bound = 'extendlinear' means that if the input value is outside 
!!!extreme values of the table, they are linearly extended  using the 
!!!last two elements of the table. A warning message is logged.
!!CALL TableGetValue ( 304., tab, 'stage', 'discharge', 'linear', out, &
!!                     bound = 'extendlinear' )
!!WRITE(*,*) 'The discharge corresponding to input value is: ', out
!!
!!!get float calculating a linear interpolation between the two nearest values. 
!!!Option bound = 'extendconstant' means that if the input value is 
!!!outside extreme values of the table, the last element is extended 
!!!as a constant. A warning message is logged.
!!CALL TableGetValue ( 304., tab, 'stage', 'discharge', 'linear', out, &
!!                     bound = 'extendconstant' )
!!WRITE(*,*) 'The discharge corresponding to input value is: ', out
!!
!!!get float searching for the nearest value.
!!CALL TableGetValue ( 302.55, tab, 'stage', 'discharge', 'nearest', out)
!!WRITE(*,*) 'The discharge nearest to input value is: ', out
!!
!!!get double calculating a linear interpolation between the two nearest values. 
!!!Option bound = 'fixed' limits the search inside the extreme values of the 
!!!table. If extreme values are exceeded, an error is thrown.
!!CALL TableGetValue ( 302.4, tab, 'stage', 'discharge', 'linear', dOut, &
!!                     bound = 'fixed' )
!!WRITE(*,*) 'The discharge corresponding to input value is: ', dOut
!!
!!!get float calculating a linear interpolation between the two nearest values. 
!!!Option bound = 'fixed' limits the search inside the extreme values of 
!!!the table. If extreme values are exceeded, an error is thrown.
!!CALL TableGetValue ( 302.4, tables, 'tab02', 'stage', 'discharge', 'linear', &
!!                     out, bound = 'fixed' )
!!WRITE(*,*) 'The discharge corresponding to input value is: ', out
!!
!!!get double calculating a linear interpolation between the two nearest values.
!!! Option bound = 'fixed' limits the search inside the extreme values of the table. 
!!!If extreme values are exceeded, an error is thrown.
!!CALL TableGetValue ( 302.4, tables, 'tab02', 'stage', 'discharge', 'linear', &
!!                     dOut, bound = 'fixed' )
!!WRITE(*,*) 'The discharge corresponding to input value is: ', dOut
!!
!!!export table on file. Name of the file is passed as argument
!!CALL TableExport ( tab, 'fileout.txt' )
!!
!!!export table on a file taht is already open. Unit of file is passed as argument
!!OPEN (UNIT = 20, file = 'exported_table.txt')
!!CALL TableExport ( tab, 20 )
!!CLOSE (20)
!!
!!!export a collection of tables on a file. Name of the file is passed as argument
!!CALL TableExport ( tables, 'table_collections.txt' )
!!
!!!export just one table from a collection of tables on a filetaht is already open. 
!!!Unit of file is passed as argument
!!OPEN (UNIT = 20, file = 'tab02.txt')
!!CALL TableExport ( tables, 20, 'tab02' )
!!CLOSE (20)
!!
!!<END PROGRAM TestTableLib
!!
!! References and Credits:
!!   ODT data table format http://math.nist.gov/oommf/doc/userguide11b2/userguide/Data_table_format_ODT.html
!!
!! Known issues:
!!   when processing table with lots of rows, stack overflow may occur. 
!!   Decrease LINELENGTH parameter or increase stack size before compiling.
!!
!! TODO
!! TableGetString
!! TableGetLong
!!
!! @author <a href="mailto:giovanni.ravazzani@polimi.it">Giovanni Ravazzani</a> 
!! @version 1.1 - 4th Januray 2011    
MODULE TableLib

! History: 
!  
! Version   Date          Comment 
! -------   ----          ------- 
! 1.0       1/Oct/2008    Original code. giovanni ravazzani
! 1.1       4/Jan/2011    Read table specifing an id
! 
! Code Description: 
!   Language:           Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!     Documenting Exchangeable Fortran 90 Code". 

! Declarations:

! Modules used:
USE DataTypeSizes, ONLY : &
!Imported parameters:
short, long, float, double

USE LogLib, ONLY : &
! Imported Routines:
Catch

USE Utilities, ONLY : &
! Imported Routines:
LinearInterp


IMPLICIT NONE

! Global (i.e. public) declarations:
!Global type definitions:

TYPE Table
CHARACTER ( LEN = 300 ) :: title !! Max length of title: 300 characters
CHARACTER ( LEN = 30 )  :: id    !! Max length of id: 30 characters
INTEGER ( KIND = long ) :: noRows !! number of rows
INTEGER ( KIND = long ) :: noCols !! number of columns
TYPE (Column), POINTER  :: col (:) 
END TYPE Table

TYPE TableCollection
INTEGER (KIND = long) :: number
TYPE (Table), POINTER :: elem (:)
END TYPE TableCollection

! Global procedures:
PUBLIC :: TableNew
PUBLIC :: TableGetValue
PUBLIC :: TableExport
PUBLIC :: TableGetNrows


! Local (i.e. private) Declarations:
! Local Procedures:
PRIVATE :: TableFileSync
PRIVATE :: TableSyncById
PRIVATE :: TableReadTitle
PRIVATE :: TableReadId
PRIVATE :: TablesGetFromFile
PRIVATE :: TablesGetFromUnit
PRIVATE :: TableGetFromFile
PRIVATE :: TableGetFromUnit
PRIVATE :: TableStoreLines
PRIVATE :: TableCountCols
PRIVATE :: TableCountRows
PRIVATE :: CheckId
PRIVATE :: TableReadHeader
PRIVATE :: TableReadUnit
PRIVATE :: TableReadContent
PRIVATE :: TableGetFloat
PRIVATE :: TableGetString
PRIVATE :: TableGetDouble
PRIVATE :: TablesGetFloat
PRIVATE :: TablesGetDouble
PRIVATE :: TablesGetString
PRIVATE :: TableWriteToFile
PRIVATE :: TableWriteToUnit
PRIVATE :: TablesWriteToFile
PRIVATE :: TablesWriteToUnit
PRIVATE :: TableGetNumberRows



! Local Type Definitions:
TYPE Column
CHARACTER (LEN = 100) :: header
CHARACTER (LEN = 100) :: unit
CHARACTER (LEN = 100), POINTER :: row (:)
END TYPE Column

PRIVATE :: Column

! Local Parameters:
INTEGER (KIND = long), PRIVATE, PARAMETER :: LINELENGTH = 1000

! Operator definitions:
! Define new operators or overload existing ones.
INTERFACE TableNew
   MODULE PROCEDURE TablesGetFromFile
   MODULE PROCEDURE TablesGetFromUnit
   MODULE PROCEDURE TableGetFromFile
   MODULE PROCEDURE TableGetFromUnit
END INTERFACE

INTERFACE TableGetValue
   MODULE PROCEDURE TableGetFloat
   MODULE PROCEDURE TableGetDouble
   MODULE PROCEDURE TableGetString
   MODULE PROCEDURE TablesGetFloat
   MODULE PROCEDURE TablesGetDouble
   MODULE PROCEDURE TablesGetString
END INTERFACE

INTERFACE TableExport
   MODULE PROCEDURE TableWriteToFile
   MODULE PROCEDURE TableWriteToUnit
   MODULE PROCEDURE TablesWriteToFile
   MODULE PROCEDURE TablesWriteToUnit
END INTERFACE

INTERFACE TableGetNrows
   MODULE PROCEDURE TableGetNumberRows
   MODULE PROCEDURE TablesGetNumberRows
END INTERFACE

!=======
CONTAINS
!=======
! Define procedures contained in this module.

!==============================================================================
!! Description:
!! write a table on file. 
!!>Arguments:
!!   tab      table to export
!!<  file     file to whom write the table
SUBROUTINE TableWriteToFile &
( tab, file )

! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringCompact

USE Utilities, ONLY: &
!Imported routines:
GetUnit

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER (LEN = *),  INTENT (IN) :: file

! Type defined arguments with intent (in):
TYPE (Table), INTENT (IN) :: tab


!Local variables:
INTEGER (KIND = short) :: iunit
INTEGER (KIND = long)  :: i, j

!------------end of declaration------------------------------------------------

!get a free fortran unit
iunit = GetUnit ()
OPEN (UNIT = iunit, FILE = file, STATUS = "new")

!write keyword for start table
WRITE(iunit,'(a)') 'Table Start'

!write title if exists
IF ( StringCompact ( tab % title ) /= '' ) THEN
  WRITE(iunit,'(a7)', ADVANCE = 'no') 'Title: ' 
  WRITE(iunit,'(a)') TRIM (tab % title)
END IF

!write id
WRITE(iunit,'(a4)', ADVANCE = 'no') 'Id: ' 
WRITE(iunit,'(a)') TRIM (tab % id)

!write column headers
WRITE(iunit,'(a9)', ADVANCE = 'no') 'Columns: '
DO i = 1, tab % noCols - 1
  WRITE(iunit,'(a1,a,a2)', ADVANCE = 'no') '[', TRIM (tab % col (i) % header ), '] ' 
END DO
WRITE(iunit,'(a1,a,a1)') '[', TRIM (tab % col (tab % noCols) % header ), ']' 

!write column unit
WRITE(iunit,'(a7)', ADVANCE = 'no') 'Units: '
DO i = 1, tab % noCols - 1
  WRITE(iunit,'(a1,a,a2)', ADVANCE = 'no') '[', TRIM (tab % col (i) % unit ), '] ' 
END DO
WRITE(iunit,'(a1,a,a1)') '[', TRIM (tab % col (tab % noCols) % unit ), ']'

!write content
DO i = 1, tab % noRows
  DO j = 1, tab % noCols - 1
    WRITE(iunit,'(a)', ADVANCE = 'no')  TRIM (tab % col (j) % row (i) ) // ' '
  END DO
  WRITE(iunit,'(a)') TRIM (tab % col (tab % noCols) % row (i) )
END DO

!write keyword for close table
WRITE(iunit,'(a)') 'Table End'

CLOSE (iunit)

END SUBROUTINE TableWriteToFile

!==============================================================================
!! Description:
!! write a table on file taht is already open. 
!!>Arguments:
!!   tab      table to export
!!<  iunit    unit of file to whom write the table
SUBROUTINE TableWriteToUnit &
( tab, iunit )

! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringCompact

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
INTEGER (KIND = short),  INTENT (IN) :: iunit

! Type defined arguments with intent (in):
TYPE (Table), INTENT (IN) :: tab


!Local variables:
INTEGER (KIND = long)  :: i, j

!------------end of declaration------------------------------------------------

!write keyword for start table
WRITE(iunit,'(a)') 'Table Start'

!write title if exists
IF ( StringCompact ( tab % title ) /= '' ) THEN
  WRITE(iunit,'(a7)', ADVANCE = 'no') 'Title: ' 
  WRITE(iunit,'(a)') TRIM (tab % title)
END IF

!write id
WRITE(iunit,'(a4)', ADVANCE = 'no') 'Id: ' 
WRITE(iunit,'(a)') TRIM (tab % id)

!write column headers
WRITE(iunit,'(a9)', ADVANCE = 'no') 'Columns: '
DO i = 1, tab % noCols - 1
  WRITE(iunit,'(a1,a,a2)', ADVANCE = 'no') '[', TRIM (tab % col (i) % header ), '] ' 
END DO
WRITE(iunit,'(a1,a,a1)') '[', TRIM (tab % col (tab % noCols) % header ), ']' 

!write column unit
WRITE(iunit,'(a7)', ADVANCE = 'no') 'Units: '
DO i = 1, tab % noCols - 1
  WRITE(iunit,'(a1,a,a2)', ADVANCE = 'no') '[', TRIM (tab % col (i) % unit ), '] ' 
END DO
WRITE(iunit,'(a1,a,a1)') '[', TRIM (tab % col (tab % noCols) % unit ), ']'

!write content
DO i = 1, tab % noRows
  DO j = 1, tab % noCols - 1
    WRITE(iunit,'(a)', ADVANCE = 'no')  TRIM (tab % col (j) % row (i) ) // ' '
  END DO
  WRITE(iunit,'(a)') TRIM (tab % col (tab % noCols) % row (i) )
END DO

!write keyword for close table
WRITE(iunit,'(a)') 'Table End'

END SUBROUTINE TableWriteToUnit


!==============================================================================
!! Description:
!! write a collection of tables on file. If id is present, only the table 
!! corresponding to that id is written.
!!>Arguments:
!!   tables   collection of tables to be exported
!!<  file     file to whom write the table
SUBROUTINE TablesWriteToFile &
( tables, file, id )

USE Utilities, ONLY: &
!Imported routines:
GetUnit

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER (LEN = *),  INTENT (IN) :: file
CHARACTER (LEN = *),  OPTIONAL, INTENT (IN) :: id

! Type defined arguments with intent (in):
TYPE (TableCollection), INTENT (IN) :: tables


!Local variables:
INTEGER (KIND = short) :: iunit
INTEGER (KIND = long)  :: i

!------------end of declaration------------------------------------------------

!get a free fortran unit
iunit = GetUnit ()
OPEN (UNIT = iunit, FILE = file, STATUS = "new")


IF ( PRESENT (id) ) THEN
  CALL TableWriteToUnit (  tables % elem ( TableSyncById (tables, id) ), iunit ) 
ELSE
  DO i = 1, tables % number
    CALL TableWriteToUnit (  tables % elem (i), iunit )
    WRITE(iunit,*)
  END DO
END IF

CLOSE (iunit)

END SUBROUTINE TablesWriteToFile

!==============================================================================
!! Description:
!! write a collection of tables on file already open. If id is present, 
!! only the table corresponding to that id is written.
!!>Arguments:
!!   tables   collection of tables to be exported
!!<  iunit    unit to whom write the table
SUBROUTINE TablesWriteToUnit &
( tables, iunit, id )

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
INTEGER (KIND = short),  INTENT (IN) :: iunit
CHARACTER (LEN = *),  OPTIONAL, INTENT (IN) :: id

! Type defined arguments with intent (in):
TYPE (TableCollection), INTENT (IN) :: tables


!Local variables:
INTEGER (KIND = long)  :: i

!------------end of declaration------------------------------------------------

IF ( PRESENT (id) ) THEN
  CALL TableWriteToUnit (  tables % elem ( TableSyncById (tables, id) ), iunit ) 
ELSE
  DO i = 1, tables % number
    CALL TableWriteToUnit (  tables % elem (i), iunit )
    WRITE(iunit,*)
  END DO
END IF

END SUBROUTINE TablesWriteToUnit


!==============================================================================
!! Description:
!! returns a float from column defined by keyOut corresponding to valueIn 
!! contained in column defined by keyIn. 
!!>Arguments:
!!   valueIn  input value
!!   tab      table to search in
!!   keyIn    defines header of the column of the input value
!!   keyOut   defines header of the column of the output value
!!   match    method to match input value. Possible values are:
!!            'exact'  = column must contain exact input value
!!            'linear' = calculates linear interpolation between two 
!!                       bounding values
!!            'nearest'  = search for the nearest value in input column
!!   bound    method to manage bounds. Possible values are:
!!            'fixed'  = extreme values are treated as a wall
!!            'extendlinear' = extend bounds with linear interpolation 
!!                             of last two extreme values 
!!<           'extendconstant'  = extend bounds preserving extreme value constant
SUBROUTINE TableGetFloat &
!
( valueIn, tab, keyIn, keyOut, match, valueOut, bound )

! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringCompact, StringToUpper, StringToFloat, ToString

USE LogLib, ONLY : &
! Imported Routines:
Catch

USE ErrorCodes, ONLY : &
! Imported parameters:
unknownOption


IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
REAL (KIND = float),  INTENT (IN) :: valueIn
CHARACTER (LEN = *),  INTENT (IN) :: keyIn
CHARACTER (LEN = *),  INTENT (IN) :: keyOut
CHARACTER (LEN = *),  INTENT (IN) :: match
CHARACTER (LEN = *),  OPTIONAL, INTENT (IN) :: bound

! Type defined arguments with intent (in):
TYPE (Table), INTENT (IN) :: tab

! Scalar arguments with intent(in):
REAL (KIND = float), INTENT (OUT) :: valueOut

! Local scalars:
TYPE (Column), POINTER :: colIn
TYPE (Column), POINTER :: colOut
INTEGER (KIND = short) :: i
CHARACTER (LEN = 100)  :: string
LOGICAL                :: foundValue
REAL (KIND = float)    :: upperIn
REAL (KIND = float)    :: lowerIn
REAL (KIND = float)    :: upperOut
REAL (KIND = float)    :: lowerOut
REAL (KIND = float)    :: bias

!------------end of declaration------------------------------------------------
!inizialization
foundValue = .FALSE.
!find columns to be processed
DO i = 1, tab % noCols
  string = StringCompact (StringToUpper (tab % col (i) % header) ) 
  IF ( string == StringToUpper(keyIn) ) THEN
    colIn => tab % col (i) !colIn is an alias of the input column
  ELSE IF ( string == StringToUpper(keyOut) ) THEN  
    colOut => tab % col (i) !colOut is an alias of the output column
  END IF 
END DO

SELECT CASE ( StringToUpper (match) )
  CASE ('EXACT')
    !bound method is not necessary, only fixed makes sense.
    DO i = 1, tab % noRows
      IF ( StringToFloat (colIn % row (i)) == valueIn ) THEN
        foundValue = .TRUE.
        valueOut = StringToFloat (colout % row (i))
      END IF
    END DO
    IF ( .NOT. foundValue ) THEN
      CALL Catch ('error', 'TableLib',   &
           TRIM ( ToString (valueIn) ) // ' not found in table: ' ,  &
		    argument = tab % id )
    END IF
  CASE ('LINEAR')
    !if bound is not specified, assume FIXED 
    IF (.NOT. PRESENT (bound) ) THEN
      IF ( StringToFloat (colIn % row (1)) > valueIn .OR. &
           StringToFloat (colIn % row (tab % noRows) ) < valueIn ) THEN
        CALL Catch ('error', 'TableLib', 'bounds exceeded in table: ',  &
			         argument = TRIM(tab % Id) )
      END IF
      !search for upper and lower value to interpolate between
      DO i = 1, tab % noRows
        IF ( StringToFloat (colIn % row (i)) <= valueIn .AND. &
             StringToFloat (colIn % row (i+1)) >= valueIn      ) THEN 
          lowerIn  = StringToFloat ( colIn % row (i) )
          upperIn  = StringToFloat ( colIn % row (i+1) )
          lowerOut = StringToFloat ( colOut % row (i) )
          upperOut = StringToFloat ( colOut % row (i+1) )
          EXIT  
        END IF
      END DO
      valueOut = LinearInterp ( lowerIn, upperIn, lowerOut, upperOut, valueIn ) 
    ELSE
      SELECT CASE ( StringToUpper (bound) )
        CASE ('FIXED')
          IF ( StringToFloat (colIn % row (1)) > valueIn .OR. &
               StringToFloat (colIn % row (tab % noRows) ) < valueIn ) THEN
            CALL Catch ('error', 'TableLib', 'bounds exceeded in table: ',  &
			             argument = TRIM(tab % Id) )
          END IF
          !search for upper and lower value to interpolate between
          DO i = 1, tab % noRows
            IF ( StringToFloat (colIn % row (i)) <= valueIn .AND. &
                 StringToFloat (colIn % row (i+1)) >= valueIn      ) THEN 
               lowerIn  = StringToFloat ( colIn % row (i) )
               upperIn  = StringToFloat ( colIn % row (i+1) )
               lowerOut = StringToFloat ( colOut % row (i) )
               upperOut = StringToFloat ( colOut % row (i+1) )
               EXIT  
            END IF
          END DO
          valueOut = LinearInterp ( lowerIn, upperIn, lowerOut, upperOut, valueIn )  
        CASE ('EXTENDLINEAR')
          !If value exceed lower bound
          IF ( StringToFloat (colIn % row (1)) >= valueIn ) THEN 
            lowerIn  = StringToFloat ( colIn  % row (1) )
            upperIn  = StringToFloat ( colIn  % row (2) )
            lowerOut = StringToFloat ( colOut % row (1) )
            upperOut = StringToFloat ( colOut % row (2) )
            !calculate interpolation
            valueOut = LinearInterp ( lowerIn, upperIn, lowerOut, upperOut, valueIn ) 
            CALL Catch ('warning', 'TableLib',   &
                        'value is under lower bound: extending linearly')
          !if value exceed upper bound  
          ELSE IF ( StringToFloat (colIn % row (tab % noRows)) <= valueIn ) THEN
            lowerIn  = StringToFloat ( colIn  % row ( tab % noRows - 1) )
            upperIn  = StringToFloat ( colIn  % row ( tab % noRows    ) )
            lowerOut = StringToFloat ( colOut % row ( tab % noRows - 1) )
            upperOut = StringToFloat ( colOut % row ( tab % noRows    ) )
            !calculate interpolation
            valueOut = LinearInterp ( lowerIn, upperIn, lowerOut, upperOut, valueIn )
            CALL Catch ('warning', 'TableLib',   &
                        'value is over upper bound: extending linearly')
          ELSE !value is between the boundary of the table 
            !search for upper and lower value to interpolate between
            DO i = 1, tab % noRows
              IF ( StringToFloat (colIn % row (i)) <= valueIn .AND. &
                   StringToFloat (colIn % row (i+1)) >= valueIn      ) THEN 
                lowerIn  = StringToFloat ( colIn % row (i) )
                upperIn  = StringToFloat ( colIn % row (i+1) )
                lowerOut = StringToFloat ( colOut % row (i) )
                upperOut = StringToFloat ( colOut % row (i+1) )
                EXIT  
              END IF
            END DO
            valueOut = LinearInterp ( lowerIn, upperIn, lowerOut, upperOut, valueIn )  
          END IF        
          
        CASE ('EXTENDCONSTANT')    
          !If value exceed lower bound
          IF ( StringToFloat (colIn % row (1)) >= valueIn ) THEN 
            valueOut = StringToFloat (colOut % row (1)) 
            CALL Catch ('warning', 'TableLib',   &
                        'value is under lower bound: extending constantly')
          !if value exceed upper bound  
          ELSE IF ( StringToFloat (colIn % row (tab % noRows)) <= valueIn ) THEN
            valueOut = StringToFloat (colOut % row (tab % noRows))
            CALL Catch ('warning', 'TableLib',   &
                        'value is over upper bound: extending constantly')
          ELSE !value is between the boundary of the table 
            !search for upper and lower value to interpolate between
            DO i = 1, tab % noRows
              IF ( StringToFloat (colIn % row (i)) <= valueIn .AND. &
                   StringToFloat (colIn % row (i+1)) >= valueIn      ) THEN 
                lowerIn  = StringToFloat ( colIn % row (i) )
                upperIn  = StringToFloat ( colIn % row (i+1) )
                lowerOut = StringToFloat ( colOut % row (i) )
                upperOut = StringToFloat ( colOut % row (i+1) )
                EXIT  
              END IF
            END DO
            valueOut = LinearInterp ( lowerIn, upperIn, lowerOut, upperOut, valueIn )  
          END IF       
        CASE DEFAULT
          CALL Catch ('error', 'TableLib',   &
                      'unknown option in call to TableGetValue: ' ,  &
			          code = unknownOption, argument = TRIM(bound) )
      END SELECT
    END IF
  CASE ('NEAREST')
    !bound method is not necessary, only fixed makes sense.
    bias = HUGE (bias) !initializa bias to biggest number
    DO i = 1, tab % noRows
      IF ( ABS ( StringToFloat (colIn % row (i)) - valueIn ) < bias ) THEN 
        bias = ABS ( StringToFloat (colIn % row (i)) - valueIn )
        valueOut = StringToFloat (colOut % row (i))      
      END IF
    END DO
  CASE DEFAULT
    CALL Catch ('error', 'TableLib',   &
           'unknown option in call to TableGetValue: ' ,  &
			code = unknownOption, argument = TRIM(match) )
END SELECT


END SUBROUTINE TableGetFloat

!==============================================================================
!! Description:
!! returns a float from column defined by keyOut corresponding to valueIn 
!! contained in column defined by keyIn. Table is identified by its id.
!!>Arguments:
!!   valueIn  input value
!!   tables   collection of tables to search in
!!   id       id of the table to search in
!!   keyIn    defines header of the column of the input value
!!   keyOut   defines header of the column of the output value
!!   match    method to match input value. Possible values are:
!!            'exact'  = column must contain exact input value
!!            'linear' = calculates linear interpolation between two 
!!                       bounding values
!!            'nearest'  = search for the nearest value in input column
!!   bound    method to manage bounds. Possible values are:
!!            'fixed'  = extreme values are treated as a wall
!!            'extendlinear' = extend bounds with linear interpolation 
!!                             of last two extreme values 
!!<           'extendconstant'  = extend bounds preserving extreme value constant
SUBROUTINE TablesGetFloat &
!
( valueIn, tables, id, keyIn, keyOut, match, valueOut, bound )

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
REAL (KIND = float),  INTENT (IN) :: valueIn
CHARACTER (LEN = *),  INTENT (IN) :: id
CHARACTER (LEN = *),  INTENT (IN) :: keyIn
CHARACTER (LEN = *),  INTENT (IN) :: keyOut
CHARACTER (LEN = *),  INTENT (IN) :: match
CHARACTER (LEN = *),  OPTIONAL, INTENT (IN) :: bound

! Type defined arguments with intent (in):
TYPE (TableCollection), INTENT (IN) :: tables

! Scalar arguments with intent(in):
REAL (KIND = float), INTENT (OUT) :: valueOut

!------------end of declaration------------------------------------------------


IF ( PRESENT (bound) ) THEN
  CALL TableGetFloat ( valueIn, tables % elem ( TableSyncById (tables, id) ), &
                       keyIn, keyOut, match, valueOut, bound )
ELSE
  CALL TableGetFloat ( valueIn, tables % elem ( TableSyncById (tables, id) ), &
                       keyIn, keyOut, match, valueOut)
END IF

END SUBROUTINE TablesGetFloat




!==============================================================================
!! Description:
!! returns a double from column defined by keyOut corresponding to valueIn 
!! contained in column defined by keyIn. 
!!>Arguments:
!!   valueIn  input value
!!   tab      table to search in
!!   keyIn    defines header of the column of the input value
!!   keyOut   defines header of the column of the output value
!!   match    method to match input value. Possible values are:
!!            'exact'  = column must contain exact input value
!!            'linear' = calculates linear interpolation between two 
!!                       bounding values
!!            'nearest'  = search for the nearest value in input column
!!   bound    method to manage bounds. Possible values are:
!!            'fixed'  = extreme values are treated as a wall
!!            'extendlinear' = extend bounds with linear interpolation 
!!                             of last two extreme values 
!!<           'extendconstant'  = extend bounds preserving extreme value constant
SUBROUTINE TableGetDouble &
!
( valueIn, tab, keyIn, keyOut, match, valueOut, bound )

! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringCompact, StringToUpper, StringToFloat, ToString, &
StringToDouble

USE LogLib, ONLY : &
! Imported Routines:
Catch

USE ErrorCodes, ONLY : &
! Imported parameters:
unknownOption


IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
REAL (KIND = float),  INTENT (IN) :: valueIn
CHARACTER (LEN = *),  INTENT (IN) :: keyIn
CHARACTER (LEN = *),  INTENT (IN) :: keyOut
CHARACTER (LEN = *),  INTENT (IN) :: match
CHARACTER (LEN = *),  OPTIONAL, INTENT (IN) :: bound

! Type defined arguments with intent (in):
TYPE (Table), INTENT (IN) :: tab

! Scalar arguments with intent(in):
REAL (KIND = double), INTENT (OUT) :: valueOut

! Local scalars:
TYPE (Column), POINTER :: colIn
TYPE (Column), POINTER :: colOut
INTEGER (KIND = short) :: i
CHARACTER (LEN = 100)  :: string
LOGICAL                :: foundValue
REAL (KIND = float)    :: upperIn
REAL (KIND = float)    :: lowerIn
REAL (KIND = double)   :: upperOut
REAL (KIND = double)   :: lowerOut
REAL (KIND = float)    :: bias

!------------end of declaration------------------------------------------------
!inizialization
foundValue = .FALSE.
!find columns to be processed
DO i = 1, tab % noCols
  string = StringCompact (StringToUpper (tab % col (i) % header) ) 
  IF ( string == StringToUpper(keyIn) ) THEN
    colIn => tab % col (i) !colIn is an alias of the input column
  ELSE IF ( string == StringToUpper(keyOut) ) THEN  
    colOut => tab % col (i) !colOut is an alias of the output column
  END IF 
END DO

SELECT CASE ( StringToUpper (match) )
  CASE ('EXACT')
    !bound method is not necessary, only fixed makes sense.
    DO i = 1, tab % noRows
      IF ( StringToFloat (colIn % row (i)) == valueIn ) THEN
        foundValue = .TRUE.
        valueOut = StringToDouble (colout % row (i))
      END IF
    END DO
    IF ( .NOT. foundValue ) THEN
      CALL Catch ('error', 'TableLib',   &
           TRIM ( ToString (valueIn) ) // ' not found in table: ' ,  &
		    argument = tab % id )
    END IF
  CASE ('LINEAR')
    !if bound is not specified, assume FIXED 
    IF (.NOT. PRESENT (bound) ) THEN
      IF ( StringToFloat (colIn % row (1)) > valueIn .OR. &
           StringToFloat (colIn % row (tab % noRows) ) < valueIn ) THEN
        CALL Catch ('error', 'TableLib', 'bounds exceeded in table: ',  &
			         argument = TRIM(tab % Id) )
      END IF
      !search for upper and lower value to interpolate between
      DO i = 1, tab % noRows
        IF ( StringToFloat (colIn % row (i)) <= valueIn .AND. &
             StringToFloat (colIn % row (i+1)) >= valueIn      ) THEN 
          lowerIn  = StringToFloat ( colIn % row (i) )
          upperIn  = StringToFloat ( colIn % row (i+1) )
          lowerOut = StringToDouble ( colOut % row (i) )
          upperOut = StringToDouble ( colOut % row (i+1) )
          EXIT  
        END IF
      END DO
      valueOut = LinearInterp ( lowerIn, upperIn, lowerOut, upperOut, valueIn ) 
    ELSE
      SELECT CASE ( StringToUpper (bound) )
        CASE ('FIXED')
          IF ( StringToFloat (colIn % row (1)) > valueIn .OR. &
               StringToFloat (colIn % row (tab % noRows) ) < valueIn ) THEN
            CALL Catch ('error', 'TableLib', 'bounds exceeded in table: ',  &
			             argument = TRIM(tab % Id) )
          END IF
          !search for upper and lower value to interpolate between
          DO i = 1, tab % noRows
            IF ( StringToFloat (colIn % row (i)) <= valueIn .AND. &
                 StringToFloat (colIn % row (i+1)) >= valueIn      ) THEN 
               lowerIn  = StringToFloat ( colIn % row (i) )
               upperIn  = StringToFloat ( colIn % row (i+1) )
               lowerOut = StringToDouble ( colOut % row (i) )
               upperOut = StringToDouble ( colOut % row (i+1) )
               EXIT  
            END IF
          END DO
          valueOut = LinearInterp ( lowerIn, upperIn, lowerOut, upperOut, valueIn )  
        CASE ('EXTENDLINEAR')
          !If value exceed lower bound
          IF ( StringToFloat (colIn % row (1)) >= valueIn ) THEN 
            lowerIn  = StringToFloat ( colIn  % row (1) )
            upperIn  = StringToFloat ( colIn  % row (2) )
            lowerOut = StringToDouble ( colOut % row (1) )
            upperOut = StringToDouble ( colOut % row (2) )
            !calculate interpolation
            valueOut = LinearInterp ( lowerIn, upperIn, lowerOut, upperOut, valueIn ) 
            CALL Catch ('warning', 'TableLib',   &
                        'value is under lower bound: extending linearly')
          !if value exceed upper bound  
          ELSE IF ( StringToFloat (colIn % row (tab % noRows)) <= valueIn ) THEN
            lowerIn  = StringToFloat ( colIn  % row ( tab % noRows - 1) )
            upperIn  = StringToFloat ( colIn  % row ( tab % noRows    ) )
            lowerOut = StringToDouble ( colOut % row ( tab % noRows - 1) )
            upperOut = StringToDouble ( colOut % row ( tab % noRows    ) )
            !calculate interpolation
            valueOut = LinearInterp ( lowerIn, upperIn, lowerOut, upperOut, valueIn )
            CALL Catch ('warning', 'TableLib',   &
                        'value is over upper bound: extending linearly')
          ELSE !value is between the boundary of the table 
            !search for upper and lower value to interpolate between
            DO i = 1, tab % noRows
              IF ( StringToFloat (colIn % row (i)) <= valueIn .AND. &
                   StringToFloat (colIn % row (i+1)) >= valueIn      ) THEN 
                lowerIn  = StringToFloat ( colIn % row (i) )
                upperIn  = StringToFloat ( colIn % row (i+1) )
                lowerOut = StringToDouble ( colOut % row (i) )
                upperOut = StringToDouble ( colOut % row (i+1) )
                EXIT  
              END IF
            END DO
            valueOut = LinearInterp ( lowerIn, upperIn, lowerOut, upperOut, valueIn )  
          END IF        
          
        CASE ('EXTENDCONSTANT')    
          !If value exceed lower bound
          IF ( StringToFloat (colIn % row (1)) >= valueIn ) THEN 
            valueOut = StringToDouble (colOut % row (1)) 
            CALL Catch ('warning', 'TableLib',   &
                        'value is under lower bound: extending constantly')
          !if value exceed upper bound  
          ELSE IF ( StringToFloat (colIn % row (tab % noRows)) <= valueIn ) THEN
            valueOut = StringToFloat (colOut % row (tab % noRows))
            CALL Catch ('warning', 'TableLib',   &
                        'value is over upper bound: extending constantly')
          ELSE !value is between the boundary of the table 
            !search for upper and lower value to interpolate between
            DO i = 1, tab % noRows
              IF ( StringToFloat (colIn % row (i)) <= valueIn .AND. &
                   StringToFloat (colIn % row (i+1)) >= valueIn      ) THEN 
                lowerIn  = StringToFloat ( colIn % row (i) )
                upperIn  = StringToFloat ( colIn % row (i+1) )
                lowerOut = StringToDouble ( colOut % row (i) )
                upperOut = StringToDouble ( colOut % row (i+1) )
                EXIT  
              END IF
            END DO
            valueOut = LinearInterp ( lowerIn, upperIn, lowerOut, upperOut, valueIn )  
          END IF       
        CASE DEFAULT
          CALL Catch ('error', 'TableLib',   &
                      'unknown option in call to TableGetValue: ' ,  &
			          code = unknownOption, argument = TRIM(bound) )
      END SELECT
    END IF
  CASE ('NEAREST')
    !bound method is not necessary, only fixed makes sense.
    bias = HUGE (bias) !initializa bias to biggest number
    DO i = 1, tab % noRows
      IF ( ABS ( StringToFloat (colIn % row (i)) - valueIn ) < bias ) THEN 
        bias = ABS ( StringToFloat (colIn % row (i)) - valueIn )
        valueOut = StringToDouble (colOut % row (i))      
      END IF
    END DO
  CASE DEFAULT
    CALL Catch ('error', 'TableLib',   &
           'unknown option in call to TableGetValue: ' ,  &
			code = unknownOption, argument = TRIM(match) )
END SELECT


END SUBROUTINE TableGetDouble



!==============================================================================
!! Description:
!! returns a double from column defined by keyOut corresponding to valueIn 
!! contained in column defined by keyIn. Table is identified by its id.
!!>Arguments:
!!   valueIn  input value
!!   tables   collection of tables to search in
!!   id       id of the table to search in
!!   keyIn    defines header of the column of the input value
!!   keyOut   defines header of the column of the output value
!!   match    method to match input value. Possible values are:
!!            'exact'  = column must contain exact input value
!!            'linear' = calculates linear interpolation between two 
!!                       bounding values
!!            'nearest'  = search for the nearest value in input column
!!   bound    method to manage bounds. Possible values are:
!!            'fixed'  = extreme values are treated as a wall
!!            'extendlinear' = extend bounds with linear interpolation 
!!                             of last two extreme values 
!!<           'extendconstant'  = extend bounds preserving extreme value constant
SUBROUTINE TablesGetDouble &
!
( valueIn, tables, id, keyIn, keyOut, match, valueOut, bound )

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
REAL (KIND = float),  INTENT (IN) :: valueIn
CHARACTER (LEN = *),  INTENT (IN) :: id
CHARACTER (LEN = *),  INTENT (IN) :: keyIn
CHARACTER (LEN = *),  INTENT (IN) :: keyOut
CHARACTER (LEN = *),  INTENT (IN) :: match
CHARACTER (LEN = *),  OPTIONAL, INTENT (IN) :: bound

! Type defined arguments with intent (in):
TYPE (TableCollection), INTENT (IN) :: tables

! Scalar arguments with intent(in):
REAL (KIND = double), INTENT (OUT) :: valueOut

!------------end of declaration------------------------------------------------


IF ( PRESENT (bound) ) THEN
  CALL TableGetDouble ( valueIn, tables % elem ( TableSyncById (tables, id) ), &
                       keyIn, keyOut, match, valueOut, bound )
ELSE
  CALL TableGetDouble ( valueIn, tables % elem ( TableSyncById (tables, id) ), &
                       keyIn, keyOut, match, valueOut)
END IF

END SUBROUTINE TablesGetDouble



!==============================================================================
!! Description:
!! returns a string from column defined by keyOut corresponding to valueIn 
!! contained in column defined by keyIn. 
!!>Arguments:
!!   valueIn  input value
!!   tab      table to search in
!!   keyIn    defines header of the column of the input value
!!<  keyOut   defines header of the column of the output value
SUBROUTINE TableGetString &
!
( valueIn, tab, keyIn, keyOut, valueOut )

! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringCompact, StringToUpper, StringToFloat, ToString

USE LogLib, ONLY : &
! Imported Routines:
Catch


IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
REAL (KIND = float),  INTENT (IN) :: valueIn
CHARACTER (LEN = *),  INTENT (IN) :: keyIn
CHARACTER (LEN = *),  INTENT (IN) :: keyOut

! Type defined arguments with intent (in):
TYPE (Table), INTENT (IN) :: tab

! Scalar arguments with intent(in):
CHARACTER (LEN = *), INTENT (OUT) :: valueOut

! Local scalars:
TYPE (Column), POINTER :: colIn
TYPE (Column), POINTER :: colOut
INTEGER (KIND = short) :: i
CHARACTER (LEN = 100)  :: string
LOGICAL                :: foundValue
!------------end of declaration------------------------------------------------
!inizialization
foundValue = .FALSE.
!find columns to be processed
DO i = 1, tab % noCols
  string = StringCompact (StringToUpper (tab % col (i) % header) ) 
  IF ( string == StringToUpper(keyIn) ) THEN
    colIn => tab % col (i) !colIn is an alias of the input column
  ELSE IF ( string == StringToUpper(keyOut) ) THEN  
    colOut => tab % col (i) !colOut is an alias of the output column
  END IF 
END DO


DO i = 1, tab % noRows
  IF ( StringToFloat (colIn % row (i)) == valueIn ) THEN
    foundValue = .TRUE.
    valueOut = colout % row (i)
  END IF
END DO
IF ( .NOT. foundValue ) THEN
  CALL Catch ('error', 'TableLib',   &
       TRIM ( ToString (valueIn) ) // ' not found in table: ' ,  &
	    argument = tab % id )
END IF

RETURN  

END SUBROUTINE TableGetString


!==============================================================================
!! Description:
!! returns a string from column defined by keyOut corresponding to valueIn 
!! contained in column defined by keyIn. Table is identified by its id.
!!>Arguments:
!!   valueIn  input value
!!   tables   collection of tables to search in
!!   id       id of the table to search in
!!   keyIn    defines header of the column of the input value
!!<  keyOut   defines header of the column of the output value
SUBROUTINE TablesGetString &
!
( valueIn, tables, id, keyIn, keyOut, valueOut)

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
REAL (KIND = float),  INTENT (IN) :: valueIn
CHARACTER (LEN = *),  INTENT (IN) :: id
CHARACTER (LEN = *),  INTENT (IN) :: keyIn
CHARACTER (LEN = *),  INTENT (IN) :: keyOut

! Type defined arguments with intent (in):
TYPE (TableCollection), INTENT (IN) :: tables

! Scalar arguments with intent(in):
CHARACTER (LEN = *), INTENT (OUT) :: valueOut

!------------end of declaration------------------------------------------------



 CALL TableGetString ( valueIn, tables % elem ( TableSyncById (tables, id) ), &
                       keyIn, keyOut, valueOut)


END SUBROUTINE TablesGetString


!==============================================================================
!! Description:
!! returns the position of table in collection of tables identified by id. 
!!>Arguments:
!!   tables   collection of tables to search in
!!<  id       id of the table
FUNCTION TableSyncById &
( tables, id ) &
RESULT (pos)

! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringToUpper

USE LogLib, ONLY : &
! Imported Routines:
Catch

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER (LEN = *),  INTENT (IN) :: id

! Type defined arguments with intent (in):
TYPE (TableCollection), INTENT (IN) :: tables

! Scalar arguments with intent(OUT):
INTEGER (KIND = long) :: pos

!Local variables:
LOGICAL :: foundTable
INTEGER (KIND = long) :: i

!------------end of declaration------------------------------------------------

foundTable = .FALSE.

DO i = 1, tables % number
  IF ( StringToUpper (tables % elem (i) % id) == StringToUpper (id) ) THEN
    foundTable = .TRUE.
    pos = i
    EXIT
  END IF
END DO

IF ( .NOT. foundTable ) THEN
  CALL Catch ('error', 'TableLib',   &
               'table not found in collection of tables: ' ,  &
		        argument = id )
END IF

END FUNCTION TableSyncById



!==============================================================================
!! Description:
!! search the file for beginning of next table defined by keyword Table Start 
!!>Arguments:
!!     unit     file in which operate search
!!     line     optional, line of file to begin search
!! Result:
!!     Return -1 when table is not found
!!<    line of beginning of a table
FUNCTION TableFileSync &
  ( unit, id, line )       &
RESULT (code)

! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringCompact, StringToUpper

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
INTEGER (KIND = short), INTENT (IN) :: unit
CHARACTER (LEN = *), OPTIONAL, INTENT(IN) :: id

! Scalar arguments with intent (inout):
INTEGER (KIND = long), OPTIONAL, INTENT (INOUT) :: line

! Local scalars:
INTEGER (KIND = short) :: code 
INTEGER (KIND = short) :: ios
INTEGER (KIND = short) :: i
CHARACTER (LEN = 300)  :: string
INTEGER (KIND = long)  :: iLine
CHARACTER (LEN = 300)  :: idLocal
!------------end of declaration------------------------------------------------

code = -1

!REWIND (unit)

IF ( PRESENT (line) ) THEN !Sync file to specified line
  REWIND (unit)
  DO i =1, line
    READ(unit,*)
  END DO
  iLine = line
END IF

DO WHILE (ios >= 0)
  READ (unit, "(a)",IOSTAT = ios) string
    iLine = iLine + 1
  IF (PRESENT (line) ) THEN
    line = line + 1
  END IF
  IF ( StringCompact (StringToUpper (string) ) == "TABLE START" ) THEN
    IF (PRESENT(id)) THEN
      DO WHILE (StringCompact (StringToUpper (string) ) /= "TABLE END" )
        READ (unit, "(a)",IOSTAT = ios) string
        IF ( StringCompact (StringToUpper (string(1:3)) ) == "ID:" ) THEN
          idLocal = StringCompact (StringToUpper (string(4:LEN_TRIM(string))))
          IF (idLocal == StringToUpper (id)) THEN
             REWIND (unit)
             DO i =1, iLine
               READ(unit,*)
            END DO
            code = 1
            RETURN
          END IF
          EXIT
        END IF
      END DO
    ELSE
      code = 1
      RETURN
    END IF
  END IF
END DO

END FUNCTION TableFileSync

!==============================================================================
!! Description:
!! Read the title of the table. Title is optional. 
!!>Arguments:
!!     lines  collections of lines
!! Result:
!!<    Return title if exists
FUNCTION TableReadTitle &
  ( lines )               &
RESULT (title)

! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringCompact, StringToUpper, StringSplit

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER (LEN = LINELENGTH), INTENT (IN), POINTER :: lines (:)

! Local scalars:
CHARACTER (LEN = 300)  :: title 
INTEGER (KIND = short) :: ios
INTEGER (KIND = short) :: i
CHARACTER (LEN = LINELENGTH)  :: string
CHARACTER (LEN = 300)  :: before
LOGICAL                :: titleFound            
!------------end of declaration------------------------------------------------

string = ''
titleFound = .FALSE.

! scan table 
DO i = 1, SIZE (lines)
  string =  lines (i)
  CALL StringSplit ( ':', string, before)
  
  IF (  StringToUpper ( before(1:5)) == "TITLE" ) THEN !found title
    CALL StringSplit ( '#', string, before) !remove inline comment
    title = before
    titleFound = .TRUE.
    RETURN
  END IF
END DO

IF ( .NOT. titleFound ) THEN
  title = ''
  !Title is not mandatory element of a table.
END IF
END FUNCTION TableReadTitle


!==============================================================================
!! Description:
!! Read the Id of the table. Id is mandatory and must be unique. 
!!>Arguments:
!!     lines  collections of lines
!! Result:
!!<    Return Id
FUNCTION TableReadId &
  ( lines )            &
RESULT (id)

! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringCompact, StringToUpper, StringSplit

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER (LEN = LINELENGTH), INTENT (IN), POINTER :: lines (:)

! Local scalars:
CHARACTER (LEN = 300)  :: id 
INTEGER (KIND = short) :: ios
INTEGER (KIND = short) :: i
CHARACTER (LEN = LINELENGTH)  :: string
CHARACTER (LEN = 300)  :: before
LOGICAL                :: idFound            
!------------end of declaration------------------------------------------------

string = ''
idFound = .FALSE.

! scan table 
DO i = 1, SIZE (lines)
  string =  lines (i)
  CALL StringSplit ( ':', string, before)
  
  IF (  StringToUpper ( before(1:2)) == "ID" ) THEN !found id
    CALL StringSplit ( '#', string, before) !remove inline comments
    id = before
    idFound = .TRUE.
    RETURN
  END IF
END DO

IF ( .NOT. idFound ) THEN !Id is mandatory in a table
  CALL Catch ('error', 'TableLib', 'Table Id not found')
END IF
END FUNCTION TableReadId


!==============================================================================
!! Description:
!! read a collection of tables from specified file. 
!!>Arguments:
!!     file     file in which table is contained
!!<    tables      returned collection of tables
SUBROUTINE TablesGetFromFile &
  ( file, tables )
  
USE Utilities, ONLY: &
!Imported routines:
GetUnit

! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringCompact, StringToUpper

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER (LEN = *), INTENT (IN) :: file

! Array arguments with intent (out):
TYPE (TableCollection), INTENT (OUT) :: tables
! Local scalars:
INTEGER (KIND = short) :: iunit
INTEGER (KIND = short) :: ios
INTEGER (KIND = long)  :: count
INTEGER (KIND = long)  :: i, j
CHARACTER (LEN = 300)  :: string

! Local Arrays:
CHARACTER (LEN = LINELENGTH), POINTER :: lines (:)

!------------end of declaration------------------------------------------------

!get a free fortran unit
iunit = GetUnit ()
OPEN (UNIT = iunit, FILE = file, STATUS = "old")

!count the number of tables present in the file
ios = 0
count = 0
DO WHILE (ios >= 0)
  READ (iunit, "(a)",IOSTAT = ios) string
  IF ( StringCompact (StringToUpper (string) ) == "TABLE START" ) THEN
    count = count + 1
  END IF
END DO
!allocate space for tables
ALLOCATE ( tables % elem (count) )

tables % number = count

!initialize tables from file
REWIND (iunit)
i = 0
DO i = 1, count
!search beginning of next table
ios = TableFileSync (iunit)
!Store significant lines in memory
CALL TableStoreLines ( iunit, lines )
!Get title
tables % elem (i) % title = TableReadTitle (lines)
!get Id
tables % elem (i) % id = TableReadId (lines)
!check that id is not replicated
CALL CheckId(tables,i)
!count number of columns
tables % elem (i) % noCols = TableCountCols (lines)
IF ( tables % elem (i) % noCols == 0) THEN
  CALL Catch ('error', 'TableLib', 'no columns found in table: ', &
               argument = tables % elem (i) % id)
END IF
!allocate columns
ALLOCATE ( tables % elem (i) % col ( tables % elem (i) % noCols ) )
!count number of rows
tables % elem (i) % noRows = TableCountRows (lines)
!allocate rows
DO j = 1, tables % elem (i) % noCols
  ALLOCATE ( tables % elem (i) % col (j) % row ( tables % elem (i) % noRows ) )
END DO
!read header unit and content of the tables.
CALL TableReadHeader ( lines, tables % elem (i) )

CALL TableReadUnit ( lines, tables % elem (i) )

CALL TableReadContent ( lines, tables % elem (i) )

! table is in memory: deallocate lines
DEALLOCATE (lines)

END DO

!tables are initialized: close file
CLOSE (iunit)

END SUBROUTINE TablesGetFromFile

!==============================================================================
!! Description:
!! read a collection of tables from specified unit. File is already open.
!!>Arguments:
!!     unit        unit of file in which table is contained
!!<    tables      returned collection of tables
SUBROUTINE TablesGetFromUnit &
  ( unit, tables )
  
USE Utilities, ONLY: &
!Imported routines:
GetUnit

! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringCompact, StringToUpper

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
INTEGER (KIND = short), INTENT (IN) :: unit

! Array arguments with intent (out):
TYPE (TableCollection), INTENT (OUT) :: tables
! Local scalars:
INTEGER (KIND = short) :: ios
INTEGER (KIND = long)  :: count
INTEGER (KIND = long)  :: i, j
CHARACTER (LEN = 300)  :: string

! Local Arrays:
CHARACTER (LEN = LINELENGTH), POINTER :: lines (:)

!------------end of declaration------------------------------------------------

!count the number of tables present in the file
ios = 0
count = 0
DO WHILE (ios >= 0)
  READ (unit, "(a)",IOSTAT = ios) string
  IF ( StringCompact (StringToUpper (string) ) == "TABLE START" ) THEN
    count = count + 1
  END IF
END DO
!allocate space for tables
ALLOCATE ( tables % elem (count) )

tables % number = count

!initialize tables from file
REWIND (unit)
i = 0
DO i = 1, count
!search beginning of next table
ios = TableFileSync (unit)
!Store significant lines in memory
CALL TableStoreLines ( unit, lines )
!Get title
tables % elem (i) % title = TableReadTitle (lines)
!get Id
tables % elem (i) % id = TableReadId (lines)
!check that id is not replicated
CALL CheckId(tables,i)
!count number of columns
tables % elem (i) % noCols = TableCountCols (lines)
IF ( tables % elem (i) % noCols == 0) THEN
  CALL Catch ('error', 'TableLib', 'no columns found in table: ', &
               argument = tables % elem (i) % id)
END IF
!allocate columns
ALLOCATE ( tables % elem (i) % col ( tables % elem (i) % noCols ) )
!count number of rows
tables % elem (i) % noRows = TableCountRows (lines)
!allocate rows
DO j = 1, tables % elem (i) % noCols
  ALLOCATE ( tables % elem (i) % col (j) % row ( tables % elem (i) % noRows ) )
END DO
!read header unit and content of the tables.
CALL TableReadHeader ( lines, tables % elem (i) )

CALL TableReadUnit ( lines, tables % elem (i) )

CALL TableReadContent ( lines, tables % elem (i) )

! table is in memory: deallocate lines
DEALLOCATE (lines)

END DO

END SUBROUTINE TablesGetFromUnit


!==============================================================================
!! Description:
!! read a table from specified file. File is not yet open.
!! If id is not specified, in a file containing multiple tables,
!! the first table is read
!!>Arguments:
!!     file     file in which table is contained
!!     tab      returned table
!!<    id       optional, id of table to read
SUBROUTINE TableGetFromFile &
  ( file, tab, id )
  
USE Utilities, ONLY: &
!Imported routines:
GetUnit

! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringCompact, StringToUpper

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER (LEN = *), INTENT (IN) :: file
CHARACTER (LEN = *), OPTIONAL, INTENT (IN) :: id

! Array arguments with intent (out):
TYPE (Table), INTENT (OUT) :: tab
! Local scalars:
INTEGER (KIND = short) :: iunit
INTEGER (KIND = short) :: ios
INTEGER (KIND = long)  :: count
INTEGER (KIND = long)  :: j
CHARACTER (LEN = 300)  :: string

! Local Arrays:
CHARACTER (LEN = LINELENGTH), POINTER :: lines (:)

!------------end of declaration------------------------------------------------

!get a free fortran unit
iunit = GetUnit ()
OPEN (UNIT = iunit, FILE = file, STATUS = "old")

!search beginning of table
IF (PRESENT(id)) THEN
  ios = TableFileSync (iunit, id = id)
ELSE
  ios = TableFileSync (iunit)
END IF

!check if table was found
IF (ios == -1) THEN
  CALL Catch ('warning', 'TableLib', 'Table not found in file: ', &
               argument = file)
  RETURN
  CLOSE (iunit)
END IF


!Store significant lines in memory
CALL TableStoreLines ( iunit, lines )
!Get title
tab % title = TableReadTitle (lines)
!get Id
tab % id = TableReadId (lines)
!count number of columns
tab % noCols = TableCountCols (lines)
IF ( tab % noCols == 0) THEN
  CALL Catch ('error', 'TableLib', 'no columns found in table: ', &
               argument = tab % id)
END IF
!allocate columns
ALLOCATE ( tab % col ( tab % noCols ) )
!count number of rows
tab % noRows = TableCountRows (lines)
!allocate rows
DO j = 1, tab % noCols
  ALLOCATE ( tab % col (j) % row ( tab % noRows ) )
END DO
!read header unit and content of the tables.
CALL TableReadHeader ( lines, tab )

CALL TableReadUnit ( lines, tab )

CALL TableReadContent ( lines, tab )

!table is initialized: close file and deallocate lines
CLOSE (iunit)
DEALLOCATE (lines)

END SUBROUTINE TableGetFromFile

!==============================================================================
!! Description:
!! read a table from specified file unit. File is already open.
!!>Arguments:
!!     unit     file in which table is contained
!!     tab      returned table
!!<    id       optional, id of table to read
SUBROUTINE TableGetFromUnit &
  ( unit, tab, id )

! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringCompact, StringToUpper

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
INTEGER (KIND = short), INTENT(IN) :: unit
CHARACTER (LEN = *), OPTIONAL, INTENT (IN) :: id

! Array arguments with intent (out):
TYPE (Table), INTENT (OUT) :: tab

! Local scalars:
INTEGER (KIND = short) :: ios
INTEGER (KIND = long)  :: count
INTEGER (KIND = long)  :: j
CHARACTER (LEN = 300)  :: string

! Local Arrays:
CHARACTER (LEN = LINELENGTH), POINTER :: lines (:)

!------------end of declaration------------------------------------------------

!search beginning of table
IF (PRESENT(id)) THEN
  ios = TableFileSync (unit, id = id)
ELSE
  ios = TableFileSync (unit)
END IF

!Store significant lines in memory
CALL TableStoreLines ( unit, lines )
!Get title
tab % title = TableReadTitle (lines)
!get Id
tab % id = TableReadId (lines)
!count number of columns
tab % noCols = TableCountCols (lines)
IF ( tab % noCols == 0) THEN
  CALL Catch ('error', 'TableLib', 'no columns found in table: ', &
               argument = tab % id)
END IF
!allocate columns
ALLOCATE ( tab % col ( tab % noCols ) )
!count number of rows
tab % noRows = TableCountRows (lines)
!allocate rows
DO j = 1, tab % noCols
  ALLOCATE ( tab % col (j) % row ( tab % noRows ) )
END DO
!read header unit and content of the tables.
CALL TableReadHeader ( lines, tab )

CALL TableReadUnit ( lines, tab )

CALL TableReadContent ( lines, tab )

!table is initialized: deallocate lines
DEALLOCATE (lines)

END SUBROUTINE TableGetFromUnit



!==============================================================================
!! Description:
!! read the content of the table. 
!!>Arguments:
!!     lines     collection of strings that contain table information
!!<    tab       table to update
SUBROUTINE TableReadContent &
  ( lines, tab )
  
! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringSplit, StringToUpper

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER (LEN = LINELENGTH), INTENT (IN), POINTER :: lines (:)

! Array arguments with intent (out):
TYPE (Table), INTENT (OUT) :: tab
! Local scalars:
CHARACTER (LEN = LINELENGTH)  :: string
CHARACTER (LEN = LINELENGTH)  :: before
INTEGER (KIND = long) :: i, j, k

! Local Arrays:

!------------end of declaration------------------------------------------------

string = ''
i = 0

! scan table to find lines that have not a keyword.
DO k = 1, SIZE (lines)
  string =  lines (k)
  !CALL StringSplit ( ':', string, before)
  
  IF ( StringToUpper ( string(1:6)) == "TITLE:"        .OR. &
       StringToUpper ( string(1:3)) == "ID:"           .OR. &
       StringToUpper ( string(1:6)) == "UNITS:"        .OR. &
       StringToUpper ( string(1:8)) == "COLUMNS:"      .OR. &
       StringToUpper ( string(1:11)) == "TABLE START" .OR. &
       StringToUpper ( string(1:9)) == "TABLE END") THEN 
    ! this is a line with a keyword not a row of table
  ELSE
    !remove inline comments
    CALL StringSplit ( '#', string, before) !remove inline comments
    string = before
    !increment row
    i = i + 1
    READ(string,*) ( tab % col (j) % row (i), j = 1, tab % noCols )
  END IF
END DO

END SUBROUTINE TableReadContent

!==============================================================================
!! Description:
!! read unit of the columns of the table. 
!!>Arguments:
!!     lines     collection of strings that contain table information
!!<    tab       table to update
SUBROUTINE TableReadUnit &
  ( lines, tab )
  
! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringSplit, StringToUpper

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER (LEN = LINELENGTH), INTENT (IN), POINTER :: lines (:)

! Array arguments with intent (out):
TYPE (Table), INTENT (OUT) :: tab
! Local scalars:
CHARACTER (LEN = LINELENGTH)  :: string
CHARACTER (LEN = LINELENGTH)  :: before
INTEGER (KIND = long) :: i
INTEGER (KIND = long) :: par1, par2

! Local Arrays:

!------------end of declaration------------------------------------------------

string = ''

! scan table to find line denoted by units keyword.
DO i = 1, SIZE (lines)
  string =  lines (i)
  CALL StringSplit ( ':', string, before)
  IF (  StringToUpper ( before(1:5)) == "UNITS" ) THEN !found units
    CALL StringSplit ( '#', string, before) !remove inline comments
    string = before
    EXIT
  END IF
END DO

!search for headers
DO i = 1, tab % noCols
  par1 = INDEX ( string, '[' )
  par2 = INDEX ( string, ']' )
  tab % col (i) % unit = string ( par1+1 : par2-1 )
  !erase part of the string already processed
  string = string ( par2+1 : LEN_TRIM (string) )   
END DO

END SUBROUTINE TableReadUnit



!==============================================================================
!! Description:
!! read header of the columns of the table. 
!!>Arguments:
!!     lines     collection of strings that contain table information
!!<    tab       table to update
SUBROUTINE TableReadHeader &
  ( lines, tab )
  
! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringSplit, StringToUpper

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER (LEN = LINELENGTH), INTENT (IN), POINTER :: lines (:)

! Array arguments with intent (out):
TYPE (Table), INTENT (OUT) :: tab
! Local scalars:
CHARACTER (LEN = LINELENGTH)  :: string
CHARACTER (LEN = LINELENGTH)  :: before
INTEGER (KIND = long) :: i
INTEGER (KIND = long) :: par1, par2

! Local Arrays:

!------------end of declaration------------------------------------------------

string = ''

! scan table to find line denoted to columns keyword.
DO i = 1, SIZE (lines)
  string =  lines (i)
  CALL StringSplit ( ':', string, before)
  IF (  StringToUpper ( before(1:7)) == "COLUMNS" ) THEN !found columns
    CALL StringSplit ( '#', string, before) !remove inline comments
    string = before
    EXIT
  END IF
END DO

!search for headers
DO i = 1, tab % noCols
  par1 = INDEX ( string, '[' )
  par2 = INDEX ( string, ']' )
  tab % col (i) % header = string ( par1+1 : par2-1 )
  !erase part of the string already processed
  string = string ( par2+1 : LEN_TRIM (string) )   
END DO

END SUBROUTINE TableReadHeader


!==============================================================================
!! Description:
!! Count the number of columns in a table stored in a collection of lines.
!!>Method:
!! count the number of tokens included in parentheses []. 
!! Arguments:
!!     lines     collections of lines
!! Result:
!!<    Return number of columns
FUNCTION TableCountCols &
  ( lines )               &
RESULT (cols)

! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringCompact, StringToUpper, StringSplit

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER (LEN = LINELENGTH), INTENT (IN), POINTER :: lines (:)

! Local scalars:
INTEGER (KIND = short) :: cols 
INTEGER (KIND = short) :: i
CHARACTER (LEN = LINELENGTH)  :: string
CHARACTER (LEN = 300)  :: before
CHARACTER (LEN = 1)    :: ch
LOGICAL                :: columnsFound            
LOGICAL                :: parOpen
!------------end of declaration------------------------------------------------

string = ''
columnsFound = .FALSE.
cols = 0

! scan table to find line denoted to columns keyword.
DO i = 1, SIZE (lines)
  string =  lines (i)
  CALL StringSplit ( ':', string, before)
  
  IF (  StringToUpper ( before(1:7)) == "COLUMNS" ) THEN !found columns
    CALL StringSplit ( '#', string, before) !remove inline comments
    string = before
    columnsFound = .TRUE.
    EXIT
  END IF
END DO

IF ( .NOT. columnsFound ) THEN 
  CALL Catch ('error', 'TableLib', 'Keyword columns not found')
END IF

!count the number of couples of parentheses (), [], {} or <>
parOpen = .FALSE.
DO i = 1, LEN_TRIM (string)
  ch = string (i:i)
  IF ( ch == "[" ) THEN
    IF (parOpen ) THEN
      CALL Catch ('error', 'TableLib', 'parentheses in columns was not closed')
    END IF
    parOpen = .TRUE.
    cols = cols + 1
  ELSE IF ( ch == "]") THEN
    IF (.NOT. parOpen ) THEN
      CALL Catch ('error', 'TableLib', 'parentheses in columns was not opened')
    END IF
    parOpen = .FALSE.
  END IF
END DO

!if parentheses are not closed, log an error
IF (parOpen ) THEN
  CALL Catch ('error', 'TableLib', 'parentheses in columns was not closed')
END IF


END FUNCTION TableCountCols


!==============================================================================
!! Description:
!! Count the number of rowss in a table stored in a collection of lines.
!!>Method:
!! count the number of non blank lines that have not a keyword. 
!! Arguments:
!!     lines     collections of lines
!! Result:
!!<    Return number of rows
FUNCTION TableCountRows &
  ( lines )             &
RESULT (rows)

! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringToUpper, StringSplit

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
CHARACTER (LEN = LINELENGTH), INTENT (IN), POINTER :: lines (:)

! Local scalars:
INTEGER (KIND = short) :: rows 
INTEGER (KIND = short) :: i
CHARACTER (LEN = LINELENGTH)  :: string
CHARACTER (LEN = 300)  :: before
!------------end of declaration------------------------------------------------

string = ''
rows = 0

! scan table to count lines that have not a keyword.
DO i = 1, SIZE (lines)
  string =  lines (i)
  CALL StringSplit ( ':', string, before)
  
  IF ( StringToUpper ( before(1:5)) == "TITLE"        .OR. &
       StringToUpper ( before(1:2)) == "ID"           .OR. &
       StringToUpper ( before(1:5)) == "UNITS"        .OR. &
       StringToUpper ( before(1:7)) == "COLUMNS"      .OR. &
       StringToUpper ( before(1:11)) == "TABLE START" .OR. &
       StringToUpper ( before(1:9)) == "TABLE END") THEN 
    ! this is a line with a keyword not a row of table
  ELSE
    rows = rows + 1
  END IF
END DO

END FUNCTION TableCountRows


!==============================================================================
!! Description:
!! Check that table Id is not duplicated. Table id must be unique. 
!!>Arguments:
!!     tables     collections of tables
!!<    pos      position of the last Id entry
SUBROUTINE CheckId    &
  ( tables, pos )

IMPLICIT NONE

! Function arguments
! Scalar arguments with intent(in):
INTEGER (KIND = long), INTENT(IN)  :: pos
! Array derived type with intent(in):
TYPE (TableCollection), INTENT(IN) :: tables

! Local scalars:
INTEGER (KIND = long) :: i
!------------end of declaration------------------------------------------------

!scan all Ids
DO i = 1, pos - 1

  IF ( tables % elem(i) % id == tables % elem(pos) % id ) THEN
    CALL Catch ('error', 'TableLib', 'Duplicate table id: ',  &
                 argument = tables % elem(pos) % id )
  END IF

END DO

END SUBROUTINE CheckId

!==============================================================================
!! Description:
!! read the lines of a table which are stored in an array of strings.
!! Non significative lines (i.e. comments or blank lines) are ignored.
!! Subroutine supposes that the cursor is sync to the first line after 
!! the keyword 'Table Start'. hence it is must benn called after
!! a call to tableFileSync. 
!!>Arguments:
!!     unit     file in which table is contained
!!<    lines    returned collection of linestable
SUBROUTINE TableStoreLines &
  ( unit, lines )

! Module used:
USE StringManipulation, ONLY: &
! imported routines:
StringCompact, StringToUpper, &
StringSplit

IMPLICIT NONE

! Subroutine arguments
! Scalar arguments with intent(in):
INTEGER (KIND = short), INTENT (IN) :: unit

! Array arguments with intent (out):
CHARACTER (LEN = LINELENGTH), INTENT (OUT), POINTER :: lines (:)
! Local scalars:
INTEGER (KIND = short) :: ios
CHARACTER (LEN = LINELENGTH)  :: string
INTEGER (KIND = long)  :: count
INTEGER (KIND = long)  :: i

! Local Type definition:
!define a dynamic list of strings
TYPE LinkedList
    TYPE(LinkedList), POINTER :: next
    CHARACTER (LEN = LINELENGTH) :: line
END TYPE LinkedList

! Local Arrays:
TYPE (LinkedList), POINTER :: list
TYPE (LinkedList), POINTER :: current
TYPE (LinkedList), POINTER :: next
TYPE (LinkedList), POINTER :: previous

!------------end of declaration------------------------------------------------

!initialization    
string = ''
count = 0
NULLIFY (list)
! scan file till end of the table keyword TABLE END
DO WHILE ( .NOT. StringCompact (StringToUpper (string) ) == "TABLE END" )
  READ (unit, "(a)",IOSTAT = ios) string
  IF ( ios > 0 ) THEN !reached the end of file without finding table end
    !CALL Catch
  END IF
  
  string = StringCompact (string)
  IF ( string == '' .OR. string(1:1) == "#" ) THEN !skip element
  ELSE !found new element 
      !increment counter
      count = count + 1
      !add an element to list
      IF(.NOT.ASSOCIATED(list)) THEN
           ALLOCATE(list)     !riconosco il primo elemento da inserire
           current => list
       ELSE
           ALLOCATE(current%next)
           current => current%next
       END IF
       !store line in the list.
       current % line = string
  END IF
END DO

!allocate space for significant lines
ALLOCATE ( lines (count) )
!transfer lines from temporary list to tab
current => list ! current is an alias of list
DO i = 1, count
  lines (i) = current % line
  previous => current
  current => current % next !current is an alias of next element of the list
  DEALLOCATE(previous)    !free memory of the previous element 
END DO

END SUBROUTINE TableStoreLines

!==============================================================================
!! Description:
!!   return the number of rows in a table
FUNCTION TableGetNumberRows &
!
( tab ) &
!
RESULT (rows)

IMPLICIT NONE

!arguments with intent (in):
TYPE (table), INTENT (IN) :: tab

!local dclarations:
INTEGER :: rows
!--------------------------------end of declarations---------------------------

rows = tab % noRows

END FUNCTION TableGetNumberRows


!==============================================================================
!! Description:
!!  return the number of rows of a table in a table collection
FUNCTION TablesGetNumberRows &
!
( tables, id ) &
!
RESULT (rows)

IMPLICIT NONE

!arguments with intent (in):
TYPE (TableCollection), INTENT (IN) :: tables
CHARACTER (LEN = *),  INTENT (IN) :: id

!local dclarations:
INTEGER :: rows
!--------------------------------end of declarations---------------------------

rows = tables % elem ( TableSyncById (tables, id) ) % noRows 

END FUNCTION TablesGetNumberRows



END MODULE TableLib