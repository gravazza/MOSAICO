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
!!   Basic set of routines to handle errors and manage log file.
!!   Three severity of log are defined:
!!   error   (logLevel = 1): report only error messages 
!!   warning (logLevel = 2): report error and warning messages 
!!   info    (logLevel = 3): report error, warning and informative messages 
!!   The default logLevel is 3 (write all) and log messages are written 
!!   on default output unit (screen).If necessary, the user can overried 
!!   default parameters by passing command line options. The user can choose
!!   to write messages also on a file and can change the logLevel.
!!   The module is initialized before the rest of the program takes place
!!   so it is better that this module is kept self containing and not to
!!   use utilities contained in other complicated modules that may want
!!   to use log facilities. 
!! @author <a href="mailto:giovanni.ravazzani@polimi.it">Giovanni Ravazzani</a> 
!! @version 1.1 - 25th September 2008  
MODULE LogLib        
			
! History: 
!  
! Version   Date          Comment 
! -------   ----          ------- 
!  1.0      01/Feb/2007   Original code. giovanni ravazzani
!  1.1      25/Sep/2008   Code revised. giovanni ravazzani
!  1.2      03/Apr/2010   Added possibility on separate log on file and screen
! 
! 
! Modules used: 
! 
USE DataTypeSizes, ONLY : &
! Imported Type Definitions:
short       

USE ErrorCodes, ONLY : & 
! Imported Parameters: 
openFileError

IMPLICIT NONE 
! Global (i.e. public) Declarations: 
         
! Global Scalars: 
LOGICAL :: verbose        = .TRUE.  !! display messages on the screen
                                    !! default value = TRUE
INTEGER (KIND = short) :: logUnit   !! file unit for log reporting

! Global Routines:
PUBLIC :: LogInit
PUBLIC :: LogStop
PUBLIC :: LogMsg
PUBLIC :: Catch

! Local Parameters:
INTEGER (KIND = short), PRIVATE, PARAMETER :: stringLen = 500
INTEGER (KIND = short), PARAMETER :: stdIn              = 5    !!standard input
INTEGER (KIND = short), PARAMETER :: stdOut             = 6    !!standar output
         
! Local Scalars:
CHARACTER (LEN = stringLen), PRIVATE :: logFile !! file for log reporting
CHARACTER (LEN = 1), PARAMETER, PRIVATE :: logsep ="," !!log separator 
LOGICAL                     :: logToScreen = .TRUE.
LOGICAL                     :: logToFile   = .FALSE.
INTEGER (KIND = short), PRIVATE :: logLevel = 3 !!log level; default=3
INTEGER (KIND = short), PRIVATE :: ios 

! Local Routines
PRIVATE :: GetUnit
PRIVATE :: TimeStamp
        
!=======
CONTAINS
!=======
! Define procedures contained in this module. 

!==============================================================================
!! Description:
!!   initialize global parameters
SUBROUTINE LogInit 

IMPLICIT NONE

! Local scalars:
INTEGER (KIND = short)      :: i
CHARACTER (LEN = stringLen) :: arg !! command line arguments
CHARACTER (LEN = stringLen) :: auxString
CHARACTER (LEN = stringLen) :: sep
!------------end of declaration------------------------------------------------

i = 1
DO WHILE ( .not. (arg == '') )

	CALL Getarg ( i, arg )
	SELECT CASE (arg)
		CASE ( '-noverbose' )
			verbose = .FALSE.
			logToScreen = .FALSE.
		CASE ( '-logfile' )
			i = i + 1
			CALL Getarg ( i , logFile ) 
			logUnit = GetUnit() 
			OPEN (UNIT = logUnit, FILE = logFile, position = 'APPEND', &
						ACTION = 'WRITE', IOSTAT = ios)
			IF ( ios /= 0 ) THEN
				!verbose = .TRUE.
				CALL Catch ('error', 'initialization',   &
									'error in opening log file: ' ,  &
									code = OpenFileError, argument = logFile )
			ENDIF
			logToFile = .TRUE.
			CALL TimeStamp (logUnit)
			sep = " "//logsep//" "//"logging start"
			WRITE(UNIT = logUnit, FMT='(a)') TRIM(sep)
		CASE ( '-loglevel' )
			i = i + 1
			CALL Getarg ( i , arg ) 
				SELECT CASE (arg)
					CASE ( 'error' ) !report only error messages
						logLevel = 1
					CASE ( 'warning' ) !report error and warning messages
						logLevel = 2
					CASE ( 'info' ) !report error, warning and info messages
						logLevel = 3
					CASE default
						WRITE(stdOut,*) 'Unknown loglevel option: ', TRIM(arg)
						WRITE(stdOut,*) 'Program terminated'
						STOP ''      
				END SELECT        
		CASE DEFAULT
			!case unknown
	END SELECT
	i = i + 1
  
ENDDO


RETURN

END SUBROUTINE LogInit


!==============================================================================
!! Description:
!!   stop logging. If open, close the log file
SUBROUTINE LogStop ()
IMPLICIT NONE

! Local scalars:
CHARACTER (LEN = stringLen) :: sep
LOGICAL                     :: fileIsOpen


!------------end of declaration------------------------------------------------


IF (logToFile) THEN
	CALL TimeStamp (logUnit)
	sep = ' '//logsep//' '//'logging stop'
	WRITE(UNIT = logUnit, FMT='(a)') TRIM(sep)
	
	INQUIRE ( UNIT = logUnit, OPENED = fileIsOpen, IOSTAT = ios )
	IF (fileIsOpen) THEN
	  CLOSE (logUnit)
	END IF
ENDIF

END SUBROUTINE LogStop

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
!prints the current YMDHMS date as a time stamp.
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
!!   write a formatted string on specified unit- It is called by Catch routine
SUBROUTINE LogMsg &
!
(unit, level, process, comment, argument)

IMPLICIT NONE

! Subroutine arguments 
! Scalar arguments with intent(in):
INTEGER	(KIND = short), INTENT(in) :: unit
CHARACTER (LEN = *), INTENT(in) :: level !! log level: info, warning, error
CHARACTER (LEN = *), INTENT(in) :: process !! process which puts log
CHARACTER (LEN = *), INTENT(in) :: comment !! comment on log
CHARACTER (LEN = *), INTENT(in), OPTIONAL :: argument !! optional argument
! Local scalars:
CHARACTER (LEN = 3) :: sep
!------------end of declaration------------------------------------------------
         
CALL TimeStamp (unit)

sep = ' '//logsep//' '
WRITE (unit,'(a)', ADVANCE = "no") sep
WRITE (unit,'(a)', ADVANCE = "no") level
WRITE (unit,'(a)', ADVANCE = "no") sep
WRITE (unit,'(a)', ADVANCE = "no") process 
IF ( PRESENT (argument) ) THEN
	WRITE (unit,'(a)', ADVANCE = "no") sep
	WRITE (unit,'(a)', ADVANCE = "no") comment 
	WRITE (unit,'(a)') TRIM(argument )
ELSE
	WRITE (unit,'(a)', ADVANCE = "no") sep
	WRITE (unit,'(a)') comment
ENDIF

END SUBROUTINE LogMsg

!==============================================================================
!! Description:
!!  exception handler
SUBROUTINE Catch &
!
(level, process, comment, code, argument)

IMPLICIT NONE

! Subroutine arguments 
! Scalar arguments with intent(in):
CHARACTER (LEN = *), INTENT(in) :: level !! log level: info | warning | error 
CHARACTER (LEN = *), INTENT(in) :: process !! process which threw esception
CHARACTER (LEN = *), INTENT(in) :: comment !! comment on exception
INTEGER (KIND = short), OPTIONAL, INTENT(in) :: code !! error code to return
CHARACTER (LEN = *), OPTIONAL, INTENT(in) :: argument !! optional argument
! Local Scalars:
INTEGER (KIND = short) :: logStatus
!------------end of declaration------------------------------------------------

!------------------------------------------------------------------------------
![1.0] Define actual log level status
!------------------------------------------------------------------------------
SELECT CASE ( level )
	CASE ("error")
		logStatus = 1
	CASE ("warning")
		logStatus = 2
	CASE ("info")
		logStatus = 3
END SELECT

!------------------------------------------------------------------------------
![2.0] Put an entry on log file
!------------------------------------------------------------------------------
IF (logToFile) THEN
	IF (logStatus <= logLevel) THEN
		IF ( PRESENT(argument) ) THEN
			CALL LogMsg(logUnit, level, process, comment, argument = argument)
		ELSE
			CALL LogMsg(logUnit, level, process, comment)
		ENDIF
	ENDIF
ENDIF 

!------------------------------------------------------------------------------
![3.0] Display on screen
!------------------------------------------------------------------------------
IF (logToScreen) THEN
	IF (logStatus <= logLevel) THEN
       IF (verbose) THEN
	      IF ( PRESENT(argument) ) THEN
		      CALL LogMsg(stdOut, level, process, comment, argument = argument)
	      ELSE
		      CALL LogMsg(stdOut, level, process, comment)
	      ENDIF
       ENDIF
    END IF
END IF

!------------------------------------------------------------------------------
![4.0] Decide program termination
!------------------------------------------------------------------------------
IF (level == 'error' ) THEN
	IF ( PRESENT (code) ) THEN
		IF (logToFile) THEN
			CALL LogStop ()
		ENDIF
		PAUSE
		CALL EXIT(code)
	ELSE
		IF (logToFile) THEN
			CALL LogStop ()
		ENDIF
		PAUSE
		CALL EXIT() 
	ENDIF
ENDIF

END SUBROUTINE Catch

END MODULE LogLib
         