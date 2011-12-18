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
!!   set of fortran routines to manage date and time.
!!   The module adhers to the International Standard ISO 8601 specifications.
!!   Date and time is expressed in the form YYYY-MM-DDThh:mm:ssTZD
!!   where:
!!     YYYY = four-digit year
!!     MM   = two-digit month (01=January, etc.)
!!     DD   = two-digit day of month (01 through 31)
!!     hh   = two digits of hour (00 through 23) (am/pm NOT allowed)
!!     mm   = two digits of minute (00 through 59)
!!     ss   = two digits of second (00 through 59)
!!     TZD  = time zone designator (Z or +hh:mm or -hh:mm)
!!   A time zone offset of "+hh:mm" indicates that the date/time uses a local 
!!   time zone which is "hh" hours and "mm" minutes ahead of UTC 
!!   (Coordinated Universal Time). A time zone offset of "-hh:mm" 
!!   indicates that the date/time uses a local time zone which is 
!!   "hh" hours and "mm" minutes behind UTC. 
!!   Example: 2007-03-05T01:00:00+02:00
!!   This standard notation helps to avoid confusion in international 
!!   communication caused by the many different national notations and 
!!   increases the portability of computer user interfaces. 
!!   In addition, these formats have several important advantages for 
!!   computer usage compared to other traditional date and time notations. 
!! References:
!!   http://www.w3.org/TR/NOTE-datetime
!!   http://en.wikipedia.org/wiki/ISO_8601
!!   http://www.probabilityof.com/iso/8601v2000.pdf  
!! @author <a href="mailto:giovanni.ravazzani@polimi.it">Giovanni Ravazzani</a> 
!! @version 0.2 - 12th June 2011  
MODULE Chronos         
			
! History: 
!  
! Version   Date                Comment 
! -------       ----                    ------- 
!  0.1        09/Nov/2008   Original code. giovanni ravazzani
!  0.2        12/Jun/2011   Add function to DateTimeIsDefault
! 
! 
! Modules used: 
! 
         
USE DataTypeSizes ,ONLY: & 
! Imported Parameters:  
short,long,float,double

USE LogLib, ONLY : &          
! Imported Routines:
Catch

USE ErrorCodes, ONLY : &
! Imported parameters:
DateTimeError, unknownOption
         
! Declarations must be of the form: 
! [type]   [VariableName]      ! Description/ purpose of variable 
     
IMPLICIT NONE 
! Global (i.e. public) Declarations: 
! Global Procedures:
PUBLIC :: IsLeapYear
PUBLIC :: DayOfYear
PUBLIC :: AddSeconds
PUBLIC :: AddMinutes
PUBLIC :: AddHours
PUBLIC :: AddDays
PUBLIC :: IsUTC
PUBLIC :: ToUTC
PUBLIC :: Now
PUBLIC :: UtcNow
PUBLIC :: GetYear
PUBLIC :: GetMonth
PUBLIC :: GetDay
PUBLIC :: GetHour
PUBLIC :: GetMinute
PUBLIC :: GetSecond
PUBLIC :: GetTimeZone
PUBLIC :: SetYear
PUBLIC :: SetMonth
PUBLIC :: SetDay
PUBLIC :: SetHour
PUBLIC :: SetMinute
PUBLIC :: SetSecond
PUBLIC :: SetTimeZone
PUBLIC :: DateTimeIsDefault

! Global Type Definitions: 
TYPE:: DateTime
	INTEGER (KIND = short) :: year
	INTEGER (KIND = short) :: month
	INTEGER (KIND = short) :: day
	INTEGER (KIND = short) :: hour
	INTEGER (KIND = short) :: minute
	INTEGER (KIND = short) :: second
	!INTEGER (KIND = short) :: millisecond
	INTEGER (KIND = short) :: TZhour
	INTEGER (KIND = short) :: TZminute
	CHARACTER (LEN = 1)    :: TZsign
END TYPE DateTime


! Global Parameters: 
INTEGER, PARAMETER :: timeStringLength = 25
CHARACTER (LEN = timeStringLength) :: timeString
CHARACTER (LEN = timeStringLength), PARAMETER :: timeDefault = '0000-00-00T00:00:00+00:00'
         
! Global Scalars: 
         
! Global Arrays: 
         
! Local (i.e. private) Declarations: 
! Local Procedures:
PRIVATE :: Equality
PRIVATE :: GreaterThan
PRIVATE :: GreaterThanOrEqual
PRIVATE :: LessThan
PRIVATE :: LessThanOrEqual
PRIVATE :: Inequality
PRIVATE :: Parse
PRIVATE :: TimeToString
PRIVATE :: DateCheck
PRIVATE :: DaysInMonth
PRIVATE :: Copy
PRIVATE :: SecondOfYear
PRIVATE :: SecondsToEnd
PRIVATE :: TimeDifference

! Local Type Definitions: 
         
! Local Parameters: 
         
! Local Scalars: 
         
! Local Arrays: 
         
! Operator definitions: 
!   Define new operators or overload existing ones. 
INTERFACE ASSIGNMENT( = )
	MODULE PROCEDURE  TimeToString
	MODULE PROCEDURE  Parse
	MODULE PROCEDURE  Copy
END INTERFACE

INTERFACE OPERATOR ( == )
	MODULE PROCEDURE  Equality
END INTERFACE

INTERFACE OPERATOR ( + )
	MODULE PROCEDURE  AddSeconds
END INTERFACE

INTERFACE OPERATOR ( - )
	MODULE PROCEDURE  TimeDifference
END INTERFACE

INTERFACE OPERATOR ( > )
	MODULE PROCEDURE  GreaterThan
END INTERFACE

INTERFACE OPERATOR ( >= )
	MODULE PROCEDURE  GreaterThanOrEqual
END INTERFACE

INTERFACE OPERATOR ( < )
	MODULE PROCEDURE  LessThan
END INTERFACE

INTERFACE OPERATOR ( <= )
	MODULE PROCEDURE  LessThanOrEqual
END INTERFACE

INTERFACE OPERATOR ( /= )
	MODULE PROCEDURE  Inequality
END INTERFACE



!=======         
CONTAINS
!======= 
! Define procedures contained in this module. 

!==============================================================================
!! Description:
!!   return true if time1 is equal to time2. Before comparison, dates are 
!!   converted to UTC
FUNCTION Equality &
!
(time1, time2) &
!
RESULT (isEqual)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time1, time2

! Local declarations:
LOGICAL :: isEqual
TYPE (DateTime) :: tempTime1, tempTime2

!------------end of declaration------------------------------------------------

!converto to utc
tempTime1 = ToUtc (time1)
tempTime2 = ToUtc (time2)

!perform comparison
IF(  tempTime1 % year   == tempTime2 % year   .AND. &
     tempTime1 % month  == tempTime2 % month  .AND. &
     tempTime1 % day    == tempTime2 % day    .AND. &
     tempTime1 % hour   == tempTime2 % hour   .AND. &
     tempTime1 % minute == tempTime2 % minute .AND. &
     tempTime1 % second == tempTime2 % second       ) THEN
  isEqual = .TRUE.
ELSE
  isEqual = .FALSE.
END IF

END FUNCTION Equality

!==============================================================================
!! Description:
!!   return true if time1 is greater than time2
FUNCTION GreaterThan &
!
(time1, time2) &
!
RESULT (greater)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time1, time2

! Local declarations:
LOGICAL :: greater
TYPE (DateTime) :: tempTime1, tempTime2

!------------end of declaration------------------------------------------------

!converto to utc
tempTime1 = ToUtc (time1)
tempTime2 = ToUtc (time2)

!perform comparison
IF(tempTime1 % year > tempTime2 % year)           THEN
   greater = .TRUE.
ELSE IF(tempTime1 % year < tempTime2 % year)      THEN
   greater = .FALSE.
ELSE IF(tempTime1 % month > tempTime2 % month)    THEN
   greater = .TRUE.
ELSE IF(tempTime1 % month < tempTime2 % month)    THEN
   greater = .FALSE.
ELSE IF(tempTime1 % day > tempTime2 % day)        THEN
   greater = .TRUE.
ELSE IF(tempTime1 % day < tempTime2 % day)        THEN
   greater = .FALSE.
ELSE IF(tempTime1 % hour > tempTime2 % hour)      THEN
   greater = .TRUE.
ELSE IF(tempTime1 % hour < tempTime2 % hour)      THEN
   greater = .FALSE.
ELSE IF(tempTime1 % minute > tempTime2 % minute)  THEN
   greater = .TRUE.
ELSE IF(tempTime1 % minute < tempTime2 % minute)  THEN
   greater = .FALSE.
ELSE IF(tempTime1 % second > tempTime2 % second)  THEN
   greater = .TRUE.
ELSE
   greater = .FALSE.
END IF

END FUNCTION GreaterThan

!==============================================================================
!! Description:
!!   return true if time1 is greater than time2
!!   or time1 is equal to time2
FUNCTION GreaterThanOrEqual &
!
(time1, time2) &
!
RESULT (greatequal)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time1, time2

! Local scalars:
LOGICAL :: greatequal
TYPE (DateTime) :: tempTime1, tempTime2

!------------end of declaration------------------------------------------------

!converto to utc
tempTime1 = ToUtc (time1)
tempTime2 = ToUtc (time2)


IF ( tempTime1 > tempTime2 .OR. tempTime1 == tempTime2 ) THEN
  greatequal = .TRUE.
ELSE
  greatequal = .FALSE.
END IF

END FUNCTION GreaterThanOrEqual

!==============================================================================
!! Description:
!!   return true if time1 is less than time2
FUNCTION LessThan &
!
(time1, time2) &
!
RESULT (less)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time1, time2

! Local declarations:
LOGICAL :: less
TYPE (DateTime) :: tempTime1, tempTime2

!------------end of declaration------------------------------------------------

!converto to utc
tempTime1 = ToUtc (time1)
tempTime2 = ToUtc (time2)

IF(tempTime1 % year < tempTime2 % year)           THEN
   less = .TRUE.
ELSE IF(tempTime1 % year > tempTime2 % year)      THEN
   less = .FALSE.
ELSE IF(tempTime1 % month < tempTime2 % month)    THEN
   less = .TRUE.
ELSE IF(tempTime1 % month > tempTime2 % month)    THEN
   less = .FALSE.
ELSE IF(tempTime1 % day < tempTime2 % day)        THEN
   less = .TRUE.
ELSE IF(tempTime1 % day > tempTime2 % day)        THEN
   less = .FALSE.
ELSE IF(tempTime1 % hour < tempTime2 % hour)      THEN
   less = .TRUE.
ELSE IF(tempTime1 % hour > tempTime2 % hour)      THEN
   less = .FALSE.
ELSE IF(tempTime1 % minute < tempTime2 % minute)  THEN
   less = .TRUE.
ELSE IF(tempTime1 % minute > tempTime2 % minute)  THEN
   less = .FALSE.
ELSE IF(tempTime1 % second < tempTime2 % second)  THEN
   less = .TRUE.
ELSE
   less = .FALSE.
END IF

END FUNCTION LessThan

!==============================================================================
!! Description:
!!   return true if time1 is less than time2
!!   or time1 is equal to time2
FUNCTION LessThanOrEqual &
!
(time1, time2) &
!
RESULT (lessequal)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time1, time2

! Local declarations:
LOGICAL :: lessequal
TYPE (DateTime) :: tempTime1, tempTime2

!------------end of declaration------------------------------------------------

!converto to utc
tempTime1 = ToUtc (time1)
tempTime2 = ToUtc (time2)

IF ( tempTime1 < tempTime2 .OR. tempTime1 == tempTime2 ) THEN
  lessequal = .TRUE.
ELSE
  lessequal = .FALSE.
END IF

END FUNCTION LessThanOrEqual

!==============================================================================
!! Description:
!!   return true if time1 is different from time2
FUNCTION Inequality &
!
(time1, time2) &
!
RESULT (isInequal)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time1, time2

! Local declarations:
LOGICAL :: isInequal
TYPE (DateTime) :: tempTime1, tempTime2

!------------end of declaration------------------------------------------------

!converto to utc
tempTime1 = ToUtc (time1)
tempTime2 = ToUtc (time2)

IF(  tempTime1 % year   /= tempTime2 % year   .OR. &
     tempTime1 % month  /= tempTime2 % month  .OR. &
     tempTime1 % day    /= tempTime2 % day    .OR. &
     tempTime1 % hour   /= tempTime2 % hour   .OR. &
     tempTime1 % minute /= tempTime2 % minute .OR. &
     tempTime1 % second /= tempTime2 % second       ) THEN
  isInequal = .TRUE.
ELSE
  isInequal = .FALSE.
END IF

END FUNCTION Inequality

!==============================================================================
!! Description:
!! Converts the specified string representation of a 
!! date and time to its DateTime equivalent.
SUBROUTINE  Parse &
!
(time, string)

USE StringManipulation, ONLY : &
! Imported routines:
StringToShort

IMPLICIT NONE

! Arguments with intent(in):
CHARACTER (LEN = timeStringLength), INTENT(IN) :: string 


! Arguments with intent(out):
TYPE (DateTime), INTENT(OUT) :: time

!------------end of declaration------------------------------------------------
!1234567890123456789012345
!2007-03-05T01:00:00+02:00

time % year     = StringToShort ( string (1:4) )
time % month    = StringToShort ( string (6:7) )
time % day      = StringToShort ( string (9:10) )
time % hour     = StringToShort ( string (12:13) )
time % minute   = StringToShort ( string (15:16) )
time % second   = StringToShort ( string (18:19) )
time % TZhour   = StringToShort ( string (21:22) )
time % TZminute = StringToShort ( string (24:25) )
time % TZsign   = string (20:20) 
CALL DateCheck ( time )
END SUBROUTINE Parse

!==============================================================================
!! Description:
!! Converts the value of the current DateTime object to its equivalent 
!! string representation
SUBROUTINE  TimeToString &
!
(string, time)

USE StringManipulation, ONLY: &
! Imported routines:
ToString

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time

! Arguments with intent(out):
CHARACTER(LEN = timeStringLength), INTENT(OUT) :: string
!------------end of declaration------------------------------------------------
string = TRIM ( ToString ( time % year, fmt = '(I4.4)' ) )   // '-' // &
         TRIM ( ToString ( time % month, fmt = '(I2.2)' ) )  // '-' // &
         TRIM ( ToString ( time % day, fmt = '(I2.2)' ) )    // 'T' // &
         TRIM ( ToString ( time % hour, fmt = '(I2.2)' ) )   // ':' // &
         TRIM ( ToString ( time % minute, fmt = '(I2.2)' ) ) // ':' // &
         TRIM ( ToString ( time % second, fmt = '(I2.2)' ) ) //        &
         time % TZsign                                       //        &
         TRIM ( ToString ( time % TZhour, fmt = '(I2.2)' ) ) // ':' // &
         TRIM ( ToString ( time % TZminute, fmt = '(I2.2)' ) )
END SUBROUTINE TimeToString


!==============================================================================
!! Description:
!! Create an exact copy of DateTime.
SUBROUTINE  Copy &
!
(time2, time1)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time1


! Arguments with intent(out):
TYPE (DateTime), INTENT(OUT) :: time2

!------------end of declaration------------------------------------------------

time2 % year     = time1 % year
time2 % month    = time1 % month
time2 % day      = time1 % day
time2 % hour     = time1 % hour
time2 % minute   = time1 % minute
time2 % second   = time1 % second
time2 % TZhour   = time1 % TZhour
time2 % TZminute = time1 % TZminute
time2 % TZsign   = time1 % TZsign
END SUBROUTINE Copy


!==============================================================================
!! Description:
!!   check that date do not contain errors.
SUBROUTINE  DateCheck &
!
( time )

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time

! Local variables:
CHARACTER (LEN = timeStringLength) :: string
!------------end of declaration------------------------------------------------

IF (DateTimeIsDefault(time)) THEN
  RETURN !skip check
END IF

IF ( time % second < 0 .OR. time % second >= 60 ) THEN
   string = time
   CALL Catch ('error', 'Chronos', 'second ', &
               code = DateTimeError, argument = string )
END IF

IF ( time % minute < 0 .OR. time % minute >= 60 ) THEN
   string = time
   CALL Catch ('error', 'Chronos', 'minute ', &
               code = DateTimeError, argument = string )
END IF

IF ( time % hour < 0 .OR. time % hour >= 25 )  THEN
   string = time
   CALL Catch ('error', 'Chronos', 'hour ', &
               code = DateTimeError, argument = string )
END IF

IF ( time % year <= 0)  THEN
   string = time
   CALL Catch ('error', 'Chronos', 'year ', &
               code = DateTimeError, argument = string )
END IF

IF ( time % month <= 0 .OR. time % month > 12 )       THEN
   string = time
   CALL Catch ('error', 'Chronos', 'month ', &
               code = DateTimeError, argument = string )
ELSE IF ( time % day <= 0 .OR. time % day > &
         DaysInMonth (time % month, time % year)     ) THEN
   string = time      
   CALL Catch ('error', 'Chronos', 'day ', &
               code = DateTimeError, argument = string )
END IF
END SUBROUTINE DateCheck

!==============================================================================
!! Description:
!!   Returns true if the specified year is a leap year
!! Method:
!!   In the Gregorian calendar, a normal year consists of 365 days. 
!!   Because the actual length of a sidereal year (the time required 
!!   for the Earth to revolve once about the Sun) is actually 365.25635 days, 
!!   a "leap year" of 366 days is used once every four years to eliminate the 
!!   error caused by three normal (but short) years. Any year that is evenly 
!!   divisible by 4 is a leap year: for example, 1988, 1992, and 1996 
!!   are leap years.
!!   However, there is still a small error that must be accounted for. 
!!   To eliminate this error, the Gregorian calendar stipulates that a year 
!!   that is evenly divisible by 100 (for example, 1900) is a leap year only 
!!   if it is also evenly divisible by 400.
!!   For this reason, the following years are not leap years:
!!   1700, 1800, 1900, 2100, 2200, 2300, 2500, 2600
!!   This is because they are evenly divisible by 100 but not by 400.
!!   The following years are leap years:
!!   1600, 2000, 2400
!!   This is because they are evenly divisible by both 100 and 400. 
FUNCTION IsLeapYear &
!
(year) &
!
RESULT (isLeap)

IMPLICIT NONE

! Arguments with intent(in):
INTEGER (KIND = short), INTENT(IN) :: year

! Local variables:
LOGICAL :: isLeap
!------------end of declaration------------------------------------------------

IF ( MOD ( year, 4 ) == 0 ) THEN
  IF ( MOD ( year, 400) > 0 .AND. MOD ( year, 100) == 0 ) THEN
     isLeap = .FALSE.
  ELSE
     isLeap = .TRUE.
  END IF
ELSE
  isLeap = .FALSE.
END IF 

END FUNCTION IsLeapYear


!==============================================================================
!! Description:
!!   Returns the number of days in the specified month. It accounts
!!   for leap years
FUNCTION DaysInMonth &
!
(month, year) &
!
RESULT (days)

IMPLICIT NONE

! Arguments with intent(in):
INTEGER (KIND = short), INTENT(IN) :: year, month

! Local variables:
INTEGER (KIND = short) :: days
!------------end of declaration------------------------------------------------

IF( (month == 1) .OR. (month == 3) .OR. (month == 5) .OR. (month == 7) .OR. &
    (month == 8) .OR. (month == 10) .OR. (month == 12) ) THEN
     days = 31
ELSE IF ( (month == 4) .OR. (month == 6) .OR. (month == 9) .OR. &
          (month == 11) ) THEN
     days = 30
ELSE IF (month == 2) THEN
     IF ( IsLeapYear (year) ) THEN
        days = 29
     ELSE
        days = 28
     END IF
END IF
END FUNCTION DaysInMonth


!==============================================================================
!! Description:
!!   Gets the day of the year represented by this instance. Returns
!!   366 for leap years
FUNCTION DayOfYear &
!
(time, leap) &
!
RESULT (day)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time
CHARACTER (LEN = *), OPTIONAL, INTENT(IN) :: leap ! 'noleap' ignores 29th february of leap years

! Local variables:
INTEGER (KIND = short) :: day, i
TYPE (DateTime) :: february29
!------------end of declaration------------------------------------------------

day = 0
DO i = 1, time % month - 1
    day = day + DaysInMonth (i, time % year)
END DO
day = day + time % day

IF ( PRESENT (leap) ) THEN
  IF ( leap == 'noleap' ) THEN
    IF ( IsLeapYear (time % year) ) THEN
      !string = time
      !february29 = string
      february29 = time
      february29 % month = 2
      february29 % day = 29
      IF ( time >= february29 ) THEN
        day = day - 1
      END IF
    END IF
  ELSE
    CALL Catch ('warning', 'Chronos', 'unknown option in DayOfYear: ', &
               code = unknownOption, argument = leap )
  END IF
ELSE
  
END IF

END FUNCTION DayOfYear

!==============================================================================
!! Description:
!!   Gets the second of the year represented by this instance.
FUNCTION SecondOfYear &
!
(time) &
!
RESULT (second)

USE Units, ONLY: &
! Imported parameters:
day, hour, minute

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time

! Local variables:
INTEGER (KIND = short) :: second
!------------end of declaration------------------------------------------------

second = (DayOfYear (time) - 1) * day

second = second + GetHour (time) * hour + GetMinute (time) * minute + &
         GetSecond (time)


END FUNCTION SecondOfYear

!==============================================================================
!! Description:
!!   Gets the second to the end of the year represented by this instance.
FUNCTION SecondsToEnd &
!
(time) &
!
RESULT (second)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time

! Local variables:
INTEGER (KIND = short) :: second
!------------end of declaration------------------------------------------------

IF ( IsLeapYear (time % year) ) THEN
  second = 366 * 86400 - SecondOfYear (time) 
ELSE
  second = 365 * 86400 - SecondOfYear (time) 
END IF

END FUNCTION SecondsToEnd

!==============================================================================
!! Description:
!!  Adds the specified number of seconds to the value of this instance.
!!  If number of seconds is a negative number, the amount is subtracted     
FUNCTION AddSeconds &
!
(time1, step) &
!
RESULT (time2)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time1
INTEGER, INTENT(IN) :: step

! Local variables:
TYPE (DateTime):: time2
INTEGER  :: maxDay
!------------end of declaration------------------------------------------------

time2 = time1
IF(step >= 0) THEN
  time2 % second = time2 % second + step
  IF(time2 % second >= 60) THEN
    time2 % minute = time2 % minute  + INT ( time2 % second / 60 )
    time2 % second = time2 % second   - INT ( time2 % second / 60 ) * 60
    IF(time2 % minute >= 60) THEN
      time2 % hour = time2 % hour  + INT ( time2 % minute / 60)
      time2 % minute = time2 % minute - INT(time2%minute / 60) * 60
      IF(time2 % hour >= 24) THEN
        time2 % day = time2 % day  + INT(time2 % hour / 24)
        time2 % hour = time2 % hour - INT(time2%hour / 24) * 24
        maxDay = DaysInMonth (time2 % month, time2 % year)
        DO WHILE (time2 % day > maxDay)
          time2 % month = time2 % month + 1
          time2 % day = time2 % day - maxDay
          IF ( time2 % month == 13) THEN
            time2 % year = time2 % year + 1
            time2 % month = 1
          END IF
          maxDay = DaysInMonth (time2 % month, time2 % year)
        END DO
      END IF
    END IF
  END IF
ELSE
  time2 % second = time2 % second + step
  IF(time2 % second < 0) THEN
    time2 % minute = time2 % minute + INT(time2 % second / 60) - 1
    time2 % second = time2 % second + (-INT(time2 % second / 60) + 1) * 60
    IF(time2 % second == 60) THEN
      time2 % second = 0
      time2 % minute = time2 % minute + 1
    END IF
    IF(time2 % minute < 0) THEN
      time2 % hour = time2 % hour + INT(time2 % minute / 60) - 1				
      time2 % minute = time2 % minute + (-INT(time2 % minute / 60) + 1) * 60
      IF(time2 % minute == 60) THEN
        time2 % minute = 0
        time2 % hour = time2 % hour + 1
      END IF
      IF(time2 % hour < 0) THEN
        time2 % day = time2 % day + INT(time2 % hour / 24) - 1
        time2 % hour = time2 % hour + (-INT(time2 % hour / 24) + 1) * 24
        IF(time2 % hour == 24) THEN
          time2 % hour = 0
          time2 % day = time2 % day + 1
        END IF
        DO WHILE (time2 % day <= 0)
          IF(time2 % month - 1 <= 0) time2 % month = 13
            maxDay = DaysInMonth(time2 % month - 1,time2 % year)
            time2 % month = time2 % month - 1
            time2 % day = time2 % day + maxDay
            IF(time2%month == 12) THEN
              time2 % year = time2 % year - 1
            END IF
            maxDay = DaysInMonth(time2 % month,time2 % year)
        END DO
      END IF
    END IF
  END IF
END IF

CALL DateCheck (time2)
	
END FUNCTION AddSeconds


!==============================================================================
!! Description:
!!  Adds the specified number of days to the value of this instance.      
FUNCTION AddDays &
!
(time1, step) &
!
RESULT (time2)

USE Units, ONLY: &
! imported parameters:
day

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time1
INTEGER, INTENT(IN) :: step

! Local variables:
TYPE (DateTime):: time2
!------------end of declaration------------------------------------------------

time2 = AddSeconds (time1, INT(step * day) )
	
END FUNCTION AddDays

!==============================================================================
!! Description:
!!  Adds the specified number of hours to the value of this instance.      
FUNCTION AddHours &
!
(time1, step) &
!
RESULT (time2)

USE Units, ONLY: &
! Imported parameters:
hour

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time1
INTEGER, INTENT(IN) :: step

! Local variables:
TYPE (DateTime):: time2
!------------end of declaration------------------------------------------------

time2 = AddSeconds (time1, INT(step * hour) )
	
END FUNCTION AddHours

!==============================================================================
!! Description:
!!  Adds the specified number of minutes to the value of this instance.      
FUNCTION AddMinutes &
!
(time1, step) &
!
RESULT (time2)

USE Units, ONLY: &
! Imported parameters:
minute

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time1
INTEGER, INTENT(IN) :: step

! Local variables:
TYPE (DateTime):: time2
!------------end of declaration------------------------------------------------

time2 = AddSeconds (time1, INT(step * minute) )
	
END FUNCTION AddMinutes

!==============================================================================
!! Description:
!!   calculate the difference in seconds between two date: time1 - time2
FUNCTION TimeDifference &
!
(time1, time2) &
RESULT (seconds)

USE Units, ONLY: &
! Imported parameters:
day

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time1, time2

! Local variables:
INTEGER (KIND = long) :: seconds, a, b, c
INTEGER (KIND = short) :: i
!------------end of declaration------------------------------------------------
a = SecondOfYear (time1)
b = SecondOfYear (time2)

c = 0
DO i = time2 % year, time1 % year - 1
  IF (IsLeapYear (i)) THEN
    c = c + 366 * day
  ELSE
    c = c + 365 * day
  END IF
END DO

seconds = a + c - b

END FUNCTION TimeDifference

!==============================================================================
!! Description:
!!   Returns true if datetime object is expressed in UTC      
FUNCTION IsUTC &
!
(time) &
!
RESULT (utc)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time

! Local variables:
LOGICAL :: utc
!------------end of declaration------------------------------------------------

IF (time % TZhour == 0 .AND. time % TZminute == 0) THEN
  utc = .TRUE.
ELSE
  utc = .FALSE.
END IF
	
END FUNCTION IsUTC


!==============================================================================
!! Description:
!!   Converts the value of the current DateTime object to 
!!   Coordinated Universal Time (UTC). 
FUNCTION  ToUTC &
!
(time1) &
!
RESULT (time2)

USE Units, ONLY: &
!Imported parameters
hour, minute

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time1

! Local variables:
TYPE (DateTime) :: time2
INTEGER :: variation

!------------end of declaration------------------------------------------------

!calculate variation in seconds
variation = time1 % TZhour * hour + time1 % TZminute * minute

!Apply variation to convert to UTC
IF (time1 % TZsign == '+' ) THEN !subtract variation
  time2 = AddSeconds (time1, - variation)
ELSE IF (time1 % TZsign == '-' ) THEN !add variation
  time2 = AddSeconds (time1, variation)
END IF
time2 % TZhour   = 0
time2 % TZminute = 0
time2 % TZsign   = '+'
END FUNCTION ToUTC

!==============================================================================
!! Description:
!!   Gets a DateTime object that is set to the current date and time on 
!!   this computer, expressed as the local time. 
FUNCTION  Now &
!
() &
!
RESULT (time)

USE StringManipulation, ONLY: &
!Imported routines
StringToShort

IMPLICIT NONE

! Local variables:
TYPE (DateTime) :: time

! Local scalars:
CHARACTER ( LEN = 8 )  :: systemDate
CHARACTER ( LEN = 10 ) :: systemTime
INTEGER (KIND = short) :: values(8)
CHARACTER ( LEN = 5 )  :: zone
!------------end of declaration------------------------------------------------
 
  CALL date_and_time ( systemDate, systemTime, zone, values )

  time % year = values(1)
  time % month = values(2)
  time % day = values(3)
  time % hour = values(5)
  time % minute = values(6)
  time % second = values(7)
  time % TZhour = StringToShort ( zone (2:3) )
  time % TZminute = StringToShort ( zone (4:5) )
  time % TZsign = zone (1:1)
  
END FUNCTION Now

!==============================================================================
!! Description:
!!   Gets a DateTime object that is set to the current date and time on 
!!   this computer, expressed as the Coordinated Universal Time (UTC).  
FUNCTION  UtcNow &
!
() &
!
RESULT (time)

IMPLICIT NONE

! Local variables:
TYPE (DateTime) :: time

!------------end of declaration------------------------------------------------

time = ToUTC ( Now() )
 
END FUNCTION UtcNow

!==============================================================================
!! Description:
!!   Gets the year of the datetime represented by this instance
FUNCTION  GetYear &
!
(time) &
!
RESULT (year)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time

! Local variables:
INTEGER (KIND = short) :: year

!------------end of declaration------------------------------------------------

year = time % year

END FUNCTION GetYear

!==============================================================================
!! Description:
!!   Gets the month of the datetime represented by this instance
FUNCTION  GetMonth &
!
(time) &
!
RESULT (month)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time

! Local variables:
INTEGER (KIND = short) :: month

!------------end of declaration------------------------------------------------

month = time % month

END FUNCTION GetMonth

!==============================================================================
!! Description:
!!   Gets the day of the datetime represented by this instance
FUNCTION  GetDay &
!
(time) &
!
RESULT (day)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time

! Local variables:
INTEGER (KIND = short) :: day

!------------end of declaration------------------------------------------------

day = time % day

END FUNCTION GetDay

!==============================================================================
!! Description:
!!   Gets the month of the datetime represented by this instance
FUNCTION  GetHour &
!
(time) &
!
RESULT (hour)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time

! Local variables:
INTEGER (KIND = short) :: hour

!------------end of declaration------------------------------------------------

hour = time % hour

END FUNCTION GetHour

!==============================================================================
!! Description:
!!   Gets the minute of the datetime represented by this instance
FUNCTION  GetMinute &
!
(time) &
!
RESULT (minute)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time

! Local variables:
INTEGER (KIND = short) :: minute

!------------end of declaration------------------------------------------------

minute = time % minute

END FUNCTION GetMinute

!==============================================================================
!! Description:
!!   Gets the second of the datetime represented by this instance
FUNCTION  GetSecond &
!
(time) &
!
RESULT (second)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time

! Local variables:
INTEGER (KIND = short) :: second

!------------end of declaration------------------------------------------------

second = time % second

END FUNCTION GetSecond

!==============================================================================
!! Description:
!!   Gets the string representing time zone of the datetime
!!   represented by this instance. Example: '+02:00'
FUNCTION  GetTimeZone &
!
(time) &
!
RESULT (timeZone)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time

! Local variables:
CHARACTER (LEN = 6) :: timeZone
CHARACTER (LEN = 25) :: string

!------------end of declaration------------------------------------------------

string = time
timeZone = string (20:25)

END FUNCTION GetTimeZone

!==============================================================================
!! Description:
!!   Set the year of the datetime represented by this instance
SUBROUTINE  SetYear &
!
(year, time)

IMPLICIT NONE

! Arguments with intent(in):
INTEGER (KIND = short), INTENT(IN) :: year

! Arguments with intent(out):
TYPE (DateTime), INTENT(OUT) :: time

!------------end of declaration------------------------------------------------

time % year = year

CALL DateCheck (time)

END SUBROUTINE SetYear

!==============================================================================
!! Description:
!!   Set the month of the datetime represented by this instance
SUBROUTINE  SetMonth &
!
(month, time)

IMPLICIT NONE

! Arguments with intent(in):
INTEGER (KIND = short), INTENT(IN) :: month

! Arguments with intent(out):
TYPE (DateTime), INTENT(OUT) :: time

!------------end of declaration------------------------------------------------

time % month = month

CALL DateCheck (time)

END SUBROUTINE SetMonth

!==============================================================================
!! Description:
!!   Set the day of the datetime represented by this instance
SUBROUTINE  SetDay &
!
(day, time)

IMPLICIT NONE

! Arguments with intent(in):
INTEGER (KIND = short), INTENT(IN) :: day

! Arguments with intent(out):
TYPE (DateTime), INTENT(OUT) :: time

!------------end of declaration------------------------------------------------

time % day = day

CALL DateCheck (time)

END SUBROUTINE SetDay

!==============================================================================
!! Description:
!!   Set the hour of the datetime represented by this instance
SUBROUTINE  SetHour &
!
(hour, time)

IMPLICIT NONE

! Arguments with intent(in):
INTEGER (KIND = short), INTENT(IN) :: hour

! Arguments with intent(out):
TYPE (DateTime), INTENT(OUT) :: time

!------------end of declaration------------------------------------------------

time % hour = hour

CALL DateCheck (time)

END SUBROUTINE SetHour

!==============================================================================
!! Description:
!!   Set the minute of the datetime represented by this instance
SUBROUTINE  SetMinute &
!
(minute, time)

IMPLICIT NONE

! Arguments with intent(in):
INTEGER (KIND = short), INTENT(IN) :: minute

! Arguments with intent(out):
TYPE (DateTime), INTENT(OUT) :: time

!------------end of declaration------------------------------------------------

time % minute = minute

CALL DateCheck (time)

END SUBROUTINE SetMinute

!==============================================================================
!! Description:
!!   Set the second of the datetime represented by this instance
SUBROUTINE  SetSecond &
!
(second, time)

IMPLICIT NONE

! Arguments with intent(in):
INTEGER (KIND = short), INTENT(IN) :: second

! Arguments with intent(out):
TYPE (DateTime), INTENT(OUT) :: time

!------------end of declaration------------------------------------------------

time % second = second

CALL DateCheck (time)

END SUBROUTINE SetSecond

!==============================================================================
!! Description:
!!   Set the timezone of the datetime represented by this instance
SUBROUTINE  SetTimeZone &
!
(timeZone, time)

USE StringManipulation, ONLY : &
! Imported routines:
StringCompact

IMPLICIT NONE

! Arguments with intent(in):
CHARACTER (LEN = *), INTENT(IN) :: timeZone

! Arguments with intent(out):
TYPE (DateTime), INTENT(OUT) :: time

! Local variables:
CHARACTER (LEN = 25) :: string

!------------end of declaration------------------------------------------------

string = time
string (20:25) = timeZone
time = string
CALL DateCheck (time)

END SUBROUTINE SetTimeZone


!==============================================================================
!! Description:
!!   Check if datetime is set to default
FUNCTION  DateTimeIsDefault &
!
(time) &
!
RESULT (isDefault)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (DateTime), INTENT(IN) :: time

! Local declarations:
LOGICAL :: isDefault

!------------end of declaration------------------------------------------------

IF (time % year == 0 .AND. &
    time % month == 0 .AND. &
    time % day == 0 .AND. &
    time % hour == 0 .AND. &
    time % minute == 0 .AND. &
    time % second == 0 ) THEN
    
    isDefault = .TRUE.
ELSE
    isDefault = .FALSE.
END IF

RETURN

END FUNCTION DateTimeIsDefault





!http://msdn.microsoft.com/en-us/library/system.datetime_members.aspx
!AddHours, AddMinutes, AddSeconds,AddDays,AddYears, AddMonths

END MODULE Chronos

!==============================================================================
!! Description:
!!  Adds the specified number of years to the value of this instance.      
!FUNCTION AddYears &
!
!(time1, step) &
!
!RESULT (time2)

!IMPLICIT NONE

! Arguments with intent(in):
!TYPE (DateTime), INTENT(IN) :: time1
!INTEGER, INTENT(IN) :: step

! Local variables:
!TYPE (DateTime):: time2
!------------end of declaration------------------------------------------------
!time2 = time1
!time2 % year = time1 % year + step
	
!END FUNCTION AddYears

!==============================================================================
!! Description:
!!   Adds the specified number of months to the value of this instance.   
!! Limits:
!!   It does not accept negative numbers   
!RECURSIVE FUNCTION AddMonths &
!
!(time1, step) &
!
!RESULT (time2)

!IMPLICIT NONE

! Arguments with intent(in):
!TYPE (DateTime), INTENT(IN) :: time1
!INTEGER, INTENT(IN) :: step

! Local variables:
!TYPE (DateTime):: time2
!INTEGER  :: months
!------------end of declaration------------------------------------------------
!time2 = time1
!time2 % month = time1 % month + step 
!IF (time2 % month > 12 ) THEN
!  time2 = AddYears (time2,1)
!  CALL SetMonth (1, time2)
!  months = 13 - time1 % month
!  time2 = AddMonths (time2, step - months)
!END IF

!CALL DateCheck (time2)
	
!END FUNCTION AddMonths
