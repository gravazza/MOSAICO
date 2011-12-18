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
!!   define error codes used by log process
!!
!! @author <a href="mailto:giovanni.ravazzani@polimi.it">Giovanni Ravazzani</a> 
!! @version 1.0 - 29th September 2008  
MODULE ErrorCodes         

! History: 
!  
! Version   Date          Comment 
! -------   ----          ------- 
!  1.0      29/Sep/2008   Original code. giovanni ravazzani
! 
! Code Description: 
!   Language:           Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!     Documenting Exchangeable Fortran 90 Code". 
! 

IMPLICIT NONE 
         
! Global Parameters:

! return codes
INTEGER, PARAMETER :: openFileError      = 1000 !! error in opening a file
INTEGER, PARAMETER :: closeFileError     = 1001 !! error in closing a file
INTEGER, PARAMETER :: genericIOError     = 1002 !! generic read-write error
INTEGER, PARAMETER :: iniIOError         = 1003 !! ini file read-write error
INTEGER, PARAMETER :: memAllocError      = 1004 !! memory allocation error
INTEGER, PARAMETER :: unknownOption      = 1005 !! unknown option
INTEGER, PARAMETER :: getUnitError       = 1006 !! error getting free fortran unit
INTEGER, PARAMETER :: consistencyError   = 1007 !! consistency error
INTEGER, PARAMETER :: ncIOError          = 1008 !! netCDF input / output
INTEGER, PARAMETER :: DateTimeError      = 1009 !! error in DateTime format


END MODULE ErrorCodes
         