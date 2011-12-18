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
!!   Provide constant values and factor for the conversion from non SI units
!!   to SI units.
!!
!! References and Credits:
!!   SI_units module by Grant W. Petty.
!!
!! @author <a href="mailto:giovanni.ravazzani@polimi.it">Giovanni Ravazzani</a> 
!! @version 1.0 - 9th July 2008  
MODULE Units        

! History: 
!  
! Version   Date          Comment 
! -------   ----          ------- 
! 1.0       9/Jul/2008    Original code. giovanni ravazzani
! 
! Code Description: 
!   Language:           Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!     Documenting Exchangeable Fortran 90 Code". 
! 
! Modules used: 
! 
USE DataTypeSizes, ONLY : & 
         
! Imported Parameters:
float, double

IMPLICIT NONE 

! Global (i.e. public) Declarations: 

! Constants of nature
REAL(KIND = double), PUBLIC, PARAMETER :: pi = 3.141592653589793_double
REAL(KIND = double), PUBLIC, PARAMETER :: speedOfLight = 2.99792458E+8_double ! [m/s]
REAL(KIND = float ), PUBLIC, PARAMETER :: stefanBoltzman = 5.6704E-8_float ![watt/ (m2 * kelvin)]
REAL(KIND = float ), PUBLIC, PARAMETER :: gravityAccel = 9.80665_float ![m/s2]
REAL(KIND = float ), PUBLIC, PARAMETER :: icePoint = 273.15_float ![k]
REAL(KIND = float ), PUBLIC, PARAMETER :: waterTriplePoint = 273.16_float ![k]

! Units of length [m]
REAL(KIND = double), PUBLIC, PARAMETER :: angstrom = 1E-10_double
REAL(KIND = double), PUBLIC, PARAMETER :: micron = 1E-6_double
REAL(KIND = float ), PUBLIC, PARAMETER :: millimeter = 1E-3_float
REAL(KIND = float ), PUBLIC, PARAMETER :: mm = 1E-3_float !abbreviation
REAL(KIND = float ), PUBLIC, PARAMETER :: centimeter = 1E-2_float
REAL(KIND = float ), PUBLIC, PARAMETER :: cm = 1E-2_float !abbreviation
REAL(KIND = float ), PUBLIC, PARAMETER :: kilometer = 1E+3_float
REAL(KIND = float ), PUBLIC, PARAMETER :: km = 1E+3_float !abbreviation
REAL(KIND = float ), PUBLIC, PARAMETER :: nauticalMile = 1.852E+3_float
REAL(KIND = float ), PUBLIC, PARAMETER :: inch = 2.54 * cm
REAL(KIND = float ), PUBLIC, PARAMETER :: foot = 12.0 * inch
REAL(KIND = float ), PUBLIC, PARAMETER :: yard = 3.0 * foot
REAL(KIND = float ), PUBLIC, PARAMETER :: mile = 5280.0 * foot

! Units of area [m2]
REAL(KIND = float ), PUBLIC, PARAMETER :: squareMillimeter = millimeter ** 2_float
REAL(KIND = float ), PUBLIC, PARAMETER :: mm2 = millimeter ** 2_float !abbreviation
REAL(KIND = float ), PUBLIC, PARAMETER :: squareCentimeter = centimeter ** 2_float
REAL(KIND = float ), PUBLIC, PARAMETER :: cm2 = centimeter ** 2_float !abbreviation
REAL(KIND = float ), PUBLIC, PARAMETER :: squareKilometer = kilometer ** 2_float
REAL(KIND = float ), PUBLIC, PARAMETER :: km2 = kilometer ** 2_float !abbreviation
REAL(KIND = float ), PUBLIC, PARAMETER :: hectare = 1E+4_float
REAL(KIND = float ), PUBLIC, PARAMETER :: ha = 1E+4_float !abbreviation
REAL(KIND = float ), PUBLIC, PARAMETER :: squareInch = inch ** 2_float
REAL(KIND = float ), PUBLIC, PARAMETER :: squareFoot = foot ** 2_float
REAL(KIND = float ), PUBLIC, PARAMETER :: squareYard = yard ** 2_float
REAL(KIND = float ), PUBLIC, PARAMETER :: squareMile = mile ** 2_float
REAL(KIND = float ), PUBLIC, PARAMETER :: acre = 4356_float * squareFoot

! Units of volume [m3]
REAL(KIND = float ), PUBLIC, PARAMETER :: liter = 1E-3_float
REAL(KIND = float ), PUBLIC, PARAMETER :: cubicCentimeter = centimeter ** 3_float
REAL(KIND = float ), PUBLIC, PARAMETER :: cc = centimeter ** 3_float !abbreviation
REAL(KIND = float ), PUBLIC, PARAMETER :: milliLiter = centimeter ** 3_float !synonym
REAL(KIND = float ), PUBLIC, PARAMETER :: cubicInch = inch ** 3_float
REAL(KIND = float ), PUBLIC, PARAMETER :: cubicFoot = foot ** 3_float
REAL(KIND = float ), PUBLIC, PARAMETER :: imperialGallon = 4.54609_float * liter
REAL(KIND = float ), PUBLIC, PARAMETER :: usaGallon = 3.785412_float * liter
REAL(KIND = float ), PUBLIC, PARAMETER :: quart = usaGallon / 4_float
REAL(KIND = float ), PUBLIC, PARAMETER :: pint = usaGallon / 8_float
REAL(KIND = float ), PUBLIC, PARAMETER :: cup = usaGallon / 16_float
REAL(KIND = float ), PUBLIC, PARAMETER :: ounce = usaGallon / 128_float
REAL(KIND = float ), PUBLIC, PARAMETER :: tableSpoon = usaGallon / 256_float
REAL(KIND = float ), PUBLIC, PARAMETER :: teaSpoon = usaGallon / 768_float

! Units of time [s]
REAL(KIND = float ), PUBLIC, PARAMETER :: minute = 60_float 
REAL(KIND = float ), PUBLIC, PARAMETER :: hour = 60_float * minute
REAL(KIND = float ), PUBLIC, PARAMETER :: day = 24_float * hour
REAL(KIND = float ), PUBLIC, PARAMETER :: week = 7_float * day
REAL(KIND = float ), PUBLIC, PARAMETER :: month = 30.416666667_float * day
REAL(KIND = float ), PUBLIC, PARAMETER :: year = 365_float * day


! Conversion factor
REAL(KIND = float), PUBLIC, PARAMETER :: degToRad = pi / 180_float !Degree to radians
REAL(KIND = float), PUBLIC, PARAMETER :: radToDeg = 180_float / pi !Degree to radians 



END MODULE Units


