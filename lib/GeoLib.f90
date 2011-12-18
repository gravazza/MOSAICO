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
!!   set of fortran routines that allows you to convert geographic 
!!   coordinates among a variety of coordinate systems, 
!!   map projections, and datums.  
!!   The function Convert is the heart of the GeoLib module.
!!   It is this function that actually performs all coordinate conversions 
!!   and datum shifts.  Given the input and output datums, the input and 
!!   output coordinate reference system, including the values of any 
!!   projection parameters, and the input coordinates, it produces 
!!   the output coordinates.
!! Methods:
!!   The general case is handled in three stages:
!!   1. Convert the input coordinates from the input coordinate reference 
!!      system to geodetic,
!!   2. Shift the intermediate geodetic coordinates from the input datum 
!!      to the output datum, converting between ellipsoid if necessary,
!!   3. Convert the shifted intermediate geodetic coordinates to the 
!!      output coordinate reference system.
!!   The first stage is accomplished by a case statement on the input 
!!   coordinate reference system, with a case for each of the coordinate 
!!   reference systems supported.  If the input coordinates are already 
!!   in geodetic, they are simply copied to the intermediate coordinates.
!!   The second stage is accomplished in two steps.  First, the intermediate 
!!   Geodetic coordinates are shifted to WGS 84, if necessary. Second, 
!!   the shifted intermediate geodetic coordinates are shifted to 
!!   the output datum.
!!   The third stage is similar to the first stage.  A case statement 
!!   on the output coordinate reference system, with a case for each 
!!   of the coordinate reference frames supported, is used to convert 
!!   the shifted intermediate Geodetic coordinates to the output coordinate 
!!   reference system.  If the output coordinate reference system is Geodetic, 
!!   the shifted intermediate coordinates are simply copied to the output
!!   coordinates.

!! References:
!!   GeoTrans: http://earth-info.nga.mil/GandG/geotrans/index.html
!!   European Petroleum Survey Group (EPSG): http://www.epsg.org
!!   http://spatialreference.org/
!!   http://www.borneo.name/topo/roma40.html
!! @author <a href="mailto:giovanni.ravazzani@polimi.it">Giovanni Ravazzani</a> 
!! @version 0.6 - 3rd February 2011 
MODULE GeoLib

! History: 
!  
! Version   Date                Comment 
! -------       ----                    ------- 
!  0.1        22/Dec/2008   Original code. giovanni ravazzani
!  0.2        09/Aug/2009   Added ScanDatum to convert string to numeric code
!  0.3        12/Jul/2010   Support for Hotine Oblique Mercator
!  0.4        15/Jul/2010   Support for Swiss Oblique Cylindrical 
!  0.5        25/Nov/2010   Pass to double precision in ConvertTransverseMercatorToGeodetic
!  0.6        03/Feb/2011   Added function for computing distance between two points
! 
! Modules used: 
USE DataTypeSizes ,ONLY: & 
! Imported Parameters:  
short,long,float,double

USE LogLib, ONLY : &          
! Imported Routines:
Catch

USE ErrorCodes, ONLY : &
! Imported parameters:
memAllocError, consistencyError, unknownOption

USE Units, ONLY: &
! Imported parameters:
degToRad, radToDeg


! Global Parameters:
!!Coordinate Reference Systems
INTEGER, PARAMETER :: &
GEODETIC =    1,   & !geodetic
UTM  =        2,   & !Universal Transverse Mercator
GAUSS_BOAGA = 3,   & !Gauss Boaga (Italy)
TM          = 4,   & !Transverse Mercator
HOM         = 5,   & !Hotine Oblique Mercator
SOC         = 6      !Swiss Oblique Cylindrical

!!Datums
INTEGER, PARAMETER :: &
WGS84  = 6326, & !!World Geodetic System 1984
ED50   = 6230, & !!European Datum 1950
ROME40 = 6265, & !!Monte Mario Italy
CH1903 = 6149    !!Switzerland 

!!general
INTEGER, PARAMETER :: &
NORTH = 1, &
SOUTH = 2, &
EAST  = 3, &
WEST  = 4 

! Global Type Definitions: 

TYPE :: Ellipsoid
  !primary ellipsoid parameters
  CHARACTER (LEN = 100)  :: name
  INTEGER (KIND = short) :: code !!EPSG code
  CHARACTER (LEN = 100)  :: epsg !!EPSG string
  REAL (KIND = Float)    :: a !!semi-major axis
  REAL (KIND = Float)    :: b !!semi-minor axis
  REAL (KIND = Float)    :: inv_f !!a/(a-b)
  !derived ellipsoid parameters
  REAL (KIND = Float)    :: e   !!eccentricity
  REAL (KIND = Float)    :: f   !!flattening
  REAL (KIND = Float)    :: e_second !!second eccentricity
END TYPE ellipsoid

TYPE :: datum
!parameters for datum transformation
  CHARACTER (LEN = 100)  :: name
  INTEGER (KIND = short) :: code
  INTEGER (KIND = short) :: ellipsoid !!reference ellipsoid code
  INTEGER (KIND = short) :: prime_meridian !!EPSG code defining prime meridian
  INTEGER (KIND = short) :: method !!EPSG code defining operation method for translation to WGS84
  REAL (KIND = Float)    :: dx !!entity of x-axis translation
  REAL (KIND = Float)    :: dy !!entity of y-axis translation
  REAL (KIND = Float)    :: dz !!entity of z-axis translation
  CHARACTER (LEN = 100)  :: epsg !!EPSG string
END TYPE datum

!!coordinate reference system
TYPE :: CRS 
  !common definitions to all systems
  INTEGER :: system !! geodetic, UTM, Gaus  Boaga, etc.. 
  CHARACTER (LEN = 50) :: name !!name of CRS
  TYPE (ellipsoid) :: ellipsoid 
  TYPE (datum) :: datum  
  INTEGER (KIND = short) :: basic !! number of basic parameters for the reference system

  !each reference system requires different parameters -> use dynamic allocation
  REAL (KIND = FLOAT), ALLOCATABLE :: param (:) !!required parameters for the definition of reference system
  CHARACTER (LEN = 100), ALLOCATABLE :: description (:) !!description of the parameter (eg. zone for UTM, etc..)
END TYPE CRS

TYPE :: Coordinate
  REAL (KIND = Float) :: easting !!X coordinate, longitude
  REAL (KIND = Float) :: northing !!Y coordinate, latitude
  TYPE (CRS)          :: system  !!coordinate reference system
END TYPE Coordinate

!Global variables:
TYPE (Coordinate) :: point1, point2 !!can be used to calculate distance between two pints


!Global routines
PUBLIC :: GeoInit
PUBLIC :: SetCRS
PUBLIC :: Convert
PUBLIC :: SetCoord
PUBLIC :: ScanDatum
PUBLIC :: SetGeodeticParameters
PUBLIC :: SetUTMparameters
PUBLIC :: SetTransverseMercatorParameters
PUBLIC :: SetGaussBoagaParameters
PUBLIC :: SetHotineParameters
PUBLIC :: Distance

! Local (i.e. private) Declarations: 
! Local Procedures:
PRIVATE :: SetCRScoord
PRIVATE :: SetCRSsystem
PRIVATE :: CopyEllipsoid
PRIVATE :: SearchEllipsoidByCode
PRIVATE :: CopyDatum
PRIVATE :: CopyCRS
PRIVATE :: SearchDatumByCode
PRIVATE :: CRSisEqual
PRIVATE :: EllipsoidIsEqual
PRIVATE :: DatumIsEqual
PRIVATE :: Rho
PRIVATE :: Nu
PRIVATE :: MeridionalDistance
PRIVATE :: GeodeticShiftToWGS84
PRIVATE :: GeodeticShiftFromWGS84
PRIVATE :: MolodenskyShift
!Geodetic
PRIVATE :: SetGeodeticParametersSystem
PRIVATE :: SetGeodeticParametersCoord
!UTM
PRIVATE :: SetUTMparametersSystem
PRIVATE :: SetUTMparametersCoord
PRIVATE :: ConvertUTMtoGeodetic
PRIVATE :: ConvertGeodeticToUTM
!Transverse Mercator
PRIVATE :: SetTransverseMercatorParametersSystem
PRIVATE :: SetTransverseMercatorParametersCoord
PRIVATE :: ConvertTransverseMercatorToGeodetic
PRIVATE :: ConvertGeodeticToTransverseMercator
!Gauss Boaga
PRIVATE :: SetGaussBoagaParametersSystem
PRIVATE :: SetGaussBoagaParametersCoord
PRIVATE :: ConvertGaussBoagaToGeodetic
PRIVATE :: ConvertGeodeticToGaussBoaga
!Hotine Oblique Mercator
PRIVATE :: SetHotineParametersSystem
PRIVATE :: SetHotineParametersCoord
PRIVATE :: ConvertHotineToGeodetic
PRIVATE :: ConvertGeodeticToHotine
!Swiss Oblique Cylindrical
PRIVATE :: SetSwissParametersSystem
PRIVATE :: SetSwissParametersCoord
PRIVATE :: ConvertSwissToGeodetic
!PRIVATE :: ConvertGeodeticToSwiss


!Local parameters

INTEGER, PRIVATE, PARAMETER :: &
!UTM related parameters:
!define array index
UTM_lat0           = 1, &
UTM_centM          = 2, &
UTM_zone           = 3, &
UTM_hemisphere     = 4, &
UTM_false_easting  = 5, &
UTM_false_northing = 6, &
UTM_scale_factor   = 7, &
UTM_override       = 8, & !!Zone override flag
!Transverse Mercator related parameters
TM_lat0            = 1, &
TM_centM           = 2, &
TM_false_easting   = 3, &
TM_false_northing  = 4, &
TM_scale_factor    = 5, &
!Gauss Boaga related parameters
!define array index
GB_lat0           = 1, &
GB_centM          = 2, &
GB_fuse           = 3, &
GB_false_easting  = 4, &
GB_false_northing = 5, &
GB_scale_factor   = 6, &
GB_override       = 7, & !!Zone override flag
!Geodetic related parameters:
GEO_a     = 1, &!semi-major axis
GEO_b     = 2, & !semi-minor axis
GEO_invf  = 3, & !inverse flattening
GEO_prime = 4, & !prime meridian
!Hotine Oblique Mercator
HOM_lat0           = 1, &
HOM_centM          = 2, &
HOM_azimuth        = 3, &
HOM_false_easting  = 4, &
HOM_false_northing = 5, &
HOM_scale_factor   = 6, &
!Swiss Oblique Cylindrical
SOC_latc           = 1, &
SOC_lonc           = 2, &
SOC_azimuth        = 3, &
SOC_false_easting  = 4, &
SOC_false_northing = 5, &
SOC_scale_factor   = 6

! Local variables
TYPE(Ellipsoid), PRIVATE, ALLOCATABLE :: ellps (:) !!array of available ellipsoids
TYPE(Datum), PRIVATE, ALLOCATABLE :: datums (:) !!array of available datums
REAL (KIND = Float), PRIVATE, PARAMETER :: null_float = -9999.9
CHARACTER (LEN = 0), PRIVATE, PARAMETER :: null_string = ''

! Operator definitions:
! Define new operators or overload existing ones.

INTERFACE OPERATOR (==)
    MODULE PROCEDURE CRSisEqual
    MODULE PROCEDURE EllipsoidIsEqual
    MODULE PROCEDURE DatumIsEqual
END INTERFACE

INTERFACE ASSIGNMENT( = )
	MODULE PROCEDURE  CopyEllipsoid
	MODULE PROCEDURE  SearchEllipsoidByCode
	MODULE PROCEDURE  CopyDatum
	MODULE PROCEDURE  SearchDatumByCode
	MODULE PROCEDURE  CopyCRS
END INTERFACE

INTERFACE SetCRS
   MODULE PROCEDURE  SetCRScoord
   MODULE PROCEDURE  SetCRSsystem
END INTERFACE

INTERFACE SetGeodeticParameters
   MODULE PROCEDURE  SetGeodeticParametersSystem
   MODULE PROCEDURE  SetGeodeticParametersCoord
END INTERFACE

INTERFACE SetUTMparameters
   MODULE PROCEDURE  SetUTMparametersSystem
   MODULE PROCEDURE  SetUTMparametersCoord
END INTERFACE

INTERFACE SetTransverseMercatorParameters
   MODULE PROCEDURE  SetTransverseMercatorParametersSystem
   MODULE PROCEDURE  SetTransverseMercatorParametersCoord
END INTERFACE

INTERFACE SetGaussBoagaParameters
   MODULE PROCEDURE  SetGaussBoagaParametersSystem
   MODULE PROCEDURE  SetGaussBoagaParametersCoord
END INTERFACE

INTERFACE SetHotineParameters
   MODULE PROCEDURE  SetHotineParametersSystem
   MODULE PROCEDURE  SetHotineParametersCoord
END INTERFACE

INTERFACE SetSwissParameters
   MODULE PROCEDURE  SetSwissParametersSystem
   MODULE PROCEDURE  SetSwissParametersCoord
END INTERFACE

!=======         
CONTAINS
!======= 
! Define procedures contained in this module. 

!==============================================================================
!! Description:
!!   The subroutine converts the current input coordinates in the coordinate
!!  system defined by the current input coordinate system parameters and 
!!  input datum, into output coordinates in the coordinate system defined 
!!  by the output coordinate system parameters and output datum.
SUBROUTINE Convert &
!
(input, output)

IMPLICIT NONE

! Arguments with intent (in):
TYPE (Coordinate), INTENT(IN) :: input

! Arguments with intent(inout):
TYPE (Coordinate), INTENT(INOUT) :: output

! Local variables:
REAL (KIND = float) :: geoLat, geoLon 
REAL (KIND = float) :: latOut, lonOut 
REAL (KIND = float) :: WGS84lat, WGS84lon


!------------end of declaration------------------------------------------------

! First coordinate conversion stage, convert to Geodetic
SELECT CASE (input % system % system )

  CASE (TM)
     CALL ConvertTransverseMercatorToGeodetic (  &
          input % easting,  &
          input % northing, &
          input % system % param (TM_scale_factor), &
          input % system % param (TM_centM), &
          input % system % param (TM_lat0), &
          input % system % ellipsoid % a, &
          input % system % ellipsoid % e, &
          input % system % ellipsoid % e_second, &
          input % system % param (TM_false_northing), &
          input % system % param (TM_false_easting), &
          geoLon, geoLat)
          
  CASE (UTM)
     CALL ConvertUTMtoGeodetic (  &
          input % easting,  &
          input % northing, &
          input % system % param (UTM_scale_factor), &
          input % system % param (UTM_centM), &
          input % system % param (UTM_lat0), &
          input % system % ellipsoid % a, &
          input % system % ellipsoid % e, &
          input % system % ellipsoid % e_second, &
          input % system % param (UTM_false_northing), &
          input % system % param (UTM_false_easting), &
          input % system % param (UTM_override), &
          geoLon, geoLat)

  CASE (GAUSS_BOAGA)
     CALL ConvertGaussBoagaToGeodetic (  &
          input % easting,  &
          input % northing, &
          input % system % param (GB_scale_factor), &
          input % system % param (GB_centM), &
          input % system % param (GB_lat0), &
          input % system % ellipsoid % a, &
          input % system % ellipsoid % e, &
          input % system % ellipsoid % e_second, &
          input % system % param (GB_false_northing), &
          input % system % param (GB_false_easting), &
          geoLon, geoLat)
          
   CASE (HOM)
     CALL ConvertHotineToGeodetic (  &
          input % easting,  &
          input % northing, &
          input % system % param (HOM_scale_factor), &
          input % system % param (HOM_centM), &
          input % system % param (HOM_lat0), &
          input % system % param (HOM_azimuth), &
          input % system % ellipsoid % a, &
          input % system % ellipsoid % e, &
          input % system % param (HOM_false_northing), &
          input % system % param (HOM_false_easting), &
          geoLon, geoLat)   
          
  CASE (SOC)
     CALL ConvertSwissToGeodetic (  &
          input % easting,  &
          input % northing, &
          input % system % param (SOC_scale_factor), &
          input % system % param (SOC_lonc), &
          input % system % param (SOC_latc), &
          input % system % param (SOC_azimuth), &
          input % system % ellipsoid % a, &
          input % system % ellipsoid % e, &
          input % system % param (SOC_false_northing), &
          input % system % param (SOC_false_easting), &
          geoLon, geoLat)  
          
  CASE (GEODETIC)
          geolon = input % easting * degToRad !input is in deg
          geolat = input % northing * degToRad 
  CASE DEFAULT
      CALL Catch ('error', 'GeoLib',  &
       'reference system not supported: ',  &
       code = unknownOption, argument=input % system % name )
END SELECT

! Datum Transformation Stage
! Shift to WGS84, apply geoid correction, shift to output datum
IF ( input % system % datum % code /= WGS84 ) THEN
  CALL GeodeticShiftToWGS84 (input, geoLat, geoLon, WGS84lat, WGS84lon) !OK
  geoLat = WGS84lat
  geoLon = WGS84lon
END IF

IF ( output % system % datum % code /= WGS84 ) THEN
  CALL GeodeticShiftFromWGS84 (output, geoLat, geoLon, latOut, lonOut)
  geoLat = latOut
  geoLon = lonOut
END IF

! Second coordinate conversion stage, convert from Geodetic

SELECT CASE (output % system % system)
  
  CASE (TM)
     CALL ConvertGeodeticToTransverseMercator (  &
          geoLon,  &
          geoLat, &
          output % system % param (TM_scale_factor), &
          output % system % param (TM_centM), &
          output % system % param (TM_lat0), &
          output % system % ellipsoid % a, &
          output % system % ellipsoid % e, &
          output % system % ellipsoid % e_second, &
          output % system % param (TM_false_northing), &
          output % system % param (TM_false_easting), &
          output % easting, &
          output % northing )
  
  CASE (UTM)
     CALL ConvertGeodeticToUTM (  &
          geoLon,  &
          geoLat, &
          output % system % param (UTM_scale_factor), &
          output % system % param (UTM_centM), &
          output % system % param (UTM_lat0), &
          output % system % ellipsoid % a, &
          output % system % ellipsoid % e, &
          output % system % ellipsoid % e_second, &
          output % system % param (UTM_false_northing), &
          output % system % param (UTM_false_easting), &
          output % easting, &
          output % northing, &
          output % system % param (UTM_zone), &
          output % system % param (UTM_hemisphere), &
          output % system % param (UTM_override) )

   CASE (GEODETIC)
     output % northing = geoLat * radToDeg !convert to deg before returning value
     output % easting  = geoLon * radToDeg 
     
   CASE (GAUSS_BOAGA) 
     CALL ConvertGeodeticToGaussBoaga (  &
          geoLon,  &
          geoLat, &
          output % system % param (GB_scale_factor), &
          output % system % param (GB_centM), &
          output % system % param (GB_lat0), &
          output % system % ellipsoid % a, &
          output % system % ellipsoid % e, &
          output % system % ellipsoid % e_second, &
          output % system % param (GB_false_northing), &
          output % system % param (GB_false_easting), &
          output % easting, &
          output % northing )
          
   CASE (HOM) 
     CALL ConvertGeodeticToHotine (  &
          geoLon,  &
          geoLat, &
          output % system % param (HOM_scale_factor), &
          output % system % param (HOM_centM), &
          output % system % param (HOM_lat0), &
          output % system % param (HOM_azimuth), &
          output % system % ellipsoid % a, &
          output % system % ellipsoid % e, &
          output % system % param (HOM_false_northing), &
          output % system % param (HOM_false_easting), &
          output % easting, &
          output % northing )
   
   CASE (SOC) 
     CALL ConvertGeodeticToSwiss (  &
          geoLon,  &
          geoLat, &
          output % system % param (SOC_scale_factor), &
          output % system % param (SOC_lonc), &
          output % system % param (SOC_latc), &
          output % system % param (SOC_azimuth), &
          output % system % ellipsoid % a, &
          output % system % ellipsoid % e, &
          output % system % param (SOC_false_northing), &
          output % system % param (SOC_false_easting), &
          output % easting, &
          output % northing )
   
   CASE DEFAULT
      CALL Catch ('error', 'GeoLib',  &
       'reference system not supported: ',  &
       code = unknownOption, argument=input % system % name )
END SELECT

END SUBROUTINE Convert


!==============================================================================
!! Description:
!!  Initialize parameters for conversion
SUBROUTINE GeoInit &
!
(file)

USE IniLib, ONLY: &
!Imported routines:
IniOpen, IniClose, IniReadInt, IniReadString, &
IniReadReal, &
!Imported type definition:
IniList

USE StringManipulation, ONLY: &
!Imported routines:
ToString

IMPLICIT NONE

! arguments with intent (in):
CHARACTER (LEN = *), INTENT (IN) :: file

! Local variables:
TYPE (IniList)  :: iniDB
INTEGER (KIND = short)  :: ios !!error return code
INTEGER (KIND = short)  :: i

!------------end of declaration------------------------------------------------

!Read file containing ellipsoid and datum parameters
CALL IniOpen (file, iniDB)

!Allocate memory
ALLOCATE  ( ellps ( IniReadInt ('n_ellipsoids', iniDB) ), STAT = ios  )
IF (ios /= 0) THEN
  CALL Catch ('error', 'GeoLib',  &
  'allocation ellipsoid parameters array ',  &
  code = memAllocError )
ENDIF

ALLOCATE  ( datums ( IniReadInt ('n_datums', iniDB) ), STAT = ios  )
IF (ios /= 0) THEN
  CALL Catch ('error', 'GeoLib',  &
  'allocation datums parameters array ',  &
  code = memAllocError )
ENDIF

!Read ellipsoids parameters
DO i = 1, IniReadInt ('n_ellipsoids', iniDB)

  ellps (i) % name = IniReadString ('name', iniDB, &
                   section = 'ellipsoid' // ToString (i) )
  ellps (i) % epsg = IniReadString ('EPSG', iniDB, &
                   section = 'ellipsoid' // ToString (i) )
  ellps (i) % code = IniReadInt ('code', iniDB, &
                   section = 'ellipsoid' // ToString (i) )
  ellps (i) % a = IniReadReal ('semimajor_axis', iniDB, &
                   section = 'ellipsoid' // ToString (i) )
  ellps (i) % b = IniReadReal ('semiminor_axis', iniDB, &
                   section = 'ellipsoid' // ToString (i) )
  ellps (i) % inv_f = IniReadReal ('inverse_flattening', iniDB, &
                   section = 'ellipsoid' // ToString (i) )
END DO

!calculate derived ellipsoid parameters
DO i = 1, IniReadInt ('n_ellipsoids', iniDB)
  ellps (i) % f = 1. / ellps (i) % inv_f
  ellps (i) % e =  ( 2. * ellps (i) % f - ellps (i) % f ** 2. ) ** 0.5 
  ellps (i) % e_second =  ( ellps (i) % e ** 2.  / &
                           (1. - ellps (i) % e ** 2. ) ) ** 0.5 
END DO

!Read datums parameters
DO i = 1, IniReadInt ('n_datums', iniDB)

  datums (i) % name = IniReadString ('name', iniDB, &
                    section = 'datum' // ToString (i) )
  datums (i) % epsg = IniReadString ('EPSG', iniDB, &
                    section = 'datum' // ToString (i) )
  datums (i) % code = IniReadInt ('code', iniDB, &
                    section = 'datum' // ToString (i) )
  datums (i) % ellipsoid = IniReadInt ('ellipsoid', iniDB, &
                    section = 'datum' // ToString (i) )                    
  datums (i) % prime_meridian = IniReadInt ('prime_meridian', iniDB, &
                    section = 'datum' // ToString (i) )   
  datums (i) % method = IniReadInt ('method', iniDB, &
                    section = 'datum' // ToString (i) )                                                          
  datums (i) % dx = IniReadReal ('dx', iniDB, &
                   section = 'datum' // ToString (i) )
  datums (i) % dy = IniReadReal ('dy', iniDB, &
                   section = 'datum' // ToString (i) )
  datums (i) % dz = IniReadReal ('dz', iniDB, &
                   section = 'datum' // ToString (i) )                  
END DO

!Close configuration file
CALL IniClose (IniDB)

END SUBROUTINE GeoInit

!==============================================================================
!! Description:
!!  Initialize Coordinate Reference System, allocate memory and set parameters
!!  to default value if necessary. subroutine receives as input a Coordinate
!!  type argument
SUBROUTINE SetCRScoord &
!
( CRStype, datumType, coord )

IMPLICIT NONE
!Arguments with intent(in):
INTEGER, INTENT (IN) :: CRStype
INTEGER, INTENT (IN) :: datumType 

!Arguments with intent(inout)
TYPE(Coordinate), INTENT (INOUT) :: coord
!------------end of declaration------------------------------------------------

CALL SetCRSsystem (CRStype, datumType, coord % system)

END SUBROUTINE SetCRScoord


!==============================================================================
!! Description:
!!  Initialize Coordinate Reference System, allocate memory and set parameters
!!  to default value if necessary. Subroutine receives as input a CRS
!!  type argument
SUBROUTINE SetCRSsystem &
!
( CRStype, datumType, rs )

IMPLICIT NONE
!Arguments with intent(in):
INTEGER, INTENT (IN) :: CRStype
INTEGER, INTENT (IN) :: datumType 

!Arguments with intent(inout)
TYPE(CRS), INTENT (INOUT) :: rs !!reference system
!------------end of declaration------------------------------------------------

!if a previous system was defined, deallocate and send a warning
IF ( ALLOCATED (rs % param ) ) THEN
 DEALLOCATE (rs % param )
 CALL Catch ('warning', 'GeoLib', 'deallocate already defined CRS parameters' )
END IF
IF ( ALLOCATED (rs % description ) ) THEN
  DEALLOCATE (rs % description )
END IF
!Initialize CRS according to reference system
rs % system = CRStype
SELECT CASE (CRStype)
  CASE (GEODETIC)
    rs % name = 'latitude_longitude'
  CASE (UTM)
    rs % name = 'Universal Transverse Mercator'
  CASE (GAUSS_BOAGA)
    rs % name = 'Gauss Boaga'
  CASE (TM)
    rs % name = 'transverse_mercator'
  CASE (HOM)
    rs % name = 'hotine_oblique_mercator'
  CASE (SOC)
    rs % name = 'swiss_oblique_cylindrical'
END SELECT
rs % datum = datumType
rs % ellipsoid = rs % datum % ellipsoid
SELECT CASE ( CRStype )
  CASE ( GEODETIC )
    rs % basic = 4
    ALLOCATE ( rs % param (4) )
    rs % param = null_float
    ALLOCATE ( rs % description (4) )
    rs % description = null_string
  CASE ( UTM )
    rs % basic = 7
    ALLOCATE ( rs % param (8) )
    rs % param = null_float
    ALLOCATE ( rs % description (8) )
    rs % description = null_string
  CASE (GAUSS_BOAGA)
    rs % basic = 6
    ALLOCATE ( rs % param (7) )
    rs % param = null_float
    ALLOCATE ( rs % description (7) )
    rs % description = null_string
    !datum is set to Monte Mario
    IF (datumType /= ROME40 ) THEN
      rs % datum = ROME40
      rs % ellipsoid = rs % datum % ellipsoid
      CALL Catch ('warning', 'GeoLib',  &
         'Gauss Boaga Datum was set to Monte Mario')
    END IF
  CASE ( TM )
    rs % basic = 5
    ALLOCATE ( rs % param (5) )
    rs % param = null_float
    ALLOCATE ( rs % description (5) )
    rs % description = null_string
  CASE ( HOM )
    rs % basic = 6
    ALLOCATE ( rs % param (6) )
    rs % param = null_float
    ALLOCATE ( rs % description (6) )
    rs % description = null_string
    
  CASE (SOC)
    rs % basic = 6
    ALLOCATE ( rs % param (6) )
    rs % param = null_float
    ALLOCATE ( rs % description (6) )
    rs % description = null_string
    !datum is set to CH1903
    IF (datumType /= CH1903 ) THEN
      rs % datum = CH1903
      rs % ellipsoid = rs % datum % ellipsoid
      CALL Catch ('warning', 'GeoLib',  &
         'Swiss Datum was set to CH1903')
    END IF
    
  CASE DEFAULT
END SELECT

END SUBROUTINE SetCRSsystem

!==============================================================================
!! Description:
!!  copy the content of a CRS variable in another CRS variable
SUBROUTINE CopyCRS &
!
(CRSout, CRSin)


IMPLICIT NONE

!Arguments with intent(in):
TYPE (CRS), INTENT (IN) :: CRSin

!Arguments with intent(out)
TYPE (CRS), INTENT (OUT) :: CRSout

!Local variables:
INTEGER :: i
!------------end of declaration------------------------------------------------

CRSout % system = CRSin % system
CRSout % name = CRSin % name
CRSout % ellipsoid = CRSin % ellipsoid
CRSout % datum = CRSin % datum
CRSout % basic = CRSin % basic
!erase eventual previous data if present 
IF (ALLOCATED (CRSout % param)) THEN
  DEALLOCATE ( CRSout % param )
END IF
IF (ALLOCATED (CRSout % description)) THEN
  DEALLOCATE ( CRSout % description )
END IF 

!Allocate space to complete copy
ALLOCATE ( CRSout % param (SIZE(CRSin % param)) )
ALLOCATE ( CRSout % description (SIZE(CRSin % description)) )

!copy parameters and descriptions
DO i = 1, SIZE(CRSin % param)
  CRSout % param(i) = CRSin % param(i)
  CRSout % description(i) = CRSin % description(i)
END DO

END SUBROUTINE CopyCRS 

!==============================================================================
!! Description:
!!   assign easting and northing coordinates
SUBROUTINE SetCoord &
!
(x, y, coord)


IMPLICIT NONE

!Arguments with intent(in):
REAL (KIND = float), INTENT (IN) :: x !!easting coordinate
REAL (KIND = float), INTENT (IN) :: y !!northing coordinate

!Arguments with intent(inout):
TYPE (Coordinate), INTENT(INOUT) :: coord
!------------end of declaration------------------------------------------------

coord % easting = x
coord % northing = y

END SUBROUTINE SetCoord

!==============================================================================
!! Description:
!!   Create an exact copy of an ellipsoid.
SUBROUTINE  CopyEllipsoid &
!
(ell2, ell1)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (Ellipsoid), INTENT(IN) :: ell1


! Arguments with intent(out):
TYPE (Ellipsoid), INTENT(OUT) :: ell2

!------------end of declaration------------------------------------------------

ell2 % name     = ell1 % name
ell2 % code     = ell1 % code
ell2 % epsg     = ell1 % epsg
ell2 % a        = ell1 % a
ell2 % b        = ell1 % b
ell2 % inv_f    = ell1 % inv_f
ell2 % e        = ell1 % e
ell2 % f        = ell1 % f
ell2 % e_second = ell1 % e_second

END SUBROUTINE CopyEllipsoid

!==============================================================================
!! Description:
!!   Search for ellipsoid parameters using a code.
SUBROUTINE  SearchEllipsoidByCode &
!
(ell, code)

USE StringManipulation, ONLY: &
!Imported routines:
ToString

IMPLICIT NONE

! Arguments with intent(in):
INTEGER, INTENT(IN) :: code

! Arguments with intent(out):
TYPE (Ellipsoid), INTENT(OUT) :: ell

! Local variables:
INTEGER (KIND = short) :: i
LOGICAL :: found

!------------end of declaration------------------------------------------------

found = .FALSE.
DO i = 1, SIZE (ellps)
  IF (ellps (i) % code == code ) THEN
    ell = ellps (i)
    found = .TRUE.
  END IF
END DO

IF ( .NOT. found ) THEN
  CALL Catch ('error', 'GeoLib',  &
         'ellipsoid code not found: ',  &
         argument = ToString (code) )
END IF

END SUBROUTINE SearchEllipsoidByCode

!==============================================================================
!! Description:
!!   Create an exact copy of a datum.
SUBROUTINE  CopyDatum &
!
(datum2, datum1)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (Datum), INTENT(IN) :: datum1


! Arguments with intent(out):
TYPE (Datum), INTENT(OUT) :: datum2

!------------end of declaration------------------------------------------------

datum2 % name     = datum1 % name
datum2 % code     = datum1 % code
datum2 % ellipsoid     = datum1 % ellipsoid
datum2 % prime_meridian        = datum1 % prime_meridian
datum2 % method        = datum1 % method
datum2 % dx    = datum1 % dx
datum2 % dy    = datum1 % dy
datum2 % dz    = datum1 % dz
datum2 % epsg    = datum1 % epsg

END SUBROUTINE CopyDatum

!==============================================================================
!! Description:
!!   Search for datum parameters using a code.
SUBROUTINE  SearchDatumByCode &
!
(dat, code)

USE StringManipulation, ONLY: &
!Imported routines:
ToString

IMPLICIT NONE

! Arguments with intent(in):
INTEGER, INTENT(IN) :: code

! Arguments with intent(out):
TYPE (Datum), INTENT(OUT) :: dat

! Local variables:
INTEGER (KIND = short) :: i
LOGICAL :: found

!------------end of declaration------------------------------------------------

found = .FALSE.
DO i = 1, SIZE (datums)
  IF (datums (i) % code == code ) THEN
    dat = datums (i)
    found = .TRUE.
  END IF
END DO

IF ( .NOT. found ) THEN
    CALL Catch ('error', 'GeoLib',  &
         'datum code not found: ',  &
         argument = ToString (code) )  
END IF

END SUBROUTINE SearchDatumByCode

!==============================================================================
!! Description:
!!   Set parameters for Geodetic reference system
SUBROUTINE SetGeodeticParametersSystem &
!
(system, prime_meridian)

USE Units, ONLY: &
! Imported parameters:
degToRad

IMPLICIT NONE

!Arguments with intent (in):
!!longitude, with respect to Greenwich, of the prime meridian
REAL (KIND = float), INTENT (IN) :: prime_meridian ![deg]
                                                   

! Arguments with intent (inout):
TYPE (CRS), INTENT (INOUT) :: system

!------------end of declaration------------------------------------------------

!set Geodetic parameters value
system % param (GEO_a)  = system % ellipsoid % a
system % param (GEO_b)  = system % ellipsoid % b
system % param (GEO_invf)  = system % ellipsoid % inv_f
system % param (GEO_prime)  = prime_meridian * degToRad !conversion to radians

!set Transverse Mercator parameters description
system % description (GEO_a) = 'semi_major_axis'
system % description (GEO_b) = 'semi_minor_axis'
system % description (GEO_invf) = 'inverse_flattening'
system % description (GEO_prime) = 'prime_meridian_longitude'

END SUBROUTINE SetGeodeticParametersSystem

!==============================================================================
!! Description:
!!   Set parameters for Geodetic reference system
SUBROUTINE SetGeodeticParametersCoord &
!
(coord, prime_meridian)

USE Units, ONLY: &
! Imported parameters:
degToRad

IMPLICIT NONE

!Arguments with intent (in):
!!longitude, with respect to Greenwich, of the prime meridian
REAL (KIND = float), INTENT (IN) :: prime_meridian ![deg]
                                                   

! Arguments with intent (inout):
TYPE (Coordinate), INTENT (INOUT) :: coord

!------------end of declaration------------------------------------------------

CALL SetGeodeticParametersSystem (coord % system, prime_meridian )

END SUBROUTINE SetGeodeticParametersCoord


!==============================================================================
!! Description:
!!   Set parameters for Transverse Mercator reference system
SUBROUTINE SetTransverseMercatorParametersSystem &
!
(system, lat0, centM, falseE, falseN, k)

IMPLICIT NONE

!Arguments with intent (in):
REAL (KIND = float), INTENT (IN) :: lat0 !!Latitude in radians at the origin of the projection
REAL (KIND = float), INTENT (IN) :: centM !!Longitude in radians at the center of the projection
REAL (KIND = float), INTENT (IN) :: falseE !!Easting/X at the center of the projection
REAL (KIND = float), INTENT (IN) :: falseN !!Northing/Y at the center of the projection 
REAL (KIND = float), INTENT (IN) :: k !!scale factor
! Arguments with intent (inout):
TYPE (CRS), INTENT (INOUT) :: system

!------------end of declaration------------------------------------------------

!set Transverse Mercator parameters value
system % param (TM_lat0)  = lat0
system % param (TM_centM)  = centM
system % param (TM_false_easting)  = falseE
system % param (TM_false_northing)  = falseN
system % param (TM_scale_factor)  = k

!set Transverse Mercator parameters description
system % description (TM_lat0) = 'latitude_of_projection_origin'
system % description (TM_centM) = 'central_meridian'
system % description (TM_false_easting) = 'false_easting'
system % description (TM_false_northing) = 'false_northing'
system % description (TM_scale_factor) = 'scale_factor'

END SUBROUTINE SetTransverseMercatorParametersSystem

!==============================================================================
!! Description:
!!   Set parameters for Transverse Mercator reference system
SUBROUTINE SetTransverseMercatorParametersCoord &
!
(coord, lat0, centM, falseE, falseN, k)

IMPLICIT NONE

!Arguments with intent (in):
REAL (KIND = float), INTENT (IN) :: lat0 !!Latitude in radians at the origin of the projection
REAL (KIND = float), INTENT (IN) :: centM !!Longitude in radians at the center of the projection
REAL (KIND = float), INTENT (IN) :: falseE !!Easting/X at the center of the projection
REAL (KIND = float), INTENT (IN) :: falseN !!Northing/Y at the center of the projection 
REAL (KIND = float), INTENT (IN) :: k !!scale factor

! Arguments with intent (inout):
TYPE (Coordinate), INTENT (INOUT) :: coord

!------------end of declaration------------------------------------------------

!set Transverse Mercator parameters value

CALL SetTransverseMercatorParametersSystem (coord % system, lat0, &
                                            centM, falseE, falseN, k)

END SUBROUTINE SetTransverseMercatorParametersCoord

!==============================================================================
!! Description:
!!   Set parameters for Universal Transverse Mercator reference system
SUBROUTINE SetUTMparametersSystem &
!
(system, zone, hemisphere, override)

IMPLICIT NONE

!Arguments with intent (in):
INTEGER (KIND = short), INTENT (IN) :: zone
INTEGER (KIND = short), INTENT (IN) :: hemisphere
INTEGER (KIND = short), OPTIONAL, INTENT (IN) :: override
! Arguments with intent (inout):
TYPE (CRS), INTENT (INOUT) :: system

!------------end of declaration------------------------------------------------

!set UTM parameters value
system % param (UTM_lat0)  = 0.
IF ( zone >= 31 ) THEN
  system % param (UTM_centM) = (6 * Zone - 183) * degToRad  
ELSE
  system % param (UTM_centM) = (6 * Zone + 177) * degToRad
END IF
system % param (UTM_zone)       = zone 
system % param (UTM_hemisphere) = hemisphere
system % param (UTM_false_easting) = 500000.
IF ( hemisphere == NORTH ) THEN
  system % param (UTM_false_northing) = 0.
ELSE
  system % param (UTM_false_northing) = 10000000.
END IF
system % param (UTM_scale_factor) = 0.9996
IF (PRESENT (override) ) THEN
  system % param (UTM_override) = override
ELSE
  !set default to 0 = override off
  system % param (UTM_override) = 0. 
END IF

!Set UTM parameters description
system % description (UTM_lat0) = 'latitude_of_projection_origin'
system % description (UTM_centM) = 'central_meridian' ! 'longitude_of_projection_origin'
system % description (UTM_zone) = 'zone'
IF ( hemisphere == NORTH ) THEN
  system % description (UTM_hemisphere) = 'North'
ELSE
  system % description (UTM_hemisphere) = 'South'
END IF
system % description (UTM_false_easting) = 'false_easting'
system % description (UTM_false_northing) = 'false_northing'
system % description (UTM_scale_factor) = 'scale_factor'

END SUBROUTINE SetUTMparametersSystem

!==============================================================================
!! Description:
!!   Set parameters for Universal Transverse Mercator reference system
SUBROUTINE SetUTMparametersCoord &
!
(coord, zone, hemisphere, override)

IMPLICIT NONE

!Arguments with intent (in):
INTEGER (KIND = short), INTENT (IN) :: zone
INTEGER (KIND = short), INTENT (IN) :: hemisphere
INTEGER (KIND = short), OPTIONAL, INTENT (IN) :: override
! Arguments with intent (inout):
TYPE (Coordinate), INTENT (INOUT) :: coord

!------------end of declaration------------------------------------------------

IF (PRESENT (override) ) THEN
  CALL SetUTMparametersSystem (coord % system, zone, hemisphere, override)
ELSE
  CALL SetUTMparametersSystem (coord % system, zone, hemisphere)
END IF

END SUBROUTINE SetUTMparametersCoord

!==============================================================================
!! Description:
!!   The subroutine  converts Transverse Mercator projection 
!!   (easting and northing) coordinates to geodetic (latitude and longitude)
!!   coordinates, according to the current ellipsoid and Transverse Mercator 
!!   projection parameters.  
SUBROUTINE ConvertTransverseMercatorToGeodetic &
!
(x, y, k, centM, lat0, a, e, eb, falseN, falseE, lon, lat)

USE Units, ONLY: &
!Imported parametes:
pi

USE StringManipulation, ONLY: &
!Imported routines:
ToString

IMPLICIT NONE

!Arguments with intent(in):
REAL (KIND = float), INTENT (IN) :: x !!easting coordinate [m]
REAL (KIND = float), INTENT (IN) :: y !!northing coordinate [m]
REAL (KIND = float), INTENT (IN) :: k !!scale factor
REAL (KIND = float), INTENT (IN) :: centM !!central meridian [radians]
REAL (KIND = float), INTENT (IN) :: lat0 !!latitude of origin [radians]
REAL (KIND = float), INTENT (IN) :: a !! semimajor axis [m]
REAL (KIND = float), INTENT (IN) :: e !! eccentricity
REAL (KIND = float), INTENT (IN) :: eb !! second eccentricity
REAL (KIND = float), INTENT (IN) :: falseN !!false northing
REAL (KIND = float), INTENT (IN) :: falseE !!false easting


!Arguments with intent (out):
REAL (KIND = float), INTENT (OUT) :: lon !!geodetic longitude [radians]
REAL (KIND = float), INTENT (OUT) :: lat !!geodetic latitude [radians]

!Local variables:
REAL (KIND = double) ::  c  !!Cosine of latitude 
REAL (KIND = double) :: es !!eccentricity squared
REAL (KIND = double) :: ebs !!second eccentricity squared
REAL (KIND = double) :: de  !!Delta easting - Difference in Easting (Easting-Fe)
REAL (KIND = double) :: dlam !!Delta longitude - Difference in Longitude
REAL (KIND = double) :: eta !!constant - ebs *c *c 
REAL (KIND = double) :: eta2 
REAL (KIND = double) :: eta3
REAL (KIND = double) :: eta4
REAL (KIND = float) :: ftphi !! Footpoint latitude
INTEGER (KIND = short) :: i  !! Loop iterator
REAL (KIND = double) :: s !!Sine of latitude
REAL (KIND = double) :: sn !!Radius of curvature in the prime vertical
REAL (KIND = double) :: sr !!Radius of curvature in the meridian
REAL (KIND = double) :: t !!Tangent of latitude
REAL (KIND = double) :: tan2
REAL (KIND = double) :: tan4
REAL (KIND = double) :: t10 !!Term in coordinate conversion formula
REAL (KIND = double) :: t11 !!Term in coordinate conversion formula
REAL (KIND = double) :: t12 !!Term in coordinate conversion formula
REAL (KIND = double) :: t13 !!Term in coordinate conversion formula
REAL (KIND = double) :: t14 !!Term in coordinate conversion formula
REAL (KIND = double) :: t15 !!Term in coordinate conversion formula
REAL (KIND = double) :: t16 !!Term in coordinate conversion formula
REAL (KIND = double) :: t17 !!Term in coordinate conversion formula
REAL (KIND = double) :: tmd !!True Meridional distance 
REAL (KIND = double) :: tmdo !!True Meridional distance for latitude of origin 

! Local parameters:
!! Maximum variance for easting and northing values for WGS 84.
REAL (KIND = float), PARAMETER :: deltaEasting = 40000000.0
REAL (KIND = float), PARAMETER :: deltaNorthing = 40000000.0

!------------end of declaration------------------------------------------------


! Check consistency of input coordinates
IF ( x < (falseE - deltaEasting) .OR. &
     x > (falseE + deltaEasting)      ) THEN
 
   CALL Catch ('error', 'GeoLib',   &
			   'Converting Transverse Mercator to Geodetic: &
			    easting out of range' ,  &
			    code = consistencyError, argument = ToString(x) )
END IF

IF ( y < (falseN - deltaNorthing) .OR. &
     y > (falseN + deltaNorthing)      ) THEN
 
   CALL Catch ('error', 'GeoLib',   &
			   'Converting Transverse Mercator to Geodetic: &
			    northing out of range' ,  &
			    code = consistencyError, argument = ToString(y) )
END IF

! True Meridional Distance for latitude of origin 
tmdo = MeridionalDistance (lat0, a, e)

!Origin
tmd = tmdo +  (y - falseN) / k 

!First Estimate
sr = Rho (0., a, e)
ftphi = tmd / sr

DO i = 1,5
  t10 = MeridionalDistance (ftphi, a, e)
  sr = Rho (ftphi, a, e)
  ftphi = ftphi + (tmd - t10) / sr
END DO

! Radius of Curvature in the meridian
sr = Rho (ftphi, a, e)

! Radius of curvature in the prime vertical
sn = Nu (ftphi, a, e)

!Eccentricities squared
es = e**2.
ebs = eb**2.

! Sine Cosine terms
s = SIN(ftphi)
c = COS(ftphi)

! Tangent Value
t = TAN(ftphi)
tan2 = t * t
tan4 = tan2 * tan2
eta = ebs * c**2.
eta2 = eta * eta
eta3 = eta2 * eta
eta4 = eta3 * eta
de = x - falseE
IF ( ABS(de) < 0.0001) THEN
   de = 0.0
END IF

! Latitude 
t10 = t / (2. * sr * sn * k**2.)
t11 = t * (5.  + 3. * tan2 + eta - 4. * eta**2. - 9. * tan2 * eta) / &
          (24. * sr * sn**3. * k**4.)
t12 = t * (61. + 90. * tan2 + 46. * eta + 45. * tan4 &
           - 252. * tan2 * eta  - 3. * eta2 + 100.   & 
           * eta3 - 66. * tan2 * eta2 - 90. * tan4   &
           * eta + 88. * eta4 + 225. * tan4 * eta2   &
           + 84. * tan2* eta3 - 192. * tan2 * eta4 ) &
           / ( 720. * sr * sn**5. * k**6. )
t13 = t * (1385. + 3633. * tan2 + 4095. * tan4 + 1575. * t**6. ) & 
           / (40320. * sr * sn**7. * k**8. )

lat = ftphi - de**2. * t10 + de**4. * t11 - de**6. * t12 + de**8. * t13

t14 = 1. / (sn * c * k)

t15 = (1. + 2. * tan2 + eta) / (6. * sn**3. * c * k**3.)

t16 = (5. + 6. * eta + 28. * tan2 - 3. * eta2 &
       + 8. * tan2 * eta + 24. * tan4 - 4.    & 
       * eta3 + 4. * tan2 * eta2 + 24.        & 
       * tan2 * eta3) / (120. * sn**5. * c * k**5. )

t17 = (61. +  662. * tan2 + 1320. * tan4 + 720. & 
       * t**6. ) / (5040. * sn**7. * c *k**7. )
       
!Difference in Longitude
dlam = de * t14 - de**3. * t15 + de**5. * t16 - de**7. * t17

!Longitude
lon = centM + dlam

! Check errors
IF ( ABS (lat) > 90. * degToRad ) THEN
   CALL Catch ('error', 'GeoLib',   &
    'Converting Transverse Mercator to Geodetic: &
    latitude out of range' ,  &
    code = consistencyError, argument = ToString(lat*radToDeg)//' deg' )
END IF
   
IF( lon > pi ) THEN
  lon = lon - (2. * pi)  
       
ELSE IF (lon < -pi ) THEN
  lon = lon + (2. * pi)
END IF

IF( ABS (lon) > pi ) THEN
     CALL Catch ('error', 'GeoLib',   &
     'Converting Transverse Mercator to Geodetic: &
     longitude out of range' ,  &
     code = consistencyError, argument = ToString(lon*radToDeg)//' deg' )
END IF

! Check for distortion and send warning.
!Distortion will result if Longitude is more than 9 degrees 
!from the Central Meridian at the equator
!and decreases to 0 degrees at the poles
!As you move towards the poles, distortion will become more significant 
IF ( ABS(dlam) > (9.0 * degToRad) * COS(lat) ) THEN
   CALL Catch ('warning', 'GeoLib',   &
     'Converting Transverse Mercator to Geodetic: &
     possible distortion in longitude computation ' ,  &
      argument = ToString(lon*radToDeg)//' deg' ) 
END IF

END SUBROUTINE ConvertTransverseMercatorToGeodetic

!==============================================================================
!! Description:
!!   The subroutine  converts geodetic(latitude and longitude) coordinates 
!!   to Transverse Mercator projection (easting and northing) coordinates, 
!!   according to the current ellipsoid and Transverse Mercator projection 
!!   coordinates.  
SUBROUTINE ConvertGeodeticToTransverseMercator &
!
(lon, lat, k, centM, lat0, a, e, eb, falseN, falseE, x, y)

USE Units, ONLY: &
!Imported parametes:
pi

USE StringManipulation, ONLY: &
!Imported routines:
ToString

IMPLICIT NONE

! Arguments with intent(in):
REAL (KIND = float), INTENT (INOUT) :: lon !!geodetic longitude [radians]
REAL (KIND = float), INTENT (IN) :: lat !!geodetic latitude [radians]
REAL (KIND = float), INTENT (IN) :: k !!scale factor
REAL (KIND = float), INTENT (IN) :: centM !!central meridian [radians]
REAL (KIND = float), INTENT (IN) :: lat0 !!latitude of origin [radians]
REAL (KIND = float), INTENT (IN) :: a !! semimajor axis [m]
REAL (KIND = float), INTENT (IN) :: e !! eccentricity
REAL (KIND = float), INTENT (IN) :: eb !! second eccentricity
REAL (KIND = float), INTENT (IN) :: falseN !!false northing
REAL (KIND = float), INTENT (IN) :: falseE !!false easting



!Arguments with intent(out):
REAL (KIND = float), INTENT (OUT) :: x !!easting coordinate [m]
REAL (KIND = float), INTENT (OUT) :: y !!northing coordinate [m]

!Local variables:
REAL (KIND = float) :: c !Cosine of latitude
REAL (KIND = float) :: c2
REAL (KIND = float) :: c3
REAL (KIND = float) :: c5
REAL (KIND = float) :: c7
REAL (KIND = float) :: es !!eccentricity squared
REAL (KIND = float) :: ebs !!second eccentricity squared
REAL (KIND = float) :: dlam  !Delta longitude - Difference in Longitude
REAL (KIND = float) :: eta !constant - TranMerc_ebs *c *c
REAL (KIND = float) :: eta2
REAL (KIND = float) :: eta3
REAL (KIND = float) :: eta4
REAL (KIND = float) :: s !Sine of latitude
REAL (KIND = float) :: sn !Radius of curvature in the prime vertical
REAL (KIND = float) :: t !Tangent of latitude
REAL (KIND = float) :: tan2
REAL (KIND = float) :: tan3
REAL (KIND = float) :: tan4
REAL (KIND = float) :: tan5
REAL (KIND = float) :: tan6
REAL (KIND = float) :: t1 !Term in coordinate conversion formula
REAL (KIND = float) :: t2 !Term in coordinate conversion formula
REAL (KIND = float) :: t3 !Term in coordinate conversion formula
REAL (KIND = float) :: t4 !Term in coordinate conversion formula
REAL (KIND = float) :: t5 !Term in coordinate conversion formula
REAL (KIND = float) :: t6 !Term in coordinate conversion formula
REAL (KIND = float) :: t7 !Term in coordinate conversion formula
REAL (KIND = float) :: t8 !Term in coordinate conversion formula
REAL (KIND = float) :: t9 !Term in coordinate conversion formula
REAL (KIND = float) :: tmd !True Meridional distance
REAL (KIND = float) :: tmdo !True Meridional distance for latitude of origin
REAL (KIND = float) :: temp_Origin
REAL (KIND = float) :: temp_Long

! Local parameters:
REAL (KIND = float), PARAMETER :: MAX_LAT = 89.99 * degToRad    ! 89.99 degrees in radians
REAL (KIND = float), PARAMETER :: MAX_DELTA_LONG =  90. * degToRad    ! 90. degrees in radians

!------------end of declaration------------------------------------------------

IF ( lat < -MAX_LAT .OR. lat > MAX_LAT ) THEN
  CALL Catch ('error', 'GeoLib',   &
    'Converting Geodetic to Transverse Mercator: &
    latitude out of range' ,  &
    code = consistencyError, argument = ToString(lat*radToDeg)//' deg' )
END IF
  
IF ( lon > pi) THEN
   lon = lon - 2*pi
END IF

 IF ( lon < centM - MAX_DELTA_LONG .OR. lon > centM + MAX_DELTA_LONG ) THEN
    IF ( lon < 0. ) THEN
      temp_Long = lon + 2 * pi
    ELSE
      temp_Long = lon
    END IF
    
    IF ( centM < 0. ) THEN
      temp_Origin = centM + 2 * pi
    ELSE
      temp_Origin = centM
    END IF
    
    IF ( temp_Long < temp_Origin - MAX_DELTA_LONG .OR. &
         temp_Long > temp_Origin + MAX_DELTA_LONG   ) THEN
        CALL Catch ('error', 'GeoLib',   &
         'Converting Geodetic to Transverse Mercator: &
         longitude out of range' ,  &
         code = consistencyError, argument = ToString(lon*radToDeg)//' deg' ) 
    END IF
    
 END IF
 
 !Delta longitude
 dlam = lon - centM
 
 ! Check for distortion and send warning.
!Distortion will result if Longitude is more than 9 degrees 
!from the Central Meridian at the equator
!and decreases to 0 degrees at the poles
!As you move towards the poles, distortion will become more significant 
IF ( ABS(dlam) > (9.0 * degToRad) * COS(lat) ) THEN
   CALL Catch ('warning', 'GeoLib',   &
     'Converting Geodetic to Transverse Mercator: &
     possible distortion in longitude computation ' ,  &
      argument = ToString(lon*radToDeg)//' deg' ) 
END IF

IF ( dlam > pi ) THEN
  dlam = dlam - (2 * pi)
END IF

IF ( dlam < -pi ) THEN
  dlam = dlam + (2 * pi)
END IF

IF ( ABS(dlam) < 2.e-10 ) THEN
  dlam = 0.0
END IF

!Eccentricities squared
es = e**2.
ebs = eb**2.

! Sine Cosine terms
s = SIN (lat)
c = COS (lat)
c2 = c * c
c3 = c2 * c
c5 = c3 * c2
c7 = c5 * c2

! Tangent Value
t = TAN (lat)
tan2 = t * t
tan3 = tan2 * t
tan4 = tan3 * t
tan5 = tan4 * t
tan6 = tan5 * t
eta = ebs * c2
eta2 = eta * eta
eta3 = eta2 * eta
eta4 = eta3 * eta

! Radius of curvature in the prime vertical
sn = Nu (lat, a, e)

!True Meridianal Distances 
tmd = MeridionalDistance (lat, a, e)  

! True Meridional Distance for latitude of origin 
tmdo = MeridionalDistance (lat0, a, e)

!northing 
t1 = (tmd - tmdo) * k
t2 = sn * s * c * k/ 2.e0
t3 = sn * s * c3 * k * (5.e0 - tan2 + 9.e0 * eta + 4.e0 * eta2) /24.e0
t4 = sn * s * c5 * k * (61.e0 - 58.e0 * tan2                     &
     + tan4 + 270.e0 * eta - 330.e0 * tan2 * eta + 445.e0 * eta2 &
     + 324.e0 * eta3 -680.e0 * tan2 * eta2 + 88.e0 * eta4        &
     -600.e0 * tan2 * eta3 - 192.e0 * tan2 * eta4) / 720.e0

t5 = sn * s * c7 * k * (1385.e0 - 3111.e0 * &
     tan2 + 543.e0 * tan4 - tan6) / 40320.e0

y = falseN + t1 + dlam**2. * t2 + dlam**4. * t3 + dlam**6. * t4 + dlam**8. * t5

!Easting 
t6 = sn * c * k
t7 = sn * c3 * k * (1.e0 - tan2 + eta ) / 6.e0
t8 = sn * c5 * k * (5.e0 - 18.e0 * tan2 + tan4                       &
     + 14.e0 * eta - 58.e0 * tan2 * eta + 13.e0 * eta2 + 4.e0 * eta3 &
     - 64.e0 * tan2 * eta2 - 24.e0 * tan2 * eta3 ) / 120.e0
t9 = sn * c7 * k * ( 61.e0 - 479.e0 * tan2 &
     + 179.e0 * tan4 - tan6 ) / 5040.e0

x = falseE + dlam * t6 + dlam**3. * t7 + dlam**5 * t8 + dlam**7. * t9 

END SUBROUTINE ConvertGeodeticToTransverseMercator

!==============================================================================
!! Description:
!!   The subroutine  converts Universal Transverse Mercator projection 
!!   (easting and northing) coordinates to geodetic (latitude and longitude)
!!   coordinates, according to the current ellipsoid and UTM 
!!   projection parameters.
SUBROUTINE ConvertUTMtoGeodetic &
!
!
(x, y, k, centM, lat0, a, e, eb, falseN, falseE, override, lon, lat)

USE StringManipulation, ONLY: &
!Imported routines:
ToString


IMPLICIT NONE

!Arguments with intent(in):
REAL (KIND = float), INTENT (IN) :: x !!easting coordinate [m]
REAL (KIND = float), INTENT (IN) :: y !!northing coordinate [m]
REAL (KIND = float), INTENT (IN) :: k !!scale factor
REAL (KIND = float), INTENT (IN) :: centM !!central meridian [radians]
REAL (KIND = float), INTENT (IN) :: lat0 !!latitude of origin [radians]
REAL (KIND = float), INTENT (IN) :: a !! semimajor axis [m]
REAL (KIND = float), INTENT (IN) :: e !! eccentricity
REAL (KIND = float), INTENT (IN) :: eb !! second eccentricity
REAL (KIND = float), INTENT (IN) :: falseN !!false northing
REAL (KIND = float), INTENT (IN) :: falseE !!false easting
REAL (KIND = float), INTENT (IN) :: override !!override option


!Arguments with intent (out):
REAL (KIND = float), INTENT (OUT) :: lon !!geodetic longitude [radians]
REAL (KIND = float), INTENT (OUT) :: lat !!geodetic latitude [radians]

!Local parameters:
REAL (KIND = float), PARAMETER :: MIN_LAT = -80.5 * degToRad ! -80.5 degrees in radians 
REAL (KIND = float), PARAMETER :: MAX_LAT = 84.5 * degToRad ! 84.5 degrees in radians 
REAL (KIND = float), PARAMETER :: MIN_EASTING = 100000.
REAL (KIND = float), PARAMETER :: MAX_EASTING = 900000.
REAL (KIND = float), PARAMETER :: MIN_NORTHING = 0.
REAL (KIND = float), PARAMETER :: MAX_NORTHING = 10000000.

!------------end of declaration------------------------------------------------

!Check out of range if override is off
IF ( override == 0. ) THEN
    IF ( x < MIN_EASTING .OR. x > MAX_EASTING ) THEN
      CALL Catch ('error', 'GeoLib',   &
			      'Converting UTM to Geodetic: &
			       easting out of range' ,  &
			       code = consistencyError, argument = ToString(x) )
    END IF
    IF ( y < MIN_NORTHING .OR. y > MAX_NORTHING ) THEN
      CALL Catch ('error', 'GeoLib',   &
			      'Converting UTM to Geodetic: &
			       northing out of range' ,  &
			       code = consistencyError, argument = ToString(y) )
    END IF
END IF

CALL ConvertTransverseMercatorToGeodetic (x, y, k, centM, lat0, a, e, eb, &
                                          falseN, falseE, lon, lat)
                                          
IF ( lat < MIN_LAT .OR. lat > MAX_LAT ) THEN
  CALL Catch ('error', 'GeoLib',   &
			 'Converting UTM to Geodetic: &
			 latitude out of range ' ,  &
			 code = consistencyError, &
			 argument = ToString(lat*radToDeg)//' deg' )
END IF                                          

END SUBROUTINE ConvertUTMtoGeodetic

!==============================================================================
!! Description:
!!   The subroutine  converts geodetic(latitude and longitude) coordinates 
!!   to UTM projection (easting and northing) coordinates, 
!!   according to the current ellipsoid and UTM projection coordinates.
SUBROUTINE ConvertGeodeticToUTM &
!
(lon, lat, k, centM, lat0, a, e, eb, falseN, falseE, x, y, zone, &
 hemisphere, override)

USE Units, ONLY: &
!Imported parametes:
pi

USE StringManipulation, ONLY: &
!Imported routines:
ToString

IMPLICIT NONE

! Arguments with intent(in):
REAL (KIND = float), INTENT (IN) :: k !!scale factor
REAL (KIND = float), INTENT (IN) :: lat0 !!latitude of origin [radians]
REAL (KIND = float), INTENT (IN) :: a !! semimajor axis [m]
REAL (KIND = float), INTENT (IN) :: e !! eccentricity
REAL (KIND = float), INTENT (IN) :: eb !! second eccentricity
REAL (KIND = float), INTENT (IN) :: falseE !!false easting
REAL (KIND = float), INTENT (IN) :: override !!override option

!Arguments with intent(inout):
REAL (KIND = float), INTENT (INOUT) :: lon !!geodetic longitude [radians]
REAL (KIND = float), INTENT (INOUT) :: lat !!geodetic latitude [radians]
REAL (KIND = float), INTENT (INOUT) :: centM !!central meridian [radians]
REAL (KIND = float), INTENT (INOUT) :: falseN !!false northing
REAL (KIND = float), INTENT (INOUT) :: zone !!zone recalculated if override = 0.
REAL (KIND = float), INTENT (INOUT) :: hemisphere !!hemisphere recalculated if override = 0.

!Arguments with intent(out):
REAL (KIND = float), INTENT (OUT) :: x !!easting coordinate [m]
REAL (KIND = float), INTENT (OUT) :: y !!northing coordinate [m]

!Local variables:
INTEGER (KIND = long) :: lat_degrees
INTEGER (KIND = long) :: lon_degrees

!Local parameters:
REAL (KIND = float), PARAMETER :: MIN_LAT = -80.5 * degToRad ! -80.5 degrees in radians 
REAL (KIND = float), PARAMETER :: MAX_LAT = 84.5 * degToRad ! 84.5 degrees in radians 
REAL (KIND = float), PARAMETER :: MIN_EASTING = 100000.
REAL (KIND = float), PARAMETER :: MAX_EASTING = 900000.
REAL (KIND = float), PARAMETER :: MIN_NORTHING = 0.
REAL (KIND = float), PARAMETER :: MAX_NORTHING = 10000000.
!------------end of declaration------------------------------------------------
!check out of range
IF ( lat < MIN_LAT .OR. lat > MAX_LAT ) THEN
  CALL Catch ('error', 'GeoLib',   &
			 'Converting Geodetic to UTM: &
			 latitude out of range ' ,  &
			 code = consistencyError, &
			 argument = ToString(lat*radToDeg)//' deg' )
END IF

IF ( lon < -pi .OR. lon > 2*pi ) THEN
  CALL Catch ('error', 'GeoLib',   &
			 'Converting Geodetic to UTM: &
			 longitude out of range ' ,  &
			 code = consistencyError, &
			 argument = ToString(lon*radToDeg)//' deg' )
END IF

IF ( lat > -1.0e-9 .AND. lat < 0. ) THEN
  lat = 0.0
END IF

IF ( lon < 0. ) THEN
  lon = lon + 2.*pi + 1.0e-10
END IF

!calculate latitude and longitude in degrees
lat_degrees = INT (lat * radToDeg)
lon_degrees = INT (lon * radToDeg)

!If override is off, recalculate zone and centM
IF ( override == 0. ) THEN
  IF (lon < pi) THEN
    zone = INT ( (31. + ((lon * radToDeg) / 6.0)) )
  ELSE
    zone = INT ( ( (lon * radToDeg) / 6.0 - 29.) )
  END IF
  
  IF (zone > 60.) THEN
      zone = 1.
  END IF
  
  !UTM special cases 
  IF ( (lat_degrees > 55) .AND. (lat_degrees < 64) .AND. &
       (lon_degrees > -1) .AND. (lon_degrees < 3) ) THEN
      zone = 31.
  END IF
  IF ( (lat_degrees > 55) .AND. (lat_degrees < 64) .AND. &
       (lon_degrees > 2) .AND. (lon_degrees < 12) ) THEN
      zone = 32.
  END IF
  IF ( (lat_degrees > 71) .AND. (lon_degrees > -1) .AND. &
       (lon_degrees < 9) ) THEN
      zone = 31.
  END IF
  IF ( (lat_degrees > 71) .AND. (lon_degrees > 8) .AND. &
       (lon_degrees < 21) ) THEN
      zone = 33.
  END IF
  IF ( (lat_degrees > 71) .AND. (lon_degrees > 20) .AND. &
       (lon_degrees < 33) ) THEN
      zone = 35.
  END IF
  IF ( (lat_degrees > 71) .AND. (lon_degrees > 32) .AND. &
       (lon_degrees < 42) ) THEN
      zone = 37.
  END IF
          
END IF

IF (zone >= 31.) THEN
   centM = (6. * zone - 183.) * degToRad
ELSE
   centM = (6. * zone + 177.) * degToRad
END IF

IF (lat < 0.) THEN
  falseN = 10000000.
  hemisphere = SOUTH
ELSE
  falseN = 0.
  hemisphere = NORTH
END IF


CALL ConvertGeodeticToTransverseMercator (lon, lat, k, centM, lat0, a, e, eb, &
                                          falseN, falseE, x, y)
                                          
!Check out of range if override is off
IF ( override == 0. ) THEN
    IF ( x < MIN_EASTING .OR. x > MAX_EASTING ) THEN
      CALL Catch ('error', 'GeoLib',   &
			      'Converting Geodetic to UTM: &
			       easting out of range' ,  &
			       code = consistencyError, argument = ToString(x) )
    END IF
    IF ( y < MIN_NORTHING .OR. y > MAX_NORTHING ) THEN
      CALL Catch ('error', 'GeoLib',   &
			      'Converting Geodetic to UTM: &
			       northing out of range' ,  &
			       code = consistencyError, argument = ToString(y) )
    END IF
END IF
      
END SUBROUTINE ConvertGeodeticToUTM

!==============================================================================
!! Description:
!!   Set parameters for Gauss Boaga reference system
SUBROUTINE SetGaussBoagaparametersSystem &
!
(system, fuse, override)

IMPLICIT NONE

!Arguments with intent (in):
INTEGER (KIND = short), INTENT (IN) :: fuse
INTEGER (KIND = short), OPTIONAL, INTENT (IN) :: override

! Arguments with intent (inout):
TYPE (CRS), INTENT (INOUT) :: system

!------------end of declaration------------------------------------------------

!Esistono due proiezioni distinte: fuso Ovest e fuso Est, che differiscono per la scelta dei meridiani di riferimento. Essi sono posti rispettivamente a 9� e a 15� ad Est di Greenwich. Ciascuna proiezione copre una zona di longitudine ampia 6�, separate dal meridiano posto a 12�.
!Le coordinate si esprimono in metri. Per evitare l'utilizzo di numeri negativi per la longitudine si impone al meridiano centrale del fuso Ovest una coordinata x pari a 1500000 (invece di zero), detta anche falso Est. Al meridiano centrale del fuso Est si impone invece un falso Est di 2520000. In questo modo la prima cifra della latitudine indica a quale fuso facciamo riferimento: cifra 1 per il fuso Ovest, cifra 2 per il fuso Est.

!set Gauss Boaga parameters value
system % param (GB_lat0)  = 0.
system % param (GB_fuse)  = fuse

IF ( system % param (GB_fuse) == WEST ) THEN
  system % param (GB_centM) = 9. * degToRad  
  system % param (GB_false_easting) = 1500000.
ELSE
  system % param (GB_centM) = 15. * degToRad  
  system % param (GB_false_easting) = 2520000.
END IF

system % param (GB_false_northing) = 0.

system % param (GB_scale_factor) = 0.9996
IF (PRESENT (override) ) THEN
  system % param (GB_override) = override
ELSE
  !set default to 0 = override off
  system % param (GB_override) = 0. 
END IF

!Set parameters description
system % description (GB_lat0) = 'latitude_of_projection_origin'
system % description (GB_centM) = 'central_meridian' ! 'longitude_of_projection_origin'
system % description (GB_fuse) = 'fuse'
system % description (GB_false_easting) = 'false_easting'
system % description (GB_false_northing) = 'false_northing'
system % description (GB_scale_factor) = 'scale_factor'

END SUBROUTINE SetGaussBoagaparametersSystem


!==============================================================================
!! Description:
!!   Set parameters for Gauss Boaga reference system
SUBROUTINE SetGaussBoagaparametersCoord &
!
(coord, fuse, override)

IMPLICIT NONE

!Arguments with intent (in):
INTEGER (KIND = short), INTENT (IN) :: fuse
INTEGER (KIND = short), OPTIONAL, INTENT (IN) :: override

! Arguments with intent (inout):
TYPE (Coordinate), INTENT (INOUT) :: coord

!------------end of declaration------------------------------------------------

IF (PRESENT (override) ) THEN
  CALL SetGaussBoagaParametersSystem (coord % system, fuse, override)
ELSE
  CALL SetGaussBoagaParametersSystem (coord % system, fuse)
END IF

END SUBROUTINE SetGaussBoagaparametersCoord

!==============================================================================
!! Description:
!!   The subroutine  converts Gauss Boaga projection for Italy
!!   (easting and northing) coordinates to geodetic (latitude and longitude)
!!   coordinates
SUBROUTINE ConvertGaussBoagaToGeodetic &
!
(x, y, k, centM, lat0, a, e, eb, falseN, falseE, lon, lat)

USE Units, ONLY: &
!Imported parametes:
pi

USE StringManipulation, ONLY: &
!Imported routines:
ToString

IMPLICIT NONE

!Arguments with intent(in):
REAL (KIND = float), INTENT (IN) :: x !!easting coordinate [m]
REAL (KIND = float), INTENT (IN) :: y !!northing coordinate [m]
REAL (KIND = float), INTENT (IN) :: k !!scale factor
REAL (KIND = float), INTENT (IN) :: centM !!central meridian [radians]
REAL (KIND = float), INTENT (IN) :: lat0 !!latitude of origin [radians]
REAL (KIND = float), INTENT (IN) :: a !! semimajor axis [m]
REAL (KIND = float), INTENT (IN) :: e !! eccentricity
REAL (KIND = float), INTENT (IN) :: eb !! second eccentricity
REAL (KIND = float), INTENT (IN) :: falseN !!false northing
REAL (KIND = float), INTENT (IN) :: falseE !!false easting


!Arguments with intent (out):
REAL (KIND = float), INTENT (OUT) :: lon !!geodetic longitude [radians]
REAL (KIND = float), INTENT (OUT) :: lat !!geodetic latitude [radians]

!Local parameters:
REAL (KIND = float), PARAMETER :: MIN_LAT = -80.5 * degToRad ! -80.5 degrees in radians 
REAL (KIND = float), PARAMETER :: MAX_LAT = 84.5 * degToRad ! 84.5 degrees in radians 
REAL (KIND = float), PARAMETER :: MIN_NORTHING = 0.
REAL (KIND = float), PARAMETER :: MAX_NORTHING = 10000000.


!------------end of declaration------------------------------------------------

!Check out of range
IF ( y < MIN_NORTHING .OR. y > MAX_NORTHING ) THEN
  CALL Catch ('error', 'GeoLib',   &
			  'Converting Gauss Boaga to Geodetic: &
			   northing out of range' ,  &
			   code = consistencyError, argument = ToString(y) )
END IF

CALL ConvertTransverseMercatorToGeodetic (x, y, k, centM, lat0, a, e, eb, &
                                          falseN, falseE, lon, lat)
   
IF ( lat < MIN_LAT .OR. lat > MAX_LAT ) THEN
  CALL Catch ('error', 'GeoLib',   &
			 'Converting Gauss Boaga to Geodetic: &
			 latitude out of range ' ,  &
			 code = consistencyError, &
			 argument = ToString(lat*radToDeg)//' deg' )
END IF                                          

END SUBROUTINE ConvertGaussBoagaToGeodetic

!==============================================================================
!! Description:
!!   The subroutine  converts geodetic(latitude and longitude) coordinates 
!!   to Gauss Boaga (easting and northing) coordinates, 
!!   according to ROME40 datum for Italy (MonteMario) 
!!   coordinates.  
SUBROUTINE ConvertGeodeticToGaussBoaga &
!
(lon, lat, k, centM, lat0, a, e, eb, falseN, falseE, x, y)

USE Units, ONLY: &
!Imported parametes:
pi

USE StringManipulation, ONLY: &
!Imported routines:
ToString


IMPLICIT NONE

! Arguments with intent(in):
REAL (KIND = float), INTENT (INOUT) :: lon !!geodetic longitude [radians]
REAL (KIND = float), INTENT (INOUT) :: lat !!geodetic latitude [radians]
REAL (KIND = float), INTENT (IN) :: k !!scale factor
REAL (KIND = float), INTENT (IN) :: centM !!central meridian [radians]
REAL (KIND = float), INTENT (IN) :: lat0 !!latitude of origin [radians]
REAL (KIND = float), INTENT (IN) :: a !! semimajor axis [m]
REAL (KIND = float), INTENT (IN) :: e !! eccentricity
REAL (KIND = float), INTENT (IN) :: eb !! second eccentricity
REAL (KIND = float), INTENT (IN) :: falseN !!false northing
REAL (KIND = float), INTENT (IN) :: falseE !!false easting

!Arguments with intent(out):
REAL (KIND = float), INTENT (OUT) :: x !!easting coordinate [m]
REAL (KIND = float), INTENT (OUT) :: y !!northing coordinate [m]

!Local variables:
REAL (KIND = float), PARAMETER :: MIN_LAT = -80.5 * degToRad ! -80.5 degrees in radians 
REAL (KIND = float), PARAMETER :: MAX_LAT = 84.5 * degToRad ! 84.5 degrees in radians 
REAL (KIND = float), PARAMETER :: MIN_NORTHING = 0.
REAL (KIND = float), PARAMETER :: MAX_NORTHING = 10000000.

!------------end of declaration------------------------------------------------
!check out of range
IF ( lat < MIN_LAT .OR. lat > MAX_LAT ) THEN
  CALL Catch ('error', 'GeoLib',   &
			 'Converting Geodetic to Gauss Boaga: &
			 latitude out of range ' ,  &
			 code = consistencyError, &
			 argument = ToString(lat*radToDeg)//' deg' )
END IF


IF ( lon < -pi .OR. lon > 2*pi ) THEN
  CALL Catch ('error', 'GeoLib',   &
			 'Converting Geodetic to Gauss Boaga: &
			 longitude out of range ' ,  &
			 code = consistencyError, &
			 argument = ToString(lon*radToDeg)//' deg' )
END IF

IF ( lat > -1.0e-9 .AND. lat < 0. ) THEN
  lat = 0.0
END IF

IF ( lon < 0. ) THEN
  lon = lon + 2.*pi + 1.0e-10
END IF

CALL ConvertGeodeticToTransverseMercator (lon, lat, k, centM, lat0, a, e, eb, &
                                          falseN, falseE, x, y)

!Check out of range
IF ( y < MIN_NORTHING .OR. y > MAX_NORTHING ) THEN
  CALL Catch ('error', 'GeoLib',   &
			  'Converting Geodetic to Gauss Boaga: &
			   northing out of range' ,  &
			   code = consistencyError, argument = ToString(y) )
END IF


END SUBROUTINE ConvertGeodeticToGaussBoaga

!==============================================================================
!! Description:
!!   Set parameters for Hotine Oblique Mercator reference system
SUBROUTINE SetHotineParametersSystem &
!
(system, lat0, centM, azimuth, falseE, falseN, k)

IMPLICIT NONE

!Arguments with intent (in):
REAL (KIND = float), INTENT (IN) :: lat0 !!Latitude of projection center [rad]
REAL (KIND = float), INTENT (IN) :: centM !!Longitude of projection center [rad]
REAL (KIND = float), INTENT (IN) :: azimuth !!azimuth of centerline
REAL (KIND = float), INTENT (IN) :: falseE !!Easting/X at the center of the projection
REAL (KIND = float), INTENT (IN) :: falseN !!Northing/Y at the center of the projection 
REAL (KIND = float), INTENT (IN) :: k !!scale factor
! Arguments with intent (inout):
TYPE (CRS), INTENT (INOUT) :: system

!------------end of declaration------------------------------------------------

!set Hotine Oblique Mercator parameters value
system % param (HOM_lat0)  = lat0
system % param (HOM_centM)  = centM
system % param (HOM_azimuth) = azimuth
system % param (HOM_false_easting)  = falseE
system % param (HOM_false_northing)  = falseN
system % param (HOM_scale_factor)  = k

!set Hotine Oblique Mercator parameters description
system % description (HOM_lat0) = 'latitude_of_projection_center'
system % description (HOM_centM) = 'longitude_of_projection_center'
system % description (HOM_azimuth) = 'azimuth_of_projection_center'
system % description (HOM_false_easting) = 'false_easting'
system % description (HOM_false_northing) = 'false_northing'
system % description (HOM_scale_factor) = 'scale_factor'

END SUBROUTINE SetHotineParametersSystem


!==============================================================================
!! Description:
!!   Set parameters for Hotine Oblique Mercator reference system
SUBROUTINE SetHotineParametersCoord &
!
(coord, lat0, centM, azimuth, falseE, falseN, k)

IMPLICIT NONE

!Arguments with intent (in):
REAL (KIND = float), INTENT (IN) :: lat0 !!Latitude of projection center [rad]
REAL (KIND = float), INTENT (IN) :: centM !!Longitude of projection center [rad]
REAL (KIND = float), INTENT (IN) :: azimuth !!azimuth of centerline
REAL (KIND = float), INTENT (IN) :: falseE !!Easting/X at the center of the projection
REAL (KIND = float), INTENT (IN) :: falseN !!Northing/Y at the center of the projection 
REAL (KIND = float), INTENT (IN) :: k !!scale factor
! Arguments with intent (inout):
TYPE (Coordinate), INTENT (INOUT) :: coord

!------------end of declaration------------------------------------------------

!set Hotine Oblique Mercator parameters value

CALL SetHotineParametersSystem (coord % system, lat0, centM, &
                                azimuth, falseE, falseN, k)

END SUBROUTINE SetHotineParametersCoord


!==============================================================================
!! Description:
!!   The subroutine  converts Hotine Oblique Mercator projection 
!!   (easting and northing) coordinates to geodetic (latitude and longitude)
!!   coordinates  
SUBROUTINE ConvertHotineToGeodetic &
!
(x, y, k, lon0, lat0, azimuth, a, e, falseN, falseE, lon, lat)

USE Units, ONLY: &
!Imported parametes:
pi

USE StringManipulation, ONLY: &
!Imported routines:
ToString


IMPLICIT NONE

!Arguments with intent(in):
REAL (KIND = float), INTENT (IN) :: x !!easting coordinate [m]
REAL (KIND = float), INTENT (IN) :: y !!northing coordinate [m]
REAL (KIND = float), INTENT (IN) :: k !!scale factor
REAL (KIND = float), INTENT (IN) :: lon0 !!longitude of center [radians]
REAL (KIND = float), INTENT (IN) :: lat0 !!latitude of center [radians]
REAL (KIND = float), INTENT (IN) :: azimuth !!azimuth of centerline
REAL (KIND = float), INTENT (IN) :: a !! semimajor axis [m]
REAL (KIND = float), INTENT (IN) :: e !! eccentricity
REAL (KIND = float), INTENT (IN) :: falseN !!false northing
REAL (KIND = float), INTENT (IN) :: falseE !!false easting


!Arguments with intent (out):
REAL (KIND = float), INTENT (OUT) :: lon !!geodetic longitude [radians]
REAL (KIND = float), INTENT (OUT) :: lat !!geodetic latitude [radians]

!Local declarations:
REAL (KIND = float) :: e2, e4, e6, e8
REAL (KIND = float) :: B
REAL (KIND = float) :: B1
REAL (KIND = float) :: t0
REAL (KIND = float) :: D
REAL (KIND = float) :: D2
REAL (KIND = float) :: F
REAL (KIND = float) :: H
REAL (KIND = float) :: G
REAL (KIND = float) :: g0
REAL (KIND = float) :: l0
REAL (KIND = float) :: uc
REAL (KIND = float) :: gc
REAL (KIND = float) :: u
REAL (KIND = float) :: v
REAL (KIND = float) :: Q
REAL (KIND = float) :: S
REAL (KIND = float) :: TT
REAL (KIND = float) :: UU
REAL (KIND = float) :: VV
REAL (KIND = float) :: t
REAL (KIND = float) :: c

!------------end of declaration------------------------------------------------

!Eccentricities squared
e2 = e**2.
e4 = e2**2.
e6 = e**6.
e8 = e**8.

!calculate constants
B = ( 1. + e2 * (COS (lat0))**4. / (1. - e2) )**0.5
B1 = a * B * k * (1. - e2)**0.5 / ( 1. - e2 * (SIN(lat0))**2. )
t0 = TAN(pi / 4. - lat0 / 2.) / ( (1. - e * SIN(lat0)) / (1. + e * SIN(lat0) ) ) ** (e/2.)
D = B * (1. - e2)**0.5 / ( COS(lat0) * ( 1. - e2 * (SIN(lat0))**2.)**0.5 )
D2 = D*D
IF (D < 1.) THEN
  !D = 1.
  D2 = 1.
END IF
F = D + (D2 - 1.)**0.5 * SIGN (1.,lat0)
H = F * t0**B
G = (F - 1. / F) / 2.
g0 = ASIN(SIN (azimuth) / D)
l0 = lon0 - (ASIN(G * TAN(g0))) / B
uc = (B1 / B) * ATAN( (D2 - 1.)**0.5 / COS(azimuth) ) * SIGN (1.,lat0)

!gc = SIN(azimuth) / D
gc = azimuth
!gc = 0.927295218
!v = (y - falseE) * COS(gc) - (x - falseN) * SIN(gc)
!u = (x - falseN) * COS(gc) + (y - falseE) * SIN(gc)
v = (x - falseN) * COS(gc) - (y - falseE) * SIN(gc)
u = (y - falseE) * COS(gc) + (x - falseN) * SIN(gc)

Q = EXP (- B * v / B1)
S = (Q - 1. / Q) / 2.
TT = (Q + 1. / Q) / 2.
VV = SIN(B * u / B1)
UU = (VV * COS(gc) + S * SIN(gc)) / TT
t = (H * ((1. -  UU) / (1. + UU))**0.5)**(1./B)
c = pi / 2. - 2. * ATAN(t)
lat = c + SIN(2.*c) * ( e2 / 2. + 5 * e4 /24. + e6 / 12. + 13 * e8 / 360. ) + &
      SIN(4.*c) * ( 7. * e4 / 48. + 29. * e6 / 240. + 811. * e8 / 11520.) + &
      SIN(6.*c) * ( 7. * e6 / 120. + 81. * e8 / 1120.) + &
      SIN(8.*c) * ( 4279. * e8 / 161280.)
lon = l0 - ATAN((S * COS(gc) - VV * SIN(gc)) / COS(B * u / B1)) / B


! Check errors
IF ( ABS (lat) > 90. * degToRad ) THEN
   CALL Catch ('error', 'GeoLib',   &
    'Converting Hotine Oblique Mercator to Geodetic: &
    latitude out of range' ,  &
    code = consistencyError, argument = ToString(lat*radToDeg)//' deg' )
END IF
   
IF( lon > pi ) THEN
  lon = lon - (2. * pi)  
       
ELSE IF (lon < -pi ) THEN
  lon = lon + (2. * pi)
END IF

IF( ABS (lon) > pi ) THEN
     CALL Catch ('error', 'GeoLib',   &
     'Converting Hotine Transverse Mercator to Geodetic: &
     longitude out of range' ,  &
     code = consistencyError, argument = ToString(lon*radToDeg)//' deg' )
END IF

END SUBROUTINE ConvertHotineToGeodetic



!==============================================================================
!! Description:
!!   The subroutine  converts geodetic (latitude and longitude)
!!   coordinates  to Hotine Oblique Mercator projection 
!!   (easting and northing)
SUBROUTINE ConvertGeodeticToHotine &
!
(lon, lat, k, lon0, lat0, azimuth, a, e, falseN, falseE, x, y)

USE Units, ONLY: &
!Imported parametes:
pi

USE StringManipulation, ONLY: &
!Imported routines:
ToString


IMPLICIT NONE

!Arguments with intent(in):
REAL (KIND = float), INTENT (IN) :: lon !!geodetic longitude [radians]
REAL (KIND = float), INTENT (IN) :: lat !!geodetic latitude [radians]
REAL (KIND = float), INTENT (IN) :: k !!scale factor
REAL (KIND = float), INTENT (IN) :: lon0 !!longitude of center [radians]
REAL (KIND = float), INTENT (IN) :: lat0 !!latitude of center [radians]
REAL (KIND = float), INTENT (IN) :: azimuth !!azimuth of centerline
REAL (KIND = float), INTENT (IN) :: a !! semimajor axis [m]
REAL (KIND = float), INTENT (IN) :: e !! eccentricity
REAL (KIND = float), INTENT (IN) :: falseN !!false northing
REAL (KIND = float), INTENT (IN) :: falseE !!false easting


!Arguments with intent (out):
REAL (KIND = float), INTENT (OUT) :: x !!easting coordinate [m]
REAL (KIND = float), INTENT (OUT) :: y !!northing coordinate [m]

!Local declarations:
REAL (KIND = float) :: e2, e4, e6, e8
REAL (KIND = float) :: B
REAL (KIND = float) :: B1
REAL (KIND = float) :: t0
REAL (KIND = float) :: D
REAL (KIND = float) :: D2
REAL (KIND = float) :: F
REAL (KIND = float) :: H
REAL (KIND = float) :: G
REAL (KIND = float) :: g0
REAL (KIND = float) :: l0
REAL (KIND = float) :: uc
REAL (KIND = float) :: gc
REAL (KIND = float) :: u
REAL (KIND = float) :: v
REAL (KIND = float) :: Q
REAL (KIND = float) :: S
REAL (KIND = float) :: TT
REAL (KIND = float) :: UU
REAL (KIND = float) :: VV
REAL (KIND = float) :: t
REAL (KIND = float) :: c

!------------end of declaration------------------------------------------------

!Eccentricities squared
e2 = e**2.
e4 = e2**2.
e6 = e**6.
e8 = e**8.

!calculate constants
B = ( 1. + e2 * (COS (lat0))**4. / (1. - e2) )**0.5

B1 = a * B * k * (1. - e2)**0.5 / ( 1. - e2 * (SIN(lat0))**2. )

t0 = TAN(pi / 4. - lat0 / 2.) / ( (1. - e * SIN(lat0)) / (1. + e * SIN(lat0) ) ) ** (e/2.)
D = B * (1. - e2)**0.5 / ( COS(lat0) * ( 1. - e2 * (SIN(lat0))**2.)**0.5 )
D2 = D*D
IF (D < 1.) THEN
  !D = 1.
  D2 = 1.
END IF
F = D + (D2 - 1.)**0.5 * SIGN (1.,lat0)
H = F * t0**B
G = (F - 1. / F) / 2.
g0 = ASIN(SIN (azimuth) / D)
l0 = lon0 - (ASIN(G * TAN(g0))) / B
!uc = (B1 / B) * ATAN( (D2 - 1.)**0.5 / COS(azimuth) ) * SIGN (1.,lat0)

!gc = SIN(azimuth) / D
gc = azimuth

t = TAN(pi /4. - lat / 2.) / ( (1. - e * SIN(lat)) / (1. + e * SIN(lat)) )**(e/2.)
Q = H / t**B
S = (Q - 1. / Q) / 2.
TT = (Q + 1. / Q) / 2.
VV = SIN(B * (lon - l0))
UU = (-VV * COS(g0) + S * SIN(g0)) / TT
v = B1 * LOG( (1. - UU) / (1. + UU) ) / (2. * B)
u = B1 * ATAN2( (S * COS(g0) + VV * SIN(g0)) , COS( B * (lon - l0)) ) / B

x = v * COS(gc) + u * SIN(gc) + falseE
y = u * COS(gc) - v * SIN(gc) + falseN

END SUBROUTINE ConvertGeodeticToHotine


!==============================================================================
!! Description:
!!   Set parameters for Swiss Oblique Cylindrical reference system
SUBROUTINE SetSwissParametersSystem &
!
(system, latc, lonc, azimuth, falseE, falseN, k)

IMPLICIT NONE

!Arguments with intent (in):
REAL (KIND = float), INTENT (IN) :: latc !!Latitude of projection center [rad]
REAL (KIND = float), INTENT (IN) :: lonc !!Longitude of projection center [rad]
REAL (KIND = float), INTENT (IN) :: azimuth !!azimuth of centerline
REAL (KIND = float), INTENT (IN) :: falseE !!Easting/X at the center of the projection
REAL (KIND = float), INTENT (IN) :: falseN !!Northing/Y at the center of the projection 
REAL (KIND = float), INTENT (IN) :: k !!scale factor
! Arguments with intent (inout):
TYPE (CRS), INTENT (INOUT) :: system

!------------end of declaration------------------------------------------------

!set Swiss Oblique Cylindrical parameters value
system % param (SOC_latc)  = latc
system % param (SOC_lonc)  = lonc
system % param (SOC_azimuth) = azimuth
system % param (SOC_false_easting)  = falseE
system % param (SOC_false_northing)  = falseN
system % param (SOC_scale_factor)  = k

!set Swiss Oblique Cylindrical parameters description
system % description (SOC_latc) = 'latitude_of_projection_center'
system % description (SOC_lonc) = 'longitude_of_projection_center'
system % description (SOC_azimuth) = 'azimuth_of_projection_center'
system % description (SOC_false_easting) = 'false_easting'
system % description (SOC_false_northing) = 'false_northing'
system % description (SOC_scale_factor) = 'scale_factor'

END SUBROUTINE SetSwissParametersSystem


!==============================================================================
!! Description:
!!   Set parameters for Swiss Oblique Cylindrical reference system
SUBROUTINE SetSwissParametersCoord &
!
(coord, latc, lonc, azimuth, falseE, falseN, k)

IMPLICIT NONE

!Arguments with intent (in):
REAL (KIND = float), INTENT (IN) :: latc !!Latitude of projection center [rad]
REAL (KIND = float), INTENT (IN) :: lonc !!Longitude of projection center [rad]
REAL (KIND = float), INTENT (IN) :: azimuth !!azimuth of centerline
REAL (KIND = float), INTENT (IN) :: falseE !!Easting/X at the center of the projection
REAL (KIND = float), INTENT (IN) :: falseN !!Northing/Y at the center of the projection 
REAL (KIND = float), INTENT (IN) :: k !!scale factor
! Arguments with intent (inout):
TYPE (Coordinate), INTENT (INOUT) :: coord

!------------end of declaration------------------------------------------------

!set Hotine Oblique Mercator parameters value

CALL SetHotineParametersSystem (coord % system, latc, lonc, &
                                azimuth, falseE, falseN, k)

END SUBROUTINE SetSwissParametersCoord


!==============================================================================
!! Description:
!!   The subroutine  converts Swiss Oblique Cylindrical projection 
!!   (easting and northing) coordinates to geodetic (latitude and longitude)
!!   coordinates  
!! Reference:
!!    Federal Office of Topography, Formulas and constants for the calculation
!!    of the Swiss conformal cylindrical projection and for the transormation
!!    between coordinate systems
!!    http://www.swisstopo.admin.ch/internet/swisstopo/en/home/topics/survey/sys/refsys.html
SUBROUTINE ConvertSwissToGeodetic &
!
(x, y, k, lon0, lat0, azimuth, a, e, falseN, falseE, lon, lat)

USE Units, ONLY: &
!Imported parametes:
pi

USE StringManipulation, ONLY: &
!Imported routines:
ToString


IMPLICIT NONE

!Arguments with intent(in):
REAL (KIND = float), INTENT (IN) :: x !!easting coordinate [m]
REAL (KIND = float), INTENT (IN) :: y !!northing coordinate [m]
REAL (KIND = float), INTENT (IN) :: k !!scale factor
REAL (KIND = float), INTENT (IN) :: lon0 !!longitude of center [radians]
REAL (KIND = float), INTENT (IN) :: lat0 !!latitude of center [radians]
REAL (KIND = float), INTENT (IN) :: azimuth !!azimuth of centerline
REAL (KIND = float), INTENT (IN) :: a !! semimajor axis [m]
REAL (KIND = float), INTENT (IN) :: e !! eccentricity
REAL (KIND = float), INTENT (IN) :: falseN !!false northing
REAL (KIND = float), INTENT (IN) :: falseE !!false easting


!Arguments with intent (out):
REAL (KIND = float), INTENT (OUT) :: lon !!geodetic longitude [radians]
REAL (KIND = float), INTENT (OUT) :: lat !!geodetic latitude [radians]

!Local declarations:
REAL (KIND = float) :: e2
REAL (KIND = float) :: R
REAL (KIND = float) :: alpha
REAL (KIND = float) :: b0
REAL (KIND = float) :: K0
REAL (KIND = float) :: xbig, ybig
REAL (KIND = float) :: l1
REAL (KIND = float) :: b1
REAL (KIND = float) :: l
REAL (KIND = float) :: b
REAL (KIND = float) :: S
INTEGER (KIND = short) :: i



!------------end of declaration------------------------------------------------

!eccentricity squared
e2 = e * e

!Radius of the projection sphere
R = a * (1. - e2)**0.5 / (1. - e2 * (SIN(lat0))**2.)

!Relat. between longitude on sphere and on ellipsoid
alpha = ( 1. + e2 / (1. - e2) * COS(lat0)**4. )**0.5

!Latitude of the fundamental point on the sphere
b0 = ASIN( SIN(lat0)/alpha )

!Constant of the latitude
K0 = LOG(TAN(pi/4. + b0/2.)) - alpha * LOG(TAN(pi/4. + lat0/2.)) + &
     alpha * e / 2. * LOG( (1. + e * SIN(lat0)) / (1. - e * SIN(lat0)) )

!convert to sphere
xbig = x - falseE
ybig = y - falseN

l1 = xbig / R

b1 = 2. * ( ATAN(EXP(ybig/R)) - pi/4.)

!pseudo-equator system -> equator system
b = ASIN( COS(b0) * SIN(b1) + SIn(b0) * COS (b1) * COS(l1) )
l = ATAN( SIN(l1) / (COS(b0) * COS(l1) - SIN(b0) * TAN(b1)) )

!sphere -> ellipsoid
lon = lon0 + l / alpha

lat = b
DO i = 1,6
  S = ( LOG(TAN(pi/4. + b/2.)) - K0 ) / alpha + e * &
       LOG(TAN(pi/4. + ASIN(e * SIN(lat))/2.))
  !S = LOG(TAN(pi/4. + lat/2.))
  lat = 2 * ATAN(EXP(S)) - pi/2.
END DO

! Check errors
IF ( ABS (lat) > 90. * degToRad ) THEN
   CALL Catch ('error', 'GeoLib',   &
    'Converting Hotine Oblique Mercator to Geodetic: &
    latitude out of range' ,  &
    code = consistencyError, argument = ToString(lat*radToDeg)//' deg' )
END IF
   
IF( lon > pi ) THEN
  lon = lon - (2. * pi)  
       
ELSE IF (lon < -pi ) THEN
  lon = lon + (2. * pi)
END IF

IF( ABS (lon) > pi ) THEN
     CALL Catch ('error', 'GeoLib',   &
     'Converting Hotine Transverse Mercator to Geodetic: &
     longitude out of range' ,  &
     code = consistencyError, argument = ToString(lon*radToDeg)//' deg' )
END IF

END SUBROUTINE ConvertSwissToGeodetic



!==============================================================================
!! Description:
!!   The subroutine  converts geodetic (latitude and longitude) to 
!!   Swiss Oblique Cylindrical projection(easting and northing) coordinates   
!! Reference:
!!    Federal Office of Topography, Formulas and constants for the calculation
!!    of the Swiss conformal cylindrical projection and for the transormation
!!    between coordinate systems
!!    http://www.swisstopo.admin.ch/internet/swisstopo/en/home/topics/survey/sys/refsys.html
SUBROUTINE ConvertGeodeticToSwiss &
!!
(lon, lat, k, lon0, lat0, azimuth, a, e, falseN, falseE, x, y)

USE Units, ONLY: &
!Imported parametes:
pi

USE StringManipulation, ONLY: &
!Imported routines:
ToString


IMPLICIT NONE

!Arguments with intent(in):
REAL (KIND = float), INTENT (IN) :: lon !!geodetic longitude [radians]
REAL (KIND = float), INTENT (IN) :: lat !!geodetic latitude [radians]
REAL (KIND = float), INTENT (IN) :: k !!scale factor
REAL (KIND = float), INTENT (IN) :: lon0 !!longitude of center [radians]
REAL (KIND = float), INTENT (IN) :: lat0 !!latitude of center [radians]
REAL (KIND = float), INTENT (IN) :: azimuth !!azimuth of centerline
REAL (KIND = float), INTENT (IN) :: a !! semimajor axis [m]
REAL (KIND = float), INTENT (IN) :: e !! eccentricity
REAL (KIND = float), INTENT (IN) :: falseN !!false northing
REAL (KIND = float), INTENT (IN) :: falseE !!false easting


!Arguments with intent (out):
REAL (KIND = float), INTENT (OUT) :: x !!easting coordinate [m]
REAL (KIND = float), INTENT (OUT) :: y !!northing coordinate [m]

!Local declarations:
REAL (KIND = float) :: e2
REAL (KIND = float) :: R
REAL (KIND = float) :: alpha
REAL (KIND = float) :: b0
REAL (KIND = float) :: K0
REAL (KIND = float) :: xbig, ybig
REAL (KIND = float) :: l1
REAL (KIND = float) :: b1
REAL (KIND = float) :: l
REAL (KIND = float) :: b
REAL (KIND = float) :: S
INTEGER (KIND = short) :: i

!------------end of declaration------------------------------------------------

!eccentricity squared
e2 = e * e

!Radius of the projection sphere
R = a * (1. - e2)**0.5 / (1. - e2 * (SIN(lat0))**2.)

!Relat. between longitude on sphere and on ellipsoid
alpha = ( 1. + e2 / (1. - e2) * COS(lat0)**4. )**0.5

!Latitude of the fundamental point on the sphere
b0 = ASIN( SIN(lat0)/alpha )

!Constant of the latitude
K0 = LOG(TAN(pi/4. + b0/2.)) - alpha * LOG(TAN(pi/4. + lat0/2.)) + &
     alpha * e / 2. * LOG( (1. + e * SIN(lat0)) / (1. - e * SIN(lat0)) )


!ellipsoid to sphere
!auxiliary value
S = alpha * LOG(TAN(pi/4. + lat/2.)) - alpha * e / 2. * &
    LOG( (1. + e * SIN(lat)) / (1. - e * SIN(lat)) ) + K0
!spherical latitude
b = 2. * ( ATAN(EXP(S)) - pi/4. )
!spherical longitude
l = alpha * (lon - lon0)

!equator system -> pseudo-equator system
l1 = ATAN( SIN(l) / (SIN(b0) * TAN(b) + COS(b0) * COS(l)) )

b1 = ASIN(COS(b0) * SIN(b) - SIn(b0) * COS(b) * COS(l))

!sphere -> projection plane
xbig = R * l1
ybig = R / 2. * LOG((1. + SIN(b1)) / (1. - SIN(b1)))

x = xbig + falseE
y = ybig + falseN


END SUBROUTINE ConvertGeodeticToSwiss





!==============================================================================
!! Description:
!!   Compute the radius of curvature of the ellipsoid in the plane of 
!!   the meridian at given latitude 
FUNCTION Rho &
!
(lat, a, e) &
!
RESULT (radius)

IMPLICIT NONE

!Arguments with intent(in):
REAL (KIND = float), INTENT (IN) :: lat !!latitude [radians]
REAL (KIND = float), INTENT (IN) :: a !!semimajor axis [m]
REAL (KIND = float), INTENT (IN) :: e !!exentricity

!Local variables:
REAL (KIND = float) :: radius

!------------end of declaration------------------------------------------------

radius = a * (1. - e**2.) / (1. - e**2. * SIN(lat)**2) ** 1.5

END FUNCTION Rho


!==============================================================================
!! Description:
!!   Compute the radius of curvature of the ellipsoid perpendicular to 
!!   the meridian at given latitude 
FUNCTION Nu &
!
(lat, a, e) &
!
RESULT (radius)

IMPLICIT NONE

!Arguments with intent(in):
REAL (KIND = float), INTENT (IN) :: lat !!latitude [radians]
REAL (KIND = float), INTENT (IN) :: a !!semimajor axis [m]
REAL (KIND = float), INTENT (IN) :: e !!eccentricity

!Local variables:
REAL (KIND = float) :: radius

!------------end of declaration------------------------------------------------

radius = a / (1. - e**2. * SIN(lat)**2) ** 0.5

END FUNCTION Nu

!==============================================================================
!! Description:
!!   Compute the distance in meters along a meridian from equator 
!!   to given latitude.
!! References:
!!   Snyder, J.P, (1987). Map Projections - A working manual, 
!!   U.S. Geological Survey Professional Paper 1395.
FUNCTION MeridionalDistance &
!
(lat, a, e) &
!
RESULT (M)

IMPLICIT NONE

!Arguments with intent(in):
REAL (KIND = float), INTENT(IN) :: lat !!latitude in [radians]
REAL (KIND = float), INTENT(IN) :: a !! semimajor axis [m]
REAL (KIND = float), INTENT(IN) :: e !! exentricity

!Local variables:
REAL (KIND = float) :: M !!meridional distance [m]
!------------end of declaration------------------------------------------------
M = a * ( ( 1. - e**2. / 4. - 3 * e**4. / 64. - 5 * e**6. / 256. ) * lat - &
    ( 3 * e**2. / 8. + 3 * e**4. / 32. + 45 * e**6. / 1024. ) * SIN(2. * lat) + & 
    ( 15. * e**4. / 256. + 45. * e**6. / 1024. ) * SIN(4. * lat ) - &
    ( 35. * e**6. / 3072. ) * SIN(6. * lat) )

END FUNCTION MeridionalDistance

!==============================================================================
!! Description:
!!   shifts geodetic coordinates relative to a given source datum
!!   to geodetic coordinates relative to WGS84.
SUBROUTINE GeodeticShiftToWGS84 &
!
(input, latIn, lonIn, WGS84lat, WGS84lon)

IMPLICIT NONE

! Arguments with intent(in):
TYPE (Coordinate), INTENT(IN) :: input
REAL (KIND = float), INTENT(IN) :: latIn
REAL (KIND = float), INTENT(IN) :: lonIn

! Arguments with intent(out):
REAL (KIND = float), INTENT(OUT) :: WGS84lat, WGS84lon

! Local variables:
REAL (KIND = float) :: hgtIn
REAL (KIND = float) :: WGS84_a !! Semi-major axis of WGS84 ellipsoid in meters 
REAL (KIND = float) :: WGS84_f !! Flattening of WGS84 ellisoid    
REAL (KIND = float) :: a !!Semi-major axis of ellipsoid in meters
REAL (KIND = float) :: f !!Flattening of ellipsoid
REAL (KIND = float) :: da !!Difference in semi-major axes
REAL (KIND = float) :: df !!Difference in flattening
REAL (KIND = float) :: dx
REAL (KIND = float) :: dy
REAL (KIND = float) :: dz 
REAL (KIND = float) :: WGS84hgt


! Local parameters:
REAL (KIND = float), PARAMETER :: MOLODENSKY_MAX  = 89.75 * degToRad !! Polar limit          

!------------end of declaration------------------------------------------------

!initialize
hgtIn = 0.
WGS84_a = 6378137.0 
WGS84_f = 1. / 298.257223563
a = input % system % ellipsoid % a
f = input % system % ellipsoid % f

!If input datum is WGS84, just copy to output
IF ( input % system % datum % code == WGS84 ) THEN
  WGS84lat = latIn
  WGS84lon = lonIn
  RETURN
END IF

!If latitude is within limits, apply Molodensky 
IF ( latIn <= MOLODENSKY_MAX .AND. &
     latIn >= - MOLODENSKY_MAX  ) THEN
     
   da = WGS84_a - a
   df = WGS84_f - f
   dx = input % system % datum % dx
   dy = input % system % datum % dy
   dz = input % system % datum % dz
   CALL MolodenskyShift(WGS84_a, da, WGS84_f, df, dx, dy, dz, latIn, lonIn, &
                         hgtIn, WGS84lat, WGS84lon, WGS84hgt)
     
ELSE !apply 3 steps method

  WRITE (*,*) 'Latitude is outside Molodensky limits'
  WRITE (*,*) '3-step datum transformation not yet implemented'
  STOP

END IF

END SUBROUTINE GeodeticShiftToWGS84

!==============================================================================
!! Description:
!!   shifts geodetic coordinates relative to WGS84
!!   to geodetic coordinates relative to a given local datum.
SUBROUTINE GeodeticShiftFromWGS84 &
!
(input, WGS84lat, WGS84lon, latOut, lonOut )

IMPLICIT NONE

! Arguments with intent(in):
TYPE (Coordinate), INTENT(IN) :: input
REAL (KIND = float), INTENT(IN) :: WGS84lat, WGS84lon

! Arguments with intent(out):
REAL (KIND = float), INTENT(OUT) :: latOut
REAL (KIND = float), INTENT(OUT) :: lonOut

! Local variables:
REAL (KIND = float) :: hgtOut
REAL (KIND = float) :: WGS84_a !! Semi-major axis of WGS84 ellipsoid in meters 
REAL (KIND = float) :: WGS84_f !! Flattening of WGS84 ellisoid    
REAL (KIND = float) :: a !!Semi-major axis of ellipsoid in meters
REAL (KIND = float) :: f !!Flattening of ellipsoid
REAL (KIND = float) :: da !!Difference in semi-major axes
REAL (KIND = float) :: df !!Difference in flattening
REAL (KIND = float) :: dx
REAL (KIND = float) :: dy
REAL (KIND = float) :: dz 
REAL (KIND = float) :: WGS84hgt


! Local parameters:
REAL (KIND = float), PARAMETER :: MOLODENSKY_MAX  = 89.75 * degToRad !! Polar limit          

!------------end of declaration------------------------------------------------

!initialize
WGS84hgt = 0.
WGS84_a = 6378137.0 
WGS84_f = 1. / 298.257223563
a = input % system % ellipsoid % a
f = input % system % ellipsoid % f

!If input datum is WGS84, just copy to output
IF ( input % system % datum % code == WGS84 ) THEN
  latOut = WGS84lat
  lonOut = WGS84lon
  RETURN
END IF

!If latitude is within limits, apply Molodensky 
IF ( WGS84lat <= MOLODENSKY_MAX .AND. &
     WGS84lat >= - MOLODENSKY_MAX  ) THEN
     
   da = a - WGS84_a
   df = f - WGS84_f
   dx = - input % system % datum % dx
   dy = - input % system % datum % dy
   dz = - input % system % datum % dz
   CALL MolodenskyShift(WGS84_a, da, WGS84_f, df, dx, dy, dz, WGS84lat, WGS84lon, &
                         WGS84hgt, latOut, lonOut, hgtOut)
   
                         
   !                      Molodensky_Shift(WGS84_a, da, WGS84_f, df, dx, dy, dz,
   !                            WGS84_Lat, WGS84_Lon, WGS84_Hgt, Lat_out, Lon_out, Hgt_out)
     
ELSE !apply 3 steps method

  WRITE (*,*) 'Latitude is outside Molodensky limits'
  WRITE (*,*) '3-step datum transformation not yet implemented'
  STOP

END IF


END SUBROUTINE GeodeticShiftFromWGS84


!==============================================================================
!! Description:
!!   shifts geodetic coordinates using the Molodensky method
SUBROUTINE MolodenskyShift &
!
(a, da, f, df, dx, dy, dz, Lat_in, Lon_in, &
 Hgt_in, WGS84_Lat, WGS84_Lon, WGS84_Hgt)
 
USE Units, ONLY : pi

IMPLICIT NONE

! Arguments with intent(in):
REAL (KIND = float), INTENT (IN) :: a
REAL (KIND = float), INTENT (IN) :: da
REAL (KIND = float), INTENT (IN) :: f
REAL (KIND = float), INTENT (IN) :: df
REAL (KIND = float), INTENT (IN) :: dx
REAL (KIND = float), INTENT (IN) :: dy
REAL (KIND = float), INTENT (IN) :: dz
REAL (KIND = float), INTENT (IN) :: Lat_in
REAL (KIND = float), INTENT (IN) :: Lon_in
REAL (KIND = float), INTENT (IN) :: Hgt_in

! Arguments with intent(out):
REAL (KIND = float), INTENT (OUT) :: WGS84_Lat
REAL (KIND = float), INTENT (OUT) :: WGS84_Lon
REAL (KIND = float), INTENT (OUT) :: WGS84_Hgt

! Local variables:
REAL (KIND = float) :: tLon_in   !!temp longitude 
REAL (KIND = float) :: e2        !!Intermediate calculations for dp, dl 
REAL (KIND = float) :: ep2       !!Intermediate calculations for dp, dl 
REAL (KIND = float) :: sin_Lat   !!sin(Latitude_1)       
REAL (KIND = float) :: sin2_Lat  !!(sin(Latitude_1))^2   
REAL (KIND = float) :: sin_Lon   !!sin(Longitude_1)      
REAL (KIND = float) :: cos_Lat   !!cos(Latitude_1)       
REAL (KIND = float) :: cos_Lon   !!cos(Longitude_1)      
REAL (KIND = float) :: w2        !!Intermediate calculations for dp, dl 
REAL (KIND = float) :: w         !!Intermediate calculations for dp, dl 
REAL (KIND = float) :: w3        !!Intermediate calculations for dp, dl 
REAL (KIND = float) :: m         !!Intermediate calculations for dp, dl 
REAL (KIND = float) :: n         !!Intermediate calculations for dp, dl 
REAL (KIND = float) :: dp        !!Delta phi             
REAL (KIND = float) :: dp1       !!Delta phi calculations
REAL (KIND = float) :: dp2       !!Delta phi calculations
REAL (KIND = float) :: dp3       !!Delta phi calculations
REAL (KIND = float) :: dl        !!Delta lambda  
REAL (KIND = float) :: dh        !!Delta height  
REAL (KIND = float) :: dh1       !!Delta height calculations 
REAL (KIND = float) :: dh2       !!Delta height calculations 

!------------end of declaration------------------------------------------------

IF (Lon_in > PI) THEN
   tLon_in = Lon_in - (2*PI)
ELSE
   tLon_in = Lon_in
END IF

e2 = 2 * f - f * f
ep2 = e2 / (1 - e2)
sin_Lat = SIN(Lat_in)
cos_Lat = COS(Lat_in)
sin_Lon = SIN(tLon_in)
cos_Lon = COS(tLon_in)
sin2_Lat = sin_Lat * sin_Lat
w2 = 1.0 - e2 * sin2_Lat
w = SQRT(w2)
w3 = w * w2
m = (a * (1.0 - e2)) / w3
n = a / w
dp1 = cos_Lat * dz - sin_Lat * cos_Lon * dx - sin_Lat * sin_Lon * dy
dp2 = ((e2 * sin_Lat * cos_Lat) / w) * da
dp3 = sin_Lat * cos_Lat * (2.0 * n + ep2 * m * sin2_Lat) * (1.0 - f) * df
dp = (dp1 + dp2 + dp3) / (m + Hgt_in)
dl = (-sin_Lon * dx + cos_Lon * dy) / ((n + Hgt_in) * cos_Lat)
dh1 = (cos_Lat * cos_Lon * dx) + (cos_Lat * sin_Lon * dy) + (sin_Lat * dz)
dh2 = -(w * da) + ((a * (1 - f)) / w) * sin2_Lat * df
dh = dh1 + dh2
WGS84_Lat = Lat_in + dp
WGS84_Lon = Lon_in + dl
WGS84_Hgt = Hgt_in + dh
IF (WGS84_Lon > (PI * 2)) THEN
   WGS84_Lon = WGS84_Lon - 2*PI
END IF
IF (WGS84_Lon < (- PI)) THEN
   WGS84_Lon = WGS84_Lon + 2*PI
END IF

END SUBROUTINE MolodenskyShift

!==============================================================================
!! Description:
!!  return .TRUE. if the two coordinate reference systems are equal
FUNCTION CRSisEqual &
!
(CRS1, CRS2) &
!
RESULT (isEqual)

IMPLICIT NONE

!Arguments with intent(in):
TYPE (CRS), INTENT(IN) :: CRS1, CRS2

!Local declarations:
LOGICAL :: isEqual
!------------------end of declarations-----------------------------------------

IF (CRS1 % system == CRS2 % system .AND. &
    CRS1 % ellipsoid == CRS2 % ellipsoid .AND. &
    CRS1 % datum == CRS2 % datum ) THEN
    
    isEqual = .TRUE.
ELSE
    isEqual = .FALSE.
END IF

END FUNCTION CRSisEqual

!==============================================================================
!! Description:
!!  return .TRUE. if the two ellipsoids are equal
FUNCTION EllipsoidIsEqual &
!
(ellps1, ellps2) &
!
RESULT (isEqual)

IMPLICIT NONE

!Arguments with intent(in):
TYPE (Ellipsoid), INTENT(IN) :: ellps1, ellps2

!Local declarations:
LOGICAL :: isEqual
!------------------end of declarations-----------------------------------------

IF (ellps1 % a == ellps2 % a .AND. &
    ellps1 % b == ellps2 % b .AND. &
    ellps1 % inv_f == ellps2 % inv_f ) THEN
    
    isEqual = .TRUE.
ELSE
    isEqual = .FALSE.
END IF

END FUNCTION EllipsoidIsEqual

!==============================================================================
!! Description:
!!  return .TRUE. if the two datums are equal
FUNCTION DatumIsEqual &
!
(datum1, datum2) &
!
RESULT (isEqual)

IMPLICIT NONE

!Arguments with intent(in):
TYPE (Datum), INTENT(IN) :: datum1, datum2

!Local declarations:
LOGICAL :: isEqual
!------------------end of declarations-----------------------------------------

IF (datum1 % ellipsoid == datum2 % ellipsoid .AND. &
    datum1 % dx == datum2 % dx .AND. &
    datum1 % dy == datum2 % dy .AND. &
    datum1 % dz == datum2 % dz ) THEN
    
    isEqual = .TRUE.
    
ELSE

    isEqual = .FALSE.
    
END IF

END FUNCTION DatumIsEqual


!==============================================================================
!! Description:
!!  return datum numeric code given a string
FUNCTION ScanDatum &
!
(datumString) &
!
RESULT (datumCode)

USE StringManipulation, ONLY: &
!Imported routines:
StringToUpper


IMPLICIT NONE

!Arguments with intent in:
CHARACTER (LEN = *), INTENT (IN) :: datumString

!Local variables:
INTEGER :: datumCode

!----------------------------end of declaration--------------------------------

IF (StringToUpper (datumString) == 'ED50') THEN
  datumCode = ED50
ELSE IF (StringToUpper (datumString) == 'ROME40') THEN
  datumCode = ROME40
ELSE IF (StringToUpper (datumString) == 'WGS84') THEN
  datumCode = WGS84
ELSE IF (StringToUpper (datumString) == 'CH1903') THEN
  datumCode = CH1903
END IF

END FUNCTION ScanDatum

!------------------------------------------------------------------------------
!! Description:
!!   compute distance between two points in, either, projected or geodetic
!!   coordinate reference system
FUNCTION Distance &
!
(point1, point2)

USE Units, ONLY: &
!Imported parameters:
degToRad

IMPLICIT NONE

!Arguments with intent in:
TYPE (Coordinate) , INTENT (IN):: point1,point2

!Local declarations:
REAL (KIND = double):: distance
REAL (KIND = float) :: maxnri,iconv,iter
TYPE (Ellipsoid)    :: a,b,inv_f
REAL (KIND = float) :: U1,U2,ell_a,ell_b,ell_f
REAL (KIND = float) :: LL, senSIGMA, cosSIGMA, SIGMA, senALFA
REAL (KIND = float) :: COS2ALFA,COS2SIGMAm,CC,LP,U2U,AA,BB,deltaSIGMA,LAMBDA
!----------------------end of declarations-------------------------------------

iconv=0
maxnri=100
iter=1

ell_a = point1 % system % ellipsoid % a
ell_b = point1 % system % ellipsoid % b
ell_f = 1./point1 % system % ellipsoid % inv_f 
   
SELECT CASE (point1 % system  % system)
  CASE (GEODETIC)
!    !Vincenty formula (NON FUNZIONA ALL'EQUATORE)
!    ell_a = punto1 % system % ellipsoid % a
!    ell_b = punto1 % system % ellipsoid % b
!    ell_f = 1/punto1 % system % ellipsoid % inv_f 
!    U1= atan((1-ell_f)*tan(punto1 % northing* degToRad))
!    U2= atan((1-ell_f)*tan(punto2 % northing* degToRad))
!    LAMBDA=(punto2 % easting - punto1 % easting)* degToRad !conversion to radians
!    LL=LAMBDA
!    DO WHILE(iconv==0 .AND. iter<maxnri)
!     senSIGMA=SQRT((cos(U2)*SIN(LL))**2+(COS(U1)*SIN(U2)-SIN(U1)*COS(U2)*COS(LL))**2)
!     IF(senSIGMA==0) THEN
!        write (*,*)'punti coincidenti'
!        pause
!     END IF
!     cosSIGMA=SIN(U1)*SIN(U2)+COS(U1)*COS(U2)*COS(LL)
!     SIGMA=ATAN2(senSIGMA,cosSIGMA)!SIGMA=ATAN(senSIGMA/cosSIGMA)
!     senALFA=COS(U1)*COS(U2)*SIN(LL)/senSIGMA
!     COS2ALFA=1-senALFA**2
!     COS2SIGMAm=cosSIGMA-2*SIN(U1)*SIN(U2)/COS2ALFA
!     CC=ell_f/16*COS2ALFA*(4+ell_f*(4-3*COS2ALFA))
!     LP=LL
!     LL=LAMBDA+(1-CC)*ell_f*senALFA*(SIGMA+CC*senSIGMA* &
!        (COS2SIGMAm+CC*cosSIGMA*(-1+2*COS2SIGMAm*COS2SIGMAm)))
!     IF(abs(LL-LP)<10.**(-12))THEN 
!	   iconv=1 
!     END IF
!     iter=iter+1
!    END DO
!    
!    U2U=COS2ALFA*(ell_a**2 - ell_b**2)/(ell_b**2)
!    AA=1+U2U/16384.*(4096+U2U*(-768+U2U*(320-175*U2U)))
!    BB=U2U/1024*(256+U2U*(-128+U2U*(74-47*U2U)))
!    deltaSIGMA=BB*senSIGMA*(COS2SIGMAm+BB/4.* &
!               (cosSIGMA*(1-2*COS2SIGMAm*COS2SIGMAm)-BB/6.*COS2SIGMAm* &
!               (-3+4*senSIGMA*senSIGMA)*(-3+4*COS2SIGMAm*COS2SIGMAm)))
!    distance=ell_b * AA * (SIGMA-deltaSIGMA)
!    WRITE(*,*)DISTANCE
    
    
    ! great circle distance formula (approximate solution)
    IF ((SIN(point1 % northing* degToRad) * SIN(point2 % northing* degToRad)+ &
         COS(point1 % northing* degToRad) * COS(point2 % northing* degToRad) * &
         COS(point1 % easting* degToRad - point2 % easting* degToRad)) < 1.) THEN         
         distance = 6378.7* 1000. * ACOS(SIN(point1 % northing* degToRad) * &
                    SIN(point2 % northing* degToRad)+ &
                   (COS(point1 % northing* degToRad) * COS(point2 % northing* degToRad) * &
                   COS(point2 % easting* degToRad - point1 % easting* degToRad)))
             
    ELSE
      distance=6378.7* 1000. *ACOS(0.9999999)
    END IF 
    
    
  CASE DEFAULT !projected coordinate reference system
       
    distance = SQRT((point1 % northing - point2 % northing)**2 + &
             (point1 % easting - point2 % easting)**2)
    
  END SELECT 

END  FUNCTION Distance

END MODULE GeoLib

