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
!! Motivation: 
!!   Portability refers to the ease with which source code can be moved 
!!   from machine to machine. A portable program requires little or no 
!!   change to the source code when compiled and run on a different machine. 
!!   Portability of numerical code is important for several reasons.
!!   Fortran 90 introduces several new mechanisms to aid in porting numerical 
!!   code to other machines. The most difficult problem in porting numerical 
!!   programs is in the portable selection of precision. Selecting the precision 
!!   of a computation in a portable way was impossible with Fortran 77. 
!!   However, with the introduction of kind values as well as intrinsic 
!!   environmental inquiry functions for selecting and inquiring about 
!!   precision, Fortran 90 programs should be much easier to port to other machines.
!!   Kind values are integer constants that can be used to further specify 
!!   the characteristics of an intrinsic type, such as integer or real. 
!!   For example, real( 2 ) selects a real type with kind value 2. 
!!   Unfortunately, kind values are processor-dependent and are therefore 
!!   not standardized. However, there are several portable ways to 
!!   select kind values based on the precision desired. Two such functions 
!!   are selected_int_kind and selected_real_kind
!!
!! Description: 
!!   Provide named kind parameters for use in declarations of real and integer 
!!   variables with specific byte sizes (i.e. one, two, four, and eight byte
!!   integers; four and eight byte reals). The parameters can then be used
!!   in (KIND = XX) modifiers in declarations.
!!   A single function (SizesOK()) is provided to ensure that the selected 
!!   kind parameters are correct.
!!
!! References and Credits:
!!   Adapted from netCDF library http://www.unidata.ucar.edu/software/netcdf/
!!   Written by Robert Pincus
!!
!! Design Notes:
!!   Fortran 90 doesn't allow one to check the number of bytes in a real variable;
!!   we check only that four byte and eight byte reals have different kind parameters. 
!!
!! @author <a href="mailto:giovanni.ravazzani@polimi.it">Giovanni Ravazzani</a> 
!! @version 1.0 - 4th July 2008  
MODULE DataTypeSizes        

! History: 
!  
! Version   Date          Comment 
! -------   ----          ------- 
! 1.0       4/Jul/2008    Original code. giovanni ravazzani
! 
! Code Description: 
!   Language:           Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!     Documenting Exchangeable Fortran 90 Code". 
! 

IMPLICIT NONE 

! Global (i.e. public) Declarations: 

!! 32 bits integer type
!!equivalent to FourByteInt type in netCDF library
 INTEGER, PUBLIC, PARAMETER ::   short = SELECTED_INT_KIND(9)

!! 64 bits integer type
!!equivalent to EightByteInt type in netCDF library
 INTEGER, PUBLIC, PARAMETER ::   long = SELECTED_INT_KIND(18)

!! 32 bits real type
!! floating point numbers with numerical precision at least 6  digits 
!! and one decimal exponent range between -37  and +37
!! equivalent to FourByteReal type in netCDF library                       
INTEGER, PUBLIC, PARAMETER :: float = SELECTED_REAL_KIND(P =  6, R =  37) 

!! 64 bits real type
!! floating point numbers with numerical precision at least 13  digits 
!! and one decimal exponent range between -307  and +307
!! equivalent to EightByteReal type in netCDF library (double precision)
INTEGER, PUBLIC, PARAMETER :: double = SELECTED_REAL_KIND(P = 13, R = 307) 



!=======
CONTAINS
!=======
! Define procedures contained in this module. 

!==============================================================================
!! Description:
!! Users may call this function once to ensure that the kind parameters 
!! the module defines are available with the current compiler. 
!! We can't ensure that the two REAL kinds are actually four and 
!! eight bytes long, but we can ensure that they are distinct. 
!! Early Fortran 90 compilers would sometimes report incorrect results for 
!! the bit_size intrinsic, but I haven't seen this in a long time. 
LOGICAL FUNCTION SizesOK &
!
()

IMPLICIT NONE

! Local scalars:
INTEGER (KIND = short)  :: short_int
INTEGER (KIND = long)   :: long_int
REAL    (KIND = float)  :: float_real
REAL    (KIND = double) :: double_real
!------------end of declaration------------------------------------------------

  IF (BIT_SIZE( short_int) == 32  .AND. BIT_SIZE( long_int) == 64 .AND.  &
      float > 0 .AND. double > 0  .AND. float /= double                  ) THEN
      SizesOK = .TRUE.
  ELSE
      SizesOK = .FALSE.
  END IF
END FUNCTION SizesOK

END MODULE DataTypeSizes


