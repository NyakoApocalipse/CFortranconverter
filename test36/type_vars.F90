!!v=================================================================================================
!!
!! *module type_vars - Define basic Fortran data types for Angelica*
!!
!!v CVS Id: $Id: type_vars.F90,v 1.14 2007/02/23 19:17:29 jlin Exp $
!!
!! *Notes*
!!
!!  1. E. Zywicz (Oct 2005) Updated
!!
!!  History:
!!
!!  - Original: R.M. Ferencz (May 2000)
!!
!   ================================================================================================
! 
!   Angelica
!   Methods Development Group, LLNL
!   Copyright (c) 2004-05 Regents of the University of California

module type_vars

    ! Scalar variable types to be used throughout code
    ! ------------------------------------------------

        implicit none
!        integer, parameter:: singR = selected_real_kind(6,35)
!        integer, parameter:: fullR = selected_real_kind(13,99)
!        integer, parameter:: singI = selected_int_kind(9)
!
!!        integer, parameter:: singR = kind(0.e0)  !problems if -r8 used
!
!The codes under this line have to be deleted while running converter(Terry 24.6.13)
       integer, parameter:: singR = selected_real_kind(5)
       integer, parameter:: fullR = kind(0.d0)
       integer, parameter:: singI = kind(0)
       integer, parameter:: fullI = selected_int_kind(18)
       integer, parameter:: singL = kind(.true.)
!The codes above this line have to be deleted while running converter

    ! Caution: the size of a singI integer type and a singL logical type
    !          must be the same for restart files to work correclty!

    ! Note: By the standard, the default real type, integer type, and
    !       logical type are all the same size, i.e., same number of bytes!


    ! Numerical constants that might be useful
    ! ----------------------------------------

        real(fullR), parameter :: ZERO     = 0.0_fullR 
        real(fullR), parameter :: ROOT_EPS = 0.000000059604644775390625_fullR ! 2^(-24) = 5.9604644775E-8
        real(fullR), parameter :: ONE      = 1.0_fullR 
        real(fullR), parameter :: TWO      = 2.0_fullR 
        real(fullR), parameter :: THREE    = 3.0_fullR 
        real(fullR), parameter :: FOUR     = 4.0_fullR 
        real(fullR), parameter :: FIVE     = 5.0_fullR 
        real(fullR), parameter :: SIX      = 6.0_fullR 
        real(fullR), parameter :: SEVEN    = 7.0_fullR 
        real(fullR), parameter :: EIGHT    = 8.0_fullR 
        real(fullR), parameter :: NINE     = 9.0_fullR 
        real(fullR), parameter :: TEN      = 10.0_fullR
        real(fullR), parameter :: SIXTEEN  = 16.0_fullR
        real(fullR), parameter :: SIXTYFOUR = 64.0_fullR 
        real(fullR), parameter :: HUNDRED   = 100.0_fullR 
        real(fullR), parameter :: THREEHALF = 1.5_fullR 
        real(fullR), parameter :: FOURTHIRD = 1.33333333333333333_fullR 
        real(fullR), parameter :: SQRT23    = 0.81649658092772603_fullR
        real(fullR), parameter :: TWOTHIRD  = 0.66666666666666667_fullR 
        real(fullR), parameter :: HALF      = 0.50000000000000000_fullR 
        real(fullR), parameter :: THIRD     = 0.33333333333333333_fullR 
        real(fullR), parameter :: FOURTH    = 0.25000000000000000_fullR 
        real(fullR), parameter :: SIXTH     = 0.16666666666666667_fullR 
        real(fullR), parameter :: EIGHTH    = 0.12500000000000000_fullR
        real(fullR), parameter :: NINTH     = 0.11111111111111111_fullR 
        real(fullR), parameter :: TWENTYFOURTH = 0.041666666666666666_fullR 
        real(fullR), parameter :: ROOT2     = 1.4142135623730950_fullR 
        real(fullR), parameter :: ROOT3     = 1.7320508075688773_fullR
        real(fullR), parameter :: PI        = 3.14159265358979323846264338327950288419716939937511_fullR 


    ! Set the standard vector length
    ! -----------------------------------------------------------------
#ifdef UNICS
        integer(singI),parameter :: lnv = 128
#else
#ifdef SUN
        integer(singI),parameter :: lnv = 16
#else
        integer(singI),parameter :: lnv = 32
#endif
#endif


    ! Other standard parameters (Probably should be in separate module)
    ! -----------------------------------------------------------------

        integer(singI), parameter:: MAX_STRING_LENGTH = 64
        character(len=*), parameter :: STRING_FORMAT = 'a64'
        character(MAX_STRING_LENGTH), parameter :: STRING_BLANK =  &
        "                                         "                                                              

!  ===========
!  End of file 
!  ===========
END MODULE type_vars

