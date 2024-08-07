module mod_restart
!
! This module contains the basic routines used to read/write data from
! F90 routines to the restart file system. There are 2 generic routines
! which write "integer", "real", "logical", or "character" type data. They
! write either fixed variable or pointers data that is either a scalar or
! an array of any of the four basic types just listed. These routines will
! not work on non-pointer "allocatable" arrays!
!
! Call rwd_data as
!
!                   call rwd_data(iobf,jw,jc,iadd,VAR) 
!
!  when the variable VAR is not a pointer, and call rwd_pdata as
!
!                   call rwd_pdata(iobf,jw,jc,iadd,pVAR) 
!
!  when the variable pVAR is a pointer. This routine automatically reads
!  or writes the status of the pointer to the restart file as well as the
!  associated data, if present. It also write the variable name to the
!  restart file or reads it from the restart file. If this is a read or
!  compare phase, the intended and read in strings are compared, and an
!  error occurs if the strings differ.
!
!
! IT IS VERY IMPORTANT NOT TO CALL RWD_DATA USING A POINTER SINCE IT 
! DOES NOT DEAL WITH SPACE ALLOCATION!
!
! The above calls are actually generic interfaces which redirect the call
! to the correct procedure. The forty individual subroutines are included
! in this file, but can only be used by using the two generic routines
! above.
!
!

  USE type_vars

  implicit none
!
! 
  INTERFACE rwd_data

    MODULE PROCEDURE  &
     rwd_inta0  ,rwd_inta1  ,rwd_inta2,  rwd_inta3,  rwd_inta4,  rwd_inta5, &
     rwd_reala0 ,rwd_reala1 ,rwd_reala2, rwd_reala3, rwd_reala4, rwd_reala5, &
     rwd_logica0,rwd_logica1,rwd_logica2,rwd_logica3,rwd_logica4,rwd_logica5, &
     rwd_chara0 ,rwd_chara1 ,rwd_chara2, rwd_chara3, rwd_chara4, rwd_chara5

  END INTERFACE rwd_data

  INTERFACE rwd_pdata

    MODULE PROCEDURE  &
     rwd_intp0,  rwd_intp1,  rwd_intp2,  rwd_intp3,  rwd_intp4,  rwd_intp5, &
     rwd_realp0, rwd_realp1, rwd_realp2, rwd_realp3, rwd_realp4, rwd_realp5, &
     rwd_logicp0,rwd_logicp1,rwd_logicp2,rwd_logicp3,rwd_logicp4,rwd_logicp5, &
     rwd_charp0, rwd_charp1, rwd_charp2, rwd_charp3, rwd_charp4, rwd_charp5

  END INTERFACE rwd_pdata
!
! Below - variables common to many subroutines. When they are used, they
!         are only used for this purpose.
!
  character*(1),dimension(:),allocatable,private :: temp_char
  integer(singI),private :: &
                    ioerr, &     !Error flag for rdabsf
                    istat, &     !Allocation error flag
                    l1,l2,l3,l4,l5, & !Loop indexes
                    lc,    &     !Length of a character string
                    pos          !Current position in the temporary string
!
! Array bounds and size
  integer(singI),dimension(11),private :: s
!
  integer(singI) :: iprec,ncpw
  common/double/iprec,ncpw
!
! Force the user to use the generic interfaces
!
  Private :: &
     rwd_inta0  ,rwd_inta1  ,rwd_inta2,  rwd_inta3,  rwd_inta4,  rwd_inta5, &
     rwd_reala0 ,rwd_reala1 ,rwd_reala2, rwd_reala3, rwd_reala4, rwd_reala5, &
     rwd_logica0,rwd_logica1,rwd_logica2,rwd_logica3,rwd_logica4,rwd_logica5, &
     rwd_chara0 ,rwd_chara1 ,rwd_chara2, rwd_chara3, rwd_chara4, rwd_chara5, &
     rwd_intp0,  rwd_intp1,  rwd_intp2,  rwd_intp3,  rwd_intp4,  rwd_intp5, &
     rwd_realp0, rwd_realp1, rwd_realp2, rwd_realp3, rwd_realp4, rwd_realp5, &
     rwd_logicp0,rwd_logicp1,rwd_logicp2,rwd_logicp3,rwd_logicp4,rwd_logicp5, &
     rwd_charp0, rwd_charp1, rwd_charp2, rwd_charp3, rwd_charp4, rwd_charp5, &
     check_rw_label
!
  Contains

   subroutine check_rw_label(fit,jw,jc,iadd,label)
!
! During a write phase, place the variable name in the restart file.
! During a read or compare phase, get the variable name from the restart
! file and compare it to what you think it should be. If they differ,
! bail!
!
!     input arguments
!            fit      the file information table
!            jw       read/write flag
!                     0 - read file
!                     1 - write file
!                     2 - compare file with array
!            jc       count flag (write phase only)
!                     0 - count phase
!                     1 - write data phase
!            label    character string to be written to disk
!            iadd     zero base disk address
!
     character*(*),intent(in)     :: label
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: fit
!
     character*(128):: string,string_from_file
     character*(16) :: blank16='                '
     integer(singI) :: nw
!
     lc = len(label)         !how many characters in label?
     nw = 1+(lc-1)/ncpw      !how many integer words is the label - round up
     if(lc > 128) then
       write(13,103) lc
       write( *,103) lc
       call adios(2)
     endif
!
!
     if(jw == 1) then         ! Write phase
!      if(jc == 0)  just count the stuff to write
       if(jc == 1) then
         string       = REPEAT(blank16,8)  !fill string with all blanks
         string(1:lc) = label              !put label in string - left justified
         call wrabsf (fit,string,nw,iadd) !write string to file
       endif
     else                     ! Read and compare phase  (jw=0 or jw=2)
       ioerr = 0
       call rdabsf(fit,string_from_file,nw,iadd,ioerr)
       if(label(1:lc) /= string_from_file(1:lc)) then
         write(13,101) label(1:lc)
         write(13,102) string_from_file(1:lc)
         write( *,101) label(1:lc)
         write( *,102) string_from_file(1:lc)
         call adios(2)
       endif
     endif
!
     iadd = iadd + nw
!
101  format(//,'******************** FATAL ERROR ********************',//,&
               'The variable being read from the restart file does agree',/,&
               'with it is supposed to be!',/,&
               'Intended variable name: ',128a)
102  format(   'Restart variable name: ',128a,//,&
               '******************** FATAL ERROR ********************',//)
103  format(//,'******************** FATAL ERROR ********************',//,&
               'Temporary space is to small in check_rw_label',/,&
               'Needed:',i8,/,&
               'Current: 128',//,&
               '******************** FATAL ERROR ********************',//)
!
   end subroutine check_rw_label

   
!___________________________________________________________
!
!    INTEGER
!___________________________________________________________
! Integers - arrays up to rank 5
   subroutine rwd_inta0(iobf,jw,jc,iadd,ia0,label)
     character*(*)  :: label
     integer(singI) :: ia0
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     call absfadd(iobf,jw,jc, ia0, 1, iadd)
   end subroutine rwd_inta0

   subroutine rwd_inta1(iobf,jw,jc,iadd,ia1,label)
     character*(*)  :: label
     integer(singI),dimension(:) :: ia1
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     if(jw /= 0) then
       s(1) = LBOUND(ia1,dim=1)
       s(2) = UBOUND(ia1,dim=1)
       s(3) = SIZE(ia1)
     endif
     call absfadd (iobf,jw,jc,s,3,iadd)
     call absfadd (iobf,jw,jc,ia1,s(3),iadd)
   end subroutine rwd_inta1

   subroutine rwd_inta2(iobf,jw,jc,iadd,ia2,label)
     character*(*)  :: label
     integer(singI),dimension(:,:) :: ia2
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     if(jw /= 0) then
       s(1:2) = LBOUND(ia2)
       s(3:4) = UBOUND(ia2)
       s(5)   = SIZE(ia2)
     endif
     call absfadd (iobf,jw,jc,s,5,iadd)
     call absfadd (iobf,jw,jc,ia2,s(5),iadd)
   end subroutine rwd_inta2

   subroutine rwd_inta3(iobf,jw,jc,iadd,ia3,label)
     character*(*)  :: label
     integer(singI),dimension(:,:,:) :: ia3
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     if(jw /= 0) then
       s(1:3) = LBOUND(ia3)
       s(4:6) = UBOUND(ia3)
       s(7)   = SIZE(ia3)
     endif
     call absfadd (iobf,jw,jc,s,7,iadd)
     call absfadd (iobf,jw,jc,ia3,s(7),iadd)
   end subroutine rwd_inta3

   subroutine rwd_inta4(iobf,jw,jc,iadd,ia4,label)
     character*(*)  :: label
     integer(singI),dimension(:,:,:,:) :: ia4
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     if(jw /= 0) then
       s(1:4) = LBOUND(ia4)
       s(5:8) = UBOUND(ia4)
       s(9)   = SIZE(ia4)
     endif
     call absfadd (iobf,jw,jc,s,9,iadd)
     call absfadd (iobf,jw,jc,ia4,s(9),iadd)
   end subroutine rwd_inta4

   subroutine rwd_inta5(iobf,jw,jc,iadd,ia5,label)
     character*(*)  :: label
     integer(singI),dimension(:,:,:,:,:) :: ia5
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     if(jw /= 0) then
       s(1:5 ) = LBOUND(ia5)
       s(6:10) = UBOUND(ia5)
       s(11)   = SIZE(ia5)
     endif
     call absfadd (iobf,jw,jc,s,11,iadd)
     call absfadd (iobf,jw,jc,ia5,s(11),iadd)
   end subroutine rwd_inta5
!
! Integers - pointers up to rank 5
   subroutine rwd_intp0(iobf,jw,jc,iadd,ip0,label)
     character*(*)  :: label
     integer(singI),pointer :: ip0
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(ip0)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw==0) allocate(ip0)
       call absfadd (iobf,jw,jc,ip0,1,iadd)
     endif
   end subroutine rwd_intp0

   subroutine rwd_intp1(iobf,jw,jc,iadd,ip1,label)
     character*(*)  :: label
     integer(singI),dimension(:),pointer :: ip1
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(ip1)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw /= 0) then
         s(1) = LBOUND(ip1,dim=1)
         s(2) = UBOUND(ip1,dim=1)
         s(3) = SIZE(ip1)
       endif
       call absfadd (iobf,jw,jc,s,3,iadd)
       if(jw==0) allocate(ip1(s(1):s(2)))
       call absfadd (iobf,jw,jc,ip1,s(3),iadd)
     endif
   end subroutine rwd_intp1

   subroutine rwd_intp2(iobf,jw,jc,iadd,ip2,label)
     character*(*)  :: label
     integer(singI),dimension(:,:),pointer :: ip2
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(ip2)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw /= 0) then
         s(1:2) = LBOUND(ip2)
         s(3:4) = UBOUND(ip2)
         s(5)   = SIZE(ip2)
       endif
       call absfadd (iobf,jw,jc,s,5,iadd)
       if(jw==0) allocate(ip2(s(1):s(3),s(2):s(4)))
       call absfadd (iobf,jw,jc,ip2,s(5),iadd)
     endif
   end subroutine rwd_intp2

   subroutine rwd_intp3(iobf,jw,jc,iadd,ip3,label)
     character*(*)  :: label
     integer(singI),dimension(:,:,:),pointer :: ip3
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(ip3)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw /= 0) then
         s(1:3) = LBOUND(ip3)
         s(4:6) = UBOUND(ip3)
         s(7)   = SIZE(ip3)
       endif
       call absfadd (iobf,jw,jc,s,7,iadd)
       if(jw==0) allocate(ip3(s(1):s(4),s(2):s(5),s(3):s(6)))
       call absfadd (iobf,jw,jc,ip3,s(7),iadd)
     endif
   end subroutine rwd_intp3

   subroutine rwd_intp4(iobf,jw,jc,iadd,ip4,label)
     character*(*)  :: label
     integer(singI),dimension(:,:,:,:),pointer :: ip4
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(ip4)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw /= 0) then
         s(1:4) = LBOUND(ip4)
         s(5:8) = UBOUND(ip4)
         s(9)   = SIZE(ip4)
       endif
       call absfadd (iobf,jw,jc,s,9,iadd)
       if(jw==0) allocate(ip4(s(1):s(5),s(2):s(6),s(3):s(7),s(4):s(8)))
       call absfadd (iobf,jw,jc,ip4,s(9),iadd)
     endif
   end subroutine rwd_intp4

   subroutine rwd_intp5(iobf,jw,jc,iadd,ip5,label)
     character*(*)  :: label
     integer(singI),dimension(:,:,:,:,:),pointer :: ip5
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(ip5)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw /= 0) then
         s(1:5 ) = LBOUND(ip5)
         s(6:10) = UBOUND(ip5)
         s(11)   = SIZE(ip5)
       endif
       call absfadd (iobf,jw,jc,s,11,iadd)
       if(jw==0) allocate(ip5(s(1):s(6),s(2):s(7),s(3):s(8), &
                              s(4):s(9),s(5):s(10)))
       call absfadd (iobf,jw,jc,ip5,s(11),iadd)
     endif
   end subroutine rwd_intp5
!___________________________________________________________
!
!    REAL
!___________________________________________________________
!
! Reals - arrays up to rank 5
   subroutine rwd_reala0(iobf,jw,jc,iadd,ra0,label)
     character*(*)  :: label
     real(fullR) :: ra0
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     call absfadd(iobf,jw,jc, ra0, iprec, iadd)
   end subroutine rwd_reala0

   subroutine rwd_reala1(iobf,jw,jc,iadd,ra1,label)
     character*(*)  :: label
     real(fullR),dimension(:) :: ra1
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     if(jw /= 0) then
       s(1) = LBOUND(ra1,dim=1)
       s(2) = UBOUND(ra1,dim=1)
       s(3) = SIZE(ra1)*iprec
     endif
     call absfadd (iobf,jw,jc,s,3,iadd)
     call absfadd (iobf,jw,jc,ra1,s(3),iadd)
   end subroutine rwd_reala1

   subroutine rwd_reala2(iobf,jw,jc,iadd,ra2,label)
     character*(*)  :: label
     real(fullR),dimension(:,:) :: ra2
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     if(jw /= 0) then
       s(1:2) = LBOUND(ra2)
       s(3:4) = UBOUND(ra2)
       s(5)   = SIZE(ra2)*iprec
     endif
     call absfadd (iobf,jw,jc,s,5,iadd)
     call absfadd (iobf,jw,jc,ra2,s(5),iadd)
   end subroutine rwd_reala2

   subroutine rwd_reala3(iobf,jw,jc,iadd,ra3,label)
     character*(*)  :: label
     real(fullR),dimension(:,:,:) :: ra3
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     if(jw /= 0) then
       s(1:3) = LBOUND(ra3)
       s(4:6) = UBOUND(ra3)
       s(7)   = SIZE(ra3)*iprec
     endif
     call absfadd (iobf,jw,jc,s,7,iadd)
     call absfadd (iobf,jw,jc,ra3,s(7),iadd)
   end subroutine rwd_reala3

   subroutine rwd_reala4(iobf,jw,jc,iadd,ra4,label)
     character*(*)  :: label
     real(fullR),dimension(:,:,:,:) :: ra4
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     if(jw /= 0) then
       s(1:4) = LBOUND(ra4)
       s(5:8) = UBOUND(ra4)
       s(9)   = SIZE(ra4)*iprec
     endif
     call absfadd (iobf,jw,jc,s,9,iadd)
     call absfadd (iobf,jw,jc,ra4,s(9),iadd)
   end subroutine rwd_reala4

   subroutine rwd_reala5(iobf,jw,jc,iadd,ra5,label)
     character*(*)  :: label
     real(fullR),dimension(:,:,:,:,:) :: ra5
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     if(jw /= 0) then
       s(1:5 ) = LBOUND(ra5)
       s(6:10) = UBOUND(ra5)
       s(11)   = SIZE(ra5)*iprec
     endif
     call absfadd (iobf,jw,jc,s,11,iadd)
     call absfadd (iobf,jw,jc,ra5,s(11),iadd)
   end subroutine rwd_reala5
!
! Reals - pointers up to rank 5
   subroutine rwd_realp0(iobf,jw,jc,iadd,rp0,label)
     character*(*)  :: label
     real(fullR),pointer :: rp0
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(rp0)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw==0) allocate(rp0)
       call absfadd (iobf,jw,jc,rp0,iprec,iadd)
     endif
   end subroutine rwd_realp0

   subroutine rwd_realp1(iobf,jw,jc,iadd,rp1,label)
     character*(*)  :: label
     real(fullR),dimension(:),pointer :: rp1
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(rp1)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw /= 0) then
         s(1) = LBOUND(rp1,dim=1)
         s(2) = UBOUND(rp1,dim=1)
         s(3) = SIZE(rp1)*iprec
       endif
       call absfadd (iobf,jw,jc,s,3,iadd)
       if(jw==0) allocate(rp1(s(1):s(2)))
       call absfadd (iobf,jw,jc,rp1,s(3),iadd)
     endif
   end subroutine rwd_realp1

   subroutine rwd_realp2(iobf,jw,jc,iadd,rp2,label)
     character*(*)  :: label
     real(fullR),dimension(:,:),pointer :: rp2
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(rp2)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw /= 0) then
         s(1:2) = LBOUND(rp2)
         s(3:4) = UBOUND(rp2)
         s(5)   = SIZE(rp2)*iprec
       endif
       call absfadd (iobf,jw,jc,s,5,iadd)
       if(jw==0) allocate(rp2(s(1):s(3),s(2):s(4)))
       call absfadd (iobf,jw,jc,rp2,s(5),iadd)
     endif
   end subroutine rwd_realp2

   subroutine rwd_realp3(iobf,jw,jc,iadd,rp3,label)
     character*(*)  :: label
     real(fullR),dimension(:,:,:),pointer :: rp3
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(rp3)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw /= 0) then
         s(1:3) = LBOUND(rp3)
         s(4:6) = UBOUND(rp3)
         s(7)   = SIZE(rp3)*iprec
       endif
       call absfadd (iobf,jw,jc,s,7,iadd)
       if(jw==0) allocate(rp3(s(1):s(4),s(2):s(5),s(3):s(6)))
       call absfadd (iobf,jw,jc,rp3,s(7),iadd)
     endif
   end subroutine rwd_realp3

   subroutine rwd_realp4(iobf,jw,jc,iadd,rp4,label)
     character*(*)  :: label
     real(fullR),dimension(:,:,:,:),pointer :: rp4
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(rp4)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw /= 0) then
         s(1:4) = LBOUND(rp4)
         s(5:8) = UBOUND(rp4)
         s(9)   = SIZE(rp4)*iprec
       endif
       call absfadd (iobf,jw,jc,s,9,iadd)
       if(jw==0) allocate(rp4(s(1):s(5),s(2):s(6),s(3):s(7),s(4):s(8)))
       call absfadd (iobf,jw,jc,rp4,s(9),iadd)
     endif
   end subroutine rwd_realp4

   subroutine rwd_realp5(iobf,jw,jc,iadd,rp5,label)
     character*(*)  :: label
     real(fullR),dimension(:,:,:,:,:),pointer :: rp5
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(rp5)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw /= 0) then
         s(1:5 ) = LBOUND(rp5)
         s(6:10) = UBOUND(rp5)
         s(11)   = SIZE(rp5)*iprec
       endif
       call absfadd (iobf,jw,jc,s,11,iadd)
       if(jw==0) allocate(rp5(s(1):s(6),s(2):s(7),s(3):s(8), &
                              s(4):s(9),s(5):s(10)))
       call absfadd (iobf,jw,jc,rp5,s(11),iadd)
     endif
   end subroutine rwd_realp5
!___________________________________________________________
!
!    LOGICAL
!___________________________________________________________
!
! Logicals - arrays up to rank 5
   subroutine rwd_logica0(iobf,jw,jc,iadd,la0,label)
     character*(*)  :: label
     logical(singL) :: la0
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     call absfadd(iobf,jw,jc, la0, 1, iadd)
   end subroutine rwd_logica0

   subroutine rwd_logica1(iobf,jw,jc,iadd,la1,label)
     character*(*)  :: label
     logical(singL),dimension(:) :: la1
     integer(singI)              :: jw,jc,iadd
     integer(singI),dimension(*) :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     if(jw /= 0) then
       s(1) = LBOUND(la1,dim=1)
       s(2) = UBOUND(la1,dim=1)
       s(3) = SIZE(la1)
     endif
     call absfadd (iobf,jw,jc,s,3,iadd)
     call absfadd (iobf,jw,jc,la1,s(3),iadd)
   end subroutine rwd_logica1

   subroutine rwd_logica2(iobf,jw,jc,iadd,la2,label)
     character*(*)  :: label
     logical(singL),dimension(:,:) :: la2
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     if(jw /= 0) then
       s(1:2) = LBOUND(la2)
       s(3:4) = UBOUND(la2)
       s(5)   = SIZE(la2)
     endif
     call absfadd (iobf,jw,jc,s,5,iadd)
     call absfadd (iobf,jw,jc,la2,s(5),iadd)
   end subroutine rwd_logica2

   subroutine rwd_logica3(iobf,jw,jc,iadd,la3,label)
     character*(*)  :: label
     logical(singL),dimension(:,:,:) :: la3
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     if(jw /= 0) then
       s(1:3) = LBOUND(la3)
       s(4:6) = UBOUND(la3)
       s(7)   = SIZE(la3)
     endif
     call absfadd (iobf,jw,jc,s,7,iadd)
     call absfadd (iobf,jw,jc,la3,s(7),iadd)
   end subroutine rwd_logica3

   subroutine rwd_logica4(iobf,jw,jc,iadd,la4,label)
     character*(*)  :: label
     logical(singL),dimension(:,:,:,:) :: la4
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     if(jw /= 0) then
       s(1:4) = LBOUND(la4)
       s(5:8) = UBOUND(la4)
       s(9)   = SIZE(la4)
     endif
     call absfadd (iobf,jw,jc,s,9,iadd)
     call absfadd (iobf,jw,jc,la4,s(9),iadd)
   end subroutine rwd_logica4

   subroutine rwd_logica5(iobf,jw,jc,iadd,la5,label)
     character*(*)  :: label
     logical(singL),dimension(:,:,:,:,:) :: la5
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     if(jw /= 0) then
       s(1:5 ) = LBOUND(la5)
       s(6:10) = UBOUND(la5)
       s(11)   = SIZE(la5)
     endif
     call absfadd (iobf,jw,jc,s,11,iadd)
     call absfadd (iobf,jw,jc,la5,s(11),iadd)
   end subroutine rwd_logica5
!
! Logicals - pointers up to rank 5
   subroutine rwd_logicp0(iobf,jw,jc,iadd,lp0,label)
     character*(*)  :: label
     logical(singL),pointer :: lp0
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(lp0)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw==0) allocate(lp0)
       call absfadd (iobf,jw,jc,lp0,1,iadd)
     endif
   end subroutine rwd_logicp0

   subroutine rwd_logicp1(iobf,jw,jc,iadd,lp1,label)
     character*(*)  :: label
     logical(singL),dimension(:),pointer :: lp1
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(lp1)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw /= 0) then
         s(1) = LBOUND(lp1,dim=1)
         s(2) = UBOUND(lp1,dim=1)
         s(3) = SIZE(lp1)
       endif
       call absfadd (iobf,jw,jc,s,3,iadd)
       if(jw==0) allocate(lp1(s(1):s(2)))
       call absfadd (iobf,jw,jc,lp1,s(3),iadd)
     endif
   end subroutine rwd_logicp1

   subroutine rwd_logicp2(iobf,jw,jc,iadd,lp2,label)
     character*(*)  :: label
     logical(singL),dimension(:,:),pointer :: lp2
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(lp2)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw /= 0) then
         s(1:2) = LBOUND(lp2)
         s(3:4) = UBOUND(lp2)
         s(5)   = SIZE(lp2)
       endif
       call absfadd (iobf,jw,jc,s,5,iadd)
       if(jw==0) allocate(lp2(s(1):s(3),s(2):s(4)))
       call absfadd (iobf,jw,jc,lp2,s(5),iadd)
     endif
   end subroutine rwd_logicp2

   subroutine rwd_logicp3(iobf,jw,jc,iadd,lp3,label)
     character*(*)  :: label
     logical(singL),dimension(:,:,:),pointer :: lp3
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(lp3)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw /= 0) then
         s(1:3) = LBOUND(lp3)
         s(4:6) = UBOUND(lp3)
         s(7)   = SIZE(lp3)
       endif
       call absfadd (iobf,jw,jc,s,7,iadd)
       if(jw==0) allocate(lp3(s(1):s(4),s(2):s(5),s(3):s(6)))
       call absfadd (iobf,jw,jc,lp3,s(7),iadd)
     endif
   end subroutine rwd_logicp3

   subroutine rwd_logicp4(iobf,jw,jc,iadd,lp4,label)
     character*(*)  :: label
     logical(singL),dimension(:,:,:,:),pointer :: lp4
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(lp4)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw /= 0) then
         s(1:4) = LBOUND(lp4)
         s(5:8) = UBOUND(lp4)
         s(9)   = SIZE(lp4)
       endif
       call absfadd (iobf,jw,jc,s,9,iadd)
       if(jw==0) allocate(lp4(s(1):s(5),s(2):s(6),s(3):s(7),s(4):s(8)))
       call absfadd (iobf,jw,jc,lp4,s(9),iadd)
     endif
   end subroutine rwd_logicp4

   subroutine rwd_logicp5(iobf,jw,jc,iadd,lp5,label)
     character*(*)  :: label
     logical(singL),dimension(:,:,:,:,:),pointer :: lp5
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(lp5)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       if(jw /= 0) then
         s(1:5 ) = LBOUND(lp5)
         s(6:10) = UBOUND(lp5)
         s(11)   = SIZE(lp5)
       endif
       call absfadd (iobf,jw,jc,s,11,iadd)
       if(jw==0) allocate(lp5(s(1):s(6),s(2):s(7),s(3):s(8), &
                              s(4):s(9),s(5):s(10)))
       call absfadd (iobf,jw,jc,lp5,s(11),iadd)
     endif
   end subroutine rwd_logicp5
!___________________________________________________________
!
!    CHARACTER
!___________________________________________________________
!
! characters - arrays up to rank 5
   subroutine form_character_string(string_out,string_in,pos)
!
! Concatenate the letters in string_in, starting from
! position "pos", to form the string string_out.
     integer(singI),intent(inout) :: pos
     character*(1),dimension(*),intent(in) :: string_in
     character*(*),intent(out)    :: string_out
!
     integer(singI) :: i
!
     do i=1,Len(string_out)
       pos             = pos + 1
       string_out(i:i) = string_in(pos)
     enddo
!
   end subroutine form_character_string

   subroutine compare_character_string(stringA,stringB,pos)
!
! Compare letter by letter stringA with stringB starting at pos
! in stringB.
     integer(singI),intent(inout)          :: pos
     character*(*),intent(in)              :: stringA
     character*(1),dimension(*),intent(in) :: stringB
!
     integer check_restart_cycle
     common/chk_restart/check_restart_cycle
!
     integer(singI) :: i,lc
!
     do i=1,Len(stringA)
       pos = pos + 1
       if(stringA(i:i) /= stringB(pos)) then
         lc = Len(stringA)
         write(13,101) trim(stringA)
         write(13,102) stringB(pos-i+1:pos-i+1+lc)
         write( *,101) trim(stringA)
         write( *,102) stringB(pos-i+1:pos-i+1+lc)
         if(check_restart_cycle == 0) call adios(2)
         exit
       endif
     enddo
!
101  format(//,'******************** FATAL ERROR ********************',//,&
               'Compare error: Differences in character strings were found.',/,&
               'String from memory: ',a)
102  format(   'String from restart file: ',128a,//,&
               '******************** FATAL ERROR ********************',//)
!
   end subroutine compare_character_string

   subroutine rwd_chara0(iobf,jw,jc,iadd,ca0,label)
     USE mod_alloc_error
!
     character*(*) :: label
     character*(*) :: ca0
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     lc   = Len(ca0)
     s(1) = 1 + (lc-1)/ncpw
!
     if(jw == 0 .or. jw == 2) then   ! read or compare
       allocate(temp_char(ncpw*s(1)),stat=istat)
       if(istat /= 0) call alloc_error("temp_char","rwd_chara0")
       ioerr = 0
       call rdabsf(iobf,temp_char,s(1),iadd,ioerr)
       iadd = iadd + s(1)
       pos  = 0
       if(jw == 0) then              ! load the array up
         call form_character_string(ca0,temp_char,pos)
       elseif(jw == 2) then          !jw = 2
         call compare_character_string(ca0,temp_char,pos)
       endif
       deallocate(temp_char)
     else
       call absfadd(iobf,jw,jc, ca0, s(1), iadd)
     endif
!
   end subroutine rwd_chara0

   subroutine rwd_chara1(iobf,jw,jc,iadd,ca1,label)
     USE mod_alloc_error
     character*(*)  :: label
     character*(*),dimension(:) :: ca1
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     lc = Len(ca1)
!
     if(jw /= 0) then
       s(1) = LBOUND(ca1,dim=1)
       s(2) = UBOUND(ca1,dim=1)
       s(3) = SIZE(ca1)*(1 + (lc-1)/ncpw)
     endif
     call absfadd (iobf,jw,jc,s,3,iadd)
!
     if(jw == 0 .or. jw == 2) then   ! read or compare
       allocate(temp_char(ncpw*s(3)),stat=istat)
       if(istat /= 0) call alloc_error("temp_char","rwd_chara1")
       ioerr = 0
       call rdabsf(iobf,temp_char,s(3),iadd,ioerr)
       iadd = iadd + s(3)
       pos  = 0
       if(jw == 0) then              ! load the array up
         do l1=s(1),s(2)
           call form_character_string(ca1(l1),temp_char,pos)
         enddo
       elseif(jw == 2) then          !jw = 2
         do l1=s(1),s(2)
           call compare_character_string(ca1(l1),temp_char,pos)
         enddo
       endif
       deallocate(temp_char)
     else
       call absfadd (iobf,jw,jc,ca1,s(3),iadd)
     endif
   end subroutine rwd_chara1

   subroutine rwd_chara2(iobf,jw,jc,iadd,ca2,label)
     USE mod_alloc_error
     character*(*)  :: label
     character*(*),dimension(:,:) :: ca2
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     lc = Len(ca2)
!
     if(jw /= 0) then
       s(1:2) = LBOUND(ca2)
       s(3:4) = UBOUND(ca2)
       s(5)   = SIZE(ca2)*(1 + (lc-1)/ncpw)
     endif
     call absfadd (iobf,jw,jc,s,5,iadd)
!
     if(jw == 0 .or. jw == 2) then   ! read or compare
       allocate(temp_char(ncpw*s(5)),stat=istat)
       if(istat /= 0) call alloc_error("temp_char","rwd_chara2")
       ioerr = 0
       call rdabsf(iobf,temp_char,s(5),iadd,ioerr)
       iadd = iadd + s(5)
       pos  = 0
       if(jw == 0) then              ! load the array up
         do l2=s(2),s(4)
         do l1=s(1),s(3)
           call form_character_string(ca2(l1,l2),temp_char,pos)
         enddo
         enddo
       elseif(jw == 2) then          !jw = 2
         do l2=s(2),s(4)
         do l1=s(1),s(3)
           call compare_character_string(ca2(l1,l2),temp_char,pos)
         enddo
         enddo
       endif
       deallocate(temp_char)
     else
       call absfadd (iobf,jw,jc,ca2,s(5),iadd)
     endif
   end subroutine rwd_chara2

   subroutine rwd_chara3(iobf,jw,jc,iadd,ca3,label)
     USE mod_alloc_error
     character*(*)  :: label
     character*(*),dimension(:,:,:) :: ca3
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     lc = Len(ca3)
!
     if(jw /= 0) then
       s(1:3) = LBOUND(ca3)
       s(4:6) = UBOUND(ca3)
       s(7)   = SIZE(ca3)*(1 + (LEN(ca3)-1)/ncpw)
     endif
     call absfadd (iobf,jw,jc,s,7,iadd)
!
     if(jw == 0 .or. jw == 2) then   ! read or compare
       allocate(temp_char(ncpw*s(7)),stat=istat)
       if(istat /= 0) call alloc_error("temp_char","rwd_chara3")
       ioerr = 0
       call rdabsf(iobf,temp_char,s(7),iadd,ioerr)
       iadd = iadd + s(7)
       pos  = 0
       if(jw == 0) then              ! load the array up
         do l3=s(3),s(6)
         do l2=s(2),s(5)
         do l1=s(1),s(4)
           call form_character_string(ca3(l1,l2,l3),temp_char,pos)
         enddo
         enddo
         enddo
       elseif(jw == 2) then          !jw = 2
         do l3=s(3),s(6)
         do l2=s(2),s(5)
         do l1=s(1),s(4)
           call compare_character_string(ca3(l1,l2,l3),temp_char,pos)
         enddo
         enddo
         enddo
       endif
       deallocate(temp_char)
     else
       call absfadd (iobf,jw,jc,ca3,s(7),iadd)
     endif
   end subroutine rwd_chara3

   subroutine rwd_chara4(iobf,jw,jc,iadd,ca4,label)
     USE mod_alloc_error
     character*(*)  :: label
     character*(*),dimension(:,:,:,:) :: ca4
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     lc = Len(ca4)
!
     if(jw /= 0) then
       s(1:4) = LBOUND(ca4)
       s(5:8) = UBOUND(ca4)
       s(9)   = SIZE(ca4)*(1 + (lc-1)/ncpw)
     endif
     call absfadd (iobf,jw,jc,s,9,iadd)
!
     if(jw == 0 .or. jw == 2) then   ! read or compare
       allocate(temp_char(ncpw*s(9)),stat=istat)
       if(istat /= 0) call alloc_error("temp_char","rwd_chara4")
       ioerr = 0
       call rdabsf(iobf,temp_char,s(9),iadd,ioerr)
       iadd = iadd + s(9)
       pos  = 0
       if(jw == 0) then              ! load the array up
         do l4=s(4),s(8)
         do l3=s(3),s(7)
         do l2=s(2),s(6)
         do l1=s(1),s(5)
           call form_character_string(ca4(l1,l2,l3,l4),temp_char,pos)
         enddo
         enddo
         enddo
         enddo
       elseif(jw == 2) then          !jw = 2
         do l4=s(4),s(8)
         do l3=s(3),s(7)
         do l2=s(2),s(6)
         do l1=s(1),s(5)
           call compare_character_string(ca4(l1,l2,l3,l4),temp_char,pos)
         enddo
         enddo
         enddo
         enddo
       endif
       deallocate(temp_char)
     else
       call absfadd (iobf,jw,jc,ca4,s(9),iadd)
     endif
   end subroutine rwd_chara4

   subroutine rwd_chara5(iobf,jw,jc,iadd,ca5,label)
     USE mod_alloc_error
     character*(*)  :: label
     character*(*),dimension(:,:,:,:,:) :: ca5
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     call check_rw_label(iobf,jw,jc,iadd,label)
     lc = Len(ca5)
!
     if(jw /= 0) then
       s(1:5 ) = LBOUND(ca5)
       s(6:10) = UBOUND(ca5)
       s(11)   = SIZE(ca5)*(1 + (LEN(ca5)-1)/ncpw)
     endif
     call absfadd (iobf,jw,jc,s,11,iadd)
!
     if(jw == 0 .or. jw == 2) then   ! read or compare
       allocate(temp_char(ncpw*s(11)),stat=istat)
       if(istat /= 0) call alloc_error("temp_char","rwd_chara5")
       ioerr = 0
       call rdabsf(iobf,temp_char,s(11),iadd,ioerr)
       iadd = iadd + s(11)
       pos  = 0
       if(jw == 0) then              ! load the array up
         do l5=s(5),s(10)
         do l4=s(4),s(9)
         do l3=s(3),s(8)
         do l2=s(2),s(7)
         do l1=s(1),s(6)
           call form_character_string(ca5(l1,l2,l3,l4,l5),temp_char,pos)
         enddo
         enddo
         enddo
         enddo
         enddo
       elseif(jw == 2) then          !jw = 2
         do l5=s(5),s(10)
         do l4=s(4),s(9)
         do l3=s(3),s(8)
         do l2=s(2),s(7)
         do l1=s(1),s(6)
           call compare_character_string(ca5(l1,l2,l3,l4,l5),temp_char,pos)
         enddo
         enddo
         enddo
         enddo
         enddo
       endif
       deallocate(temp_char)
     else
       call absfadd (iobf,jw,jc,ca5,s(11),iadd)
     endif
   end subroutine rwd_chara5
!
! characters - pointers up to rank 5
   subroutine rwd_charp0(iobf,jw,jc,iadd,cp0,label)
     USE mod_alloc_error
     character*(*)  :: label
     character*(*),pointer :: cp0
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(cp0)
     call absfadd(iobf,jw,jc, active, 1,iadd)
!
     if(active) then
       lc = Len(cp0)
       if(jw /=0) s(1) = 1 + (lc-1)/ncpw
       call absfadd (iobf,jw,jc,cp0,s(1),iadd)
!
       if(jw == 0 .or. jw == 2) then   ! read or compare
         allocate(temp_char(ncpw*s(1)),stat=istat)
         if(istat /= 0) call alloc_error("temp_char","rwd_charp0")
         ioerr = 0
         call rdabsf(iobf,temp_char,s(1),iadd,ioerr)
         iadd = iadd + s(1)
         pos  = 0
         if(jw == 0) then              ! create and load the array
           allocate(cp0)
           call form_character_string(cp0,temp_char,pos)
         elseif(jw == 2) then          !jw = 2
           call compare_character_string(cp0,temp_char,pos)
         endif
         deallocate(temp_char)
       else
         call absfadd(iobf,jw,jc, cp0, s(1), iadd)
       endif
     endif
   end subroutine rwd_charp0

   subroutine rwd_charp1(iobf,jw,jc,iadd,cp1,label)
     USE mod_alloc_error
     character*(*)  :: label
     character*(*),dimension(:),pointer :: cp1
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(cp1)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       lc = Len(cp1)
       if(jw /= 0) then
         s(1) = LBOUND(cp1,dim=1)
         s(2) = UBOUND(cp1,dim=1)
         s(3) = SIZE(cp1)*(1 + (lc-1)/ncpw)
       endif
       call absfadd (iobf,jw,jc,s,3,iadd)
!
       if(jw == 0 .or. jw == 2) then   ! read or compare
         allocate(temp_char(ncpw*s(3)),stat=istat)
         if(istat /= 0) call alloc_error("temp_char","rwd_charp1")
         ioerr = 0
         call rdabsf(iobf,temp_char,s(3),iadd,ioerr)
         iadd = iadd + s(3)
         pos  = 0
         if(jw == 0) then              ! create & load the array up
           allocate(cp1(s(1):s(2)))
           do l1=s(1),s(2)
             call form_character_string(cp1(l1),temp_char,pos)
           enddo
         elseif(jw == 2) then          !jw = 2
           do l1=s(1),s(2)
             call compare_character_string(cp1(l1),temp_char,pos)
           enddo
         endif
         deallocate(temp_char)
       else
         call absfadd (iobf,jw,jc,cp1,s(3),iadd)
       endif
     endif
   end subroutine rwd_charp1

   subroutine rwd_charp2(iobf,jw,jc,iadd,cp2,label)
     USE mod_alloc_error
     character*(*)  :: label
     character*(*),dimension(:,:),pointer :: cp2
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(cp2)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       lc = Len(cp2)
       if(jw /= 0) then
         s(1:2) = LBOUND(cp2)
         s(3:4) = UBOUND(cp2)
         s(5)   = SIZE(cp2)*(1 + (lc-1)/ncpw)
       endif
       call absfadd (iobf,jw,jc,s,5,iadd)

!
       if(jw == 0 .or. jw == 2) then   ! read or compare
         allocate(temp_char(ncpw*s(5)),stat=istat)
         if(istat /= 0) call alloc_error("temp_char","rwd_charp2")
         ioerr = 0
         call rdabsf(iobf,temp_char,s(5),iadd,ioerr)
         iadd = iadd + s(5)
         pos  = 0
         if(jw == 0) then              ! create & load the array up
           allocate(cp2(s(1):s(3),s(2):s(4)))
           do l2=s(2),s(4)
           do l1=s(1),s(3)
             call form_character_string(cp2(l1,l2),temp_char,pos)
           enddo
           enddo
         elseif(jw == 2) then          !jw = 2
           do l2=s(2),s(4)
           do l1=s(1),s(3)
             call compare_character_string(cp2(l1,l2),temp_char,pos)
           enddo
           enddo
         endif
         deallocate(temp_char)
       else
         call absfadd (iobf,jw,jc,cp2,s(5),iadd)
       endif
     endif
   end subroutine rwd_charp2

   subroutine rwd_charp3(iobf,jw,jc,iadd,cp3,label)
     USE mod_alloc_error
     character*(*)  :: label
     character*(*),dimension(:,:,:),pointer :: cp3
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(cp3)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       lc = Len(cp3)
       if(jw /= 0) then
         s(1:3) = LBOUND(cp3)
         s(4:6) = UBOUND(cp3)
         s(7)   = SIZE(cp3)*(1 + (lc-1)/ncpw)
       endif
       call absfadd (iobf,jw,jc,s,7,iadd)
!
       if(jw == 0 .or. jw == 2) then   ! read or compare
         allocate(temp_char(ncpw*s(7)),stat=istat)
         if(istat /= 0) call alloc_error("temp_char","rwd_charp3")
         ioerr = 0
         call rdabsf(iobf,temp_char,s(7),iadd,ioerr)
         iadd = iadd + s(7)
         pos  = 0
         if(jw == 0) then              ! load the array up
           allocate(cp3(s(1):s(4),s(2):s(5),s(3):s(6)))
           do l3=s(3),s(6)
           do l2=s(2),s(5)
           do l1=s(1),s(4)
             call form_character_string(cp3(l1,l2,l3),temp_char,pos)
           enddo
           enddo
           enddo
         elseif(jw == 2) then          !jw = 2
           do l3=s(3),s(6)
           do l2=s(2),s(5)
           do l1=s(1),s(4)
             call compare_character_string(cp3(l1,l2,l3),temp_char,pos)
           enddo
           enddo
           enddo
         endif
         deallocate(temp_char)
       else
         call absfadd (iobf,jw,jc,cp3,s(7),iadd)
       endif
     endif
   end subroutine rwd_charp3

   subroutine rwd_charp4(iobf,jw,jc,iadd,cp4,label)
     USE mod_alloc_error
     character*(*)  :: label
     character*(*),dimension(:,:,:,:),pointer :: cp4
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(cp4)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       lc = Len(cp4)
       if(jw /= 0) then
         s(1:4) = LBOUND(cp4)
         s(5:8) = UBOUND(cp4)
         s(9)   = SIZE(cp4)*(1 + (lc-1)/ncpw)
       endif
       call absfadd (iobf,jw,jc,s,9,iadd)
!
       if(jw == 0 .or. jw == 2) then   ! read or compare
         allocate(temp_char(ncpw*s(9)),stat=istat)
         if(istat /= 0) call alloc_error("temp_char","rwd_charp4")
         ioerr = 0
         call rdabsf(iobf,temp_char,s(9),iadd,ioerr)
         iadd = iadd + s(9)
         pos  = 0
         if(jw == 0) then              ! load the array up
           allocate(cp4(s(1):s(5),s(2):s(6),s(3):s(7),s(4):s(8)))
           do l4=s(4),s(8)
           do l3=s(3),s(7)
           do l2=s(2),s(6)
           do l1=s(1),s(5)
             call form_character_string(cp4(l1,l2,l3,l4),temp_char,pos)
           enddo
           enddo
           enddo
           enddo
       elseif(jw == 2) then          !jw = 2
           do l4=s(4),s(8)
           do l3=s(3),s(7)
           do l2=s(2),s(6)
           do l1=s(1),s(5)
             call compare_character_string(cp4(l1,l2,l3,l4),temp_char,pos)
           enddo
           enddo
           enddo
           enddo
         endif
         deallocate(temp_char)
       else
         call absfadd (iobf,jw,jc,cp4,s(9),iadd)
       endif
     endif
   end subroutine rwd_charp4

   subroutine rwd_charp5(iobf,jw,jc,iadd,cp5,label)
     USE mod_alloc_error
     character*(*)  :: label
     character*(*),dimension(:,:,:,:,:),pointer :: cp5
     integer(singI)               :: jw,jc,iadd
     integer(singI),dimension(*)  :: iobf
     logical(singL) :: active
!
     call check_rw_label(iobf,jw,jc,iadd,label)
     active = Associated(cp5)
     call absfadd(iobf,jw,jc, active, 1,iadd)
     if(active) then
       lc = Len(cp5)
       if(jw /= 0) then
         s(1:5 ) = LBOUND(cp5)
         s(6:10) = UBOUND(cp5)
         s(11)   = SIZE(cp5)*(1 + (lc-1)/ncpw)
       endif
       call absfadd (iobf,jw,jc,s,11,iadd)
!
       if(jw == 0 .or. jw == 2) then   ! read or compare
         allocate(temp_char(ncpw*s(11)),stat=istat)
         if(istat /= 0) call alloc_error("temp_char","rwd_charp5")
         ioerr = 0
         call rdabsf(iobf,temp_char,s(11),iadd,ioerr)
         iadd = iadd + s(11)
         pos  = 0
         if(jw == 0) then              ! load the array up
           allocate(cp5(s(1):s(6),s(2):s(7),s(3):s(8), &
                        s(4):s(9),s(5):s(10)))
           do l5=s(5),s(10)
           do l4=s(4),s(9)
           do l3=s(3),s(8)
           do l2=s(2),s(7)
           do l1=s(1),s(6)
             call form_character_string(cp5(l1,l2,l3,l4,l5),temp_char,pos)
           enddo
           enddo
           enddo
           enddo
           enddo
         elseif(jw == 2) then          !jw = 2
           do l5=s(5),s(10)
           do l4=s(4),s(9)
           do l3=s(3),s(8)
           do l2=s(2),s(7)
           do l1=s(1),s(6)
             call compare_character_string(cp5(l1,l2,l3,l4,l5),temp_char,pos)
           enddo
           enddo
           enddo
           enddo
           enddo
         endif
         deallocate(temp_char)
       else
         call absfadd (iobf,jw,jc,cp5,s(11),iadd)
       endif
     endif
   end subroutine rwd_charp5


END module mod_restart
