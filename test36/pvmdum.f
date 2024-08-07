      subroutine pvmdum
#ifndef PVM
c
c PURPOSE:  dummy library of pvm calls. To use pvm, recompile with
c           the entire source with pvm conditional compile flag on
c           and link with your local pvm library.
c
      ! entry pvmfinitsend
      ! entry pvmfpack
      ! entry pvmfbcast
      ! entry pvmfrecv
      ! entry pvmfunpack
      ! entry pvmfgsize
      ! entry pvmfjoingroup
      ! entry pvmfmytid
      ! entry pvmflvgroup
      ! entry pvmfexit
      write(*,*)'            ******* Error ******** '
      write(*,*)
     1    'DYNA3D is not currently complied/linked with PVM library. '
      write(*,*)'          '
      write(*,*)'To use the PVM link option to ATB and MADYMO,'
      write(*,*)'uncomment the PVM options in the makefile dyna3d.mk'
      write(*,*)'and remake the executable. '
      write(*,*)'          '
      write(*,*)'            ******* Error ******** '
      call adios(3)
#endif
      return
      end



      subroutine pvmfinitsend
      write(*,*)'            ******* Error ******** '
      write(*,*)
     1    'DYNA3D is not currently complied/linked with PVM library. '
      write(*,*)'          '
      write(*,*)'To use the PVM link option to ATB and MADYMO,'
      write(*,*)'uncomment the PVM options in the makefile dyna3d.mk'
      write(*,*)'and remake the executable. '
      write(*,*)'          '
      write(*,*)'            ******* Error ******** '
      call adios(3)
      return
      end

      subroutine pvmfpack
      call pvmfinitsend
      return
      end
      
      subroutine pvmfbcast
            call pvmfinitsend
            return
            end

      subroutine pvmfrecv
            call pvmfinitsend
            return
            end

      subroutine pvmfunpack
            call pvmfinitsend
            return
            end

      subroutine pvmfgsize
            call pvmfinitsend
            return
            end

      subroutine pvmfjoingroup
            call pvmfinitsend
            return
            end

      subroutine pvmfmytid
            call pvmfinitsend
            return
            end

      subroutine pvmflvgroup
            call pvmfinitsend
            return
            end

      subroutine pvmfexit
            call pvmfinitsend
            return
            end