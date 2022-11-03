      subroutine addptr(ipointer,nwords,iflag)
#ifdef DEBUG_MEMORY
      USE type_vars !!! TODO: USE shoud remain where they are found, instead of promoted to beginning of file. Should order be rigidly kept? Or we could let fortran files get preprocessed before translation
      implicit none
#include "heapinfo.h"
      integer nwords,iflag
      real(fullR) b(*)
      pointer(ipointer,b)
      integer i

      i=1
      do while (i.le.heapinfosize)
          if (.not.ptrpresent(i)) then
c             empty space, take it
              ptrpresent(i)=.true.
              call ptrcpy(ipointer,ptrheap(i))
              ptrsize(i)=nwords
              ptrtype(i)=iflag
              return
          endif
          i=i+1
      end do
c     no slots left, give up
#endif
      return
      end