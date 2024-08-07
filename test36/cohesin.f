      subroutine cohesin()
c
c Read in cohesive data and elements.
c Write and initizialize data as necessary.
c
!
      USE mod_generic_print_utils
      USE type_vars
!
      implicit none
!
c
c delamination common block
c
      integer(singI) ::
     &      numcoh,ncohd,nccons,icpropc,icnmid,iclinkc,
     &      iccohdat,icixch,icdamg,icfailc,icnhexc,icinum
      common/cohes1/numcoh,ncohd,nccons,icpropc,icnmid,iclinkc,
     &      iccohdat,icixch,icdamg,icfailc,icnhexc,icinum
c
      logical vhsp
      common/shorthsp/vhsp
c
      common/freeinput/txts,lcount,icolumn
      integer(singI) :: lcount,icolumn
      character*80 txts
c
      integer(singI) :: linkc,inum
      dimension linkc(10,*),inum(*)
c
c Local variables
c
      character*1 blank
      save blank
      character*80 key,option,mssg
      integer(singI) :: i,j,k,min_matn,max_matn,mspan,iss,moff
      logical dkey
c
      integer(singI) :: itmp,nspan
      pointer(ipitmp,itmp(10,*))
      pointer(ipnspan,nspan(*))
c
      real(fullR) :: deltsmin
      common/delam_dtmin/deltsmin
c
      data blank/' '/
c
c   To read in the next variable use:
c      call get_iword(ivar)         for integers
c      call get_rword(rvar)         for reals
c      call get_fword(fvar)         for file names
c   To read data from the keyword string use:
c      call get_data(key,0,rdum,idum)  for integer
c      call get_data(key,1,rdum,idum)  for real
c    data is returned as rdum or idum
c
c Get delam defintions from keywords:
c
      entry cohesin1( )
      deltsmin=zero
c
c Prepare to start reading
      icolumn=81
      option=blank
      dkey=.false.
c
5     call getwordz(key)
c
      if    (key(1:8).eq.'cohesive') then
       option=blank
c
      else if(key(1:9).eq.'materials' .or. key(1:4).eq.'defi')then
       call get_iword(ncohd)
c
      else if(key(1:8).eq.'elements' .or. key(1:4).eq.'elem')then
       call get_iword(numcoh)
c
      else if(key(1:5).eq.'tsmin') then
       call get_rword(deltsmin)
c
      else if(key(1:7).eq.'endfree') then
       if(ncohd*numcoh.eq.0) then
        mssg='error in COHESIVE: # of elements or materials is zero'
        call termin (txts,mssg,lcount,1)
       endif
       return
c
      else
       mssg=' error in COHESIVE control cards: unknown option'//
     &      ' or data'
       call termin (txts,mssg,lcount,1)
c
      endif
c
      goto 5
c
c -----------------------------------------------------------------
c
c Read in cohesive definitions
c
      entry cohesin4(linkc,inum)
c
c Prepare to start reading
      icolumn=81
      do i=1,numcoh
       do j=1,10
        call get_iword(linkc(j,i))
       enddo
      enddo
c
c Write out cohesive definitions
      call write_section_title("Cohesive Elements")
      write(13,1100) numcoh
      if(vhsp) then
       write(13,1110)
       do i=1,numcoh
        write(13,1115)(linkc(j,i),j=1,10)
       enddo
      endif
c
c Redo element connectivity - bunch material numbers together
      min_matn=1000000
      max_matn=0
      do i=1,numcoh
       min_matn=min(min_matn,linkc(2,i))
       max_matn=max(max_matn,linkc(2,i))
      enddo
      mspan=max_matn-min_matn+1
      moff=min_matn-1
      call space(mspan,ipnspan,0)    !span array
c
c count how many of each material type there are
      do i=1,numcoh
       nspan(linkc(2,i)-moff)=nspan(linkc(2,i)-moff)+1
      enddo
c
c move elements around for vectorization
      call space(numcoh*10,ipitmp,0)  !temporary space for linkd
      iss=1
      call blkcpi(linkc,itmp,10*numcoh)
      do j=min_matn,max_matn
       if(nspan(j-moff).gt.0) then
        do i=1,numcoh
         if(itmp(2,i).eq.j) then
          do k=1,10
           linkc(k,iss)=itmp(k,i)
          enddo
          inum(itmp(1,i))=iss         !inverse re-order array
          iss=iss+1
         endif
        enddo
       endif
      enddo
c
c free temp space
      call fspace(mspan,ipnspan,0)    !span array
      call fspace(numcoh*10,ipitmp,0) !temporary space for linkd
c
      return
1100  format(/,' Data defined ',i8,' cohesive elements'//)
1110  format(3x,'#',4x, 2x,'mat',3x,' node 1',2x,'node 2',
     & 2x,'node 3',2x,'node 4',2x,'node 5',
     & 2x,'node 6',2x,'node 7',2x,'node 8')
1115  format(10i8)
      end
