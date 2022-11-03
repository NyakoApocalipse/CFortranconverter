      subroutine acmfcs(icsd,mnmxs,nsshs,nsh,myresult,numcsd,nnm1)
      USE type_vars
      implicit real(fullR) (a-h, o-z)
      common/aux36/lft,llt
      common/csfsav/
     &sfx1(lnv),sfy1(lnv),sfz1(lnv),
     &sfx2(lnv),sfy2(lnv),sfz2(lnv),
     &sfx3(lnv),sfy3(lnv),sfz3(lnv),
     &sfx4(lnv),sfy4(lnv),sfz4(lnv),
     &fail(lnv),ndof,ifail

      dimension icsd(3,*),mnmxs(2,*),nsshs(*),nsh(4,*),myresult(9,*)
      k0=0
      nf=nnm1+lft
      nl=nnm1+llt
      do 50 i=1,numcsd
      nnsh=icsd(3,i)
      if (nnsh.eq.0) go to 50
      if(mnmxs(1,i).gt.nl) go to 40
      if(mnmxs(2,i).lt.nf) go to 40
      do 30 l=1,nnsh
      nel =nsshs(l+k0)
      if (nel.ge.nf.and.nel.le.nl) then
      nel =nel-nnml
      if (nsh(1,l).eq.1) then
      myresult(1,i)=myresult(1,i)-fail(nel)*sfx1(nel)
      myresult(2,i)=myresult(2,i)-fail(nel)*sfy1(nel)
      myresult(3,i)=myresult(3,i)-fail(nel)*sfz1(nel)
      endif
      if (nsh(2,l).eq.1) then
      myresult(1,i)=myresult(1,i)-fail(nel)*sfx2(nel)
      myresult(2,i)=myresult(2,i)-fail(nel)*sfy2(nel)
      myresult(3,i)=myresult(3,i)-fail(nel)*sfz2(nel)
      endif
      if (nsh(3,l).eq.1) then
      myresult(1,i)=myresult(1,i)-fail(nel)*sfx3(nel)
      myresult(2,i)=myresult(2,i)-fail(nel)*sfy3(nel)
      myresult(3,i)=myresult(3,i)-fail(nel)*sfz3(nel)
      endif
      if (ndof.eq.4) then
      if (nsh(4,l).eq.1)then
      myresult(1,i)=myresult(1,i)-fail(nel)*sfx4(nel)
      myresult(2,i)=myresult(2,i)-fail(nel)*sfy4(nel)
      myresult(3,i)=myresult(3,i)-fail(nel)*sfz4(nel)
      endif
      endif
      endif
   30 continue
   40 k0=k0+nnsh
   50 continue
      return
      end
