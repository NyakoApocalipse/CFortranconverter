      subroutine f3dm25(cm,npc,plc)
c
c.... optional vectorized version
c
c this routine should always run in double precision unless it is on the
c cray.
c
c
      USE mod_control
      USE type_vars
c
      implicit none
!
! Incoming
      integer(singI),dimension(*) :: npc
      real(fullR),dimension(*) :: cm,plc
!
! Local parameter
      real(fullR),parameter :: tol = 1.0d-7
!
! Common blocks
      integer(singI) :: iburn,isdo,iorder
      real(fullR)    :: dt1,dt2,dtx
      common/bk02/dt1,dt2,dtx,iburn,isdo,iorder
!
      real(fullR) :: dd1,dd2,dd3,dd4,dd5,dd6,
     1 wzzdt,wyydt,wxxdt,einc
      common/aux2/dd1(lnv),dd2(lnv),dd3(lnv),dd4(lnv),dd5(lnv),
     1 dd6(lnv),wzzdt(lnv),wyydt(lnv),wxxdt(lnv),einc(lnv)
!
      real(fullR) :: sig1,sig2,sig3,sig4,sig5,sig6,epx1,
     1 epa1,epa2,epa4,epa5,epa6,capa,dam,xmode,charlen,evp
      common/aux14/
     1 sig1(lnv),sig2(lnv),sig3(lnv),sig4(lnv),
     2 sig5(lnv),sig6(lnv),epx1(lnv),
     3 epa1(lnv),epa2(lnv),epa4(lnv),epa5(lnv),epa6(lnv),capa(lnv),
     4 dam(lnv),xmode(lnv),charlen(lnv),evp(lnv)
c... sig1-6 : stress
c... epx1   : equiv. plastic strain (or stored variable)
c... epa1-6 : back stress
c... capa   : kappa
c... dam    : tensile failure damage evolution
c... xmode  : solution mode
c... charlen: element characteristic length
c... evp    : previous plastic volume strain
!
      real(fullR) :: dd,def
      common/aux18/dd(lnv),def(lnv)
!
      integer(singI) :: ix1,ix2,ix3,ix4,ix5,ix6,ix7,ix8,mxt,nmel
      common/aux33/ix1(lnv),ix2(lnv),ix3(lnv),ix4(lnv),ix5(lnv),
     1             ix6(lnv),ix7(lnv),ix8(lnv),mxt(lnv),nmel
!
      real(fullR) :: rhoa,cb,ddavg,p
      common/aux35/rhoa(lnv),cb(lnv),ddavg(lnv),p(lnv)
!
      integer(singI) :: lft,llt
      common/aux36/lft,llt
!
! Local variables
      logical(singL) :: done,kinematic_hardening,saturated,
     & tensile_failure
      logical(singL),dimension(0:11) :: nmode
      logical(singL),dimension(lnv) :: fail

      integer(singI) :: i,i1,i2,iter_dam,ivec,j,ltype,n1,nplot

      real(fullR) :: a1,acons,ak,akt,ale,alphao,alphadt,anorm,asdd,
     1 b1,b2,b3,bd(6),betao,bulk,c1,cbar,
     1 d1d,d2d,d3d,davgr,damax,ddam,ddev1,ddev2,dori,dphit,dtrs,dcur,
     1 epa3,
     2 fac7,fac8,falfac,fcto,fcut,fedum,frate,gamao,
     2 lmod,odda,oddc,odcur,om15,phi_sat,presst,
     2 racon,ratio,regf,rfact,ro,
     3 s,s44,s55,s66,shear,shear2,shear2dt1,sig_cr2,
     3 smin,ss,strs,strengtht,sx1,sx2,
     4 t,tcut,theta,tol_phit,trs,
     5 wo,x1old,xg1,xhat,xkdum,xlen,xlmin,xo
!
      real(fullR) :: gtr11,gtr22,gtr33,gtr12,gtr23,gtr31,
     2 s1,s2,s3,s4,s5,s6, x1tr,x2dtr,x1,x2d,tmises,tcrit
      common/scratch1/gtr11(lnv),gtr22(lnv),gtr33(lnv),gtr12(lnv),
     1 gtr23(lnv),gtr31(lnv),
     2 s1(lnv),s2(lnv),s3(lnv),s4(lnv),s5(lnv),s6(lnv),
     3 x1tr(lnv),x2dtr(lnv),x1(lnv),x2d(lnv),tmises(lnv),tcrit(lnv)
!
      integer(singI) :: imode,iter,iupd2
      real(fullR) :: f1tr,fe,fep,fc2tr,f2tr,capan,capal,xk,evpn,devp,
     3 xkn,elcapn,devpb,x1crt,ptrl,alphad,evol,p1dev,
     5 phit,alpha,gama,beta,effs,rf,fept,fet,davg,fct,r,w,d
      common/scratch2/ 
     1 f1tr(lnv),fe(lnv),fep(lnv),fc2tr(lnv),f2tr(lnv),
     2 capan(lnv),capal(lnv),xk(lnv),evpn(lnv),devp(lnv),
     3 xkn(lnv),elcapn(lnv),devpb(lnv),x1crt(lnv),
     4 ptrl(lnv),alphad(lnv),evol(lnv),p1dev(lnv),
     5 phit(lnv),alpha(lnv),gama(lnv),beta(lnv),
     6 effs(lnv),rf(lnv),fept(lnv),fet(lnv),davg(lnv),
     7 fct(lnv),r(lnv),w(lnv),d(lnv),
     8 imode(lnv),iter(lnv),iupd2(lnv)
!
      real(fullR) :: dhdk,dfdk,elcap,omega,dcd, f,delcap,delam,
     1  fac1,fac2,sdtr11,sdtr22,sdtr33,sdev,fbar,acon,ccon,
     2  strength,press,evmin
      common/scratch3/ 
     1  dhdk(lnv),dfdk(lnv),elcap(lnv),omega(lnv),dcd(lnv),
     2  f(lnv),delcap(lnv),delam(lnv),fac1(lnv),fac2(lnv),
     3  sdtr11(lnv),sdtr22(lnv),sdtr33(lnv),sdev(6,lnv),fbar(lnv),
     4  acon(lnv),ccon(lnv),strength(lnv),press(lnv),evmin(lnv)
!
c
c.... ltype : 1 = soil, 2 = rock
c
      om15   = 1.0d-15
      bulk   = cm(1)
      shear  = cm(2)
      alphao = cm(3)
      theta  = cm(4)
      gamao  = cm(5)
      betao  = cm(6)
      ro     = cm(7)
      dori   = cm(8)
      wo     = cm(9)
      xo     = cm(10)
      nplot  = nint(cm(11))
      ltype  = nint(cm(12))
      tcut   = cm(13)    ! input tensile failure
      fcut   = cm(16)    ! calc Fe/J1 intercept
      cbar   = cm(18)
      falfac = cm(19)
      fcto   = cm(20)    ! principal fail stress 
      frate  = cm(21)    ! fracture energy release
      s      = three
      regf   = cm(22)    ! rate enhancment growth factor
      i1     = cm(48)    ! load curve number for rate effects

      ivec   = 1
      if (cm(17).ne.zero) ivec=0
      alphao = alphao-falfac
      if(fcto .ne. zero) then
        tensile_failure = .true.  ! prin stress failure flag
        t               = fcut    !
      else
        tensile_failure = .false. ! prin stress failure flag
        t               = max(fcut,tcut)
      endif
      ss     = bulk+fourthird*shear
      shear2 = shear*two
!
! Kinematic hardening - subtract off the back stress
      if (falfac .ne. zero) then
        kinematic_hardening = .true.
        do i=lft,llt
          trs=(sig1(i)+sig2(i)+sig3(i))*third
          sdev(1,i)=sig1(i)-trs
          sdev(2,i)=sig2(i)-trs
          sdev(3,i)=sig3(i)-trs
          sdev(4,i)=sig4(i)
          sdev(5,i)=sig5(i)
          sdev(6,i)=sig6(i)
        enddo
        do i=lft,llt
          sig1(i)=sig1(i)-epa1(i)
          sig2(i)=sig2(i)-epa2(i)
          sig3(i)=sig3(i)+epa1(i)+epa2(i)
          sig4(i)=sig4(i)-epa4(i)
          sig5(i)=sig5(i)-epa5(i)
          sig6(i)=sig6(i)-epa6(i)
        enddo
      else
        kinematic_hardening = .false.
      endif
!
! Flip signs to eliminate (create) confusion
      do i=lft,llt
        rf(i)   = one
        effs(i) = one
 	cb(i)	= ss
 	sig1(i) = -sig1(i)
 	sig2(i) = -sig2(i)
 	sig3(i) = -sig3(i)
 	sig4(i) = -sig4(i)
 	sig5(i) = -sig5(i)
 	sig6(i) = -sig6(i)
 	dd1(i)  = -dd1(i)
 	dd2(i)  = -dd2(i)
 	dd3(i)  = -dd3(i)
 	dd4(i)  = -dd4(i)*half  ! comes in as gamma, now tensorial 
 	dd5(i)  = -dd5(i)*half
 	dd6(i)  = -dd6(i)*half
 	iter(i) = 0
 	iupd2(i)= 1
      enddo
!
! Trial stresses: total and deviatoric
      shear2dt1=shear2*dt1
      do i=lft,llt
        evol(i)  = -log(def(i)) - evp(i) ! elastic volume strain : comp is positive
        ptrl(i)  = bulk*evol(i)          ! trial: pressure
        p(i)     = ptrl(i) - third*(sig1(i)+sig2(i)+sig3(i))
        davg(i)  = third*(dd1(i) +dd2(i) +dd3(i)) ! has been mult by -1
        gtr11(i) = sig1(i) + p(i) +shear2dt1*(dd1(i) - davg(i))
        gtr22(i) = sig2(i) + p(i) +shear2dt1*(dd2(i) - davg(i))
        gtr33(i) = sig3(i) + p(i) +shear2dt1*(dd3(i) - davg(i))
        gtr12(i) = sig4(i)        +shear2dt1* dd4(i)
        gtr23(i) = sig5(i)        +shear2dt1* dd5(i)
        gtr31(i) = sig6(i)        +shear2dt1* dd6(i)
!
        x1tr(i)  = three*ptrl(i)       !trial: 3 times pressure
        s1(i)    = gtr11(i) - ptrl(i)  !trial: deviatoric stress
        s2(i)    = gtr22(i) - ptrl(i)
        s3(i)    = gtr33(i) - ptrl(i)
        s4(i)    = gtr12(i)
        s5(i)    = gtr23(i)
        s6(i)    = gtr31(i)
      enddo

!
! Start of rate enhancement
      if (i1.ne.0) then
        i2 = npc(i1)           ! starting pointer for this load curve
        n1 = (npc(i1+1)-i2)/2  ! number of points in load curve
        do i=lft,llt
          davgr   = third*(dd1(i) + dd2(i)+ dd3(i))
          d1d     = -dd1(i) + davgr
          d2d     = -dd2(i) + davgr
          d3d     = -dd3(i) + davgr
          effs(i) = sqrt(threehalf*(d1d**2 + d2d**2 + d3d**2)+
     1            three*(dd4(i)**2 + dd5(i)**2 + dd6(i)**2))
          if(plc(i2) .lt. zero .and. ptrl(i) .lt. zero) 
     1           effs(i) = -effs(i)
        enddo
        call gtpcve(plc(i2),effs,n1,rf,lft,llt)
      endif
!
! Incorporate rate effects into the tensile failure strength
      do i=lft,llt
        fct(i) = max((rf(i)*fcto), fcto)
      enddo
!
! Start of added tensile failure portion
      if(tensile_failure) then
!
        smin     = 1.0d-5*fcto              ! minimum strength
        damax    = 0.9999999d0              ! maximum damage
        tol_phit = 1.0d-10*fcto             ! phit convergence tolerance
!
! Determine damage constants based upon element size and current maximum
        a1   = two*frate*shear2
        c1   = two*frate*s*bulk
        lmod = 0.85d0*min(a1,c1)
        do i=lft,llt
          sig_cr2  = fct(i)*fct(i)
          xlmin    = lmod/sig_cr2            ! min allow crit length
          xlen     = min(charlen(i), xlmin)  ! characteristic length
          acon(i)  = fct(i)*xlen/(two*frate*shear2)
          ccon(i)  = (c1/(xlen*sig_cr2)) - one
          evmin(i) = -two*frate*s/(fct(i)*xlen) ! volumetric failure strain
        enddo
!
! Calculate current degraded strength and adjust tensile trial
! pressure for damage.
        do i=lft,llt
          dcur        = min(damax,dam(i))
          odcur       = one-dcur
          strength(i) = fct(i)*odcur + smin
          if(ptrl(i) < zero) then
            ptrl(i)  = bulk*max(evol(i),evmin(i))
            press(i) = ptrl(i)*odcur/(one+dcur*ccon(i))
          else
            press(i) = ptrl(i)
          endif
        enddo
!
! Calculate the maximum principal stress. This value is positive
! when it is tensile, hence the weird sign convention below.
        do i=lft,llt
c     Compute principal stress
c.... Compute mean and deviatoric (upper triangular part) tensors
          b1     = -press(i)
          bd(1)  = -s1(i)
          bd(2)  = -s2(i)
          bd(3)  = -s3(i) 
          bd(4)  = -s4(i)
          bd(5)  = -s5(i)
          bd(6)  = -s6(i)
c.... Compute 2nd and 3rd invariants of deviator
          s44	= bd(4)*bd(4)
          s55	= bd(5)*bd(5)
          s66	= bd(6)*bd(6)
          b2	= half*(bd(1)*bd(1)+bd(2)*bd(2)+bd(3)*bd(3))
     1            + s44 + s55 + s66

c...Calc deviatoric part of principal stress
          if(b2.le.tol*b1*b1) then
            p1dev(i) = zero
          else
            b3  = bd(1)*bd(2)*bd(3) + two*bd(4)*bd(5)*bd(6)   ! Ed's mods
     1           - bd(1)*s55 - bd(2)*s66 - bd(3)*s44            

c....Set constants 
            xg1      = two*sqrt(b2*third)
            akt      = four*b3/(xg1*xg1*xg1+tol)
            ak       = sign(min(abs(akt), one), akt)
            ale      = third*acos(ak)
            p1dev(i) = xg1*dcos(ale)
          endif
!
! Principal stress and assocaited failure surface
          phit(i)  = (p1dev(i) - press(i)) - strength(i)
          fail(i)  = (phit(i) > tol_phit)
        enddo
c...............................................
c ...Find deviatoric scale factor as necessary .
c...............................................
        do i=lft,llt
          if(fail(i)) then
            ddam  = zero
!
! Solution for when the volume strain is tensile
            if(evol(i) < zero) then
              saturated = (dam(i) >= damax)
!
! Check if a solution exists in the unsaturated regime. Assume
! ddam = damax-dam, and check the sign of phi.
              if(.not.saturated) then
                odcur     = one-damax
                presst    = ptrl(i)*odcur/(one+damax*ccon(i))
                strengtht = fct(i)*odcur + smin
                acons     = acon(i)*strengtht
                alphadt   = acons/(acons+(damax-dam(i)))
                phi_sat   = p1dev(i)*alphadt - presst
     &                    - strengtht
! If phi > 0 when d=damax, the solution requires d > damax
                saturated = (phi_sat > zero)
              endif
!
              if(.not.saturated) then
                dcur  = min(damax,dam(i))   !current d value
                odcur = one-dcur
                Dam_ten: do iter_dam = 1,20
!
! Calculate derivative of phit and update ddam
                  asdd  = acon(i)*strength(i) + ddam
                  oddc  = one+dcur*ccon(i)
                  dphit = -p1dev(i)*acon(i)*(strength(i)+ddam*fct(i))
     &                     /(asdd*asdd)
     &                    + ptrl(i)*(one+ccon(i))/(oddc*oddc)
     &                    + fct(i)
                  ddam = ddam - phit(i)/dphit
                  ddam = min(ddam,one)
!
! Update press and phit
                  dcur        = min(damax,dam(i)+ddam)
                  odcur       = one-dcur
                  press(i)    = ptrl(i)*odcur/(one+dcur*ccon(i))
                  strength(i) = fct(i)*odcur + smin
                  acons       = acon(i)*strength(i)
                  alphad(i)   = acons/(acons+ddam)
                  phit(i)     = p1dev(i)*alphad(i) - press(i)
     &                        - strength(i)
                  done        = (abs(phit(i)) < tol_phit)
                  if(done) exit dam_ten
                enddo Dam_ten
                if(.not.done) then
                  print *,"F3dm25 failed to converge - prin1",i
                endif
!
              else
!
                dcur     = damax
                odcur    = one-dcur
                press(i) = ptrl(i)*odcur/(one+dcur*ccon(i))
                if(p1dev(i) > zero) then
                  alphadt   = (press(i)+fct(i)*odcur+smin)/p1dev(i)
                  alphad(i) = max(zero,alphadt)
                  ddam      = p1dev(i)*acon(i)*(one-alphad(i))
                else
                  alphad(i) = zero
                  ddam      = one
                endif
              endif
!
            else
!
! When the volume strain is compressive
!
! If dam not saturated, try the non-saturated solution.
              if(dam(i) < damax) 
     &          ddam = acon(i)*phit(i)/(one-acon(i)*fct(i))
              dcur = min(damax,dam(i)+ddam)   !current d value
!
! If dcur is saturated, use the saturated solution.
              if(dcur >= damax)
     &          ddam = acon(i)*(
     &                 p1dev(i)-ptrl(i)-fct(i)*(one-damax)-smin )
!
! Divide should be okay since phi_t > zero in the first place.
              alphad(i) = one - ddam/(acon(i)*p1dev(i))
!
            endif
!
            dam(i) = min(one, dam(i) + ddam) !cap dam at one
!
          endif   ! if (fail(i))) then
        enddo
!
! Apply tensile damage scale factors to trial deviatoric stresses
        do i=lft,llt
          if(fail(i)) then
            s1(i) = alphad(i)*s1(i)
            s2(i) = alphad(i)*s2(i)
            s3(i) = alphad(i)*s3(i)
            s4(i) = alphad(i)*s4(i)
            s5(i) = alphad(i)*s5(i)
            s6(i) = alphad(i)*s6(i)
          endif
        enddo
!
! Update all the trial stresses for use in the remaining coding.
! Points that did not violate phi may still have a degraded pressure.
        do i=lft,llt
          sig1(i)  = s1(i) + press(i) !use possibly degraded pressure
          sig2(i)  = s2(i) + press(i)
          sig3(i)  = s3(i) + press(i)
          sig4(i)  = s4(i)          !shear only changes if damage evolves
          sig5(i)  = s5(i)
          sig6(i)  = s6(i)
          gtr11(i) = sig1(i) 
          gtr22(i) = sig2(i) 
          gtr33(i) = sig3(i) 
          gtr12(i) = sig4(i) 
          gtr23(i) = sig5(i) 
          gtr31(i) = sig6(i) 
          x1tr(i)  = gtr11(i) + gtr22(i) + gtr33(i) !trial: 3 times pressure
        enddo
!
! If tensile and fully damaged, do only elastic work
        do i=lft,llt
          if(dam(i) >= damax .and. ptrl(i) <= zero) then
            iupd2(i) = 0
            p(i)     = third*x1tr(i)
          endif
        enddo
!
      endif   ! if(tensile_failure)
c****************************************************************
c              End of added tensile failure portion             *   
c****************************************************************

      do i=lft,llt
       alpha(i)  = rf(i)*alphao
       gama(i)   = rf(i)*gamao
       beta(i)   = betao/rf(i)
       if(regf .gt. zero) then
         rfact   = rf(i)**regf   ! rate enhancment growth factor 
       else
         rfact   = one
       endif
       r(i)      = ro*rfact  
       w(i)      = wo/rfact  
       d(i)      = dori/rfact  
       sdtr11(i) = s1(i)
       sdtr22(i) = s2(i)
       sdtr33(i) = s3(i)
       x2dtr(i)  = half*(s1(i)**2 + s2(i)**2 + s3(i)**2)
     1             + gtr12(i)**2 + gtr23(i)**2 + gtr31(i)**2
       x2dtr(i)  = sqrt(x2dtr(i))                 !trial: Effective shear stress

       fet(i)    = alpha(i) - gama(i)*exp(-beta(i)*t) + theta*t
       fept(i)   = gama(i)*beta(i)*exp(-beta(i)*t) + theta
       fe(i)     = alpha(i) - gama(i)*exp(-beta(i)*capa(i)) + 
     1             theta*capa(i)
       xkn(i)    = capa(i) + r(i)*fe(i)
       elcapn(i) = max(capa(i),zero)
       tmises(i) = abs(xkn(i) - elcapn(i))/r(i)
      enddo

      do  i=lft,llt
        fe(i)    = alpha(i) - gama(i)*exp(-beta(i)*x1tr(i)) + 
     1             theta*x1tr(i)
        f1tr(i)  = x2dtr(i) - min(fe(i),tmises(i))
        tcrit(i) = t - nine*bulk/shear*(x2dtr(i) - fet(i))*fept(i)
        fe(i)    = alpha(i) - gama(i)*exp(-beta(i)*elcapn(i)) + 
     1             theta*elcapn(i)
        fep(i)   = beta(i)*gama(i)*exp(-beta(i)*elcapn(i)) + theta
        fc2tr(i) = ((xkn(i)-elcapn(i))**2-(x1tr(i)-elcapn(i))**2)/
     1               r(i)**2
        f2tr(i)  = x2dtr(i)**2 - fc2tr(i)
        x1crt(i) = elcapn(i) - (x2dtr(i) - fe(i))
     1             *nine*bulk*fep(i)/shear
        fe(i)    = alpha(i) - gama(i)*exp(-beta(i)*capa(i)) + 
     1             theta*capa(i)
        capal(i) = min(zero,capa(i) - tol*fe(i))
        fe(i)    = alpha(i) - gama(i)*exp(-beta(i)*capal(i)) + 
     1             theta*capal(i)
        xk(i)    = capal(i) + r(i)*fe(i)
        evpn(i)  = w(i)*(one - exp(d(i)*(Xo-xkn(i))))
        devpb(i) = w(i)*(one - exp(d(i)*(Xo-xk(i)))) - evpn(i)
        capan(i) = capa(i)
      enddo

c.........................................
c       Set imode logic flag             .
c.........................................
      nmode(0:11) = .false.   ! track which modes are present 
      do 20 i=lft,llt
      if( iupd2(i) == 0 ) then
           imode(i) = 0       ! do no further work
           nmode(0) = .true.
!
! The tensile pressure is less than "t". ("t" is the pressure cut 
! off tcut or where the pressure fe is zero - fcut).
      elseif( (x1tr(i).le. t)  .and. (x2dtr(i).le.fet(i))
     1      .and. (ltype.eq.1) .and. (capan(i).gt.zero)
     2      .and. (devpb(i).ge.zero) ) then
           imode(i) = 2
           nmode(2) = .true.
      elseif( (x1tr(i).le. t)  .and. (x2dtr(i).le.fet(i))
     1      .and. (ltype.eq.1) .and. (capan(i).gt.zero)
     2      .and. (devpb(i).lt.zero) )then
           imode(i) = 7
           nmode(7) = .true.
      elseif( (x1tr(i).le. t)  .and. (x2dtr(i).le.fet(i))
     1   .and. ((ltype.eq.2) .or. (capan(i).le.zero)) )then
           imode(i) = 3
           nmode(3) = .true.
      elseif( (x1tr(i).le. t) .and. (x2dtr(i).gt.fet(i))
     1  .and. (x1tr(i).le.tcrit(i)) .and. (ltype.eq.1)
     2  .and. (capan(i).gt.zero) .and. (devpb(i).ge.zero) ) then
           imode(i) = 4
           nmode(4) = .true.
      elseif( (x1tr(i).le. t) .and. (x2dtr(i).gt.fet(i)) .and.
     1  (x1tr(i).le.tcrit(i)) .and. (ltype.eq.1)
     2  .and. (capan(i).gt.zero) .and. (devpb(i).lt.zero) ) then
           imode(i) = 6
           nmode(6) = .true.
      elseif( (x1tr(i).le. t) .and. (x2dtr(i).gt.fet(i)) .and.
     1  (x1tr(i).le.tcrit(i)) .and. ((ltype.eq.2) .or.
     2  (capan(i).le.zero) ) ) then
           imode(i) = 5
           nmode(5) = .true.
      elseif( (x1tr(i).le. t) .and. (x2dtr(i).gt.fet(i)) .and.
     1  (x1tr(i).gt.tcrit(i)) .and. (capan(i).ge.zero) ) then
           imode(i)  = 10
           nmode(10) = .true.
      elseif( (x1tr(i).le. t) .and. (x2dtr(i).gt.fet(i)) .and.
     1  (x1tr(i).gt.tcrit(i)) .and. (capan(i).lt.zero) ) then
           imode(i) = 8
           nmode(8) = .true.
!
! The pressure is more compressive than "t". ("t" is the pressure cut 
! off tcut or where the pressure fe is zero - fcut).
      elseif( (x1tr(i).gt.  t) .and. (x1tr(i).le.elcapn(i))
     1  .and. (f1tr(i).le.tol) ) then
           imode(i) = 1        !Elastic state
           nmode(1) = .true.
      elseif( (x1tr(i).gt.  t) .and. (x1tr(i).le.elcapn(i))
     1  .and. (f1tr(i).gt.tol) .and. (x1tr(i).ge. x1crt(i)) ) then
           imode(i) = 9        !Violates only f1 - in critical region
           nmode(9) = .true.
      elseif( (x1tr(i).gt.  t) .and. (x1tr(i).le.elcapn(i))
     1  .and. (f1tr(i).gt.tol) .and. (x1tr(i).lt. x1crt(i))
     2  .and. (capan(i).ge.zero) ) then
           imode(i)  = 10      !Violates only f1 - away from critical region
           nmode(10) = .true.
      elseif( (x1tr(i).gt. t) .and. (x1tr(i).le.elcapn(i))
     1  .and. (f1tr(i).gt.tol) .and. (x1tr(i).lt. x1crt(i))
     2  .and. (capan(i).lt.zero) ) then
           imode(i) = 8        !Violates only f1 - away from critical region
           nmode(8) = .true.
      elseif( (x1tr(i).gt. t) .and. (x1tr(i).gt.elcapn(i))
     1  .and. (f2tr(i).lt.tol) ) then
           imode(i) = 1        !Violates ?
           nmode(1) = .true.
      elseif( (x1tr(i).gt. t) .and. (x1tr(i).gt.elcapn(i))
     1  .and. (f2tr(i).ge.tol) ) then
           imode(i)  = 11      !Violates f1 and f2 (fe and fcap)
           nmode(11) = .true.
      else
        if(.not.Control%emergency) then
          Control%emergency911 = i
          Control%emergency    = .true.
          write(* ,1001)
          write(13,1001)
1001    format("Fatal Error: Model 25 dropped through imode logic")
        endif
        return
      endif
   20 continue
c
c.... Elastic mode = 1 
c
      if(nmode(1)) then
        do i=lft,llt
          if(imode(i).eq.1 ) then
            x1(i)   = x1tr(i)
            x2d(i)  = x2dtr(i)
            capa(i) = capan(i)
            p(i)    = third*x1(i)
            sig1(i) = s1(i) + p(i)
            sig2(i) = s2(i) + p(i)
            sig3(i) = s3(i) + p(i)
            sig4(i) = gtr12(i)
            sig5(i) = gtr23(i)
            sig6(i) = gtr31(i)
          endif
        enddo
      endif
c
c.... mode = 2
c
      if(nmode(2)) then
        do i=lft,llt
          if(imode(i).eq.2 ) then
            x1(i)   = t
            x2d(i)  = x2dtr(i)
            capa(i) = zero
            p(i)    = third*x1(i)
            sig1(i) = s1(i) + p(i)
            sig2(i) = s2(i) + p(i)
            sig3(i) = s3(i) + p(i)
            sig4(i) = gtr12(i)
            sig5(i) = gtr23(i)
            sig6(i) = gtr31(i)
          endif
        enddo
      endif
c
c.... mode = 3 tensile failure below fet
c
      if(nmode(3)) then
        do i=lft,llt
          if(imode(i).eq.3 ) then
  	    x1(i)   = t
  	    x2d(i)  = x2dtr(i)
  	    capa(i) = capan(i)
  	    p(i)    = third*x1(i)
  	    sig1(i) = s1(i) + p(i)
  	    sig2(i) = s2(i) + p(i)
  	    sig3(i) = s3(i) + p(i)
  	    sig4(i) = gtr12(i)
  	    sig5(i) = gtr23(i)
  	    sig6(i) = gtr31(i)
          endif
        enddo
      endif
c
c.... mode = 4
c
      if(nmode(4)) then
        do i=lft,llt
          if(imode(i).eq.4 ) then
            x1(i)   = t
            x2d(i)  = fet(i)
            capa(i) = zero
            p(i)    = third*x1(i)
            ratio   = x2d(i)/x2dtr(i)
            sig1(i) = s1(i)*ratio + p(i)
            sig2(i) = s2(i)*ratio + p(i)
            sig3(i) = s3(i)*ratio + p(i)
            sig4(i) = gtr12(i)*ratio
            sig5(i) = gtr23(i)*ratio
            sig6(i) = gtr31(i)*ratio
          endif
        enddo
      endif
c
c.... mode = 5
c
      if(nmode(5)) then
        do i=lft,llt
          if(imode(i).eq.5 ) then
            x1(i)   = t
            x2d(i)  = fet(i)
            capa(i) = capan(i)
            p(i)    = third*x1(i)
            ratio   = x2d(i)/x2dtr(i)
            sig1(i) = s1(i)*ratio + p(i)
            sig2(i) = s2(i)*ratio + p(i)
            sig3(i) = s3(i)*ratio + p(i)
            sig4(i) = gtr12(i)*ratio
            sig5(i) = gtr23(i)*ratio
            sig6(i) = gtr31(i)*ratio
          endif
        enddo
      endif
c
c.... mode = 6
c
      if(nmode(6)) then
        do i=lft,llt
          if (imode(i).eq.6) then
            x1(i)   = t
            x2d(i)  = fet(i)
            x1old   = sig1(i) + sig2(i) + sig3(i)
            devp(i) = dd(i) - (x1(i) - x1old)/(three*bulk)
            xhat    = min(devpb(i),devp(i))
            capa(i) = capa(i) + devp(i)*(capal(i) - capan(i))/xhat
            capa(i) = max(capa(i),zero)
            p(i)    = third*x1(i)
            ratio   = x2d(i)/x2dtr(i)
            sig1(i) = s1(i)*ratio + p(i)
            sig2(i) = s2(i)*ratio + p(i)
            sig3(i) = s3(i)*ratio + p(i)
            sig4(i) = gtr12(i)*ratio
            sig5(i) = gtr23(i)*ratio
            sig6(i) = gtr31(i)*ratio
          endif
        enddo
      endif
c
c.... mode = 7
c
      if(nmode(7)) then
        do i=lft,llt
          if( imode(i).eq.7) then
            x1(i)   = t
            x2d(i)  = x2dtr(i)
            x1old   = sig1(i) + sig2(i) + sig3(i)
            devp(i) =  dd(i) - (x1(i) - x1old)/(three*bulk)
            xhat    = min(devpb(i),devp(i))
            capa(i) = capa(i) + devp(i)*(capal(i) - capan(i))/xhat
            capa(i) = max(capa(i),zero)
            p(i)    = third*x1(i)
            sig1(i) = s1(i) + p(i)
            sig2(i) = s2(i) + p(i)
            sig3(i) = s3(i) + p(i)
            sig4(i) = gtr12(i)
            sig5(i) = gtr23(i)
            sig6(i) = gtr31(i)
          endif
        enddo
      endif
c
c.... mode = 8
c
      if(nmode(8)) then
        do i=lft,llt
          if(imode(i).eq.8 ) then
            x1(i)   = x1tr(i)
            fe(i)   = alpha(i) - gama(i)*exp(-beta(i)*x1(i)) + 
     1                theta*x1(i)
            x2d(i)  = min(fe(i),tmises(i))
            capa(i) = capan(i)
            ratio   = x2d(i)/x2dtr(i)
            p(i)    = third*x1(i)
            sig1(i) = s1(i)*ratio + p(i)
            sig2(i) = s2(i)*ratio + p(i)
            sig3(i) = s3(i)*ratio + p(i)
            sig4(i) = gtr12(i)*ratio
            sig5(i) = gtr23(i)*ratio
            sig6(i) = gtr31(i)*ratio
          endif
        enddo
      endif
c
c.... mode = 9
c
      if(nmode(9)) then
        do i=lft,llt
          if(imode(i).eq.9 ) then
            x1(i)   = elcapn(i)
            x2d(i)  = alpha(i) - gama(i)*exp(-beta(i)*x1(i)) + 
     1                theta*x1(i)
            capa(i) = capan(i)
            ratio   = x2d(i)/x2dtr(i)
            p(i)    = third*x1(i)
            sig1(i) = s1(i)*ratio + p(i)
            sig2(i) = s2(i)*ratio + p(i)
            sig3(i) = s3(i)*ratio + p(i)
            sig4(i) = gtr12(i)*ratio
            sig5(i) = gtr23(i)*ratio
            sig6(i) = gtr31(i)*ratio
          endif
        enddo
      endif
c
c Start of imode=10
c
      if(nmode(10)) then
      if(ivec.eq.0) then
       do 90 i=lft,llt
        if(imode(i).eq.10 ) then
         sig1(i)=gtr11(i)
         sig2(i)=gtr22(i)
         sig3(i)=gtr33(i)
         sig4(i)=gtr12(i)
         sig5(i)=gtr23(i)
         sig6(i)=gtr31(i)
c
      call feit (capan(i),x1tr(i),x2dtr(i),alpha(i),gama(i),beta(i),
     1 theta,w(i),d(i),r(i),bulk,shear,x1(i),capa(i),sig1(i),
     2 sig2(i),sig3(i),sig4(i),sig5(i),sig6(i),iter(i),xo)
c
         if(x1(i) .gt. elcapn(i)) x1(i)=elcapn(i)
         if( (ltype.eq.2) .or. (capan(i).eq.zero) ) capa(i)=capan(i)
         if( x1(i) .gt. capa(i)) capa(i) = x1(i)
         if((ltype.eq.1).and.(capan(i).ge.zero))
     1       capa(i)=max(capa(i),zero)
         fe(i) = alpha(i) - gama(i)*exp(-beta(i)*x1(i)) +theta*x1(i)
         x2d(i)  = min(fe(i),tmises(i))
         ratio   = x2d(i)/x2dtr(i)
         p(i)    = third*x1(i)
         sig1(i) = s1(i)*ratio + p(i)
         sig2(i) = s2(i)*ratio + p(i)
         sig3(i) = s3(i)*ratio + p(i)
         sig4(i) = gtr12(i)*ratio
         sig5(i) = gtr23(i)*ratio
         sig6(i) = gtr31(i)*ratio
        endif
   90  continue
c
      else    !ivec=1 now
!
! This is strictly an update using the failure surface f1 when the 
! pressure is compressive. Note, kappa is also updated.
       do i=lft,llt
         if(imode(i).eq.10 ) then
           sig1(i) = gtr11(i)
           sig2(i) = gtr22(i)
           sig3(i) = gtr33(i)
           sig4(i) = gtr12(i)
           sig5(i) = gtr23(i)
           sig6(i) = gtr31(i)
           capa(i) = capan(i)
           x1(i)   = x1tr(i)
           x2d(i)  = x2dtr(i)
           s1(i)   = sig1(i)-x1tr(i)*third
           s2(i)   = sig2(i)-x1tr(i)*third
           s3(i)   = sig3(i)-x1tr(i)*third
           s4(i)   = sig4(i)
           s5(i)   = sig5(i)
           s6(i)   = sig6(i)
           iter(i) = 4
!
! Do four iterations -
! [Do not make the iteration number a variable. This way the compiler can
!  unroll the loop at compile time if it wants to!]
           do j=1,4
! Current X(kappa)
             xk(i)    = capa(i) + r(i)*(alpha(i) - 
     1                   gama(i)*exp(-beta(i)*capa(i)) +
     2                   theta*capa(i) )
! Needed derivatives
! dhdk = d_Evp/d_kappa = d_Evp/d_X(kappa) * d_X(kappa)/d_kappa
! omega = d_fe/d_J1
! dcd = ?
             dhdk(i)  = w(i)*d(i)*exp(-d(i)*(xk(i)-xo))*
     1                  (one+r(i)*(theta +
     2                    gama(i)*beta(i)*exp(-beta(i)*capa(i))))
             omega(i) = theta+gama(i)*beta(i)*exp(-beta(i)*x1(i))
             dcd(i)   = shear+nine*bulk*omega(i)**2
!
! Evaluate failure surface - f1
             f(i) = x2d(i) - (
     1              alpha(i) - gama(i)*exp( -beta(i)*x1(i) ) +
     2              theta*x1(i) )
! Increment in kappa and current lambda
             delcap(i) = -three*omega(i)*f(i)/(dcd(i)*dhdk(i))
! original   delam(i)  = -dhdk(i)*delcap(i)/(three*omega(i))
             delam(i)  = f(i)/dcd(i)
! Revised kappa
             capa(i) = capa(i)+delcap(i)
!
             xk(i)   = capa(i) + r(i)*(alpha(i) -
     1                  gama(i)*exp(-beta(i)*capa(i)) +
     2                  theta*capa(i) )
             fac1(i) = shear/x2d(i)
             fac2(i) = three*bulk*omega(i)
             sig1(i) = sig1(i)-delam(i)*(fac1(i)*s1(i)-fac2(i))
             sig2(i) = sig2(i)-delam(i)*(fac1(i)*s2(i)-fac2(i))
             sig3(i) = sig3(i)-delam(i)*(fac1(i)*s3(i)-fac2(i))
             sig4(i) = sig4(i)-delam(i)*fac1(i)*s4(i)
             sig5(i) = sig5(i)-delam(i)*fac1(i)*s5(i)
             sig6(i) = sig6(i)-delam(i)*fac1(i)*s6(i)
             x1(i)   = sig1(i)+sig2(i)+sig3(i)
             s1(i)   = sig1(i)-x1(i)*third
             s2(i)   = sig2(i)-x1(i)*third
             s3(i)   = sig3(i)-x1(i)*third
             s4(i)   = sig4(i)
             s5(i)   = sig5(i)
             s6(i)   = sig6(i)
             x2d(i)  = sqrt(half*(s1(i)**2+s2(i)**2+s3(i)**2)
     1                          + s4(i)**2+s5(i)**2+s6(i)**2 )
           enddo
         endif
       enddo
       do 98 i=lft,llt
        if(imode(i).eq.10 ) then
         if(x1(i) .gt. elcapn(i)) x1(i)=elcapn(i)
         if( (ltype.eq.2) .or. (capan(i).eq.zero) ) capa(i)=capan(i)
         if( x1(i) .gt. capa(i)) capa(i) = x1(i)
         if((ltype.eq.1).and.(capan(i).ge.zero))
     1           capa(i)=max(capa(i),zero)
        endif
   98  continue
c
       do 99 i=lft,llt
        if(imode(i).eq.10 ) then
         fe(i)   = alpha(i) - gama(i)*exp(-beta(i)*x1(i)) + 
     1             theta*x1(i)
         x2d(i)  = min(fe(i),tmises(i))
         ratio   = x2d(i)/x2dtr(i)
         p(i)    = third*x1(i)
         sig1(i) = sdtr11(i)*ratio + p(i)
         sig2(i) = sdtr22(i)*ratio + p(i)
         sig3(i) = sdtr33(i)*ratio + p(i)
         sig4(i) = gtr12(i)*ratio
         sig5(i) = gtr23(i)*ratio
         sig6(i) = gtr31(i)*ratio
        endif
   99  continue
      endif
      endif
c
c End of imode=10
c
c Start of imode=11
c
      if(nmode(11)) then
!
! This is strictly an update using the cap surface fc when the 
! pressure is compressive. Note, kappa is also updated.
      if(ivec.eq.0) then
       do 160 i=lft,llt
        if(imode(i).eq.11 ) then
         sig1(i)=gtr11(i)
         sig2(i)=gtr22(i)
         sig3(i)=gtr33(i)
         sig4(i)=gtr12(i)
         sig5(i)=gtr23(i)
         sig6(i)=gtr31(i)
c
      call fcit (capan(i),x1tr(i),x2dtr(i),alpha(i),gama(i),beta(i),
     1 theta,w(i),d(i),r(i),bulk,shear,x1(i),capa(i),sig1(i),
     2 sig2(i),sig3(i),sig4(i),sig5(i),sig6(i),iter(i),xo)
        endif
  160  continue
c
      else    !ivec=1 now
c
       do i=lft,llt
         if(imode(i).eq.11 ) then
           sig1(i) = gtr11(i)
           sig2(i) = gtr22(i)
           sig3(i) = gtr33(i)
           sig4(i) = gtr12(i)
           sig5(i) = gtr23(i)
           sig6(i) = gtr31(i)
           capa(i) = capan(i)
           x1(i)   = x1tr(i)
           x2d(i)  = x2dtr(i)
           s1(i)   = sig1(i)-x1tr(i)*third
           s2(i)   = sig2(i)-x1tr(i)*third
           s3(i)   = sig3(i)-x1tr(i)*third
           s4(i)   = sig4(i)
           s5(i)   = sig5(i)
           s6(i)   = sig6(i)
           iter(i) = 4
! Do not replace the end do-loop value with a variable. This way
! the compiler can unroll the loop if it wants to.
           do j=1,4
             xk(i)   = capa(i)+r(i)*(alpha(i)-gama(i)*
     1                  exp(-beta(i)*capa(i)) + theta*capa(i))
! Needed derivatives
! dhdk = d_Evp/d_kappa = d_Evp/d_X(kappa) * d_X(kappa)/d_kappa
! omega = ?
! dcd = ?
             dhdk(i) = w(i)*d(i)*exp(-d(i)*(xk(i)-xo))*(one+r(i)*
     1                 (theta+gama(i)*beta(i)*exp(-beta(i)*capa(i))))
             if(capa(i).ge.zero) then
                dfdk(i) = -two*(abs(x1(i)-capa(i))/r(i)**2
     1                  + (alpha(i)-gama(i)*exp(-beta(i)*capa(i))
     2                  + theta*capa(i))*(theta+gama(i)*beta(i)*
     3                    exp(-beta(i)*capa(i))))
             else
                dfdk(i) = -two*xk(i)*(one+r(i)*(theta +
     1                      gama(i)*beta(i)*exp(-beta(i)*capa(i))))
     1                        /r(i)**2
             endif
             elcap(i) = max(capa(i),zero)
             omega(i) = min(-two*abs(x1(i)-elcap(i))/r(i)**2,-om15)
             dcd(i)   = four*shear*x2d(i)**2 + 
     1                  nine*bulk*(two*(x1(i) - elcap(i))/r(i)**2)**2
! Current cap surface (in squared form)
             f(i)     = x2d(i)**2 +
     1                 ( (x1(i)-elcap(i))**2 - (xk(i)-elcap(i))**2 )
     1                                   /r(i)**2
! Increment in kappa and the current lambda
             delcap(i) = -f(i)/(dfdk(i)+(dcd(i)*dhdk(i))/
     1                                  (three*omega(i)) )
             delam(i)  = -dhdk(i)*delcap(i)/(three*omega(i))
! Revised Kappa and X(kappa)
             capa(i) = capa(i) + delcap(i)
             xk(i)   = capa(i) + r(i)*(alpha(i) - 
     1                     gama(i)*exp(-beta(i)*capa(i)) +
     1                     theta*capa(i) )
c            elcap=max(capa(i),zero)
             fac1(i) = shear2
             fac2(i) = six*bulk*abs(x1(i)-elcap(i))/r(i)**2
             sig1(i) = sig1(i)-delam(i)*(fac1(i)*s1(i)+fac2(i))
             sig2(i) = sig2(i)-delam(i)*(fac1(i)*s2(i)+fac2(i))
             sig3(i) = sig3(i)-delam(i)*(fac1(i)*s3(i)+fac2(i))
             sig4(i) = sig4(i)-delam(i)*fac1(i)*s4(i)
             sig5(i) = sig5(i)-delam(i)*fac1(i)*s5(i)
             sig6(i) = sig6(i)-delam(i)*fac1(i)*s6(i)
             x1(i)   = sig1(i)+sig2(i)+sig3(i)
             s1(i)   = sig1(i)-x1(i)*third
             s2(i)   = sig2(i)-x1(i)*third
             s3(i)   = sig3(i)-x1(i)*third
             s4(i)   = sig4(i)
             s5(i)   = sig5(i)
             s6(i)   = sig6(i)
             x2d(i)  = sqrt(half*(s1(i)**2+s2(i)**2+s3(i)**2)
     1                          + s4(i)**2+s5(i)**2+s6(i)**2 )
           enddo
           if(x1(i).lt.elcap(i)) then
             x1(i)   = elcap(i)+abs(x1(i)-elcap(i))
             sig1(i) = s1(i)+x1(i)*third
             sig2(i) = s2(i)+x1(i)*third
             sig3(i) = s3(i)+x1(i)*third
           endif
           p(i) = third*(sig1(i)+sig2(i)+sig3(i))
         endif
       enddo
      endif
      endif
c
c.........................................
c Finished with imode branches
c.........................................
      do i=lft,llt
        sig1(i) = -sig1(i)
        sig2(i) = -sig2(i)
        sig3(i) = -sig3(i)
        sig4(i) = -sig4(i)
        sig5(i) = -sig5(i)
        sig6(i) = -sig6(i)
        dd1(i)  = -dd1(i)
        dd2(i)  = -dd2(i)
        dd3(i)  = -dd3(i)
        dd4(i)  = -dd4(i)*two
        dd5(i)  = -dd5(i)*two
        dd6(i)  = -dd6(i)*two
      enddo
!
! Calculate the volumetric plastic strain using the static properties.
! When dynamic ones are used, evp can change with strain rate even if
! when kappa does not change.
      do i=lft,llt
        if(imode(i) .eq. 2) then
          xkdum = alphao - gamao
        else
          fedum = alphao - gamao*exp(-betao*capa(i)) + 
     1              theta*capa(i)
          xkdum = capa(i) + ro*fedum
        endif
        evp(i)   = wo*(one - exp(dori*(Xo-xkdum)))
        xmode(i) = imode(i)
      enddo
c
c Select user specified output variable
c
      if (nplot.eq.0) then
        epx1(lft:llt) = zero
      elseif (nplot.eq.1) then
        epx1(lft:llt) = phit(lft:llt)
      elseif (nplot.eq.2) then
        epx1(lft:llt) = xk(lft:llt)
      elseif (nplot.eq.3) then
        epx1(lft:llt) = ptrl(lft:llt)
      elseif (nplot.eq.4) then
        epx1(lft:llt) = x1(lft:llt)
      elseif (nplot.eq.5) then
        epx1(lft:llt) = alphad(lft:llt)
      elseif (nplot.eq.6) then
        epx1(lft:llt) = tmises(lft:llt)
      elseif (nplot.eq.7) then
        epx1(lft:llt) = fct(lft:llt)
      elseif (nplot.eq.8) then
        epx1(lft:llt) = iter(lft:llt)
      elseif (nplot.eq.9) then
        epx1(lft:llt) = effs(lft:llt)
      elseif (nplot.eq.10) then
        epx1(lft:llt) = evol(lft:llt)
      elseif (nplot.eq.11) then
        epx1(lft:llt) = def(lft:llt)
      endif
c
c.........................................
c For kinematic hardening
c.........................................
      if (kinematic_hardening) then  
       do 600 i=lft,llt
        epa3=-epa1(i)-epa2(i)
        fac7= half*(sig1(i)*epa1(i)+sig2(i)*epa2(i)+sig3(i)*epa3
     1      +two*(sig4(i)*epa4(i)+sig5(i)*epa5(i)+sig6(i)*epa6(i)))
        fac8 = alpha(i) - gama(i)*exp(-beta(i)*x1(i)) + theta*x1(i)
        fbar(i)=max(zero,one-fac7/(falfac*fac8))
  600  continue
       fac7=one/shear2
       do 610 i=lft,llt
        dtrs=(dd1(i)+dd2(i)+dd3(i))*third
        ddev1=dd1(i)-dtrs
        ddev2=dd2(i)-dtrs
        strs=(sig1(i)+sig2(i)+sig3(i))*third
        sx1=sig1(i)-strs
        sx2=sig2(i)-strs
        epa1(i)=epa1(i)+cbar*fbar(i)*(ddev1*dt1
     1          -fac7*(sx1-(sdev(1,i)-epa1(i))))
        epa2(i)=epa2(i)+cbar*fbar(i)*(ddev2*dt1
     1          -fac7*(sx2-(sdev(2,i)-epa2(i))))
        epa4(i)=epa4(i)+cbar*fbar(i)*(dd4(i)/two*dt1
     1          -fac7*(sig4(i)-(sdev(4,i)-epa4(i))))
        epa5(i)=epa5(i)+cbar*fbar(i)*(dd5(i)/two*dt1
     1          -fac7*(sig5(i)-(sdev(5,i)-epa5(i))))
        epa6(i)=epa6(i)+cbar*fbar(i)*(dd6(i)/two*dt1
     1          -fac7*(sig6(i)-(sdev(6,i)-epa6(i))))
  610  continue
c
c.... radial return
c
       do 620 i=lft,llt
        epa3=-epa1(i)-epa2(i)
        anorm=sqrt(half*(epa1(i)**2+epa2(i)**2+epa3**2
     1            +two*(epa4(i)**2+epa5(i)**2+epa6(i)**2)))
        if (anorm .gt. falfac) then
         fac8=falfac/anorm
         epa1(i)=epa1(i)*fac8
         epa2(i)=epa2(i)*fac8
         epa4(i)=epa4(i)*fac8
         epa5(i)=epa5(i)*fac8
         epa6(i)=epa6(i)*fac8
        endif
  620  continue
       do 630 i=lft,llt
        sig1(i)=sig1(i)+epa1(i)
        sig2(i)=sig2(i)+epa2(i)
        sig3(i)=sig3(i)-epa1(i)-epa2(i)
        sig4(i)=sig4(i)+epa4(i)
        sig5(i)=sig5(i)+epa5(i)
        sig6(i)=sig6(i)+epa6(i)
  630  continue
      endif   ! if (kinematic_hardening) then 
c
      return
      end
