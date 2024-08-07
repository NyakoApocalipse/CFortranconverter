      subroutine hrgfb_pst(x0,hour_eng)
c
c Physical stabilization hourglass method for hyperelastic
c materials. This is the total displacement formulation.
c That is, the total hourglass force is calculated each
c time from the total displacement. No update is used.
c
c This version uses the algorithm:
c  f = B^t K B j y u  where K = J^-t C J^-1
c where B is the strain displacement matrix, j is the
c jacobian at the element center and y is the stabilization
c vector with respect to the reference coordinates.
c
c Alternate version:
c  f = B^t J^-t sig   where sig = C J^-1 B j y u
c is commented out at bottom of source file. This is the
c same as the NIKE3D version.
c
c The aforementioned version is faster on the SGI.
c
      USE type_vars
c
      implicit none
c
      real(fullR) :: hour_eng
c
      integer iburn,isdo,iorder
      real(fullR) :: dt1,dt2,dtx,b12,b2,caq
      common/bk02/dt1,dt2,dtx,iburn,isdo,iorder
      common/bk12/b12,b2,caq
c
      real(fullR) ::       
     & x1,x2,x3,x4,x5,x6,x7,x8,y1,y2,y3,y4,y5,y6,y7,y8,
     & z1,z2,z3,z4,z5,z6,z7,z8
      common/aux8/
     & x1(lnv),x2(lnv),x3(lnv),x4(lnv),        ! n+1 coords
     & x5(lnv),x6(lnv),x7(lnv),x8(lnv),
     & y1(lnv),y2(lnv),y3(lnv),y4(lnv),
     & y5(lnv),y6(lnv),y7(lnv),y8(lnv),
     & z1(lnv),z2(lnv),z3(lnv),z4(lnv),
     & z5(lnv),z6(lnv),z7(lnv),z8(lnv)
c
      real(fullR) ::       vlrho,vol,volint
      common/aux9/vlrho(lnv),vol(lnv),volint(lnv)         ! element volume
c
      real(fullR) ::
     1 Xx1,Xx2,Xx3,Xx4,
     & Xx5,Xx6,Xx7,Xx8,
     2 Xy1,Xy2,Xy3,Xy4,                                                 
     & Xy5,Xy6,Xy7,Xy8,                                                 
     3 Xz1,Xz2,Xz3,Xz4,                                                 
     & Xz5,Xz6,Xz7,Xz8,                                                 
     4 vx1,vx2,vx3,vx4,              ! incremental displacements        
     5 vx5,vx6,vx7,vx8,                                                 
     6 vy1,vy2,vy3,vy4,                                                 
     7 vy5,vy6,vy7,vy8,                                                 
     8 vz1,vz2,vz3,vz4,                                                 
     9 vz5,vz6,vz7,vz8
      common/scratch3/                                                     
     1 Xx1(lnv),Xx2(lnv),Xx3(lnv),Xx4(lnv),
     & Xx5(lnv),Xx6(lnv),Xx7(lnv),Xx8(lnv),
     2 Xy1(lnv),Xy2(lnv),Xy3(lnv),Xy4(lnv),                             
     & Xy5(lnv),Xy6(lnv),Xy7(lnv),Xy8(lnv),                             
     3 Xz1(lnv),Xz2(lnv),Xz3(lnv),Xz4(lnv),                             
     & Xz5(lnv),Xz6(lnv),Xz7(lnv),Xz8(lnv),                             
     4 vx1(lnv),vx2(lnv),vx3(lnv),vx4(lnv),              ! incremental displacements
     5 vx5(lnv),vx6(lnv),vx7(lnv),vx8(lnv),                             
     6 vy1(lnv),vy2(lnv),vy3(lnv),vy4(lnv),                             
     7 vy5(lnv),vy6(lnv),vy7(lnv),vy8(lnv),                             
     8 vz1(lnv),vz2(lnv),vz3(lnv),vz4(lnv),                             
     9 vz5(lnv),vz6(lnv),vz7(lnv),vz8(lnv)                              
c
      real(fullR) ::        
     1 px1,px2,px3,px4,              ! strain disp at center
     & px5,px6,px7,px8,              ! D/Dx(n+1)            
     2 py1,py2,py3,py4,
     & py5,py6,py7,py8,
     3 pz1,pz2,pz3,pz4,
     & pz5,pz6,pz7,pz8
      common/aux10/
     1 px1(lnv),px2(lnv),px3(lnv),px4(lnv),  !Due to load_brick_jacobi_defgrad,
     & px5(lnv),px6(lnv),px7(lnv),px8(lnv),  !this now contains the pxs from
     2 py1(lnv),py2(lnv),py3(lnv),py4(lnv),  !the initial configuration.
     & py5(lnv),py6(lnv),py7(lnv),py8(lnv),
     3 pz1(lnv),pz2(lnv),pz3(lnv),pz4(lnv),
     & pz5(lnv),pz6(lnv),pz7(lnv),pz8(lnv)
c
       integer lft,llt
       common/aux36/lft,llt
c
      real(fullR) :: 
     & sig1,sig2,sig3,sig4,sig5,sig6,
     & sv,v1,eg
      common/aux14/                                       !common lnv*100 long?
     & sig1(lnv),sig2(lnv),sig3(lnv),sig4(lnv),sig5(lnv),sig6(lnv),
     & sv (lnv,8,4),                                      !stabilization vects
     & v1 (lnv,3,4),                                      !hourglass deformation
     & eg (lnv,6,3)                                       !hourglass strains
c
      integer ix1,ix2,ix3,ix4,ix5,ix6,ix7,ix8,mxt,nmel
      common/aux33/ix1(lnv),ix2(lnv),ix3(lnv),ix4(lnv),ix5(lnv),
     1             ix6(lnv),ix7(lnv),ix8(lnv),mxt(lnv),nmel
c
      real(fullR) ::
     & hgx1,hgx2,hgx3,hgx4,hgy1,hgy2,
     & hgy3,hgy4,hgz1,hgz2,hgz3,hgz4,
     & hx1 ,hx2 ,hx3 ,hx4 ,hy1 ,hy2 ,
     & hy3 ,hy4 ,hz1 ,hz2 ,hz3 ,hz4 ,
     & hx1v,hx2v,hx3v,hx4v,hy1v,hy2v,hy3v,hy4v,hz1v,hz2v,hz3v,hz4v,hgwk
      common/scratch2/                                   !common 64x53 long?
     & hgx1(lnv),hgx2(lnv),hgx3(lnv),hgx4(lnv),hgy1(lnv),hgy2(lnv), !temp variable =
     & hgy3(lnv),hgy4(lnv),hgz1(lnv),hgz2(lnv),hgz3(lnv),hgz4(lnv), !h^t * X
     & hx1(lnv) ,hx2(lnv) ,hx3(lnv) ,hx4(lnv) ,hy1(lnv) ,hy2(lnv) ,
     & hy3(lnv) ,hy4(lnv) ,hz1(lnv) ,hz2(lnv) ,hz3(lnv) ,hz4(lnv) ,
     & hx1v(lnv),hx2v(lnv),hx3v(lnv),hx4v(lnv),hy1v(lnv),hy2v(lnv),
     & hy3v(lnv),hy4v(lnv),hz1v(lnv),hz2v(lnv),hz3v(lnv),hz4v(lnv),
     & hgwk(lnv)
c
      integer ortho
      logical hyper,mat_coord,rot1pk,weibull_mat,gpeaks
      common/model_formulation/ortho,hyper,mat_coord,rot1pk,
     & weibull_mat,gpeaks
c
      real(fullR) :: aji6,temp1,ds_ps
      common/scratch1/                                   !common 64x56 long?
     & aji6(6,lnv,6),temp1(6,lnv,6),ds_ps(6,6,lnv) 
c
c Jacobian and its inverse:   Caution - sometimes the common block is read
c                                       in terms of the transpose.
c
c        | dx/ds dy/ds dz/ds |   | J11 J12 J13 |
c J    = | dx/ds dy/ds dz/ds | = | J21 J22 J23 |
c        | dx/ds dy/ds dz/ds |   | J31 J32 J33 |
c
c        | Ji11 Ji12 Ji13 |    Here J is 8 times the actual J
c J^-1 = | Ji21 Ji22 Ji23 |    and J^-1 is 1/8 times the actual J^-1.
c        | Ji31 Ji32 Ji33 |
c
      real(fullR) ::
     1 aj11,aj21,aj31, aj12,aj22,aj32, aj13,aj23,aj33,
     2 aji11,aji21,aji31, aji12,aji22,aji32, aji13,aji23,aji33
c
c Caution: In this routine aj and aji represent the transpose of the:
c          Jacobian and inverse Jacobian. This is accomplised
c          by reading the common block in the "transpose" order.
c
      common/jacobian/                         ! keep for physical stabilization
     1 aj11(lnv),aj21(lnv),aj31(lnv),         !Caution - transposed read
     2 aj12(lnv),aj22(lnv),aj32(lnv),
     3 aj13(lnv),aj23(lnv),aj33(lnv),         ! 8*jacobian transpose
     4 aji11(lnv),aji21(lnv),aji31(lnv),
     5 aji12(lnv),aji22(lnv),aji32(lnv),
     6 aji13(lnv),aji23(lnv),aji33(lnv)       ! 1/8*jacobian transpose inverse
c
      real(fullR) :: sieu,failu,faill
      common/failu/sieu(lnv),failu(lnv),faill(lnv)
c
      integer :: ihg
      common/hour_glass/ihg
!
      integer :: iplas,idum
      real(fullR) :: emu,elam,elamu,dstab
      common/hour_glass_prop/iplas,idum,emu,elam,elamu,dstab(6,6,lnv)
c
      real(fullR) :: x0(3,*)
c
c
      integer i,j,k
      real(fullR) :: 
     & et,en,a1,a2,a3,tempt,tempn,ivol,
     & aji11s,aji22s,aji33s,aji44s,aji55s,aji66s,
     & ajo11,ajo12,ajo13,ajo21,ajo22,ajo23,ajo31,ajo32,ajo33
c
      real(fullR) :: 
     & ett,enn,sixty_four
      data sixty_four/64.0_fullR/
      data ett/0.333333333333333333333_fullR/              ! 1/8 * 8/3
      data enn/0.111111111111111111111_fullR/              ! 1/8 * 8/9
c
c     if hourglass coefficient equals zero, zero p
c
      if(caq.eq.zero) then
        do i=1,24*lnv
          x1(i)=zero
        enddo
        return
      endif
c
c NOTE: h1 and h2 are swapped compared to hrgfb.f for dyna3d and nike3d
c       to be consistent with jacobian matrix calcd in prtal
c
      et=-ett     !want negative of force
      en=-enn
      do 5 i=lft,llt        ! load initial coordinates
       Xx1(i)=x0(1,ix1(i))
       Xy1(i)=x0(2,ix1(i))
       Xz1(i)=x0(3,ix1(i))
       Xx2(i)=x0(1,ix2(i))
       Xy2(i)=x0(2,ix2(i))
       Xz2(i)=x0(3,ix2(i))
       Xx3(i)=x0(1,ix3(i))
       Xy3(i)=x0(2,ix3(i))
       Xz3(i)=x0(3,ix3(i))
       Xx4(i)=x0(1,ix4(i))
       Xy4(i)=x0(2,ix4(i))
       Xz4(i)=x0(3,ix4(i))
       Xx5(i)=x0(1,ix5(i))
       Xy5(i)=x0(2,ix5(i))
       Xz5(i)=x0(3,ix5(i))
       Xx6(i)=x0(1,ix6(i))
       Xy6(i)=x0(2,ix6(i))
       Xz6(i)=x0(3,ix6(i))
       Xx7(i)=x0(1,ix7(i))
       Xy7(i)=x0(2,ix7(i))
       Xz7(i)=x0(3,ix7(i))
       Xx8(i)=x0(1,ix8(i))
       Xy8(i)=x0(2,ix8(i))
    5  Xz8(i)=x0(3,ix8(i))
      do 10 i=lft,llt
      hx1(i)=Xx3(i)-Xx4(i)-Xx7(i)+Xx8(i)
      hx2(i)=Xx2(i)-Xx3(i)-Xx5(i)+Xx8(i)
      hx3(i)=Xx1(i)-Xx4(i)-Xx6(i)+Xx7(i)
      hx4(i)=Xx1(i)-Xx2(i)-Xx5(i)+Xx6(i)
      hy1(i)=Xy3(i)-Xy4(i)-Xy7(i)+Xy8(i)
      hy2(i)=Xy2(i)-Xy3(i)-Xy5(i)+Xy8(i)
      hy3(i)=Xy1(i)-Xy4(i)-Xy6(i)+Xy7(i)
      hy4(i)=Xy1(i)-Xy2(i)-Xy5(i)+Xy6(i)
      hz1(i)=Xz3(i)-Xz4(i)-Xz7(i)+Xz8(i)
      hz2(i)=Xz2(i)-Xz3(i)-Xz5(i)+Xz8(i)
      hz3(i)=Xz1(i)-Xz4(i)-Xz6(i)+Xz7(i)
   10 hz4(i)=Xz1(i)-Xz2(i)-Xz5(i)+Xz6(i)
      do 20 i=lft,llt
      hgx1(i)=hx3(i)+hx2(i)           ! h1^t x
      hgx2(i)=hx3(i)-hx2(i)
      hgx3(i)=hx4(i)-hx1(i)
      hgx4(i)=hx4(i)+hx1(i)           ! h4^t x
      hgy1(i)=hy3(i)+hy2(i)           ! h1^t y
      hgy2(i)=hy3(i)-hy2(i)           
      hgy3(i)=hy4(i)-hy1(i)
      hgy4(i)=hy4(i)+hy1(i)           ! h4^t y
      hgz1(i)=hz3(i)+hz2(i)           ! h1^t z
      hgz2(i)=hz3(i)-hz2(i)
      hgz3(i)=hz4(i)-hz1(i)
   20 hgz4(i)=hz4(i)+hz1(i)           ! h4^t z
c
      do 22 i=lft,llt        ! calc total displacements u(t) = x(t) - x(0)
       x1(i)=x1(i)-Xx1(i)
       y1(i)=y1(i)-Xy1(i)
       z1(i)=z1(i)-Xz1(i)
       x2(i)=x2(i)-Xx2(i)
       y2(i)=y2(i)-Xy2(i)
       z2(i)=z2(i)-Xz2(i)
       x3(i)=x3(i)-Xx3(i)
       y3(i)=y3(i)-Xy3(i)
       z3(i)=z3(i)-Xz3(i)
       x4(i)=x4(i)-Xx4(i)
       y4(i)=y4(i)-Xy4(i)
       z4(i)=z4(i)-Xz4(i)
       x5(i)=x5(i)-Xx5(i)
       y5(i)=y5(i)-Xy5(i)
       z5(i)=z5(i)-Xz5(i)
       x6(i)=x6(i)-Xx6(i)
       y6(i)=y6(i)-Xy6(i)
       z6(i)=z6(i)-Xz6(i)
       x7(i)=x7(i)-Xx7(i)
       y7(i)=y7(i)-Xy7(i)
       z7(i)=z7(i)-Xz7(i)
       x8(i)=x8(i)-Xx8(i)
       y8(i)=y8(i)-Xy8(i)
   22  z8(i)=z8(i)-Xz8(i)
c
c  ... calc 8*gamma stabilization vectors (8 will cancel later)
c
      do i=lft,llt
       sv(i,1,1)= one-hgx1(i)*px1(i)-hgy1(i)*py1(i)-hgz1(i)*pz1(i)  ! h1 - h1^t xi bi(0)
       sv(i,2,1)= one-hgx1(i)*px2(i)-hgy1(i)*py2(i)-hgz1(i)*pz2(i)
       sv(i,3,1)=-one-hgx1(i)*px3(i)-hgy1(i)*py3(i)-hgz1(i)*pz3(i)
       sv(i,4,1)=-one-hgx1(i)*px4(i)-hgy1(i)*py4(i)-hgz1(i)*pz4(i)
       sv(i,1,2)= one-hgx2(i)*px1(i)-hgy2(i)*py1(i)-hgz2(i)*pz1(i)  ! h2 - h2^t xi bi(0)
       sv(i,2,2)=-one-hgx2(i)*px2(i)-hgy2(i)*py2(i)-hgz2(i)*pz2(i)
       sv(i,3,2)= one-hgx2(i)*px3(i)-hgy2(i)*py3(i)-hgz2(i)*pz3(i)
       sv(i,4,2)=-one-hgx2(i)*px4(i)-hgy2(i)*py4(i)-hgz2(i)*pz4(i)
      enddo
      do i=lft,llt
       sv(i,1,3)= one-hgx3(i)*px1(i)-hgy3(i)*py1(i)-hgz3(i)*pz1(i)  ! h3 - h3^t xi bi(0)
       sv(i,2,3)=-one-hgx3(i)*px2(i)-hgy3(i)*py2(i)-hgz3(i)*pz2(i)
       sv(i,3,3)=-one-hgx3(i)*px3(i)-hgy3(i)*py3(i)-hgz3(i)*pz3(i)
       sv(i,4,3)= one-hgx3(i)*px4(i)-hgy3(i)*py4(i)-hgz3(i)*pz4(i)
       sv(i,1,4)= one-hgx4(i)*px1(i)-hgy4(i)*py1(i)-hgz4(i)*pz1(i)  ! h4 - h4^t xi bi(0)
       sv(i,2,4)=-one-hgx4(i)*px2(i)-hgy4(i)*py2(i)-hgz4(i)*pz2(i)
       sv(i,3,4)= one-hgx4(i)*px3(i)-hgy4(i)*py3(i)-hgz4(i)*pz3(i)
       sv(i,4,4)=-one-hgx4(i)*px4(i)-hgy4(i)*py4(i)-hgz4(i)*pz4(i)
      enddo
c
      if(ihg.eq.8) then
        do i=lft,llt
          sv(i,5,1)=-two-sv(i,3,1)
          sv(i,6,1)=-two-sv(i,4,1)
          sv(i,7,1)= two-sv(i,1,1)
          sv(i,8,1)= two-sv(i,2,1)
          sv(i,5,2)= two-sv(i,3,2)
          sv(i,6,2)=-two-sv(i,4,2)
          sv(i,7,2)= two-sv(i,1,2)
          sv(i,8,2)=-two-sv(i,2,2)
        enddo
        do i=lft,llt
          sv(i,5,3)=-two-sv(i,3,3)
          sv(i,6,3)= two-sv(i,4,3)
          sv(i,7,3)= two-sv(i,1,3)
          sv(i,8,3)=-two-sv(i,2,3)
          sv(i,5,4)=-sv(i,3,4)
          sv(i,6,4)=-sv(i,4,4)
          sv(i,7,4)=-sv(i,1,4)
          sv(i,8,4)=-sv(i,2,4)
        enddo
      elseif(ihg.eq.10) then
        do i=lft,llt
         sv(i,5,1)=-one-hgx1(i)*px5(i)-hgy1(i)*py5(i)-hgz1(i)*pz5(i)
         sv(i,6,1)=-one-hgx1(i)*px6(i)-hgy1(i)*py6(i)-hgz1(i)*pz6(i)
         sv(i,7,1)= one-hgx1(i)*px7(i)-hgy1(i)*py7(i)-hgz1(i)*pz7(i)
         sv(i,8,1)= one-hgx1(i)*px8(i)-hgy1(i)*py8(i)-hgz1(i)*pz8(i)
         sv(i,5,2)= one-hgx2(i)*px5(i)-hgy2(i)*py5(i)-hgz2(i)*pz5(i)
         sv(i,6,2)=-one-hgx2(i)*px6(i)-hgy2(i)*py6(i)-hgz2(i)*pz6(i)
         sv(i,7,2)= one-hgx2(i)*px7(i)-hgy2(i)*py7(i)-hgz2(i)*pz7(i)
         sv(i,8,2)=-one-hgx2(i)*px8(i)-hgy2(i)*py8(i)-hgz2(i)*pz8(i)
        enddo
        do i=lft,llt
         sv(i,5,3)=-one-hgx3(i)*px5(i)-hgy3(i)*py5(i)-hgz3(i)*pz5(i)
         sv(i,6,3)= one-hgx3(i)*px6(i)-hgy3(i)*py6(i)-hgz3(i)*pz6(i)
         sv(i,7,3)= one-hgx3(i)*px7(i)-hgy3(i)*py7(i)-hgz3(i)*pz7(i)
         sv(i,8,3)=-one-hgx3(i)*px8(i)-hgy3(i)*py8(i)-hgz3(i)*pz8(i)
         sv(i,5,4)=-one-hgx4(i)*px5(i)-hgy4(i)*py5(i)-hgz4(i)*pz5(i)
         sv(i,6,4)= one-hgx4(i)*px6(i)-hgy4(i)*py6(i)-hgz4(i)*pz6(i)
         sv(i,7,4)=-one-hgx4(i)*px7(i)-hgy4(i)*py7(i)-hgz4(i)*pz7(i)
         sv(i,8,4)= one-hgx4(i)*px8(i)-hgy4(i)*py8(i)-hgz4(i)*pz8(i)
        enddo
      endif
c
      do 40 i=lft,llt                                               ! calc y^t*u where
      hx1v(i)=x1(i)*sv(i,1,1)+x2(i)*sv(i,2,1)                        ! y is hourglass mode
     &       +x3(i)*sv(i,3,1)+x4(i)*sv(i,4,1)                        ! and u is the total
     &       +x5(i)*sv(i,5,1)+x6(i)*sv(i,6,1)                        ! displacement
     &       +x7(i)*sv(i,7,1)+x8(i)*sv(i,8,1)
      hx2v(i)=x1(i)*sv(i,1,2)+x2(i)*sv(i,2,2)
     &       +x3(i)*sv(i,3,2)+x4(i)*sv(i,4,2)
     &       +x5(i)*sv(i,5,2)+x6(i)*sv(i,6,2)
     &       +x7(i)*sv(i,7,2)+x8(i)*sv(i,8,2)
40    hx3v(i)=x1(i)*sv(i,1,3)+x2(i)*sv(i,2,3)
     &       +x3(i)*sv(i,3,3)+x4(i)*sv(i,4,3)
     &       +x5(i)*sv(i,5,3)+x6(i)*sv(i,6,3)
     &       +x7(i)*sv(i,7,3)+x8(i)*sv(i,8,3)
      do 45 i=lft,llt
      hx4v(i)=x1(i)*sv(i,1,4)+x2(i)*sv(i,2,4)
     &       +x3(i)*sv(i,3,4)+x4(i)*sv(i,4,4)
     &       +x5(i)*sv(i,5,4)+x6(i)*sv(i,6,4)
     &       +x7(i)*sv(i,7,4)+x8(i)*sv(i,8,4)
      hy1v(i)=y1(i)*sv(i,1,1)+y2(i)*sv(i,2,1)
     &       +y3(i)*sv(i,3,1)+y4(i)*sv(i,4,1)
     &       +y5(i)*sv(i,5,1)+y6(i)*sv(i,6,1)
     &       +y7(i)*sv(i,7,1)+y8(i)*sv(i,8,1)
45    hy2v(i)=y1(i)*sv(i,1,2)+y2(i)*sv(i,2,2)
     &       +y3(i)*sv(i,3,2)+y4(i)*sv(i,4,2)
     &       +y5(i)*sv(i,5,2)+y6(i)*sv(i,6,2)
     &       +y7(i)*sv(i,7,2)+y8(i)*sv(i,8,2)
      do 50 i=lft,llt
      hy3v(i)=y1(i)*sv(i,1,3)+y2(i)*sv(i,2,3)
     &       +y3(i)*sv(i,3,3)+y4(i)*sv(i,4,3)
     &       +y5(i)*sv(i,5,3)+y6(i)*sv(i,6,3)
     &       +y7(i)*sv(i,7,3)+y8(i)*sv(i,8,3)
      hy4v(i)=y1(i)*sv(i,1,4)+y2(i)*sv(i,2,4)
     &       +y3(i)*sv(i,3,4)+y4(i)*sv(i,4,4)
     &       +y5(i)*sv(i,5,4)+y6(i)*sv(i,6,4)
     &       +y7(i)*sv(i,7,4)+y8(i)*sv(i,8,4)
50    hz1v(i)=z1(i)*sv(i,1,1)+z2(i)*sv(i,2,1)
     &       +z3(i)*sv(i,3,1)+z4(i)*sv(i,4,1)
     &       +z5(i)*sv(i,5,1)+z6(i)*sv(i,6,1)
     &       +z7(i)*sv(i,7,1)+z8(i)*sv(i,8,1)
      do 60 i=lft,llt
      hz2v(i)=z1(i)*sv(i,1,2)+z2(i)*sv(i,2,2)
     &       +z3(i)*sv(i,3,2)+z4(i)*sv(i,4,2)
     &       +z5(i)*sv(i,5,2)+z6(i)*sv(i,6,2)
     &       +z7(i)*sv(i,7,2)+z8(i)*sv(i,8,2)
      hz3v(i)=z1(i)*sv(i,1,3)+z2(i)*sv(i,2,3)
     &       +z3(i)*sv(i,3,3)+z4(i)*sv(i,4,3)
     &       +z5(i)*sv(i,5,3)+z6(i)*sv(i,6,3)
     &       +z7(i)*sv(i,7,3)+z8(i)*sv(i,8,3)
60    hz4v(i)=z1(i)*sv(i,1,4)+z2(i)*sv(i,2,4)
     &       +z3(i)*sv(i,3,4)+z4(i)*sv(i,4,4)
     &       +z5(i)*sv(i,5,4)+z6(i)*sv(i,6,4)
     &       +z7(i)*sv(i,7,4)+z8(i)*sv(i,8,4)
c
      if(ortho.gt.0) then
c
c  ... calc [J]^-1 ( actually this is 1/64 [J]^-1, 64 will cancel later)
c
       do 65 i=lft,llt                                  ! 54 ops
c    ... aji6(i,1:3,1:3)
        aji6(1,i,1) = aji11(i) * aji11(i)
        aji6(1,i,2) = aji21(i) * aji21(i)
        aji6(1,i,3) = aji31(i) * aji31(i)
        aji6(2,i,1) = aji12(i) * aji12(i)
        aji6(2,i,2) = aji22(i) * aji22(i)
        aji6(2,i,3) = aji32(i) * aji32(i)
        aji6(3,i,1) = aji13(i) * aji13(i)
        aji6(3,i,2) = aji23(i) * aji23(i)
        aji6(3,i,3) = aji33(i) * aji33(i)
c    ... aji6,1(i:3,4:6)
        aji6(1,i,4) = aji11(i) * aji21(i)
        aji6(1,i,5) = aji21(i) * aji31(i)
        aji6(1,i,6) = aji31(i) * aji11(i)
        aji6(2,i,4) = aji12(i) * aji22(i)
        aji6(2,i,5) = aji22(i) * aji32(i)
        aji6(2,i,6) = aji32(i) * aji12(i)
        aji6(3,i,4) = aji13(i) * aji23(i)
        aji6(3,i,5) = aji23(i) * aji33(i)
        aji6(3,i,6) = aji33(i) * aji13(i)
c    ... aji6,4(i:6,1:6)
        aji6(4,i,1) = aji11(i) * aji12(i) *two
        aji6(4,i,2) = aji21(i) * aji22(i) *two
        aji6(4,i,3) = aji31(i) * aji32(i) *two
        aji6(5,i,1) = aji12(i) * aji13(i) *two
        aji6(5,i,2) = aji22(i) * aji23(i) *two
        aji6(5,i,3) = aji32(i) * aji33(i) *two
        aji6(6,i,1) = aji13(i) * aji11(i) *two
        aji6(6,i,2) = aji23(i) * aji21(i) *two
        aji6(6,i,3) = aji33(i) * aji31(i) *two
c    ... aji6,4(i:6,4:6)
        aji6(4,i,4) = aji11(i) * aji22(i)  +  aji21(i) * aji12(i)
        aji6(4,i,5) = aji21(i) * aji32(i)  +  aji31(i) * aji22(i)
        aji6(4,i,6) = aji31(i) * aji12(i)  +  aji11(i) * aji32(i)
        aji6(5,i,4) = aji12(i) * aji23(i)  +  aji22(i) * aji13(i)
        aji6(5,i,5) = aji22(i) * aji33(i)  +  aji32(i) * aji23(i)
        aji6(5,i,6) = aji32(i) * aji13(i)  +  aji12(i) * aji33(i)
        aji6(6,i,4) = aji13(i) * aji21(i)  +  aji23(i) * aji11(i)
        aji6(6,i,5) = aji23(i) * aji31(i)  +  aji33(i) * aji21(i)
        aji6(6,i,6) = aji33(i) * aji11(i)  +  aji13(i) * aji31(i)
65     continue
c____________________________________________________________________________________
c
c  ... [K] = [J]^t C [J]^-1
c
c                 orthotropic D matrix - all elements the same
       if(ortho.eq.1) then
        do k=1,6                                    ! 72 ops
          do i=lft,llt
            temp1(1,i,k)=dstab(1,1,1)*aji6(1,i,k)+
     &                   dstab(1,2,1)*aji6(2,i,k)+
     &                   dstab(1,3,1)*aji6(3,i,k) 
            temp1(2,i,k)=dstab(2,1,1)*aji6(1,i,k)+
     &                   dstab(2,2,1)*aji6(2,i,k)+
     &                   dstab(2,3,1)*aji6(3,i,k) 
            temp1(3,i,k)=dstab(3,1,1)*aji6(1,i,k)+
     &                   dstab(3,2,1)*aji6(2,i,k)+
     &                   dstab(3,3,1)*aji6(3,i,k) 
            temp1(4,i,k)=dstab(4,4,1)*aji6(4,i,k) 
            temp1(5,i,k)=dstab(5,5,1)*aji6(5,i,k) 
            temp1(6,i,k)=dstab(6,6,1)*aji6(6,i,k)
          enddo
        enddo
c                 orthotropic D matrix - all elements are different
! Not presently used for any model
       elseif(ortho.eq.2) then
        do k=1,6                                    ! 72 ops
          do i=lft,llt
            temp1(1,i,k)=dstab(1,1,i)*aji6(1,i,k)+
     &                   dstab(1,2,i)*aji6(2,i,k)+
     &                   dstab(1,3,i)*aji6(3,i,k) 
            temp1(2,i,k)=dstab(2,1,i)*aji6(1,i,k)+
     &                   dstab(2,2,i)*aji6(2,i,k)+
     &                   dstab(2,3,i)*aji6(3,i,k) 
            temp1(3,i,k)=dstab(3,1,i)*aji6(1,i,k)+
     &                   dstab(3,2,i)*aji6(2,i,k)+
     &                   dstab(3,3,i)*aji6(3,i,k) 
            temp1(4,i,k)=dstab(4,4,i)*aji6(4,i,k) 
            temp1(5,i,k)=dstab(5,5,i)*aji6(5,i,k) 
            temp1(6,i,k)=dstab(6,6,i)*aji6(6,i,k)
          enddo
        enddo
c                           Anisotropic D matrix - all elements are the same
      elseif(ortho.eq.3) then
        do j=1,6                                    ! 216 ops
          do k=1,6
            do i=lft,llt
              temp1(j,i,k)=dstab(1,j,1)*aji6(1,i,k)+
     &                     dstab(2,j,1)*aji6(2,i,k)+
     &                     dstab(3,j,1)*aji6(3,i,k)+
     &                     dstab(4,j,1)*aji6(4,i,k)+
     &                     dstab(5,j,1)*aji6(5,i,k)+
     &                     dstab(6,j,1)*aji6(6,i,k)
            enddo
          enddo
        enddo
c                           Anisotropic D matrix - all elements different
! Not presently used for any model
      else
        do j=1,6
          do k=1,6
            do i=lft,llt
              temp1(j,i,k)=dstab(1,j,i)*aji6(1,i,k)+
     &                     dstab(2,j,i)*aji6(2,i,k)+
     &                     dstab(3,j,i)*aji6(3,i,k)+
     &                     dstab(4,j,i)*aji6(4,i,k)+
     &                     dstab(5,j,i)*aji6(5,i,k)+
     &                     dstab(6,j,i)*aji6(6,i,k)
            enddo
          enddo
        enddo
c
       endif
c
c Finish other half
       do j=1,6                                    ! 120 ops
         do k=j,6
           do i=lft,llt
             ds_ps(j,k,i)=aji6(1,i,j)*temp1(1,i,k)+       ! ds_ps is upper diag
     &                    aji6(2,i,j)*temp1(2,i,k)+
     &                    aji6(3,i,j)*temp1(3,i,k)+
     &                    aji6(4,i,j)*temp1(4,i,k)+
     &                    aji6(5,i,j)*temp1(5,i,k)+
     &                    aji6(6,i,j)*temp1(6,i,k)
           enddo
         enddo
       enddo
c
c
      else           ! Isotropic
        do i=lft,llt    !Initial Jacobian in reference coord.
c          ivol = one/(
c     &           aji11(i)*(aji22(i)*aji33(i)-aji23(i)*aji32(i))
c     &         + aji12(i)*(aji31(i)*aji23(i)-aji21(i)*aji33(i))
c     &         + aji13(i)*(aji21(i)*aji32(i)-aji22(i)*aji31(i)) )
c Cheaper to do it this way!          
          ivol  = sixty_four*volint(i)
          ajo11 = (aji22(i)*aji33(i) - aji23(i)*aji32(i))*ivol
          ajo12 = (aji13(i)*aji32(i) - aji12(i)*aji33(i))*ivol
          ajo13 = (aji12(i)*aji23(i) - aji13(i)*aji22(i))*ivol
          ajo21 = (aji23(i)*aji31(i) - aji21(i)*aji33(i))*ivol
          ajo22 = (aji11(i)*aji33(i) - aji13(i)*aji31(i))*ivol
          ajo23 = (aji13(i)*aji21(i) - aji11(i)*aji23(i))*ivol
          ajo31 = (aji21(i)*aji32(i) - aji22(i)*aji31(i))*ivol
          ajo32 = (aji12(i)*aji31(i) - aji11(i)*aji32(i))*ivol
          ajo33 = (aji11(i)*aji22(i) - aji12(i)*aji21(i))*ivol
!         Inverted diagonalized initial jacobian - works better in bending
!         than the diagonalized inverse initial jacobian.
          aji11s       = one/(ajo11*ajo11+ajo21*ajo21+ajo31*ajo31)                ! = j1
          aji22s       = one/(ajo12*ajo12+ajo22*ajo22+ajo32*ajo32)                ! = j2        
          aji33s       = one/(ajo13*ajo13+ajo23*ajo23+ajo33*ajo33)                ! = j3        
          ds_ps(1,1,i) = aji11s*aji11s * elamu            ! j1*j1
          ds_ps(2,2,i) = aji22s*aji22s * elamu            ! j2*j2
          ds_ps(3,3,i) = aji33s*aji33s * elamu            ! j3*j3
          aji44s       = aji11s*aji22s                    ! j1*j2 = j4*j4
          aji55s       = aji22s*aji33s                    ! j2*j3 = j5*j5
          aji66s       = aji33s*aji11s                    ! j3*j1 = j6*j6
          ds_ps(1,2,i) = aji44s * elam                    ! ds_ps is upper diag
          ds_ps(1,3,i) = aji66s * elam
          ds_ps(2,3,i) = aji55s * elam
          ds_ps(4,4,i) = aji44s * emu
          ds_ps(5,5,i) = aji55s * emu
          ds_ps(6,6,i) = aji66s * emu 
        enddo 
      endif
c____________________________________________________________________________________
c
c      elseif(mte.eq.63) then  !this needs to be carefully checked. 
! It is only a place holder!!!!!
c        do i=lft,llt                                        ! ds_ps is lower diag
c          aji11(i)   = one/(aj11(i)*aj11(i)+aj21(i)*aj21(i)+
c     &                      aj31(i)*aj31(i))                ! = j1
c          aji22(i)   = one/(aj12(i)*aj12(i)+aj22(i)*aj22(i)+
c     &                      aj32(i)*aj32(i))                ! = j2        
c          aji33(i)   = one/(aj13(i)*aj13(i)+aj23(i)*aj23(i)+
c     &                      aj33(i)*aj33(i))                ! = j3        
c          ds_ps11(i) = aji11(i)*aji11(i) * elamu            ! j1*j1
c          ds_ps22(i) = aji22(i)*aji22(i) * elamu            ! j2*j2
c          ds_ps33(i) = aji33(i)*aji33(i) * elamu            ! j3*j3
c          aji44s     = aji11(i)*aji22(i)                    ! j1*j2 = j4*j4
c          aji55s     = aji22(i)*aji33(i)                    ! j2*j3 = j5*j5
c          aji66s     = aji33(i)*aji11(i)                    ! j3*j1 = j6*j6
c          ds_ps21(i) = aji44s * elam
c          ds_ps31(i) = aji66s * elam
c          ds_ps32(i) = aji55s * elam
c          ds_ps44(i) = aji44s * emu
c          ds_ps55(i) = aji55s * emu
c          ds_ps66(i) = aji66s * emu 
c        enddo 
c____________________________________________________________________________________
c
c
c
c
c____________________________________________________________________________________
c
c                                    |j11 j21 j31| |hxj|
c  ... v1(1:3,j) = [j]^t [yj]^t du = |j12 j22 j32| |hyj| (again j is 8*j, 8 will cancel)
c                                    |j13 j23 j33| |hzj|
       do 70 i=lft,llt                                  ! 32 ops
        v1(i,1,1)=aj11(i)*hx1v(i) + aj21(i)*hy1v(i) + aj31(i)*hz1v(i)
        v1(i,2,1)=aj12(i)*hx1v(i) + aj22(i)*hy1v(i) + aj32(i)*hz1v(i)
        v1(i,3,1)=aj13(i)*hx1v(i) + aj23(i)*hy1v(i) + aj33(i)*hz1v(i)
        v1(i,1,2)=aj11(i)*hx2v(i) + aj21(i)*hy2v(i) + aj31(i)*hz2v(i)
        v1(i,2,2)=aj12(i)*hx2v(i) + aj22(i)*hy2v(i) + aj32(i)*hz2v(i)
        v1(i,3,2)=aj13(i)*hx2v(i) + aj23(i)*hy2v(i) + aj33(i)*hz2v(i)
        v1(i,1,3)=aj11(i)*hx3v(i) + aj21(i)*hy3v(i) + aj31(i)*hz3v(i)
        v1(i,2,3)=aj12(i)*hx3v(i) + aj22(i)*hy3v(i) + aj32(i)*hz3v(i)
        v1(i,3,3)=aj13(i)*hx3v(i) + aj23(i)*hy3v(i) + aj33(i)*hz3v(i)
        v1(i,1,4)=aj11(i)*hx4v(i) + aj21(i)*hy4v(i) + aj31(i)*hz4v(i)
        v1(i,2,4)=aj12(i)*hx4v(i) + aj22(i)*hy4v(i) + aj32(i)*hz4v(i)
        v1(i,3,4)=aj13(i)*hx4v(i) + aj23(i)*hy4v(i) + aj33(i)*hz4v(i)
70     continue     
c____________________________________________________________________________________
c 
c  ... eg(i,1:6,1) = [K] ( [B](1,1) [j]^t [y1]^t du + [B](2,1) [j]^t [y2]^t du)
c 
c   where            |  0   0   0  |                      |  0   0   0  |  
c                    |  0   1   0  |                      |  0   0   0  |  
c         [B](1,1) = |  0   0   0  |           [B](2,1) = |  0   0   1  |  
c                    |  0   0   0  |                      |  0   0   0  |  
c                    |  0   0   1  |                      |  0   1   0  |  
c                    |  0   0   0  |                      |  0   0   0  |  
                                        
c  and               |  0  j12 j15 |                      |  0  j15 j13 |
c                    |  0  j22 j25 |                      |  0  j25 j23 |
c  [J]^-1 [B](1,1) = |  0  j32 j35 |    [J]^-1 [B](2,1) = |  0  j35 j33 |
c                    |  0  j42 j45 |                      |  0  j45 j43 |
c                    |  0  j52 j55 |                      |  0  j55 j53 |
c                    |  0  j62 j65 |                      |  0  j65 j63 |

c  then strain in global coords eg given by
 
c  ... eg(i,1:6,1) = { [J]^-1 [B](1,1) } v1(i,1:3,1) + { [J]^-1 [B](2,1) } v1(i,1:3,2)
c____________________________________________________________________________________
c 
c  ... eg(i,1:6,2) = [J]^-1 ( [B](1,2) [j]^t [y1]^t du + [B](3,2) [j]^t [y3]^t du)
c 
c   where            |  1   0   0  |                      |  0   0   0  |  
c                    |  0   0   0  |                      |  0   0   0  |  
c         [B](1,2) = |  0   0   0  |           [B](3,2) = |  0   0   1  |  
c                    |  0   0   0  |                      |  0   0   0  |  
c                    |  0   0   0  |                      |  0   0   0  |  
c                    |  0   0   1  |                      |  1   0   0  |  
                                        
c  and               | j11  0  j16 |                      | j16  0  j13 |
c                    | j21  0  j26 |                      | j26  0  j23 |
c  [J]^-1 [B](1,2) = | j31  0  j36 |    [J]^-1 [B](3,2) = | j36  0  j33 |
c                    | j41  0  j46 |                      | j46  0  j43 |
c                    | j51  0  j56 |                      | j56  0  j53 |
c                    | j61  0  j66 |                      | j66  0  j63 |

c  then strain in global coords eg given by
 
c  ... eg(i,1:6,2) = { [J]^-1 [B](1,2) } v1(i,1:3,1) + { [J]^-1 [B](3,2) } v1(i,1:3,3)
c____________________________________________________________________________________
c 
c  ... eg(i,1:6,3) = [J]^-1 ( [B](2,3) [j]^t [y2]^t du + [B](3,3) [j]^t [y3]^t du)
c 
c   where            |  1   0   0  |                      |  0   0   0  |  
c                    |  0   0   0  |                      |  0   1   0  |  
c         [B](2,3) = |  0   0   0  |           [B](3,3) = |  0   0   0  |  
c                    |  0   1   0  |                      |  1   0   0  |  
c                    |  0   0   0  |                      |  0   0   0  |  
c                    |  0   0   0  |                      |  0   0   0  |  
                                        
c  and               | j11 j14  0  |                      | j14 j12  0  |
c                    | j21 j24  0  |                      | j24 j22  0  |
c  [J]^-1 [B](2,3) = | j31 j34  0  |    [J]^-1 [B](3,3) = | j34 j32  0  |
c                    | j41 j44  0  |                      | j44 j42  0  |
c                    | j51 j54  0  |                      | j54 j52  0  |
c                    | j61 j64  0  |                      | j64 j62  0  |

c  then strain in global coords eg given by
 
c  ... eg(i,1:6,3) = { [J]^-1 [B](2,3) } v1(i,1:3,2) + { [J]^-1 [B](3,3) } v1(i,1:3,3)
c____________________________________________________________________________________

c  ... df1 = 8/3 *( [B](1,1)^t [J]^-t sig(i,1:6,1) + [B](1,2)^t [J]^-t sig(i,1:6,2) )

c      NOTE: do 8/3 multiplication later

c                |  0   0   0   0   0   0  |               |  1   0   0   0   0   0  |
c    [B]^t(1,1) =|  0   1   0   0   0   0  |   [B]^t(1,2) =|  0   0   0   0   0   0  |
c                |  0   0   0   0   1   0  |               |  0   0   0   0   0   1  |

c                |  0   0   0   0   0   0  |               | j11 j21 j31 j41 j51 j61 |
c  [B]^t [J]^-t =| j12 j22 j32 j42 j52 j62 | [B]^t [J]^-t =|  0   0   0   0   0   0  |
c                | j15 j25 j35 j45 j55 j65 |               | j16 j26 j36 j46 j56 j66 |
c____________________________________________________________________________________

c  ... df2 = 8/3 *( [B](2,1)^t [J]^-t sig(i,1:6,1) + [B](2,3)^t [J]^-t sig(i,1:6,3) )

c      NOTE: do 8/3 multiplication later

c                |  0   0   0   0   0   0  |               |  1   0   0   0   0   0  |
c    [B]^t(2,1) =|  0   0   0   0   1   0  |   [B]^t(2,3) =|  0   0   0   1   0   0  |
c                |  0   0   1   0   0   0  |               |  0   0   0   0   0   0  |

c                |  0   0   0   0   0   0  |               | j11 j21 j31 j41 j51 j61 |
c  [B]^t [J]^-t =| j15 j25 j35 j45 j55 j65 | [B]^t [J]^-t =| j14 j24 j34 j44 j54 j64 |
c                | j13 j23 j33 j43 j53 j63 |               |  0   0   0   0   0   0  |
c____________________________________________________________________________________

c  ... df3 = 8/3 *( [B](3,2)^t [J]^-t sig(i,1:6,2) + [B](3,3)^t [J]^-t sig(i,1:6,3) )

c      NOTE: do 8/3 multiplication later

c                |  0   0   0   0   0   1  |               |  0   0   0   1   0   0  |
c    [B]^t(3,2) =|  0   0   0   0   0   0  |   [B]^t(3,3) =|  0   1   0   0   0   0  |
c                |  0   0   1   0   0   0  |               |  0   0   0   0   0   0  |

c                | j16 j26 j36 j46 j56 j66 |               | j14 j24 j34 j44 j54 j64 |
c  [B]^t [J]^-t =|  0   0   0   0   0   0  | [B]^t [J]^-t =| j12 j22 j32 j42 j52 j62 |
c                | j13 j23 j33 j43 j53 j63 |               |  0   0   0   0   0   0  |
c____________________________________________________________________________________

c    ... j = det[J] = 1/8 vol
c                             ==> et = 1/8 8/3 = 1/3
c                               & en = 1/8 8/9 = 1/9
c____________________________________________________________________________________
      if(ortho.gt.0) then
        do 80 i=lft,llt
c______________________________________________________________________________
c
c Add for enhanced strain
        a1 =-( ds_ps(1,2,i) * v1(i,2,1) + ds_ps(1,3,i) * v1(i,3,2)+
     &         ds_ps(1,5,i) *(v1(i,2,2) + v1(i,3,1)) )/ds_ps(1,1,i)
        a2 =-( ds_ps(1,2,i) * v1(i,1,1) + ds_ps(2,3,i) * v1(i,3,3)+
     &         ds_ps(2,6,i) *(v1(i,1,3) + v1(i,3,1)) )/ds_ps(2,2,i)
        a3 =-( ds_ps(1,3,i) * v1(i,1,2) + ds_ps(2,3,i) * v1(i,2,3)+
     &         ds_ps(3,4,i) *(v1(i,1,3) + v1(i,2,2)) )/ds_ps(3,3,i)
c______________________________________________________________________________
        eg(i,2,1) = ds_ps(2,2,i) * v1(i,2,1) + ds_ps(2,5,i) * v1(i,3,1)+
     &              ds_ps(2,5,i) * v1(i,2,2) + ds_ps(2,3,i) * v1(i,3,2)+
     &              ds_ps(1,2,i) * a1
        eg(i,3,1) = ds_ps(2,3,i) * v1(i,2,1) + ds_ps(3,5,i) * v1(i,3,1)+
     &              ds_ps(3,5,i) * v1(i,2,2) + ds_ps(3,3,i) * v1(i,3,2)+
     &              ds_ps(1,3,i) * a1
        eg(i,5,1) = ds_ps(2,5,i) * v1(i,2,1) + ds_ps(5,5,i) * v1(i,3,1)+
     &              ds_ps(5,5,i) * v1(i,2,2) + ds_ps(3,5,i) * v1(i,3,2)+
     &              ds_ps(1,5,i) * a1
        eg(i,1,2) = ds_ps(1,1,i) * v1(i,1,1) + ds_ps(1,6,i) * v1(i,3,1)+
     &              ds_ps(1,6,i) * v1(i,1,3) + ds_ps(1,3,i) * v1(i,3,3)+
     &              ds_ps(1,2,i) * a2
        eg(i,3,2) = ds_ps(1,3,i) * v1(i,1,1) + ds_ps(3,6,i) * v1(i,3,1)+
     &              ds_ps(3,6,i) * v1(i,1,3) + ds_ps(3,3,i) * v1(i,3,3)+
     &              ds_ps(2,3,i) * a2
        eg(i,6,2) = ds_ps(1,6,i) * v1(i,1,1) + ds_ps(6,6,i) * v1(i,3,1)+
     &              ds_ps(6,6,i) * v1(i,1,3) + ds_ps(3,6,i) * v1(i,3,3)+
     &              ds_ps(2,6,i) * a2
        eg(i,1,3) = ds_ps(1,1,i) * v1(i,1,2) + ds_ps(1,4,i) * v1(i,2,2)+
     &              ds_ps(1,4,i) * v1(i,1,3) + ds_ps(1,2,i) * v1(i,2,3)+
     &              ds_ps(1,3,i) * a3
        eg(i,2,3) = ds_ps(1,2,i) * v1(i,1,2) + ds_ps(2,4,i) * v1(i,2,2)+
     &              ds_ps(2,4,i) * v1(i,1,3) + ds_ps(2,2,i) * v1(i,2,3)+
     &              ds_ps(2,3,i) * a3
        eg(i,4,3) = ds_ps(1,4,i) * v1(i,1,2) + ds_ps(4,4,i) * v1(i,2,2)+
     &              ds_ps(4,4,i) * v1(i,1,3) + ds_ps(2,4,i) * v1(i,2,3)+
     &              ds_ps(3,4,i) * a3
80      continue
c
      else
        do i=lft,llt
c______________________________________________________________________________
c
c Add for enhanced strain
        a1 =-( ds_ps(1,2,i) * v1(i,2,1) + ds_ps(1,3,i) * v1(i,3,2)
     &          )/ds_ps(1,1,i)

        a2 =-( ds_ps(1,2,i) * v1(i,1,1) + ds_ps(2,3,i) * v1(i,3,3)
     &          )/ds_ps(2,2,i)

        a3 =-( ds_ps(1,3,i) * v1(i,1,2) + ds_ps(2,3,i) * v1(i,2,3)
     &         )/ds_ps(3,3,i)
c______________________________________________________________________________
        eg(i,2,1) = ds_ps(2,2,i) * v1(i,2,1) + ds_ps(2,3,i) * v1(i,3,2)+
     &              ds_ps(1,2,i) * a1
        eg(i,3,1) = ds_ps(2,3,i) * v1(i,2,1) + ds_ps(3,3,i) * v1(i,3,2)+
     &              ds_ps(1,3,i) * a1
        eg(i,5,1) = ds_ps(5,5,i) * (v1(i,3,1) + v1(i,2,2))
c
        eg(i,1,2) = ds_ps(1,1,i) * v1(i,1,1) + ds_ps(1,3,i) * v1(i,3,3)+
     &              ds_ps(1,2,i) * a2
        eg(i,3,2) = ds_ps(1,3,i) * v1(i,1,1) + ds_ps(3,3,i) * v1(i,3,3)+
     &              ds_ps(2,3,i) * a2
        eg(i,6,2) = ds_ps(6,6,i) * (v1(i,3,1) + v1(i,1,3))
c
        eg(i,1,3) = ds_ps(1,1,i) * v1(i,1,2) + ds_ps(1,2,i) * v1(i,2,3)+
     &              ds_ps(1,3,i) * a3
        eg(i,2,3) = ds_ps(1,2,i) * v1(i,1,2) + ds_ps(2,2,i) * v1(i,2,3)+
     &              ds_ps(2,3,i) * a3
        eg(i,4,3) = ds_ps(4,4,i) * (v1(i,2,2) + v1(i,1,3))
        enddo
      endif
c
        do i=lft,llt                            ! 18 ops    476 ops extra
         tempt = et*volint(i)*failu(i)
         v1(i,1,1) = tempt* eg(i,1,2)
         v1(i,2,1) = tempt* eg(i,2,1)
         v1(i,3,1) = tempt*(eg(i,5,1) + eg(i,6,2))
         v1(i,1,2) = tempt* eg(i,1,3)
         v1(i,2,2) = tempt*(eg(i,5,1) + eg(i,4,3))
         v1(i,3,2) = tempt* eg(i,3,1)
         v1(i,1,3) = tempt*(eg(i,6,2) + eg(i,4,3))
         v1(i,2,3) = tempt* eg(i,2,3)
         v1(i,3,3) = tempt* eg(i,3,2)
c
         tempn = en*volint(i)*failu(i)
         v1(i,3,4) = tempn * ds_ps(3,3,i) * v1(i,3,4)          ! eg(i,3,1)
         v1(i,1,4) = tempn * ds_ps(1,1,i) * v1(i,1,4)          ! eg(i,1,2)
         v1(i,2,4) = tempn * ds_ps(2,2,i) * v1(i,2,4)          ! eg(i,2,3)
       enddo
c____________________________________________________________________________________
c 
c  ... eg(i,1:6,1) = [K] [B](4,4) [j]^t [y4]^t du
c 
c   where            |  0   0   0  |
c                    |  0   0   0  |
c         [B](4,4) = |  0   0   1  |
c                    |  0   0   0  |
c                    |  0   0   0  |
c                    |  0   0   0  |
                                        
c  and               |  0   0  k13 |
c                    |  0   0  k23 |
c     [K] [B](4,4) = |  0   0  k33 |
c                    |  0   0  k43 |
c                    |  0   0  k53 |
c                    |  0   0  k63 |

c  then strain in global coords eg given by
 
c  ... eg(i,1:4,1) = { [K] [B](4,4) } v1(i,1:3,4)
c____________________________________________________________________________________
c 
c  ... eg(i,1:6,2) = [K] [B](4,5) [j]^t [y4]^t du
c 
c   where            |  1   0   0  |
c                    |  0   0   0  |
c         [B](4,5) = |  0   0   0  |
c                    |  0   0   0  |
c                    |  0   0   0  |
c                    |  0   0   0  |
                                        
c  and               | k11  0   0  |
c                    | k21  0   0  |
c     [K] [B](4,4) = | k31  0   0  |
c                    | k41  0   0  |
c                    | k51  0   0  |
c                    | k61  0   0  |

c  then strain in global coords eg given by
 
c  ... eg(i,1:4,2) = { [K] [B](4,5) } v1(i,1:3,4)
c____________________________________________________________________________________
c 
c  ... eg(i,1:6,3) = [K] [B](4,6) [j]^t [y4]^t du
c 
c   where            |  0   0   0  |
c                    |  0   1   0  |
c         [B](4,6) = |  0   0   0  |
c                    |  0   0   0  |
c                    |  0   0   0  |
c                    |  0   0   0  |
                                        
c  and               |  0  k12  0  |
c                    |  0  k22  0  |
c     [K] [B](4,6) = |  0  k32  0  |
c                    |  0  k42  0  |
c                    |  0  k52  0  |
c                    |  0  k62  0  |

c  then strain in global coords eg given by
 
c  ... eg(i,1:4,3) = { [K] [B](4,6) } v1(i,1:3,4)
c____________________________________________________________________________________

c  ... df4 = 8/9 *( [B](4,4)^t eg(4) + [B](4,5)^t eg(4)
c                   [B](4,6)^t eg(4))

c      NOTE: do 8/9 multiplication later

c                |  0   0   0   0   0   0  |               |  1   0   0   0   0   0  |
c    [B]^t(4,4) =|  0   0   0   0   0   0  |   [B]^t(4,5) =|  0   0   0   0   0   0  |
c                |  0   0   1   0   0   0  |               |  0   0   0   0   0   0  |

c                |  0   0   0   0   0   0  |
c    [B]^t(4,6) =|  0   1   0   0   0   0  |
c                |  0   0   0   0   0   0  |

c                |  0   0   0   0   0   0  |               | j11 j21 j31 j41 j51 j61 |
c  [B]^t [J]^-t =|  0   0   0   0   0   0  | [B]^t [J]^-t =|  0   0   0   0   0   0  |
c                | j13 j23 j33 j43 j53 j63 |               |  0   0   0   0   0   0  |

c                |  0   0   0   0   0   0  |
c  [B]^t [J]^-t =| j12 j22 j32 j42 j52 j62 |
c                |  0   0   0   0   0   0  |
c____________________________________________________________________________________
c
c      |hxj|               |j11 j12 j13| |v1(i,1,j)|
c  ... |hyj| = [j] [fj] =  |j21 j22 j23| |v1(i,2,j)| 
c      |hzj|               |j31 j32 j33| |v1(i,3,j)|
c
       do 205 i=lft,llt
        hx1(i)=aj11(i)*v1(i,1,1) + aj12(i)*v1(i,2,1) + aj13(i)*v1(i,3,1)
        hy1(i)=aj21(i)*v1(i,1,1) + aj22(i)*v1(i,2,1) + aj23(i)*v1(i,3,1)
        hz1(i)=aj31(i)*v1(i,1,1) + aj32(i)*v1(i,2,1) + aj33(i)*v1(i,3,1)
        hx2(i)=aj11(i)*v1(i,1,2) + aj12(i)*v1(i,2,2) + aj13(i)*v1(i,3,2)
        hy2(i)=aj21(i)*v1(i,1,2) + aj22(i)*v1(i,2,2) + aj23(i)*v1(i,3,2)
        hz2(i)=aj31(i)*v1(i,1,2) + aj32(i)*v1(i,2,2) + aj33(i)*v1(i,3,2)
        hx3(i)=aj11(i)*v1(i,1,3) + aj12(i)*v1(i,2,3) + aj13(i)*v1(i,3,3)
        hy3(i)=aj21(i)*v1(i,1,3) + aj22(i)*v1(i,2,3) + aj23(i)*v1(i,3,3)
        hz3(i)=aj31(i)*v1(i,1,3) + aj32(i)*v1(i,2,3) + aj33(i)*v1(i,3,3)
        hx4(i)=aj11(i)*v1(i,1,4) + aj12(i)*v1(i,2,4) + aj13(i)*v1(i,3,4)
        hy4(i)=aj21(i)*v1(i,1,4) + aj22(i)*v1(i,2,4) + aj23(i)*v1(i,3,4)
        hz4(i)=aj31(i)*v1(i,1,4) + aj32(i)*v1(i,2,4) + aj33(i)*v1(i,3,4)
205    continue     
c
c Sum the hourglass work
      do i=lft,llt
        hgwk(i)=
     &   hx1(i)*hx1v(i)+hx2(i)*hx2v(i)+hx3(i)*hx3v(i)+hx4(i)*hx4v(i)
     &  +hy1(i)*hy1v(i)+hy2(i)*hy2v(i)+hy3(i)*hy3v(i)+hy4(i)*hy4v(i)
     &  +hz1(i)*hz1v(i)+hz2(i)*hz2v(i)+hz3(i)*hz3v(i)+hz4(i)*hz4v(i)
      enddo
      hour_eng = hour_eng - half*SUM(hgwk(lft:llt))
c
      do 210 i=lft,llt
       x1(i)=hx1(i)*sv(i,1,1)+hx2(i)*sv(i,1,2)
     &      +hx3(i)*sv(i,1,3)+hx4(i)*sv(i,1,4)
       x2(i)=hx1(i)*sv(i,2,1)+hx2(i)*sv(i,2,2)
     &      +hx3(i)*sv(i,2,3)+hx4(i)*sv(i,2,4)
       x3(i)=hx1(i)*sv(i,3,1)+hx2(i)*sv(i,3,2)
     &      +hx3(i)*sv(i,3,3)+hx4(i)*sv(i,3,4)
       x4(i)=hx1(i)*sv(i,4,1)+hx2(i)*sv(i,4,2)
     &      +hx3(i)*sv(i,4,3)+hx4(i)*sv(i,4,4)
       x5(i)=hx1(i)*sv(i,5,1)+hx2(i)*sv(i,5,2)
     &      +hx3(i)*sv(i,5,3)+hx4(i)*sv(i,5,4)
       x6(i)=hx1(i)*sv(i,6,1)+hx2(i)*sv(i,6,2)
     &      +hx3(i)*sv(i,6,3)+hx4(i)*sv(i,6,4)
       x7(i)=hx1(i)*sv(i,7,1)+hx2(i)*sv(i,7,2)
     &      +hx3(i)*sv(i,7,3)+hx4(i)*sv(i,7,4)
210    x8(i)=hx1(i)*sv(i,8,1)+hx2(i)*sv(i,8,2)
     &      +hx3(i)*sv(i,8,3)+hx4(i)*sv(i,8,4)
      do 220 i=lft,llt
       y1(i)=hy1(i)*sv(i,1,1)+hy2(i)*sv(i,1,2)
     &      +hy3(i)*sv(i,1,3)+hy4(i)*sv(i,1,4)
       y2(i)=hy1(i)*sv(i,2,1)+hy2(i)*sv(i,2,2)
     &      +hy3(i)*sv(i,2,3)+hy4(i)*sv(i,2,4)
       y3(i)=hy1(i)*sv(i,3,1)+hy2(i)*sv(i,3,2)
     &      +hy3(i)*sv(i,3,3)+hy4(i)*sv(i,3,4)
       y4(i)=hy1(i)*sv(i,4,1)+hy2(i)*sv(i,4,2)
     &      +hy3(i)*sv(i,4,3)+hy4(i)*sv(i,4,4)
       y5(i)=hy1(i)*sv(i,5,1)+hy2(i)*sv(i,5,2)
     &      +hy3(i)*sv(i,5,3)+hy4(i)*sv(i,5,4)
       y6(i)=hy1(i)*sv(i,6,1)+hy2(i)*sv(i,6,2)
     &      +hy3(i)*sv(i,6,3)+hy4(i)*sv(i,6,4)
       y7(i)=hy1(i)*sv(i,7,1)+hy2(i)*sv(i,7,2)
     &      +hy3(i)*sv(i,7,3)+hy4(i)*sv(i,7,4)
220    y8(i)=hy1(i)*sv(i,8,1)+hy2(i)*sv(i,8,2)
     &      +hy3(i)*sv(i,8,3)+hy4(i)*sv(i,8,4)
      do 230 i=lft,llt
       z1(i)=hz1(i)*sv(i,1,1)+hz2(i)*sv(i,1,2)
     &      +hz3(i)*sv(i,1,3)+hz4(i)*sv(i,1,4)
       z2(i)=hz1(i)*sv(i,2,1)+hz2(i)*sv(i,2,2)
     &      +hz3(i)*sv(i,2,3)+hz4(i)*sv(i,2,4)
       z3(i)=hz1(i)*sv(i,3,1)+hz2(i)*sv(i,3,2)
     &      +hz3(i)*sv(i,3,3)+hz4(i)*sv(i,3,4)
       z4(i)=hz1(i)*sv(i,4,1)+hz2(i)*sv(i,4,2)
     &      +hz3(i)*sv(i,4,3)+hz4(i)*sv(i,4,4)
       z5(i)=hz1(i)*sv(i,5,1)+hz2(i)*sv(i,5,2)
     &      +hz3(i)*sv(i,5,3)+hz4(i)*sv(i,5,4)
       z6(i)=hz1(i)*sv(i,6,1)+hz2(i)*sv(i,6,2)
     &      +hz3(i)*sv(i,6,3)+hz4(i)*sv(i,6,4)
       z7(i)=hz1(i)*sv(i,7,1)+hz2(i)*sv(i,7,2)
     &      +hz3(i)*sv(i,7,3)+hz4(i)*sv(i,7,4)
230    z8(i)=hz1(i)*sv(i,8,1)+hz2(i)*sv(i,8,2)
     &      +hz3(i)*sv(i,8,3)+hz4(i)*sv(i,8,4)
c
      return
      end
