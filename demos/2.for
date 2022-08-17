program arrayReshape
      implicit none
      integer,dimension(3,4),parameter:: a(3,4)=(/1,2,3,4,5,6,7,8,9,10,11,12/)
      integer,dimension(3,4),parameter:: b(3,4)=(/(/1,2,3/),(/4,5,6/),(/7,8,9/),(/10,11,12/)/)
      integer,dimension(3,4),parameter::c(3,4) = reshape( (/ 1,2,3,4,5,6,7,8,9,10,11,12/), (/ 3, 4 /) )
write(*,*) a(2,3),b(2,3),c(2,3)

end program arrayReshape
