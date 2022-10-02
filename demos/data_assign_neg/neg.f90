program main
  integer, parameter :: CONST_NUM_SHELL_NODE = 4
  real,dimension(3,4):: a(3,4)=(/1,-2,3,4,5,6,7,8,9,10,11,12 /)
  real(fullR), parameter :: EdgeFixedCompVal(4) = (/-1.0, 1.0, 1.0, 1.0/) ! fixed component value
  real(fullR), parameter :: NodePrntCoord(2,CONST_NUM_SHELL_NODE) = (/(/-1.0, -1.0/), (/1.0, -1.0/), (/1.0, 1.0/), (/-1.0, 1.0/)/)
  write(*,*) a(1,3)
  write(*,*) EdgeFixedCompVal(1)
  write(*,*) NodePrntCoord(1,4)

end program