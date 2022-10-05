program ex1004
  implicit none
  integer nodet,i,j
  pointer(ipnodet,nodet(3,*)) !ipnodet是指针,其值是指针对象的地址；nodet(3)是指针对象，一个13的数组
  ipnodet = malloc(12) !6（一个整数）为内存分配数，返回值为分配的内存块起始位置的地址
  do i=1,3
    do j=1,4
      nodet(i,j)=i+j
    end do
  end do
  write(*,*)ipnodet,nodet(2,3)
end program