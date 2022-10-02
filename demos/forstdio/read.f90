program main
  implicit none
  character(20)::str
  integer::i,j
  character(20)::buf
  str='111 222 apple'
  read(str,*)i
  write(*,*)i
  read(str,*)j
  write(*,*)j
  read(str,*)i,j,buf
  write(*,*)i,j,buf

end program main
! expected output
! 111
! 111
! 111         222
