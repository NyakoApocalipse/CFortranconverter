program main
  implicit none
  character(20)::str
  character::chr
  integer::i,j
  character(20)::buf
  str='111 222 apple'
  read(str,"(I)")i
  write(*,*)i
  read(str,"(I)")j
  write(*,*)j
  read(str,"(I,I,A1,A)") i,j,chr,buf
  write(*,*)i,j,chr,buf

end program main
! expected output
! 111
! 111
! 111         222 a pple
