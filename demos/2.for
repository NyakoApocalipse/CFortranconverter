program main
    implicit none
    real,dimension(3,4):: a(3,4)=(/1,2,3,4,5,6,7,8,9,10,11,12 /)
    real,dimension(3,4):: b(3,4)=(/(/1,2,3/),(/4,5,6/),(/7,8,9/),(/10,11,12/) /)
    real,dimension(3,4)::c = reshape( (/ 1,2,3,4,5,6,7,8,9,10,11,12/), (/ 3, 4 /) )
end program