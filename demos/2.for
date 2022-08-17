program main
    implicit none
    integer,dimension(3,4),parameter::c(3,4) = reshape( (/ 1,2,3,4,5,6,7,8,9,10,11,12/), (/ 3, 4 /) )
    !   const farray<int> c {{1,1},{3,4},forreshape({1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}, {3, 4})};
end program