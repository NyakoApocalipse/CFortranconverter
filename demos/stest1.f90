program stest1
    implicit none
    real x, y
    x = 5.0
    y = 100.0
    call subr(x, y, 10)
    print *, x, y
end program stest1
