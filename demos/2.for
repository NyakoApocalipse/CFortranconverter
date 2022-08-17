program arrayReshape
    implicit none
    integer i
    integer,parameter::one=1,zero=0
    integer,dimension(3,12)::a
    do i=1,3
        a(i,1:9) = (/one,zero,zero, zero,one,zero, zero,zero,one/)
        ! assign_forslice(a, make_init_list({one,zero,zero, zero,one,zero, zero,zero,one}),{{i},{1,9}});
    enddo
    write(*,*) a(1,1)
end program arrayReshape
