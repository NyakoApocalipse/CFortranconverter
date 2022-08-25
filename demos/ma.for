module ma
  type car
  real speed
  end type
  type(car),target::car1
  contains
  subroutine xx(a)
    real a
    write(*,*)a
  end subroutine
end module
