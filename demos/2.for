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

program main
  use ma
  type(car),pointer::car2=>null()
  type(car),target::car1
  car1%speed=1.0
  car2=>car1
  call xx(car2%speed)
end program
