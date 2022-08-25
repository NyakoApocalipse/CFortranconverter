program main
  use ma
  type(car),pointer::car2=>null()
  type(car),target::car1
  car1%speed=1.0
  car2=>car1
  call xx(car2%speed)
end program
