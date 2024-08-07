

PROGRAM custom_struct_example
 
  IMPLICIT NONE
 
  TYPE::t_person
    CHARACTER(len=20) :: name
    INTEGER :: age
  END TYPE t_person
 
  TYPE(t_person) :: person
 
  person%name = 'Alice'
  person%age = 30
 
  WRITE(*,*) 'Name: ', person%name
  WRITE(*,*) 'Age: ', person%age
 
END PROGRAM custom_struct_example