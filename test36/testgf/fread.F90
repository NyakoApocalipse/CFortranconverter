subroutine read_from_file(filename, value)
    character(len=100), intent(in) :: filename
    real, intent(out) :: value
    open(unit=10, file=filename, status='old', action='read')
    read(10, *) value
    close(10)
  end subroutine read_from_file
   
