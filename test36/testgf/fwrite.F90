subroutine write_to_file(filename, value)
    character(len=100), intent(in) :: filename
    real, intent(in) :: value
    open(unit=10, file=filename, status='replace', action='write')
    write(10, *) value
    close(10)
  end subroutine write_to_file