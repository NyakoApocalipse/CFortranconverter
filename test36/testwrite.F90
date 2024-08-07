program file_operations  
    implicit none  
    character(4) :: dataa  
    integer :: iostatt  , i
    logical :: eof  
  
    ! 打开文件  
    open(unit=10, access='direct', iostat=iostatt, file='temp.txt', recl=4, status='old',  form='unformatted')  
    if (iostatt /= 0) then  
        print *, 'Error opening file'  
        stop  
    end if  
  
    ! 从文件中读取数据  
    do i = 1, 3  
        read(10, rec=i) dataa  
        if (iostatt /= 0) then  
            print *, 'Error reading from file'  
            exit  
        end if  
        print *, 'Read data:', dataa  
    end do  
  
    ! 在文件末尾写入数据  
    write(10, *) '1234'  
    write(10, *) '5678'  
  
    ! 为了读取刚才写入的数据，需要将文件指针重置到文件开始  
    ! 但由于我们想在末尾继续，实际上这里应该重新打开文件或以追加模式打开  
    ! 不过为了简单起见，这里我们假设直接读取接下来的数据（这在实际操作中可能不是最佳做法）  
  
    ! 尝试读取并打印新写入的数据（注意：在实际应用中，可能需要先关闭再重新打开文件，或者调整文件指针）  
    ! rewind(10) ! 将文件指针移回文件开头，仅为了示例，实际上应该重新打开文件或寻求其他方式读取末尾数据  
    ! ! 如果文件很大，不想从头开始读取，应该使用其他方法来定位到文件末尾  
    ! do while (.not. eof(10))  
    !     read(10, *, iostat=iostat) data  
    !     if (iostat /= 0) then  
    !         if (iostat == -1) then  
    !             eof = .true.  
    !         else  
    !             print *, 'Error reading from file during rewind'  
    !             exit  
    !         end if  
    !     else  
    !         print *, 'Data after rewind:', data  
    !     end if  
    ! end do  
  
    ! 关闭文件  
    close(10)  
  
    ! 注意：在实际应用中，为了读取文件末尾新写入的数据，更好的做法是使用追加模式（status='unknown'）打开文件，  
    ! 或者在写入数据后关闭文件，然后以适当的模式重新打开它。  
end program file_operations