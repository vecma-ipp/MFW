
! -----------------------------------------------------------------------
subroutine update_file(filename, n, values)
    
    use euitm_schemas,   only: type_coreprof 
    
    use read_structures, only: open_read_file,  &
                            &  close_read_file, &
                            &  read_cpo
    
    use write_structures, only: open_write_file,  &
                             &  close_write_file, &
                             &  write_cpo
    
    use deallocate_structures, only: deallocate_cpo
    
    implicit none
    
    character(len=*), intent(in) :: filename   
    integer, intent(in) :: n
    real(kind=8), dimension (n), intent(in) :: values 
    
    integer :: ios
    type(type_coreprof) , pointer :: corep(:) => NULL()
    allocate(corep(1))
    
   print*, '>>> VALUES = ', values 
    ! ... 
    open (unit = 10, file = filename, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, filename)
       call read_cpo(corep(1), 'coreprof')
       call close_read_file
       corep(1)%te%value(1:n)=values
    else
       print *,"CPO file not found:", filename
       STOP
    end if
    
    call open_write_file(11, 'ets_coreprof_out.cpo')
    call write_cpo(corep(1), 'coreprof')
    call close_write_file
  
    call deallocate_cpo(corep)

end subroutine update_file
! -----------------------------------------------------------------------
