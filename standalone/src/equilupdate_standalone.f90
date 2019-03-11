module equilupdate_standalone

contains

  subroutine equilupdate2cpo(corep_in, toroidf_in, equil_in, equil_out)
    use euitm_schemas: only type_coreprof, type_toroidfield, type_equilibrium
    use equilibrium_input: only equil_input
    implicit none

    type(type_coreprof), pointer :: corep_in(:)
    type(type_toroidfield), pointer :: toroidf_in(:)
    type(type_equilibrium), pointer :: equil_in(:), equil_out(:)

    call equil_input(corep_in, toroidf_in, equil_in, equil_out)

  end subroutine equilupdate2cpo


  subroutine equilupdate2file(corep_in_file, toroidf_in_file, equil_in_file, equil_out_file)
    use euitm_schemas: only type_coreprof, type_toroidfield, type_equilibrium
    use equilibrium_input: only equil_input
    use read_structures
    use write_structures
    implicit none

    character(len=*), intent(in) :: corep_in_file
    character(len=*), intent(in) :: toroidf_in_file
    character(len=*), intent(in) :: equil_in_file, equil_out_file

    type(type_coreprof), pointer :: corep_in(:)
    type(type_toroidfield), pointer :: toroidf_in(:)
    type(type_equilibrium), pointer :: equil_in(:), equil_out(:)

    allocate(corep_in(1))
    allocate(toroidf_in(1))
    allocate(equil_in(1))

    open (unit = 10, file = corep_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, corep_in_file )
       call read_cpo(corep_in(1), 'coreprof' )
       call close_read_file
    else
       print *,"CPO file not found:",corep_in_file
       STOP
    end if
    open (unit = 10, file = toroidf_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, toroidf_in_file )
       call read_cpo(toroidf_in(1), 'toroidfield' )
       call close_read_file
    else
       print *,"CPO file not found:",toroidf_in_file
       STOP
    end if
    open (unit = 10, file = equil_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, equil_in_file )
       call read_cpo(equil_in(1), 'equilibrium' )
       call close_read_file
    else
       print *,"CPO file not found:",equil_in_file
       STOP
    end if

    call equil_input(corep_in, toroidf_in, equil_in, equil_out)

    equil_out_file = 'ets_equilibrium_out.cpo'
    !...  write the results
    call open_write_file(11,equil_out_file)
    call write_cpo(equil_out(1),'equilibrium')
    call close_write_file

    call deallocate_cpo(corep_in)
    call deallocate_cpo(toroidf_in)
    call deallocate_cpo(equil_in)
    call deallocate_cpo(equil_out)

  end subroutine equilupdate2file


end module equilupdate_standalone
