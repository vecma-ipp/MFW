module gem0_standalone
  use euitm_schemas
  implicit none

  integer, save :: init_step             !initial step count


  interface
     subroutine gem(equil, corep, coret, code_parameters)
       use euitm_schemas
       type (type_equilibrium), pointer ::  equil(:)
       type (type_coreprof), pointer :: corep(:)
       type (type_coretransp), pointer :: coret(:)
       type (type_param) :: code_parameters
     end subroutine gem
  end interface

contains

  subroutine gem0_cpo(equil, corep, coret) 
    use xml_file_reader
    use deallocate_structures
    implicit none

    type (type_equilibrium), pointer :: equil(:)
    type (type_coreprof), pointer :: corep(:)
    type (type_coretransp), pointer :: coret(:)
    type (type_param) :: code_parameters

    print *,"fortran GEM0 wrapper"

    print *,"get code params"
    call fill_param(code_parameters, 'gem0.xml', '', 'gem0.xsd')

    print *,"run gem0 routine"

   !  print *, ">printing first ", 64, " of equil of size ", size(equil) !DEBUG
   !  print *, equil(1:64) !DEBUG
   !  print *, ">printing first ", 64, " of corep of size ", size(corep) !DEBUG
   !  print *, corep(1:64) !DEBUG

    call gem(equil, corep, coret, code_parameters)
    print *, "finished gem0 routine"

  end subroutine gem0_cpo


  subroutine gem02buf(equil_in_buf, corep_in_buf, coret_out_buf) 
    use iso_c_binding
    use string_binding
    use deallocate_structures
    use read_structures
    use write_structures
    use c_tools
    implicit none

    character(kind=c_char), pointer :: equil_in_buf(:)
    character(kind=c_char), pointer :: corep_in_buf(:)
    character(kind=c_char), pointer :: coret_out_buf(:)
    character(kind=c_char), pointer :: tmpbuf(:)

    type (type_equilibrium), pointer :: equil_in(:)
    type (type_coreprof), pointer :: corep_in(:)
    type (type_coretransp), pointer :: coret_out(:)

    character(F_STR_SIZE) :: equil_in_file, corep_in_file, coret_out_file
    character(F_STR_SIZE) :: username, tmpdir
    integer :: tmpsize, ios

    allocate(equil_in(1))
    allocate(corep_in(1))

    call getenv("USER",username)
    call getenv("CPO_SERIALIZATION_DIR",tmpdir)
    tmpsize = len_trim(tmpdir)
    if (tmpsize.ne.0) then
       if (tmpdir(tmpsize:tmpsize) .ne. '/') then
          tmpdir = trim(tmpdir)//'/'
       end if
    end if

    equil_in_file = TRIM(tmpdir)//TRIM(username)//'_gem0_equilibrium_in.cpo'
    call byte2file(equil_in_file, equil_in_buf, size(equil_in_buf))
    open (unit = 10, file = equil_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, equil_in_file )
       call read_cpo(equil_in(1), 'equilibrium')
       call close_read_file
    else
       print *,"ERROR: no input equilibrium"
       STOP
    end if

    corep_in_file = TRIM(tmpdir)//TRIM(username)//'_gem0_coreprof_in.cpo'
    call byte2file(corep_in_file, corep_in_buf, size(corep_in_buf))
    print *, ">opening corep_in_file" !DEBUG
    open (unit = 10, file = corep_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
   print *, ">opened corep_in_file" !DEBUG
    if (ios == 0) then
       print *, ">reading corep_in_file" !DEBUG
       close (10)
       call open_read_file(10, corep_in_file )
       call read_cpo(corep_in(1), 'coreprof')
       call close_read_file
       print *, ">read corep_in_file" !DEBUG
    else
       print *,"ERROR: no input coreprof"
       STOP
    end if

    print *, ">calling gem0" !DEBUG    
    call gem0_cpo(equil_in, corep_in, coret_out)

    ! transfer CPO to buf
    !...  write the results
    print *, ">writing coret_out_file" !DEBUG   
    coret_out_file = 'gem0_coretransp_out.cpo'
    call open_write_file(11,coret_out_file)
    call write_cpo(coret_out(1),'coretransp')
    call close_write_file

    call file2byte(coret_out_file, tmpbuf, tmpsize)
    allocate(coret_out_buf(tmpsize))
    coret_out_buf(1:tmpsize) = tmpbuf(1:tmpsize)

    print *, ">deallocating datastructures" !DEBUG   
    call dealloc_cbytebuf(tmpbuf)

    call deallocate_cpo(equil_in)
    call deallocate_cpo(corep_in)
    call deallocate_cpo(coret_out)

  end subroutine gem02buf


end module gem0_standalone
