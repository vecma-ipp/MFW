module gem_wrapper
  use iso_c_binding
  use string_binding
  use itm_constants
  use read_structures
  use write_structures
  use xml_file_reader
  use deallocate_structures
  use c_tools
  use euitm_schemas
#ifdef MPI2
  use mpi
#endif
    implicit none
#ifdef MPI
#ifndef MPI2
    include "mpif.h"
#endif
#endif
  implicit none

  interface
     subroutine gem(equil, corep, coret, code_parameters)
       use euitm_schemas
       type (type_equilibrium), pointer ::  equil(:)
       type (type_coreprof), pointer :: corep(:)
       type (type_coretransp), pointer :: coret(:)
       type (type_param) :: code_parameters
     end subroutine gem
  end interface


    integer(kind=c_signed_char), pointer :: corep_in(:)
    integer(kind=c_signed_char), pointer :: equil_in(:)
    integer(kind=c_signed_char), pointer :: coret_out(:)

    type (type_equilibrium), pointer :: equil(:)
    type (type_coreprof), pointer :: corep(:)
    type (type_coretransp), pointer :: coret(:)
    type (type_param) :: code_parameters

    character(F_STR_SIZE) :: corep_file_in, equil_file_in, coret_file_out
    integer :: tmpsize

    integer :: ios
    integer, save :: cpt = 0
    character(4)  :: cptstr

#ifdef MPI
    integer :: ierr, npes, irank, ipe
    call MPI_Init(ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, npes, ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, irank, ierr)

    if (irank.eq.0) then
       print *,"fortran GEM wrapper"
    endif
#endif


    !...  set initial state
    allocate(corep(1))
    allocate(equil(1))    

    !print *,'before calling byte2file...'

    print *,'size of corep_in = ',size(corep_in)
    print *,'size of equil_in = ',size(equil_in)

    ! transfer buf to CPO
    corep_file_in = 'gem_coreprof_in.cpo'
    equil_file_in = 'gem_equilibrium_in.cpo'
    call byte2file(corep_file_in, corep_in, size(corep_in))
    call byte2file(equil_file_in, equil_in, size(equil_in))

    !print *,"byte2file ok"

#ifdef MPI
    !if (irank.eq.0) then
    !endif

    call MPI_Barrier(MPI_COMM_WORLD, ierr)
#endif

    open (unit = 10, file = corep_file_in, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, corep_file_in )
       call read_cpo(corep(1), 'coreprof' )
       call close_read_file
    else
       print *,"CPO file not found ", corep_file_in
       STOP
    end if
    open (unit = 11, file = equil_file_in, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (11)
       call open_read_file(11, equil_file_in )
       call read_cpo(equil(1), 'equilibrium' )
       call close_read_file
    else
       print *,"CPO file not found ", equil_file_in
       STOP
    end if


    !print *,"get code params"
    call fill_param(code_parameters, 'gem.xml', '', 'gem.xsd')
    !call get_code_parms(code_parameters, 'gem.xml', '', 'gem.xsd')

    !print *,"run gem routine"
    call gem(equil, corep, coret, code_parameters)


#ifdef MPI
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    if (irank.eq.0) then
#endif
       ! transfer CPO to buf
       print *,"write coretransp CPO"
       write(cptstr,'(I4.4)') cpt
       cpt = cpt+1
       coret_file_out = 'gem_coretransp_'//cptstr//'.cpo'
       !coret_file_out = 'gem_coretransp_out.cpo'
       call open_write_file(12,coret_file_out)
       call write_cpo(coret(1),'coretransp')
       call close_write_file

       call file2byte(coret_file_out, coret_out, tmpsize)
#ifdef MPI
    endif
#endif

    call deallocate_cpo(corep)
    call deallocate_cpo(equil)
    call deallocate_cpo(coret)


  end subroutine gem2buf




  subroutine gem2file(equil_in, corep_in, coret_out) 
#ifdef MPI2
    use mpi
#endif
    use iso_c_binding
    use string_binding
    use itm_constants
    use read_structures
    use write_structures
    use xml_file_reader
    use deallocate_structures
    implicit none
#ifdef MPI
#ifndef MPI2
    include "mpif.h"
#endif
#endif

    character(kind=c_char,len=1), dimension(F_STR_SIZE), intent(in) :: corep_in
    character(kind=c_char,len=1), dimension(F_STR_SIZE), intent(in) :: equil_in
    character(kind=c_char,len=1), dimension(F_STR_SIZE), intent(out) :: coret_out
    character(F_STR_SIZE) :: corep_file_in, equil_file_in, coret_file_out

    integer :: ios
    integer, save :: cpt = 0
    character(4)  :: cptstr

    type (type_equilibrium), pointer :: equil(:)
    type (type_coreprof), pointer :: corep(:)
    type (type_coretransp), pointer :: coret(:)
    type (type_param) :: code_parameters

#ifdef MPI
    integer :: ierr, npes, irank, ipe

#ifdef NOTEST
    call MPI_Init(ierr)
#endif

    call MPI_Comm_size(MPI_COMM_WORLD, npes, ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, irank, ierr)
#endif


    !print *,"fortran GEM wrapper"

    !... convert C strings into F strings
    call str_c2f(corep_in, corep_file_in)
    call str_c2f(equil_in, equil_file_in)

    !print *,"time dependent cpo files:",cpt
    write(cptstr,'(I4.4)') cpt
    cpt = cpt+1
    coret_file_out = 'gem_coretransp_'//cptstr//'.cpo'
    call str_f2c(coret_file_out,coret_out)


    !...  set initial state
    allocate(corep(1))
    allocate(equil(1))    


    open (unit = 10, file = corep_file_in, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, corep_file_in )
       call read_cpo(corep(1), 'coreprof' )
       call close_read_file
    else
       print *,"CPO file not found ", corep_file_in
       STOP
    end if

    open (unit = 11, file = equil_file_in, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (11)
       call open_read_file(11, equil_file_in )
       call read_cpo(equil(1), 'equilibrium' )
       call close_read_file
    else
       print *,"CPO file not found ", equil_file_in
       STOP
    end if


#ifdef MPI
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
#endif


    !print *,"get code params"
    !call get_code_parms(code_parameters, 'gem.xml', '', 'gem.xsd')
    call fill_param(code_parameters, 'gem.xml', '', 'gem.xsd')

    !print *,"run gem routine"
    call gem(equil, corep, coret, code_parameters)


#ifdef MPI
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    if (irank.eq.0) then
       !if (irank.eq.0) then
#endif
       !print *,"write coretransp CPO"
       call open_write_file(12,coret_file_out)
       call write_cpo(coret(1),'coretransp')
       call close_write_file
#ifdef MPI
    endif
#endif

    call deallocate_cpo(corep)
    call deallocate_cpo(equil)
    call deallocate_cpo(coret)

#ifdef NOTEST
    call MPI_Finalize(ierr)
#endif

  end subroutine gem2file

  
end module gem_wrapper
