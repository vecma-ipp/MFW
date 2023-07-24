module gem_standalone
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

  subroutine gem_cpo(equil, corep, coret) 
    use xml_file_reader
    use deallocate_structures
    implicit none

    type (type_equilibrium), pointer :: equil(:)
    type (type_coreprof), pointer :: corep(:)
    type (type_coretransp), pointer :: coret(:)
    type (type_param) :: code_parameters

    !print *,"fortran GEM wrapper"

    !print *,"get code params"
    call fill_param(code_parameters, 'gem.xml', '', 'gem.xsd')

    !print *,"run gem routine"
    !DEBUG
    !print *, "Calling gem "
    !End of Debug

    call gem(equil, corep, coret, code_parameters)

  end subroutine gem_cpo


  subroutine gem2buf(equil_in_buf, corep_in_buf, coret_out_buf) 
    use iso_c_binding
    use string_binding
    use deallocate_structures
    use read_structures
    use write_structures
    use c_tools
    implicit none

    include "mpif.h"    !OL keep or not?

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

    integer :: ierr, npes, irank
    print *, "gem2buf: calling mpi comm and rank" !!!DEBUG
    call MPI_Comm_size(MPI_COMM_WORLD, npes, ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, irank, ierr)
    print *, "gem2buf: npes= ", npes !!!DEBUG
    print *, "gem2buf: irank= ", irank !!!DEBUG

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

    print *, "gem2buf: calling byte2file for equilibrium_in", npes !!!DEBUG
    equil_in_file = TRIM(tmpdir)//TRIM(username)//'_gem_equilibrium_in.cpo'
    call byte2file(equil_in_file, equil_in_buf, size(equil_in_buf))

    print *, "gem2buf: calling byte2file for coreprof_in", npes !!!DEBUG
    corep_in_file = TRIM(tmpdir)//TRIM(username)//'_gem_coreprof_in.cpo'
    call byte2file(corep_in_file, corep_in_buf, size(corep_in_buf))

    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    print *, "gem2buf: after barrier opening equilibrium_in", npes !!!DEBUG

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

    print *, "gem2buf: opening coreprof_in", npes !!!DEBUG

    open (unit = 10, file = corep_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, corep_in_file )
       call read_cpo(corep_in(1), 'coreprof')
       call close_read_file
    else
       print *,"ERROR: no input coreprof"
       STOP
    end if

    print *, "gem2buf: calling gem_cpo", npes !!!DEBUG
    call gem_cpo(equil_in, corep_in, coret_out)

    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    print *, "gem2buf: after barrier filling in coretransp_out", npes !!!DEBUG

    if (irank.eq.0) then
       ! transfer CPO to buf
       !...  write the results
       coret_out_file = 'gem_coretransp_out.cpo'
       call open_write_file(11,coret_out_file)
       call write_cpo(coret_out(1),'coretransp')
       call close_write_file
       
       print *, "gem2buf: wrote down gem_coretransp_out.cpo" !!!DEBUG
       call file2byte(coret_out_file, tmpbuf, tmpsize)
       print *, "gem2buf: called file2buf" !!!DEBUG
       allocate(coret_out_buf(tmpsize))
       coret_out_buf(1:tmpsize) = tmpbuf(1:tmpsize)
       print *, "gem2buf: allocated and wrote coret_out_buf" !!!DEBUG
       call dealloc_cbytebuf(tmpbuf)
    endif

    call deallocate_cpo(equil_in)
    call deallocate_cpo(corep_in)
    call deallocate_cpo(coret_out)

  end subroutine gem2buf

end module gem_standalone
