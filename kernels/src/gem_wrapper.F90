module gem_wrapper
  use euitm_schemas
  use c_tools
#ifdef PERF
  use c_perf
#endif
  implicit none

  integer, save :: init_step = 0    !initial step count
#ifdef PERF
  integer(kind=c_long_long), save :: t0,t1,tt,tread,texec,twrite,tiowrite,tioread,tbarrier,tbarrier2
#endif
  

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





  subroutine gem2buf(equil_in, corep_in, coret_out) 
#ifdef MPI2
    use mpi
#endif
    use iso_c_binding
    use string_binding
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

    integer(kind=c_signed_char), pointer :: corep_in(:)
    integer(kind=c_signed_char), pointer :: equil_in(:)
    integer(kind=c_signed_char), pointer :: coret_out(:)
    integer(kind=c_signed_char), pointer :: tmpbuf(:)

    type (type_equilibrium), pointer :: equil(:) => NULL()
    type (type_coreprof), pointer :: corep(:) => NULL()
    type (type_coretransp), pointer :: coret(:) => NULL()
    type (type_param) :: code_parameters

    character(F_STR_SIZE) :: corep_file_in, equil_file_in, coret_file_out, username, tmpdir
    integer :: tmpsize

    integer :: ios
    integer, save :: cpt = 0
    character(4)  :: cptstr

#ifdef MPI
    integer :: ierr, npes, irank, ipe
    call MPI_Comm_size(MPI_COMM_WORLD, npes, ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, irank, ierr)

#ifdef DEBUG
    if (irank.eq.0) then
       print *,"fortran GEM wrapper"
    endif
#endif
#endif


    allocate(corep(1))
    allocate(equil(1))    


#ifdef PERF
    call c_getMillis(t0)
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    call c_getMillis(t1)
    tbarrier = t1 - t0
#endif



#ifdef PERF
    call c_getMillis(t0)
#endif
    ! transfer buf to CPO
    call getenv("USER",username)
    call getenv("MUSCLE_TMP_DIR",tmpdir)
    tmpsize = len_trim(tmpdir)
    if (tmpsize.ne.0) then
       if (tmpdir(tmpsize:tmpsize) .ne. '/') then
          tmpdir = trim(tmpdir)//'/'
       end if
    end if
    corep_file_in = TRIM(tmpdir)//TRIM(username)//'_gem_coreprof_in.cpo'
    equil_file_in = TRIM(tmpdir)//TRIM(username)//'_gem_equilibrium_in.cpo'
    call byte2file(corep_file_in, corep_in, size(corep_in))
    call byte2file(equil_file_in, equil_in, size(equil_in))
 
#ifdef PERF
    call c_getMillis(tt)
    tiowrite = tt - t0
#endif


#ifdef MPI
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
#ifdef PERF
    call c_getMillis(t1)
    tbarrier2 = t1 - tt
#endif
#endif

#ifdef PERF
    call c_getMillis(tt)
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
#ifdef PERF
    call c_getMillis(t1)
    tread = t1 - t0
    tioread = t1 - tt
#endif


#ifdef PERF
    call c_getMillis(t0)
#endif
    call fill_param(code_parameters, 'gem.xml', '', 'gem.xsd')

    call gem(equil, corep, coret, code_parameters)
#ifdef PERF
    call c_getMillis(t1)
    texec = t1 - t0
#endif




#ifdef MPI
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    if (irank.eq.0) then
#endif
#ifdef PERF
       call c_getMillis(t0)
#endif
       ! transfer CPO to buf
       print *,"write coretransp CPO"
       write(cptstr,'(I4.4)') init_step+cpt
       cpt = cpt+1
       coret_file_out = 'gem_coretransp_'//cptstr//'.cpo'
       call open_write_file(12,coret_file_out)
       call write_cpo(coret(1),'coretransp')
       call close_write_file

       call file2byte(coret_file_out, tmpbuf, tmpsize)
       allocate(coret_out(tmpsize))
       coret_out(1:tmpsize) = tmpbuf(1:tmpsize)
       call dealloc_cbytebuf(tmpbuf)

#ifdef PERF
       call c_getMillis(t1)
       twrite = t1 - t0
#endif
#ifdef MPI
    endif
#endif

    call deallocate_cpo(corep)
    call deallocate_cpo(equil)
    call deallocate_cpo(coret)

    if (associated(code_parameters%schema)) then
       deallocate(code_parameters%schema)
    endif
    if (associated(code_parameters%parameters)) then
       deallocate(code_parameters%parameters)
    endif
    if (associated(code_parameters%default_param)) then
       deallocate(code_parameters%default_param)
    endif

  end subroutine gem2buf


  
end module gem_wrapper
