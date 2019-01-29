program dfefi_kernelB
#ifdef MPI2
  use mpi
#endif
  use dfefi_wrapper
  use muscle_fortran
  use string_binding
  implicit none
#ifdef MPI
#ifndef MPI2
  include "mpif.h"
#endif
#endif

  character(kind=c_char,len=255) :: propName, kernelName, prop

  integer(kind=c_signed_char), pointer :: corep_in(:), corep_buf(:)
  integer(kind=c_signed_char), pointer :: equil_in(:), equil_buf(:)
  integer(kind=c_signed_char), pointer :: coret_out(:)

  integer :: corep_size, equil_size

#ifdef MPI
  integer :: ierr, npes, irank, ipe
  integer :: corep_root_size=0, equil_root_size=0
#endif

  integer :: it
  real(8)    :: tau=0.               !time step
  integer    :: target_step          !targeted step count

#ifdef PERF
  integer(kind=c_long_long) :: tt0,tt1,trecv,tsend,twrap
#endif


  print *,'DFEFI_B kernel: start'

#ifdef MPI
  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, npes, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, irank, ierr)

  if (irank.eq.0) then
#endif

     call muscle_fortran_init()

     call MUSCLE_Kernel_Name(kernelName)
     print *,'kernelName: ',trim(kernelName)

     propName = c_char_"command"//char(0)
     call MUSCLE_Get_Property(propName, prop)
     print *,'prop:',trim(prop)

#ifdef MPI
     call muscle_step_props(init_step, target_step, tau)

  endif

  call MPI_Bcast(init_step, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(target_step, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(tau, 1, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)

#ifdef DEBUG
  print *,'rank',irank,'tau = ',tau
  print *,'rank',irank,'init_step = ',init_step
  print *,'rank',irank,'target_step = ',target_step
#endif

#endif
  
  allocate(corep_buf(BUF_SIZE))
  allocate(equil_buf(BUF_SIZE))

  do it = init_step,target_step

#ifdef MPI
     if (irank.eq.0) then
#endif
#ifdef PERF
        call c_getMillis(tt0)
#endif
        print *,'DFEFI_B kernel: receive data'
        corep_size = BUF_SIZE
        call Muscle_Receive(c_char_"coreprof_in"//char(0), corep_buf, &
             corep_size, %REF(MUSCLE_RAW))

        equil_size = BUF_SIZE
        call Muscle_Receive(c_char_"equilibrium_in"//char(0), equil_buf, &
             equil_size, %REF(MUSCLE_RAW))
#ifdef PERF
        call c_getMillis(tt1)
        trecv = tt1 - tt0
#endif


#ifdef PERF
        call c_getMillis(tt0)
#endif
#ifdef MPI
     endif

     call MPI_Bcast(corep_size, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
     call MPI_Bcast(equil_size, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

     call MPI_Bcast(corep_buf, corep_size, MPI_BYTE, & 
          0, MPI_COMM_WORLD, ierr)

     call MPI_Bcast(equil_buf, equil_size, MPI_BYTE, & 
          0, MPI_COMM_WORLD, ierr)

     corep_in => corep_buf(1:corep_size)
     equil_in => equil_buf(1:equil_size)

     call MPI_Barrier(MPI_COMM_WORLD, ierr)
#endif
     
     call dfefi2buf(equil_in, corep_in, coret_out)
#ifdef PERF
     call c_getMillis(tt1)
     twrap = tt1 - tt0
#endif

#ifdef MPI
     if (irank.eq.0) then
#ifdef PERF
        call c_getMillis(tt0)
#endif
        print *,'DFEFI_B kernel: send data'
        call MUSCLE_Send(c_char_"coretransp_out"//char(0), coret_out, &
             %REF(size(coret_out)) ,%REF(MUSCLE_RAW))
#ifdef PERF
        call c_getMillis(tt1)
        tsend = tt1 - tt0
        print *,'PERF_DFEFI:',it,trecv,twrap,tsend,tread,tbarrier,tiowrite,tioread,texec,twrite
#endif
        deallocate(coret_out)
     endif
#endif

  end do

#ifdef DEBUG
  print *,'DFEFI_B kernel: end of loop'
#endif

#ifdef MPI
  if (irank.eq.0) then 
     call MUSCLE_Finalize()

  endif
#endif

  deallocate(corep_buf)
  deallocate(equil_buf)

#ifdef MPI
  call MPI_Finalize(ierr)
#endif

end program dfefi_kernelB


