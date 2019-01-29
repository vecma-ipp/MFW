program chease_kernelB
  use chease_wrapper
  use muscle_fortran
  use string_binding
  implicit none

  integer :: i
  character(kind=c_char,len=255) :: propName, kernelName, prop

  integer(kind=c_signed_char), pointer :: equil_in(:), equil_buf(:)
  integer(kind=c_signed_char), pointer :: equil_out(:)

  integer :: recv_size

  integer :: it
  real(8)    :: tau                 !time step
  integer    :: target_step         !targeted step count

#ifdef PERF
  integer(kind=c_long_long) :: tt0,tt1,trecv,tsend,twrap
#endif

  print *,'CHEASE_B kernel: start'

  call muscle_fortran_init()

  call MUSCLE_Kernel_Name(kernelName)
  print *,'kernelName: ',trim(kernelName)

  propName = c_char_"command"//char(0)
  call MUSCLE_Get_Property(propName, prop)
  print *,'prop:',trim(prop)

  call muscle_step_props(init_step, target_step, tau)
  
  allocate(equil_buf(BUF_SIZE))

  do it = init_step,target_step

#ifdef PERF
     call c_getMillis(tt0)
#endif
     print *,'CHEASE_B kernel: receive data'
     recv_size = BUF_SIZE
     call Muscle_Receive(c_char_"equilibrium_in"//char(0), equil_buf, &
          recv_size, %REF(MUSCLE_RAW))
     equil_in => equil_buf(1:recv_size)
#ifdef PERF
     call c_getMillis(tt1)
     trecv = tt1 - tt0
#endif

#ifdef PERF
     call c_getMillis(tt0)
#endif
     print *,'CHEASE_B kernel: call native routine'
     call chease2buf(equil_in, equil_out)
#ifdef PERF
     call c_getMillis(tt1)
     twrap = tt1 - tt0
#endif

#ifdef PERF
     call c_getMillis(tt0)
#endif
     print *,'CHEASE_B kernel: send data'
     call MUSCLE_Send(c_char_"equilibrium_out"//char(0), equil_out, &
          %REF(size(equil_out)) ,%REF(MUSCLE_RAW))
#ifdef PERF
     call c_getMillis(tt1)
     tsend = tt1 - tt0
     print *,'PERF_CHEASE:',it,trecv,twrap,tsend,tread,texec,twrite
#endif

     !print *,'deallocate *out'
     deallocate(equil_out)
  end do

  print *,'CHEASE_B kernel: end of loop'


  call MUSCLE_Finalize()

  print *,'deallocate *buf'
  deallocate(equil_buf)

end program chease_kernelB


