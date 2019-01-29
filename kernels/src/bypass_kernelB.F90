program bypass_kernelB
  use muscle_fortran
  use string_binding
#ifdef PERF
  use c_perf
#endif
  implicit none

  character(kind=c_char,len=255) :: propName, kernelName, prop

  integer(kind=c_signed_char), pointer :: cpo(:), cpo_buf(:)
  integer :: cpo_size

  logical :: end_loop

  integer       :: it
  real(8)       :: tau               !time step
  integer       :: target_step       !targeted step count
  integer, save :: init_step = 0     !initial step count

#ifdef PERF
  integer(kind=c_long_long) :: tt0,tt1,trecv,tsend,twrap
#endif

  print *,'BYPASS kernel: start'

  call muscle_fortran_init()

  call MUSCLE_Kernel_Name(kernelName)
  print *,'kernelName: ',trim(kernelName)

  propName = c_char_"command"//char(0)
  call MUSCLE_Get_Property(propName, prop)
  print *,'prop:',trim(prop)

  call muscle_step_props(init_step, target_step, tau)

  allocate(cpo_buf(BUF_SIZE))

  print *,'***** INITIALIZATION *****'

  print *,'BYPASS kernel: receive init data'
  cpo_size = BUF_SIZE
  call Muscle_Receive(c_char_"cpo_in_init"//char(0), cpo_buf, &
       cpo_size, %REF(MUSCLE_RAW))
  cpo => cpo_buf(1:cpo_size)
  
  print *,'***** START: SENDS INIT CPO *****'
  !start or restart: we send directly input 
  call MUSCLE_Send(c_char_"cpo_out"//char(0), cpo, &
       %REF(size(cpo)) ,%REF(MUSCLE_RAW))

  do it = init_step,target_step
     print *,'***** STEP = ',it,'*****'

#ifdef PERF
     call c_getMillis(tt0)
#endif
     print *,'BYPASS kernel: receive data'
     cpo_size = BUF_SIZE
     call Muscle_Receive(c_char_"cpo_in_loop"//char(0), cpo_buf, &
          cpo_size, %REF(MUSCLE_RAW))
     cpo => cpo_buf(1:cpo_size)
#ifdef PERF
     call c_getMillis(tt1)
     trecv = tt1 - tt0
#endif


#ifdef PERF
     call c_getMillis(tt0)
#endif
     print *,'BYPASS kernel: send data'
     call MUSCLE_Send(c_char_"cpo_out"//char(0), cpo, &
          %REF(size(cpo)) ,%REF(MUSCLE_RAW))
#ifdef PERF
     call c_getMillis(tt1)
     tsend = tt1 - tt0
     print *,'PERF_BYPASS:',it,trecv,tsend
#endif

  end do

  call MUSCLE_Finalize()

  print *,'deallocate *buf'
  deallocate(cpo_buf)

end program bypass_kernelB
