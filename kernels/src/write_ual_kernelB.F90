program write_ual_kernelB
  use write_ual_wrapper
  use muscle_fortran
  use string_binding
  implicit none

  character(kind=c_char,len=255) :: propName, kernelName, prop

  integer(kind=c_signed_char), pointer :: corep_in(:), corep_buf(:)
  integer(kind=c_signed_char), pointer :: equil_in(:), equil_buf(:)
  integer(kind=c_signed_char), pointer :: coret_in(:), coret_buf(:)

  integer :: corep_size, equil_size, coret_size

  integer :: it, step0
  real(8)    :: tau                 !time step
  integer    :: init_step           !initial step count
  integer    :: target_step         !targeted step count

  print *,'WRITE_UAL kernel: start'

  call muscle_fortran_init()

  call MUSCLE_Kernel_Name(kernelName)
  print *,'kernelName: ',trim(kernelName)

  propName = c_char_"command"//char(0)
  call MUSCLE_Get_Property(propName, prop)
  print *,'WRITE_UAL kernel: command=',trim(prop)


  ! get shot, run numbers and machine name
  propName = c_char_"shot"//C_NULL_CHAR
  call MUSCLE_Get_Property(propName, prop)
  print *,trim(propName),' = ',trim(prop)
  read(prop,'(i10)') shot

  propName = c_char_"run"//C_NULL_CHAR
  call MUSCLE_Get_Property(propName, prop)
  print *,trim(propName),' = ',trim(prop)
  read(prop,'(i10)') run

  propName = c_char_"machine"//C_NULL_CHAR
  call MUSCLE_Get_Property(propName, prop)
  print *,trim(propName),' = ',trim(prop)
  machine = trim(prop)

  call muscle_step_props(init_step, target_step, tau)

  allocate(corep_buf(BUF_SIZE))
  allocate(coret_buf(BUF_SIZE))
  allocate(equil_buf(BUF_SIZE))

  do it = init_step,target_step

     ! receive CPOs
     print *,'WRITE_UAL kernel: receive data'
     corep_size = BUF_SIZE
     call Muscle_Receive(c_char_"coreprof_in"//char(0), corep_buf, &
          corep_size, %REF(MUSCLE_RAW))
     corep_in => corep_buf(1:corep_size)

     equil_size = BUF_SIZE
     call Muscle_Receive(c_char_"equilibrium_in"//char(0), equil_buf, &
          equil_size, %REF(MUSCLE_RAW))
     equil_in => equil_buf(1:equil_size)

     coret_size = BUF_SIZE
     call Muscle_Receive(c_char_"coretransp_in"//char(0), coret_buf, &
          coret_size, %REF(MUSCLE_RAW))
     coret_in => coret_buf(1:coret_size)


     print *,'WRITE_UAL kernel: call native routine'
     call write_buf2ual(corep_in, coret_in, equil_in)

  end do

  
  print *,'WRITE_UAL kernel: end of loop'
  call close_buf2ual()

  call MUSCLE_Finalize()


  print *,'deallocate *buf'
  deallocate(corep_buf)
  deallocate(coret_buf)
  deallocate(equil_buf)


end program write_ual_kernelB


