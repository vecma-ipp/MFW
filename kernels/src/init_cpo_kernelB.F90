program init_cpo_kernelB
  use init_cpo_wrapper
  use muscle_fortran
  use string_binding
  implicit none

  character(kind=c_char,len=255) :: propName, kernelName, prop
  
  character(kind=c_char,len=F_STR_SIZE) :: cpo_file_in

  integer(kind=c_signed_char), pointer :: corep_out(:)
  integer(kind=c_signed_char), pointer :: coret_out(:)
  integer(kind=c_signed_char), pointer :: cores_out(:)
  integer(kind=c_signed_char), pointer :: corei_out(:)
  integer(kind=c_signed_char), pointer :: coren_out(:)
  integer(kind=c_signed_char), pointer :: equil_out(:)
  integer(kind=c_signed_char), pointer :: toroidf_out(:)


  print *,'INIT_CPO_B kernel: start'

  call muscle_fortran_init()

  call MUSCLE_Kernel_Name(kernelName)
  print *,'kernelName: ',trim(kernelName)

  propName = c_char_"command"//char(0)
  call MUSCLE_Get_Property(propName, prop)
  print *,'INIT_CPO_B kernel: command=',trim(prop)

  propName = c_char_"cpo_file"//char(0)
  call MUSCLE_Get_Property(propName, prop)
  cpo_file_in = trim(prop)
  print *,'INIT_CPO_B kernel: cpofile=',trim(cpo_file_in)

  ! get modifiers coefs for reshaping density profile
  propName = c_char_"n_add_coef"//C_NULL_CHAR
  call MUSCLE_Get_Property(propName, prop)
  print *,trim(propName),' = ',trim(prop)
  read(prop,'(f10.15)') n_addcoef
  
  propName = c_char_"n_mult_coef"//C_NULL_CHAR
  call MUSCLE_Get_Property(propName, prop)
  print *,trim(propName),' = ',trim(prop)
  read(prop,'(f10.15)') n_multcoef
  

  

  print *,'INIT_CPO_B kernel: call native routine'
  call init_cpo2buf(cpo_file_in, &
       corep_out, coret_out, cores_out, &
       corei_out, coren_out, equil_out, &
       toroidf_out)

  print *,'INIT_CPO_B kernel: send data'


  print *,'INIT_CPO_B kernel: corep_out = ',size(corep_out)/1024,' KB'
  call MUSCLE_Send(c_char_"coreprof_out"//char(0), corep_out, &
       %REF(size(corep_out)) ,%REF(MUSCLE_RAW))

  print *,'INIT_CPO_B kernel: coret_out = ',size(coret_out)/1024,' KB'
  call MUSCLE_Send(c_char_"coretransp_out"//char(0), coret_out, &
       %REF(size(coret_out)) ,%REF(MUSCLE_RAW))

  print *,'INIT_CPO_B kernel: cores_out = ',size(cores_out)/1024,' KB'
  call MUSCLE_Send(c_char_"coresource_out"//char(0), cores_out, &
       %REF(size(cores_out)) ,%REF(MUSCLE_RAW))

  print *,'INIT_CPO_B kernel: corei_out = ',size(corei_out)/1024,' KB'
  call MUSCLE_Send(c_char_"coreimpur_out"//char(0), corei_out, &
       %REF(size(corei_out)) ,%REF(MUSCLE_RAW))

  print *,'INIT_CPO_B kernel: equil_out = ',size(equil_out)/1024,' KB'
  call MUSCLE_Send(c_char_"equilibrium_out"//char(0), equil_out, &
       %REF(size(equil_out)) ,%REF(MUSCLE_RAW))

  print *,'INIT_CPO_B kernel: toroidf_out = ',size(toroidf_out)/1024,' KB'
  call MUSCLE_Send(c_char_"toroidfield_out"//char(0), toroidf_out, &
       %REF(size(toroidf_out)) ,%REF(MUSCLE_RAW))

  print *,'INIT_CPO_B kernel: END'

  call MUSCLE_Finalize()


  print *,'deallocate *out'
  deallocate(corep_out)
  deallocate(coret_out)
  deallocate(cores_out)
  deallocate(corei_out)
  deallocate(coren_out)
  deallocate(equil_out)
  deallocate(toroidf_out)


end program init_cpo_kernelB


