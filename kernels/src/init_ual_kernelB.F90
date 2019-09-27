program init_ual_kernelB
  use init_ual_wrapper
  use muscle_fortran
  use string_binding
  implicit none

  character(kind=c_char,len=255) :: propName, kernelName, prop
  
  character(255) :: user, machine, version
  integer :: shot,run
  real(8) :: time

  !character(kind=c_char,len=F_STR_SIZE) :: cpo_file_in

  integer(kind=c_signed_char), pointer :: corep_out(:)
  integer(kind=c_signed_char), pointer :: coret_out(:)
  integer(kind=c_signed_char), pointer :: cores_out(:)
  integer(kind=c_signed_char), pointer :: corei_out(:)
  integer(kind=c_signed_char), pointer :: equil_out(:)
  integer(kind=c_signed_char), pointer :: toroidf_out(:)



  print *,'INIT_FROM_UAL kernel: start'

  call muscle_fortran_init()

  call MUSCLE_Kernel_Name(kernelName)
  print *,'kernelName: ',trim(kernelName)

  propName = c_char_"command"//char(0)
  call MUSCLE_Get_Property(propName, prop)
  print *,'INIT_FROM_UAL kernel: command=',trim(prop)


  ! get modifiers coefs for reshaping density profile
  propName = c_char_"n_add_coef"//C_NULL_CHAR
  call MUSCLE_Get_Property(propName, prop)
  print *,trim(propName),' = ',trim(prop)
  read(prop,'(f10.15)') n_addcoef
  
  propName = c_char_"n_mult_coef"//C_NULL_CHAR
  call MUSCLE_Get_Property(propName, prop)
  print *,trim(propName),' = ',trim(prop)
  read(prop,'(f10.15)') n_multcoef

  propName = c_char_"solve_Ne"//C_NULL_CHAR
  call MUSCLE_Get_Property(propName, prop)
  print *,trim(propName),' = ',trim(prop)
  read(prop,'(i10)') solve_ne

  propName = c_char_"solve_Ni"//C_NULL_CHAR
  call MUSCLE_Get_Property(propName, prop)
  print *,trim(propName),' = ',trim(prop)
  read(prop,'(i10)') solve_ni

  propName = c_char_"solve_psi"//C_NULL_CHAR
  call MUSCLE_Get_Property(propName, prop)
  print *,trim(propName),' = ',trim(prop)
  read(prop,'(i10)') solve_psi


!!$  propName = c_char_"cpo_file"//char(0)
!!$  call MUSCLE_Get_Property(propName, prop)
!!$  cpo_file_in = trim(prop)
!!$  print *,'INIT_FROM_UAL kernel: cpofile=',trim(cpo_file_in)

  call muscle_ual_params(user,machine,version,shot,run,time)
  

  print *,'INIT_FROM_UAL kernel: call native routine'
  call init_ual2buf(user,machine,version,shot,run,time, &
       corep_out, coret_out, cores_out, &
       corei_out, equil_out, toroidf_out)

  print *,'INIT_FROM_UAL kernel: send data'


  print *,'INIT_FROM_UAL kernel: corep_out = ',size(corep_out)/1024,' KB'
  call MUSCLE_Send(c_char_"coreprof_out"//char(0), corep_out, &
       %REF(size(corep_out)) ,%REF(MUSCLE_RAW))

  print *,'INIT_FROM_UAL kernel: coret_out = ',size(coret_out)/1024,' KB'
  call MUSCLE_Send(c_char_"coretransp_out"//char(0), coret_out, &
       %REF(size(coret_out)) ,%REF(MUSCLE_RAW))

  print *,'INIT_FROM_UAL kernel: cores_out = ',size(cores_out)/1024,' KB'
  call MUSCLE_Send(c_char_"coresource_out"//char(0), cores_out, &
       %REF(size(cores_out)) ,%REF(MUSCLE_RAW))

  print *,'INIT_FROM_UAL kernel: corei_out = ',size(corei_out)/1024,' KB'
  call MUSCLE_Send(c_char_"coreimpur_out"//char(0), corei_out, &
       %REF(size(corei_out)) ,%REF(MUSCLE_RAW))

  print *,'INIT_FROM_UAL kernel: equil_out = ',size(equil_out)/1024,' KB'
  call MUSCLE_Send(c_char_"equilibrium_out"//char(0), equil_out, &
       %REF(size(equil_out)) ,%REF(MUSCLE_RAW))

  print *,'INIT_FROM_UAL kernel: toroidf_out = ',size(toroidf_out)/1024,' KB'
  call MUSCLE_Send(c_char_"toroidfield_out"//char(0), toroidf_out, &
       %REF(size(toroidf_out)) ,%REF(MUSCLE_RAW))


  print *,'INIT_FROM_UAL kernel: END'

  call MUSCLE_Finalize()


  print *,'deallocate *out'
  deallocate(corep_out)
  deallocate(coret_out)
  deallocate(cores_out)
  deallocate(corei_out)
  deallocate(equil_out)
  deallocate(toroidf_out)


contains

  subroutine muscle_ual_params(user, machine, version, shot, run, time)
    use iso_c_binding
    implicit none
    integer, intent(out) :: shot, run
    real(8), intent(out) :: time
    character(len=255) :: user, machine, version
    character(kind=c_char, len=255) :: prop, propName
    
    propName = c_char_"user"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(A)')  user

    propName = c_char_"machine"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(A)')  machine

    propName = c_char_"version"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(A)')  version

    propName = c_char_"shot"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(i10)') shot

    propName = c_char_"run"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(i10)') run

    propName = c_char_"time"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') time

  end subroutine muscle_ual_params


end program init_ual_kernelB


