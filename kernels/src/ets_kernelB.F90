program ets_kernelB
  use ets_wrapper
  use muscle_fortran
  use string_binding
  implicit none

  character(kind=c_char,len=255) :: propName, kernelName, prop

  integer(kind=c_signed_char), pointer :: corep_in(:), corep_buf(:)
  integer(kind=c_signed_char), pointer :: equil_in(:), equil_buf(:)
  integer(kind=c_signed_char), pointer :: coret_in(:), coret_buf(:)
#ifdef CONST_TRANSP
  integer(kind=c_signed_char), pointer :: coret_save(:)
#endif
  integer(kind=c_signed_char), pointer :: cores_in(:), cores_buf(:)
  integer(kind=c_signed_char), pointer :: corei_in(:), corei_buf(:)
  integer(kind=c_signed_char), pointer :: toroidf_in(:), toroidf_buf(:)

  integer(kind=c_signed_char), pointer :: corep_out(:)
  integer(kind=c_signed_char), pointer :: equil_out(:)

  integer :: corep_size, equil_size, coret_size, cores_size
  integer :: corei_size, toroidf_size

  integer :: control_integer(5)
  real(8)    :: control_double(6)

  logical :: end_loop

  integer :: it
  real(8)    :: tau                 !time step
  integer    :: target_step         !targeted step count

#ifdef PERF
  integer(kind=c_long_long) :: tt0,tt1,trecv,tsend,twrap
#endif

  print *,'ETS_B kernel: start'

  call muscle_fortran_init()

  call MUSCLE_Kernel_Name(kernelName)
  print *,'kernelName: ',trim(kernelName)

  propName = c_char_"command"//char(0)
  call MUSCLE_Get_Property(propName, prop)
  print *,'prop:',trim(prop)

  call muscle_step_props(init_step, target_step, tau)
  call muscle_ets_control(control_integer, control_double)

  allocate(equil_buf(BUF_SIZE))
  allocate(corep_buf(BUF_SIZE))
  allocate(coret_buf(BUF_SIZE))
  allocate(cores_buf(BUF_SIZE))
  allocate(corei_buf(BUF_SIZE))
  allocate(toroidf_buf(BUF_SIZE))


  print *,'***** INITIALIZATION *****'

  print *,'ETS_B kernel: receive init data'
  corep_size = BUF_SIZE
  call Muscle_Receive(c_char_"coreprof_init"//char(0), corep_buf, &
       corep_size, %REF(MUSCLE_RAW))
  corep_in => corep_buf(1:corep_size)
  coret_size = BUF_SIZE
  call Muscle_Receive(c_char_"coretransp_init"//char(0), coret_buf, &
       coret_size, %REF(MUSCLE_RAW))
  coret_in => coret_buf(1:coret_size)
#ifdef CONST_TRANSP
  allocate(coret_save(size(coret_in)))
  coret_save = coret_in
#endif
  cores_size = BUF_SIZE
  call Muscle_Receive(c_char_"coresource_init"//char(0), cores_buf, &
       cores_size, %REF(MUSCLE_RAW))
  cores_in => cores_buf(1:cores_size)
  corei_size = BUF_SIZE
  call Muscle_Receive(c_char_"coreimpur_init"//char(0), corei_buf, &
       corei_size, %REF(MUSCLE_RAW))
  corei_in => corei_buf(1:corei_size)
  equil_size = BUF_SIZE
  call Muscle_Receive(c_char_"equilibrium_init"//char(0), equil_buf, &
       equil_size, %REF(MUSCLE_RAW))
  equil_in => equil_buf(1:equil_size)
  toroidf_size = BUF_SIZE
  call Muscle_Receive(c_char_"toroidfield_init"//char(0), toroidf_buf, &
       toroidf_size, %REF(MUSCLE_RAW))
  toroidf_in => toroidf_buf(1:toroidf_size)
  
!!$  if (init_step.eq.0) then
!!$     print *,'***** INITIAL STEP OF ETS (smoothing input data) *****'
!!$     call ets2buf(corep_in, equil_in, coret_in, &
!!$          cores_in, corei_in, toroidf_in, &
!!$          control_integer, control_double, &
!!$          corep_out, equil_out)
!!$     call MUSCLE_Send(c_char_"coreprof_out"//char(0), corep_out, &
!!$          %REF(size(corep_out)) ,%REF(MUSCLE_RAW))
!!$     call MUSCLE_Send(c_char_"equilibrium_out"//char(0), equil_out, &
!!$          %REF(size(equil_out)) ,%REF(MUSCLE_RAW))
!!$     deallocate(corep_out)
!!$     deallocate(equil_out)
!!$  else
  print *,'***** START: SKIP FIRST CALL OF ETS *****'
  !start or restart: we send directly input 
  call MUSCLE_Send(c_char_"coreprof_out"//char(0), corep_in, &
       %REF(size(corep_in)) ,%REF(MUSCLE_RAW))
  call MUSCLE_Send(c_char_"equilibrium_out"//char(0), equil_in, &
       %REF(size(equil_in)) ,%REF(MUSCLE_RAW))



  do it = init_step,target_step
     print *,'***** STEP = ',it,'*****'

#ifdef PERF
     call c_getMillis(tt0)
#endif
     print *,'ETS_B kernel: receive data'
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
#ifdef CONST_TRANSP
     coret_in => coret_save
#else
     coret_in => coret_buf(1:coret_size)
#endif
#ifdef PERF
     call c_getMillis(tt1)
     trecv = tt1 - tt0
#endif

     print *,'ETS_B kernel: call native routine'

#ifdef PERF
     call c_getMillis(tt0)
#endif
     call ets2buf(corep_in, equil_in, coret_in, &
          cores_in, corei_in, toroidf_in, &
          control_integer, control_double, &
          corep_out, equil_out)
#ifdef PERF
     call c_getMillis(tt1)
     twrap = tt1 - tt0
#endif


#ifdef PERF
     call c_getMillis(tt0)
#endif
     print *,'ETS_B kernel: send data'
     call MUSCLE_Send(c_char_"coreprof_out"//char(0), corep_out, &
          %REF(size(corep_out)) ,%REF(MUSCLE_RAW))
     call MUSCLE_Send(c_char_"equilibrium_out"//char(0), equil_out, &
          %REF(size(equil_out)) ,%REF(MUSCLE_RAW))
#ifdef PERF
     call c_getMillis(tt1)
     tsend = tt1 - tt0
     print *,'PERF_ETS:',it,trecv,twrap,tsend,tread,texec,twrite
#endif

     if (it.lt.target_step) then
        deallocate(corep_out)
        deallocate(equil_out)
     end if
     
  end do

!this might not be needed as we start with an additional send from ETS kernel
!!$  print *,'ETS_B kernel: end of loop'
!!$  ! last receive?
!!$  print *,'ETS_B kernel: receive data'
!!$  call Muscle_Receive(c_char_"equilibrium_in"//char(0), equil_buf, &
!!$       equil_size, %REF(MUSCLE_RAW))
!!$  equil_in => equil_buf(1:equil_size)
!!$  
!!$  call Muscle_Receive(c_char_"coreprof_in"//char(0), corep_buf, &
!!$       corep_size, %REF(MUSCLE_RAW))
!!$  corep_in => corep_buf(1:corep_size)
!!$  
!!$  call Muscle_Receive(c_char_"coretransp_in"//char(0), coret_buf, &
!!$       coret_size, %REF(MUSCLE_RAW))
!!$  coret_in => coret_buf(1:coret_size)
  
  !write last .in files in order to ease continued runs
  call byte2file('ets_coreprof_in.cpo', corep_out, size(corep_out))
  call byte2file('ets_equilibrium_in.cpo', equil_out, size(equil_out))
  call byte2file('ets_coretransp_in.cpo', coret_in, size(coret_in))
  call byte2file('ets_coresource_in.cpo', cores_in, size(cores_in))
  call byte2file('ets_coreimpur_in.cpo', corei_in, size(corei_in))
  call byte2file('ets_toroidfield_in.cpo', toroidf_in, size(toroidf_in))

  deallocate(corep_out)
  deallocate(equil_out)
  
  call MUSCLE_Finalize()


  print *,'deallocate *buf'
  deallocate(equil_buf)
  deallocate(corep_buf)
  deallocate(coret_buf)
  deallocate(cores_buf)
  deallocate(corei_buf)
  deallocate(toroidf_buf)


contains

  subroutine muscle_ets_control(control_integer, control_double)
    use iso_c_binding
    implicit none
    integer, intent(out) :: control_integer(5)
    real(8), intent(out) :: control_double(6)
    character(kind=c_char, len=255) :: prop, propName

    control_integer = 0

    propName = c_char_"solver_type"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(i4)') control_integer(1)

    propName = c_char_"sigma_source"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(i4)') control_integer(2)

    propName = c_char_"quasi_neut"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(i4)') control_integer(3)

    propName = c_char_"tau"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') control_double(1)

    propName = c_char_"amix"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') control_double(2)

    propName = c_char_"amixtr"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') control_double(3)

    propName = c_char_"conv"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') control_double(4)

    propName = c_char_"convrec"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') control_double(5)

    propName = c_char_"ohmic_heating_multiplier"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') control_double(6)


    ! inner stepping and evolution control values
    propName = c_char_"inner_steps_init"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(i6)') inner_steps_init

    propName = c_char_"inner_steps_limit"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(i9)') inner_steps_limit

    propName = c_char_"inner_steps_incr_factor"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(i6)') inner_steps_incr_factor

    propName = c_char_"limit_te_deviation"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') delTe_limit

    propName = c_char_"limit_dte_deviation"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') deldTe_limit


    ! floor values for D's and V's at the core and the edge
    propName = c_char_"d_floor"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') d_floor

    propName = c_char_"d_ceil"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') d_ceil

    propName = c_char_"d_max_evo"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') d_max_evo

    propName = c_char_"floor_alpha"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') floor_alpha

    propName = c_char_"floor_beta"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') floor_beta

    propName = c_char_"floor_gamma"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') floor_gamma

!!$    do i=1,100
!!$       d_prof(i) = floor_alpha + floor_beta * (0.01 * i) ** floor_gamma
!!$    end do

!!$    propName = c_char_"pseudo_conv"//C_NULL_CHAR
!!$    call MUSCLE_Get_Property(propName, prop)
!!$    print *,trim(propName),' = ',trim(prop)
!!$    read(prop,'(l)') pseudo_conv

    propName = c_char_"limit_evo"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(l)') limit_evo

    propName = c_char_"d_limit"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(l)') d_limit

    propName = c_char_"edge_d_cst"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(l)') e_d_cst

    propName = c_char_"core_d_cst"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(l)') c_d_cst

    propName = c_char_"v_floor"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') v_floor

    propName = c_char_"v_ceil"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') v_ceil

    propName = c_char_"v_max_evo"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f10.15)') v_max_evo

!!$    v_prof = v_floor * v_prof

    propName = c_char_"v_limit"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(l)') v_limit

    propName = c_char_"edge_v_cst"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(l)') e_v_cst

    propName = c_char_"core_v_cst"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(l)') c_v_cst

    propName = c_char_"end_loop"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(l)') end_loop

  end subroutine muscle_ets_control

end program ets_kernelB
