module ets_wrapper
  use euitm_schemas
  use c_tools
#ifdef PERF
  use c_perf
#endif
  implicit none

  logical, save :: d_limit = .false.
  logical, save :: v_limit = .false.
  logical, save :: limit_evo = .false.
  real(8), save :: d_max_evo = 0._8
  real(8), save :: v_max_evo = 0._8
  real(8), save :: d_floor = 0._8
  real(8), save :: d_ceil = 0._8
  real(8), save :: floor_alpha = 0._8
  real(8), save :: floor_beta = 0._8
  real(8), save :: floor_gamma = 0._8
  real(8), save :: v_floor = 0._8
  real(8), save :: v_ceil = 0._8
  logical, save :: e_d_cst = .false.
  logical, save :: e_v_cst = .false.
  logical, save :: c_d_cst = .false.
  logical, save :: c_v_cst = .false.
  logical, save :: pseudo_conv = .false.

  integer, save :: init_step = 0     !initial step count

  integer, save :: inner_steps_init = 0       !!! initial inner steps count
  integer, save :: inner_steps_limit = 0       !!! max number of inner steps (power of ten)
  integer, save :: inner_steps_incr_factor = 0 !!! factor to increase inner_steps_counts
  real(8), save :: delTe_limit = 0._8          !!! limit value for Te_frac
  real(8), save :: deldTe_limit = 0._8         !!! limit value for dTe_frac

  real(8), pointer :: d_prof(:) => NULL() ! 1. !(/ ((1/log(i+1.)),i=1,100) /)
  real(8), pointer :: v_prof(:) => NULL() ! 1. !(/ ((1/log(i+1.)),i=1,100) /)

  type (type_coretransp), pointer :: coret_old(:) => NULL()

#ifdef PERF
  integer(kind=c_long_long), save :: t0,t1,tread,texec,twrite
#endif
  

  interface
     subroutine ITM_ETS(corep_old, corep_iter, corep_new, &
          equil_old, equil_iter, coret,                   &
          cores, corei,                                   &
          control_integer, control_double,                &
          code_parameters)
       use euitm_schemas
       use itm_types
       type (type_coreprof), pointer :: corep_old(:), corep_iter(:), corep_new(:)
       type (type_equilibrium), pointer ::  equil_old(:), equil_iter(:)
       type (type_coretransp), pointer :: coret(:)
       type (type_coresource), pointer :: cores(:)
       type (type_coreimpur), pointer :: corei(:)
       integer   :: control_integer(2)
       real(R8)  :: control_double(6)  
       type (type_param) :: code_parameters
     end subroutine ITM_ETS

     subroutine equil_input(corep, toroidf, equil, equil_new)
       use euitm_schemas
       type (type_coreprof), pointer :: corep(:)
       type (type_toroidfield), pointer :: toroidf(:)
       type (type_equilibrium), pointer :: equil(:), equil_new(:)
     end subroutine equil_input
  end interface

contains

  
  subroutine ets2buf(corep_in, &
       equil_in, &
       coret_in, &
       cores_in, &
       corei_in, &
       toroidf_in, &
       control_integer, &
       control_double, &
       corep_out, &
       equil_out) 
    use iso_c_binding
    use string_binding
    use itm_types
    use read_structures
    use write_structures
    use copy_structures
    use deallocate_structures
    use allocate_deallocate
    use xml_file_reader
    use ets
    use equilibrium_input
#ifdef SIGMA_SPITZER
    use spitzer
#endif
    implicit none

    integer(kind=c_signed_char), pointer :: corep_in(:)
    integer(kind=c_signed_char), pointer :: equil_in(:)
    integer(kind=c_signed_char), pointer :: coret_in(:)
    integer(kind=c_signed_char), pointer :: cores_in(:)
    integer(kind=c_signed_char), pointer :: corei_in(:)
    integer(kind=c_signed_char), pointer :: toroidf_in(:)

    integer, intent(in)  :: control_integer(3)  !integer control parameters
    real(R8), intent(in) :: control_double(6)   !real control parameters

    integer(kind=c_signed_char), pointer :: corep_out(:)
    integer(kind=c_signed_char), pointer :: equil_out(:)
    integer(kind=c_signed_char), pointer :: tmpbuf(:)

    integer :: ios
    integer :: tmpsize
    logical :: tmp_d_bool, tmp_v_bool


    type (type_coreprof), pointer :: corep_old(:) => NULL(), corep_iter(:) => NULL(), corep_new(:) => NULL()
    type (type_equilibrium), pointer :: equil_old(:) => NULL(), equil_iter(:) => NULL(), equil_new(:) => NULL()
    type (type_coretransp), pointer :: coret(:) => NULL()
    type (type_coresource), pointer :: cores(:) => NULL()
    type (type_coreimpur), pointer :: corei(:) => NULL()
    type (type_toroidfield), pointer :: toroidf(:) => NULL()
    type (type_param) :: code_parameters

    type (type_coretransp), pointer :: coret_work(:) => NULL(), coret_ext(:) => NULL()
    type (type_coresource), pointer :: cores_work(:) => NULL()
#ifdef SIGMA_SPITZER
    type (type_coretransp), pointer :: coret_sigma(:) => NULL()
#endif

    real(R8)  :: tau                 !time step

    integer   :: ii                  !!! dummy variable
    integer   :: j                   !!! dummy variable
    real(R8)  :: dtime               !!! incremental time step size
    real      :: rho_tor_max         !!! max tho_tor value
    real :: Te_dev, dTe_dev
    real, allocatable :: Te_frac(:)   !!! deviation of Te
    real, allocatable :: dTe_frac(:)  !!! deviation of dTe/drho
    logical   :: exceeds_limit       !!! have we exceeded these limits?

    type (type_coreprof), pointer :: corep_old_test(:) => NULL(), corep_iter_test(:) => NULL(), corep_new_test(:) => NULL(), corep_ref(:) => NULL()
                                     !!! coreprof relevant to the Te test
    real(R8)  :: control_double_test(6)
                                     !!! control_double that replaces tau with dtime

    character(F_STR_SIZE) :: corep_in_file, corep_out_file
    character(F_STR_SIZE) :: equil_in_file, equil_out_file
    character(F_STR_SIZE) :: coret_in_file, cores_in_file
    character(F_STR_SIZE) :: corei_in_file, toroidf_in_file
    character(F_STR_SIZE) :: username, tmpdir

    integer, save :: cpt = 0
    character(4)  :: cptstr

    real(R8) :: time_in

    integer :: i
    integer :: nrho, nion, nimp, max_nzimp, NNUCL, NNEUT
    integer :: inner_steps_cur

    integer, allocatable :: nzimp(:), ntype(:), ncomp(:)

    tau = control_double(1)


    !...  read inputs
    allocate(corep_old(1))
    allocate(corep_iter(1))
    allocate(equil_old(1))
    allocate(equil_iter(1))
    allocate(coret(1))
    allocate(cores(1))
    allocate(corei(1))
    allocate(toroidf(1))

    !allocate(coret_work(1))
    allocate(coret_ext(1))
    allocate(cores_work(1))

#ifdef PERF
    call c_getMillis(t0)
#endif
    call getenv("USER",username)
    call getenv("MUSCLE_TMP_DIR",tmpdir)
    tmpsize = len_trim(tmpdir)
    if (tmpsize.ne.0) then
       if (tmpdir(tmpsize:tmpsize) .ne. '/') then
          tmpdir = trim(tmpdir)//'/'
       end if
    end if
    corep_in_file = TRIM(tmpdir)//TRIM(username)//'_ets_coreprof_in.cpo'
    equil_in_file = TRIM(tmpdir)//TRIM(username)//'_ets_equilibrium_in.cpo'
    coret_in_file = TRIM(tmpdir)//TRIM(username)//'_ets_coretransp_in.cpo'
    cores_in_file = TRIM(tmpdir)//TRIM(username)//'_ets_coresource_in.cpo'
    corei_in_file = TRIM(tmpdir)//TRIM(username)//'_ets_coreimpur_in.cpo'
    toroidf_in_file = TRIM(tmpdir)//TRIM(username)//'_ets_toroidfield_in.cpo'
    
    ! transfer buffer to CPO file
    call byte2file(corep_in_file, corep_in, size(corep_in))
    call byte2file(equil_in_file, equil_in, size(equil_in))
    call byte2file(coret_in_file, coret_in, size(coret_in))
    call byte2file(cores_in_file, cores_in, size(cores_in))
    call byte2file(corei_in_file, corei_in, size(corei_in))
    call byte2file(toroidf_in_file, toroidf_in, size(toroidf_in))

    ! read CPO file to CPO type
    open (unit = 10, file = corep_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, corep_in_file )
       call read_cpo(corep_old(1), 'coreprof' )
       call close_read_file
    else
       print *,"CPO file not found:",corep_in_file
       STOP
    end if

    open (unit = 11, file = equil_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (11)
       call open_read_file(11, equil_in_file )
       call read_cpo(equil_old(1), 'equilibrium' )
       call close_read_file
    else
       print *,"CPO file not found:",equil_in_file
       STOP
    end if

    open (unit = 12, file = coret_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (12)
       call open_read_file(12, coret_in_file )
       call read_cpo(coret(1), 'coretransp' )
       call close_read_file
    else
       print *,"CPO file not found:",coret_in_file
       STOP
    end if

    open (unit = 13, file = cores_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (13)
       call open_read_file(13, cores_in_file )
       call read_cpo(cores(1), 'coresource' )
       call close_read_file
    else
       print *,"CPO file not found:",cores_in_file
       STOP
    end if

    open (unit = 14, file = corei_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (14)
       call open_read_file(14, corei_in_file )
       call read_cpo(corei(1), 'coreimpur' )
       call close_read_file
    else
       print *,"CPO file not found:",corei_in_file
       STOP
    end if

    open (unit = 15, file = toroidf_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (15)
       call open_read_file(15, toroidf_in_file )
       call read_cpo(toroidf(1), 'toroidfield' )
       call close_read_file
    else
       print *,"CPO file not found:",toroidf_in_file
       STOP
    end if  

#ifdef PERF  
    call c_getMillis(t1)
    tread = t1-t0
    call c_getMillis(t0)
#endif    

    if (.not. ASSOCIATED(d_prof)) then 
       print *,"init background floor profile"
       nrho = size(corep_old(1)%rho_tor)
       allocate(d_prof(nrho))
       allocate(v_prof(nrho))
       do i=1,nrho
          d_prof(i) = floor_alpha + floor_beta * &
               ((1./real(nrho)) * i) ** floor_gamma
       end do
       v_prof = 0._8
    endif

    print *,'No convergence loop: copy old -> iter'
    call copy_cpo(corep_old(1),corep_iter(1))
    call copy_cpo(equil_old(1),equil_iter(1))

    time_in = corep_old(1)%time

    print *,"run ets"
    !...  run ETS
    call fill_param(code_parameters, 'ets.xml', '', 'ets.xsd')

    write(cptstr,'(I4.4)') init_step+cpt

    nrho = SIZE(corep_old(1)%rho_tor)
    nion = SIZE(corep_old(1)%ni%value, DIM=2)
    nimp = 0
    max_nzimp = 0
    
    !! ===> 4.10a <===
    nneut = 0
    nnucl = nion+nimp
    allocate(NZIMP(NIMP))
    allocate(NTYPE(NNEUT))
    allocate(NCOMP(NNEUT))

    print *,'nrho=',nrho,'nion=',nion
    
    
    !! DO I REALLY NEED THAT LINE ???
    CALL ALLOCATE_CORETRANSP_CPO (1, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, coret_work)

    print *,'interpolate_transp'

    
    if (ASSOCIATED(coret(1)%values(1)%rho_tor_norm)) then
       if (SIZE(coret(1)%values(1)%rho_tor_norm) < SIZE(corep_old(1)%rho_tor_norm)) then
          print *,'coretransp on different rho_tor'

          if (coret(1)%values(1)%rho_tor_norm(1) .gt. 0.) then
             print *,'coretransp with non-zero axis: extending'
             CALL EXTEND_TRANSP(coret(1), coret_ext(1), corep_old(1)%rho_tor(1), corep_old(1)%rho_tor(nrho))
          endif

          if (limit_evo) then 
             print *,'limit evolution of transport coefs'
             if (associated(coret_old)) then
                CALL LIMIT_TRANSP(coret_old(1),coret_ext(1))
             else
                allocate(coret_old(1))
             endif
             call copy_cpo(coret_ext(1),coret_old(1))
          endif
#ifdef SAVECORET
          call open_write_file(31,"ets_coretransp-ext_in_"//cptstr//".cpo")
          call write_cpo(coret_ext(1),'coretransp')
          call close_write_file
#endif
          CALL INTERPOLATE_TRANSP(NRHO, NION, corep_old(1)%rho_tor, &
               coret_ext(1), coret_work(1))

       else
          print *,'coretransp on same rho_tor size'

          if (coret(1)%values(1)%rho_tor_norm(1) .gt. 0.) then
             print *,'coretransp with non-zero axis: setting it to 0'
             coret(1)%values(1)%rho_tor_norm(1) = 0.
          endif

          tmp_d_bool = d_limit
          tmp_v_bool = v_limit
          d_limit = .false.
          v_limit = .false.
          call copy_cpo(coret(1),coret_work(1))
          call INTERPOLATE_TRANSP(NRHO, NION, corep_old(1)%rho_tor, &
               coret(1), coret_work(1))
          d_limit = tmp_d_bool
          v_limit = tmp_v_bool
          !!!TO BE UPDATE!!! v_limit = .true.
       endif
    endif

!!$    !same for sources if some are externals
!!$    !allocate_coresource_cpo
!!$    !interpolate_source
!!$    NRHO                          = SIZE(COREPROF(1)%rho_tor)
!!$    NION                          = SIZE(COREPROF(1)%ni%value, DIM=2)
!!$    NIMP                          = SIZE(CORESOURCE(1)%sz%imp, DIM=2)
!!$    MAX_NZIMP                     = SIZE(CORESOURCE(1)%sz%imp, DIM=3)
!!$    call deallocate_cpo(coresource)
!!$    CALL ALLOCATE_CORESOURCE_CPO    (NOCUR, NRHO, NION, NIMP, MAX_NZIMP,  CORESOURCE)
!!$    CORESOURCE(1)%rho_tor         = COREPROF(1)%rho_tor     
!!$    CALL GET_SOURCE_STATUS          (TIME,  INTERPOL, CORESOURCE,       CORESOURCE_WORK)
!!$    call interpolate_source(NRHO, NION, NIMP, MAX_NZIMP, COREPROF(1)%rho_tor, &
!!$         coresource_work(1), coresource_arr(1))
!!$    call deallocate_cpo(CORESOURCE_WORK)
!!$

#ifdef SIGMA_SPITZER
    print *,'call SPITZER_RESISTIVITY'
    call SPITZER_RESISTIVITY(corep_old,coret_sigma)
    if (.not.associated(coret_work(1)%values(1)%sigma)) then
       allocate(coret_work(1)%values(1)%sigma(size(coret_sigma(1)%values(1)%sigma)))
    end if
    coret_work(1)%values(1)%sigma = coret_sigma(1)%values(1)%sigma
#endif

#ifdef SAVECORET
    call open_write_file(30,"ets_coretransp-work_in_"//cptstr//".cpo")
    call write_cpo(coret_work(1),'coretransp')
    call close_write_file
#endif


    print *,"delTe_limit = ",delTe_limit*100,"%"
    print *,"deldTe_limit = ",deldTe_limit*100,"%"

    rho_tor_max = corep_old(1)%rho_tor(nrho)

    allocate(corep_ref(1))                                   !!! corep before Te test
    call copy_cpo(corep_old(1),corep_ref(1))
    
    control_double_test = control_double                     !!! control_double w/ dtime for time step size

    allocate(corep_old_test(1))                              !!! corep_old in the Te test
    allocate(corep_iter_test(1))                             !!! corep_iter in the Te test

    call copy_cpo(corep_old(1),corep_old_test(1))
    call copy_cpo(corep_old(1),corep_iter_test(1))


    ii              = 1
    inner_steps_cur = inner_steps_init
    dtime           = tau/REAL(inner_steps_cur)              !!! delta t value
    control_double_test(1) = dtime
    allocate(Te_frac(nrho))
    allocate(dTe_frac(nrho))

    do while (ii .le. inner_steps_cur)

       call ITM_ETS(corep_old_test, corep_iter_test, corep_new_test, &
            equil_old, equil_iter, coret_work,                       &
            cores, corei,                                            &
            control_integer, control_double_test,                    &
            code_parameters)

       exceeds_limit = .false.
       Te_frac = abs( (corep_ref(1)%te%value - corep_new_test(1)%te%value) / corep_ref(1)%te%value )
       Te_dev = MAXVAL(Te_frac)

       dTe_frac = abs( (corep_ref(1)%te%ddrho - corep_new_test(1)%te%ddrho) / (abs(corep_ref(1)%te%ddrho) + corep_ref(1)%te%value / rho_tor_max) )
       dTe_dev = MAXVAL(dTe_frac)

       if (Te_dev .ge. delTe_limit) then 
          write(*,"('At inner step ',I2,': Te deviates by ',F6.2,'%')") ii, Te_dev*100
          exceeds_limit = .true.
       end if
       if (dTe_dev .ge. deldTe_limit) then 
          write(*,"('At inner step ',I2,': dTe/drho deviates by ',F6.2,'%')") ii, dTe_dev*100
          exceeds_limit = .true.
       end if

       if (exceeds_limit) then
          if (ii.eq.1) then
             ! increase number of inner steps and try again (with smaller dtime)
             inner_steps_cur = inner_steps_cur * inner_steps_incr_factor
             if (inner_steps_cur > inner_steps_limit) then
                print *,"!!!!!Exceeded inner stepping limit, stopping!!!!!"
                write(*,"('inner_steps_cur=',I11,' and inner_steps_limit=',I11)") inner_steps_cur,inner_steps_limit
                write(*,"('Inner dt=',F15.12,' max deviation Te=',F6.2,'% and dTe=',F6.2,'%')") tau/REAL(inner_steps_cur),Te_dev*100,dTe_dev*100
                write(*,"('Te_frac = ',100(F6.2))") Te_frac
                write(*,"('dTe_frac = ',100(F6.2))") dTe_frac
#ifdef DEBUG
                call open_write_file(40,"ets_coreprof-inner_"//cptstr//".cpo")
                call write_cpo(corep_new_test(1),'coreprof')
                call close_write_file
#endif
                STOP
             endif
             dtime = tau/REAL(inner_steps_cur)
             control_double_test(1) = dtime
          else
             ! stop the inner stepping
             write(*,"('Stops advancing before inner step ',I2,' for total time = ',F9.6,': max deviation for Te = ',F6.2,'% and dTe/drho = ',F6.2,'%')") ii, ii*dtime, Te_dev*100, dTe_dev*100
             EXIT
          end if
       else
          ii = ii + 1
          call copy_cpo(corep_new_test(1),corep_old_test(1))    !!! replace corep_old with corep_new in Te test
          call copy_cpo(corep_new_test(1),corep_iter_test(1))
       endif
       
    end do

    deallocate(Te_frac)
    deallocate(dTe_frac)

    ! in all cases we want corep_old_test
    call copy_cpo(corep_old_test,corep_new)
    call equil_input(corep_new,toroidf,equil_old,equil_iter)

#ifdef PERF
    call c_getMillis(t1)
    texec = t1 - t0
#endif   

    write(*,"('Loop#',I4.4,': advanced with inner steps to a total step of ',F9.6,' (targeted tau=',F9.6,')')") init_step+cpt,(ii-1)*dtime,tau
    equil_iter(1)%time = time_in + (ii-1)*dtime
    corep_new(1)%time = time_in + (ii-1)*dtime

    write(cptstr,'(I4.4)') init_step+cpt
    cpt = cpt+1

    corep_out_file = 'ets_coreprof_'//cptstr//'.cpo'
    equil_out_file = 'ets_equilibrium_'//cptstr//'.cpo'

#ifdef PERF
    call c_getMillis(t0)
#endif
    !...  write the results
    call open_write_file(20,corep_out_file)
    call write_cpo(corep_new(1),'coreprof')
    call close_write_file
    call open_write_file(21,equil_out_file)
    call write_cpo(equil_iter(1),'equilibrium')
    call close_write_file

    ! transfer CPO file to buffer
    call file2byte(corep_out_file, tmpbuf, tmpsize)
    allocate(corep_out(tmpsize))
    corep_out(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)

    call file2byte(equil_out_file, tmpbuf, tmpsize)
    allocate(equil_out(tmpsize))
    equil_out(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)

#ifdef PERF
    call c_getMillis(t1)
    twrite = t1 - t0
#endif

    call deallocate_cpo(corep_old)
    call deallocate_cpo(corep_iter)
    call deallocate_cpo(corep_new)
    call deallocate_cpo(equil_old)
    call deallocate_cpo(equil_iter)
    call deallocate_cpo(coret)
    call deallocate_cpo(cores)
    call deallocate_cpo(corei)
    call deallocate_cpo(toroidf)

    call deallocate_cpo(coret_sigma)

    CALL deallocate_cpo(coret_work)
    CALL deallocate_cpo(coret_ext)
    CALL deallocate_cpo(cores_work)
    if (limit_evo) then
       CALL deallocate_cpo(coret_old)
    endif

    call deallocate_cpo(corep_old_test) 
    call deallocate_cpo(corep_iter_test)
    call deallocate_cpo(corep_new_test)
    call deallocate_cpo(corep_ref)

    if (associated(code_parameters%schema)) then
       deallocate(code_parameters%schema)
    endif
    if (associated(code_parameters%parameters)) then
       deallocate(code_parameters%parameters)
    endif
    if (associated(code_parameters%default_param)) then
       deallocate(code_parameters%default_param)
    endif

    print *,"return from fortran wrapper"

11  end subroutine ets2buf




  subroutine interpolate_transp(nrho_out, nion, rho_tor_out, coretransp_in, coretransp_out)
    use euitm_schemas
    use itm_types

    implicit none

    integer nrho_out, nion
    real (R8) :: rho_tor_out(:)
    TYPE (TYPE_CORETRANSP), intent(in) :: CORETRANSP_in
    TYPE (TYPE_CORETRANSP), intent(inout) :: CORETRANSP_out

    integer nrho_in

    integer iion, icon

    print *,'interpolate_transp'

    nrho_in=size(CORETRANSP_in%values(1)%rho_tor)


    deallocate(CORETRANSP_out%values(1)%rho_tor)
    allocate(CORETRANSP_out%values(1)%rho_tor(nrho_out))
    CORETRANSP_out%values(1)%rho_tor=rho_tor_out

! sigma
    if(associated(CORETRANSP_in%values(1)%sigma)) then
       deallocate(CORETRANSP_out%values(1)%sigma)
       allocate(CORETRANSP_out%values(1)%sigma(nrho_out))
       call Linterp(CORETRANSP_in%values(1)%sigma, CORETRANSP_in%values(1)%rho_tor, nrho_in,  &
            CORETRANSP_out%values(1)%sigma, CORETRANSP_out%values(1)%rho_tor, nrho_out)
    endif

! ni
    if(associated(CORETRANSP_in%values(1)%ni_transp%diff_eff)) then
       deallocate(CORETRANSP_out%values(1)%ni_transp%diff_eff)
       allocate(CORETRANSP_out%values(1)%ni_transp%diff_eff(nrho_out,nion,3))
       do iion=1,nion
          do icon=1,3
             call Linterp(CORETRANSP_in%values(1)%ni_transp%diff_eff(:,iion,icon), CORETRANSP_in%values(1)%rho_tor, nrho_in,  &
                  CORETRANSP_out%values(1)%ni_transp%diff_eff(:,iion,icon), CORETRANSP_out%values(1)%rho_tor, nrho_out)
          enddo
          if (d_limit) then
             icon = 2
             CORETRANSP_out%values(1)%ni_transp%diff_eff(:,iion,icon) = MAX(CORETRANSP_out%values(1)%ni_transp%diff_eff(:,iion,icon),d_prof)!,d_ceil)
          endif
       enddo
    endif
    if(associated(CORETRANSP_in%values(1)%ni_transp%vconv_eff)) then
       deallocate(CORETRANSP_out%values(1)%ni_transp%vconv_eff)
       allocate(CORETRANSP_out%values(1)%ni_transp%vconv_eff(nrho_out,nion,3))
       do iion=1,nion
          do icon=1,3
             call Linterp(CORETRANSP_in%values(1)%ni_transp%vconv_eff(:,iion,icon), CORETRANSP_in%values(1)%rho_tor, nrho_in,  &
                  CORETRANSP_out%values(1)%ni_transp%vconv_eff(:,iion,icon), CORETRANSP_out%values(1)%rho_tor, nrho_out)
          enddo
!!$          if (v_limit) then
!!$             icon = 2
!!$             CORETRANSP_out%values(1)%ni_transp%vconv_eff(:,iion,icon) = MAX(CORETRANSP_out%values(1)%ni_transp%vconv_eff(:,iion,icon),v_prof)!,v_ceil)
!!$          endif
       enddo
    endif
       
! ne
    if(associated(CORETRANSP_in%values(1)%ne_transp%diff_eff)) then
       deallocate(CORETRANSP_out%values(1)%ne_transp%diff_eff)
       allocate(CORETRANSP_out%values(1)%ne_transp%diff_eff(nrho_out,3))
       do icon=1,3
          call Linterp(CORETRANSP_in%values(1)%ne_transp%diff_eff(:,icon), CORETRANSP_in%values(1)%rho_tor, nrho_in,  &
               CORETRANSP_out%values(1)%ne_transp%diff_eff(:,icon), CORETRANSP_out%values(1)%rho_tor, nrho_out)
       enddo
       if (d_limit) then
          icon = 2
          CORETRANSP_out%values(1)%ne_transp%diff_eff(:,icon) = MAX(CORETRANSP_out%values(1)%ne_transp%diff_eff(:,icon),d_prof)!,d_ceil)
       endif
    endif
    if(associated(CORETRANSP_in%values(1)%ne_transp%vconv_eff)) then
       deallocate(CORETRANSP_out%values(1)%ne_transp%vconv_eff)
       allocate(CORETRANSP_out%values(1)%ne_transp%vconv_eff(nrho_out,3))
       do icon=1,3
          call Linterp(CORETRANSP_in%values(1)%ne_transp%vconv_eff(:,icon), CORETRANSP_in%values(1)%rho_tor, nrho_in,  &
               CORETRANSP_out%values(1)%ne_transp%vconv_eff(:,icon), CORETRANSP_out%values(1)%rho_tor, nrho_out)
       enddo
!!$       if (v_limit) then
!!$          icon = 2
!!$          CORETRANSP_out%values(1)%ne_transp%vconv_eff(:,icon) = MAX(CORETRANSP_out%values(1)%ne_transp%vconv_eff(:,icon),v_prof)!,v_ceil)
!!$       endif
    endif
       
! Ti
    if(associated(CORETRANSP_in%values(1)%ti_transp%diff_eff)) then
       deallocate(CORETRANSP_out%values(1)%ti_transp%diff_eff)
       allocate(CORETRANSP_out%values(1)%ti_transp%diff_eff(nrho_out,nion))
       do iion=1,nion
          call Linterp(CORETRANSP_in%values(1)%ti_transp%diff_eff(:,iion), CORETRANSP_in%values(1)%rho_tor, nrho_in,  &
               CORETRANSP_out%values(1)%ti_transp%diff_eff(:,iion), CORETRANSP_out%values(1)%rho_tor, nrho_out)
          if (d_limit) then
             CORETRANSP_out%values(1)%ti_transp%diff_eff(:,iion) = MAX(CORETRANSP_out%values(1)%ti_transp%diff_eff(:,iion), d_prof)!,d_ceil)
          endif
       enddo
    endif
    if(associated(CORETRANSP_in%values(1)%Ti_transp%vconv_eff)) then
       deallocate(CORETRANSP_out%values(1)%Ti_transp%vconv_eff)
       allocate(CORETRANSP_out%values(1)%Ti_transp%vconv_eff(nrho_out,nion))
       do iion=1,nion
          call Linterp(CORETRANSP_in%values(1)%Ti_transp%vconv_eff(:,iion), CORETRANSP_in%values(1)%rho_tor, nrho_in,  &
               CORETRANSP_out%values(1)%Ti_transp%vconv_eff(:,iion), CORETRANSP_out%values(1)%rho_tor, nrho_out)
!!$          if (v_limit) then
!!$             CORETRANSP_out%values(1)%Ti_transp%vconv_eff(:,iion) = MAX(CORETRANSP_out%values(1)%Ti_transp%vconv_eff(:,iion),v_prof)!,v_ceil)
!!$          endif
       enddo
    endif

! Te    
    if(associated(CORETRANSP_in%values(1)%te_transp%diff_eff)) then
       deallocate(CORETRANSP_out%values(1)%te_transp%diff_eff)
       allocate(CORETRANSP_out%values(1)%te_transp%diff_eff(nrho_out))
       call Linterp(CORETRANSP_in%values(1)%te_transp%diff_eff, CORETRANSP_in%values(1)%rho_tor, nrho_in,  &
            CORETRANSP_out%values(1)%te_transp%diff_eff, CORETRANSP_out%values(1)%rho_tor, nrho_out)
       if (d_limit) then
          CORETRANSP_out%values(1)%te_transp%diff_eff = MAX(CORETRANSP_out%values(1)%te_transp%diff_eff,d_prof)!,d_ceil)
       endif
    endif
    if(associated(CORETRANSP_in%values(1)%Te_transp%vconv_eff)) then
       deallocate(CORETRANSP_out%values(1)%Te_transp%vconv_eff)
       allocate(CORETRANSP_out%values(1)%Te_transp%vconv_eff(nrho_out))
       call Linterp(CORETRANSP_in%values(1)%Te_transp%vconv_eff, CORETRANSP_in%values(1)%rho_tor, nrho_in,  &
            CORETRANSP_out%values(1)%Te_transp%vconv_eff, CORETRANSP_out%values(1)%rho_tor, nrho_out)
!!$       if (v_limit) then
!!$          CORETRANSP_out%values(1)%Te_transp%vconv_eff = MAX(CORETRANSP_out%values(1)%Te_transp%vconv_eff,v_prof)!,v_ceil)
!!$       endif
    endif

! vtor
    if(associated(CORETRANSP_in%values(1)%vtor_transp%diff_eff)) then
       deallocate(CORETRANSP_out%values(1)%vtor_transp%diff_eff)
       allocate(CORETRANSP_out%values(1)%vtor_transp%diff_eff(nrho_out,nion))
       do iion=1,nion
          call Linterp(CORETRANSP_in%values(1)%vtor_transp%diff_eff(:,iion), CORETRANSP_in%values(1)%rho_tor, nrho_in,  &
               CORETRANSP_out%values(1)%vtor_transp%diff_eff(:,iion), CORETRANSP_out%values(1)%rho_tor, nrho_out)
          if (d_limit) then
             CORETRANSP_out%values(1)%vtor_transp%diff_eff(:,iion) = MAX(CORETRANSP_out%values(1)%vtor_transp%diff_eff(:,iion),d_prof)!,d_ceil)
          endif
       enddo
    endif
    if(associated(CORETRANSP_in%values(1)%vtor_transp%vconv_eff)) then
       deallocate(CORETRANSP_out%values(1)%vtor_transp%vconv_eff)
       allocate(CORETRANSP_out%values(1)%vtor_transp%vconv_eff(nrho_out,nion))
       do iion=1,nion
          call Linterp(CORETRANSP_in%values(1)%vtor_transp%vconv_eff(:,iion), CORETRANSP_in%values(1)%rho_tor, nrho_in,  &
               CORETRANSP_out%values(1)%vtor_transp%vconv_eff(:,iion), CORETRANSP_out%values(1)%rho_tor, nrho_out)
!!$          if (v_limit) then 
!!$             CORETRANSP_out%values(1)%vtor_transp%vconv_eff(:,iion) = MAX(CORETRANSP_out%values(1)%vtor_transp%vconv_eff(:,iion),v_prof)!,v_ceil)
!!$          endif
       enddo
    endif

    return

  end subroutine interpolate_transp


  subroutine extend_transp(coret_in, coret_out, rho_1, rho_n) !, d_floor)
    use euitm_schemas
    use itm_types
    use copy_structures

    implicit none

    TYPE (TYPE_CORETRANSP) :: coret_in
    TYPE (TYPE_CORETRANSP) :: coret_out
    integer :: nrho, nion, icon, iion, ir
    real (R8) :: rho_1, rho_n

    nrho = size(coret_in%values(1)%rho_tor)
    nion = size(coret_in%values(1)%ni_transp%diff_eff, DIM=2)

    call copy_cpo(coret_in,coret_out)

    deallocate(coret_out%values(1)%rho_tor)
    allocate(coret_out%values(1)%rho_tor(nrho+2))
    coret_out%values(1)%rho_tor(2:nrho+1) = coret_in%values(1)%rho_tor(:)
    coret_out%values(1)%rho_tor(1) = rho_1 !coret_in%values(1)%rho_tor(1)
    coret_out%values(1)%rho_tor(nrho+2) = rho_n !coret_in%values(1)%rho_tor(nrho)

! sigma
    if(associated(coret_in%values(1)%sigma)) then
       deallocate(coret_out%values(1)%sigma)
       allocate(coret_out%values(1)%sigma(nrho+2))
       coret_out%values(1)%sigma(2:nrho+1) = coret_in%values(1)%sigma(:)
       coret_out%values(1)%sigma(1) = coret_in%values(1)%sigma(1)
       coret_out%values(1)%sigma(nrho+2) = coret_in%values(1)%sigma(nrho)
    endif

! ni
    if(associated(coret_in%values(1)%ni_transp%diff_eff)) then
       deallocate(coret_out%values(1)%ni_transp%diff_eff)
       allocate(coret_out%values(1)%ni_transp%diff_eff(nrho+2,nion,3))
       do icon=1,3
          do iion=1,nion
             coret_out%values(1)%ni_transp%diff_eff(2:nrho+1,iion,icon) = &
                  coret_in%values(1)%ni_transp%diff_eff(:,iion,icon)
             coret_out%values(1)%ni_transp%diff_eff(1,iion,icon) = &
                  coret_in%values(1)%ni_transp%diff_eff(1,iion,icon)
             coret_out%values(1)%ni_transp%diff_eff(nrho+2,iion,icon) = &
                  coret_in%values(1)%ni_transp%diff_eff(nrho,iion,icon)
          enddo
       enddo
       icon = 2 ! <== floor limit only for Turb transp coefficients
       if (.not. c_d_cst) then
          coret_out%values(1)%ni_transp%diff_eff(1,:,icon) = d_floor
       endif
       if (.not. e_d_cst) then
          coret_out%values(1)%ni_transp%diff_eff(nrho+2,:,icon) = d_floor
       endif
       if (d_limit) then
          coret_out%values(1)%ni_transp%diff_eff(:,:,icon) = MAX(coret_out%values(1)%ni_transp%diff_eff(:,:,icon),d_floor)
          coret_out%values(1)%ni_transp%diff_eff(:,:,icon) = MIN(coret_out%values(1)%ni_transp%diff_eff(:,:,icon),d_ceil)
       endif
    endif
    if(associated(coret_in%values(1)%ni_transp%vconv_eff)) then
       deallocate(coret_out%values(1)%ni_transp%vconv_eff)
       allocate(coret_out%values(1)%ni_transp%vconv_eff(nrho+2,nion,3))
       do icon=1,3
          do iion=1,nion
             coret_out%values(1)%ni_transp%vconv_eff(2:nrho+1,iion,icon) = &
                  coret_in%values(1)%ni_transp%vconv_eff(:,iion,icon)
             coret_out%values(1)%ni_transp%vconv_eff(1,iion,icon) = &
                  coret_in%values(1)%ni_transp%vconv_eff(1,iion,icon)
             coret_out%values(1)%ni_transp%vconv_eff(nrho+2,iion,icon) = &
                  coret_in%values(1)%ni_transp%vconv_eff(nrho,iion,icon)
          enddo
       enddo
       icon = 2 ! <== floor limit only for Turb transp coefficients
       if (.not. c_v_cst) then 
          coret_out%values(1)%ni_transp%vconv_eff(1,:,icon) = v_floor
       endif
       if (.not. e_v_cst) then 
          coret_out%values(1)%ni_transp%vconv_eff(nrho+2,:,icon) = v_floor
       endif
       if (v_limit) then
          coret_out%values(1)%ni_transp%vconv_eff(:,:,icon) = MAX(coret_out%values(1)%ni_transp%vconv_eff(:,:,icon),v_floor)
          coret_out%values(1)%ni_transp%vconv_eff(:,:,icon) = MIN(coret_out%values(1)%ni_transp%vconv_eff(:,:,icon),v_ceil)
       endif
    endif

! ne
    if(associated(coret_in%values(1)%ne_transp%diff_eff)) then
       deallocate(coret_out%values(1)%ne_transp%diff_eff)
       allocate(coret_out%values(1)%ne_transp%diff_eff(nrho+2,3))
       do icon=1,3
          coret_out%values(1)%ne_transp%diff_eff(2:nrho+1,icon) = &
               coret_in%values(1)%ne_transp%diff_eff(:,icon)
          coret_out%values(1)%ne_transp%diff_eff(1,icon) = &
               coret_in%values(1)%ne_transp%diff_eff(1,icon)
          coret_out%values(1)%ne_transp%diff_eff(nrho+2,icon) = &
               coret_in%values(1)%ne_transp%diff_eff(nrho,icon)
       enddo
       icon = 2 ! <== floor limit only for Turb transp coefficients
       if (.not. c_d_cst) then 
          coret_out%values(1)%ne_transp%diff_eff(1,icon) = d_floor
       endif
       if (.not. e_d_cst) then 
          coret_out%values(1)%ne_transp%diff_eff(nrho+2,icon) = d_floor
       endif
       if (d_limit) then 
          coret_out%values(1)%ne_transp%diff_eff(:,icon) = MAX(coret_out%values(1)%ne_transp%diff_eff(:,icon),d_floor)
          coret_out%values(1)%ne_transp%diff_eff(:,icon) = MIN(coret_out%values(1)%ne_transp%diff_eff(:,icon),d_ceil)
       endif
    endif
    if(associated(coret_in%values(1)%ne_transp%vconv_eff)) then
       deallocate(coret_out%values(1)%ne_transp%vconv_eff)
       allocate(coret_out%values(1)%ne_transp%vconv_eff(nrho+2,3))
       do icon=1,3
          coret_out%values(1)%ne_transp%vconv_eff(2:nrho+1,icon) = &
               coret_in%values(1)%ne_transp%vconv_eff(:,icon)
          coret_out%values(1)%ne_transp%vconv_eff(1,icon) = &
               coret_in%values(1)%ne_transp%vconv_eff(1,icon)
          coret_out%values(1)%ne_transp%vconv_eff(nrho+2,icon) = &
               coret_in%values(1)%ne_transp%vconv_eff(nrho,icon)
       enddo
       icon = 2 ! <== floor limit only for Turb transp coefficients       
       if (.not. c_v_cst) then
          coret_out%values(1)%ne_transp%vconv_eff(1,icon) = v_floor
       endif
       if (.not. e_v_cst) then
          coret_out%values(1)%ne_transp%vconv_eff(nrho+2,icon) = v_floor
       endif
       if (v_limit) then 
          coret_out%values(1)%ne_transp%vconv_eff(:,icon) = MAX(coret_out%values(1)%ne_transp%vconv_eff(:,icon),v_floor)
          coret_out%values(1)%ne_transp%vconv_eff(:,icon) = MIN(coret_out%values(1)%ne_transp%vconv_eff(:,icon),v_ceil)
       endif
    endif
   
! Ti
    if(associated(coret_in%values(1)%ti_transp%diff_eff)) then
       deallocate(coret_out%values(1)%ti_transp%diff_eff)
       allocate(coret_out%values(1)%ti_transp%diff_eff(nrho+2,nion))
       do iion=1,nion
          coret_out%values(1)%ti_transp%diff_eff(2:nrho+1,iion) = &
               coret_in%values(1)%ti_transp%diff_eff(:,iion)
          coret_out%values(1)%ti_transp%diff_eff(1,iion) = &
               coret_in%values(1)%ti_transp%diff_eff(1,iion)
          coret_out%values(1)%ti_transp%diff_eff(nrho+2,iion) = &
               coret_in%values(1)%ti_transp%diff_eff(nrho,iion)
       enddo
       if (.not. c_d_cst) then
          coret_out%values(1)%ti_transp%diff_eff(1,:) = d_floor
       endif
       if (.not. e_d_cst) then
          coret_out%values(1)%ti_transp%diff_eff(nrho+2,:) = d_floor
       endif
       if (d_limit) then 
          coret_out%values(1)%ti_transp%diff_eff(:,:) = MAX(coret_out%values(1)%ti_transp%diff_eff(:,:),d_floor)
          coret_out%values(1)%ti_transp%diff_eff(:,:) = MIN(coret_out%values(1)%ti_transp%diff_eff(:,:),d_ceil)
       endif
    endif
    if(associated(coret_in%values(1)%ti_transp%vconv_eff)) then
       deallocate(coret_out%values(1)%ti_transp%vconv_eff)
       allocate(coret_out%values(1)%ti_transp%vconv_eff(nrho+2,nion))
       do iion=1,nion
          coret_out%values(1)%ti_transp%vconv_eff(2:nrho+1,iion) = &
               coret_in%values(1)%ti_transp%vconv_eff(:,iion)
          coret_out%values(1)%ti_transp%vconv_eff(1,iion) = &
               coret_in%values(1)%ti_transp%vconv_eff(1,iion)
          coret_out%values(1)%ti_transp%vconv_eff(nrho+2,iion) = &
               coret_in%values(1)%ti_transp%vconv_eff(nrho,iion)
       enddo
       if (.not. c_v_cst) then
          coret_out%values(1)%ti_transp%vconv_eff(1,:) = v_floor
       endif
       if (.not. e_v_cst) then
          coret_out%values(1)%ti_transp%vconv_eff(nrho+2,:) = v_floor
       endif
       if (v_limit) then
          coret_out%values(1)%ti_transp%vconv_eff(:,:) = MAX(coret_out%values(1)%ti_transp%vconv_eff(:,:),v_floor)
          coret_out%values(1)%ti_transp%vconv_eff(:,:) = MIN(coret_out%values(1)%ti_transp%vconv_eff(:,:),v_ceil)
       endif
    endif

! Te    
    if(associated(coret_in%values(1)%te_transp%diff_eff)) then
       deallocate(coret_out%values(1)%te_transp%diff_eff)
       allocate(coret_out%values(1)%te_transp%diff_eff(nrho+2))
       coret_out%values(1)%te_transp%diff_eff(2:nrho+1) = &
            coret_in%values(1)%te_transp%diff_eff(:)
       coret_out%values(1)%te_transp%diff_eff(1) = &
            coret_in%values(1)%te_transp%diff_eff(1)
       coret_out%values(1)%te_transp%diff_eff(nrho+2) = &
            coret_in%values(1)%te_transp%diff_eff(nrho)
       if (.not. c_d_cst) then
          coret_out%values(1)%te_transp%diff_eff(1) = d_floor
       endif
       if (.not. e_d_cst) then
          coret_out%values(1)%te_transp%diff_eff(nrho+2) = d_floor
       endif
       if (d_limit) then 
          coret_out%values(1)%te_transp%diff_eff(:) = MAX(coret_out%values(1)%te_transp%diff_eff(:),d_floor)
          coret_out%values(1)%te_transp%diff_eff(:) = MIN(coret_out%values(1)%te_transp%diff_eff(:),d_ceil)
       endif
    endif
    if(associated(coret_in%values(1)%te_transp%vconv_eff)) then
       deallocate(coret_out%values(1)%te_transp%vconv_eff)
       allocate(coret_out%values(1)%te_transp%vconv_eff(nrho+2))
       coret_out%values(1)%te_transp%vconv_eff(2:nrho+1) = &
            coret_in%values(1)%te_transp%vconv_eff(:)
       coret_out%values(1)%te_transp%vconv_eff(1) = &
            coret_in%values(1)%te_transp%vconv_eff(1)
       coret_out%values(1)%te_transp%vconv_eff(nrho+2) = &
            coret_in%values(1)%te_transp%vconv_eff(nrho)
       if (.not. c_v_cst) then
          coret_out%values(1)%te_transp%vconv_eff(1) = v_floor
       endif
       if (.not. e_v_cst) then
          coret_out%values(1)%te_transp%vconv_eff(nrho+2) = v_floor
       endif
       if (v_limit) then
          coret_out%values(1)%te_transp%vconv_eff(:) = MAX(coret_out%values(1)%te_transp%vconv_eff(:),v_floor)
          coret_out%values(1)%te_transp%vconv_eff(:) = MIN(coret_out%values(1)%te_transp%vconv_eff(:),v_ceil)
       endif
    endif

! vtor
    if(associated(coret_in%values(1)%vtor_transp%diff_eff)) then
       deallocate(coret_out%values(1)%vtor_transp%diff_eff)
       allocate(coret_out%values(1)%vtor_transp%diff_eff(nrho+2,nion))
       do iion=1,nion
          coret_out%values(1)%vtor_transp%diff_eff(2:nrho+1,nion) = &
               coret_in%values(1)%vtor_transp%diff_eff(:,nion)
          coret_out%values(1)%vtor_transp%diff_eff(1,nion) = &
               coret_in%values(1)%vtor_transp%diff_eff(1,nion)
          coret_out%values(1)%vtor_transp%diff_eff(nrho+2,nion) = &
               coret_in%values(1)%vtor_transp%diff_eff(nrho,nion)
       enddo
       if (.not. c_d_cst) then
          coret_out%values(1)%vtor_transp%diff_eff(1,:) = d_floor
       endif
       if (.not. e_d_cst) then
          coret_out%values(1)%vtor_transp%diff_eff(nrho+2,:) = d_floor          
       endif
       if (d_limit) then
          coret_out%values(1)%vtor_transp%diff_eff(:,:) = MAX(coret_out%values(1)%vtor_transp%diff_eff(:,:),d_floor)
          coret_out%values(1)%vtor_transp%diff_eff(:,:) = MIN(coret_out%values(1)%vtor_transp%diff_eff(:,:),d_ceil)
       endif
    endif
    if(associated(coret_in%values(1)%vtor_transp%vconv_eff)) then
       deallocate(coret_out%values(1)%vtor_transp%vconv_eff)
       allocate(coret_out%values(1)%vtor_transp%vconv_eff(nrho+2,nion))
       do iion=1,nion
          coret_out%values(1)%vtor_transp%vconv_eff(2:nrho+1,nion) = &
               coret_in%values(1)%vtor_transp%vconv_eff(:,nion)
          coret_out%values(1)%vtor_transp%vconv_eff(1,nion) = &
               coret_in%values(1)%vtor_transp%vconv_eff(1,nion)
          coret_out%values(1)%vtor_transp%vconv_eff(nrho+2,nion) = &
               coret_in%values(1)%vtor_transp%vconv_eff(nrho,nion)
       enddo
       if (.not. c_v_cst) then
          coret_out%values(1)%vtor_transp%vconv_eff(1,:) = v_floor
       endif
       if (.not. e_v_cst) then
          coret_out%values(1)%vtor_transp%vconv_eff(nrho+2,:) = v_floor
       endif
       if (v_limit) then 
          coret_out%values(1)%vtor_transp%vconv_eff(:,:) = MAX(coret_out%values(1)%vtor_transp%vconv_eff(:,:),v_floor)
          coret_out%values(1)%vtor_transp%vconv_eff(:,:) = MIN(coret_out%values(1)%vtor_transp%vconv_eff(:,:),v_ceil)
       endif
    endif

    return

  end subroutine extend_transp


  subroutine LIMIT_TRANSP(coret_old,coret_ext)
    use euitm_schemas
    use itm_types
    implicit none

    TYPE (TYPE_CORETRANSP) :: coret_old
    TYPE (TYPE_CORETRANSP) :: coret_ext
    real(R8) :: evo
    Integer :: irho, nrho, iion, nion, icon

    print *,'limit_transp'
    
    nrho = size(coret_old%values(1)%rho_tor)
    nion = size(coret_old%values(1)%ni_transp%diff_eff, DIM=2)
    icon = 2

! ni
    if(associated(coret_old%values(1)%ni_transp%diff_eff)) then
       do iion=1,nion
          do irho=1,nrho
             evo = coret_ext%values(1)%ni_transp%diff_eff(irho,iion,icon) - &
                  coret_old%values(1)%ni_transp%diff_eff(irho,iion,icon)
             if (ABS(evo) > d_max_evo) then
                if (evo > 0.) then
                   coret_ext%values(1)%ni_transp%diff_eff(irho,iion,icon) = &
                        coret_old%values(1)%ni_transp%diff_eff(irho,iion,icon) +d_max_evo
                else
                   coret_ext%values(1)%ni_transp%diff_eff(irho,iion,icon) = &
                        coret_old%values(1)%ni_transp%diff_eff(irho,iion,icon) -d_max_evo
                endif
             endif
          enddo
       enddo
    endif
    if(associated(coret_old%values(1)%ni_transp%vconv_eff)) then
       do iion=1,nion
          do irho=1,nrho
             evo = coret_ext%values(1)%ni_transp%vconv_eff(irho,iion,icon) - &
                  coret_old%values(1)%ni_transp%vconv_eff(irho,iion,icon)
             if (ABS(evo) > d_max_evo) then
                if (evo > 0.) then
                   coret_ext%values(1)%ni_transp%vconv_eff(irho,iion,icon) = &
                        coret_old%values(1)%ni_transp%vconv_eff(irho,iion,icon) +d_max_evo
                else
                   coret_ext%values(1)%ni_transp%vconv_eff(irho,iion,icon) = &
                        coret_old%values(1)%ni_transp%vconv_eff(irho,iion,icon) -d_max_evo
                endif
             endif
          enddo
       enddo
    endif

! ne
    if(associated(coret_old%values(1)%ne_transp%diff_eff)) then
       do irho=1,nrho
          evo = coret_ext%values(1)%ne_transp%diff_eff(irho,icon) - &
               coret_old%values(1)%ne_transp%diff_eff(irho,icon)
          if (ABS(evo) > d_max_evo) then
             if (evo > 0.) then
                coret_ext%values(1)%ne_transp%diff_eff(irho,icon) = &
                     coret_old%values(1)%ne_transp%diff_eff(irho,icon) +d_max_evo
             else
                coret_ext%values(1)%ne_transp%diff_eff(irho,icon) = &
                     coret_old%values(1)%ne_transp%diff_eff(irho,icon) -d_max_evo
             endif
          endif
       enddo
    endif
    if(associated(coret_old%values(1)%ne_transp%vconv_eff)) then
       do irho=1,nrho
          evo = coret_ext%values(1)%ne_transp%vconv_eff(irho,icon) - &
               coret_old%values(1)%ne_transp%vconv_eff(irho,icon)
          if (ABS(evo) > d_max_evo) then
             if (evo > 0.) then
                coret_ext%values(1)%ne_transp%vconv_eff(irho,icon) = &
                     coret_old%values(1)%ne_transp%vconv_eff(irho,icon) +d_max_evo
             else
                coret_ext%values(1)%ne_transp%vconv_eff(irho,icon) = &
                     coret_old%values(1)%ne_transp%vconv_eff(irho,icon) -d_max_evo
             endif
          endif
       enddo
    endif
   
! Ti
    if(associated(coret_old%values(1)%ti_transp%diff_eff)) then
       do iion=1,nion
          do irho=1,nrho
             evo = coret_ext%values(1)%ti_transp%diff_eff(irho,iion) - &
                  coret_old%values(1)%ti_transp%diff_eff(irho,iion)
             if (ABS(evo) > d_max_evo) then
                if (evo > 0.) then
                   coret_ext%values(1)%ti_transp%diff_eff(irho,iion) = &
                        coret_old%values(1)%ti_transp%diff_eff(irho,iion) +d_max_evo
                else
                   coret_ext%values(1)%ti_transp%diff_eff(irho,iion) = &
                        coret_old%values(1)%ti_transp%diff_eff(irho,iion) -d_max_evo
                endif
             endif
          enddo
       enddo
    endif
    if(associated(coret_old%values(1)%ti_transp%vconv_eff)) then
       do iion=1,nion
          do irho=1,nrho
             evo = coret_ext%values(1)%ti_transp%vconv_eff(irho,iion) - &
                  coret_old%values(1)%ti_transp%vconv_eff(irho,iion)
             if (ABS(evo) > d_max_evo) then
                if (evo > 0.) then
                   coret_ext%values(1)%ti_transp%vconv_eff(irho,iion) = &
                        coret_old%values(1)%ti_transp%vconv_eff(irho,iion) +d_max_evo
                else
                   coret_ext%values(1)%ti_transp%vconv_eff(irho,iion) = &
                        coret_old%values(1)%ti_transp%vconv_eff(irho,iion) -d_max_evo
                endif
             endif
          enddo
       enddo
    endif

! Te
    if(associated(coret_old%values(1)%te_transp%diff_eff)) then
       do irho=1,nrho
          evo = coret_ext%values(1)%te_transp%diff_eff(irho) - &
               coret_old%values(1)%te_transp%diff_eff(irho)
          if (ABS(evo) > d_max_evo) then
             if (evo > 0.) then
                coret_ext%values(1)%te_transp%diff_eff(irho) = &
                     coret_old%values(1)%te_transp%diff_eff(irho) +d_max_evo
             else
                coret_ext%values(1)%te_transp%diff_eff(irho) = &
                     coret_old%values(1)%te_transp%diff_eff(irho) -d_max_evo
             endif
          endif
       enddo
    endif
    if(associated(coret_old%values(1)%te_transp%vconv_eff)) then
       do irho=1,nrho
          evo = coret_ext%values(1)%te_transp%vconv_eff(irho) - &
               coret_old%values(1)%te_transp%vconv_eff(irho)
          if (ABS(evo) > d_max_evo) then
             if (evo > 0.) then
                coret_ext%values(1)%te_transp%vconv_eff(irho) = &
                     coret_old%values(1)%te_transp%vconv_eff(irho) +d_max_evo
             else
                coret_ext%values(1)%te_transp%vconv_eff(irho) = &
                     coret_old%values(1)%te_transp%vconv_eff(irho) -d_max_evo
             endif
          endif
       enddo
    endif

! vtor
    if(associated(coret_old%values(1)%vtor_transp%diff_eff)) then
       do iion=1,nion
          do irho=1,nrho
             evo = coret_ext%values(1)%vtor_transp%diff_eff(irho,iion) - &
                  coret_old%values(1)%vtor_transp%diff_eff(irho,iion)
             if (ABS(evo) > d_max_evo) then
                if (evo > 0.) then
                   coret_ext%values(1)%vtor_transp%diff_eff(irho,iion) = &
                        coret_old%values(1)%vtor_transp%diff_eff(irho,iion) +d_max_evo
                else
                   coret_ext%values(1)%vtor_transp%diff_eff(irho,iion) = &
                        coret_old%values(1)%vtor_transp%diff_eff(irho,iion) -d_max_evo
                endif
             endif
          enddo
       enddo
    endif
    if(associated(coret_old%values(1)%vtor_transp%vconv_eff)) then
       do iion=1,nion
          do irho=1,nrho
             evo = coret_ext%values(1)%vtor_transp%vconv_eff(irho,iion) - &
                  coret_old%values(1)%vtor_transp%vconv_eff(irho,iion)
             if (ABS(evo) > d_max_evo) then
                if (evo > 0.) then
                   coret_ext%values(1)%vtor_transp%vconv_eff(irho,iion) = &
                        coret_old%values(1)%vtor_transp%vconv_eff(irho,iion) +d_max_evo
                else
                   coret_ext%values(1)%vtor_transp%vconv_eff(irho,iion) = &
                        coret_old%values(1)%vtor_transp%vconv_eff(irho,iion) -d_max_evo
                endif
             endif
          enddo
       enddo
    endif

    return

  end subroutine LIMIT_TRANSP

end module ets_wrapper
