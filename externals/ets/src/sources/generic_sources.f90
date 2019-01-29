! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> implements generic sources (ECRH, ICRH, NBI)
!>
!> \author D. Coster 
!>
!> \version "$Id: generic_sources.f90 1763 2016-06-22 15:41:53Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
module generic_sources

  USE EUITM_SCHEMAS
  USE ITM_TYPES
  USE ALLOCATE_DEALLOCATE
  USE DEALLOCATE_STRUCTURES
  USE EUITM_XML_PARSER  
  USE XML_FILE_READER

  implicit none

  public ecrh, icrh, nbi

  private

  integer, parameter                       :: BUFLEN=256
  REAL(R8),              ALLOCATABLE       :: RHO(:), VPRIME(:), RHONRM(:)
  TYPE (TYPE_PARAM), save                  :: code_parameters
  integer                                  :: nrho
!irena for impurity
      INTEGER                              :: NNUCL                 !number of nuclei species
      INTEGER                              :: NION                  !number of ion species
      INTEGER                              :: NIMP,     IIMP        !number of impurity species
      INTEGER,                 ALLOCATABLE :: NZIMP(:)              !number of ionization states for each impurity
      INTEGER                              :: NNEUT,    INEUT       !number of neutrals species
      INTEGER,                 ALLOCATABLE :: NCOMP(:)              !number of components for each neutral
      INTEGER,                 ALLOCATABLE :: NTYPE(:)              !number of types for each neutral

contains

  subroutine ecrh(equilibrium, coreprof, coresource)

    TYPE (TYPE_EQUILIBRIUM), POINTER         :: EQUILIBRIUM(:)
    TYPE (TYPE_COREPROF),    POINTER         :: COREPROF(:)
    TYPE (TYPE_CORESOURCE),  POINTER         :: CORESOURCE(:)
    character(len=BUFLEN), save              :: J_SRC_F, QE_EXP_F, QE_IMP_F
    character(len=BUFLEN), allocatable, save :: QI_EXP_F(:), QI_IMP_F(:), SI_EXP_F(:), SI_IMP_F(:)
!irena for impurity
!    character(len=BUFLEN), allocatable, save :: QZ_EXP_F(:,:), QZ_IMP_F(:,:), SZ_EXP_F(:,:), SZ_IMP_F(:,:)

    character(len=BUFLEN), allocatable, save :: UI_EXP_F(:), UI_IMP_F(:)
    real(R8), save                           :: J_SRC_F_INT, QE_EXP_F_INT, QE_IMP_F_INT
    real(R8), allocatable, save              :: QI_EXP_F_INT(:), QI_IMP_F_INT(:), SI_EXP_F_INT(:), SI_IMP_F_INT(:)
!irena for impurity
!    real(R8), allocatable, save              :: QZ_EXP_F_INT(:), QZ_IMP_F_INT(:), SZ_EXP_F_INT(:), SZ_IMP_F_INT(:)

    real(R8), allocatable, save              :: UI_EXP_F_INT(:), UI_IMP_F_INT(:)
    integer                                  :: return_status, irho, iion
    logical, save                            :: first=.true.
    REAL(R8)                                 :: integral, integral_err

    write(*,*) 'ECRH module'

    if(associated(equilibrium(1)%profiles_1d%rho_vol)) then
       NRHO = size(equilibrium(1)%profiles_1d%rho_vol)
       allocate(RHONRM(NRHO))
       RHONRM = equilibrium(1)%profiles_1d%rho_vol
       write(*,*) 'RHONRM based on input equilibrium RHO_VOL'
    elseif(associated(equilibrium(1)%profiles_1d%rho_tor)) then
       NRHO = size(equilibrium(1)%profiles_1d%rho_tor)
       RHONRM = equilibrium(1)%profiles_1d%rho_tor / equilibrium(1)%profiles_1d%rho_tor(NRHO)
       write(*,*) 'RHONRM based on input equilibrium RHO_TOR'
    elseif(associated(equilibrium(1)%profiles_1d%psi)) then
       NRHO = size(equilibrium(1)%profiles_1d%psi)
       RHONRM = equilibrium(1)%profiles_1d%psi / equilibrium(1)%profiles_1d%psi(NRHO)
       write(*,*) 'RHONRM based on input equilibrium  PSI'
    else
       write(*,*) 'Could not find a suitable radial coordinate'
       stop
    endif




    if(first) then
       CALL FILL_PARAM (code_parameters, 'XML/ecrh.xml', '', 'XML/sources.xsd')
       call assign_code_parameters(code_parameters, return_status, &
            J_SRC_F, QE_EXP_F, QE_IMP_F, QI_EXP_F, QI_IMP_F, &
            SI_EXP_F, SI_IMP_F, UI_EXP_F, UI_IMP_F, &
            J_SRC_F_INT, QE_EXP_F_INT, QE_IMP_F_INT, QI_EXP_F_INT, QI_IMP_F_INT, &
            SI_EXP_F_INT, SI_IMP_F_INT, UI_EXP_F_INT, UI_IMP_F_INT)
       call deallocate_cpo(code_parameters)
       first=.false.
    endif
     
    CALL GET_COMP_DIMENSIONS (COREPROF(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
    CALL ALLOCATE_CORESOURCE_CPO  (1, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORESOURCE)


    allocate(RHO(NRHO), VPRIME(NRHO))
    RHO    = EQUILIBRIUM(1)%profiles_1d%rho_tor
    VPRIME = EQUILIBRIUM(1)%profiles_1d%vprime

    CORESOURCE(1)%time                        = COREPROF(1)%time
    CORESOURCE(1)%VALUES(1)%rho_tor                     = equilibrium(1)%profiles_1d%rho_tor
    CORESOURCE(1)%VALUES(1)%j(:)                        = profile(J_SRC_F, integral, integral_err)
    CORESOURCE(1)%VALUES(1)%qe%exp(:)                   = profile(QE_EXP_F, integral, integral_err)
    if(QE_EXP_F_int.ne.0.0_R8) then
       CORESOURCE(1)%VALUES(1)%qe%exp(:) = CORESOURCE(1)%VALUES(1)%qe%exp(:) * QE_EXP_F_int / integral
       write(*,*) 'Integral Qe rescaled to ', QE_EXP_F_int
    ELSE
       write(*,*) 'Integral Qe = ', integral, ' +- ', integral_err
    ENDIF
    CORESOURCE(1)%VALUES(1)%qe%imp(:)                   = profile(QE_IMP_F, integral, integral_err)
    do IION = 1, NION
       CORESOURCE(1)%VALUES(1)%si%exp(:,IION)           = profile(SI_EXP_F(IION), integral, integral_err)
       if(SI_EXP_F_INT(IION).ne.0.0_R8) then
          CORESOURCE(1)%VALUES(1)%si%exp(:,IION) = CORESOURCE(1)%VALUES(1)%si%exp(:,IION) * SI_EXP_F_INT(IION) / integral
          write(*,*) 'Integral Si (',IION,') rescaled to ', SI_EXP_F_INT(IION)
       ELSE
          write(*,*) 'Integral Si (',IION,') = ', integral, ' +- ', integral_err
       ENDIF
       CORESOURCE(1)%VALUES(1)%si%imp(:,IION)           = profile(SI_IMP_F(IION), integral, integral_err)
       CORESOURCE(1)%VALUES(1)%qi%exp(:,IION)           = profile(QI_EXP_F(IION), integral, integral_err)
       if(QI_EXP_F_INT(IION).ne.0.0_R8) then
          CORESOURCE(1)%VALUES(1)%qi%exp(:,IION) = CORESOURCE(1)%VALUES(1)%qi%exp(:,IION) * QI_EXP_F_INT(IION) / integral
          write(*,*) 'Integral Qi (',IION,') rescaled to ', QI_EXP_F_INT(IION)
       ELSE
          write(*,*) 'Integral Qi (',IION,') = ', integral, ' +- ', integral_err
       ENDIF
       CORESOURCE(1)%VALUES(1)%qi%imp(:,IION)           = profile(QI_IMP_F(IION), integral, integral_err)
       CORESOURCE(1)%VALUES(1)%ui%exp(:,IION)           = profile(UI_EXP_F(IION), integral, integral_err)
       if(UI_EXP_F_INT(IION).ne.0.0_R8) then
          CORESOURCE(1)%VALUES(1)%ui%exp(:,IION) = CORESOURCE(1)%VALUES(1)%ui%exp(:,IION) * UI_EXP_F_INT(IION) / integral
          write(*,*) 'Integral Ui (',IION,') rescaled to ', UI_EXP_F_INT(IION)
       ELSE
          write(*,*) 'Integral Ui (',IION,') = ', integral, ' +- ', integral_err
       ENDIF
       CORESOURCE(1)%VALUES(1)%UI%IMP(:,IION)           = profile(UI_IMP_F(IION), integral, integral_err)
    enddo
  
    deallocate(RHONRM, RHO, VPRIME)

  end subroutine ecrh

  subroutine icrh(equilibrium, coreprof, coresource)

    TYPE (TYPE_EQUILIBRIUM), POINTER         :: EQUILIBRIUM(:)
    TYPE (TYPE_COREPROF),    POINTER         :: COREPROF(:)
    TYPE (TYPE_CORESOURCE),  POINTER         :: CORESOURCE(:)
    character(len=BUFLEN), save              :: J_SRC_F, QE_EXP_F, QE_IMP_F
    character(len=BUFLEN), allocatable, save :: QI_EXP_F(:), QI_IMP_F(:), SI_EXP_F(:), SI_IMP_F(:)
    character(len=BUFLEN), allocatable, save :: UI_EXP_F(:), UI_IMP_F(:)
    real(R8), save                           :: J_SRC_F_INT, QE_EXP_F_INT, QE_IMP_F_INT
    real(R8), allocatable, save              :: QI_EXP_F_INT(:), QI_IMP_F_INT(:), SI_EXP_F_INT(:), SI_IMP_F_INT(:)
    real(R8), allocatable, save              :: UI_EXP_F_INT(:), UI_IMP_F_INT(:)
    integer                                  :: return_status, irho, iion
    logical, save                            :: first=.true.
    REAL(R8)                                 :: integral, integral_err

    write(*,*) 'ICRH module'

    if(associated(equilibrium(1)%profiles_1d%rho_vol)) then
       NRHO = size(equilibrium(1)%profiles_1d%rho_vol)
       allocate(RHONRM(NRHO))
       RHONRM = equilibrium(1)%profiles_1d%rho_vol
       write(*,*) 'RHONRM based on input equilibrium RHO_VOL'
    elseif(associated(equilibrium(1)%profiles_1d%rho_tor)) then
       NRHO = size(equilibrium(1)%profiles_1d%rho_tor)
       RHONRM = equilibrium(1)%profiles_1d%rho_tor / equilibrium(1)%profiles_1d%rho_tor(NRHO)
       write(*,*) 'RHONRM based on input equilibrium RHO_TOR'
    elseif(associated(equilibrium(1)%profiles_1d%psi)) then
       NRHO = size(equilibrium(1)%profiles_1d%psi)
       RHONRM = equilibrium(1)%profiles_1d%psi / equilibrium(1)%profiles_1d%psi(NRHO)
       write(*,*) 'RHONRM based on input equilibrium  PSI'
    else
       write(*,*) 'Could not find a suitable radial coordinate'
       stop
    endif

    if(first) then
       CALL FILL_PARAM (code_parameters, 'XML/icrh.xml', '', 'XML/sources.xsd')
       call assign_code_parameters(code_parameters, return_status, &
            J_SRC_F, QE_EXP_F, QE_IMP_F, QI_EXP_F, QI_IMP_F, &
            SI_EXP_F, SI_IMP_F, UI_EXP_F, UI_IMP_F, &
            J_SRC_F_INT, QE_EXP_F_INT, QE_IMP_F_INT, QI_EXP_F_INT, QI_IMP_F_INT, &
            SI_EXP_F_INT, SI_IMP_F_INT, UI_EXP_F_INT, UI_IMP_F_INT)
       call deallocate_cpo(code_parameters)
       first=.false.
    endif
     
    CALL GET_COMP_DIMENSIONS (COREPROF(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
    CALL ALLOCATE_CORESOURCE_CPO  (1, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORESOURCE)
    allocate(RHO(NRHO), VPRIME(NRHO))
    RHO    = EQUILIBRIUM(1)%profiles_1d%rho_tor
    VPRIME = EQUILIBRIUM(1)%profiles_1d%vprime

    CORESOURCE(1)%time                        = COREPROF(1)%time
    CORESOURCE(1)%VALUES(1)%rho_tor                     = equilibrium(1)%profiles_1d%rho_tor
    CORESOURCE(1)%VALUES(1)%j(:)                        = profile(J_SRC_F, integral, integral_err)
    CORESOURCE(1)%VALUES(1)%qe%exp(:)                   = profile(QE_EXP_F, integral, integral_err)
    if(QE_EXP_F_int.ne.0.0_R8) then
       CORESOURCE(1)%VALUES(1)%qe%exp(:) = CORESOURCE(1)%VALUES(1)%qe%exp(:) * QE_EXP_F_int / integral
       write(*,*) 'Integral Qe rescaled to ', QE_EXP_F_int
    ELSE
       write(*,*) 'Integral Qe = ', integral, ' +- ', integral_err
    ENDIF
    CORESOURCE(1)%VALUES(1)%qe%imp(:)                   = profile(QE_IMP_F, integral, integral_err)
    do IION = 1, NION
       CORESOURCE(1)%VALUES(1)%si%exp(:,IION)           = profile(SI_EXP_F(IION), integral, integral_err)
       if(SI_EXP_F_INT(IION).ne.0.0_R8) then
          CORESOURCE(1)%VALUES(1)%si%exp(:,IION) = CORESOURCE(1)%VALUES(1)%si%exp(:,IION) * SI_EXP_F_INT(IION) / integral
          write(*,*) 'Integral Si (',IION,') rescaled to ', SI_EXP_F_INT(IION)
       ELSE
          write(*,*) 'Integral Si (',IION,') = ', integral, ' +- ', integral_err
       ENDIF
       CORESOURCE(1)%VALUES(1)%si%imp(:,IION)           = profile(SI_IMP_F(IION), integral, integral_err)
       CORESOURCE(1)%VALUES(1)%qi%exp(:,IION)           = profile(QI_EXP_F(IION), integral, integral_err)
       if(QI_EXP_F_INT(IION).ne.0.0_R8) then
          CORESOURCE(1)%VALUES(1)%qi%exp(:,IION) = CORESOURCE(1)%VALUES(1)%qi%exp(:,IION) * QI_EXP_F_INT(IION) / integral
          write(*,*) 'Integral Qi (',IION,') rescaled to ', QI_EXP_F_INT(IION)
       ELSE
          write(*,*) 'Integral Qi (',IION,') = ', integral, ' +- ', integral_err
       ENDIF
       CORESOURCE(1)%VALUES(1)%qi%imp(:,IION)           = profile(QI_IMP_F(IION), integral, integral_err)
       CORESOURCE(1)%VALUES(1)%ui%exp(:,IION)           = profile(UI_EXP_F(IION), integral, integral_err)
       if(UI_EXP_F_INT(IION).ne.0.0_R8) then
          CORESOURCE(1)%VALUES(1)%ui%exp(:,IION) = CORESOURCE(1)%VALUES(1)%ui%exp(:,IION) * UI_EXP_F_INT(IION) / integral
          write(*,*) 'Integral Ui (',IION,') rescaled to ', UI_EXP_F_INT(IION)
       ELSE
          write(*,*) 'Integral Ui (',IION,') = ', integral, ' +- ', integral_err
       ENDIF
       CORESOURCE(1)%VALUES(1)%ui%imp(:,IION)           = profile(UI_IMP_F(IION), integral, integral_err)
    enddo
  
    deallocate(RHONRM, RHO, VPRIME)

  end subroutine icrh

  subroutine nbi(equilibrium, coreprof, coresource)

    TYPE (TYPE_EQUILIBRIUM), POINTER         :: EQUILIBRIUM(:)
    TYPE (TYPE_COREPROF),    POINTER         :: COREPROF(:)
    TYPE (TYPE_CORESOURCE),  POINTER         :: CORESOURCE(:)
    character(len=BUFLEN), save              :: J_SRC_F, QE_EXP_F, QE_IMP_F
    character(len=BUFLEN), allocatable, save :: QI_EXP_F(:), QI_IMP_F(:), SI_EXP_F(:), SI_IMP_F(:)
    character(len=BUFLEN), allocatable, save :: UI_EXP_F(:), UI_IMP_F(:)
    real(R8), save                           :: J_SRC_F_INT, QE_EXP_F_INT, QE_IMP_F_INT
    real(R8), allocatable, save              :: QI_EXP_F_INT(:), QI_IMP_F_INT(:), SI_EXP_F_INT(:), SI_IMP_F_INT(:)
    real(R8), allocatable, save              :: UI_EXP_F_INT(:), UI_IMP_F_INT(:)
    integer                                  :: return_status, irho, iion
    logical, save                            :: first=.true.
    REAL(R8)                                 :: integral, integral_err

    write(*,*) 'NBI module'

    if(associated(equilibrium(1)%profiles_1d%rho_vol)) then
       NRHO = size(equilibrium(1)%profiles_1d%rho_vol)
       allocate(RHONRM(NRHO))
       RHONRM = equilibrium(1)%profiles_1d%rho_vol
       write(*,*) 'RHONRM based on input equilibrium RHO_VOL'
    elseif(associated(equilibrium(1)%profiles_1d%rho_tor)) then
       NRHO = size(equilibrium(1)%profiles_1d%rho_tor)
       RHONRM = equilibrium(1)%profiles_1d%rho_tor / equilibrium(1)%profiles_1d%rho_tor(NRHO)
       write(*,*) 'RHONRM based on input equilibrium RHO_TOR'
    elseif(associated(equilibrium(1)%profiles_1d%psi)) then
       NRHO = size(equilibrium(1)%profiles_1d%psi)
       RHONRM = equilibrium(1)%profiles_1d%psi / equilibrium(1)%profiles_1d%psi(NRHO)
       write(*,*) 'RHONRM based on input equilibrium  PSI'
    else
       write(*,*) 'Could not find a suitable radial coordinate'
       stop
    endif

    if(first) then
       CALL FILL_PARAM (code_parameters, 'XML/nbi.xml', '', 'XML/sources.xsd')
       call assign_code_parameters(code_parameters, return_status, &
            J_SRC_F, QE_EXP_F, QE_IMP_F, QI_EXP_F, QI_IMP_F, &
            SI_EXP_F, SI_IMP_F, UI_EXP_F, UI_IMP_F, &
            J_SRC_F_INT, QE_EXP_F_INT, QE_IMP_F_INT, QI_EXP_F_INT, QI_IMP_F_INT, &
            SI_EXP_F_INT, SI_IMP_F_INT, UI_EXP_F_INT, UI_IMP_F_INT)
       call deallocate_cpo(code_parameters)
       first=.false.
    endif
     
    CALL GET_COMP_DIMENSIONS (COREPROF(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
    CALL ALLOCATE_CORESOURCE_CPO  (1, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORESOURCE)

    allocate(RHO(NRHO), VPRIME(NRHO))
    RHO    = EQUILIBRIUM(1)%profiles_1d%rho_tor
    VPRIME = EQUILIBRIUM(1)%profiles_1d%vprime

    CORESOURCE(1)%time                        = COREPROF(1)%time
    CORESOURCE(1)%VALUES(1)%rho_tor                     = equilibrium(1)%profiles_1d%rho_tor
    CORESOURCE(1)%VALUES(1)%j(:)                        = profile(J_SRC_F, integral, integral_err)
    CORESOURCE(1)%VALUES(1)%qe%exp(:)                   = profile(QE_EXP_F, integral, integral_err)
    if(QE_EXP_F_int.ne.0.0_R8) then
       CORESOURCE(1)%VALUES(1)%qe%exp(:) = CORESOURCE(1)%VALUES(1)%qe%exp(:) * QE_EXP_F_int / integral
       write(*,*) 'Integral Qe rescaled to ', QE_EXP_F_int
    ELSE
       write(*,*) 'Integral Qe = ', integral, ' +- ', integral_err
    ENDIF
    CORESOURCE(1)%VALUES(1)%qe%imp(:)                   = profile(QE_IMP_F, integral, integral_err)
    do IION = 1, NION
       CORESOURCE(1)%VALUES(1)%si%exp(:,IION)           = profile(SI_EXP_F(IION), integral, integral_err)
       if(SI_EXP_F_INT(IION).ne.0.0_R8) then
          CORESOURCE(1)%VALUES(1)%si%exp(:,IION) = CORESOURCE(1)%VALUES(1)%si%exp(:,IION) * SI_EXP_F_INT(IION) / integral
          write(*,*) 'Integral Si (',IION,') rescaled to ', SI_EXP_F_INT(IION)
       ELSE
          write(*,*) 'Integral Si (',IION,') = ', integral, ' +- ', integral_err
       ENDIF
       CORESOURCE(1)%VALUES(1)%si%imp(:,IION)           = profile(SI_IMP_F(IION), integral, integral_err)
       CORESOURCE(1)%VALUES(1)%qi%exp(:,IION)           = profile(QI_EXP_F(IION), integral, integral_err)
       if(QI_EXP_F_INT(IION).ne.0.0_R8) then
          CORESOURCE(1)%VALUES(1)%qi%exp(:,IION) = CORESOURCE(1)%VALUES(1)%qi%exp(:,IION) * QI_EXP_F_INT(IION) / integral
          write(*,*) 'Integral Qi (',IION,') rescaled to ', QI_EXP_F_INT(IION)
       ELSE
          write(*,*) 'Integral Qi (',IION,') = ', integral, ' +- ', integral_err
       ENDIF
       CORESOURCE(1)%VALUES(1)%qi%imp(:,IION)           = profile(QI_IMP_F(IION), integral, integral_err)
       CORESOURCE(1)%VALUES(1)%ui%exp(:,IION)           = profile(UI_EXP_F(IION), integral, integral_err)
       if(UI_EXP_F_INT(IION).ne.0.0_R8) then
          CORESOURCE(1)%VALUES(1)%ui%exp(:,IION) = CORESOURCE(1)%VALUES(1)%ui%exp(:,IION) * UI_EXP_F_INT(IION) / integral
          write(*,*) 'Integral Ui (',IION,') rescaled to ', UI_EXP_F_INT(IION)
       ELSE
          write(*,*) 'Integral Ui (',IION,') = ', integral, ' +- ', integral_err
       ENDIF
       CORESOURCE(1)%VALUES(1)%ui%imp(:,IION)           = profile(UI_IMP_F(IION), integral, integral_err)
    enddo
  
    deallocate(RHONRM, RHO, VPRIME)

  end subroutine nbi

  function profile(function_string, integral, integral_err)

    real(R8)               :: profile(1:NRHO), integral, integral_err
    character (len=BUFLEN) :: function_string

    integer*8              :: evaluator_create, function_descriptor
    double precision       :: evaluator_evaluate_x
    external                  evaluator_destroy

    integer                :: i

    function_descriptor = evaluator_create (trim(function_string))
    if(function_descriptor == 0) then
       write(*,*) 'Invalid function ', trim(function_string)
       stop
    endif

    do i = 1, NRHO
       profile(i) = evaluator_evaluate_x (function_descriptor, rhonrm(i))
    enddo

    call evaluator_destroy(function_descriptor)

    call cubint(nrho, RHO, VPRIME*profile, 1, nrho, integral, integral_err)

  end function profile

  subroutine assign_code_parameters(codeparameters, return_status, &
       J_SRC_F, QE_EXP_F, QE_IMP_F, QI_EXP_F, QI_IMP_F, &
       SI_EXP_F, SI_IMP_F, UI_EXP_F, UI_IMP_F, &
       J_SRC_F_INT, QE_EXP_F_INT, QE_IMP_F_INT, QI_EXP_F_INT, QI_IMP_F_INT, &
       SI_EXP_F_INT, SI_IMP_F_INT, UI_EXP_F_INT, UI_IMP_F_INT)

    use mod_f90_kind

    implicit none

    character(len=BUFLEN)              :: J_SRC_F, QE_EXP_F, QE_IMP_F
    character(len=BUFLEN), allocatable :: QI_EXP_F(:), QI_IMP_F(:), SI_EXP_F(:), SI_IMP_F(:)
    character(len=BUFLEN), allocatable :: UI_EXP_F(:), UI_IMP_F(:)
    real(R8)                           :: J_SRC_F_INT, QE_EXP_F_INT, QE_IMP_F_INT
    real(R8), allocatable              :: QI_EXP_F_INT(:), QI_IMP_F_INT(:), SI_EXP_F_INT(:), SI_IMP_F_INT(:)
    real(R8), allocatable              :: UI_EXP_F_INT(:), UI_IMP_F_INT(:)

    type (type_param), intent(in) :: codeparameters
    integer(ikind), intent(out) :: return_status 

    type(tree) :: parameter_list
    type(element), pointer :: temp_pointer
    integer(ikind) :: i, nparm, n_values
    character(len = 132) :: cname
    character (len=256), allocatable :: tmp_string(:)
    integer :: lng

    J_SRC_F='0.0'
    QE_EXP_F='0.0'
    QE_IMP_F='0.0'
    allocate(qi_exp_f(NION)) ; lng=NION ; qi_exp_f='0.0'
    allocate(qi_imp_f(NION)) ; lng=NION ; qi_imp_f='0.0'
    allocate(si_exp_f(NION)) ; lng=NION ; si_exp_f='0.0'
    allocate(si_imp_f(NION)) ; lng=NION ; si_imp_f='0.0'
    allocate(ui_exp_f(NION)) ; lng=NION ; ui_exp_f='0.0'
    allocate(ui_imp_f(NION)) ; lng=NION ; ui_imp_f='0.0'
    J_SRC_F_INT=0.0_R8
    QE_EXP_F_INT=0.0_R8
    QE_IMP_F_INT=0.0_R8
    allocate(qi_exp_f_int(NION)) ; lng=NION ; qi_exp_f_int=0.0_R8
    allocate(qi_imp_f_int(NION)) ; lng=NION ; qi_imp_f_int=0.0_R8
    allocate(si_exp_f_int(NION)) ; lng=NION ; si_exp_f_int=0.0_R8
    allocate(si_imp_f_int(NION)) ; lng=NION ; si_imp_f_int=0.0_R8
    allocate(ui_exp_f_int(NION)) ; lng=NION ; ui_exp_f_int=0.0_R8
    allocate(ui_imp_f_int(NION)) ; lng=NION ; ui_imp_f_int=0.0_R8

    return_status = 0      ! no error

      !-- parse xml-string codeparameters%parameters

    write(*,*) 'Calling euitm_xml_parse'
    call euitm_xml_parse(code_parameters, nparm, parameter_list)
    write(*,*) 'Called euitm_xml_parse'

      !-- assign variables

    temp_pointer => parameter_list%first

    outer: do
       cname = char2str(temp_pointer%cname)   ! necessary for AIX
       select case (cname)
       case ('parameters')
          temp_pointer => temp_pointer%child
          cycle

!--   coresource parameters
         case ('coresource')
            temp_pointer => temp_pointer%child
            cycle
         case ('j')
            if (allocated(temp_pointer%cvalue)) &
                 j_src_f = char2str(temp_pointer%cvalue)
         case ('qe_exp')
            if (allocated(temp_pointer%cvalue)) &
                 qe_exp_f = char2str(temp_pointer%cvalue)
         case ('qe_imp')
            if (allocated(temp_pointer%cvalue)) &
                 qe_imp_f = char2str(temp_pointer%cvalue)
         case ('qi_exp')
            if (allocated(temp_pointer%cvalue)) &
                 call scan_str2str(char2str(temp_pointer%cvalue), 256, qi_exp_f, lng)
         case ('qi_imp')
            if (allocated(temp_pointer%cvalue)) &
                 call scan_str2str(char2str(temp_pointer%cvalue), 256, qi_imp_f, lng)
         case ('si_exp')
            if (allocated(temp_pointer%cvalue)) &
                 call scan_str2str(char2str(temp_pointer%cvalue), 256, si_exp_f, lng)
         case ('si_imp')
            if (allocated(temp_pointer%cvalue)) &
                 call scan_str2str(char2str(temp_pointer%cvalue), 256, si_imp_f, lng)
         case ('ui_exp')
            if (allocated(temp_pointer%cvalue)) &
                 call scan_str2str(char2str(temp_pointer%cvalue), 256, ui_exp_f, lng)
         case ('ui_imp')
            if (allocated(temp_pointer%cvalue)) &
                 call scan_str2str(char2str(temp_pointer%cvalue), 256, ui_imp_f, lng)
         case ('j_int')
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, j_src_f_int)
         case ('qe_exp_int')
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, qe_exp_f_int)
         case ('qe_imp_int')
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, qe_imp_f_int)
         case ('qi_exp_int')
            if (allocated(temp_pointer%cvalue)) &
                 call scan_str2real(char2str(temp_pointer%cvalue), qi_exp_f_int, lng)
         case ('qi_imp_int')
            if (allocated(temp_pointer%cvalue)) &
                 call scan_str2real(char2str(temp_pointer%cvalue), qi_imp_f_int, lng)
         case ('si_exp_int')
            if (allocated(temp_pointer%cvalue)) &
                 call scan_str2real(char2str(temp_pointer%cvalue), si_exp_f_int, lng)
         case ('si_imp_int')
            if (allocated(temp_pointer%cvalue)) &
                 call scan_str2real(char2str(temp_pointer%cvalue), si_imp_f_int, lng)
         case ('ui_exp_int')
            if (allocated(temp_pointer%cvalue)) &
                 call scan_str2real(char2str(temp_pointer%cvalue), ui_exp_f_int, lng)
         case ('ui_imp_int')
            if (allocated(temp_pointer%cvalue)) &
                 call scan_str2real(char2str(temp_pointer%cvalue), ui_imp_f_int, lng)

!--  default
       case default
          write(*, *) 'ERROR: invalid parameter', cname
          return_status = 1
          exit
         end select
         do
            if (associated(temp_pointer%sibling)) then
               temp_pointer => temp_pointer%sibling
               exit
            end if
            if (associated(temp_pointer%parent, parameter_list%first )) &
                 exit outer
            if (associated(temp_pointer%parent)) then
               temp_pointer => temp_pointer%parent
            else
               write(*, *) 'ERROR: broken list.'
               return
            end if
         end do
      end do outer
      
      !-- destroy tree
      call destroy_xml_tree(parameter_list)
      
      return

    end subroutine assign_code_parameters

end module generic_sources
