! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
PROGRAM PREPARE_INPUT_CPOS                          

!-------------------------------------------------------!
!     This routine saves plasma profiles transport      !
!     coefficients and sources into the CPO based       !
!     data base                                         !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     changes for impurity                               !
!                                                       !
!-------------------------------------------------------!

! +++ Declaration of variables: 
  USE EUITM_SCHEMAS
  USE EUITM_ROUTINES
  USE ALLOCATE_DEALLOCATE
  USE itm_constants
  USE write_structures
  USE deallocate_structures
  USE XML_FILE_READER
  USE euitm_xml_parser 
  USE PLASMA_COMPOSITION
  use copy_structures

  IMPLICIT NONE

    INTEGER                          :: NSLICE=1
    INTEGER                          :: NRHO=100,   IRHO
    INTEGER                          :: NNUCL=1,    INUCL     !number of nuclei species
    INTEGER                          :: NION=1,     IION      !number of ion species
    INTEGER                          :: NIMP=0,     IIMP      !number of impurity species
    INTEGER,             ALLOCATABLE :: NZIMP(:)              !number of ionization states for each impurity
    INTEGER                          ::             IZIMP       
    INTEGER                          :: NNEUT=0,    INEUT     !number of neutrals species
    INTEGER,             ALLOCATABLE :: NCOMP(:)              !number of components for each neutral
    INTEGER                          ::             ICOMP       
    INTEGER,             ALLOCATABLE :: NTYPE(:)              !number of types for each neutral
    INTEGER                          ::             ITYPE       
    INTEGER                          :: NPSI=100              !number of points for equilibrium 1-D arrays
    INTEGER                          :: NDIM1=100,  IDIM1     !number of points for equilibrium 2-D arrays, first dimension
    INTEGER                          :: NDIM2=100,  IDIM2     !number of points for equilibrium 2-D arrays, second dimension
    INTEGER                          :: NPOINTS               !number of points for equilibrium boundary 
    INTEGER                          :: NCOLD=1, NTHERMAL=1, NFAST=0, NNBI=0     

    REAL (R8),           ALLOCATABLE :: AMN(:)
    REAL (R8),           ALLOCATABLE :: ZN(:)
    REAL (R8),           ALLOCATABLE :: ZION(:)
    REAL (R8),           ALLOCATABLE :: AMN_IMP(:)
    REAL (R8),           ALLOCATABLE :: ZN_IMP(:)
    REAL (R8),           ALLOCATABLE :: MAX_Z_IMP(:)

    INTEGER                          :: COLD_NEUTRALS=0
    INTEGER                          :: THERMAL_NEUTRALS=0
    INTEGER                          :: FAST_NEUTRALS=0
    INTEGER                          :: NBI_NEUTRALS=0

    INTEGER                          :: force_compositions



! +++ CPO derived types:
  TYPE (TYPE_EQUILIBRIUM), POINTER   :: EQUILIBRIUM(:)
  TYPE (TYPE_EQUILIBRIUM), POINTER   :: EQUILIBRIUM_EXT(:)
!irena compositionc  
  TYPE (TYPE_compositionc),POINTER   :: compositionc(:)
  TYPE (TYPE_compositionc),POINTER   :: compositionc_EXT(:)
!irena compositionc    
  TYPE (TYPE_COREPROF),    POINTER   :: COREPROF(:) 
  TYPE (TYPE_COREPROF),    POINTER   :: COREPROF_COMP(:) 
  TYPE (TYPE_COREPROF),    POINTER   :: COREPROF_EXT(:)
  TYPE (TYPE_CORETRANSP),  POINTER   :: CORETRANSP(:)
  TYPE (TYPE_CORETRANSP),  POINTER   :: CORETRANSP_EXT(:)
  TYPE (TYPE_CORESOURCE),  POINTER   :: CORESOURCE(:)
  TYPE (TYPE_CORESOURCE),  POINTER   :: CORESOURCE_EXT(:)
  TYPE (TYPE_COREIMPUR),   POINTER   :: COREIMPUR(:)
  TYPE (TYPE_COREIMPUR),   POINTER   :: COREIMPUR_EXT(:)
  TYPE (TYPE_TOROIDFIELD), POINTER   :: TOROIDFIELD(:)  
  TYPE (TYPE_TOROIDFIELD), POINTER   :: TOROIDFIELD_EXT(:)  
!irena neutrals
  TYPE (TYPE_CORENEUTRALS),POINTER   :: CORENEUTRALS(:)
  TYPE (TYPE_CORENEUTRALS),POINTER   :: CORENEUTRALS_EXT(:)
!irena neutrals

! +++ Control parameters:
  INTEGER                            :: IDX          !index (internal)
  INTEGER                            :: REFSHOT      !shot number
  INTEGER                            :: REFRUN       !run number
  INTEGER                            :: ind, TINDEX

  CHARACTER(len=5)                   :: TREENAME
  CHARACTER(len=8)                   :: COREPROFPATH
  CHARACTER(len=10)                  :: CORETRANSPPATH
  CHARACTER(len=10)                  :: CORESOURCEPATH
  CHARACTER(len=9)                   :: COREIMPURPATH
  CHARACTER(len=11)                  :: EQUILIBRIUMPATH
  CHARACTER(len=11)                  :: TOROIDFIELDPATH
  
!irena
  CHARACTER(len=12)                  :: CORENEUTRALSPATH
  CHARACTER(len=12)                  :: compositioncPATH
!irena
  CHARACTER(len=256)                 :: user, machine, ual_version

  CHARACTER(len=17)                  :: filename

! +++ Plasma parameters:
  REAL(R8),            ALLOCATABLE   :: RHO(:)
  REAL(R8),            ALLOCATABLE   :: RHONRM(:)
  REAL(R8)                           :: RHOB
  REAL(R8)                           :: RHOX,     IPT
  REAL(R8)                           :: CURR,     CURR_TOTAL
  REAL(R8),            ALLOCATABLE   :: NE(:)
  REAL(R8),            ALLOCATABLE   :: TE(:)
  REAL(R8),            ALLOCATABLE   :: NI(:,:)
  REAL(R8),            ALLOCATABLE   :: ZI(:,:)
  REAL(R8),            ALLOCATABLE   :: TI(:,:)
  REAL(R8),            ALLOCATABLE   :: VTOR(:,:)
  REAL(R8),            ALLOCATABLE   :: JPAR(:),  INTJpar(:)
  REAL(R8),            ALLOCATABLE   :: QSF(:)
  REAL(R8),            ALLOCATABLE   :: PR(:)
  REAL(R8),            ALLOCATABLE   :: PSI(:)
  REAL(R8),            ALLOCATABLE   :: FUN(:)

  LOGICAL                            :: EXTERNAL_COREPROF     = .FALSE., EXTERNAL_EQUILIBRIUM  = .FALSE., &
                                        EXTERNAL_CORESOURCE   = .FALSE., EXTERNAL_CORETRANSP   = .FALSE., &
                                        EXTERNAL_COREIMPUR    = .FALSE., EXTERNAL_TOROIDFIELD  = .FALSE., &
					EXTERNAL_CORENEUTRALS = .FALSE., EXTERNAL_compositionc = .FALSE.

  INTEGER                            :: iargc
 
  REAL (R8)                          :: theta

!!!
! Size of input buffer.
  INTEGER, PARAMETER                 :: BUFLEN = 256

  TYPE (TYPE_PARAM)                  :: code_parameters
  INTEGER                            :: return_status

  INTEGER                            :: SHOT=0, RUN=0
  REAL(R8)                           :: TIME=0, R0=0, B0=0, A0=0, IP=0, RGEO=0
  REAL(R8)                           :: EL=0, TR_U=0, TR_L=0
  CHARACTER(len=BUFLEN)              :: RHO_F
  REAL(R8),              ALLOCATABLE :: rho_1(:), rho_2(:), rho_3(:), rho_4(:)
  REAL(R8)                           :: dummy1, dummy2, x
  CHARACTER(len=BUFLEN), ALLOCATABLE :: NI_F(:),TI_F(:),VTOR_F(:)
  CHARACTER(len=BUFLEN)              :: TE_F, JPAR_F, QSF_F
  CHARACTER(len=BUFLEN)              :: SIGMA_F, NE_DIFF_F(3), NE_CONV_F(3)
  CHARACTER(len=BUFLEN), ALLOCATABLE :: NI_DIFF_F(:,:), NI_CONV_F(:,:)
  CHARACTER(len=BUFLEN)              :: TE_DIFF_F, TE_CONV_F
  CHARACTER(len=BUFLEN), ALLOCATABLE :: TI_DIFF_F(:), TI_CONV_F(:)
  CHARACTER(len=BUFLEN), ALLOCATABLE :: VTOR_DIFF_F(:), VTOR_CONV_F(:)
  CHARACTER(len=BUFLEN)              :: J_SRC_F, SIGMA_SRC_F, QE_EXP_F, QE_IMP_F
  CHARACTER(len=BUFLEN), ALLOCATABLE :: QI_EXP_F(:), QI_IMP_F(:), SI_EXP_F(:), SI_IMP_F(:)
!irena source for impurity
  CHARACTER(len=BUFLEN), ALLOCATABLE :: QZ_EXP_F(:), QZ_IMP_F(:), SZ_EXP_F(:), SZ_IMP_F(:)
  CHARACTER(len=BUFLEN), ALLOCATABLE :: UI_EXP_F(:), UI_IMP_F(:)
  
  CHARACTER(len=BUFLEN), ALLOCATABLE :: IMP_NZ_F(:), IMP_DIFF_F(:), IMP_CONV_F(:)

  CHARACTER(len=80)                  :: equilibrium_external='', coreprof_external='',  coreneutrals_external='',compositionc_external='',&
                                        coretransp_external='',coresource_external='', coreimpur_external='', toroidfield_external='',  &
                                        tmp_external

  CHARACTER(len=80)                  :: prepare_input_cpos_xml = 'prepare_input_cpos_1.xml'

!irena neutrlas
  INTEGER                            :: INNEUT,IMAX_NTYPE,IMAX_COMP
!irena  
  INTEGER                            :: shot_in, run_in

  INTEGER                            :: i

  IF(iargc().GT.0) THEN
     CALL getarg(1,prepare_input_cpos_xml)
  ENDIF

  CALL FILL_PARAM (code_parameters, trim(prepare_input_cpos_xml), '', 'XML/prepare_input_cpos.xsd')

  CALL assign_code_parameters(code_parameters, return_status,    &
!  
                              NZIMP,   NCOMP,  NTYPE,            &
!  
                              AMN,     ZN,     ZION,             &
!  
                              AMN_IMP, ZN_IMP, MAX_Z_IMP,        &
!
                              NPOINTS)


  IF(rgeo == 0) rgeo = r0

  WRITE(*,*) 'EXTERNAL_COREPROF = ', EXTERNAL_COREPROF
  WRITE(*,*) '     NRHO = ', NRHO
  WRITE(*,*) '     NION = ', NION
  WRITE(*,*) '     NIMP = ', NIMP
  IF(ALLOCATED(NZIMP)) &
  WRITE(*,*) '    NZIMP = ', NZIMP
  WRITE(*,*) '    NDIM1 = ', NDIM1
  WRITE(*,*) '    NDIM2 = ', NDIM2
  WRITE(*,*) '  NPOINTS = ', NPOINTS

!irena neutrals
  WRITE(*,*) '   NNEUT = ', NNEUT
  IF(ALLOCATED(NCOMP)) &
  WRITE(*,*) '   NCOMP = ', NCOMP
  IF(ALLOCATED(NTYPE)) &
  WRITE(*,*) '   NTYPE = ', NTYPE
!Irena  
  WRITE(*,*) '     SHOT = ', SHOT
  WRITE(*,*) '      RUN = ', RUN
  WRITE(*,*) '       EL = ', EL
  WRITE(*,*) '     TR_U = ', TR_U
  WRITE(*,*) '     TR_L = ', TR_L
  WRITE(*,*) '      AMN = ', AMN
  WRITE(*,*) '       ZN = ', ZN
  WRITE(*,*) '     ZION = ', ZION
  IF(.NOT.EXTERNAL_COREPROF) THEN
     DO iion=1, nion    
        WRITE(*,*) '  NI_F ',iion,' : ',TRIM(NI_F(iion))
     ENDDO
     WRITE(*,*) '  TE_F ', 0, ' : ',    TRIM(TE_F)
     DO iion=1, nion
        WRITE(*,*) '  TI_F ',iion,' : ',TRIM(TI_F(iion))
     ENDDO
     DO iion=1, nion
        WRITE(*,*) 'VTOR_F ',iion,' : ',TRIM(VTOR_F(iion))
     ENDDO
  ENDIF
  IF(NIMP > 0) THEN
     WRITE(*,*) '  AMN_IMP = ', AMN_IMP
     WRITE(*,*) '   ZN_IMP = ', ZN_IMP
  ENDIF
  WRITE(*,*) 
  WRITE(*,*) 

  REFSHOT     = 0
  REFRUN      = 0

#ifdef UAL
  IF(EXTERNAL_EQUILIBRIUM) THEN
     CALL parse_external(equilibrium_external, shot_in, run_in, user, machine, ual_version)
     TREENAME                        = 'euitm'   
     EQUILIBRIUMPATH                    = 'equilibrium' 
     CALL euitm_open_env(treename,shot_in,run_in,idx,TRIM(USER),TRIM(machine),TRIM(ual_version))
     CALL EUITM_GET    (IDX, "equilibrium", EQUILIBRIUM_EXT)
!!! DPC HACK
     WRITE(*,*) '1: input equilibrium B0 ', EQUILIBRIUM_EXT(1)%global_param%toroid_field%b0
!!     EQUILIBRIUM_EXT(1)%global_param%toroid_field%b0 = ABS(EQUILIBRIUM_EXT(1)%global_param%toroid_field%b0)
     WRITE(*,*) '2: input equilibrium B0 ', EQUILIBRIUM_EXT(1)%global_param%toroid_field%b0

  ENDIF

  IF(EXTERNAL_COREPROF) THEN
     CALL parse_external(coreprof_external, shot_in, run_in, user, machine, ual_version)
     TREENAME                        = 'euitm'   
     COREPROFPATH                    = 'coreprof' 
     CALL euitm_open_env(treename,shot_in,run_in,idx,TRIM(USER),TRIM(machine),TRIM(ual_version))
     CALL EUITM_GET    (IDX, "coreprof", COREPROF_EXT)
     NRHO              = SIZE (COREPROF_EXT(1)%rho_tor, DIM=1)
     IF(ASSOCIATED(COREPROF_EXT(1)%composition%amn)) THEN
        NION              = SIZE (COREPROF_EXT(1)%compositions%ions)
        ALLOCATE(zn(nion),amn(nion),zion(nion))
        zn = COREPROF_EXT(1)%composition%zn
        amn = COREPROF_EXT(1)%composition%amn
        zion = COREPROF_EXT(1)%composition%zion
     ELSE
        NION              = 1
        WRITE(*,*) 'Assuming 1 one species, D'
        ALLOCATE(zn(nion),amn(nion),zion(nion))
        zn = 1
        amn = 2
        zion = 1
     ENDIF
  ENDIF


  IF(EXTERNAL_CORESOURCE) THEN
     CALL parse_external(coresource_external, shot_in, run_in, user, machine, ual_version)
     TREENAME                        = 'euitm'   
     CORESOURCEPATH                  = 'coresource' 
     CALL euitm_open_env(treename,shot_in,run_in,idx,TRIM(USER),TRIM(machine),TRIM(ual_version))
     CALL EUITM_GET    (IDX, "coresource", CORESOURCE_EXT)
  ENDIF
  IF(EXTERNAL_CORETRANSP) THEN
     CALL parse_external(coretransp_external, shot_in, run_in, user, machine, ual_version)
     TREENAME                        = 'euitm'   
     CORETRANSPPATH                  = 'coretransp' 
     CALL euitm_open_env(treename,shot_in,run_in,idx,TRIM(USER),TRIM(machine),TRIM(ual_version))
     CALL EUITM_GET    (IDX, "coretransp", CORETRANSP_EXT)
  ENDIF
  IF(EXTERNAL_COREIMPUR) THEN
     CALL parse_external(coreimpur_external, shot_in, run_in, user, machine, ual_version)
     TREENAME                        = 'euitm'   
     COREIMPURPATH                   = 'coreimpur' 
     CALL euitm_open_env(treename,shot_in,run_in,idx,TRIM(USER),TRIM(machine),TRIM(ual_version))
     CALL EUITM_GET    (IDX, "coreimpur", COREIMPUR_EXT)
  ENDIF

 IF(EXTERNAL_CORENEUTRALS) THEN
     CALL parse_external(coreneutrals_external, shot_in, run_in, user, machine, ual_version)
     TREENAME                        = 'euitm'   
     CORENEUTRALSPATH                   = 'coreneutrals' 
     CALL euitm_open_env(treename,shot_in,run_in,idx,TRIM(USER),TRIM(machine),TRIM(ual_version))
     CALL EUITM_GET    (IDX, "coreneutrals", CORENEUTRALS_EXT)
  ENDIF
  
   IF(EXTERNAL_compositionc) THEN
     CALL parse_external(compositionc_external, shot_in, run_in, user, machine, ual_version)
     TREENAME                        = 'euitm'   
     compositioncPATH                   = 'compositionc' 
     CALL euitm_open_env(treename,shot_in,run_in,idx,TRIM(USER),TRIM(machine),TRIM(ual_version))
     CALL EUITM_GET    (IDX, "compositionc", compositionc_EXT)
  ENDIF

 !irena
  
  IF(EXTERNAL_TOROIDFIELD) THEN
     CALL parse_external(toroidfield_external, shot_in, run_in, user, machine, ual_version)
     TREENAME                        = 'euitm'   
     TOROIDFIELDPATH                 = 'toroidfield' 
     CALL euitm_open_env(treename,shot_in,run_in,idx,TRIM(USER),TRIM(machine),TRIM(ual_version))
     CALL EUITM_GET    (IDX, "toroidfield", TOROIDFIELD_EXT)
  ENDIF
#else
  IF(EXTERNAL_EQUILIBRIUM.OR.EXTERNAL_COREPROF.OR.EXTERNAL_CORESOURCE.OR.EXTERNAL_CORETRANSP.OR. &
       EXTERNAL_COREIMPUR.OR.EXTERNAL_TOROIDFIELD) THEN
     WRITE(*,*) 'No UAL available at compile time'
     STOP 'No UAL'
  ENDIF
#endif
  
  ALLOCATE      (     RHO(NRHO)      )
  ALLOCATE      (  RHONRM(NRHO)      )
  ALLOCATE      (      NE(NRHO)      )
  ALLOCATE      (      TE(NRHO)      )
  ALLOCATE      (     QSF(NRHO)      )
  ALLOCATE      (    JPAR(NRHO)      )
  ALLOCATE      ( INTJPAR(NRHO)      )
  ALLOCATE      (      NI(NRHO,NION) )
  ALLOCATE      (      ZI(NRHO,NION) )
  ALLOCATE      (      TI(NRHO,NION) )
  ALLOCATE      (    VTOR(NRHO,NION) )
  ALLOCATE      (      PR(NRHO)      )
  ALLOCATE      (     PSI(NRHO)      )
  ALLOCATE      (     FUN(NRHO)      )
  
  
  NPOINTS       =  100
  
  
! +++ Allocate output CPO and internal derived types:
    CALL ALLOCATE_COREPROF_CPO     (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COREPROF    )
    CALL ALLOCATE_CORETRANSP_CPO   (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORETRANSP  )        
    CALL ALLOCATE_CORESOURCE_CPO   (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORESOURCE  )
    CALL ALLOCATE_CORENEUTRALS_CPO (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORENEUTRALS)
    CALL ALLOCATE_COREIMPUR_CPO    (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COREIMPUR   )        
    CALL ALLOCATE_TOROIDFIELD_CPO  (NSLICE,                                                          TOROIDFIELD )         
    CALL ALLOCATE_EQUILIBRIUM_CPO  (NSLICE,  NPSI,  NDIM1, NDIM2, NPOINTS,                           EQUILIBRIUM )         
    CALL ALLOCATE_COMPOSITIONC_CPO (NSLICE,         NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COMPOSITIONC)        


  IF(EXTERNAL_COREPROF) THEN
     RHOB   = COREPROF_EXT(1)%rho_tor(NRHO)
  ELSE
     RHOB   = A0
  ENDIF


  IF(EXTERNAL_COREPROF) THEN
     RHO    = COREPROF_EXT(1)%rho_tor
     RHONRM = RHO/RHO(NRHO)
  ELSE
     ALLOCATE(rho_1(0:10*nrho), rho_2(0:10*nrho), rho_3(0:10*nrho), rho_4(nrho))
     DO i=0,10*nrho
        x=1.0d0/(10*nrho)*i
        rho_1(i) = x
     ENDDO
     rho_2 = profile (rho_f, rho_1)
     rho_3(0)=0
     DO i=1,10*nrho
        CALL cubint(10*nrho+1, rho_1, rho_2, 1, i+1, rho_3(i), dummy1)
     ENDDO
     rho_3=rho_3/rho_3(10*nrho)
     rho_4 = (/ (1.0_R8/(NRHO-1) * (IRHO-1), IRHO=1,NRHO) /)
     CALL l3interp(rho_1, rho_3, 10*nrho+1, rhonrm, rho_4, nrho)
     DEALLOCATE(rho_1, rho_2, rho_3, rho_4)
     RHO    = RHONRM * RHOB
  ENDIF
  RHOX   = SQRT(EL)

  IF(EXTERNAL_COREPROF) THEN
     JPAR = COREPROF_EXT(1)%profiles1d%jtot%value
     PR   = COREPROF_EXT(1)%profiles1d%pr_perp%value
     QSF  = COREPROF_EXT(1)%profiles1d%q%value
  ELSE
     JPAR            = profile(JPAR_F,rhonrm)                 !Jpar    [A/m^2]
     QSF             = profile(QSF_F,rhonrm)                  !q       [-]
     TE              = profile(TE_F,rhonrm)                   !Te      [eV]
     NE              = 0
     PR              = 0
     DO iion = 1, nion
        NI(:,iion)   = profile(NI_F(iion),rhonrm)             !ni      [m^-3]
        TI(:,iion)   = profile(TI_F(iion),rhonrm)             !Ti      [eV]
        VTOR(:,iion) = profile(VTOR_F(iion),rhonrm)           !Vtor    [m/s]
        NE           = NE + NI(:,iion) * zion(iion)           !Pr      [Pa]
        PR           = PR + NI(:,iion) * TI(:,iion)           !ne      [m^-3]
     ENDDO
     
     PR     = (PR + NE * TE) * itm_ev                               

! +++ Computed parameters:

  ENDIF

  IPT             = IP/EL
  IPT             = IP

7 CONTINUE

  CALL INTEGRAL        (NRHO, RHO, Jpar, INTJpar)

!DPC logic changed to avoid dividing by 0
  RHO_LOOP2: DO IRHO =1,NRHO
     IF(INTJpar(IRHO).NE.0.e0_R8) THEN
        QSF(IRHO)    = -RHO(IRHO)**2*B0/R0/1.25e-6_R8/INTJpar(IRHO)
     ELSE
        QSF(IRHO)    = 0.0_R8
     ENDIF
     IF (QSF(IRHO).NE.0.e0_R8) THEN
        FUN(IRHO)  = 2.E0_R8*itm_pi*B0/QSF(IRHO)
     END IF
  END DO RHO_LOOP2
  QSF(1) = QSF(2)
  FUN(1) = FUN(2)
  CALL INTEGRAL        (NRHO, RHO, FUN,  PSI)
  
  FUN  = Jpar*4.E0_R8*itm_pi**2*RHO*R0
  CALL INTEGRAL_VALUE  (NRHO, RHO, FUN, INTJpar)
  CURR = INTJpar(NRHO)/(2.E0_R8*itm_pi*R0)
  
  WRITE(6,*) curr
  IF (DABS(1.0_R8 - CURR/IPT) .GE. 1.0e-5_R8) THEN
     JPAR             = JPAR * IPT / CURR
     GOTO 7
  END IF
  
  PRINT *, IP / CURR
  WRITE (6,*) 'TOTAL CURRENT NORMALISED TO', CURR
  IF(EL.NE.1.0_R8) THEN
     RHO=RHO * RHOX
  ENDIF

  psi = sign(psi, -ip)

! COREPROF:
!==============================================================================================
! +++ Set up profiles of plasma parameters:

  IF(.NOT.EXTERNAL_COREPROF) THEN
     COREPROF(1)%time                          = TIME
     COREPROF(1)%datainfo%cocos                = 13
     COREPROF(1)%toroid_field%r0               = R0
     COREPROF(1)%toroid_field%b0               = B0
     COREPROF(1)%globalparam%current_tot       = IP
     COREPROF(1)%rho_tor                       = RHO
     COREPROF(1)%profiles1d%jtot%value         = JPAR
     COREPROF(1)%profiles1d%q%value            = QSF
     COREPROF(1)%psi%value                     = PSI
     COREPROF(1)%ne%value                      = NE
     COREPROF(1)%te%value                      = TE
     COREPROF(1)%ni%value                      = NI
     COREPROF(1)%ti%value                      = TI
     COREPROF(1)%vtor%value                    = VTOR
     COREPROF(1)%profiles1d%pr_th%value        = PR
     COREPROF(1)%profiles1d%pr_perp%value      = PR
     COREPROF(1)%profiles1d%pr_parallel%value  = PR
     COREPROF(1)%profiles1d%pe%value           = NE*TE * itm_ev
     COREPROF(1)%profiles1d%pi%value           = NI*TI * itm_ev
     coreprof(1)%psi%sigma_par%value           = 0.0_R8


! +++ Set up boundary conditions:
     COREPROF(1)%psi%boundary%value            = 0
     COREPROF(1)%ne%boundary%value             = 0
     COREPROF(1)%te%boundary%value             = 0
     COREPROF(1)%ni%boundary%value             = 0
     COREPROF(1)%ti%boundary%value             = 0
     COREPROF(1)%vtor%boundary%value           = 0

     COREPROF(1)%psi%boundary%type             = 2
     COREPROF(1)%psi%boundary%value(1)         = COREPROF(1)%globalparam%current_tot
     COREPROF(1)%psi%boundary%rho              = rho(nrho)
  
     COREPROF(1)%ne%boundary%type              = 1
     COREPROF(1)%ne%boundary%value(1)          = COREPROF(1)%ne%value(NRHO)
     COREPROF(1)%ne%boundary%rho_tor           = rho(nrho)
  
     COREPROF(1)%te%boundary%type              = 1
     COREPROF(1)%te%boundary%value(1)          = COREPROF(1)%te%value(NRHO)
     COREPROF(1)%te%boundary%rho_tor           = rho(nrho)

     DO IION=1,NION
        COREPROF(1)%ni%boundary%type(IION)        = 1
        COREPROF(1)%ni%boundary%value(1,IION)     = COREPROF(1)%ni%value(NRHO,IION)
        COREPROF(1)%ni%boundary%rho_tor(IION)     = rho(nrho)
     
        COREPROF(1)%ti%boundary%type(IION)        = 1
        COREPROF(1)%ti%boundary%value(1,IION)     = COREPROF(1)%ti%value(NRHO,IION)
        COREPROF(1)%ti%boundary%rho_tor(IION)     = rho(nrho)
     
        COREPROF(1)%vtor%boundary%type(IION)      = 1
        COREPROF(1)%vtor%boundary%value(1,IION)   = COREPROF(1)%vtor%value(NRHO,IION)
        COREPROF(1)%vtor%boundary%rho_tor(IION)   = rho(nrho)
     END DO

!     COREPROF(1)%composition%zion(:) = ZION(:)
!     COREPROF(1)%composition%amn(:)  = AMN(:)
!     COREPROF(1)%composition%zn(:)   = ZN(:)

     DO irho=1, nrho
        COREPROF(1)%profiles1d%zeff%value(irho) = 0.0_R8
        DO iion=1, nion
           COREPROF(1)%profiles1d%zeff%value(irho) = COREPROF(1)%profiles1d%zeff%value(irho) +  &
                COREPROF(1)%compositions%ions(iion)%zion**2 * COREPROF(1)%ni%value(IRHO,IION)
        ENDDO
        COREPROF(1)%profiles1d%zeff%value(irho) = COREPROF(1)%profiles1d%zeff%value(irho) / COREPROF(1)%ne%value(IRHO)
     ENDDO
  ELSE
     IF(.NOT.ASSOCIATED(coreprof_ext(1)%psi%sigma_par%value)) THEN
        ALLOCATE(coreprof_ext(1)%psi%sigma_par%value(NRHO))
        coreprof_ext(1)%psi%sigma_par%value= 0.0_R8
     ENDIF
  ENDIF
  
!==============================================================================================
   

! CORETRANSP:
!==============================================================================================
! +++ Set up profiles of transport coefficients:
  CORETRANSP(1)%time                             = TIME
  CORETRANSP(1)%datainfo%cocos                   = 13
  CORETRANSP(1)%VALUES(1)%rho_tor                          = RHO                          !rho     [m]
  CORETRANSP(1)%VALUES(1)%sigma                            = profile(SIGMA_F,rhonrm)             !sigma   [1/(Ohm*m)]

  DO TINDEX = 1, 3
     CORETRANSP(1)%VALUES(1)%ne_transp%diff_eff(:,TINDEX)  = profile(NE_DIFF_F(TINDEX),rhonrm)   !Diff_ne [m^2/s]
     CORETRANSP(1)%VALUES(1)%ne_transp%vconv_eff(:,TINDEX) = profile(NE_CONV_F(TINDEX),rhonrm)   !Vcon_ne [m/s]
     DO IION = 1, NION
        CORETRANSP(1)%VALUES(1)%ni_transp%diff_eff(:,IION,TINDEX)  = profile(NI_DIFF_F(TINDEX,IION),rhonrm)  !Diff_ni [m^2/s]
        CORETRANSP(1)%VALUES(1)%ni_transp%vconv_eff(:,IION,TINDEX) = profile(NI_CONV_F(TINDEX,IION),rhonrm)  !Vcon_ni [m/s]
     ENDDO
  ENDDO

  CORETRANSP(1)%VALUES(1)%te_transp%diff_eff(:)  = profile(TE_DIFF_F,rhonrm)                     !Diff_Te [m^2/s]
  CORETRANSP(1)%VALUES(1)%te_transp%vconv_eff(:) = profile(TE_CONV_F,rhonrm)                     !Vcon_Te [m/s]

  DO IION = 1, NION
     CORETRANSP(1)%VALUES(1)%ti_transp%diff_eff(:,IION)  = profile(TI_DIFF_F(IION),rhonrm)       !Diff_Ti [m^2/s]
     CORETRANSP(1)%VALUES(1)%ti_transp%vconv_eff(:,IION) = profile(TI_CONV_F(IION),rhonrm)       !Vcon_Ti [m/s]
     CORETRANSP(1)%VALUES(1)%vtor_transp%diff_eff(:,IION)  = profile(VTOR_DIFF_F(IION),rhonrm)   !Diff_Vtor [m^2/s]
     CORETRANSP(1)%VALUES(1)%vtor_transp%vconv_eff(:,IION) = profile(VTOR_CONV_F(IION),rhonrm)   !Vcon_Vtor [m/s]
  ENDDO

  ind=0
  DO IIMP = 1, NIMP
	
     WRITE(*,*) 'nint(ZN_IMP(IIMP))=',NINT(ZN_IMP(IIMP))
     WRITE(*,*)'NIMP=',nimp
	
     DO IZIMP = 1, NZIMP(IIMP)
        ind = ind + 1
        CORETRANSP(1)%VALUES(1)%nz_transp(iimp)%DIFF_EFF(:,izimp)        = profile(IMP_DIFF_F(ind),rhonrm)
        CORETRANSP(1)%VALUES(1)%nz_transp(iimp)%VCONV_EFF(:,izimp)       = profile(IMP_CONV_F(ind),rhonrm)
        ENDDO
     ENDDO

!==============================================================================================


! CORESOURCE:
!==============================================================================================
! +++ Set up profiles of sources:
  CORESOURCE(1)%time                        = TIME
  CORESOURCE(1)%datainfo%cocos              = 13
  CORESOURCE(1)%VALUES(1)%rho_tor                     = RHO                          !rho     [m]
  CORESOURCE(1)%VALUES(1)%j(:)                        = profile(J_SRC_F,rhonrm)             !j_ni    [A/m^2]
  coresource(1)%values(1)%sigma                       = profile (SIGMA_SRC_F,rhonrm)        !sigma
  CORESOURCE(1)%VALUES(1)%qe%exp(:)                   = profile(QE_EXP_F,rhonrm)            !Qe_exp  [W/m^3]
  CORESOURCE(1)%VALUES(1)%qe%imp(:)                   = profile(QE_IMP_F,rhonrm)            !Qe_imp  [1/m^3/s]
  DO IION = 1, NION
     CORESOURCE(1)%VALUES(1)%si%exp(:,IION)           = profile(SI_EXP_F(IION),rhonrm)      !Si_exp  [1/m^3/s]
     CORESOURCE(1)%VALUES(1)%si%imp(:,IION)           = profile(SI_IMP_F(IION),rhonrm)      !Si_imp  [1/s]
     CORESOURCE(1)%VALUES(1)%qi%exp(:,IION)           = profile(QI_EXP_F(IION),rhonrm)      !Qi_exp  [W/m^3]
     CORESOURCE(1)%VALUES(1)%qi%imp(:,IION)           = profile(QI_IMP_F(IION),rhonrm)      !Qi_imp  [1/m^3/s]
     CORESOURCE(1)%VALUES(1)%ui%exp(:,IION)           = profile(UI_EXP_F(IION),rhonrm)      !Ui_exp  [kg/m/s^2]
     CORESOURCE(1)%VALUES(1)%ui%imp(:,IION)           = profile(UI_IMP_F(IION),rhonrm)      !Ui_imp  [kg/m^2/s]
  ENDDO
! part coresource for impurity  
   
   
  DO IIMP = 1, NIMP
  
    WRITE(*,*) 'nint(ZN_IMP(IIMP))=',NINT(ZN_IMP(IIMP))
     
     DO IZIMP = 1,NINT(ZN_IMP(IIMP))
!ir        ind=ind+1
         CORESOURCE(1)%VALUES(1)%sz(IIMP)%exp(:,IZIMP) = profile(SZ_EXP_F(izimp),rhonrm) !Sz_exp  [1/m^3/s]
!        CORESOURCE(1)%VALUES(1)%sz(IIMP)%imp(:,IZIMP) = profile(SZ_IMP_F(izimp),rhonrm) !Sz_imp  [1/s]
!        CORESOURCE(1)%VALUES(1)%qz(IIMP)%exp(:,IZIMP) = profile(Qz_EXP_F(izimp),rhonrm) !Qz_exp  [W/m^3]
!         CORESOURCE(1)%VALUES(1)%qz(IIMP)%imp(:,IZIMP) = profile(Qz_IMP_F(izimp),rhonrm) !Qz_imp  [1/m^3/s]
     ENDDO
  ENDDO
   WRITE(*,*)'do tad'

!==============================================================================================

 

! COREIMPUR:
!==============================================================================================
! +++ Set up impurity profiles:
! initial to give the value 0

  COREIMPUR(1)%time                         = TIME
  COREIMPUR(1)%datainfo%cocos               = 13
  if(NIMP.gt.0) then
     
     COREIMPUR(1)%rho_tor                      = RHO                       !rho     [m]
!     COREIMPUR(1)%desc_impur%nzimp          = ZN_IMP
!     COREIMPUR(1)%desc_impur%zn             = ZN_IMP
!     COREIMPUR(1)%desc_impur%amn            = AMN_IMP
     
     ind=0
     DO IIMP = 1, NIMP
	
	WRITE(*,*) 'nint(ZN_IMP(IIMP))=',NINT(ZN_IMP(IIMP))
	WRITE(*,*)'NIMP=',nimp
	
        DO IZIMP = 1, NZIMP(IIMP)
           ind = ind + 1
!           COREIMPUR(1)%desc_impur%zmin(iimp,izimp)                     = izimp
!	   COREIMPUR(1)%desc_impur%zmax(iimp,izimp)                     = izimp
           COREIMPUR(1)%impurity(iimp)%z(:,izimp)                       = izimp
           COREIMPUR(1)%impurity(iimp)%zsq(:,izimp)                     = izimp**2
	   COREIMPUR(1)%impurity(iimp)%nz(:,izimp)                      = profile(IMP_NZ_F(ind),rhonrm)
!	   COREIMPUR(1)%impurity(iimp)%TRANSP_COEF%DIFF(:,izimp)        = profile(IMP_DIFF_F(ind),rhonrm)
!           COREIMPUR(1)%impurity(iimp)%TRANSP_COEF%VCONV(:,izimp)       = profile(IMP_CONV_F(ind),rhonrm)
           COREIMPUR(1)%impurity(iimp)%BOUNDARY%TYPE(izimp)             = 1
           COREIMPUR(1)%impurity(iimp)%BOUNDARY%RHO(izimp)              = rho(nrho)
 	   COREIMPUR(1)%IMPURITY(IIMP)%BOUNDARY%VALUE(1,IZIMP)          = COREIMPUR(1)%IMPURITY(IIMP)%nz(nrho,izimp)
        ENDDO
     ENDDO
  endif
  

!==============================================================================================
 
!irena neutrals
! CORENEUTRALS:
!==============================================================================================
! +++ Set up neutrals profiles:
! initial to give the value 0
  CORENEUTRALS(1)%time                         = TIME
  CORENEUTRALS(1)%datainfo%cocos               = 13
  CORENEUTRALS(1)%rho_tor                      = RHO   
  IF(nneut .GT. 0 ) THEN
!     CORENEUTRALS(1)%neutcompo%atomlist(1)%amn             = 2.0_R8
!     CORENEUTRALS(1)%neutcompo%atomlist(1)%zn		= 1.0_R8
!     CORENEUTRALS(1)%neutcompo%atomlist(2)%amn		= 12.0_R8
!     CORENEUTRALS(1)%neutcompo%atomlist(2)%zn		= 6.0_R8

!     CORENEUTRALS(1)%neutcompo%neutral(1)%neutcomp(1)%nucindex=1
!     CORENEUTRALS(1)%neutcompo%neutral(1)%neutcomp(1)%multiplicity=1
!     CORENEUTRALS(1)%neutcompo%neutral(1)%type(1)%flag	= 1     ! 1=thermal neutral
!     CORENEUTRALS(1)%neutcompo%neutral(1)%type(2)%flag	= 2     ! 2=fast neutral

!     CORENEUTRALS(1)%neutcompo%neutral(2)%neutcomp(1)%nucindex=2
!     CORENEUTRALS(1)%neutcompo%neutral(2)%neutcomp(1)%multiplicity=1
!     CORENEUTRALS(1)%neutcompo%neutral(2)%type%flag	= 1     ! 1=thermal neutral
!     CORENEUTRALS(1)%neutcompo%neutral(2)%type%flag	= 2     ! 2=fast neutral

     DO INEUT = 1,NNEUT
        DO ITYPE=1,NTYPE(INEUT)
           CORENEUTRALS(1)%profiles(ineut)%neutraltype(ITYPE)%n0%boundary%type           =1.
           CORENEUTRALS(1)%profiles(ineut)%neutraltype(ITYPE)%n0%boundary%rho_tor        =rho(nrho)
           CORENEUTRALS(1)%profiles(ineut)%neutraltype(ITYPE)%t0%boundary%type           =1.
           CORENEUTRALS(1)%profiles(ineut)%neutraltype(ITYPE)%t0%boundary%rho_tor        =rho(nrho)
        END DO
     END DO
	 
     CORENEUTRALS(1)%profiles(1)%neutraltype(1)%t0%value(:)          	= 1.0_R8   !in eV
     CORENEUTRALS(1)%profiles(1)%neutraltype(2)%t0%value(:)          	= 100.0_R8  !in eV
     CORENEUTRALS(1)%profiles(2)%neutraltype(1)%t0%value(:)          	= 1.0_R8  !in eV
     CORENEUTRALS(1)%profiles(2)%neutraltype(2)%t0%value(:)          	= 100.0_R8 !in eV

            

     CORENEUTRALS(1)%profiles(1)%neutraltype(1)%n0%value(NRHO)           = 1.0e+16_R8
     CORENEUTRALS(1)%profiles(2)%neutraltype(2)%n0%value(NRHO)		= 0.0_R8
     CORENEUTRALS(1)%profiles(1)%neutraltype(1)%n0%value(NRHO)		= 1.0E+3_R8
     CORENEUTRALS(1)%profiles(2)%neutraltype(2)%n0%value(NRHO)		= 0.0_R8
     CORENEUTRALS(1)%profiles(1)%neutraltype(1)%n0%boundary%value(3)	= 1.0E+16_R8
     CORENEUTRALS(1)%profiles(1)%neutraltype(2)%n0%boundary%value(3)     = 0.0_R8
     CORENEUTRALS(1)%profiles(2)%neutraltype(1)%n0%boundary%value(3)	= 1.0E+3_R8
     CORENEUTRALS(1)%profiles(2)%neutraltype(2)%n0%boundary%value(3)	= 0.0_R8
     CORENEUTRALS(1)%profiles(1)%neutraltype(1)%t0%boundary%value(3)     = 1._R8
     CORENEUTRALS(1)%profiles(1)%neutraltype(2)%t0%boundary%value(3)     = 100.0_R8
     CORENEUTRALS(1)%profiles(2)%neutraltype(1)%t0%boundary%value(3)     = 1.0_R8
     CORENEUTRALS(1)%profiles(2)%neutraltype(2)%t0%boundary%value(3)     = 100.0_R8

  END IF

!irena
! EQUILIBRIUM:
!==============================================================================================
! +++ Set up equilibrium parameters:

  if(.not.EXTERNAL_EQUILIBRIUM) then
     if(NPSI .NE. NRHO) then
        write(*,*) ' NPSI != NRHO '
        stop 'Error: NPSI != NRHO'
     endif
     EQUILIBRIUM(1)%time                         = TIME
     EQUILIBRIUM(1)%datainfo%cocos               = 13
     EQUILIBRIUM(1)%global_param%i_plasma        = IP
     EQUILIBRIUM(1)%global_param%toroid_field%r0 = R0
     EQUILIBRIUM(1)%global_param%toroid_field%b0 = B0
     EQUILIBRIUM(1)%eqgeometry%geom_axis%r       = R0
     EQUILIBRIUM(1)%eqgeometry%geom_axis%z       = 0.0_R8
     EQUILIBRIUM(1)%global_param%mag_axis%position%r = RGEO
     EQUILIBRIUM(1)%global_param%mag_axis%position%z = 0.0_R8
     EQUILIBRIUM(1)%global_param%mag_axis%bphi   = R0*B0/RGEO
     EQUILIBRIUM(1)%global_param%mag_axis%q      = QSF(1)

     EQUILIBRIUM(1)%profiles_1d%rho_tor          = RHO
     EQUILIBRIUM(1)%profiles_1d%q                = QSF
     EQUILIBRIUM(1)%profiles_1d%pressure         = PR
     EQUILIBRIUM(1)%profiles_1d%jparallel        = JPAR
     EQUILIBRIUM(1)%eqgeometry%elongation        = EL
     EQUILIBRIUM(1)%eqgeometry%tria_upper        = TR_U
     EQUILIBRIUM(1)%eqgeometry%tria_lower        = TR_L
     EQUILIBRIUM(1)%eqgeometry%a_minor           = A0
      
     EQUILIBRIUM(1)%profiles_1d%gm1              = 4.E0_R8*ITM_PI**2*RHO/R0 
     EQUILIBRIUM(1)%profiles_1d%gm2              = 1.E0_R8/R0**2
     EQUILIBRIUM(1)%profiles_1d%gm3              = 1.E0_R8 
     EQUILIBRIUM(1)%profiles_1d%gm4              = 1.E0_R8/B0**2
     EQUILIBRIUM(1)%profiles_1d%gm5              = B0**2 
     EQUILIBRIUM(1)%profiles_1d%gm6              = 4.E0_R8*ITM_PI**2*RHO*R0/B0**2 
     EQUILIBRIUM(1)%profiles_1d%gm7              = 1.E0_R8 
     EQUILIBRIUM(1)%profiles_1d%volume           = 2.E0_R8*ITM_PI**2*RHO**2*R0
!!!DPC-EQ-4.08b-problem
     EQUILIBRIUM(1)%profiles_1d%vprime           = 4.E0_R8*ITM_PI**2*RHO*R0
     EQUILIBRIUM(1)%profiles_1d%area             = ITM_PI*RHO**2
     EQUILIBRIUM(1)%profiles_1d%aprime           = 4.E0_R8*ITM_PI**2*R0 
     EQUILIBRIUM(1)%profiles_1d%F_dia            = B0*R0
     EQUILIBRIUM(1)%profiles_1d%rho_vol          = SQRT(EQUILIBRIUM(1)%profiles_1d%volume/EQUILIBRIUM(1)%profiles_1d%volume(nrho))

     EQUILIBRIUM(1)%profiles_1d%elongation       = EL
     EQUILIBRIUM(1)%profiles_1d%tria_upper       = TR_U
     EQUILIBRIUM(1)%profiles_1d%tria_lower       = TR_L
     EQUILIBRIUM(1)%profiles_1d%r_inboard        = RGEO - RHO/RHOX
     EQUILIBRIUM(1)%profiles_1d%r_outboard       = RGEO + RHO/RHOX

     EQUILIBRIUM(1)%profiles_1d%psi              = PSI
     EQUILIBRIUM(1)%profiles_1d%phi              = RHO**2 * ITM_PI * B0

     EQUILIBRIUM(1)%global_param%volume          = EQUILIBRIUM(1)%profiles_1d%volume(NRHO)
     EQUILIBRIUM(1)%global_param%area            = EQUILIBRIUM(1)%profiles_1d%area(NRHO)

  
     DO idim2=1, ndim2
        theta=REAL(idim2-1,R8)/REAL(ndim2,R8)*2.0_R8*itm_pi
        DO idim1=1, ndim1
           EQUILIBRIUM(1)%coord_sys%position%R(idim1, idim2) = RGEO +    &
                rho(nrho)*idim1/ndim1/RHOX * (COS(theta)-0.5_R8*(TR_U+TR_L)*(SIN(theta))**2)
           EQUILIBRIUM(1)%coord_sys%position%Z(idim1, idim2) =  rho(nrho)*idim1/ndim1/RHOX * EL * SIN(theta)
        ENDDO
     ENDDO

     DO i = 1, npoints
        theta=REAL(i-1,R8)/REAL(npoints)*2.0_R8*itm_pi
        EQUILIBRIUM(1)%eqgeometry%boundary(1)%r(i) = RGEO +    &
             rho(nrho)/RHOX * (COS(theta)-0.5_R8*(TR_U+TR_L)*(SIN(theta))**2)
        EQUILIBRIUM(1)%eqgeometry%boundary(1)%z(i) = rho(nrho)/RHOX * EL * SIN(theta)
     ENDDO
     
     EQUILIBRIUM(1)%codeparam%output_flag = 0  
  endif

!==============================================================================================




! TOROIDFIELD:
!==============================================================================================
! +++ Set up toroidal field:
  TOROIDFIELD(1)%time                       = TIME
  TOROIDFIELD(1)%datainfo%cocos               = 13
  TOROIDFIELD(1)%r0                         = R0
  TOROIDFIELD(1)%current%value              = IP
  TOROIDFIELD(1)%bvac_r%value               = B0*R0
!==============================================================================================









!==============================================================================================
! +++ Put plasma compositions:
      CALL ALLOCATE_COREPROF_CPO   (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COREPROF_COMP)
      CALL SET_PLASMA_COMPOSITION     (COREPROF_COMP,                         &
                                       NION,      NIMP,      NNEUT,           &
                                       AMN,       ZN,        ZION,            &
                                       AMN_IMP,   ZN_IMP,    MAX_Z_IMP,       &
                                       NCOMP,     NTYPE,                      &
                                       NCOLD,     NTHERMAL,  NFAST,   NNBI)

      call deallocate_cpo(COREPROF(1)%COMPOSITIONS)
      CALL COPY_CPO                 (COREPROF_COMP(1)%COMPOSITIONS, COREPROF(1)%COMPOSITIONS)
      call deallocate_cpo(CORETRANSP(1)%COMPOSITIONS)
      CALL COPY_CPO                 (COREPROF_COMP(1)%COMPOSITIONS, CORETRANSP(1)%COMPOSITIONS)
      call deallocate_cpo(CORESOURCE(1)%COMPOSITIONS)
      CALL COPY_CPO                 (COREPROF_COMP(1)%COMPOSITIONS, CORESOURCE(1)%COMPOSITIONS)
      call deallocate_cpo(COREIMPUR(1)%COMPOSITIONS)
      CALL COPY_CPO                 (COREPROF_COMP(1)%COMPOSITIONS, COREIMPUR(1)%COMPOSITIONS)
      call deallocate_cpo(CORENEUTRALS(1)%COMPOSITIONS)
      CALL COPY_CPO                 (COREPROF_COMP(1)%COMPOSITIONS, CORENEUTRALS(1)%COMPOSITIONS)
      call deallocate_cpo(COMPOSITIONC(1)%COMPOSITIONS)
      CALL COPY_CPO                 (COREPROF_COMP(1)%COMPOSITIONS, COMPOSITIONC(1)%COMPOSITIONS)


    IF (force_compositions.EQ.1) THEN

      IF(EXTERNAL_COREPROF)     THEN
         DO i=1,SIZE(COREPROF_EXT)
            call deallocate_cpo(COREPROF_EXT(i)%COMPOSITIONS)
            CALL COPY_CPO (COREPROF_COMP(1)%COMPOSITIONS, COREPROF_EXT(i)%COMPOSITIONS)
         end do
      end if
      IF(EXTERNAL_CORETRANSP) then
         call deallocate_cpo(CORETRANSP_EXT(1)%COMPOSITIONS)
         CALL COPY_CPO (COREPROF_COMP(1)%COMPOSITIONS, CORETRANSP_EXT(1)%COMPOSITIONS)
      endif
      IF(EXTERNAL_CORESOURCE) then
         call deallocate_cpo(CORESOURCE_EXT(1)%COMPOSITIONS)
         CALL COPY_CPO (COREPROF_COMP(1)%COMPOSITIONS, CORESOURCE_EXT(1)%COMPOSITIONS)
      endif
      IF(EXTERNAL_COREIMPUR) then
         call deallocate_cpo(COREIMPUR_EXT(1)%COMPOSITIONS)
         CALL COPY_CPO (COREPROF_COMP(1)%COMPOSITIONS, COREIMPUR_EXT(1)%COMPOSITIONS)
      endif
      IF(EXTERNAL_CORENEUTRALS) then
         call deallocate_cpo(CORENEUTRALS_EXT(1)%COMPOSITIONS)
         CALL COPY_CPO (COREPROF_COMP(1)%COMPOSITIONS, CORENEUTRALS_EXT(1)%COMPOSITIONS)
      endif


    END IF

!==============================================================================================


! +++ Define tree_name and CPO_name for the data base:
  TREENAME                        = 'euitm'   
  COREPROFPATH                    = 'coreprof' 
  CORETRANSPPATH                  = 'coretransp' 
  CORESOURCEPATH                  = 'coresource' 
  COREIMPURPATH                   = 'coreimpur' 
!irena 
  CORENEUTRALSPATH                = 'coreneutrals' 
  COMPOSITIONCPATH                = 'COMPOSITIONC' 
!irena
  EQUILIBRIUMPATH                 = 'equilibrium' 
  TOROIDFIELDPATH                 = 'toroidfield' 

  WRITE(filename,'(''CPO_'',I6.6,''_'',I6.6)') shot, run
  CALL open_write_file(1, filename)
  IF(EXTERNAL_COREPROF) THEN
     CALL write_cpo(coreprof_ext(1), 'coreprof')
  ELSE
     CALL write_cpo(coreprof(1), 'coreprof')
  ENDIF
  IF(EXTERNAL_CORETRANSP) THEN
     CALL write_cpo(coretransp_ext(1), 'coretransp')
  ELSE
     CALL write_cpo(coretransp(1), 'coretransp')
  ENDIF
  IF(EXTERNAL_CORESOURCE) THEN
    CALL write_cpo(coresource_ext(1), 'coresource')
  ELSE
     CALL write_cpo(coresource(1), 'coresource')
  ENDIF
  IF(EXTERNAL_COREIMPUR) THEN
    CALL write_cpo(coreimpur_ext(1), 'coreimpur')
  ELSE
     CALL write_cpo(coreimpur(1), 'coreimpur')
  ENDIF
!irena
 IF(EXTERNAL_CORENEUTRALS) THEN
    CALL write_cpo(coreneutrals_ext(1), 'coreneutrals')
  ELSE
     CALL write_cpo(coreneutrals(1), 'coreneutrals')
  ENDIF
  IF(EXTERNAL_COMPOSITIONC) THEN
    CALL write_cpo(COMPOSITIONC_ext, 'COMPOSITIONC')
  ELSE
     CALL write_cpo(COMPOSITIONC, 'COMPOSITIONC')
  ENDIF
!irena
  IF(EXTERNAL_EQUILIBRIUM) THEN
    CALL write_cpo(equilibrium_ext(1), 'equilibrium')
  ELSE
     CALL write_cpo(equilibrium(1), 'equilibrium')
  ENDIF
  IF(EXTERNAL_TOROIDFIELD) THEN
    CALL write_cpo(toroidfield_ext(1), 'toroidfield')
  ELSE
     CALL write_cpo(toroidfield(1), 'toroidfield')
  ENDIF
  CALL close_write_file

! +++ Write CPOs in the data base:
#ifdef UAL
  WRITE(tmp_external,'(i,''/'',i)') shot, run
  WRITE(*,*) tmp_external
  CALL parse_external(tmp_external, shot_in, run_in, user, machine, ual_version)
  WRITE(*,*) shot_in, run_in, trim(user), trim(machine), trim(ual_version)
  TREENAME                        = 'euitm'   
  COREPROFPATH                    = 'coreprof' 
  CALL euitm_create_env(treename,shot_in,run_in,REFSHOT,REFRUN, idx,TRIM(USER),TRIM(machine),TRIM(ual_version))
!  CALL EUITM_CREATE               (TREENAME, SHOT, RUN, REFSHOT, REFRUN, IDX)
  IF(EXTERNAL_COREPROF) THEN
     CALL EUITM_PUT_NON_TIMED        (IDX, COREPROFPATH,   COREPROF_EXT(1)  )
     CALL EUITM_PUT_SLICE            (IDX, COREPROFPATH,   COREPROF_EXT(1)  )
  ELSE
     CALL EUITM_PUT_NON_TIMED        (IDX, COREPROFPATH,   COREPROF(1)  )
     CALL EUITM_PUT_SLICE            (IDX, COREPROFPATH,   COREPROF(1)  )
  ENDIF
  PRINT *, '==============================================================='
  PRINT *, '>>>>>>>>>>>>>>    COREPROF FINISHED'
  PRINT *, '==============================================================='
  PRINT *, ' '
  PRINT *, ' '
  IF(EXTERNAL_CORETRANSP) THEN
     CALL EUITM_PUT_NON_TIMED        (IDX, CORETRANSPPATH, CORETRANSP_EXT(1))
     CALL EUITM_PUT_SLICE            (IDX, CORETRANSPPATH, CORETRANSP_EXT(1))
  ELSE
     CALL EUITM_PUT_NON_TIMED        (IDX, CORETRANSPPATH, CORETRANSP(1))
     CALL EUITM_PUT_SLICE            (IDX, CORETRANSPPATH, CORETRANSP(1))
  ENDIF
  PRINT *, '==============================================================='
  PRINT *, '>>>>>>>>>>>>>>    CORETRANSP FINISHED'
  PRINT *, '==============================================================='
  PRINT *, ' '
  PRINT *, ' '
  IF(EXTERNAL_CORESOURCE) THEN
     CALL EUITM_PUT_NON_TIMED        (IDX, CORESOURCEPATH, CORESOURCE_EXT(1))
     CALL EUITM_PUT_SLICE            (IDX, CORESOURCEPATH, CORESOURCE_EXT(1))
  ELSE
     CALL EUITM_PUT_NON_TIMED        (IDX, CORESOURCEPATH, CORESOURCE(1))
     CALL EUITM_PUT_SLICE            (IDX, CORESOURCEPATH, CORESOURCE(1))
  ENDIF
  PRINT *, '==============================================================='
  PRINT *, '>>>>>>>>>>>>>>    CORESOURCE FINISHED'
  PRINT *, '==============================================================='
  PRINT *, ' '
  PRINT *, ' '
  IF(EXTERNAL_COREIMPUR) THEN
     CALL EUITM_PUT_NON_TIMED        (IDX, COREIMPURPATH, COREIMPUR_EXT(1))
     CALL EUITM_PUT_SLICE            (IDX, COREIMPURPATH, COREIMPUR_EXT(1))
  ELSE
     CALL EUITM_PUT_NON_TIMED        (IDX, COREIMPURPATH, COREIMPUR(1))
     CALL EUITM_PUT_SLICE            (IDX, COREIMPURPATH, COREIMPUR(1))
  ENDIF
  PRINT *, '==============================================================='
  PRINT *, '>>>>>>>>>>>>>>    COREIMPUR FINISHED'
  PRINT *, '==============================================================='
  !irena
  PRINT *, ' '
  PRINT *, ' '
  IF(EXTERNAL_CORENEUTRALS) THEN
     CALL EUITM_PUT_NON_TIMED        (IDX, CORENEUTRALSPATH, CORENEUTRALS_EXT(1))
     CALL EUITM_PUT_SLICE            (IDX, CORENEUTRALSPATH, CORENEUTRALS_EXT(1))
  ELSE
     CALL EUITM_PUT_NON_TIMED        (IDX, CORENEUTRALSPATH, CORENEUTRALS(1))
     CALL EUITM_PUT_SLICE            (IDX, CORENEUTRALSPATH, CORENEUTRALS(1))
  ENDIF
  PRINT *, '==============================================================='
  PRINT *, '>>>>>>>>>>>>>>    CORENEUTRALS FINISHED'
  PRINT *, '==============================================================='
  !irena
  PRINT *, ' '
  PRINT *, ' '
  IF(EXTERNAL_EQUILIBRIUM) THEN
     CALL EUITM_PUT_NON_TIMED        (IDX, EQUILIBRIUMPATH, EQUILIBRIUM_EXT(1))
     CALL EUITM_PUT_SLICE            (IDX, EQUILIBRIUMPATH, EQUILIBRIUM_EXT(1))
  ELSE
     CALL EUITM_PUT_NON_TIMED        (IDX, EQUILIBRIUMPATH, EQUILIBRIUM(1))
     CALL EUITM_PUT_SLICE            (IDX, EQUILIBRIUMPATH, EQUILIBRIUM(1))
  ENDIF
  PRINT *, '==============================================================='
  PRINT *, '>>>>>>>>>>>>>>    EQUILIBRIUM FINISHED'
  PRINT *, '==============================================================='
  PRINT *, ' '
  PRINT *, ' '
  IF(EXTERNAL_TOROIDFIELD) THEN
     CALL EUITM_PUT_NON_TIMED        (IDX, TOROIDFIELDPATH, TOROIDFIELD_EXT(1))
     CALL EUITM_PUT_SLICE            (IDX, TOROIDFIELDPATH, TOROIDFIELD_EXT(1))
  ELSE
     CALL EUITM_PUT_NON_TIMED        (IDX, TOROIDFIELDPATH, TOROIDFIELD(1))
     CALL EUITM_PUT_SLICE            (IDX, TOROIDFIELDPATH, TOROIDFIELD(1))
  ENDIF
  PRINT *, '==============================================================='
  PRINT *, '>>>>>>>>>>>>>>    TOROIDFIELD FINISHED'
  PRINT *, '==============================================================='
  PRINT *, ' '
  PRINT *, ' '
  CALL EUITM_CLOSE                (IDX)
#endif

  WRITE(*,*) 'Data written for ',SHOT, RUN
  WRITE(*,*)
  WRITE(*,*)

! +++ Deallocate CPOs:
  CALL DEALLOCATE_CPO (COREPROF   )        
  CALL DEALLOCATE_CPO (CORETRANSP )          
  CALL DEALLOCATE_CPO (CORESOURCE )          
  CALL DEALLOCATE_CPO (COREIMPUR  ) 
!irena
  CALL DEALLOCATE_CPO (CORENEUTRALS ) 
  CALL DEALLOCATE_CPO (COMPOSITIONC ) 
!irena         
  CALL DEALLOCATE_CPO (EQUILIBRIUM)         
  CALL DEALLOCATE_CPO (TOROIDFIELD)         
  
  DEALLOCATE      (     RHO )
  DEALLOCATE      (      NE )
  DEALLOCATE      (      TE )
  DEALLOCATE      (     QSF )
  DEALLOCATE      (    JPAR )
  DEALLOCATE      ( INTJPAR )
  DEALLOCATE      (      NI )
  DEALLOCATE      (      TI )
  DEALLOCATE      (    VTOR )
  DEALLOCATE      (      PR )
  DEALLOCATE      (     PSI )
  DEALLOCATE      (     FUN )

CONTAINS

  FUNCTION profile(function_string, x)

    IMPLICIT NONE

    REAL(R8)               :: x(:), profile(1:SIZE(x))
    CHARACTER (len=BUFLEN) :: function_string

    INTEGER*8              :: evaluator_create, function_descriptor
    DOUBLE PRECISION       :: evaluator_evaluate_x
    EXTERNAL                  evaluator_destroy

    INTEGER                :: i

    function_descriptor = evaluator_create (TRIM(function_string))
    IF(function_descriptor == 0) THEN
       WRITE(*,*) 'Invalid function ', TRIM(function_string)
       STOP
    ENDIF

    DO i = 1, SIZE(x)
       profile(i) = evaluator_evaluate_x (function_descriptor, x(i))
    ENDDO

    CALL evaluator_destroy(function_descriptor)

  END FUNCTION profile

  SUBROUTINE assign_code_parameters(codeparameters, return_status,&
!  
                              NZIMP,   NCOMP,  NTYPE,            &
!  
                              AMN,     ZN,     ZION,             &
!  
                              AMN_IMP, ZN_IMP, MAX_Z_IMP,        &
!
                              NPOINTS)

      !-----------------------------------------------------------------------
      ! calls the XML parser for the code parameters and assign the
      ! resulting values to the corresponding variables
      !TODO: check an alternative and more elegant solution in Perl
      !-----------------------------------------------------------------------

    USE mod_f90_kind

    IMPLICIT NONE

    TYPE (type_param), INTENT(in)    :: codeparameters
    INTEGER(ikind),   INTENT(out)    :: return_status 

    TYPE(tree)                       :: parameter_list
    TYPE(element), POINTER           :: temp_pointer
    INTEGER(ikind)                   :: i, nparm, n_values
    INTEGER                          :: n_data1, n_data2, n_data3
    CHARACTER(len = 132)             :: cname
    CHARACTER (len=256), ALLOCATABLE :: tmp_string(:)
    REAL (R8)                        :: tmp_real(10*100)
    INTEGER                          :: tmp_int(10*100)
    INTEGER                          :: n_data
    INTEGER                          :: lng
    INTEGER                          :: nzimps
    INTEGER                          :: integer_data(1000)
    REAL(R8)                         :: real_data(1000)

    LOGICAL                          :: l_nion=.FALSE., l_nimp=.FALSE., l_nzimp=.FALSE.

    INTEGER,             ALLOCATABLE :: NZIMP(:)              !number of ionization states for each impurity
    INTEGER,             ALLOCATABLE :: NCOMP(:)              !number of components for each neutral
    INTEGER,             ALLOCATABLE :: NTYPE(:)              !number of types for each neutral
    INTEGER                          :: NPOINTS               !number of points

    REAL (R8),           ALLOCATABLE :: AMN(:)
    REAL (R8),           ALLOCATABLE :: ZN(:)
    REAL (R8),           ALLOCATABLE :: ZION(:)
    REAL (R8),           ALLOCATABLE :: AMN_IMP(:)
    REAL (R8),           ALLOCATABLE :: ZN_IMP(:)
    REAL (R8),           ALLOCATABLE :: MAX_Z_IMP(:)


    RHO_F          = '1.0'
    TE_F           = '0.0'
    JPAR_F         = '0.0'
    QSF_F          = '0.0'
    SIGMA_F        = '0.0'
    TE_DIFF_F      = '0.0'
    TE_CONV_F      = '0.0'
    J_SRC_F        = '0.0'
    SIGMA_SRC_F    = '0.0'
    QE_EXP_F       = '0.0'
    QE_IMP_F       = '0.0'

    n_data1 = 0
    n_data2 = 0
    n_data3 = 0

    nzimps         = 0

    return_status  = 0      ! no error

      !-- parse xml-string codeparameters%parameters

    WRITE(*,*) 'Calling euitm_xml_parse'
    CALL euitm_xml_parse(code_parameters, nparm, parameter_list)
    WRITE(*,*) 'Called euitm_xml_parse'

      !-- assign variables

    temp_pointer => parameter_list%first

    outer: DO

       IF(l_nion) THEN
          ALLOCATE(ni_f(NION))            ; ni_f        = '0.0'
          ALLOCATE(ti_f(NION))            ; ti_f        = '0.0'
          ALLOCATE(vtor_f(NION))          ; vtor_f      = '0.0'
          ALLOCATE(ni_diff_f(3,NION))     ; ni_diff_f   = '0.0'
          ALLOCATE(ni_conv_f(3,NION))     ; ni_conv_f   = '0.0'
          ALLOCATE(ti_diff_f(NION))       ; ti_diff_f   = '0.0'
          ALLOCATE(ti_conv_f(NION))       ; ti_conv_f   = '0.0'
          ALLOCATE(vtor_diff_f(NION))     ; vtor_diff_f = '0.0'
          ALLOCATE(vtor_conv_f(NION))     ; vtor_conv_f = '0.0'
          ALLOCATE(qi_exp_f(NION))        ; qi_exp_f    = '0.0'
          ALLOCATE(qi_imp_f(NION))        ; qi_imp_f    = '0.0'
          ALLOCATE(si_exp_f(NION))        ; si_exp_f    = '0.0'
          ALLOCATE(si_imp_f(NION))        ; si_imp_f    = '0.0'
          ALLOCATE(ui_exp_f(NION))        ; ui_exp_f    = '0.0'
          ALLOCATE(ui_imp_f(NION))        ; ui_imp_f    = '0.0'
          l_nion =.FALSE.
       ENDIF

       IF(l_nimp.AND.l_nzimp) THEN
          IF(nzimps > 0) THEN
             ALLOCATE(imp_nz_f(nzimps))   ; imp_nz_f    = '0.0'
             ALLOCATE(imp_diff_f(nzimps)) ; imp_diff_f  = '0.0'
             ALLOCATE(imp_conv_f(nzimps)) ; imp_conv_f  = '0.0'

             ALLOCATE(qz_exp_f(nzimps))   ; qz_exp_f    = '0.0'
             ALLOCATE(qz_imp_f(nzimps))   ; qz_imp_f    = '0.0'
             ALLOCATE(sz_exp_f(nzimps))   ; sz_exp_f    = '0.0'
             ALLOCATE(sz_imp_f(nzimps))   ; sz_imp_f    = '0.0'	     
          ENDIF
          l_nimp    =.FALSE.
          l_nzimp   =.FALSE.
       ENDIF

 

      cname = char2str(temp_pointer%cname)   ! necessary for AIX
       SELECT CASE (cname)
       CASE ("parameters")
          temp_pointer => temp_pointer%child
          CYCLE

!--   dimensions parameters
       CASE ("dimensions")
          temp_pointer => temp_pointer%child
          CYCLE
       CASE ("nrho")
          IF (ALLOCATED(temp_pointer%cvalue)) &
               CALL char2num(temp_pointer%cvalue, NRHO)
          WRITE(*,*) 'NRHO = ', nrho
         CASE ("npsi")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, npsi)
         CASE ("neq_dim1")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, ndim1)
         CASE ("neq_dim2")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, ndim2)
         CASE ("neq_max_npoints")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, npoints)


!--   output parameters
         CASE ("output")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("shot")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, SHOT)
         CASE ("run")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, RUN)



!--   global parameters
         CASE ("global")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("time")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, TIME)
         CASE ("R0")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, R0)
         CASE ("B0")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, B0)
         CASE ("A0")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, A0)
         CASE ("Ip")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, Ip)
         CASE ("Rgeo")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, Rgeo)
         CASE ("rho")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 rho_f = char2str(temp_pointer%cvalue)



!--   compositions
         CASE ("compositions")
            temp_pointer => temp_pointer%child
            CYCLE


         CASE ("force_compositions")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, force_compositions)
 

         CASE ("ions")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("amn")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data1)
            ALLOCATE(amn(n_data1))
            amn  = real_data(1:n_data1)
            
         CASE ("zn")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data2)
            ALLOCATE(zn(n_data2))
            zn   = real_data(1:n_data2)

         CASE ("zion")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data3)
            ALLOCATE(zion(n_data3))
            zion = real_data(1:n_data3)

         NION    =  MIN(n_data1, n_data2, n_data3)
         l_nion=.TRUE.

 
         CASE ("impurity")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("amn_imp")
            IF (ALLOCATED(temp_pointer%cvalue)) then
               CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data1)
               ALLOCATE(amn_imp(n_data1))
               amn_imp  = real_data(1:n_data1)
            else
               n_data1 = 0
            endif

         CASE ("zn_imp")
            IF (ALLOCATED(temp_pointer%cvalue)) then
               CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data2)
               ALLOCATE(zn_imp(n_data2))
               zn_imp   = real_data(1:n_data2)
            else
               n_data2 = 0
            endif

         CASE ("max_z_imp")
            IF (ALLOCATED(temp_pointer%cvalue)) then
               CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data3)
               ALLOCATE(max_z_imp(n_data3))
               max_z_imp = real_data(1:n_data3)
            else
               n_data3 = 0
            endif

         
            NIMP    =  MIN(n_data1, n_data2, n_data3)
            if(nimp.gt.0) then
               ALLOCATE (NZIMP(NIMP))
               NZIMP   =  NINT(max_z_imp)
               l_nimp  = .TRUE.
               l_nzimp = .TRUE.
               nzimps  =  SUM(NZIMP)   ! need tp specify 1 entity for each impurity charge state
            endif

         CASE ("neutrals")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("cold_neutrals")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, cold_neutrals)
            IF (cold_neutrals.gt.0) cold_neutrals = 1
         CASE ("thermal_neutrals")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, thermal_neutrals)
            IF (thermal_neutrals.gt.0) thermal_neutrals = 1
         CASE ("fast_neutrals")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, fast_neutrals)
            IF (fast_neutrals.gt.0) fast_neutrals = 1
         CASE ("NBI_neutrals")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, NBI_neutrals)
            IF (NBI_neutrals.gt.0) NBI_neutrals = 1

         NNUCL   = NION + NIMP !assummption of all species being different


         if(cold_neutrals + thermal_neutrals + fast_neutrals + NBI_neutrals .gt. 0) then
            NNEUT   = NION + NIMP !assummption of all species being different
            ALLOCATE (NCOMP(NNEUT))
            ALLOCATE (NTYPE(NNEUT))
	    NCOMP   = 1
            NTYPE   = cold_neutrals + thermal_neutrals + fast_neutrals + NBI_neutrals
         else
            NNEUT = 0
         endif

!--   equilibrium parameters
         CASE ("equilibrium")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("equilibrium_ext")
            IF (ALLOCATED(temp_pointer%cvalue)) THEN
               equilibrium_external = char2str(temp_pointer%cvalue)
               WRITE(*,*) '<<',TRIM(equilibrium_external),'>>'
               external_equilibrium = equilibrium_external /= ''
            ENDIF
         CASE ("el")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, el)
         CASE ("tr_u")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, tr_u)
         CASE ("tr_l")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, tr_l)

!--   coreprof parameters
         CASE ("coreprof")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("coreprof_ext")
            IF (ALLOCATED(temp_pointer%cvalue)) THEN
               coreprof_external = char2str(temp_pointer%cvalue)
               WRITE(*,*) '<<',TRIM(coreprof_external),'>>'
               external_coreprof = coreprof_external /= ''
            ENDIF
!            ENDIF
         CASE ("ni")
            lng=NION
            IF(.NOT.external_coreprof) THEN
               IF (ALLOCATED(temp_pointer%cvalue)) &
                    CALL scan_str2str(char2str(temp_pointer%cvalue), 256, ni_f, lng)
            ENDIF
         CASE ("ti")
            lng=NION
            IF(.NOT.external_coreprof) THEN
               IF (ALLOCATED(temp_pointer%cvalue)) &
                    CALL scan_str2str(char2str(temp_pointer%cvalue), 256, ti_f, lng)
            ENDIF
         CASE ("vtor")
            lng=NION
            IF(.NOT.external_coreprof) THEN
               IF (ALLOCATED(temp_pointer%cvalue)) &
                    CALL scan_str2str(char2str(temp_pointer%cvalue), 256, vtor_f, lng)
            ENDIF
         CASE ("te")
            IF(.NOT.external_coreprof) THEN
               IF (ALLOCATED(temp_pointer%cvalue)) &
                    te_f = char2str(temp_pointer%cvalue)
            ENDIF
         CASE ("jpar")
            IF(.NOT.external_coreprof) THEN
               IF (ALLOCATED(temp_pointer%cvalue)) &
                    jpar_f = char2str(temp_pointer%cvalue)
            ENDIF
         CASE ("qsf")
            IF(.NOT.external_coreprof) THEN
               IF (ALLOCATED(temp_pointer%cvalue)) &
                    qsf_f = char2str(temp_pointer%cvalue)
            ENDIF

!--   coretransp parameters
         CASE ("coretransp")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("coretransp_ext")
            IF (ALLOCATED(temp_pointer%cvalue)) THEN
               coretransp_external = char2str(temp_pointer%cvalue)
               WRITE(*,*) '<<',TRIM(coretransp_external),'>>'
               external_coretransp = coretransp_external /= ''
            ENDIF
         CASE ("sigma")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 sigma_f = char2str(temp_pointer%cvalue)
         CASE ("ne_diff")
            lng=3
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2str(char2str(temp_pointer%cvalue), 256, ne_diff_f, lng)
         CASE ("ne_conv")
            lng=3
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2str(char2str(temp_pointer%cvalue), 256, ne_conv_f, lng)
         CASE ("ni_diff")
            ALLOCATE(tmp_string(3*NION)) ; lng =3*NION
            IF (ALLOCATED(temp_pointer%cvalue)) THEN
               CALL scan_str2str(char2str(temp_pointer%cvalue), 256, tmp_string, lng)
               ni_diff_f=RESHAPE(tmp_string,SHAPE(ni_diff_f))
            ENDIF
            DEALLOCATE(tmp_string)
         CASE ("ni_conv")
            ALLOCATE(tmp_string(3*NION)) ; lng =3*NION
            IF (ALLOCATED(temp_pointer%cvalue)) THEN
               CALL scan_str2str(char2str(temp_pointer%cvalue), 256, tmp_string, lng)
               ni_conv_f=RESHAPE(tmp_string,SHAPE(ni_conv_f))
            ENDIF
            DEALLOCATE(tmp_string)
         CASE ("te_diff")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 te_diff_f = char2str(temp_pointer%cvalue)
         CASE ("te_conv")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 te_conv_f = char2str(temp_pointer%cvalue)
         CASE ("ti_diff")
            lng=NION
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2str(char2str(temp_pointer%cvalue), 256, ti_diff_f, lng)
         CASE ("ti_conv")
            lng=NION
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2str(char2str(temp_pointer%cvalue), 256, ti_conv_f, lng)
         CASE ("vtor_diff")
            lng=NION
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2str(char2str(temp_pointer%cvalue), 256, vtor_diff_f, lng)
         CASE ("vtor_conv")
            lng=NION
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2str(char2str(temp_pointer%cvalue), 256, vtor_conv_f, lng)
         CASE ("nz_diff")
            IF(nzimps > 0) THEN
               ALLOCATE(tmp_string(nzimps)) ; lng=nzimps
               IF (ALLOCATED(temp_pointer%cvalue)) THEN
                  CALL scan_str2str(char2str(temp_pointer%cvalue), 256, tmp_string, lng)
                  imp_diff_f=tmp_string
               ENDIF
               DEALLOCATE(tmp_string)
            ENDIF
         CASE ("nz_conv")
            IF(nzimps > 0) THEN
               ALLOCATE(tmp_string(nzimps)) ; lng=nzimps
               IF (ALLOCATED(temp_pointer%cvalue)) THEN
                  CALL scan_str2str(char2str(temp_pointer%cvalue), 256, tmp_string, lng)
                  imp_conv_f=tmp_string
               ENDIF
               DEALLOCATE(tmp_string)
            ENDIF

!--   coresource parameters
         CASE ("coresource")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("coresource_ext")
            IF (ALLOCATED(temp_pointer%cvalue)) THEN
               coresource_external = char2str(temp_pointer%cvalue)
               WRITE(*,*) '<<',TRIM(coresource_external),'>>'
               external_coresource = coresource_external /= ''
            ENDIF
         CASE ("j")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 j_src_f = char2str(temp_pointer%cvalue)
         CASE ("sigma_src")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 sigma_src_f = char2str(temp_pointer%cvalue)
         CASE ("qe_exp")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 qe_exp_f = char2str(temp_pointer%cvalue)
         CASE ("qe_imp")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 qe_imp_f = char2str(temp_pointer%cvalue)
         CASE ("qi_exp")
            lng=NION
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2str(char2str(temp_pointer%cvalue), 256, qi_exp_f, lng)
         CASE ("qi_imp")
            lng=NION
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2str(char2str(temp_pointer%cvalue), 256, qi_imp_f, lng)
         CASE ("qz_exp")
            IF(nzimps > 0) THEN
               ALLOCATE(tmp_string(NZIMPS)) ; lng=NZIMPS
               IF (ALLOCATED(temp_pointer%cvalue)) THEN
                  CALL scan_str2str(char2str(temp_pointer%cvalue), 256, tmp_string, lng)
                  qz_exp_f=tmp_string
               ENDIF
               DEALLOCATE(tmp_string)
            ENDIF
	  CASE ("qz_imp")
            IF(nzimps > 0) THEN
               ALLOCATE(tmp_string(NZIMPS)) ; lng=NZIMPS
               IF (ALLOCATED(temp_pointer%cvalue)) THEN
                  CALL scan_str2str(char2str(temp_pointer%cvalue), 256, tmp_string, lng)
                  qz_imp_f=tmp_string
               ENDIF
               DEALLOCATE(tmp_string)
            ENDIF
	 CASE ("si_exp")
            lng=NION
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2str(char2str(temp_pointer%cvalue), 256, si_exp_f, lng)
         CASE ("si_imp")
            lng=NION
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2str(char2str(temp_pointer%cvalue), 256, si_imp_f, lng)
	 CASE ("sz_exp")
            IF(nzimps> 0) THEN
               ALLOCATE(tmp_string(NZIMPS)) ; lng=NZIMPS
               IF (ALLOCATED(temp_pointer%cvalue)) THEN
                  CALL scan_str2str(char2str(temp_pointer%cvalue), 256, tmp_string, lng)
                  sz_exp_f=tmp_string
               ENDIF
               DEALLOCATE(tmp_string)
            ENDIF
	  CASE ("sz_imp")
            IF(nzimps > 0) THEN
               ALLOCATE(tmp_string(NZIMPS)) ; lng=NZIMPS
               IF (ALLOCATED(temp_pointer%cvalue)) THEN
                  CALL scan_str2str(char2str(temp_pointer%cvalue), 256, tmp_string, lng)
                  sz_imp_f=tmp_string
               ENDIF
               DEALLOCATE(tmp_string)
            ENDIF  
         CASE ("ui_exp")
            lng=NION
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2str(char2str(temp_pointer%cvalue), 256, ui_exp_f, lng)
         CASE ("ui_imp")
            lng=NION
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2str(char2str(temp_pointer%cvalue), 256, ui_imp_f, lng)

!--   coreimpur parameters
         CASE ("coreimpur")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("coreimpur_ext")
            IF (ALLOCATED(temp_pointer%cvalue)) THEN
               coreimpur_external = char2str(temp_pointer%cvalue)
               WRITE(*,*) '<<',TRIM(coreimpur_external),'>>'
               external_coreimpur = coreimpur_external /= ''
            ENDIF
         CASE ("nz")
            IF(nzimps > 0) THEN
               ALLOCATE(tmp_string(nzimps)) ; lng=nzimps
               IF (ALLOCATED(temp_pointer%cvalue)) THEN
                  CALL scan_str2str(char2str(temp_pointer%cvalue), 256, tmp_string, lng)
                  imp_nz_f=tmp_string
               ENDIF
               DEALLOCATE(tmp_string)
            ENDIF

!--  default
       CASE default
          WRITE(*, *) 'ERROR: invalid parameter', cname
          return_status = 1
          EXIT
         END SELECT
         DO
            IF (ASSOCIATED(temp_pointer%sibling)) THEN
               temp_pointer => temp_pointer%sibling
               EXIT
            END IF
            IF (ASSOCIATED(temp_pointer%parent, parameter_list%first )) &
                 EXIT outer
            IF (ASSOCIATED(temp_pointer%parent)) THEN
               temp_pointer => temp_pointer%parent
            ELSE
               WRITE(*, *) 'ERROR: broken list.'
               RETURN
            END IF
         END DO
      END DO outer
      
      !-- destroy tree
      CALL destroy_xml_tree(parameter_list)
      
      RETURN

    END SUBROUTINE assign_code_parameters

    SUBROUTINE parse_external(EXTERNAL, shot, run, user, machine, ual_version)
      CHARACTER*(*) EXTERNAL, user, machine, ual_version
      INTEGER shot, run

      INTEGER i1, i2, c, slash

! format of external is
! user/machine/ual_version/shot/run OR shot/run
! fields can be empty

      shot = 0
      run  = 0
      CALL getenv('USER', user)
      CALL getenv('DATAVERSION', ual_version)
      IF(ual_version.EQ.'') THEN
         ual_version = '4.08b'
      ENDIF
      CALL getenv('TOKAMAKNAME', machine)
      IF(machine.EQ.'') THEN
         machine = 'test'
      ENDIF

      i1=1
      i2=LEN_TRIM(EXTERNAL)
      slash=INDEX(EXTERNAL(i1:i2),'/')
      c=0
      DO WHILE (slash.NE.0)
         c=c+1
         i1=i1+slash+1
         IF(i1.LE.i2) THEN
            slash=INDEX(EXTERNAL(i1:i2),'/')
         ELSE
            slash=0
         ENDIF
      ENDDO
      
      IF(c.EQ.1) THEN
         i1=1
         slash=INDEX(EXTERNAL(i1:i2),'/')
         IF(slash.GT.1) THEN
            READ(EXTERNAL(i1:i1+slash-2),*) shot
         ELSE
            WRITE(*,*) 'shot not specified'
            STOP
         ENDIF
         i1=i1+slash
         IF(i1.LE.i2) THEN
            READ(EXTERNAL(i1:i2),*) run
         ELSE
            WRITE(*,*) 'run not specified'
            STOP
         ENDIF
      ELSE IF(c.EQ.4) THEN
         i1=1
         slash=INDEX(EXTERNAL(i1:i2),'/')
         IF(slash.GT.1) THEN
            user = EXTERNAL(i1:i1+slash-2)
         ENDIF
         i1=i1+slash
            
         slash=INDEX(EXTERNAL(i1:i2),'/')
         IF(slash.GT.1) THEN
            machine = EXTERNAL(i1:i1+slash-2)
         ENDIF
         i1=i1+slash

         slash=INDEX(EXTERNAL(i1:i2),'/')
         IF(slash.GT.1) THEN
            ual_version = EXTERNAL(i1:i1+slash-2)
         ENDIF
         i1=i1+slash

         slash=INDEX(EXTERNAL(i1:i2),'/')
         IF(slash.GT.1) THEN
            READ(EXTERNAL(i1:i1+slash-1),*) shot
         ELSE
            WRITE(*,*) 'shot not specified'
            STOP
         ENDIF
         i1=i1+slash

         IF(i1.LE.i2) THEN
            READ(EXTERNAL(i1:i2),*) run
         ELSE
            WRITE(*,*) 'run not specified'
            STOP
         ENDIF
      ELSE
         WRITE(*,*) 'Could not parse ', TRIM(EXTERNAL)
         STOP
      ENDIF

    END SUBROUTINE parse_external


END PROGRAM PREPARE_INPUT_CPOS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!  This subroutine calculates integral of function Y(X)*X
!  from X=0
SUBROUTINE INTEGRAL(N,X,Y,INTYX)
  
  USE itm_types
  
  IMPLICIT NONE
  
  INTEGER :: N                                          ! number of radial points (input)
  INTEGER :: I
  
  REAL (R8) :: X(N), &                                  ! argument array (input)
       Y(N), &                                  ! function array (input)
       INTYX(N)                                  ! function integral array (output)
  
  INTYX(1)=Y(1)*X(1)**2/2.e0_R8
  DO I=2,N
     INTYX(I)=INTYX(I-1)+(Y(I-1)*X(I-1)+Y(I)*X(I))*(X(I)-X(I-1))/2.e0_R8
  END DO
  
  RETURN
END SUBROUTINE INTEGRAL
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!  This subroutine calculates integral of a function y(x)
!  from X=0
SUBROUTINE INTEGRAL_VALUE(N,X,Y,INTY)
  
  USE itm_types
  
  IMPLICIT NONE
  
  INTEGER :: N                                          ! number of radial points (input)
  INTEGER :: I
  
  REAL (R8) :: X(N), &                                  ! argument array (input)
       Y(N), &                                  ! function array (input)
       INTY(N)                                  ! function integral array (output)
  
  
  DO i=2,N
     INTY(I)=INTY(I-1)+(Y(I)+Y(I-1))*(X(I)-X(I-1))/2.E0_R8
  ENDDO
  
  RETURN
END SUBROUTINE INTEGRAL_VALUE
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






