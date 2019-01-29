MODULE EQUILIBRIUM_WORK

CONTAINS


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

  SUBROUTINE  PARABOLIC_PROF  (COREPROF_IN,  EQUILIBRIUM_IN, &
                               COREPROF_OUT, EQUILIBRIUM_OUT)

!-------------------------------------------------------!
!     This routine puts parabolic                       !
!     profiles of psi, q and jparallel                  !
!                                                       !
!     information received in: COREPROF_IN                 !
!                                                       !
!     information saved in:    COREPROF                 !
!                                                       !
!                                                       !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Comments:     created for V&V between ETS, JETTO  !
!                   and ASTRA                           !
!                                                       !
!-------------------------------------------------------!


    USE EUITM_SCHEMAS
    USE EUITM_ROUTINES
    USE COPY_STRUCTURES
    USE ITM_CONSTANTS

    IMPLICIT NONE

    INTEGER                           :: ifail

! +++ CPO derived types:
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_IN(:)         !input/output CPO with plasma profiles   
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_OUT(:)        !input/output CPO with plasma profiles   
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_IN(:)      !input CPO with geometry quantities from previous time
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_OUT(:)     !input CPO with geometry quantities from previous time


! +++ Dimensions:
    INTEGER                           :: NRHO, IRHO          !number and index of radial points     
    INTEGER                           :: NION     


! +++ Local variables:
    REAL (R8),            ALLOCATABLE :: RHO(:)
    REAL (R8),            ALLOCATABLE :: RHOMOD(:)

    REAL (R8),            ALLOCATABLE :: PSI(:)
    REAL (R8),            ALLOCATABLE :: QSF(:) 
    REAL (R8),            ALLOCATABLE :: Jpar(:)
    REAL (R8),            ALLOCATABLE :: PR(:)

    REAL (R8),            ALLOCATABLE :: NE(:)
    REAL (R8),            ALLOCATABLE :: TE(:) 
    REAL (R8),            ALLOCATABLE :: NI(:,:)
    REAL (R8),            ALLOCATABLE :: TI(:,:)

    REAL (R8),            ALLOCATABLE :: FUN(:) 
    REAL (R8),            ALLOCATABLE :: INTJpar(:)
    REAL (R8),            ALLOCATABLE :: INTFUN(:)

    REAL (R8),            ALLOCATABLE :: FDIA(:) 
    REAL (R8),            ALLOCATABLE :: GM2(:)
    REAL (R8),            ALLOCATABLE :: VPRIME(:)

    REAL (R8)                         :: CURR,   CURR_TOTAL
    REAL (R8)                         :: Jpar0,  JparB 
    REAL (R8)                         :: PR0,    PRB

    REAL (R8)                         :: R0,     B0

    REAL (R8)                         :: BND_VALUE(3)
    INTEGER                           :: BND_TYPE

    INTEGER                           :: NX
    INTEGER                           :: NPSI

    REAL (R8)                         :: s_ip, s_bt, s_q, s_psi, s_jpar


! +++ Allocation:
    ALLOCATE             (COREPROF_OUT(1))
    ALLOCATE             (EQUILIBRIUM_OUT(1))
    


! +++ Fill the iterration CPO with parameters obtained on input:
    CALL COPY_CPO        (COREPROF_IN(1),    COREPROF_OUT(1))
    CALL COPY_CPO        (EQUILIBRIUM_IN(1), EQUILIBRIUM_OUT(1))



! +++ Set dimensions and allocate parameters:
    NRHO              =  SIZE (COREPROF_IN(1)%rho_tor,  DIM=1)
    NION              =  SIZE (COREPROF_IN(1)%ni%value, DIM=2)
    NPSI              =  SIZE (EQUILIBRIUM_IN(1)%profiles_1d%psi, DIM=1)

    s_ip             = SIGN(1.0_R8, coreprof_in(1)%globalparam%current_tot)
    s_bt             = SIGN(1.0_R8, coreprof_in(1)%toroid_field%b0)
    s_q              = SIGN(1.0_R8, coreprof_in(1)%profiles1d%q%value(1))
    s_psi            = SIGN(1.0_R8, coreprof_in(1)%psi%value(nrho)-coreprof_in(1)%psi%value(1))
    s_jpar           = SIGN(1.0_R8, coreprof_in(1)%profiles1d%jtot%value(1))

    WRITE(*,*) 's_ip, s_bt, s_q, s_psi, s_jpar = ', s_ip, s_bt, s_q, s_psi, s_jpar

! +++ Allocation of local variables:
    ALLOCATE             (RHO(NRHO)     )
    ALLOCATE             (RHOMOD(NRHO)  )

    ALLOCATE             (PSI(NRHO)     )
    ALLOCATE             (QSF(NRHO)     )
    ALLOCATE             (Jpar(NRHO)    )
    ALLOCATE             (PR(NRHO)      )

    ALLOCATE             (NE(NRHO)      )
    ALLOCATE             (TE(NRHO)      )
    ALLOCATE             (NI(NRHO,NION) )
    ALLOCATE             (TI(NRHO,NION) )

    ALLOCATE             (FUN(NRHO)     )
    ALLOCATE             (INTJpar(NRHO) )
    ALLOCATE             (INTFUN(NRHO))

    ALLOCATE             (FDIA(NRHO)    )
    ALLOCATE             (GM2(NRHO)     )
    ALLOCATE             (VPRIME(NRHO)  )



! +++ Local variables:
    RHO               =  COREPROF_IN(1)%rho_tor
    CURR_TOTAL        =  COREPROF_IN(1)%globalparam%current_tot

    R0                =  COREPROF_IN(1)%toroid_field%r0
    B0                =  COREPROF_IN(1)%toroid_field%b0

    CALL L3DERIV        (EQUILIBRIUM_IN(1)%profiles_1d%volume,   EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                         VPRIME,                                 RHO,                                    NRHO)
    CALL L3INTERP       (EQUILIBRIUM_IN(1)%profiles_1d%F_dia,    EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                         FDIA,                                   RHO,                                    NRHO)
    CALL L3INTERP       (EQUILIBRIUM_IN(1)%profiles_1d%gm2,      EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                         GM2,                                    RHO,                                    NRHO)
    RHOMOD            =  (1.E0_R8-RHO**2 / RHO(NRHO)**2)

    NE                =  COREPROF_IN(1)%ne%value
    TE                =  COREPROF_IN(1)%te%value
    NI                =  COREPROF_IN(1)%ni%value
    TI                =  COREPROF_IN(1)%ti%value
    


! +++ Central and edge values for parabolic profiles:
    Jpar0             =  1.E6_R8 *  s_jpar                              !Jpar    [A/m^2]
    JparB             =  0.e0_R8



! +++ Profiles of  PSI, Q and CURRENT:
7   Jpar              =  (JPAR0-JPARB) * RHOMOD + JPARB                 !Jpar    [A/m^2]
     

!   parabolic profile for Jpar:
    Jpar              =  (Jpar0-JparB) * RHOMOD + JparB


!   Recalculated profile of QSF:
    FUN            =  Jpar * ITM_MU0 * VPRIME / FDIA**2 / RHO
    FUN(1)         =  FUN(2)
    CALL INTEGRAL     (NRHO, RHO, FUN, INTFUN)
    QSF            =  - VPRIME * GM2 / FDIA * RHO / INTFUN
    QSF(1)         =  QSF(2)


!   Recalculated profile of PSI:
    FUN            =  2.E0_R8 * itm_pi * B0 / QSF
    FUN(1)         =  FUN(2)
    CALL INTEGRAL     (NRHO, RHO, FUN,  PSI)



!+++ Renormalize current:
    FUN            =  VPRIME * Jpar / 2.0E0_R8 / ITM_PI * B0 / FDIA**2
    CALL INTEGRAL_VALUE (NRHO, RHO, FUN, INTJpar)
    CURR           =  INTJpar(NRHO) * FDIA(NRHO)

    IF (DABS(1.0_R8 - CURR/CURR_TOTAL) .GE. 1.0e-5_R8) THEN

       JPAR0          =  JPAR0 * CURR_TOTAL / CURR
       GOTO 7
    END IF



    WRITE (*,*) 'Parabolic current set to', CURR


! +++ Parabolic pressure profile:
    PR                =  (NE * TE + SUM(NI * TI, DIM=2)) * ITM_EV       !Pr      [Pa]

    PR0               =  PR(1)
    PRB               =  0.e0_R8

    PR                =  (PR(1)-PR(NRHO))   * RHOMOD + PR(NRHO)



! +++ Output profiles in CPO
    COREPROF_OUT(1)%psi%value                    = PSI
    COREPROF_OUT(1)%profiles1d%q%value           = QSF
    COREPROF_OUT(1)%profiles1d%jtot%value        = Jpar
    COREPROF_OUT(1)%profiles1d%pr_th%value       = PR
    COREPROF_OUT(1)%profiles1d%pr_perp%value     = PR
    COREPROF_OUT(1)%profiles1d%pr_parallel%value = PR


! +++ Deallocation of local variables:
    DEALLOCATE           (RHO)
    DEALLOCATE           (RHOMOD)

    DEALLOCATE           (PSI)
    DEALLOCATE           (QSF)
    DEALLOCATE           (Jpar)
    DEALLOCATE           (PR)

    DEALLOCATE           (NE)
    DEALLOCATE           (TE)
    DEALLOCATE           (NI)
    DEALLOCATE           (TI)

    DEALLOCATE           (FUN)
    DEALLOCATE           (INTJpar)
    DEALLOCATE           (INTFUN)

    DEALLOCATE           (FDIA)
    DEALLOCATE           (GM2)
    DEALLOCATE           (VPRIME)



    RETURN

  END SUBROUTINE PARABOLIC_PROF

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

  SUBROUTINE READJUST_PROFILES            &
       (PROF_FLAG, Q0_FLAG, COREPROF_IN, EQUILIBRIUM_IN, COREPROF_OUT) 

!-------------------------------------------------------!
!     This routine readjusts consistently               !
!     profiles of psi, q and jparallel                  !
!                                                       !
!     information received in: COREPROF                 !
!                              EQUILIBRIUM.             !
!                                                       !
!     information saved in:    COREPROF                 !
!                                                       !
!     controling parameter:    PROF_FLAG                !
!                                                       !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Comments:     created for V&V between ETS, JETTO  !
!                   and ASTRA                           !
!                                                       !
!-------------------------------------------------------!


    USE EUITM_SCHEMAS
    USE ITM_CONSTANTS
    USE COPY_STRUCTURES

    IMPLICIT NONE

    INTEGER                           :: ifail

! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_IN(:)   !input CPO with geometry quantities from previous time
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_IN(:)      !input/output CPO with plasma profiles   
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_OUT(:)     !input/output CPO with plasma profiles   


! +++ Dimensions:
    INTEGER                           :: NRHO, IRHO          !number and index of radial points     

    INTEGER                           :: PROF_FLAG, FLAG     !determines primary profile
    INTEGER                           :: Q0_FLAG             !Flag for positive dq/drho: 0-allowed, >0-cut off
    INTEGER                           :: NPSI




! +++ Local variables:
    REAL (R8),            ALLOCATABLE :: RHO(:)
    REAL (R8),            ALLOCATABLE :: RHOMOD(:)
    REAL (R8),            ALLOCATABLE :: PSI(:)
    REAL (R8),            ALLOCATABLE :: DPSI(:)
    REAL (R8),            ALLOCATABLE :: QSF(:) 
    REAL (R8),            ALLOCATABLE :: Jpar(:)

    REAL (R8),            ALLOCATABLE :: FUN(:) 
    REAL (R8),            ALLOCATABLE :: DFUN(:)
    REAL (R8),            ALLOCATABLE :: INTFUN(:)

    REAL (R8),            ALLOCATABLE :: FDIA(:) 
    REAL (R8),            ALLOCATABLE :: GM2(:)
    REAL (R8),            ALLOCATABLE :: VPRIME(:)

    REAL (R8)                         :: CURR_TOTAL
    REAL (R8)                         :: PSI0,   PSIB
    REAL (R8)                         :: QSF0,   QSFB
    REAL (R8)                         :: Jpar0,  JparB 
    REAL (R8)                         :: PR0,    PRB

    REAL (R8)                         :: R0,     B0
    REAL (R8)                         :: RHO_MIN, QSF_MIN

    REAL (R8)                         :: s_ip, s_bt, s_q, s_psi, s_jpar


! +++ Set dimensions and parameters:
    NRHO             =  SIZE (COREPROF_IN(1)%rho_tor, DIM=1)
    NPSI             =  SIZE (EQUILIBRIUM_IN(1)%profiles_1d%psi, DIM=1)
    FLAG             =  PROF_FLAG

    s_ip             = SIGN(1.0_R8, coreprof_in(1)%globalparam%current_tot)
    s_bt             = SIGN(1.0_R8, coreprof_in(1)%toroid_field%b0)
    s_q              = SIGN(1.0_R8, coreprof_in(1)%profiles1d%q%value(1))
    s_psi            = SIGN(1.0_R8, coreprof_in(1)%psi%value(nrho)-coreprof_in(1)%psi%value(1))
    s_jpar           = SIGN(1.0_R8, coreprof_in(1)%profiles1d%jtot%value(1))

    WRITE(*,*) 's_ip, s_bt, s_q, s_psi, s_jpar = ', s_ip, s_bt, s_q, s_psi, s_jpar

! +++ Allocation of local variables:
    ALLOCATE             (RHO(NRHO))
    ALLOCATE             (RHOMOD(NRHO))

    ALLOCATE             (PSI(NRHO))
    ALLOCATE             (DPSI(NRHO))
    ALLOCATE             (QSF(NRHO))
    ALLOCATE             (Jpar(NRHO))

    ALLOCATE             (FUN(NRHO))
    ALLOCATE             (DFUN(NRHO))
    ALLOCATE             (INTFUN(NRHO))

    ALLOCATE             (FDIA(NRHO))
    ALLOCATE             (GM2(NRHO))
    ALLOCATE             (VPRIME(NRHO))

    ALLOCATE             (COREPROF_OUT(1))



! +++ Local variables:
    RHO               =  COREPROF_IN(1)%rho_tor
    PSI               =  COREPROF_IN(1)%psi%value
    QSF               =  COREPROF_IN(1)%profiles1d%q%value
    Jpar              =  COREPROF_IN(1)%profiles1d%jtot%value

    R0                =  COREPROF_IN(1)%toroid_field%r0
    B0                =  COREPROF_IN(1)%toroid_field%b0

    CALL L3DERIV        (EQUILIBRIUM_IN(1)%profiles_1d%volume,   EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                         VPRIME,                                 RHO,                                    NRHO)
    CALL L3INTERP       (EQUILIBRIUM_IN(1)%profiles_1d%F_dia,    EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                         FDIA,                                   RHO,                                    NRHO)
    CALL L3INTERP       (EQUILIBRIUM_IN(1)%profiles_1d%gm2,      EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                         GM2,                                    RHO,                                    NRHO)



! +++ Recalculate PSI, Q and CURRENT profiles
    IF     (FLAG.EQ.1) THEN            !profile flag = 1: PSI is primary quantity
!      Recalculated profile of QSF:
       CALL DERIVN       (NRHO, RHO, PSI, DPSI)
       QSF            =  2.E0_R8 * itm_pi * B0 * RHO / DPSI
       QSF(1)         =  QSF(2)



!      Recalculated profile of Jpar:
       FUN            =  DPSI * R0 * B0 * VPRIME * GM2 / FDIA
       CALL DERIVN       (NRHO, RHO, FUN, DFUN)
       Jpar           =  - DFUN / ITM_MU0 / R0 / 2.E0_R8 / itm_pi / VPRIME / B0**2 * FDIA**2 
       Jpar(1)        =  Jpar(2)


       IF (QSF(1)*s_q.GT.MINVAL(QSF*s_q).AND.Q0_FLAG.GT.0.0D0)  THEN
          RHO_MIN        =  RHO(1)
          QSF_MIN        =  QSF(1)
          DO IRHO = 2,NRHO
             IF (QSF(IRHO)*s_q.LT.QSF_MIN*s_q) THEN
                RHO_MIN  =  RHO(IRHO)
                QSF_MIN  =  QSF(IRHO)
             END IF
          END DO
          DO IRHO = 1,NRHO
             IF (RHO(IRHO).LT.RHO_MIN) THEN
                QSF(IRHO) = QSF_MIN
             END IF
          END DO
          FUN             =  VPRIME * GM2 / FDIA * RHO / QSF
          CALL DERIVN       (NRHO, RHO, FUN, DFUN)
          Jpar            =  DFUN / ITM_MU0 / VPRIME * FDIA**2
          Jpar(1)         =  Jpar(2)
       END IF



    ELSE IF(FLAG.EQ.2) THEN            !profile flag = 2: Q is primary quantity 
!      Recalculated profile of PSI:
       FUN            =  2.E0_R8 * itm_pi * B0 / QSF
       FUN(1)         =  FUN(2)
       CALL INTEGRAL     (NRHO, RHO, FUN,  PSI)
       
!      Recalculated profile of Jpar:
       FUN            =  VPRIME * GM2 / FDIA * RHO / QSF
       CALL DERIVN       (NRHO, RHO, FUN, DFUN)
       Jpar           =  DFUN / ITM_MU0 / VPRIME * FDIA**2
       Jpar(1)        =  Jpar(2)



    ELSE IF(FLAG.EQ.3) THEN            !profile flag = 3: current is primary quantity
!      Recalculated profile of QSF:
       FUN            =  Jpar * ITM_MU0 * VPRIME / FDIA**2 / RHO
       FUN(1)         =  FUN(2)
       CALL INTEGRAL     (NRHO, RHO, FUN, INTFUN)
       QSF            =  - VPRIME * GM2 / FDIA * RHO / INTFUN
       QSF(1)         =  QSF(2)

!      Recalculated profile of PSI:
       FUN            =  2.E0_R8 * itm_pi * B0 / QSF
       FUN(1)         =  FUN(2)
       CALL INTEGRAL     (NRHO, RHO, FUN,  PSI)
       

    END IF




! +++ Fill the iterration CPO with readjusted profiles:
    CALL COPY_CPO            (COREPROF_IN(1), COREPROF_OUT(1))



! +++ Update profiles
    COREPROF_OUT(1)%psi%value               =  PSI
    COREPROF_OUT(1)%profiles1d%q%value      =  QSF
    COREPROF_OUT(1)%profiles1d%jtot%value   =  Jpar



! +++ Deallocation of local variables:
    DEALLOCATE           (RHO)
    DEALLOCATE           (RHOMOD)

    DEALLOCATE           (PSI)
    DEALLOCATE           (DPSI)
    DEALLOCATE           (QSF)
    DEALLOCATE           (Jpar)

    DEALLOCATE           (FUN)
    DEALLOCATE           (DFUN)
    DEALLOCATE           (INTFUN)

    DEALLOCATE           (FDIA)
    DEALLOCATE           (GM2)
    DEALLOCATE           (VPRIME)



    RETURN

  END SUBROUTINE READJUST_PROFILES

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

  SUBROUTINE CORRECT_CURRENT_PROF  (J0_FLAG, COREPROF_IN, EQUILIBRIUM, COREPROF_OUT) 
!-------------------------------------------------------!
!     This routine corrects profiles                    !
!     of psi, q and jparallel such, that                !
!     there is no negative current.                     !
!                                                       !
!     information received in: COREPROF_IN              !
!                                                       !
!     information saved in:    COREPROF_OUT             !
!                                                       !
!     controling parameter:    PROF_FLAG                !
!                                                       !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Comments:     created for V&V between ETS, JETTO  !
!                   and ASTRA                           !
!                                                       !
!-------------------------------------------------------!


    USE EUITM_SCHEMAS
    USE EUITM_ROUTINES
    USE ITM_CONSTANTS
    USE COPY_STRUCTURES

    IMPLICIT NONE

    INTEGER                           :: ifail

! +++ CPO derived types:
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_IN(:)      !input/output CPO with plasma profiles   
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_OUT(:)     !output/output CPO with plasma profiles   
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM(:)      !input CPO with geometry quantities from previous time



! +++ Dimensions:
    INTEGER                           :: NRHO, IRHO          !number and index of radial points     
    INTEGER                           :: NPSI, IPSI          !number and index of radial points     
    INTEGER                           :: J0_FLAG             !flag for negative current, if J0_FLAG>0 negative current will be cut off



! +++ Local variables:
    REAL (R8),            ALLOCATABLE :: RHO(:)
    REAL (R8),            ALLOCATABLE :: PSI(:)
    REAL (R8),            ALLOCATABLE :: QSF(:) 
    REAL (R8),            ALLOCATABLE :: Jpar(:)
    REAL (R8),            ALLOCATABLE :: INTJpar(:)

    REAL (R8),            ALLOCATABLE :: FUN(:) 
    REAL (R8),            ALLOCATABLE :: INTFUN(:)

    REAL (R8),            ALLOCATABLE :: VPRIME(:)
    REAL (R8),            ALLOCATABLE :: FDIA(:) 
    REAL (R8),            ALLOCATABLE :: GM2(:)

    REAL (R8)                         :: CURR_TOTAL, CURR
    REAL (R8)                         :: R0,    B0

    REAL (R8),            ALLOCATABLE :: Jpar_BG(:)
    REAL (R8)                         :: CURR_BG, BG

    REAL (R8)                         :: s_ip, s_bt, s_q, s_psi, s_jpar

! +++ Set dimensions and allocate parameters:
    NRHO             =  SIZE (COREPROF_IN(1)%rho_tor, DIM=1)
    NPSI             =  SIZE (EQUILIBRIUM(1)%profiles_1d%psi, DIM=1)

    s_ip             =  SIGN (1.0_R8, coreprof_in(1)%globalparam%current_tot)
    s_bt             =  SIGN (1.0_R8, coreprof_in(1)%toroid_field%b0)
    s_q              =  SIGN (1.0_R8, coreprof_in(1)%profiles1d%q%value(1))
    s_psi            =  SIGN (1.0_R8, coreprof_in(1)%psi%value(nrho)-coreprof_in(1)%psi%value(1))
    s_jpar           =  SIGN (1.0_R8, coreprof_in(1)%profiles1d%jtot%value(1))

!    WRITE(*,*) 's_ip, s_bt, s_q, s_psi, s_jpar = ', s_ip, s_bt, s_q, s_psi, s_jpar

! +++ Allocation of local variables:
    ALLOCATE             (RHO(NRHO))

    ALLOCATE             (PSI(NRHO))
    ALLOCATE             (QSF(NRHO))
    ALLOCATE             (Jpar(NRHO))
    ALLOCATE             (INTJpar(NRHO))

    ALLOCATE             (FUN(NRHO))
    ALLOCATE             (INTFUN(NRHO))

    ALLOCATE             (VPRIME(NRHO))
    ALLOCATE             (FDIA(NRHO))
    ALLOCATE             (GM2(NRHO))

    ALLOCATE             (Jpar_BG(NRHO))

    ALLOCATE             (COREPROF_OUT(1))



! +++ Local variables:
    RHO               =  COREPROF_IN(1)%rho_tor
    PSI               =  COREPROF_IN(1)%psi%value
    QSF               =  COREPROF_IN(1)%profiles1d%q%value
    Jpar              =  COREPROF_IN(1)%profiles1d%jtot%value

    CURR_TOTAL        =  COREPROF_IN(1)%globalparam%current_tot
    R0                =  COREPROF_IN(1)%toroid_field%r0
    B0                =  COREPROF_IN(1)%toroid_field%b0

    CALL L3DERIV        (EQUILIBRIUM(1)%profiles_1d%volume,   EQUILIBRIUM(1)%profiles_1d%rho_tor,  NPSI,    &
                         VPRIME,                              RHO,                                 NRHO)
    CALL L3INTERP       (EQUILIBRIUM(1)%profiles_1d%F_dia,    EQUILIBRIUM(1)%profiles_1d%rho_tor,  NPSI,    &
                         FDIA,                                RHO,                                 NRHO)
    CALL L3INTERP       (EQUILIBRIUM(1)%profiles_1d%gm2,      EQUILIBRIUM(1)%profiles_1d%rho_tor,  NPSI,    &
                         GM2,                                 RHO,                                 NRHO)


! +++ Determination of negative current area:
    IF (J0_FLAG.GT.0)  THEN
       DO IRHO = 1,NRHO
          IF (Jpar(IRHO)*s_jpar.LE.0.0E0_R8)  Jpar(IRHO)  =  1.0E-6_R8*s_jpar
       END DO
    END IF


!+++ Renormalize current:
    FUN            =  VPRIME * Jpar / 2.0E0_R8 / ITM_PI * B0 / FDIA**2
    CALL INTEGRAL_VALUE  (NRHO, RHO, FUN, INTJpar)
    CURR           =  INTJpar(NRHO) * FDIA(NRHO)


!+++ Difference in total current between required and real:
    CURR_BG        =  CURR_TOTAL - CURR

    IF (CURR_BG*s_ip.LE.0.0_R8) THEN
       DO IRHO = 1,NRHO
          IF (INTJpar(IRHO) * FDIA(IRHO)*s_ip.GE.CURR_TOTAL*s_ip)  Jpar(IRHO)  =  1.0E-6_R8*s_jpar
       END DO

    ELSE IF (CURR_BG*s_ip.GT.0.0_R8) THEN
       BG          =  1.0_R8*s_jpar
       Jpar_BG     =  BG * (1.0_R8 - 0.5_R8/(1.0_R8+((RHO(NRHO)-RHO)/(RHO(NRHO)*0.05_R8))**3))
 7     FUN         =  VPRIME * MAX(Jpar*s_jpar,Jpar_BG*s_jpar)*s_jpar / 2.0E0_R8 / ITM_PI * B0 / FDIA**2
       CALL INTEGRAL_VALUE  (NRHO, RHO, FUN, INTJpar)
       CURR        =  INTJpar(NRHO) * FDIA(NRHO)

       IF (ABS(1.0_R8-CURR/CURR_TOTAL).GE.1.E-4_R8) THEN
          BG       =  BG * CURR_TOTAL / CURR
          Jpar_BG  =  Jpar_BG * CURR_TOTAL / CURR
          GOTO 7
       END IF

       DO IRHO = 1,NRHO
          IF (Jpar_BG(IRHO)*s_jpar.GT.Jpar(IRHO)*s_jpar)  THEN
            Jpar(IRHO:NRHO)  =  Jpar_BG (IRHO:NRHO)
            GOTO 10
          END IF
       END DO
!       Jpar        =  MAX(Jpar,Jpar_BG) 

    END IF







!   Recalculated profile of QSF:
 10 FUN            =  Jpar * ITM_MU0 * VPRIME / FDIA**2 / RHO
    FUN(1)         =  FUN(2)
    CALL INTEGRAL     (NRHO, RHO, FUN, INTFUN)
    QSF            =  - VPRIME * GM2 / FDIA * RHO / INTFUN
    QSF(1)         =  QSF(2)


!   Recalculated profile of PSI:
    FUN            =  2.E0_R8 * itm_pi * B0 / QSF
    FUN(1)         =  FUN(2)
    CALL INTEGRAL     (NRHO, RHO, FUN,  PSI)




! +++ Fill the iterration CPO with readjusted profiles:
    CALL COPY_CPO            (COREPROF_IN(1), COREPROF_OUT(1))



! +++ Update profiles
    COREPROF_OUT(1)%psi%value               =  PSI
    COREPROF_OUT(1)%profiles1d%q%value      =  QSF
    COREPROF_OUT(1)%profiles1d%jtot%value   =  Jpar



! +++ Deallocation of local variables:
    DEALLOCATE           (RHO)

    DEALLOCATE           (PSI)
    DEALLOCATE           (QSF)
    DEALLOCATE           (Jpar)
    DEALLOCATE           (INTJpar)

    DEALLOCATE           (FUN)
    DEALLOCATE           (INTFUN)

    DEALLOCATE           (VPRIME)
    DEALLOCATE           (FDIA)
    DEALLOCATE           (GM2)

    DEALLOCATE           (Jpar_BG)



    RETURN


   END SUBROUTINE CORRECT_CURRENT_PROF
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

  SUBROUTINE NEGATIVE_CURRENT  (J0_FLAG, COREPROF_IN, EQUILIBRIUM, COREPROF_OUT) 
!-------------------------------------------------------!
!     This routine corrects profiles                    !
!     of psi, q and jparallel such, that                !
!     there is no negative current.                     !
!                                                       !
!     information received in: COREPROF_IN              !
!                                                       !
!     information saved in:    COREPROF_OUT             !
!                                                       !
!     controling parameter:    PROF_FLAG                !
!                                                       !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Comments:     created for V&V between ETS, JETTO  !
!                   and ASTRA                           !
!                                                       !
!-------------------------------------------------------!


    USE EUITM_SCHEMAS
    USE EUITM_ROUTINES
    USE ITM_CONSTANTS
    USE COPY_STRUCTURES

    IMPLICIT NONE

    INTEGER                           :: ifail

! +++ CPO derived types:
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_IN(:)      !input/output CPO with plasma profiles   
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_OUT(:)     !output/output CPO with plasma profiles   
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM(:)      !input CPO with geometry quantities from previous time



! +++ Dimensions:
    INTEGER                           :: NRHO, IRHO          !number and index of radial points     
    INTEGER                           :: NPSI    
    INTEGER                           :: J0_FLAG             !flag for negative current, if J0_FLAG>0 negative current will be cut off
    INTEGER                           :: IZERO               !inner position of negative current region



! +++ Local variables:
    REAL (R8),            ALLOCATABLE :: RHO(:)
    REAL (R8),            ALLOCATABLE :: PSI(:)
    REAL (R8),            ALLOCATABLE :: QSF(:) 
    REAL (R8),            ALLOCATABLE :: Jpar(:)
    REAL (R8),            ALLOCATABLE :: INTJpar(:)

    REAL (R8),            ALLOCATABLE :: FUN(:) 
    REAL (R8),            ALLOCATABLE :: INTFUN(:)

    REAL (R8),            ALLOCATABLE :: VPRIME(:)
    REAL (R8),            ALLOCATABLE :: FDIA(:) 
    REAL (R8),            ALLOCATABLE :: GM2(:)

    REAL (R8)                         :: CURR_TOTAL, CURR
    REAL (R8)                         :: R0,    B0

    REAL (R8)                         :: s_ip, s_bt, s_q, s_psi, s_jpar



! +++ Set dimensions and allocate parameters:
    NRHO             =  SIZE (COREPROF_IN(1)%rho_tor, DIM=1)
    NPSI             =  SIZE (EQUILIBRIUM(1)%profiles_1d%psi, DIM=1)

    s_ip             = SIGN(1.0_R8, coreprof_in(1)%globalparam%current_tot)
    s_bt             = SIGN(1.0_R8, coreprof_in(1)%toroid_field%b0)
    s_q              = SIGN(1.0_R8, coreprof_in(1)%profiles1d%q%value(1))
    s_psi            = SIGN(1.0_R8, coreprof_in(1)%psi%value(nrho)-coreprof_in(1)%psi%value(1))
    s_jpar           = SIGN(1.0_R8, coreprof_in(1)%profiles1d%jtot%value(1))

    WRITE(*,*) 's_ip, s_bt, s_q, s_psi, s_jpar = ', s_ip, s_bt, s_q, s_psi, s_jpar

! +++ Allocation of local variables:
    ALLOCATE             (RHO(NRHO))

    ALLOCATE             (PSI(NRHO))
    ALLOCATE             (QSF(NRHO))
    ALLOCATE             (Jpar(NRHO))
    ALLOCATE             (INTJpar(NRHO))

    ALLOCATE             (FUN(NRHO))
    ALLOCATE             (INTFUN(NRHO))

    ALLOCATE             (VPRIME(NRHO))
    ALLOCATE             (FDIA(NRHO))
    ALLOCATE             (GM2(NRHO))


    ALLOCATE             (COREPROF_OUT(1))



! +++ Local variables:
    RHO               =  COREPROF_IN(1)%rho_tor
    PSI               =  COREPROF_IN(1)%psi%value
    QSF               =  COREPROF_IN(1)%profiles1d%q%value
    Jpar              =  COREPROF_IN(1)%profiles1d%jtot%value
    CURR_TOTAL        =  COREPROF_IN(1)%globalparam%current_tot

    R0                =  COREPROF_IN(1)%toroid_field%r0
    B0                =  COREPROF_IN(1)%toroid_field%b0


    CALL L3DERIV        (EQUILIBRIUM(1)%profiles_1d%volume,   EQUILIBRIUM(1)%profiles_1d%rho_tor,  NPSI,    &
                         VPRIME,                              RHO,                                    NRHO)
    CALL L3INTERP       (EQUILIBRIUM(1)%profiles_1d%F_dia,    EQUILIBRIUM(1)%profiles_1d%rho_tor,  NPSI,    &
                         FDIA,                                RHO,                                    NRHO)
    CALL L3INTERP       (EQUILIBRIUM(1)%profiles_1d%gm2,      EQUILIBRIUM(1)%profiles_1d%rho_tor,  NPSI,    &
                         GM2,                                 RHO,                                    NRHO)


! +++ Determination of negative current area:
    DO IRHO = 1,NRHO
       IF (Jpar(IRHO)*s_jpar.LE.0.0E0_R8)  Jpar(IRHO)  =  1.0E-6_R8*s_jpar
    END DO


!+++ Renormalize current:
    FUN            =  VPRIME * Jpar / 2.0E0_R8 / ITM_PI * B0 / FDIA**2
    CALL INTEGRAL_VALUE  (NRHO, RHO, FUN, INTJpar)
    CURR           =  INTJpar(NRHO) * FDIA(NRHO)

    DO IRHO = 1,NRHO
       IF (INTJpar(IRHO) * FDIA(IRHO)*s_jpar.GT.CURR_TOTAL*s_jpar)  Jpar(IRHO)  =  1.0E-6_R8*s_jpar
    END DO


! +++ Fill the iterration CPO with readjusted profiles:
    CALL COPY_CPO            (COREPROF_IN(1), COREPROF_OUT(1))



! +++ Update profiles
    IF (J0_FLAG.GT.0)  COREPROF_OUT(1)%profiles1d%jtot%value   =  Jpar




! +++ Deallocation of local variables:
    DEALLOCATE           (RHO)

    DEALLOCATE           (PSI)
    DEALLOCATE           (QSF)
    DEALLOCATE           (Jpar)
    DEALLOCATE           (INTJpar)

    DEALLOCATE           (FUN)
    DEALLOCATE           (INTFUN)

    DEALLOCATE           (VPRIME)
    DEALLOCATE           (FDIA)
    DEALLOCATE           (GM2)



    RETURN


   END SUBROUTINE NEGATIVE_CURRENT
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This subroutine calculates integral of function Y(X)*X from X=0 until X=X(N) 
!>
!> These subroutines have been extracted from RITM code
!> and consist of derivation and integration routines
!>
!> \author D.Kalupin, R.Stankiewicz
!>
!> \version "$Id: equilibrium_start.f90 1437 2012-10-24 15:48:13Z kalupin $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
SUBROUTINE INTEGRAL(N,X,Y,INTY)
!-------------------------------------------------------!
!  This subroutine calculates integral of function      !
!  Y(X)*X from X=0 until X=X(N)                         !
!-------------------------------------------------------!

  USE itm_types

  IMPLICIT NONE

  INTEGER :: N                                          ! number of radial points (input)
  INTEGER :: I

  REAL (R8) :: X(N), &                                  ! argument array (input)
       Y(N), &                                  ! function array (input)
       INTY(N)                                  ! function integral array (output)

  INTY(1)=Y(1)*X(1)**2/2.e0_R8
  DO I=2,N
     INTY(I)=INTY(I-1)+(Y(I-1)*X(I-1)+Y(I)*X(I))*(X(I)-X(I-1))/2.e0_R8
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


  INTY(1)=Y(1)*X(1)/2.0_R8
  DO i=2,N
     INTY(I)=INTY(I-1)+(Y(I)+Y(I-1))*(X(I)-X(I-1))/2.0_R8
  ENDDO
 RETURN
  END SUBROUTINE INTEGRAL_VALUE
  ! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  ! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> These subroutines calculate first and second derivatives, DY1 and DY2, of function Y respect to argument X  
!>
!> These subroutines have been extracted from RITM code
!> and consist of derivation and integration routines
!>
!> \author D.Kalupin, R.Stankiewicz
!>
!> \version "$Id: equilibrium_start.f90 1437 2012-10-24 15:48:13Z kalupin $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
SUBROUTINE DERIVN(N,X,Y,DY1)

!-------------------------------------------------------!
!       These subroutines calculate first and second    !
!       derivatives, DY1 and DY2, of function Y respect !
!       to argument X                                   !
!-------------------------------------------------------!

  USE itm_types
  IMPLICIT NONE

  INTEGER :: N                                          ! number of radial points (input)
  INTEGER :: I

  REAL (R8) :: X(N), &                                  ! argument array (input)
       Y(N), &                                  ! function array (input)
       DY1(N)                                   ! function derivative array (output)
  REAL (R8) :: H(N),DY2(N)

  DO I=1,N-1
     H(I)=X(I+1)-X(I)
  END DO

  DO I=2,N-1
     DY1(I)=((Y(I+1)-Y(I))*H(I-1)/H(I)+(Y(I)-Y(I-1))*H(I)/H(I-1)) &
          /(H(I)+H(I-1))
     DY2(I)=2.e0_R8*((Y(I-1)-Y(I))/H(I-1)+(Y(I+1)-Y(I))/H(I)) &
          /(H(I)+H(I-1))
  END DO

  DY1(1)=DY1(2)-DY2(2)*H(1)
  DY1(N)=DY1(N-1)+DY2(N-1)*H(N-1)

  RETURN
END SUBROUTINE DERIVN
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


END MODULE EQUILIBRIUM_WORK
