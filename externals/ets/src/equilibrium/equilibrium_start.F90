! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> EQUILIBRIUM_START
!>
!> This routine checks for the consistency of
!> profiles (psi, q, jparallel) and equilibrium
!>
!> \author D. Coster
!>
!> \version "$Id: equilibrium_start.F90 1604 2014-10-06 12:19:41Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
MODULE EQUILIBRIUM_START

CONTAINS



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

  SUBROUTINE START_PROFILES_CONSISTENCY                               &
       (PROF_FLAG, J0_FLAG, Q0_FLAG, EXT_EQUIL,                       &
        COREPROF_IN, EQUILIBRIUM_IN, TOROIDFIELD_IN, COREPROF_OUT, EQUILIBRIUM_OUT) 

!-------------------------------------------------------!
!     This routine checks for the consistency of        !
!     profiles (psi, q, jparallel) and equilibrium      !
!                                                       !
!     information received in: COREPROF_IN              !
!                              EQUILIBRIUM_IN.          !
!                              TOROIDFIELD_IN.          !
!                              EQUILIBRIUM_IN.          !
!                                                       !
!     information saved in:    COREPROF_OUT             !
!                              EQUILIBRIUM_OUT.         !
!                                                       !
!     controling parameter:    PROF_FLAG                !
!                              EXT_EQUIL                !
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
    USE DEALLOCATE_STRUCTURES

    USE EQUILIBRIUM_INPUT
    USE ETS_WRAPPER_EMEQ_E3M
    USE ETS_WRAPPER_BDSEQ
#ifdef GOT_HELENA
    USE ETS_WRAPPER_HELENA
#endif
#ifdef GOT_CHEASE
  USE ETS_WRAPPER_CHEASE
#endif
 
    USE ALLOCATE_DEALLOCATE
    USE ITM_CONSTANTS
    USE EQUILIBRIUM_WORK

    IMPLICIT NONE

    INTEGER                           :: ifail

! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_IN(:)   !input CPO with geometry quantities from previous time
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_OUT(:)  !output CPO with geometry quantities from previous iteration
    TYPE (TYPE_TOROIDFIELD), POINTER  :: TOROIDFIELD_IN(:)   !toroidal field, major radius, total current
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_IN(:)      !input CPO with plasma profiles   
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_ITER(:)    !iterration CPO with plasma profiles   
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_OUT(:)     !output CPO with plasma profiles   
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_NEW(:)     !output CPO with plasma profiles   


! +++ Dimensions:
    INTEGER                           :: PROF_FLAG, FLAG     !determines primary profile
    INTEGER                           :: J0_FLAG             !flag for negative current, if J0_FLAG>0 negative current will be cut off
    INTEGER                           :: Q0_FLAG             !Flag for positive dq/drho: 0-allowed, >0-cut off
    INTEGER                           :: EXT_EQUIL           !determines equilibrium module connected
    INTEGER                           :: INDEX


! +++ Local parameters:
    REAL (R8)                         :: ERR_PSI, ERR_Q , ERR_JPAR
    REAL (R8)                         :: CONV

    FLAG  = 3                                                ! primary iterrations on current
    INDEX = 0                                                ! Starting from parabolic profiles


!DPC
    COREPROF_IN(1)%rho_tor = COREPROF_IN(1)%rho_tor  &
         / COREPROF_IN(1)%rho_tor(SIZE(COREPROF_IN(1)%rho_tor,  DIM=1))  &
         * EQUILIBRIUM_IN(1)%profiles_1d%rho_tor(SIZE (EQUILIBRIUM_IN(1)%profiles_1d%psi, DIM=1))
!CPD 

    ALLOCATE                 (COREPROF_ITER(1))
    ALLOCATE                 (COREPROF_NEW(1))
    CALL COPY_CPO            (COREPROF_IN(1), COREPROF_ITER(1))
    CALL COPY_CPO            (COREPROF_IN(1), COREPROF_NEW(1))


! +++ Replace starting profiles with parabolas:
    CALL PARABOLIC_PROF      (COREPROF_IN,  EQUILIBRIUM_IN, &
                              COREPROF_OUT, EQUILIBRIUM_OUT)



! +++ Provide consistent input for equilibrium:
 7  CALL EQUIL_INPUT         (COREPROF_OUT, TOROIDFIELD_IN, EQUILIBRIUM_OUT, EQUILIBRIUM_IN)



! +++ Call the selected equilibrium:
    IF     (EXT_EQUIL.EQ.1) THEN
       CALL BDSEQ_WRAPPER    (EQUILIBRIUM_IN, EQUILIBRIUM_OUT)
    ELSE IF(EXT_EQUIL.EQ.2) THEN 
       CALL EMEQ_E3M_WRAPPER (EQUILIBRIUM_IN, EQUILIBRIUM_OUT)
    ELSE IF(EXT_EQUIL.EQ.3) THEN
#ifdef GOT_HELENA
       CALL DEALLOCATE_CPO     (EQUILIBRIUM_OUT)
       CALL HELENA_WRAPPER     (EQUILIBRIUM_IN, EQUILIBRIUM_OUT)
#else
       WRITE(*,*) 'No HELENA available at compile time'
       STOP 'No HELENA'
#endif
    ELSE IF (ext_equil.EQ.4) THEN
#ifdef GOT_CHEASE
       CALL DEALLOCATE_CPO     (EQUILIBRIUM_OUT)
       CALL CHEASE_WRAPPER     (EQUILIBRIUM_IN, EQUILIBRIUM_OUT)
#else
       WRITE(*,*) 'No CHEASE available at compile time'
       STOP 'No CHEASE'
#endif
    END IF

!DPC
    COREPROF_OUT(1)%rho_tor = COREPROF_OUT(1)%rho_tor  &
         / COREPROF_OUT(1)%rho_tor(SIZE(COREPROF_OUT(1)%rho_tor,  DIM=1))  &
         * EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor(SIZE (EQUILIBRIUM_OUT(1)%profiles_1d%psi, DIM=1))
!CPD 



! +++ Readjust profiles:
    CALL READJUST_PROFILES            &
       (3, Q0_FLAG, COREPROF_OUT, EQUILIBRIUM_OUT, COREPROF_IN)



! +++ Check for negative currents and total current:
    CALL CORRECT_CURRENT_PROF  (1, COREPROF_IN, EQUILIBRIUM_OUT, COREPROF_OUT) 



! +++ Check for convergence of profiles :
    ERR_PSI          = MAXVAL(ABS(1.0_R8 - ABS(COREPROF_ITER(1)%psi%value(2:) / COREPROF_OUT(1)%psi%value(2:))))
    ERR_Q            = MAXVAL(ABS(1.0_R8 - ABS(COREPROF_ITER(1)%profiles1d%q%value(2:) / COREPROF_OUT(1)%profiles1d%q%value(2:))))
    ERR_JPAR         = MAXVAL(ABS(1.0_R8 - ABS(COREPROF_ITER(1)%profiles1d%jtot%value(2:) / COREPROF_OUT(1)%profiles1d%jtot%value(2:))))

    CONV             = MAX(ERR_PSI, ERR_Q , ERR_JPAR)

    write(*,'(a,1p,4(1x,g12.5))') 'START_PROFILES_CONSISTENCY convergence (inner): ', CONV, ERR_PSI, ERR_Q, ERR_JPAR

! +++ Fill the iterration CPO with readjusted profiles:
    CALL COPY_CPO            (COREPROF_OUT(1), COREPROF_ITER(1))



    IF (CONV.GT.0.001_R8) GOTO 7



! +++ Insert experimental profiles:
    CALL READJUST_PROFILES   (PROF_FLAG, Q0_FLAG, COREPROF_NEW, EQUILIBRIUM_OUT, COREPROF_OUT) 

 

! +++ Check for convergence of profiles :
    ERR_PSI          = MAXVAL(ABS(1.0_R8 - ABS(COREPROF_OUT(1)%psi%value(2:) / COREPROF_NEW(1)%psi%value(2:))))
    ERR_Q            = MAXVAL(ABS(1.0_R8 - ABS(COREPROF_OUT(1)%profiles1d%q%value(2:) / COREPROF_NEW(1)%profiles1d%q%value(2:))))
    ERR_JPAR         = MAXVAL(ABS(1.0_R8 - ABS(COREPROF_OUT(1)%profiles1d%jtot%value(2:) / COREPROF_NEW(1)%profiles1d%jtot%value(2:))))

    CONV             = MAX(ERR_PSI, ERR_Q , ERR_JPAR)

    write(*,'(a,1p,4(1x,g12.5))') 'START_PROFILES_CONSISTENCY convergence (outer): ', CONV, ERR_PSI, ERR_Q, ERR_JPAR
    
    CALL COPY_CPO         (COREPROF_OUT(1), COREPROF_NEW(1))

    IF (CONV.GT.0.001_R8) GOTO 7




! +++ Deallocate CPOs:
    CALL DEALLOCATE_CPO  (COREPROF_ITER)        
    CALL DEALLOCATE_CPO  (COREPROF_NEW)        



    RETURN


  END SUBROUTINE START_PROFILES_CONSISTENCY

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   








END MODULE EQUILIBRIUM_START




