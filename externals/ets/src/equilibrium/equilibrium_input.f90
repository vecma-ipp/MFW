! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> EQUILIBRIUM_INPUT
!>
!> This routine provides consistent input for the 
!> equilibrium solver (EQUILIBRIUM_OUT) from the
!> information saved in COREPROF_IN and EQUILIBRIUM_IN.
!>
!> \author D.Kalupin
!>
!> \version "$Id: equilibrium_input.f90 1754 2016-05-27 09:36:33Z past $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
MODULE EQUILIBRIUM_INPUT
  
CONTAINS
  


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

  SUBROUTINE EQUIL_INPUT                               &
       (COREPROF_IN, TOROIDFIELD_IN, EQUILIBRIUM_IN, EQUILIBRIUM_OUT) 

!-------------------------------------------------------!
!     This routine provides consistent input for the    !
!     equilibrium solver (EQUILIBRIUM_OUT) from the     !
!     information saved in COREPROF_IN and              !
!     EQUILIBRIUM_IN.                                   !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     created for V&V between ETS and     !
!                   ASTRA                               !
!                                                       !
!-------------------------------------------------------!


    USE EUITM_SCHEMAS
    USE ITM_CONSTANTS
    USE COPY_STRUCTURES
    USE DEALLOCATE_STRUCTURES
    USE ETS_PLASMA
    
    IMPLICIT NONE
    
! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_IN(:)   !input CPO with geometry quantities from previous time
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_OUT(:)  !output CPO with geometry quantities from previous iteration
    TYPE (TYPE_TOROIDFIELD), POINTER  :: TOROIDFIELD_IN(:)   !toroidal field, major radius, total current
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_IN(:)      !input CPO with plasma profiles   
    
! +++ Local derived types:
    TYPE (DIAGNOSTIC)                 :: DIAG                !diagnostic output 


! +++ Dimensions:
    INTEGER                           :: NRHO, IRHO          !number and index of radial points     
    INTEGER                           :: NION, IION          !number and index of ion components
    INTEGER                           :: NEQ,  IEQ

    INTEGER                           :: IEQ1, IEQ2
    REAL (R8)                         :: sign
    REAL (R8), ALLOCATABLE            :: PRESSURE(:)
    REAL (R8), DIMENSION (:), POINTER :: rho_tor, psi, rho_vol

    REAL (R8), ALLOCATABLE            :: j_edge_mask(:)
    REAL (R8)                         :: j_edge_pos, j_edge_pow
    CHARACTER*3                       :: char1, char2
    CHARACTER*1000                    :: OUTPUT_DIAG
    INTEGER                           :: NLINE, i
    


! +++ Nullify diagnostic output:
    DIAG%ERROR_MESSAGE    = " "
    DIAG%IERR             = 0


! +++ Set dimensions and allocate parameters:
    NRHO   =  SIZE (COREPROF_IN(1)%rho_tor,            DIM=1)
    NION   =  SIZE (COREPROF_IN(1)%ni%value,           DIM=2)
    NEQ    =  SIZE (EQUILIBRIUM_IN(1)%profiles_1d%psi, DIM=1)
    


! +++ Fill the output EQUILIBRIUM CPO:
    IF      (ASSOCIATED(EQUILIBRIUM_OUT))    &
    CALL DEALLOCATE_CPO(EQUILIBRIUM_OUT)
    ALLOCATE           (EQUILIBRIUM_OUT(1))
    CALL COPY_CPO      (EQUILIBRIUM_IN(1), EQUILIBRIUM_OUT(1))

    IF (.NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor))      ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor(NEQ))
    IF (.NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%rho_vol))      ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%rho_vol(NEQ))
    IF (.NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%psi))          ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%psi(NEQ))
    IF (.NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%q))            ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%q(NEQ))
    IF (.NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%jphi))         ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%jphi(NEQ))
    IF (.NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%jparallel))    ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%jparallel(NEQ))
    IF (.NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%pressure))     ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%pressure(NEQ))
    IF (.NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%phi))          ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%phi(NEQ))
 
    ALLOCATE (PRESSURE(NRHO))

! dpc
    ALLOCATE(j_edge_mask(neq))
    j_edge_pos = 0.02_R8
    j_edge_pow = 8.0_R8
!    write(*,*) '   j_edge_pos = ', j_edge_pos
!    write(*,*) '   j_edge_pow = ', j_edge_pow
    j_edge_mask = EXP(-(j_edge_pos/(1.0_R8+1.0e-12_R8-EQUILIBRIUM_IN(1)%profiles_1d%rho_tor/EQUILIBRIUM_IN(1)%profiles_1d%rho_tor(neq)))**j_edge_pow)
! cpd
   

! +++ Fill the output EQUILIBRIUMvector%profiles_1d:
    EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor                 = EQUILIBRIUM_IN(1)%profiles_1d%rho_tor 

    IF (COREPROF_IN(1)%rho_tor(NRHO).LT.EQUILIBRIUM_IN(1)%profiles_1d%rho_tor(NEQ))                              &
        COREPROF_IN(1)%rho_tor(NRHO)                       = EQUILIBRIUM_IN(1)%profiles_1d%rho_tor(NEQ)

    IF ( ASSOCIATED(EQUILIBRIUM_IN(1)%profiles_1d%rho_vol )) THEN
        EQUILIBRIUM_OUT(1)%profiles_1d%rho_vol                 = EQUILIBRIUM_IN(1)%profiles_1d%rho_vol 
    ELSE IF (ASSOCIATED( EQUILIBRIUM_IN(1)%profiles_1d%volume)) THEN
        EQUILIBRIUM_OUT(1)%profiles_1d%rho_vol                 =    SQRT(EQUILIBRIUM_IN(1)%profiles_1d%volume     & 
                                                                    /EQUILIBRIUM_IN(1)%profiles_1d%volume(NEQ))             
        DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"No rho_vol supplied; calculated from volume "
        DIAG%IERR             = DIAG%IERR + 1
    ELSE
        EQUILIBRIUM_OUT(1)%profiles_1d%rho_vol                 = ITM_INVALID_FLOAT 
        DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"No rho_vol supplied; set to itm_unvalid "
        DIAG%IERR             = DIAG%IERR + 1
    END IF


    IF (ASSOCIATED (COREPROF_IN(1)%psi%value))               THEN
       CALL l3interp(COREPROF_IN(1)%psi%value,               COREPROF_IN(1)%rho_tor,                  NRHO,       &
                    EQUILIBRIUM_OUT(1)%profiles_1d%psi,      EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor,  NEQ)
    ELSE
       EQUILIBRIUM_OUT(1)%profiles_1d%psi                  = 0.0_R8
       DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"No Psi supplied; "
       DIAG%IERR             = DIAG%IERR + 1
    END IF

    EQUILIBRIUM_OUT(1)%global_param%psi_ax                 = EQUILIBRIUM_OUT(1)%profiles_1d%psi(1)
    EQUILIBRIUM_OUT(1)%global_param%psi_bound              = EQUILIBRIUM_OUT(1)%profiles_1d%psi(NEQ)


    IF (ASSOCIATED (COREPROF_IN(1)%profiles1d%q%value))      THEN
       CALL l3interp(COREPROF_IN(1)%profiles1d%q%value,      COREPROF_IN(1)%rho_tor,                  NRHO,       &
                    EQUILIBRIUM_OUT(1)%profiles_1d%q,        EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor,  NEQ)
    ELSE
       EQUILIBRIUM_OUT(1)%profiles_1d%q                    =  0.0_R8
       DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"No q supplied; "
       DIAG%IERR             = DIAG%IERR + 1
    END IF



    IF (ASSOCIATED (COREPROF_IN(1)%profiles1d%jtot%value))   THEN
      CALL l3interp(COREPROF_IN(1)%profiles1d%jtot%value,    COREPROF_IN(1)%rho_tor,                  NRHO,       &
                    EQUILIBRIUM_OUT(1)%profiles_1d%jparallel,EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor,  NEQ)
! dpc
      IF(ALLOCATED(j_edge_mask)) THEN
!         write(*,*) 'Smashing edge jparallel'
!         EQUILIBRIUM_OUT(1)%profiles_1d%jparallel = EQUILIBRIUM_OUT(1)%profiles_1d%jparallel * j_edge_mask
      ENDIF
! cpd    
    ELSE
       EQUILIBRIUM_OUT(1)%profiles_1d%jparallel            =  0.0_R8
       DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"No Jpar supplied; "
       DIAG%IERR             = DIAG%IERR + 1
    END IF



    IF (ASSOCIATED (COREPROF_IN(1)%profiles1d%jphi%value))   THEN
      CALL l3interp(COREPROF_IN(1)%profiles1d%jphi%value,    COREPROF_IN(1)%rho_tor,                  NRHO,       &
                    EQUILIBRIUM_OUT(1)%profiles_1d%jphi,EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor,  NEQ)
! dpc
      IF(ALLOCATED(j_edge_mask)) THEN
!         write(*,*) 'Smashing edge jphi'
!         EQUILIBRIUM_OUT(1)%profiles_1d%jphi = EQUILIBRIUM_OUT(1)%profiles_1d%jphi * j_edge_mask
      ENDIF
! cpd    
    ELSE
       EQUILIBRIUM_OUT(1)%profiles_1d%jphi                 =  0.0_R8
       DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"No jphi supplied; "
       DIAG%IERR             = DIAG%IERR + 1
    END IF



    PRESSURE             =  COREPROF_IN(1)%ne%value * COREPROF_IN(1)%te%value * itm_ev

    RHO_LOOP1: DO IRHO=1,NRHO
       ION_LOOP1: DO IION=1,NION
          PRESSURE(IRHO) =  PRESSURE(IRHO) +                                                                       &
                            COREPROF_IN(1)%ni%value(IRHO,IION) * COREPROF_IN(1)%ti%value(IRHO,IION) * itm_ev  
       END DO ION_LOOP1
    END DO RHO_LOOP1


    CALL l3interp(PRESSURE,                                   COREPROF_IN(1)%rho_tor,                  NRHO,       &
                  EQUILIBRIUM_OUT(1)%profiles_1d%pressure,    EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor,  NEQ)





! dpc 2011-08-10  Force psi to be monotonic

    rho_tor => EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor
    psi => EQUILIBRIUM_OUT(1)%profiles_1d%psi

    IF(psi(1) .LT. psi(NEQ)) THEN
       sign = +1.0_R8
    ELSE IF(psi(1) .GT. psi(NEQ)) THEN
       sign = -1.0_R8
    ELSE
       DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"PSI end points equal; "
       DIAG%IERR             = - 1
       GOTO 112
    ENDIF
    IEQ = 1
    DO WHILE(IEQ .LT. NEQ)
       IF(psi(IEQ)*sign .LT. psi(IEQ+1)*sign) THEN
          IEQ = IEQ + 1
       ELSE  ! non-monotonic
          IEQ1 = IEQ+1
          IF(IEQ1 .LT. NEQ) THEN
             DO WHILE(IEQ1 .LT. NEQ .AND. &
                  psi(IEQ)*sign .GE. psi(IEQ1)*sign)
                IEQ1 = IEQ1 + 1
             ENDDO
          ENDIF
          IF(IEQ1 .LT. NEQ) THEN  ! problem lies between IEQ and IEQ1
             WRITE (char1,'(i3)') IEQ
             WRITE (char2,'(i3)') IEQ1
             DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"fixed problem in PSI between "//TRIM(char1)//" and"//TRIM(char2)//"; " 
             DIAG%IERR             =  DIAG%IERR + 1
             DO IEQ2 = IEQ+1, IEQ1-1
                psi(IEQ2) = psi(IEQ) + &
                     (rho_tor(IEQ2) - rho_tor(IEQ))/(rho_tor(IEQ1) - rho_tor(IEQ)) * &
                     (psi(IEQ1) - psi(IEQ))
             ENDDO
             IEQ = IEQ1
          ELSE  ! problem lies to the left of IEQ
             IEQ1 = IEQ
             DO WHILE(IEQ1 .GT. 1 .AND. &
                  psi(IEQ1)*sign .GE. psi(NEQ)*sign)
                IEQ1 = IEQ1 - 1
             ENDDO
             IF(IEQ1 .GT. 1) THEN  ! problem lies between IEQ1 and NEQ
                WRITE (char1,'(i3)') IEQ
                WRITE (char2,'(i3)') NEQ
                DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"fixed problem in PSI between "//TRIM(char1)//" and"//TRIM(char2)//"; " 
                DIAG%IERR             =  DIAG%IERR + 1
                DO IEQ2 = IEQ1+1, NEQ-1
                   psi(IEQ2) = psi(IEQ1) + &
                        (rho_tor(IEQ2) - rho_tor(IEQ1))/(rho_tor(NEQ) - rho_tor(IEQ1)) * &
                        (psi(NEQ) - psi(IEQ1))
                ENDDO
                IEQ = NEQ
             ELSE
                DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"this should not happen! "
                DIAG%IERR             = - 1
                GOTO 112
             ENDIF
          ENDIF
       ENDIF
    END DO

! Force rho_vol to be monotonic

    rho_tor => EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor
    rho_vol => EQUILIBRIUM_OUT(1)%profiles_1d%rho_vol

    IF(rho_vol(1) .LT. rho_vol(NEQ)) THEN
       sign = +1.0_R8
    ELSE IF(rho_vol(1) .GT. rho_vol(NEQ)) THEN
       sign = -1.0_R8
    ELSE
       DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"RHO_VOL end points equal; "
       DIAG%IERR             =  DIAG%IERR + 1
    ENDIF
    IEQ = 1
    DO WHILE(IEQ .LT. NEQ)
       IF(rho_vol(IEQ)*sign .LT. rho_vol(IEQ+1)*sign) THEN
          IEQ = IEQ + 1
       ELSE  ! non-monotonic
          IEQ1 = IEQ+1
          IF(IEQ1 .LT. NEQ) THEN
             DO WHILE(IEQ1 .LT. NEQ .AND. &
                  rho_vol(IEQ)*sign .GE. rho_vol(IEQ1)*sign)
                IEQ1 = IEQ1 + 1
             ENDDO
          ENDIF
          IF(IEQ1 .LT. NEQ) THEN  ! problem lies between IEQ and IEQ1
             WRITE (char1,'(i3)') IEQ
             WRITE (char2,'(i3)') IEQ1
             DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"fixed problem in RHO_VOL between "//TRIM(char1)//" and"//TRIM(char2)//"; " 
             DIAG%IERR             =  DIAG%IERR + 1
              DO IEQ2 = IEQ+1, IEQ1-1
                rho_vol(IEQ2) = rho_vol(IEQ) + &
                     (rho_tor(IEQ2) - rho_tor(IEQ))/(rho_tor(IEQ1) - rho_tor(IEQ)) * &
                     (rho_vol(IEQ1) - rho_vol(IEQ))
             ENDDO
             IEQ = IEQ1
          ELSE  ! problem lies to the left of IEQ
             IEQ1 = IEQ
             DO WHILE(IEQ1 .GT. 1 .AND. &
                  rho_vol(IEQ1)*sign .GE. rho_vol(NEQ)*sign)
                IEQ1 = IEQ1 - 1
             ENDDO
             IF(IEQ1 .GT. 1) THEN  ! problem lies between IEQ1 and NEQ
                WRITE (char1,'(i3)') IEQ
                WRITE (char2,'(i3)') NEQ
                DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"fixed problem in RHO_VOL between "//TRIM(char1)//" and"//TRIM(char2)//"; " 
                DIAG%IERR             =  DIAG%IERR + 1
                 DO IEQ2 = IEQ1+1, NEQ-1
                   rho_vol(IEQ2) = rho_vol(IEQ1) + &
                        (rho_tor(IEQ2) - rho_tor(IEQ1))/(rho_tor(NEQ) - rho_tor(IEQ1)) * &
                        (rho_vol(NEQ) - rho_vol(IEQ1))
                ENDDO
                IEQ = NEQ
             ELSE
                DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"this should not happen! "
                DIAG%IERR             = - 1
                GOTO 112
             ENDIF
          ENDIF
       ENDIF
    END DO

! end of dpc addition      
       
    EQUILIBRIUM_OUT(1)%global_param%psi_ax     = EQUILIBRIUM_OUT(1)%profiles_1d%psi(1)
    EQUILIBRIUM_OUT(1)%global_param%psi_bound  = EQUILIBRIUM_OUT(1)%profiles_1d%psi(NEQ)
    EQUILIBRIUM_OUT(1)%global_param%volume     = EQUILIBRIUM_OUT(1)%profiles_1d%volume(NEQ)

    DEALLOCATE (PRESSURE)
! dpc
    IF(ALLOCATED(j_edge_mask)) DEALLOCATE(j_edge_mask)
! cpd
! +++ CODEPARAM:
112 OUTPUT_DIAG = "EQUILIBRIUM_INPUT: "
    OUTPUT_DIAG = OUTPUT_DIAG//TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))
    NLINE = FLOOR(LEN_TRIM(ADJUSTL(OUTPUT_DIAG))/132.001)+1
    IF(ASSOCIATED(EQUILIBRIUM_OUT(1)%codeparam%codename))    &
       DEALLOCATE(EQUILIBRIUM_OUT(1)%codeparam%codename)
    IF(ASSOCIATED(EQUILIBRIUM_OUT(1)%codeparam%codeversion)) &
       DEALLOCATE(EQUILIBRIUM_OUT(1)%codeparam%codeversion)
    IF(ASSOCIATED(EQUILIBRIUM_OUT(1)%codeparam%output_diag)) &
       DEALLOCATE(EQUILIBRIUM_OUT(1)%codeparam%output_diag)
    ALLOCATE     (EQUILIBRIUM_OUT(1)%codeparam%codename(1))
    ALLOCATE     (EQUILIBRIUM_OUT(1)%codeparam%codeversion(1))
    ALLOCATE     (EQUILIBRIUM_OUT(1)%codeparam%output_diag(NLINE))

    EQUILIBRIUM_OUT(1)%codeparam%codename            = 'EQILIBRIUM_INPUT'
    EQUILIBRIUM_OUT(1)%codeparam%codeversion         = 'EQILIBRIUM_INPUT_4.10b.10'
    EQUILIBRIUM_OUT(1)%codeparam%output_flag         =  DIAG%IERR
    DO i = 1,NLINE
    EQUILIBRIUM_OUT(1)%codeparam%output_diag(i)      =  OUTPUT_DIAG(((i-1)*132+1):(i*132))
    END DO


    RETURN

  END SUBROUTINE EQUIL_INPUT

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

  SUBROUTINE ETS_INPUT                               &
       (EQUILIBRIUM_IN, COREPROF_OUT) 

!-------------------------------------------------------!
!     This routine updates the q-profile in the COREPROF!                                   !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     created for V&V between ETS and     !
!                   ASTRA                               !
!                                                       !
!-------------------------------------------------------!


    USE EUITM_SCHEMAS
    USE itm_constants
    
    IMPLICIT NONE

! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_IN(:)   !input CPO with geometry quantities from previous time
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_OUT(:)     !output CPO with plasma profiles   


! +++ Dimensions:
    INTEGER                           :: NRHO         !number of coreprof radial points     
    INTEGER                           :: NEQ          !number and equilibrium radial points

! +++ Internal parameters(0-D parameters):

!     Input
!!    REAL (R8),            ALLOCATABLE :: QSF(:)

! +++ Set dimensions and allocate parameters:
    NEQ    =  SIZE (EQUILIBRIUM_IN(1)%profiles_1d%rho_tor, DIM=1)
    NRHO   =  SIZE (COREPROF_OUT(1)%rho_tor, DIM=1)

!!    ALLOCATE  (QSF(NRHO))

!!    QSF    =  EQUILIBRIUM_IN(1)%profiles_1d%q

! +++ Fill the output COREPROF_OUT CPO:
!!    COREPROF_OUT(1)%profiles1d%q%value = QSF

    CALL l3interp(EQUILIBRIUM_IN(1)%profiles_1d%q,  &
         EQUILIBRIUM_IN(1)%profiles_1d%rho_tor / EQUILIBRIUM_IN(1)%profiles_1d%rho_tor(NEQ),  &
         NEQ,  &
         COREPROF_OUT(1)%profiles1d%q%value,  &
         COREPROF_OUT(1)%rho_tor / COREPROF_OUT(1)%rho_tor(NRHO),  &
         NRHO)

    
!!    DEALLOCATE  (QSF)


    RETURN

  END SUBROUTINE ETS_INPUT

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

END MODULE EQUILIBRIUM_INPUT
