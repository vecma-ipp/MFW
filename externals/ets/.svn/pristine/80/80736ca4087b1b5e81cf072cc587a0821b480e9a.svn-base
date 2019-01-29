MODULE FILL_CPOS


CONTAINS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE FILLCOREPROF (COREPROF_DB,  COREPROF_GRID,  COREPROF_OUT, INTERPOL) 

! +++ Declaration of variables: 
    USE ITM_TYPES
    USE ITM_CONSTANTS
    USE EUITM_SCHEMAS
    USE ALLOCATE_DEALLOCATE
    USE COPY_STRUCTURES
    USE DEALLOCATE_STRUCTURES
    USE INTERPOLATE_CPO


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
    IMPLICIT NONE

    INTEGER,              PARAMETER  :: NSLICE = 1             !number of CPO ocurancies in the work flow
    INTEGER                          :: NRHO1, NRHO2
    INTEGER                          :: NNUCL
    INTEGER                          :: NION, IION
    INTEGER                          :: NIMP       
    INTEGER,             ALLOCATABLE :: NZIMP(:)
    INTEGER                          :: NNEUT
    INTEGER,             ALLOCATABLE :: NCOMP(:)
    INTEGER,             ALLOCATABLE :: NTYPE(:)

    INTEGER                          :: INTERPOL           !interpolation index "0"-based on RHO_TOR; all other - based on RHO_TOR_NORM  
    INTEGER                          :: IRHO  

! +++ CPO derived types:
    TYPE (TYPE_COREPROF), POINTER    :: COREPROF_GRID(:)   !input CPO with internal ETS parameters 
    TYPE (TYPE_COREPROF), POINTER    :: COREPROF_OUT(:)    !output CPO with profiles uploaded from the data base 
    TYPE (TYPE_COREPROF), POINTER    :: COREPROF_DB(:)     !time independent CPO slice 
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   


! +++ RHO grid:
    NRHO1                 =  SIZE (COREPROF_GRID(1)%rho_tor,         DIM=1)
    NRHO2                 =  SIZE (COREPROF_DB(1)%rho_tor,           DIM=1)


! +++ Allocate output CPO and internal derived types:
    CALL GET_COMP_DIMENSIONS      (COREPROF_GRID(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
    CALL COPY_CPO                 (COREPROF_GRID, COREPROF_OUT)



    IF (INTERPOL.NE.0) &
       COREPROF_DB(1)%rho_tor =    COREPROF_DB(1)%rho_tor            &
                                 / COREPROF_DB(1)%rho_tor(NRHO2)     &
                                 * COREPROF_OUT(1)%rho_tor(NRHO1) 



! +++ Check for Zeff:
    IF (ASSOCIATED (COREPROF_DB(1)%profiles1d%zeff%value)) THEN
      DO IRHO = 1, NRHO2
         IF (COREPROF_DB(1)%profiles1d%zeff%value(IRHO).LT.1.0_R8)        &
             COREPROF_DB(1)%profiles1d%zeff%value(IRHO) =  1.0_R8
      END DO
    ELSE
      WRITE(*,*) '!!!! WARNING: input coreprof CPO'
      WRITE(*,*) '  ZEFF profile not set --> allocated and set to unity'
      ALLOCATE (COREPROF_DB(1)%profiles1d%zeff%value(NRHO2))
      COREPROF_DB(1)%profiles1d%zeff%value(:) =  1.0_R8
    END IF

! +++ Interpolate profiles:
    CALL INTERPOLATE_PROF          (COREPROF_DB(1), COREPROF_OUT(1))




! +++ Check for COCOS13:
    
! for COCOS 13, Ip > 0 ==> Psi decreasing
    IF ( COREPROF_OUT(1)%globalparam%current_tot * &
         (COREPROF_OUT(1)%psi%value(NRHO1) - COREPROF_OUT(1)%psi%value(1)) .GE.0.0_R8 ) THEN
       WRITE(*,*) '!!!! WARNING: input coreprof CPO'
       WRITE(*,*) 'We have a COCOS violation: Ip > 0 ==> Psi decreasing'
       WRITE(*,*) 'Ip = ', COREPROF_OUT(1)%globalparam%current_tot
       WRITE(*,*) 'Psi_a - Psi_0 = ', COREPROF_OUT(1)%psi%value(NRHO1)-COREPROF_OUT(1)%psi%value(1)
!       stop 'COCOS violation'
       WRITE(*,*) '!!!! Psi is reverted to be compliant with COCOS13'
       COREPROF_OUT(1)%psi%value = -1.0_R8 * COREPROF_OUT(1)%psi%value
       WRITE(*,*) ' '
    ENDIF

! for COCOS 13, q has the opposite sign to the sign of Ip*B0
    IF ( COREPROF_OUT(1)%profiles1d%q%value(1) * &
         COREPROF_OUT(1)%globalparam%current_tot * COREPROF_OUT(1)%toroid_field%b0 .GE. 0.0_R8 ) THEN
       WRITE(*,*) '!!!! WARNING: input coreprof CPO'
       WRITE(*,*) 'We have a COCOS violation: Ip*B0*q should be < 0'
       WRITE(*,*) 'Ip = ', COREPROF_OUT(1)%globalparam%current_tot
       WRITE(*,*) 'B0 = ', COREPROF_OUT(1)%toroid_field%b0
       WRITE(*,*) 'q0 = ', COREPROF_OUT(1)%profiles1d%q%value(1)
!       stop 'COCOS violation'
       WRITE(*,*) '!!!! q is reverted to be compliant with COCOS13'
       COREPROF_OUT(1)%profiles1d%q%value = -1.0_R8 * COREPROF_OUT(1)%profiles1d%q%value
       WRITE(*,*) ' '

    ENDIF

! Check the direction of current (should coinside with IP)
    IF ( COREPROF_OUT(1)%globalparam%current_tot * COREPROF_OUT(1)%profiles1d%jtot%value(1) .LE.0.0_R8 ) THEN
       WRITE(*,*) '!!!! WARNING: input coreprof CPO'
       WRITE(*,*) 'We have different sign for IP and Jpar'
       WRITE(*,*) 'Ip_sign = ',   SIGN(1.0_R8, COREPROF_OUT(1)%globalparam%current_tot)
       WRITE(*,*) 'Jpar_sign = ', SIGN(1.0_R8, COREPROF_OUT(1)%profiles1d%jtot%value(1)) 
!       stop 'COCOS violation'
       WRITE(*,*) '!!!! Jpar profile is reverted to be consistent with total current'
       COREPROF_OUT(1)%profiles1d%jtot%value = -1.0_R8 * COREPROF_OUT(1)%profiles1d%jtot%value
       WRITE(*,*) ' '
    ENDIF



! +++ Boundary conditions from input CPO:

    IF (COREPROF_OUT(1)%psi%boundary%type .GE. 6)        THEN
        COREPROF_OUT(1)%psi%boundary%value(1)            =  COREPROF_OUT(1)%psi%value(NRHO1)
        COREPROF_OUT(1)%psi%boundary%value(2)            =  0._R8
        COREPROF_OUT(1)%psi%boundary%value(3)            =  0._R8

        IF (COREPROF_OUT(1)%psi%boundary%type .EQ. 6)    THEN
            COREPROF_OUT(1)%psi%boundary%type            =  1
        ELSE 
            COREPROF_OUT(1)%psi%boundary%type            =  0
        END IF


    END IF

    IF (COREPROF_OUT(1)%ne%boundary%type .GE. 6) THEN
        COREPROF_OUT(1)%ne%boundary%value(1)             =  COREPROF_OUT(1)%ne%value(NRHO1)
        COREPROF_OUT(1)%ne%boundary%value(2)             =  0._R8
        COREPROF_OUT(1)%ne%boundary%value(3)             =  0._R8

        IF (COREPROF_OUT(1)%ne%boundary%type .EQ. 6)    THEN
            COREPROF_OUT(1)%ne%boundary%type             =  1
        ELSE 
            COREPROF_OUT(1)%ne%boundary%type             =  0
        END IF

    END IF

    IF (COREPROF_OUT(1)%te%boundary%type .GE. 6) THEN
        COREPROF_OUT(1)%te%boundary%value(1)             =  COREPROF_OUT(1)%te%value(NRHO1)
        COREPROF_OUT(1)%te%boundary%value(2)             =  0._R8
        COREPROF_OUT(1)%te%boundary%value(3)             =  0._R8

        IF (COREPROF_OUT(1)%te%boundary%type .EQ. 6)    THEN
            COREPROF_OUT(1)%te%boundary%type             =  1
        ELSE 
            COREPROF_OUT(1)%te%boundary%type             =  0
        END IF

    END IF

    DO IION = 1, NION
       IF (COREPROF_OUT(1)%ni%boundary%type(IION) .GE. 6) THEN
           COREPROF_OUT(1)%ni%boundary%value(1,IION)    =  COREPROF_OUT(1)%ni%value(NRHO1,IION)
           COREPROF_OUT(1)%ni%boundary%value(2,IION)    =  0._R8
           COREPROF_OUT(1)%ni%boundary%value(3,IION)    =  0._R8

           IF (COREPROF_OUT(1)%ni%boundary%type(IION) .EQ. 6)    THEN
               COREPROF_OUT(1)%ni%boundary%type(IION)   =  1
           ELSE 
               COREPROF_OUT(1)%ni%boundary%type(IION)   =  0
           END IF

       END IF

       IF (COREPROF_OUT(1)%ti%boundary%type(IION) .GE. 6) THEN
           COREPROF_OUT(1)%ti%boundary%value(1,IION)    =  COREPROF_OUT(1)%ti%value(NRHO1,IION)
           COREPROF_OUT(1)%ti%boundary%value(2,IION)    =  0._R8
           COREPROF_OUT(1)%ti%boundary%value(3,IION)    =  0._R8

           IF (COREPROF_OUT(1)%ti%boundary%type(IION) .EQ. 6)    THEN
               COREPROF_OUT(1)%ti%boundary%type(IION)   =  1
           ELSE 
               COREPROF_OUT(1)%ti%boundary%type(IION)   =  0
           END IF

       END IF

       IF (COREPROF_OUT(1)%vtor%boundary%type(IION) .GE. 6) THEN
           COREPROF_OUT(1)%vtor%boundary%value(1,IION)  =  COREPROF_OUT(1)%vtor%value(NRHO1,IION)
           COREPROF_OUT(1)%vtor%boundary%value(2,IION)  =  0._R8
           COREPROF_OUT(1)%vtor%boundary%value(3,IION)  =  0._R8

           IF (COREPROF_OUT(1)%vtor%boundary%type(IION) .EQ. 6)    THEN
               COREPROF_OUT(1)%vtor%boundary%type(IION) =  1
           ELSE 
               COREPROF_OUT(1)%vtor%boundary%type(IION) =  0
           END IF

       END IF

    END DO



! +++ Initial Pressure:
    COREPROF_OUT(1)%profiles1d%pe%value                 = COREPROF_OUT(1)%te%value * COREPROF_OUT(1)%ne%value * itm_ev
    COREPROF_OUT(1)%profiles1d%pi%value                 = COREPROF_OUT(1)%ti%value * COREPROF_OUT(1)%ni%value * itm_ev
    COREPROF_OUT(1)%profiles1d%pi_tot%value             = SUM(COREPROF_OUT(1)%profiles1d%pi%value, dim=2)
    COREPROF_OUT(1)%profiles1d%pr_th%value              = COREPROF_OUT(1)%profiles1d%pe%value + SUM(COREPROF_OUT(1)%profiles1d%pi%value, dim=2)
    COREPROF_OUT(1)%profiles1d%pr_perp%value            = COREPROF_OUT(1)%profiles1d%pr_th%value


! +++ Deallocate internal derived types:

 10 IF(ALLOCATED(NZIMP)) DEALLOCATE                    (NZIMP)
    IF(ALLOCATED(NCOMP)) DEALLOCATE                    (NCOMP)
    IF(ALLOCATED(NTYPE)) DEALLOCATE                    (NTYPE)


    RETURN 
 

    END SUBROUTINE FILLCOREPROF
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  







! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE FILLCORETRANSP (CORETRANSP_DB, CORETRANSP_GRID, CORETRANSP_OUT, INTERPOL) 


! +++ Declaration of variables: 
    USE EUITM_SCHEMAS
    USE EUITM_ROUTINES
    USE ALLOCATE_DEALLOCATE
    USE COPY_STRUCTURES
    USE DEALLOCATE_STRUCTURES
    USE INTERPOLATE_CPO
    USE ITM_TYPES

    USE ETS_PLASMA


    IMPLICIT NONE

    INTEGER,              PARAMETER  :: NSLICE = 1             !number of CPO ocurancies in the work flow
    INTEGER                          :: NRHO1, NRHO2
    INTEGER                          :: NNUCL
    INTEGER                          :: NION
    INTEGER                          :: NIMP       
    INTEGER,             ALLOCATABLE :: NZIMP(:)
    INTEGER                          :: NNEUT
    INTEGER,             ALLOCATABLE :: NCOMP(:)
    INTEGER,             ALLOCATABLE :: NTYPE(:)

    INTEGER                          :: INTERPOL           !interpolation index "0"-based on RHO_TOR; all other - based on RHO_TOR_NORM  
    INTEGER                          :: IIMP
    INTEGER                          :: IVAL

! +++ CPO derived types:
    TYPE (TYPE_CORETRANSP), POINTER  :: CORETRANSP_GRID(:) !input CPO with internal parameters 
    TYPE (TYPE_CORETRANSP), POINTER  :: CORETRANSP_OUT(:)  !output CPO with profiles uploaded from the data base 
    TYPE (TYPE_CORETRANSP), POINTER  :: CORETRANSP_DB(:)   !time independent CPO slice 
    TYPE (TYPE_CORETRANSP), POINTER  :: CORETRANSP_TMP(:)   





! +++ Allocate and define grid of output CPO:
    NRHO1                  = SIZE (CORETRANSP_GRID(1)%VALUES(1)%rho_tor,            DIM=1)
    CALL GET_COMP_DIMENSIONS      (CORETRANSP_GRID(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
    CALL COPY_CPO                 (CORETRANSP_GRID, CORETRANSP_OUT)


! +++ Check if interpolation can be done:
    IF            (.NOT.ASSOCIATED(CORETRANSP_DB(1)%VALUES))               GOTO 10
    IF            (.NOT.ASSOCIATED(CORETRANSP_DB(1)%VALUES(1)%rho_tor))    GOTO 10
    IF            (MAXVAL(CORETRANSP_DB(1)%VALUES(1)%rho_tor).LE.0.0_R8)   GOTO 10

    NRHO2                  = SIZE (CORETRANSP_DB(1)%VALUES(1)%rho_tor)
    IF (INTERPOL.NE.0)             CORETRANSP_DB(1)%VALUES(1)%rho_tor =          CORETRANSP_DB(1)%VALUES(1)%rho_tor            &
                                                                               / CORETRANSP_DB(1)%VALUES(1)%rho_tor(NRHO2)     &
                                                                               * CORETRANSP_OUT(1)%VALUES(1)%rho_tor(NRHO1) 

! +++ Check for NaN in input shot:
    IF (SIZE(CORETRANSP_DB(1)%VALUES).GT.1) THEN
       ALLOCATE                      (CORETRANSP_TMP(1))
       ALLOCATE                      (CORETRANSP_TMP(1)%VALUES(1))
       CALL COPY_CPO                 (CORETRANSP_DB(1)%VALUES(1), CORETRANSP_TMP(1)%VALUES(1))
       CALL DEALLOCATE_CPO           (CORETRANSP_DB(1)%VALUES)
       ALLOCATE                      (CORETRANSP_DB(1)%VALUES(1))
       CALL COPY_CPO                 (CORETRANSP_TMP(1)%VALUES(1), CORETRANSP_DB(1)%VALUES(1))       
    END IF
      
    CALL CHECK_NANS_IN_CORETRANSP (CORETRANSP_DB)


! +++ Interpolate CPO:
    CALL INTERPOLATE_TRANSP       (CORETRANSP_DB(1), CORETRANSP_OUT(1), 0)



! +++ Deallocate internal derived types:

 10 IF(ALLOCATED(NZIMP)) DEALLOCATE                    (NZIMP)
    IF(ALLOCATED(NCOMP)) DEALLOCATE                    (NCOMP)
    IF(ALLOCATED(NTYPE)) DEALLOCATE                    (NTYPE)



    RETURN 


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
    CONTAINS
      SUBROUTINE CHECK_NANS_IN_CORETRANSP (CORETRANSP)
        IMPLICIT NONE
        INTEGER                          :: IVAL
        TYPE (TYPE_CORETRANSP), POINTER  :: CORETRANSP(:)  

        DO IVAL = 1, SIZE (CORETRANSP(1)%VALUES)
           IF(ASSOCIATED(CORETRANSP(1)%VALUES(IVAL)%sigma))                        THEN
              IF (ANY( ISNAN(CORETRANSP(1)%VALUES(IVAL)%sigma)))                      &
                   CORETRANSP(1)%VALUES(IVAL)%sigma                     = 0.0_R8
           END IF
           IF(ASSOCIATED(CORETRANSP(1)%VALUES(IVAL)%Ne_transp%diff_eff))           THEN
              IF (ANY( ISNAN(CORETRANSP(1)%VALUES(IVAL)%Ne_transp%diff_eff)))         &
                   CORETRANSP(1)%VALUES(IVAL)%Ne_transp%diff_eff        = 0.0_R8
           END IF
           IF(ASSOCIATED(CORETRANSP(1)%VALUES(IVAL)%Ne_transp%vconv_eff))          THEN
              IF (ANY( ISNAN(CORETRANSP(1)%VALUES(IVAL)%Ne_transp%vconv_eff)))        &
                   CORETRANSP(1)%VALUES(IVAL)%Ne_transp%vconv_eff       = 0.0_R8
           END IF
           IF(ASSOCIATED(CORETRANSP(1)%VALUES(IVAL)%Te_transp%diff_eff))           THEN
              IF (ANY( ISNAN(CORETRANSP(1)%VALUES(IVAL)%Te_transp%diff_eff)))         &
                   CORETRANSP(1)%VALUES(IVAL)%Te_transp%diff_eff        = 0.0_R8
           END IF
           IF(ASSOCIATED(CORETRANSP(1)%VALUES(IVAL)%Te_transp%vconv_eff))          THEN
              IF (ANY( ISNAN(CORETRANSP(1)%VALUES(IVAL)%Te_transp%vconv_eff)))        &
                   CORETRANSP(1)%VALUES(IVAL)%Te_transp%vconv_eff       = 0.0_R8
           END IF
           IF(ASSOCIATED(CORETRANSP(1)%VALUES(IVAL)%Ni_transp%diff_eff))           THEN
              IF (ANY( ISNAN(CORETRANSP(1)%VALUES(IVAL)%Ni_transp%diff_eff)))         &
                   CORETRANSP(1)%VALUES(IVAL)%Ni_transp%diff_eff        = 0.0_R8
           END IF
           IF(ASSOCIATED(CORETRANSP(1)%VALUES(IVAL)%Ni_transp%vconv_eff))          THEN
              IF (ANY( ISNAN(CORETRANSP(1)%VALUES(IVAL)%Ni_transp%vconv_eff)))        &
                   CORETRANSP(1)%VALUES(IVAL)%Ni_transp%vconv_eff       = 0.0_R8
           END IF
           IF(ASSOCIATED(CORETRANSP(1)%VALUES(IVAL)%Ti_transp%diff_eff))           THEN
              IF (ANY( ISNAN(CORETRANSP(1)%VALUES(IVAL)%Ti_transp%diff_eff)))         &
                   CORETRANSP(1)%VALUES(IVAL)%Ti_transp%diff_eff        = 0.0_R8
           END IF
           IF(ASSOCIATED(CORETRANSP(1)%VALUES(IVAL)%Ti_transp%vconv_eff))          THEN
              IF (ANY( ISNAN(CORETRANSP(1)%VALUES(IVAL)%Ti_transp%vconv_eff)))        &
                   CORETRANSP(1)%VALUES(IVAL)%Ti_transp%vconv_eff       = 0.0_R8
           END IF
           IF(ASSOCIATED(CORETRANSP(1)%VALUES(IVAL)%Vtor_transp%diff_eff))         THEN
              IF (ANY( ISNAN(CORETRANSP(1)%VALUES(IVAL)%Vtor_transp%diff_eff)))       &
                   CORETRANSP(1)%VALUES(IVAL)%Vtor_transp%diff_eff      = 0.0_R8
           END IF
           IF(ASSOCIATED(CORETRANSP(1)%VALUES(IVAL)%Vtor_transp%vconv_eff))        THEN
              IF (ANY( ISNAN(CORETRANSP(1)%VALUES(IVAL)%Vtor_transp%vconv_eff)))      &
                   CORETRANSP(1)%VALUES(IVAL)%Vtor_transp%vconv_eff     = 0.0_R8
           END IF
           DO IIMP = 1, SIZE (CORETRANSP(1)%VALUES(IVAL)%Nz_transp,        DIM=1)
              IF(ASSOCIATED(CORETRANSP(1)%VALUES(IVAL)%Nz_transp(IIMP)%diff_eff))  THEN
                 IF (ANY( ISNAN(CORETRANSP(1)%VALUES(IVAL)%Nz_transp(IIMP)%diff_eff)))&
                      CORETRANSP(1)%VALUES(IVAL)%Nz_transp(IIMP)%diff_eff  = 0.0_R8
              END IF
              IF(ASSOCIATED(CORETRANSP(1)%VALUES(IVAL)%Nz_transp(IIMP)%vconv_eff)) THEN
                 IF (ANY( ISNAN(CORETRANSP(1)%VALUES(IVAL)%Nz_transp(IIMP)%vconv_eff)))&
                      CORETRANSP(1)%VALUES(IVAL)%Nz_transp(IIMP)%vconv_eff = 0.0_R8
              END IF
              IF(ASSOCIATED(CORETRANSP(1)%VALUES(IVAL)%Tz_transp(IIMP)%diff_eff))  THEN
                 IF (ANY( ISNAN(CORETRANSP(1)%VALUES(IVAL)%Tz_transp(IIMP)%diff_eff)))&
                      CORETRANSP(1)%VALUES(IVAL)%Tz_transp(IIMP)%diff_eff  = 0.0_R8
              END IF
              IF(ASSOCIATED(CORETRANSP(1)%VALUES(IVAL)%Tz_transp(IIMP)%vconv_eff)) THEN
                 IF (ANY( ISNAN(CORETRANSP(1)%VALUES(IVAL)%Tz_transp(IIMP)%vconv_eff)))&
                      CORETRANSP(1)%VALUES(IVAL)%Tz_transp(IIMP)%vconv_eff = 0.0_R8
              END IF
           END DO
        END DO
        
        RETURN
      END SUBROUTINE CHECK_NANS_IN_CORETRANSP
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   


    END SUBROUTINE FILLCORETRANSP
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  







! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE FILLCORESOURCE (CORESOURCE_DB, CORESOURCE_GRID, CORESOURCE_OUT, INTERPOL) 


! +++ Declaration of variables: 
    USE EUITM_SCHEMAS
    USE EUITM_ROUTINES
    USE ALLOCATE_DEALLOCATE
    USE COPY_STRUCTURES
    USE DEALLOCATE_STRUCTURES
    USE INTERPOLATE_CPO

    USE ETS_PLASMA


    IMPLICIT NONE

    INTEGER                          :: NRHO1, NRHO2
    INTEGER                          :: NNUCL
    INTEGER                          :: NION
    INTEGER                          :: NIMP       
    INTEGER,             ALLOCATABLE :: NZIMP(:)
    INTEGER                          :: NNEUT
    INTEGER,             ALLOCATABLE :: NCOMP(:)
    INTEGER,             ALLOCATABLE :: NTYPE(:)

    INTEGER                          :: INTERPOL           !interpolation index "0"-based on RHO_TOR; all other - based on RHO_TOR_NORM  
    INTEGER                          :: IIMP
    INTEGER                          :: IVAL

! +++ CPO derived types:
    TYPE (TYPE_CORESOURCE), POINTER  :: CORESOURCE_GRID(:) !input CPO with internal parameters 
    TYPE (TYPE_CORESOURCE), POINTER  :: CORESOURCE_OUT(:)  !output CPO with profiles uploaded from the data base 
    TYPE (TYPE_CORESOURCE), POINTER  :: CORESOURCE_DB(:)   !time independent CPO slice 
    TYPE (TYPE_CORESOURCE), POINTER  :: CORESOURCE_TMP(:)





! +++ Allocate and define grid of output CPO:
    NRHO1                  = SIZE (CORESOURCE_GRID(1)%VALUES(1)%rho_tor, DIM=1)
    CALL GET_COMP_DIMENSIONS      (CORESOURCE_GRID(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
    CALL COPY_CPO                 (CORESOURCE_GRID, CORESOURCE_OUT)



! +++ Check if interpolation can be done:
    IF            (.NOT.ASSOCIATED(CORESOURCE_DB(1)%VALUES))            GOTO 10
    IF            (.NOT.ASSOCIATED(CORESOURCE_DB(1)%VALUES(1)%rho_tor)) GOTO 10
    IF            (MAXVAL(CORESOURCE_DB(1)%VALUES(1)%rho_tor).LE.0.0_R8)GOTO 10
    NRHO2                  = SIZE (CORESOURCE_DB(1)%VALUES(1)%rho_tor,              DIM=1)

    IF (INTERPOL.NE.0)    CORESOURCE_DB(1)%VALUES(1)%rho_tor =    CORESOURCE_DB(1)%VALUES(1)%rho_tor            &
                                                                / CORESOURCE_DB(1)%VALUES(1)%rho_tor(NRHO2)     &
                                                                * CORESOURCE_OUT(1)%VALUES(1)%rho_tor(NRHO1) 



! +++ Check for NaN in input shot:
    IF (SIZE(CORESOURCE_DB(1)%VALUES).GT.1) THEN
       ALLOCATE                      (CORESOURCE_TMP(1))
       ALLOCATE                      (CORESOURCE_TMP(1)%VALUES(1))
       CALL COPY_CPO                 (CORESOURCE_DB(1)%VALUES(1), CORESOURCE_TMP(1)%VALUES(1))
       CALL DEALLOCATE_CPO           (CORESOURCE_DB(1)%VALUES)
       ALLOCATE                      (CORESOURCE_DB(1)%VALUES(1))
       CALL COPY_CPO                 (CORESOURCE_TMP(1)%VALUES(1), CORESOURCE_DB(1)%VALUES(1))       
    END IF
      
    CALL CHECK_NANS_IN_CORESOURCE (CORESOURCE_DB)


! +++ Interpolate CPO:
    CALL INTERPOLATE_SOURCE       (CORESOURCE_DB(1), CORESOURCE_OUT(1))




! +++ Deallocate internal derived types:

 10 IF(ALLOCATED(NZIMP)) DEALLOCATE                    (NZIMP)
    IF(ALLOCATED(NCOMP)) DEALLOCATE                    (NCOMP)
    IF(ALLOCATED(NTYPE)) DEALLOCATE                    (NTYPE)



    RETURN 

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
    CONTAINS
      SUBROUTINE CHECK_NANS_IN_CORESOURCE (CORESOURCE)
        IMPLICIT NONE
        INTEGER                          :: IVAL
        TYPE (TYPE_CORESOURCE), POINTER  :: CORESOURCE(:)  

        DO IVAL = 1, SIZE (CORESOURCE(1)%VALUES)
           IF(ASSOCIATED(CORESOURCE(1)%VALUES(IVAL)%j))                             THEN
              IF(ANY(ISNAN(CORESOURCE(1)%VALUES(IVAL)%j)))                             &
                   CORESOURCE(1)%VALUES(IVAL)%j       = 0.0_R8
           END IF
           IF(ASSOCIATED(CORESOURCE(1)%VALUES(IVAL)%sigma))                         THEN
              IF(ANY(ISNAN(CORESOURCE(1)%VALUES(IVAL)%sigma)))                         &
                   CORESOURCE(1)%VALUES(IVAL)%sigma   = 0.0_R8
           END IF
           IF(ASSOCIATED(CORESOURCE(1)%VALUES(IVAL)%Se%exp))                        THEN
              IF(ANY(ISNAN(CORESOURCE(1)%VALUES(IVAL)%Se%exp)))                        &
                   CORESOURCE(1)%VALUES(IVAL)%Se%exp  = 0.0_R8
           END IF
           IF(ASSOCIATED(CORESOURCE(1)%VALUES(IVAL)%Se%imp))                        THEN
              IF(ANY(ISNAN(CORESOURCE(1)%VALUES(IVAL)%Se%imp)))                        &
                   CORESOURCE(1)%VALUES(IVAL)%Se%imp  = 0.0_R8
           END IF
           IF(ASSOCIATED(CORESOURCE(1)%VALUES(IVAL)%Qe%exp))                        THEN
              IF(ANY(ISNAN(CORESOURCE(1)%VALUES(IVAL)%Qe%exp)))                        &
                   CORESOURCE(1)%VALUES(IVAL)%Qe%exp  = 0.0_R8
           END IF
           IF(ASSOCIATED(CORESOURCE(1)%VALUES(IVAL)%Qe%imp))                        THEN
              IF(ANY(ISNAN(CORESOURCE(1)%VALUES(IVAL)%Qe%imp)))                        &
                   CORESOURCE(1)%VALUES(IVAL)%Qe%imp  = 0.0_R8
           END IF
           IF(ASSOCIATED(CORESOURCE(1)%VALUES(IVAL)%Si%exp))                        THEN
              IF(ANY(ISNAN(CORESOURCE(1)%VALUES(IVAL)%Si%exp)))                        &
                   CORESOURCE(1)%VALUES(IVAL)%Si%exp  = 0.0_R8
           END IF
           IF(ASSOCIATED(CORESOURCE(1)%VALUES(IVAL)%Si%imp))                        THEN
              IF(ANY(ISNAN(CORESOURCE(1)%VALUES(IVAL)%Si%imp)))                        &
                   CORESOURCE(1)%VALUES(IVAL)%Si%imp  = 0.0_R8
           END IF
           IF(ASSOCIATED(CORESOURCE(1)%VALUES(IVAL)%Qi%exp))                        THEN
              IF(ANY(ISNAN(CORESOURCE(1)%VALUES(IVAL)%Qi%exp)))                        &
               CORESOURCE(1)%VALUES(IVAL)%Qi%exp  = 0.0_R8
           END IF
           IF(ASSOCIATED(CORESOURCE(1)%VALUES(IVAL)%Qi%imp))                        THEN
              IF(ANY(ISNAN(CORESOURCE(1)%VALUES(IVAL)%Qi%imp)))                        &
                   CORESOURCE(1)%VALUES(IVAL)%Qi%imp  = 0.0_R8
           END IF
           IF(ASSOCIATED(CORESOURCE(1)%VALUES(IVAL)%Ui%exp))                        THEN
              IF(ANY(ISNAN(CORESOURCE(1)%VALUES(IVAL)%Ui%exp)))                        &
                   CORESOURCE(1)%VALUES(IVAL)%Ui%exp  = 0.0_R8
           END IF
           IF(ASSOCIATED(CORESOURCE(1)%VALUES(IVAL)%Ui%imp))                        THEN
              IF(ANY(ISNAN(CORESOURCE(1)%VALUES(IVAL)%Ui%imp)))                        &
                   CORESOURCE(1)%VALUES(IVAL)%Ui%imp  = 0.0_R8
           END IF
           DO IIMP = 1, SIZE (CORESOURCE(1)%VALUES(IVAL)%Sz,   DIM=1)
              IF(ASSOCIATED(CORESOURCE(1)%VALUES(IVAL)%Sz(IIMP)%exp))               THEN
                 IF(ANY(ISNAN(CORESOURCE(1)%VALUES(IVAL)%Sz(IIMP)%exp)))               &
                      CORESOURCE(1)%VALUES(IVAL)%Sz(IIMP)%exp  = 0.0_R8
              END IF
              IF(ASSOCIATED(CORESOURCE(1)%VALUES(IVAL)%Sz(IIMP)%imp))               THEN
                 IF(ANY(ISNAN(CORESOURCE(1)%VALUES(IVAL)%Sz(IIMP)%imp)))               &
                      CORESOURCE(1)%VALUES(IVAL)%Sz(IIMP)%imp  = 0.0_R8
              END IF
              IF(ASSOCIATED(CORESOURCE(1)%VALUES(IVAL)%Qz(IIMP)%exp))               THEN
                 IF(ANY(ISNAN(CORESOURCE(1)%VALUES(IVAL)%Qz(IIMP)%exp)))               &
                      CORESOURCE(1)%VALUES(IVAL)%Qz(IIMP)%exp  = 0.0_R8
              END IF
              IF(ASSOCIATED(CORESOURCE(1)%VALUES(IVAL)%Qz(IIMP)%imp))               THEN
                 IF(ANY(ISNAN(CORESOURCE(1)%VALUES(IVAL)%Qz(IIMP)%imp)))               &
                      CORESOURCE(1)%VALUES(IVAL)%Qz(IIMP)%imp  = 0.0_R8
              END IF
           END DO
        END DO
        
        RETURN

      END SUBROUTINE CHECK_NANS_IN_CORESOURCE
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

    END SUBROUTINE FILLCORESOURCE
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  







! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE FILLCOREIMPUR (COREIMPUR_DB, COREIMPUR_GRID, COREIMPUR_OUT, INTERPOL) 


! +++ Declaration of variables: 
    USE EUITM_SCHEMAS
    USE EUITM_ROUTINES
    USE ALLOCATE_DEALLOCATE
    USE COPY_STRUCTURES
    USE DEALLOCATE_STRUCTURES
    USE INTERPOLATE_CPO

    USE ETS_PLASMA


    IMPLICIT NONE

    INTEGER,              PARAMETER  :: NSLICE = 1             !number of CPO ocurancies in the work flow
    INTEGER                          :: NRHO1, NRHO2
    INTEGER                          :: NNUCL
    INTEGER                          :: NION
    INTEGER                          :: NIMP,     IIMP  
    INTEGER,             ALLOCATABLE :: NZIMP(:)
    INTEGER                          ::           IZIMP
    INTEGER                          :: NNEUT
    INTEGER,             ALLOCATABLE :: NCOMP(:)
    INTEGER,             ALLOCATABLE :: NTYPE(:)

    INTEGER                          :: INTERPOL           !interpolation index "0"-based on RHO_TOR; all other - based on RHO_TOR_NORM  

 
! +++ CPO derived types:
    TYPE (TYPE_COREIMPUR), POINTER   :: COREIMPUR_GRID(:)     !input CPO with internal parameters 
    TYPE (TYPE_COREIMPUR), POINTER   :: COREIMPUR_OUT(:)      !output CPO with sources uploaded from the data base 
    TYPE (TYPE_COREIMPUR), POINTER   :: COREIMPUR_DB(:)       !time independent CPO slice 



! +++ Allocate and define grid of output CPO:
    NRHO1                  = SIZE (COREIMPUR_GRID(1)%rho_tor, DIM=1)
    CALL GET_COMP_DIMENSIONS      (COREIMPUR_GRID(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
    CALL COPY_CPO                 (COREIMPUR_GRID, COREIMPUR_OUT)



! +++ Check if interpolation can be done:
    IF  (.NOT.ASSOCIATED(COREIMPUR_DB(1)%rho_tor).OR.NIMP.LE.0) GOTO 10
    IF  (MAXVAL(COREIMPUR_DB(1)%rho_tor).LE.0.0_R8) GOTO 10
    NRHO2                  = SIZE (COREIMPUR_DB(1)%rho_tor,   DIM=1)
    IF (INTERPOL.NE.0) &
       COREIMPUR_DB(1)%rho_tor =    COREIMPUR_DB(1)%rho_tor            &
                                  / COREIMPUR_DB(1)%rho_tor(NRHO2)     &
                                  * COREIMPUR_OUT(1)%rho_tor(NRHO1) 


!! +++ Interpolate CPO:
    CALL INTERPOLATE_IMPUR        (COREIMPUR_DB(1), COREIMPUR_OUT(1))


    DO IIMP = 1, NIMP
       DO IZIMP = 1, NZIMP(IIMP)
          IF (COREIMPUR_OUT(1)%IMPURITY(IIMP)%boundary%type(IZIMP) .EQ. 6) THEN
              COREIMPUR_OUT(1)%IMPURITY(IIMP)%boundary%type(IZIMP)       =  1

              COREIMPUR_OUT(1)%IMPURITY(IIMP)%boundary%value(1,IZIMP)    =  COREIMPUR_OUT(1)%IMPURITY(IIMP)%nz(NRHO1,IZIMP)
              COREIMPUR_OUT(1)%IMPURITY(IIMP)%boundary%value(2,IZIMP)    =  0._R8
              COREIMPUR_OUT(1)%IMPURITY(IIMP)%boundary%value(3,IZIMP)    =  0._R8
          END IF
       END DO
    END DO




! +++ Deallocate internal derived types:

 10 IF(ALLOCATED(NZIMP)) DEALLOCATE                    (NZIMP)
    IF(ALLOCATED(NCOMP)) DEALLOCATE                    (NCOMP)
    IF(ALLOCATED(NTYPE)) DEALLOCATE                    (NTYPE)




    RETURN 


    END SUBROUTINE FILLCOREIMPUR
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  







! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE FILLCORENEUTRALS (CORENEUTRALS_DB, CORENEUTRALS_GRID, CORENEUTRALS_OUT, INTERPOL) 


! +++ Declaration of variables: 
    USE EUITM_SCHEMAS
    USE EUITM_ROUTINES
    USE ALLOCATE_DEALLOCATE
    USE COPY_STRUCTURES
    USE DEALLOCATE_STRUCTURES
    USE INTERPOLATE_CPO

    USE ETS_PLASMA


    IMPLICIT NONE

    INTEGER,                 PARAMETER  :: NSLICE = 1             !number of CPO ocurancies in the work flow
    INTEGER                             :: NRHO1, NRHO2
    INTEGER                             :: NNUCL
    INTEGER                             :: NION
    INTEGER                             :: NIMP  
    INTEGER,                ALLOCATABLE :: NZIMP(:)
    INTEGER                             :: NNEUT
    INTEGER,                ALLOCATABLE :: NCOMP(:)
    INTEGER,                ALLOCATABLE :: NTYPE(:)

    INTEGER                             :: INTERPOL               !interpolation index "0"-based on RHO_TOR; all other - based on RHO_TOR_NORM  

 
! +++ CPO derived types:
    TYPE (TYPE_CORENEUTRALS), POINTER   :: CORENEUTRALS_GRID(:)   !input CPO with internal parameters 
    TYPE (TYPE_CORENEUTRALS), POINTER   :: CORENEUTRALS_OUT(:)    !output CPO with sources uploaded from the data base 
    TYPE (TYPE_CORENEUTRALS), POINTER   :: CORENEUTRALS_DB(:)     !time independent CPO slice 



! +++ Allocate and define grid of output CPO:
    NRHO1                     = SIZE (CORENEUTRALS_GRID(1)%rho_tor, DIM=1)
    CALL GET_COMP_DIMENSIONS         (CORENEUTRALS_GRID(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
    CALL COPY_CPO                    (CORENEUTRALS_GRID, CORENEUTRALS_OUT)
    CORENEUTRALS_OUT(1)%rho_tor          = CORENEUTRALS_GRID(1)%rho_tor
    CORENEUTRALS_OUT(1)%rho_tor_norm     = CORENEUTRALS_GRID(1)%rho_tor_norm



! +++ Check if interpolation can be done:
    IF               (.NOT.ASSOCIATED(CORENEUTRALS_DB(1)%rho_tor)) GOTO 10
    IF               (MAXVAL(CORENEUTRALS_DB(1)%rho_tor).LE.0.0_R8)GOTO 10
    NRHO2                     = SIZE (CORENEUTRALS_DB(1)%rho_tor,   DIM=1)
    IF (INTERPOL.NE.0) &
       CORENEUTRALS_DB(1)%rho_tor =    CORENEUTRALS_DB(1)%rho_tor            &
                                     / CORENEUTRALS_DB(1)%rho_tor(NRHO2)     &
                                     * CORENEUTRALS_OUT(1)%rho_tor(NRHO1) 


! +++ Interpolate CPO:
    CALL INTERPOLATE_NEUTRALS        (CORENEUTRALS_DB(1), CORENEUTRALS_OUT(1))




! +++ Deallocate internal derived types:

 10 IF(ALLOCATED(NZIMP)) DEALLOCATE                    (NZIMP)
    IF(ALLOCATED(NCOMP)) DEALLOCATE                    (NCOMP)
    IF(ALLOCATED(NTYPE)) DEALLOCATE                    (NTYPE)




    RETURN 


    END SUBROUTINE FILLCORENEUTRALS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE FILLEQUILIBRIUM (EQUILIBRIUM_DB, EQUILIBRIUM_GRID, EQUILIBRIUM_OUT, INTERPOL) 


! +++ Declaration of variables: 
    USE EUITM_SCHEMAS
    USE EUITM_ROUTINES
    USE ALLOCATE_DEALLOCATE
    USE COPY_STRUCTURES
    USE DEALLOCATE_STRUCTURES
    USE ETS_PLASMA
    USE ITM_TYPES
    USE ITM_CONSTANTS


    IMPLICIT NONE


! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM_GRID(:)  !input CPO with internal parameters 
    TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM_OUT(:)   !output CPO with sources uploaded from the data base 
    TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM_DB(:)    !time independent CPO slice 


! +++ Local derived types:
    TYPE (DIAGNOSTIC)                :: DIAG                 !diagnostic output 


! +++ Internal derived types:
    INTEGER                          :: NPSI1                !total number of rho grid on grid and output CPOs    
    INTEGER                          :: NPSI2                !total number of rho grid from the data base  
    INTEGER                          :: INTERPOL             !interpolation index "0"-based on RHO_TOR; all other - based on RHO_TOR_NORM  
     
    REAL (R8),           ALLOCATABLE :: RHO1(:), RHO2(:)



! +++ Allocate output CPO and internal derived types:
    NPSI1           = SIZE (EQUILIBRIUM_GRID(1)%profiles_1d%rho_tor,      DIM=1)
    NPSI2           = SIZE (EQUILIBRIUM_DB(1)%profiles_1d%rho_tor,        DIM=1)

    ALLOCATE        ( RHO1(NPSI1))
    ALLOCATE        ( RHO2(NPSI2))

    IF (INTERPOL.EQ.0) THEN
       RHO1         = EQUILIBRIUM_GRID(1)%profiles_1d%rho_tor
       RHO2         = EQUILIBRIUM_DB(1)%profiles_1d%rho_tor
    ELSE
       RHO1         = EQUILIBRIUM_GRID(1)%profiles_1d%rho_tor / EQUILIBRIUM_GRID(1)%profiles_1d%rho_tor(NPSI1)
       RHO2         = EQUILIBRIUM_DB(1)%profiles_1d%rho_tor   / EQUILIBRIUM_DB(1)%profiles_1d%rho_tor(NPSI2)
    END IF


! +++ Allocate and define grid of output CPO:
    IF(.NOT.ASSOCIATED(EQUILIBRIUM_OUT)) ALLOCATE(EQUILIBRIUM_OUT(1))
    CALL COPY_CPO(EQUILIBRIUM_GRID(1), EQUILIBRIUM_OUT(1))


! +++ PROFILES_1D:
    IF(     ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%jparallel) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%jparallel))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%jparallel(NPSI1))
         CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%jparallel,      RHO2, NPSI2, &
                       EQUILIBRIUM_OUT(1)%profiles_1d%jparallel,     RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%jphi) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%jphi))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%jphi(NPSI1))
         CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%jphi,           RHO2, NPSI2, &
                       EQUILIBRIUM_OUT(1)%profiles_1d%jphi,          RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%q) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%q))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%q(NPSI1)) 
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%q,               RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%q,              RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%pressure) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%pressure))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%pressure(NPSI1)) 
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%pressure,        RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%pressure,       RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%pprime) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%pprime))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%pprime(NPSI1)) 
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%pprime,          RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%pprime,         RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%ffprime) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%ffprime))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%ffprime(NPSI1)) 
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%ffprime,         RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%ffprime,        RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%gm1) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%gm1))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%gm1(NPSI1)) 
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%gm1,             RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%gm1,            RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%gm2) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%gm2))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%gm2(NPSI1)) 
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%gm2,             RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%gm2,            RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%gm3)  .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%gm3))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%gm3(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%gm3,             RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%gm3,            RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%gm4) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%gm4))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%gm4(NPSI1)) 
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%gm4,             RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%gm4,            RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%gm5) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%gm5))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%gm5(NPSI1)) 
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%gm5,             RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%gm5,            RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%gm6) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%gm6))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%gm6(NPSI1)) 
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%gm6,             RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%gm6,            RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%gm7) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%gm7))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%gm7(NPSI1)) 
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%gm7,             RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%gm7,            RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%gm8) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%gm8))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%gm8(NPSI1)) 
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%gm8,             RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%gm8,            RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%gm9) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%gm9))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%gm9(NPSI1)) 
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%gm9,             RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%gm9,            RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%F_dia) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%F_dia))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%F_dia(NPSI1)) 
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%F_dia,           RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%F_dia,          RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%volume) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%volume))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%volume(NPSI1)) 
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%volume,          RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%volume,         RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%vprime) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%vprime))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%vprime(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%vprime,          RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%vprime,         RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%area) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%area))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%area(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%area,            RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%area,           RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%aprime) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%aprime))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%aprime(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%aprime,          RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%aprime,         RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%elongation) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%elongation))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%elongation(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%elongation,      RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%elongation,     RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%tria_upper) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%tria_upper))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%tria_upper(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%tria_upper,      RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%tria_upper,     RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%tria_lower) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%tria_lower))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%tria_lower(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%tria_lower,      RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%tria_lower,     RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%r_inboard)  .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%r_inboard))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%r_inboard(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%r_inboard,       RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%r_inboard,      RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%r_outboard) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%r_outboard))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%r_outboard(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%r_outboard,      RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%r_outboard,     RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%psi)  .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%psi))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%psi(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%psi,             RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%psi,            RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%phi)  .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%phi))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%phi(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%phi,             RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%phi,            RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%rho_vol) .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%rho_vol))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%rho_vol(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%rho_vol,         RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%rho_vol,        RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%dpsidrho_tor)  .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%dpsidrho_tor))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%dpsidrho_tor(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%dpsidrho_tor,    RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%dpsidrho_tor,   RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%beta_pol)  .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%beta_pol))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%beta_pol(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%beta_pol,        RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%beta_pol,       RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%li)  .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%li))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%li(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%li,              RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%li,             RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%dvdrho)  .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%dvdrho))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%dvdrho(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%dvdrho,          RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%dvdrho,         RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%surface)  .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%surface))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%surface(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%surface,         RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%surface,        RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%ftrap)  .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%ftrap))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%ftrap(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%ftrap,           RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%ftrap,          RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%b_av)  .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%b_av))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%b_av(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%b_av,            RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%b_av,           RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%b_min)  .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%b_min))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%b_min(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%b_min,           RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%b_min,          RHO1, NPSI1)
    END IF
    IF(ASSOCIATED(EQUILIBRIUM_DB(1)%profiles_1d%b_max)  .AND.             &
       .NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%b_max))    THEN
              ALLOCATE(EQUILIBRIUM_OUT(1)%profiles_1d%b_max(NPSI1))
        CALL L3interp(EQUILIBRIUM_DB(1)%profiles_1d%b_max,           RHO2, NPSI2, &
                      EQUILIBRIUM_OUT(1)%profiles_1d%b_max,          RHO1, NPSI1)
    END IF


! +++ Check for COCOS13:    
! for COCOS 13, Ip > 0 ==> Psi decreasing
    IF ( ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%psi)) THEN
       IF (EQUILIBRIUM_DB(1)%global_param%i_plasma * (EQUILIBRIUM_OUT(1)%profiles_1d%psi(NPSI2) - EQUILIBRIUM_OUT(1)%profiles_1d%psi(1)) .GE.0.0_R8 ) THEN
          EQUILIBRIUM_OUT(1)%profiles_1d%psi = -1.0_R8 * EQUILIBRIUM_OUT(1)%profiles_1d%psi
          DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"COCOS violation: Ip > 0 ==> Psi decreasing. Psi is reverted to be compliant with COCOS13"
          DIAG%IERR             = DIAG%IERR + 1
       ENDIF
    ENDIF

! for COCOS 13, q has the opposite sign to the sign of Ip*B0
    IF ( ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%q)) THEN
       IF (EQUILIBRIUM_OUT(1)%profiles_1d%q(1) * EQUILIBRIUM_DB(1)%global_param%i_plasma * EQUILIBRIUM_DB(1)%global_param%toroid_field%b0 .GE. 0.0_R8 ) THEN
          EQUILIBRIUM_OUT(1)%profiles_1d%q = -1.0_R8 * EQUILIBRIUM_OUT(1)%profiles_1d%q
          DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"COCOS violation: Ip*B0*q should be < 0. q is reverted to be compliant with COCOS13"
          DIAG%IERR             = DIAG%IERR + 1
       ENDIF
    ENDIF

! Check the direction of current (should coinside with IP)
    IF ( ASSOCIATED(EQUILIBRIUM_OUT(1)%profiles_1d%jparallel)) THEN
       IF (EQUILIBRIUM_DB(1)%global_param%i_plasma * EQUILIBRIUM_OUT(1)%profiles_1d%jparallel(1) .LE.0.0_R8 ) THEN
          EQUILIBRIUM_OUT(1)%profiles_1d%jparallel = -1.0_R8 * EQUILIBRIUM_OUT(1)%profiles_1d%jparallel
          DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"COCOS violation: different sign for IP and Jpar. Jpar profile is reverted to be consistent with total current"
          DIAG%IERR             = DIAG%IERR + 1
       ENDIF
    ENDIF



! +++ Deallocate internal derived types:
    DEALLOCATE       (RHO1)
    DEALLOCATE       (RHO2)

! +++ CODEPARAM:
    IF(ASSOCIATED(EQUILIBRIUM_OUT(1)%codeparam%codename))    &
       DEALLOCATE(EQUILIBRIUM_OUT(1)%codeparam%codename)
    IF(ASSOCIATED(EQUILIBRIUM_OUT(1)%codeparam%codeversion)) &
       DEALLOCATE(EQUILIBRIUM_OUT(1)%codeparam%codeversion)
    IF(ASSOCIATED(EQUILIBRIUM_OUT(1)%codeparam%output_diag)) &
       DEALLOCATE(EQUILIBRIUM_OUT(1)%codeparam%output_diag)
    ALLOCATE     (EQUILIBRIUM_OUT(1)%codeparam%codename(1))
    ALLOCATE     (EQUILIBRIUM_OUT(1)%codeparam%codeversion(1))
    ALLOCATE     (EQUILIBRIUM_OUT(1)%codeparam%output_diag(1))

    EQUILIBRIUM_OUT(1)%codeparam%codename            = 'Fill_EQILIBRIUM'
    EQUILIBRIUM_OUT(1)%codeparam%codeversion         = 'Fill_EQILIBRIUM_4.10b.10'
    EQUILIBRIUM_OUT(1)%codeparam%output_flag         =  DIAG%IERR
    EQUILIBRIUM_OUT(1)%codeparam%output_diag(1)      = "FILL EQUILIBRIUM: "//TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))
    RETURN 

  END SUBROUTINE FILLEQUILIBRIUM
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE FILLTOROIDFIELD (TOROIDFIELD, TOROIDFIELD_GRID, TOROIDFIELD_OUT)

!-------------------------------------------------------!
!     This routine loads geometry quantities from the   !
!     EQUILIBRIUM CPO stored in ITM data base.          !
!     The settings, like switches or boundary           !
!     conditions, are not updated!!! These quantities   !
!     are taken from the EQUILIBRIUM_IN.                !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!


! +++ Declaration of variables: 
    USE EUITM_SCHEMAS
    USE EUITM_ROUTINES
    USE ITM_TYPES
    USE COPY_STRUCTURES
    USE DEALLOCATE_STRUCTURES

    IMPLICIT NONE


! +++ CPO derived types:
    TYPE (TYPE_TOROIDFIELD), POINTER :: TOROIDFIELD_OUT(:)  
    TYPE (TYPE_TOROIDFIELD), POINTER :: TOROIDFIELD(:)  
    TYPE (TYPE_TOROIDFIELD), POINTER :: TOROIDFIELD_GRID(:)
    

! +++ Retrieve CPO from the data base:


    ALLOCATE              (TOROIDFIELD_OUT(1))
    
    CALL COPY_CPO         (TOROIDFIELD_GRID(1), TOROIDFIELD_OUT(1))


    RETURN 


    END SUBROUTINE FILLTOROIDFIELD
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE FILLNEOCLASSIC (NEOCLASSIC_DB,  NEOCLASSIC_GRID,  NEOCLASSIC_OUT, INTERPOL) 

! +++ Declaration of variables: 
    USE ITM_TYPES
    USE EUITM_SCHEMAS
    USE ALLOCATE_DEALLOCATE
    USE COPY_STRUCTURES
    USE DEALLOCATE_STRUCTURES
    USE INTERPOLATE_CPO


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
    IMPLICIT NONE

    INTEGER,              PARAMETER  :: NSLICE = 1             !number of CPO ocurancies in the work flow
    INTEGER                          :: NRHO1, NRHO2
    INTEGER                          :: NNUCL
    INTEGER                          :: NION, IION
    INTEGER                          :: NIMP       
    INTEGER,             ALLOCATABLE :: NZIMP(:)
    INTEGER                          :: NNEUT
    INTEGER,             ALLOCATABLE :: NCOMP(:)
    INTEGER,             ALLOCATABLE :: NTYPE(:)

    INTEGER                          :: INTERPOL               !interpolation index "0"-based on RHO_TOR; all other - based on RHO_TOR_NORM  

! +++ CPO derived types:
    TYPE (TYPE_NEOCLASSIC),  POINTER :: NEOCLASSIC_GRID(:)     !input CPO with internal ETS parameters 
    TYPE (TYPE_NEOCLASSIC),  POINTER :: NEOCLASSIC_OUT(:)      !output CPO with profiles uploaded from the data base 
    TYPE (TYPE_NEOCLASSIC),  POINTER :: NEOCLASSIC_DB(:)       !time independent CPO slice 



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   


! +++ RHO grid:
    NRHO1                 =  SIZE (NEOCLASSIC_GRID(1)%rho_tor,         DIM=1)
    NRHO2                 =  SIZE (NEOCLASSIC_DB(1)%rho_tor,           DIM=1)


! +++ Allocate output CPO and internal derived types:
    CALL GET_COMP_DIMENSIONS      (NEOCLASSIC_GRID(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
    CALL COPY_CPO                 (NEOCLASSIC_GRID, NEOCLASSIC_OUT)



! +++ Check if interpolation can be done:
    IF (.NOT.ASSOCIATED(NEOCLASSIC_DB(1)%rho_tor))  GOTO 10
    IF (MAXVAL(NEOCLASSIC_DB(1)%rho_tor).LE.0.0_R8) GOTO 10

    IF (INTERPOL.NE.0) &
       NEOCLASSIC_DB(1)%rho_tor =    NEOCLASSIC_DB(1)%rho_tor            &
                                 / NEOCLASSIC_DB(1)%rho_tor(NRHO2)       &
                                 * NEOCLASSIC_OUT(1)%rho_tor(NRHO1) 



! +++ Interpolate profiles:
    CALL INTERPOLATE_NEOCLASSIC   (NEOCLASSIC_DB(1), NEOCLASSIC_OUT(1))




! +++ Deallocate internal derived types:

 10 IF(ALLOCATED(NZIMP)) DEALLOCATE                    (NZIMP)
    IF(ALLOCATED(NCOMP)) DEALLOCATE                    (NCOMP)
    IF(ALLOCATED(NTYPE)) DEALLOCATE                    (NTYPE)


    RETURN 
 

    END SUBROUTINE FILLNEOCLASSIC
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  



END MODULE FILL_CPOS










