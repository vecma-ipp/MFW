MODULE INTERPOLATE_CPO
!-------------------------------------------------------!  
!                                                       !
!     This module contains routines for                 !
!     interpolation of CPOs used in ETS                 !
!                                                       !
!-------------------------------------------------------!  
!                                                       !
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!  

CONTAINS




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  SUBROUTINE INTERPOLATE_SOURCE(CORESOURCE_IN, CORESOURCE_OUT)

    USE EUITM_SCHEMAS
    USE ITM_TYPES
    USE ALLOCATE_DEALLOCATE

    IMPLICIT NONE

    TYPE (TYPE_CORESOURCE) :: CORESOURCE_IN
    TYPE (TYPE_CORESOURCE) :: CORESOURCE_OUT

    INTEGER                :: NVAL1, NVAL2
    INTEGER                :: IVAL
    INTEGER                :: NRHO1, NRHO2
    INTEGER                :: NNUCL1,INUCL1
    INTEGER                :: NNUCL2,INUCL2
    INTEGER                :: NION1, IION1
    INTEGER                :: NION2, IION2
    INTEGER                :: NIMP1, IIMP1        
    INTEGER                :: NIMP2, IIMP2
    INTEGER,   ALLOCATABLE :: NZIMP1(:)
    INTEGER,   ALLOCATABLE :: NZIMP2(:)
    INTEGER                :: IZIMP1,IZIMP2
    INTEGER                :: NNEUT1,INEUT1
    INTEGER                :: NNEUT2,INEUT2
    INTEGER,   ALLOCATABLE :: NCOMP1(:)
    INTEGER,   ALLOCATABLE :: NCOMP2(:)
    INTEGER,   ALLOCATABLE :: NTYPE1(:)
    INTEGER,   ALLOCATABLE :: NTYPE2(:)

    REAL (R8), ALLOCATABLE :: RHO1(:), RHO2(:)
    REAL (R8), ALLOCATABLE :: FUN(:)
    REAL (R8)              :: ZMIN1,   ZMIN2
    REAL (R8)              :: ZMAX1,   ZMAX2
!==============================================

    NVAL1              = 1
    NVAL2              = SIZE(CORESOURCE_IN%VALUES)


    NRHO1              = SIZE(CORESOURCE_OUT%VALUES(1)%rho_tor)

    ALLOCATE (FUN(NRHO1))
    ALLOCATE (RHO1(NRHO1))

    RHO1               = CORESOURCE_OUT%VALUES(1)%rho_tor

    CALL GET_COMP_DIMENSIONS  (CORESOURCE_OUT%COMPOSITIONS, NNUCL1, NION1,  NIMP1,  NZIMP1, NNEUT1, NTYPE1, NCOMP1)
    CALL GET_COMP_DIMENSIONS  (CORESOURCE_IN%COMPOSITIONS,  NNUCL2, NION2,  NIMP2,  NZIMP2, NNEUT2, NTYPE2, NCOMP2)

    IF(.NOT.ASSOCIATED(CORESOURCE_IN%VALUES)) GOTO 10



! +++ Nullify initial profiles:
    CORESOURCE_OUT%VALUES(1)%j                 =  0.0_R8
    CORESOURCE_OUT%VALUES(1)%sigma             =  0.0_R8
    CORESOURCE_OUT%VALUES(1)%Se%exp            =  0.0_R8
    CORESOURCE_OUT%VALUES(1)%Se%imp            =  0.0_R8
    CORESOURCE_OUT%VALUES(1)%Qe%exp            =  0.0_R8
    CORESOURCE_OUT%VALUES(1)%Qe%imp            =  0.0_R8
    CORESOURCE_OUT%VALUES(1)%Si%exp            =  0.0_R8
    CORESOURCE_OUT%VALUES(1)%Si%imp            =  0.0_R8
    CORESOURCE_OUT%VALUES(1)%Qi%exp            =  0.0_R8
    CORESOURCE_OUT%VALUES(1)%Qi%imp            =  0.0_R8
    CORESOURCE_OUT%VALUES(1)%Ui%exp            =  0.0_R8
    CORESOURCE_OUT%VALUES(1)%Ui%imp            =  0.0_R8
    DO IIMP1 = 1, NIMP1
       CORESOURCE_OUT%VALUES(1)%Sz(IIMP1)%exp  =  0.0_R8
       CORESOURCE_OUT%VALUES(1)%Sz(IIMP1)%imp  =  0.0_R8
       CORESOURCE_OUT%VALUES(1)%Qz(IIMP1)%exp  =  0.0_R8
       CORESOURCE_OUT%VALUES(1)%Qz(IIMP1)%imp  =  0.0_R8
    END DO




! +++ Check information saved to different VALUES:
    LOOP_on_VALUES: DO IVAL = 1, NVAL2
       NRHO2   = SIZE(CORESOURCE_IN%VALUES(1)%rho_tor)
       ALLOCATE (RHO2(NRHO2))
       RHO2    = CORESOURCE_IN%VALUES(IVAL)%rho_tor




! j
       FUN(:)                                            =  0.0_R8
       IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%j))      THEN                  
          CALL L3interp(CORESOURCE_IN%VALUES(IVAL)%j,          RHO2,   NRHO2,  &
                        FUN,                                   RHO1,   NRHO1)
          CORESOURCE_OUT%VALUES(1)%j         = CORESOURCE_OUT%VALUES(1)%j + FUN
       END IF

! sigma
       FUN(:)                                            =  0.0_R8
       IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%sigma))  THEN                   
          CALL L3interp(CORESOURCE_IN%VALUES(IVAL)%sigma,      RHO2,   NRHO2,  &
                        FUN,                                   RHO1,   NRHO1)
          CORESOURCE_OUT%VALUES(1)%sigma     = CORESOURCE_OUT%VALUES(1)%sigma + FUN
       END IF

! Se
       FUN(:)                                            =  0.0_R8
       IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%Se%exp)) THEN                  
          CALL L3interp(CORESOURCE_IN%VALUES(IVAL)%Se%exp,     RHO2,   NRHO2,  &
                        FUN,                                   RHO1,   NRHO1)
          CORESOURCE_OUT%VALUES(1)%Se%exp    = CORESOURCE_OUT%VALUES(1)%Se%exp + FUN
       END IF

       FUN(:)                                            =  0.0_R8
       IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%Se%imp)) THEN                  
          CALL L3interp(CORESOURCE_IN%VALUES(IVAL)%Se%imp,     RHO2,   NRHO2,  &
                        FUN,                                   RHO1,   NRHO1)
          CORESOURCE_OUT%VALUES(1)%Se%imp    = CORESOURCE_OUT%VALUES(1)%Se%imp + FUN
       END IF

! Qe 
       FUN(:)                                            =  0.0_R8
       IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%Qe%exp)) THEN                  
          CALL L3interp(CORESOURCE_IN%VALUES(IVAL)%Qe%exp,     RHO2,   NRHO2,  &
                        FUN,                                   RHO1,   NRHO1)
          CORESOURCE_OUT%VALUES(1)%Qe%exp    = CORESOURCE_OUT%VALUES(1)%Qe%exp + FUN
       END IF

       FUN(:)                                            =  0.0_R8
       IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%Qe%imp)) THEN                  
          CALL L3interp(CORESOURCE_IN%VALUES(IVAL)%Qe%imp,     RHO2,   NRHO2,  &
                        FUN,                                   RHO1,   NRHO1)
          CORESOURCE_OUT%VALUES(1)%Qe%imp    = CORESOURCE_OUT%VALUES(1)%Qe%imp + FUN
       END IF



! +++ IONS
       OUTPUT_IONS_LOOP: DO IION1 = 1, NION1
          INUCL1      = CORESOURCE_OUT%COMPOSITIONS%IONS(IION1)%nucindex
          INPUT_IONS_LOOP: DO IION2 = 1, NION2
             INUCL2   = CORESOURCE_IN%COMPOSITIONS%IONS(IION2)%nucindex

             IF (INUCL2.LE.0 .OR. INUCL2.GT.SIZE(CORESOURCE_IN%COMPOSITIONS%NUCLEI)) GOTO 5

             CHECK_FOR_IONS_CONSISTENCY: IF &
               (ABS(CORESOURCE_OUT%COMPOSITIONS%NUCLEI(INUCL1)%amn   - CORESOURCE_IN%COMPOSITIONS%NUCLEI(INUCL2)%amn)  .LE. 0.25  .AND.  &
                ABS(CORESOURCE_OUT%COMPOSITIONS%NUCLEI(INUCL1)%zn    - CORESOURCE_IN%COMPOSITIONS%NUCLEI(INUCL2)%zn )  .LE. 0.25  .AND.  &
                ABS(CORESOURCE_OUT%COMPOSITIONS%IONS(IION1)%zion     - CORESOURCE_IN%COMPOSITIONS%IONS(IION2)%zion  )  .LE. 0.25) THEN


! Si
                FUN(:)                                            =  0.0_R8
                IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%Si%exp)) THEN                                                    
                   CALL L3interp(CORESOURCE_IN%VALUES(IVAL)%Si%exp(:,iion2),  RHO2,  NRHO2,  &
                                 FUN,                                         RHO1,  NRHO1)
                   CORESOURCE_OUT%VALUES(1)%Si%exp(:,iion1)          = CORESOURCE_OUT%VALUES(1)%Si%exp(:,iion1) + FUN
                END IF
                FUN(:)                                            =  0.0_R8
                IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%Si%imp)) THEN                         
                   CALL L3interp(CORESOURCE_IN%VALUES(IVAL)%Si%imp(:,iion2),  RHO2,  NRHO2,  &
                                 FUN,                                         RHO1,  NRHO1)
                   CORESOURCE_OUT%VALUES(1)%Si%imp(:,iion1)          = CORESOURCE_OUT%VALUES(1)%Si%imp(:,iion1) + FUN
                END IF
       
! Qi
                FUN(:)                                            =  0.0_R8
                IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%Qi%exp)) THEN                         
                   CALL L3interp(CORESOURCE_IN%VALUES(IVAL)%Qi%exp(:,iion2),  RHO2,  NRHO2,  &
                                 FUN,                                         RHO1,  NRHO1)
                   CORESOURCE_OUT%VALUES(1)%Qi%exp(:,iion1)          = CORESOURCE_OUT%VALUES(1)%Qi%exp(:,iion1) + FUN
                END IF
                FUN(:)                                            =  0.0_R8
                IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%Qi%imp)) THEN                         
                   CALL L3interp(CORESOURCE_IN%VALUES(IVAL)%Qi%imp(:,iion2),  RHO2,  NRHO2,  &
                                 FUN,                                         RHO1,  NRHO1)
                   CORESOURCE_OUT%VALUES(1)%Qi%imp(:,iion1)          = CORESOURCE_OUT%VALUES(1)%Qi%imp(:,iion1) + FUN
                END IF

! Ui
                FUN(:)                                            =  0.0_R8
                IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%Ui%exp)) THEN                         
                   CALL L3interp(CORESOURCE_IN%VALUES(IVAL)%Ui%exp(:,iion2),  RHO2,  NRHO2,  &
                                 FUN,                                         RHO1,  NRHO1)
                   CORESOURCE_OUT%VALUES(1)%Ui%exp(:,iion1)          = CORESOURCE_OUT%VALUES(1)%Ui%exp(:,iion1) + FUN
                END IF
                FUN(:)                                            =  0.0_R8
                IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%Ui%imp)) THEN                         
                   CALL L3interp(CORESOURCE_IN%VALUES(IVAL)%Ui%imp(:,iion2),  RHO2,  NRHO2,  &
                                 FUN,                                         RHO1,  NRHO1)
                   CORESOURCE_OUT%VALUES(1)%Ui%imp(:,iion1)          = CORESOURCE_OUT%VALUES(1)%Ui%imp(:,iion1) + FUN
                END IF
 
             END IF CHECK_FOR_IONS_CONSISTENCY

5            CONTINUE

          END DO INPUT_IONS_LOOP
       END DO OUTPUT_IONS_LOOP



! +++ IMPURITY
       IF (NIMP1*NIMP2.LE.0) GOTO 8
       OUTPUT_IMPURITY_LOOP: DO IIMP1 = 1, NIMP1
          INUCL1      = CORESOURCE_OUT%COMPOSITIONS%IMPURITIES(IIMP1)%nucindex

          INPUT_IMPURITY_LOOP: DO IIMP2 = 1, NIMP2
             INUCL2   = CORESOURCE_IN%COMPOSITIONS%IMPURITIES(IIMP2)%nucindex

             IF (INUCL2.LE.0 .OR. INUCL2.GT.SIZE(CORESOURCE_IN%COMPOSITIONS%NUCLEI)) GOTO 7

             CHECK_FOR_NUCLEI_CONSISTENCY: IF &
               (ABS(CORESOURCE_OUT%COMPOSITIONS%NUCLEI(INUCL1)%amn - CORESOURCE_IN%COMPOSITIONS%NUCLEI(INUCL2)%amn) .LE. 0.25   .AND.  &
                ABS(CORESOURCE_OUT%COMPOSITIONS%NUCLEI(INUCL1)%zn  - CORESOURCE_IN%COMPOSITIONS%NUCLEI(INUCL2)%zn ) .LE. 0.25  ) THEN

                OUTPUT_IONIZATION_STATE: DO IZIMP1 = 1, NZIMP1(IIMP1)
                   INPUT_IONIZATION_STATE: DO IZIMP2 = 1, NZIMP2(IIMP2)

                    ZMIN1    = CORESOURCE_OUT%COMPOSITIONS%IMPURITIES(IIMP1)%zmin(IZIMP1)
                    ZMAX1    = CORESOURCE_OUT%COMPOSITIONS%IMPURITIES(IIMP1)%zmax(IZIMP1)
                    ZMIN2    = CORESOURCE_IN%COMPOSITIONS%IMPURITIES(IIMP2)%zmin(IZIMP2)
                    ZMAX2    = CORESOURCE_IN%COMPOSITIONS%IMPURITIES(IIMP2)%zmax(IZIMP2)

                    CHECK_FOR_IONIZATION_STATE_CONSISTENCY: IF &
                      (ABS((ZMAX1+ZMIN1)/2.0 - (ZMAX2+ZMIN2)/2.0).LE. 0.25) THEN
! Sz
                       IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%Sz))                      THEN
                          FUN(:)                                            =  0.0_R8
                          IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%Sz(IIMP2)%exp))        THEN                 
                             CALL L3interp(CORESOURCE_IN%VALUES(IVAL)%Sz(IIMP2)%exp(:,izimp2),  RHO2, NRHO2,   &
                                           FUN,                                                 RHO1, NRHO1)
                             CORESOURCE_OUT%VALUES(1)%Sz(IIMP1)%exp(:,izimp1) = CORESOURCE_OUT%VALUES(1)%Sz(IIMP1)%exp(:,izimp1) + FUN
                          END IF
                          FUN(:)                                            =  0.0_R8
                          IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%Sz(IIMP2)%imp))        THEN                  
                             CALL L3interp(CORESOURCE_IN%VALUES(IVAL)%Sz(IIMP2)%imp(:,izimp2),  RHO2, NRHO2,   &
                                           FUN,                                                 RHO1, NRHO1)
                             CORESOURCE_OUT%VALUES(1)%Sz(IIMP1)%imp(:,izimp1) = CORESOURCE_OUT%VALUES(1)%Sz(IIMP1)%imp(:,izimp1) + FUN
                          END IF
                       END IF
! Qz
                       IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%Qz))                      THEN
                          FUN(:)                                            =  0.0_R8
                          IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%Qz(IIMP2)%exp))        THEN                 
                             CALL L3interp(CORESOURCE_IN%VALUES(IVAL)%Qz(IIMP2)%exp(:,izimp2),  RHO2, NRHO2,   &
                                           FUN,                                                 RHO1, NRHO1)
                             CORESOURCE_OUT%VALUES(1)%Qz(IIMP1)%exp(:,izimp1) = CORESOURCE_OUT%VALUES(1)%Qz(IIMP1)%exp(:,izimp1) + FUN
                          END IF
                          FUN(:)                                            =  0.0_R8
                          IF(ASSOCIATED(CORESOURCE_IN%VALUES(IVAL)%Qz(IIMP2)%imp))        THEN                 
                             CALL L3interp(CORESOURCE_IN%VALUES(IVAL)%Qz(IIMP2)%imp(:,izimp2),  RHO2, NRHO2,   &
                                           FUN,                                                 RHO1, NRHO1)
                             CORESOURCE_OUT%VALUES(1)%Qz(IIMP1)%imp(:,izimp1) = CORESOURCE_OUT%VALUES(1)%Qz(IIMP1)%imp(:,izimp1) + FUN
                          END IF
                       END IF
  
                    END IF CHECK_FOR_IONIZATION_STATE_CONSISTENCY

                 END DO INPUT_IONIZATION_STATE
              END DO OUTPUT_IONIZATION_STATE

           END IF CHECK_FOR_NUCLEI_CONSISTENCY

7          CONTINUE

        ENDDO INPUT_IMPURITY_LOOP
     ENDDO OUTPUT_IMPURITY_LOOP

8    IF(ALLOCATED(RHO2))   DEALLOCATE (RHO2)

  END DO LOOP_on_VALUES



! +++ Deallocate local variables:
  IF(ALLOCATED  (NZIMP1)) DEALLOCATE (NZIMP1)
  IF(ALLOCATED  (NCOMP1)) DEALLOCATE (NCOMP1)
  IF(ALLOCATED  (NTYPE1)) DEALLOCATE (NTYPE1)
  IF(ALLOCATED  (NZIMP2)) DEALLOCATE (NZIMP2)
  IF(ALLOCATED  (NCOMP2)) DEALLOCATE (NCOMP2)
  IF(ALLOCATED  (NTYPE2)) DEALLOCATE (NTYPE2)
  IF(ALLOCATED  (RHO1))   DEALLOCATE (RHO1)
  IF(ALLOCATED  (FUN))    DEALLOCATE (FUN)


10 RETURN

  END SUBROUTINE INTERPOLATE_SOURCE
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  SUBROUTINE INTERPOLATE_TRANSP(CORETRANSP_IN, CORETRANSP_OUT, NEGATIVE_DIFF)

    USE EUITM_SCHEMAS
    USE ITM_TYPES
    USE ALLOCATE_DEALLOCATE

    IMPLICIT NONE

    TYPE (TYPE_CORETRANSP) :: CORETRANSP_IN
    TYPE (TYPE_CORETRANSP) :: CORETRANSP_OUT

    INTEGER                :: NVAL2, IVAL
    INTEGER                :: NRHO1, NRHO2
    INTEGER                :: IRHO1, IRHO2
    INTEGER                :: NNUCL1,INUCL1
    INTEGER                :: NNUCL2,INUCL2
    INTEGER                :: NION1, IION1
    INTEGER                :: NION2, IION2
    INTEGER                :: NIMP1, IIMP1        
    INTEGER                :: NIMP2, IIMP2
    INTEGER,   ALLOCATABLE :: NZIMP1(:)
    INTEGER,   ALLOCATABLE :: NZIMP2(:)
    INTEGER                :: IZIMP1,IZIMP2
    INTEGER                :: NNEUT1,INEUT1
    INTEGER                :: NNEUT2,INEUT2
    INTEGER,   ALLOCATABLE :: NCOMP1(:)
    INTEGER,   ALLOCATABLE :: NCOMP2(:)
    INTEGER,   ALLOCATABLE :: NTYPE1(:)
    INTEGER,   ALLOCATABLE :: NTYPE2(:)

    REAL (R8), ALLOCATABLE :: RHO1(:), RHO2(:)
    REAL (R8), ALLOCATABLE :: FUN(:)
    REAL (R8)              :: ZMIN1,   ZMIN2
    REAL (R8)              :: ZMAX1,   ZMAX2

    INTEGER                :: NEGATIVE_DIFF
    INTEGER                :: ICON
!==============================================

    NVAL2      = SIZE(CORETRANSP_IN%VALUES)


    NRHO1      = SIZE(CORETRANSP_OUT%VALUES(1)%rho_tor)

    ALLOCATE (FUN(NRHO1))
    ALLOCATE (RHO1(NRHO1))

    RHO1       = CORETRANSP_OUT%VALUES(1)%rho_tor

    CALL GET_COMP_DIMENSIONS  (CORETRANSP_OUT%COMPOSITIONS, NNUCL1, NION1,  NIMP1,  NZIMP1, NNEUT1, NTYPE1, NCOMP1)
    CALL GET_COMP_DIMENSIONS  (CORETRANSP_IN%COMPOSITIONS,  NNUCL2, NION2,  NIMP2,  NZIMP2, NNEUT2, NTYPE2, NCOMP2)

    IF(.NOT.ASSOCIATED(CORETRANSP_IN%VALUES)) GOTO 10


! +++ Nullify initial profiles:
    CORETRANSP_OUT%VALUES(1)%sigma                           =  0.0_R8
    CORETRANSP_OUT%VALUES(1)%Ne_transp%diff_eff              =  0.0_R8
    CORETRANSP_OUT%VALUES(1)%Ne_transp%vconv_eff             =  0.0_R8
    CORETRANSP_OUT%VALUES(1)%Te_transp%diff_eff              =  0.0_R8
    CORETRANSP_OUT%VALUES(1)%Te_transp%vconv_eff             =  0.0_R8
    CORETRANSP_OUT%VALUES(1)%ni_transp%diff_eff              =  0.0_R8
    CORETRANSP_OUT%VALUES(1)%ni_transp%vconv_eff             =  0.0_R8
    CORETRANSP_OUT%VALUES(1)%Ti_transp%diff_eff              =  0.0_R8
    CORETRANSP_OUT%VALUES(1)%Ti_transp%vconv_eff             =  0.0_R8
    CORETRANSP_OUT%VALUES(1)%Vtor_transp%diff_eff            =  0.0_R8
    CORETRANSP_OUT%VALUES(1)%Vtor_transp%vconv_eff           =  0.0_R8
    DO IIMP1 = 1, NIMP1
       CORETRANSP_OUT%VALUES(1)%Nz_transp(IIMP1)%diff_eff    =  0.0_R8
       CORETRANSP_OUT%VALUES(1)%Nz_transp(IIMP1)%vconv_eff   =  0.0_R8
       CORETRANSP_OUT%VALUES(1)%Tz_transp(IIMP1)%diff_eff    =  0.0_R8
       CORETRANSP_OUT%VALUES(1)%Tz_transp(IIMP1)%vconv_eff   =  0.0_R8
    END DO






! +++ Check information saved to different VALUES:
    LOOP_on_VALUES: DO IVAL = 1, NVAL2
       NRHO2   = SIZE(CORETRANSP_IN%VALUES(1)%rho_tor)
       ALLOCATE (RHO2(NRHO2))
       RHO2    = CORETRANSP_IN%VALUES(IVAL)%rho_tor


! sigma
       FUN(:)                                         =  0.0_R8
       IF(ASSOCIATED(CORETRANSP_IN%VALUES(IVAL)%sigma))                                                   & 
            CALL L3interp(CORETRANSP_IN%VALUES(IVAL)%sigma,                       RHO2,   NRHO2,  &
                          FUN,                                                    RHO1,   NRHO1)
       CORETRANSP_OUT%VALUES(1)%sigma                 =  FUN

! Ne
       FUN(:)                                         =  0.0_R8
       IF(ASSOCIATED(CORETRANSP_IN%VALUES(IVAL)%Ne_transp%diff_eff))       THEN
          DO ICON = 1,3
             CALL L3interp(CORETRANSP_IN%VALUES(IVAL)%Ne_transp%diff_eff(:,ICON),  RHO2,   NRHO2,  &
                           FUN,                                                    RHO1,   NRHO1)
             CORETRANSP_OUT%VALUES(1)%Ne_transp%diff_eff(:,ICON)  = FUN
          END DO
       END IF
       FUN(:)                                         =  0.0_R8
       IF(ASSOCIATED(CORETRANSP_IN%VALUES(IVAL)%Ne_transp%vconv_eff))      THEN
          DO ICON = 1,3
             CALL L3interp(CORETRANSP_IN%VALUES(IVAL)%Ne_transp%vconv_eff(:,ICON), RHO2,   NRHO2,  &
                           FUN,                                                    RHO1,   NRHO1)
             CORETRANSP_OUT%VALUES(1)%Ne_transp%vconv_eff(:,ICON)  = FUN
          END DO
       END IF

! Te 
       FUN(:)                                         =  0.0_R8
       IF(ASSOCIATED(CORETRANSP_IN%VALUES(IVAL)%Te_transp%diff_eff))       THEN                               
          CALL L3interp(CORETRANSP_IN%VALUES(IVAL)%Te_transp%diff_eff,           RHO2,   NRHO2,  &
                        FUN,                                                     RHO1,   NRHO1)
          CORETRANSP_OUT%VALUES(1)%Te_transp%diff_eff           = FUN
       END IF
       FUN(:)                                         =  0.0_R8
       IF(ASSOCIATED(CORETRANSP_IN%VALUES(IVAL)%Te_transp%vconv_eff))      THEN                              
          CALL L3interp(CORETRANSP_IN%VALUES(IVAL)%Te_transp%vconv_eff,          RHO2,   NRHO2,  &
                        FUN,                                                     RHO1,   NRHO1)
          CORETRANSP_OUT%VALUES(1)%Te_transp%vconv_eff          = FUN
       END IF




! +++ IONS
       OUTPUT_IONS_LOOP: DO IION1 = 1, NION1
       INUCL1      = CORETRANSP_OUT%COMPOSITIONS%IONS(IION1)%nucindex

       INPUT_IONS_LOOP: DO IION2 = 1, NION2
          INUCL2   = CORETRANSP_IN%COMPOSITIONS%IONS(IION2)%nucindex 

          IF (INUCL2.LE.0 .OR. INUCL2.GT.SIZE(CORETRANSP_IN%COMPOSITIONS%NUCLEI)) GOTO 5

          CHECK_FOR_IONS_CONSISTENCY: IF &
             (ABS(CORETRANSP_OUT%COMPOSITIONS%NUCLEI(INUCL1)%amn   - CORETRANSP_IN%COMPOSITIONS%NUCLEI(INUCL2)%amn)  .LE. 0.25  .AND.  &
              ABS(CORETRANSP_OUT%COMPOSITIONS%NUCLEI(INUCL1)%zn    - CORETRANSP_IN%COMPOSITIONS%NUCLEI(INUCL2)%zn )  .LE. 0.25  .AND.  &
              ABS(CORETRANSP_OUT%COMPOSITIONS%IONS(IION1)%zion     - CORETRANSP_IN%COMPOSITIONS%IONS(IION2)%zion  )  .LE. 0.25) THEN
! Ni
             FUN(:)                                         =  0.0_R8
             IF(ASSOCIATED(CORETRANSP_IN%VALUES(IVAL)%ni_transp%diff_eff))                      THEN                             
                DO ICON = 1,3
                   CALL L3interp(CORETRANSP_IN%VALUES(IVAL)%ni_transp%diff_eff(:,iion2,ICON),   RHO2,  NRHO2,  &
                                 FUN,                                                           RHO1,  NRHO1)
                   CORETRANSP_OUT%VALUES(1)%ni_transp%diff_eff(:,iion1,ICON) = FUN
                END DO
             END IF
             FUN(:)                                         =  0.0_R8
             IF(ASSOCIATED(CORETRANSP_IN%VALUES(IVAL)%ni_transp%vconv_eff))                     THEN  
                DO ICON = 1,3
                   CALL L3interp(CORETRANSP_IN%VALUES(IVAL)%ni_transp%vconv_eff(:,iion2,ICON),  RHO2,  NRHO2,  &
                                 FUN,                                                           RHO1,  NRHO1)
                   CORETRANSP_OUT%VALUES(1)%ni_transp%vconv_eff(:,iion1,ICON) = FUN
                END DO
             END IF
       
! Ti
             FUN(:)                                         =  0.0_R8
             IF(ASSOCIATED(CORETRANSP_IN%VALUES(IVAL)%ti_transp%diff_eff)) THEN  
                CALL L3interp(CORETRANSP_IN%VALUES(IVAL)%ti_transp%diff_eff(:,iion2),           RHO2,  NRHO2,  &
                              FUN,                                                              RHO1,  NRHO1)
                CORETRANSP_OUT%VALUES(1)%ti_transp%diff_eff(:,iion1) = FUN
             END IF
             FUN(:)                                         =  0.0_R8
             IF(ASSOCIATED(CORETRANSP_IN%VALUES(IVAL)%ti_transp%vconv_eff)) THEN 
                CALL L3interp(CORETRANSP_IN%VALUES(IVAL)%ti_transp%vconv_eff(:,iion2),          RHO2,  NRHO2,  &
                              FUN,                                                              RHO1,  NRHO1)
                CORETRANSP_OUT%VALUES(1)%ti_transp%vconv_eff(:,iion1) = FUN
             END IF

! Vtor
             FUN(:)                                         =  0.0_R8
             IF(ASSOCIATED(CORETRANSP_IN%VALUES(IVAL)%vtor_transp%diff_eff)) THEN
                CALL L3interp(CORETRANSP_IN%VALUES(IVAL)%vtor_transp%diff_eff(:,iion2),         RHO2,  NRHO2,  &
                              FUN,                                                              RHO1,  NRHO1)
                CORETRANSP_OUT%VALUES(1)%vtor_transp%diff_eff(:,iion1) = FUN
             END IF
             FUN(:)                                         =  0.0_R8
             IF(ASSOCIATED(CORETRANSP_IN%VALUES(IVAL)%vtor_transp%vconv_eff)) THEN 
                CALL L3interp(CORETRANSP_IN%VALUES(IVAL)%vtor_transp%vconv_eff(:,iion2),        RHO2,  NRHO2,  &
                              FUN,                                                              RHO1,  NRHO1)
                CORETRANSP_OUT%VALUES(1)%vtor_transp%vconv_eff(:,iion1) = FUN
             END IF

 
          END IF CHECK_FOR_IONS_CONSISTENCY

5         CONTINUE

       END DO INPUT_IONS_LOOP
    END DO OUTPUT_IONS_LOOP




! +++ IMPURITY
    IF (NIMP1*NIMP2.LE.0) GOTO 8
    OUTPUT_IMPURITY_LOOP: DO IIMP1 = 1, NIMP1
       INUCL1      = CORETRANSP_OUT%COMPOSITIONS%IMPURITIES(IIMP1)%nucindex

       INPUT_IMPURITY_LOOP: DO IIMP2 = 1, NIMP2
          INUCL2   = CORETRANSP_IN%COMPOSITIONS%IMPURITIES(IIMP2)%nucindex

          IF (INUCL2.LE.0 .OR. INUCL2.GT.SIZE(CORETRANSP_IN%COMPOSITIONS%NUCLEI)) GOTO 7

          CHECK_FOR_IMPURITY_CONSISTENCY: IF &
             (ABS(CORETRANSP_OUT%COMPOSITIONS%NUCLEI(INUCL1)%amn  - CORETRANSP_IN%COMPOSITIONS%NUCLEI(INUCL2)%amn)  .LE. 0.25   .AND.  &
              ABS(CORETRANSP_OUT%COMPOSITIONS%NUCLEI(INUCL1)%zn   - CORETRANSP_IN%COMPOSITIONS%NUCLEI(INUCL2)%zn )  .LE. 0.25  ) THEN

              OUTPUT_IONIZATION_STATE: DO IZIMP1 = 1, NZIMP1(IIMP1)
                 INPUT_IONIZATION_STATE: DO IZIMP2 = 1, NZIMP2(IIMP2)

                    ZMIN1    = CORETRANSP_OUT%COMPOSITIONS%IMPURITIES(IIMP1)%zmin(IZIMP1)
                    ZMAX1    = CORETRANSP_OUT%COMPOSITIONS%IMPURITIES(IIMP1)%zmax(IZIMP1)
                    ZMIN2    = CORETRANSP_IN%COMPOSITIONS%IMPURITIES(IIMP2)%zmin(IZIMP2)
                    ZMAX2    = CORETRANSP_IN%COMPOSITIONS%IMPURITIES(IIMP2)%zmax(IZIMP2)

                    CHECK_FOR_IONIZATION_STATE_CONSISTENCY: IF &
                      (ABS((ZMAX1+ZMIN1)/2.0 - (ZMAX2+ZMIN2)/2.0).LE. 0.25)                        THEN
! nz
                       IF(ASSOCIATED(CORETRANSP_IN%VALUES(IVAL)%Nz_transp))                        THEN
                          FUN(:)                                         =  0.0_R8
                          IF(ASSOCIATED(CORETRANSP_IN%VALUES(IVAL)%Nz_transp(IIMP2)%diff_eff))  THEN
                             CALL L3interp(CORETRANSP_IN%VALUES(IVAL)%Nz_transp(IIMP2)%diff_eff(:,izimp2),  RHO2, NRHO2,   &
                                              FUN,                                                             RHO1, NRHO1)
                             CORETRANSP_OUT%VALUES(1)%Nz_transp(IIMP1)%diff_eff(:,izimp1) = FUN
                          END IF
                          FUN(:)                                         =  0.0_R8
                          IF(ASSOCIATED(CORETRANSP_IN%VALUES(IVAL)%Nz_transp(IIMP2)%vconv_eff)) THEN 
                             CALL L3interp(CORETRANSP_IN%VALUES(IVAL)%Nz_transp(IIMP2)%vconv_eff(:,izimp2), RHO2, NRHO2,   &
                                              FUN,                                                             RHO1, NRHO1)
                             CORETRANSP_OUT%VALUES(1)%Nz_transp(IIMP1)%vconv_eff(:,izimp1) = FUN
                          END IF
                       END IF
! Tz
                       IF(ASSOCIATED(CORETRANSP_IN%VALUES(IVAL)%Tz_transp))                         THEN
                          FUN(:)                                         =  0.0_R8
                          IF(ASSOCIATED(CORETRANSP_IN%VALUES(IVAL)%Tz_transp(IIMP2)%diff_eff))   THEN
                             CALL L3interp(CORETRANSP_IN%VALUES(IVAL)%Tz_transp(IIMP2)%diff_eff(:,izimp2),  RHO2, NRHO2,   &
                                              FUN,                                                             RHO1, NRHO1)
                             CORETRANSP_OUT%VALUES(1)%Tz_transp(IIMP1)%diff_eff(:,izimp1) = FUN
                          END IF
                          FUN(:)                                         =  0.0_R8
                          IF(ASSOCIATED(CORETRANSP_IN%VALUES(IVAL)%Tz_transp(IIMP2)%vconv_eff))  THEN
                             CALL L3interp(CORETRANSP_IN%VALUES(IVAL)%Tz_transp(IIMP2)%vconv_eff(:,izimp2), RHO2, NRHO2,  &
                                              FUN,                                                             RHO1, NRHO1)
                             CORETRANSP_OUT%VALUES(1)%Tz_transp(IIMP1)%vconv_eff(:,izimp1) = FUN
                          END IF
                       END IF
                    END IF CHECK_FOR_IONIZATION_STATE_CONSISTENCY

                 END DO INPUT_IONIZATION_STATE
              END DO OUTPUT_IONIZATION_STATE

           END IF CHECK_FOR_IMPURITY_CONSISTENCY

 7        CONTINUE

        END DO INPUT_IMPURITY_LOOP
     END DO OUTPUT_IMPURITY_LOOP


 8   IF(ALLOCATED(RHO2))   DEALLOCATE (RHO2)

  END DO LOOP_on_VALUES




! +++ Cut negative diffusion off:
    IF (NEGATIVE_DIFF.NE.0) THEN

       IF(ASSOCIATED(CORETRANSP_OUT%VALUES(1)%ni_transp%diff_eff))   THEN
          DO irho1=1,NRHO1
             DO icon=1,3
                DO iion1=1,nion1
                   IF (CORETRANSP_OUT%VALUES(1)%ni_transp%diff_eff(irho1,iion1,icon).LT.0.0_R8) &
                       CORETRANSP_OUT%VALUES(1)%ni_transp%diff_eff(irho1,iion1,icon)  =  0.0_R8
                ENDDO
             ENDDO
          ENDDO
       END IF

       IF(ASSOCIATED(CORETRANSP_OUT%VALUES(1)%ne_transp%diff_eff))   THEN
          DO irho1=1,NRHO1
             DO icon=1,3
                   IF (CORETRANSP_OUT%VALUES(1)%ne_transp%diff_eff(irho1,icon).LT.0.0_R8)        &
                       CORETRANSP_OUT%VALUES(1)%ne_transp%diff_eff(irho1,icon) =  0.0_R8
             ENDDO
          ENDDO
       END IF

       IF(ASSOCIATED(CORETRANSP_OUT%VALUES(1)%ti_transp%diff_eff))   THEN
          DO irho1=1,NRHO1
             DO iion1=1,nion1
                IF (CORETRANSP_OUT%VALUES(1)%ti_transp%diff_eff(irho1,iion1).LT.0.0_R8)          &
                    CORETRANSP_OUT%VALUES(1)%ti_transp%diff_eff(irho1,iion1) =  0.0_R8
             ENDDO
          ENDDO
       END IF

       IF(ASSOCIATED(CORETRANSP_OUT%VALUES(1)%te_transp%diff_eff))   THEN
          DO irho1=1,NRHO1
             IF (CORETRANSP_OUT%VALUES(1)%te_transp%diff_eff(irho1).LT.0.0_R8)                   &
                 CORETRANSP_OUT%VALUES(1)%te_transp%diff_eff(irho1) =  0.0_R8
          ENDDO
       END IF

       IF(ASSOCIATED(CORETRANSP_OUT%VALUES(1)%vtor_transp%diff_eff)) THEN
          DO irho1=1,NRHO1
             DO iion1=1,nion1
                IF (CORETRANSP_OUT%VALUES(1)%vtor_transp%diff_eff(irho1,iion1).LT.0.0_R8)        &
                    CORETRANSP_OUT%VALUES(1)%vtor_transp%diff_eff(irho1,iion1) =  0.0_R8
             ENDDO
          ENDDO
       END IF

       DO IIMP1 = 1, NIMP1
          IF(ASSOCIATED(CORETRANSP_OUT%VALUES(1)%Nz_transp))                     THEN
             IF (ASSOCIATED(CORETRANSP_OUT%VALUES(1)%Nz_transp(IIMP1)%diff_eff)) THEN
                DO IRHO1=1,NRHO1
                   DO IZIMP1=1,NZIMP1(IIMP1)
                      IF (CORETRANSP_OUT%VALUES(1)%Nz_transp(IIMP1)%diff_eff(IRHO1,IZIMP1).LT.0.0_R8) &
                          CORETRANSP_OUT%VALUES(1)%Nz_transp(IIMP1)%diff_eff(IRHO1,IZIMP1) =  0.0_R8
                   ENDDO
                ENDDO
             ENDIF
          END IF

          IF(ASSOCIATED(CORETRANSP_OUT%VALUES(1)%Tz_transp))                     THEN
             IF (ASSOCIATED(CORETRANSP_OUT%VALUES(1)%Tz_transp(IIMP1)%diff_eff)) THEN
                DO IRHO1=1,NRHO1
                   DO IZIMP1=1,NZIMP1(IIMP1)
                      IF (CORETRANSP_OUT%VALUES(1)%Tz_transp(IIMP1)%diff_eff(IRHO1,IZIMP1).LT.0.0_R8) &
                          CORETRANSP_OUT%VALUES(1)%Tz_transp(IIMP1)%diff_eff(IRHO1,IZIMP1) =  0.0_R8
                   ENDDO
                ENDDO
             ENDIF
          END IF
       ENDDO

    END IF



    IF(ALLOCATED  (NZIMP1)) DEALLOCATE (NZIMP1)
    IF(ALLOCATED  (NCOMP1)) DEALLOCATE (NCOMP1)
    IF(ALLOCATED  (NTYPE1)) DEALLOCATE (NTYPE1)
    IF(ALLOCATED  (NZIMP2)) DEALLOCATE (NZIMP2)
    IF(ALLOCATED  (NCOMP2)) DEALLOCATE (NCOMP2)
    IF(ALLOCATED  (NTYPE2)) DEALLOCATE (NTYPE2)
    IF(ALLOCATED  (RHO1))   DEALLOCATE (RHO1)
    IF(ALLOCATED  (FUN))    DEALLOCATE (FUN)




 10 RETURN

  END SUBROUTINE INTERPOLATE_TRANSP
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  SUBROUTINE INTERPOLATE_PROF(COREPROF_IN, COREPROF_OUT)

    USE EUITM_SCHEMAS
    USE ITM_TYPES
    USE ALLOCATE_DEALLOCATE

    IMPLICIT NONE

    TYPE (TYPE_COREPROF)   :: COREPROF_IN
    TYPE (TYPE_COREPROF)   :: COREPROF_OUT

    INTEGER                :: NRHO1, NRHO2
    INTEGER                :: IRHO1, IRHO2
    INTEGER                :: NNUCL1,INUCL1
    INTEGER                :: NNUCL2,INUCL2
    INTEGER                :: NION1, IION1
    INTEGER                :: NION2, IION2
    INTEGER                :: NIMP1, IIMP1        
    INTEGER                :: NIMP2, IIMP2
    INTEGER,   ALLOCATABLE :: NZIMP1(:)
    INTEGER,   ALLOCATABLE :: NZIMP2(:)
    INTEGER                :: IZIMP1,IZIMP2
    INTEGER                :: NNEUT1,INEUT1
    INTEGER                :: NNEUT2,INEUT2
    INTEGER,   ALLOCATABLE :: NCOMP1(:)
    INTEGER,   ALLOCATABLE :: NCOMP2(:)
    INTEGER,   ALLOCATABLE :: NTYPE1(:)
    INTEGER,   ALLOCATABLE :: NTYPE2(:)

    REAL (R8), ALLOCATABLE :: RHO1(:), RHO2(:)
    REAL (R8)              :: ZMIN1,   ZMIN2
    REAL (R8)              :: ZMAX1,   ZMAX2

    INTEGER                :: NEGATIVE_DIFF
    INTEGER                :: ICON

! +++ Profile derivatives !AF 25.Apr.2016
    INTEGER, PARAMETER     :: CALCULATE_DERIVATIVES_HERE = 1 !this is a temporarily solution to facilitate disabling the calculation of the derivatives from the !AF 25.Apr.2016
                                                             !input profiles, which finally should not be hardcoded here but controlled by the worlflow instead !AF 25.Apr.2016
    REAL (R8), ALLOCATABLE :: Y(:), DY(:) !AF 25.Apr.2016

!==============================================



    NRHO1      = SIZE(COREPROF_OUT%rho_tor)
    NRHO2      = SIZE(COREPROF_IN%rho_tor)

    ALLOCATE (RHO1(NRHO1))
    ALLOCATE (RHO2(NRHO2))

    ALLOCATE (Y(NRHO1)) !AF - 25.Apr.2016
    ALLOCATE (DY(NRHO1)) !AF - 25.Apr.2016

    RHO1       = COREPROF_OUT%rho_tor
    RHO2       = COREPROF_IN%rho_tor

    CALL GET_COMP_DIMENSIONS  (COREPROF_OUT%COMPOSITIONS, NNUCL1, NION1,  NIMP1,  NZIMP1, NNEUT1, NTYPE1, NCOMP1)
    CALL GET_COMP_DIMENSIONS  (COREPROF_IN%COMPOSITIONS,  NNUCL2, NION2,  NIMP2,  NZIMP2, NNEUT2, NTYPE2, NCOMP2)




! +++ Initial values
    COREPROF_OUT%psi%value                       = 0.0_R8
    COREPROF_OUT%psi%ddrho                       = 0.0_R8 !AF - 25.Apr.2016
    COREPROF_OUT%ne%value                        = 0.0_R8
    COREPROF_OUT%ne%ddrho                        = 0.0_R8 !AF - 25.Apr.2016
    COREPROF_OUT%ne%flux%flux_dv                 = 0.0_R8
    COREPROF_OUT%te%value                        = 0.0_R8
    COREPROF_OUT%te%ddrho                        = 0.0_R8 !AF - 25.Apr.2016
    COREPROF_OUT%te%flux%flux_dv                 = 0.0_R8
    COREPROF_OUT%ni%value                        = 0.0_R8
    COREPROF_OUT%ni%ddrho                        = 0.0_R8 !AF - 25.Apr.2016
    COREPROF_OUT%ni%flux%flux_dv                 = 0.0_R8
    COREPROF_OUT%ti%value                        = 0.0_R8
    COREPROF_OUT%ti%ddrho                        = 0.0_R8 !AF - 25.Apr.2016
    COREPROF_OUT%ti%flux%flux_dv                 = 0.0_R8
    COREPROF_OUT%vtor%value                      = 0.0_R8
    COREPROF_OUT%vtor%ddrho                      = 0.0_R8 !AF - 25.Apr.2016
    COREPROF_OUT%vtor%flux%flux_dv               = 0.0_R8
    COREPROF_OUT%profiles1d%q%value              = 0.0_R8
    COREPROF_OUT%profiles1d%zeff%value           = 1.0_R8
    COREPROF_OUT%profiles1d%jtot%value           = 0.0_R8
    COREPROF_OUT%profiles1d%jphi%value           = 0.0_R8
    COREPROF_OUT%psi%sigma_par%value             = 0.0_R8





! +++ 1-D profiles
    IF (ASSOCIATED(COREPROF_IN%psi%value))                                                     &
    CALL L3interp                (COREPROF_IN%psi%value,                        RHO2, NRHO2,   &
                                  COREPROF_OUT%psi%value,                       RHO1, NRHO1)
    IF (ASSOCIATED(COREPROF_IN%psi%ddrho))                                                     &
    CALL L3interp                (COREPROF_IN%psi%ddrho,                        RHO2, NRHO2,   &
                                  COREPROF_OUT%psi%ddrho,                       RHO1, NRHO1)
    IF (ASSOCIATED(COREPROF_IN%psi%sigma_par%value))                                           &
    CALL L3interp                (COREPROF_IN%psi%sigma_par%value,              RHO2, NRHO2,   &
                                  COREPROF_OUT%psi%sigma_par%value,             RHO1, NRHO1)
    IF (ASSOCIATED(COREPROF_IN%ne%value))                                                      &
    CALL L3interp                (COREPROF_IN%ne%value,                         RHO2, NRHO2,   &
                                  COREPROF_OUT%ne%value,                        RHO1, NRHO1)
    IF (ASSOCIATED(COREPROF_IN%ne%ddrho))                                                      & 
    CALL L3interp                (COREPROF_IN%ne%ddrho,                         RHO2, NRHO2,   & 
                                  COREPROF_OUT%ne%ddrho,                        RHO1, NRHO1) 
    IF (ASSOCIATED(COREPROF_IN%ne%flux%flux_dv))                                               &
    CALL L3interp                (COREPROF_IN%ne%flux%flux_dv,                  RHO2, NRHO2,   &
                                  COREPROF_OUT%ne%flux%flux_dv,                 RHO1, NRHO1)
    IF (ASSOCIATED(COREPROF_IN%te%value))                                                      &
    CALL L3interp                (COREPROF_IN%te%value,                         RHO2, NRHO2,   &
                                  COREPROF_OUT%te%value,                        RHO1, NRHO1)
    IF (ASSOCIATED(COREPROF_IN%te%ddrho))                                                      &
    CALL L3interp                (COREPROF_IN%te%ddrho,                         RHO2, NRHO2,   &
                                  COREPROF_OUT%te%ddrho,                        RHO1, NRHO1)
    IF (ASSOCIATED(COREPROF_IN%te%flux%flux_dv))                                               &
    CALL L3interp                (COREPROF_IN%te%flux%flux_dv,                  RHO2, NRHO2,   &
                                  COREPROF_OUT%te%flux%flux_dv,                 RHO1, NRHO1)

    IF (ASSOCIATED(COREPROF_IN%profiles1d%q%value))                                            &
    CALL L3interp                (COREPROF_IN%profiles1d%q%value,               RHO2, NRHO2,   &
                                  COREPROF_OUT%profiles1d%q%value,              RHO1, NRHO1)
    IF (ASSOCIATED(COREPROF_IN%profiles1d%zeff%value))                                         &
    CALL L3interp                (COREPROF_IN%profiles1d%zeff%value,            RHO2, NRHO2,   &
                                  COREPROF_OUT%profiles1d%zeff%value,           RHO1, NRHO1)
    IF (ASSOCIATED(COREPROF_IN%profiles1d%jtot%value))                                         &
    CALL L3interp                (COREPROF_IN%profiles1d%jtot%value,            RHO2, NRHO2,   &
                                  COREPROF_OUT%profiles1d%jtot%value,           RHO1, NRHO1)
    IF (ASSOCIATED(COREPROF_IN%profiles1d%jni%value))                                          &
    CALL L3interp                (COREPROF_IN%profiles1d%jni%value,             RHO2, NRHO2,   &
                                  COREPROF_OUT%profiles1d%jni%value,            RHO1, NRHO1)
    IF (ASSOCIATED(COREPROF_IN%profiles1d%joh%value))                                          &
    CALL L3interp                (COREPROF_IN%profiles1d%joh%value,             RHO2, NRHO2,   &
                                  COREPROF_OUT%profiles1d%joh%value,            RHO1, NRHO1)
    IF (ASSOCIATED(COREPROF_IN%profiles1d%vloop%value))                                        &
    CALL L3interp                (COREPROF_IN%profiles1d%vloop%value,           RHO2, NRHO2,   &
                                  COREPROF_OUT%profiles1d%vloop%value,          RHO1, NRHO1)
    IF (ASSOCIATED(COREPROF_IN%profiles1d%jphi%value))                                         &
    CALL L3interp                (COREPROF_IN%profiles1d%jphi%value,            RHO2, NRHO2,   &
                                  COREPROF_OUT%profiles1d%jphi%value,           RHO1, NRHO1)
    IF (ASSOCIATED(COREPROF_IN%profiles1d%pe%value))                                           &
    CALL L3interp                (COREPROF_IN%profiles1d%pe%value,              RHO2, NRHO2,   &
                                  COREPROF_OUT%profiles1d%pe%value,             RHO1, NRHO1)
    IF (ASSOCIATED(COREPROF_IN%profiles1d%pi_tot%value))                                       &
    CALL L3interp                (COREPROF_IN%profiles1d%pi_tot%value,          RHO2, NRHO2,   &
                                  COREPROF_OUT%profiles1d%pi_tot%value,         RHO1, NRHO1)




! +++ IONS
    OUTPUT_IONS_LOOP: DO IION1 = 1, NION1
       INUCL1      = COREPROF_OUT%COMPOSITIONS%IONS(IION1)%nucindex
       INPUT_IONS_LOOP: DO IION2 = 1, NION2
          INUCL2   = COREPROF_IN%COMPOSITIONS%IONS(IION2)%nucindex

          IF (INUCL2.LE.0 .OR. INUCL2.GT.SIZE(COREPROF_IN%COMPOSITIONS%NUCLEI)) GOTO 5

          CHECK_FOR_IONS_CONSISTENCY: IF &
             (ABS(COREPROF_OUT%COMPOSITIONS%NUCLEI(INUCL1)%amn   - COREPROF_IN%COMPOSITIONS%NUCLEI(INUCL2)%amn)  .LE. 0.25  .AND.  &
              ABS(COREPROF_OUT%COMPOSITIONS%NUCLEI(INUCL1)%zn    - COREPROF_IN%COMPOSITIONS%NUCLEI(INUCL2)%zn )  .LE. 0.25  .AND.  &
              ABS(COREPROF_OUT%COMPOSITIONS%IONS(IION1)%zion     - COREPROF_IN%COMPOSITIONS%IONS(IION2)%zion  )  .LE. 0.25) THEN
! Ni
                   IF (ASSOCIATED(COREPROF_IN%ni%value))                                      &
                   CALL  L3interp(COREPROF_IN%ni%value(:,IION2),               RHO2, NRHO2,   &
                                  COREPROF_OUT%ni%value(:,IION1),              RHO1, NRHO1)
                   IF (ASSOCIATED(COREPROF_IN%ni%ddrho))                                      &
                   CALL  L3interp(COREPROF_IN%ni%ddrho(:,IION2),               RHO2, NRHO2,   &
                                  COREPROF_OUT%ni%ddrho(:,IION1),              RHO1, NRHO1)
                   IF (ASSOCIATED(COREPROF_IN%ni%flux%flux_dv))                               &
                   CALL  L3interp(COREPROF_IN%ni%flux%flux_dv(:,IION2),        RHO2, NRHO2,   &
                                  COREPROF_OUT%ni%flux%flux_dv(:,IION1),       RHO1, NRHO1)
! Ti
                   IF (ASSOCIATED(COREPROF_IN%ti%value))                                      &
                   CALL  L3interp(COREPROF_IN%ti%value(:,IION2),               RHO2, NRHO2,   &
                                  COREPROF_OUT%ti%value(:,IION1),              RHO1, NRHO1)
                   IF (ASSOCIATED(COREPROF_IN%ti%ddrho))                                      & 
                   CALL L3interp(COREPROF_IN%ti%ddrho(:,IION2),                RHO2, NRHO2,   &
                                 COREPROF_OUT%ti%ddrho(:,IION1),               RHO1, NRHO1)
                   IF (ASSOCIATED(COREPROF_IN%ti%flux%flux_dv))                               &
                   CALL  L3interp(COREPROF_IN%ti%flux%flux_dv(:,IION2),        RHO2, NRHO2,   &
                                  COREPROF_OUT%ti%flux%flux_dv(:,IION1),       RHO1, NRHO1)
! Pi
                   IF (ASSOCIATED(COREPROF_IN%profiles1d%pi%value))                           &
                   CALL  L3interp(COREPROF_IN%profiles1d%pi%value(:,IION2),    RHO2, NRHO2,   &
                                  COREPROF_OUT%profiles1d%pi%value(:,IION1),   RHO1, NRHO1)
! Vtor
                   IF (ASSOCIATED(COREPROF_IN%vtor%value))                                    &
                   CALL  L3interp(COREPROF_IN%vtor%value(:,IION2),             RHO2, NRHO2,   &
                                  COREPROF_OUT%vtor%value(:,IION1),            RHO1, NRHO1)
                   IF (ASSOCIATED(COREPROF_IN%vtor%ddrho))                                    &
                   CALL  L3interp(COREPROF_IN%vtor%ddrho(:,IION2),             RHO2, NRHO2,   &
                                  COREPROF_OUT%vtor%ddrho(:,IION1),            RHO1, NRHO1)
                   IF (ASSOCIATED(COREPROF_IN%vtor%flux%flux_dv))                             &
                   CALL  L3interp(COREPROF_IN%vtor%flux%flux_dv(:,IION2),      RHO2, NRHO2,   &
                                  COREPROF_OUT%vtor%flux%flux_dv(:,IION1),     RHO1, NRHO1)
       

             END IF CHECK_FOR_IONS_CONSISTENCY

 5        CONTINUE

       END DO INPUT_IONS_LOOP
    END DO OUTPUT_IONS_LOOP


    !AF - 25.Apr.2016

    IF (CALCULATE_DERIVATIVES_HERE.EQ.1) THEN

      Y = COREPROF_OUT%ne%value
      CALL DERIVN_START(NRHO1,RHO1,Y,DY)
      COREPROF_OUT%ne%ddrho = DY

      Y = COREPROF_OUT%te%value
      CALL DERIVN_START(NRHO1,RHO1,Y,DY)
      COREPROF_OUT%te%ddrho = DY

      DO IION1 = 1, NION1

        Y = COREPROF_OUT%ni%value(:,IION1)
        CALL DERIVN_START(NRHO1,RHO1,Y,DY)
        COREPROF_OUT%ni%ddrho(:,IION1) = DY

        Y = COREPROF_OUT%ti%value(:,IION1)
        CALL DERIVN_START(NRHO1,RHO1,Y,DY)
        COREPROF_OUT%ti%ddrho(:,IION1) = DY

      END DO

    END IF
    
    !AF - 25.Apr.2016 - End


    IF(ALLOCATED  (NZIMP1)) DEALLOCATE (NZIMP1)
    IF(ALLOCATED  (NCOMP1)) DEALLOCATE (NCOMP1)
    IF(ALLOCATED  (NTYPE1)) DEALLOCATE (NTYPE1)
    IF(ALLOCATED  (NZIMP2)) DEALLOCATE (NZIMP2)
    IF(ALLOCATED  (NCOMP2)) DEALLOCATE (NCOMP2)
    IF(ALLOCATED  (NTYPE2)) DEALLOCATE (NTYPE2)
    IF(ALLOCATED  (RHO1))   DEALLOCATE (RHO1)
    IF(ALLOCATED  (RHO2))   DEALLOCATE (RHO2)


    DEALLOCATE (Y) !AF - 25.Apr.2016
    DEALLOCATE (DY) !AF - 25.Apr.2016


    RETURN

  END SUBROUTINE INTERPOLATE_PROF
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  SUBROUTINE INTERPOLATE_IMPUR(COREIMPUR_IN, COREIMPUR_OUT)

    USE EUITM_SCHEMAS
    USE ITM_TYPES
    USE ALLOCATE_DEALLOCATE

    IMPLICIT NONE

    TYPE (TYPE_COREIMPUR)  :: COREIMPUR_IN
    TYPE (TYPE_COREIMPUR)  :: COREIMPUR_OUT

    INTEGER                :: NRHO1, NRHO2
    INTEGER                :: IRHO1, IRHO2
    INTEGER                :: NNUCL1,INUCL1
    INTEGER                :: NNUCL2,INUCL2
    INTEGER                :: NION1, IION1
    INTEGER                :: NION2, IION2
    INTEGER                :: NIMP1, IIMP1        
    INTEGER                :: NIMP2, IIMP2
    INTEGER,   ALLOCATABLE :: NZIMP1(:)
    INTEGER,   ALLOCATABLE :: NZIMP2(:)
    INTEGER                :: IZIMP1,IZIMP2
    INTEGER                :: NNEUT1,INEUT1
    INTEGER                :: NNEUT2,INEUT2
    INTEGER,   ALLOCATABLE :: NCOMP1(:)
    INTEGER,   ALLOCATABLE :: NCOMP2(:)
    INTEGER,   ALLOCATABLE :: NTYPE1(:)
    INTEGER,   ALLOCATABLE :: NTYPE2(:)

    REAL (R8), ALLOCATABLE :: RHO1(:), RHO2(:)
    REAL (R8)              :: ZMIN1,   ZMIN2
    REAL (R8)              :: ZMAX1,   ZMAX2

!==============================================



    NRHO1      = SIZE(COREIMPUR_OUT%rho_tor)
    NRHO2      = SIZE(COREIMPUR_IN%rho_tor)

    ALLOCATE (RHO1(NRHO1))
    ALLOCATE (RHO2(NRHO2))

    RHO1       = COREIMPUR_OUT%rho_tor
    RHO2       = COREIMPUR_IN%rho_tor

    CALL GET_COMP_DIMENSIONS  (COREIMPUR_OUT%COMPOSITIONS, NNUCL1, NION1,  NIMP1,  NZIMP1, NNEUT1, NTYPE1, NCOMP1)
    CALL GET_COMP_DIMENSIONS  (COREIMPUR_IN%COMPOSITIONS,  NNUCL2, NION2,  NIMP2,  NZIMP2, NNEUT2, NTYPE2, NCOMP2)



! +++ IMPURITY
    IF (NIMP1*NIMP2.LE.0) GOTO 8
    OUTPUT_IMPURITY_LOOP: DO IIMP1 = 1, NIMP1
       INUCL1      = COREIMPUR_OUT%COMPOSITIONS%IMPURITIES(IIMP1)%nucindex


       COREIMPUR_OUT%IMPURITY(IIMP1)%nz           =  0.0_R8
       COREIMPUR_OUT%IMPURITY(IIMP1)%flux%flux_dv =  0.0_R8

       INPUT_IMPURITY_LOOP: DO IIMP2 = 1, NIMP2
          INUCL2   = COREIMPUR_IN%COMPOSITIONS%IMPURITIES(IIMP2)%nucindex

          IF (INUCL2.LE.0 .OR. INUCL2.GT.SIZE(COREIMPUR_IN%COMPOSITIONS%NUCLEI)) GOTO 7

          CHECK_IMPURITY_CONSISTENCY: IF &
             (ABS(COREIMPUR_OUT%COMPOSITIONS%NUCLEI(INUCL1)%amn     - COREIMPUR_IN%COMPOSITIONS%NUCLEI(INUCL2)%amn)    .LE. 0.25   .AND.  &
              ABS(COREIMPUR_OUT%COMPOSITIONS%NUCLEI(INUCL1)%zn      - COREIMPUR_IN%COMPOSITIONS%NUCLEI(INUCL2)%zn )    .LE. 0.25  ) THEN

              OUTPUT_IONZATION_STATE: DO IZIMP1 = 1, NZIMP1(IIMP1)
                 INPUT_IONZATION_STATE: DO IZIMP2 = 1, NZIMP2(IIMP2)

                    ZMIN1    = COREIMPUR_OUT%COMPOSITIONS%IMPURITIES(IIMP1)%zmin(IZIMP1)
                    ZMAX1    = COREIMPUR_OUT%COMPOSITIONS%IMPURITIES(IIMP1)%zmax(IZIMP1)
                    ZMIN2    = COREIMPUR_IN%COMPOSITIONS%IMPURITIES(IIMP2)%zmin(IZIMP2)
                    ZMAX2    = COREIMPUR_IN%COMPOSITIONS%IMPURITIES(IIMP2)%zmax(IZIMP2)

                    CHECK_IONZATION_STATE_CONSISTENCY: IF&
                      (ABS((ZMAX1+ZMIN1)/2.0 - (ZMAX2+ZMIN2)/2.0).LE. 0.25) THEN
! nz
                       IF(ASSOCIATED(COREIMPUR_IN%IMPURITY(IIMP2)%nz))                                        &
                          CALL L3interp(COREIMPUR_IN%IMPURITY(IIMP2)%nz(:,izimp2),            RHO2, NRHO2,    &
                                        COREIMPUR_OUT%IMPURITY(IIMP1)%nz(:,izimp1),           RHO1, NRHO1)
                       IF(ASSOCIATED(COREIMPUR_IN%IMPURITY(IIMP2)%flux%flux_dv))                              &
                          CALL L3interp(COREIMPUR_IN%IMPURITY(IIMP2)%flux%flux_dv(:,izimp2),  RHO2, NRHO2,    &
                                        COREIMPUR_OUT%IMPURITY(IIMP1)%flux%flux_dv(:,izimp1), RHO1, NRHO1)
! tz
                       IF(ASSOCIATED(COREIMPUR_IN%IMPURITY(IIMP2)%tz))                                        &
                          CALL L3interp(COREIMPUR_IN%IMPURITY(IIMP2)%tz(:,izimp2),            RHO2, NRHO2,    &
                                        COREIMPUR_OUT%IMPURITY(IIMP1)%tz(:,izimp1),           RHO1, NRHO1)
! z
                       IF(ASSOCIATED(COREIMPUR_IN%IMPURITY(IIMP2)%z))                                         &
                          CALL L3interp(COREIMPUR_IN%IMPURITY(IIMP2)%z(:,izimp2),             RHO2, NRHO2,    &
                                        COREIMPUR_OUT%IMPURITY(IIMP1)%z(:,izimp1),            RHO1, NRHO1)
! zsq
                       IF(ASSOCIATED(COREIMPUR_IN%IMPURITY(IIMP2)%zsq))                                       &
                          CALL L3interp(COREIMPUR_IN%IMPURITY(IIMP2)%zsq(:,izimp2),           RHO2, NRHO2,    &
                                        COREIMPUR_OUT%IMPURITY(IIMP1)%zsq(:,izimp1),          RHO1, NRHO1)
! radiation
                       IF(ASSOCIATED(COREIMPUR_IN%IMPURITY(IIMP2)%diagnostic%radiation%line_rad%profile))                                 &
                          CALL L3interp(COREIMPUR_IN%IMPURITY(IIMP2)%diagnostic%radiation%line_rad%profile(:,izimp2),     RHO2, NRHO2,    &
                                        COREIMPUR_OUT%IMPURITY(IIMP1)%diagnostic%radiation%line_rad%profile(:,izimp1),    RHO1, NRHO1)
                        IF(ASSOCIATED(COREIMPUR_IN%IMPURITY(IIMP2)%diagnostic%radiation%brem_radrec%profile))                             &
                          CALL L3interp(COREIMPUR_IN%IMPURITY(IIMP2)%diagnostic%radiation%brem_radrec%profile(:,izimp2),  RHO2, NRHO2,    &
                                        COREIMPUR_OUT%IMPURITY(IIMP1)%diagnostic%radiation%brem_radrec%profile(:,izimp1), RHO1, NRHO1)
                       IF(ASSOCIATED(COREIMPUR_IN%IMPURITY(IIMP2)%diagnostic%radiation%sum%profile))                                      &
                          CALL L3interp(COREIMPUR_IN%IMPURITY(IIMP2)%diagnostic%radiation%sum%profile(:,izimp2),          RHO2, NRHO2,    &
                                        COREIMPUR_OUT%IMPURITY(IIMP1)%diagnostic%radiation%sum%profile(:,izimp1),         RHO1, NRHO1)
                   END IF CHECK_IONZATION_STATE_CONSISTENCY

                 END DO INPUT_IONZATION_STATE
              END DO OUTPUT_IONZATION_STATE

           ENDIF CHECK_IMPURITY_CONSISTENCY

7          CONTINUE

        END DO INPUT_IMPURITY_LOOP
     END DO OUTPUT_IMPURITY_LOOP








8   IF(ALLOCATED(NZIMP1)) DEALLOCATE (NZIMP1)
    IF(ALLOCATED(NCOMP1)) DEALLOCATE (NCOMP1)
    IF(ALLOCATED(NTYPE1)) DEALLOCATE (NTYPE1)
    IF(ALLOCATED(NZIMP2)) DEALLOCATE (NZIMP2)
    IF(ALLOCATED(NCOMP2)) DEALLOCATE (NCOMP2)
    IF(ALLOCATED(NTYPE2)) DEALLOCATE (NTYPE2)

    IF(ALLOCATED(RHO1))   DEALLOCATE (RHO1)
    IF(ALLOCATED(RHO2))   DEALLOCATE (RHO2)



    RETURN

  END SUBROUTINE INTERPOLATE_IMPUR
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  SUBROUTINE INTERPOLATE_NEUTRALS(CORENEUTRALS_IN, CORENEUTRALS_OUT)

    USE EUITM_SCHEMAS
    USE ITM_TYPES
    USE ALLOCATE_DEALLOCATE

    IMPLICIT NONE

    TYPE (TYPE_CORENEUTRALS)  :: CORENEUTRALS_IN
    TYPE (TYPE_CORENEUTRALS)  :: CORENEUTRALS_OUT

    INTEGER                   :: NRHO1, NRHO2
    INTEGER                   :: IRHO1, IRHO2
    INTEGER                   :: NNUCL1,INUCL1
    INTEGER                   :: NNUCL2,INUCL2
    INTEGER                   :: NION1
    INTEGER                   :: NION2
    INTEGER                   :: NIMP1       
    INTEGER                   :: NIMP2
    INTEGER,      ALLOCATABLE :: NZIMP1(:)
    INTEGER,      ALLOCATABLE :: NZIMP2(:)
    INTEGER                   :: NNEUT1,INEUT1
    INTEGER                   :: NNEUT2,INEUT2
    INTEGER,      ALLOCATABLE :: NCOMP1(:)
    INTEGER,      ALLOCATABLE :: NCOMP2(:)
    INTEGER,      ALLOCATABLE :: NTYPE1(:)
    INTEGER,      ALLOCATABLE :: NTYPE2(:)
    INTEGER                   :: ITYPE1,ICOMP1
    INTEGER                   :: ITYPE2,ICOMP2

    REAL (R8),    ALLOCATABLE :: RHO1(:), RHO2(:)

!==============================================



    NRHO1      = SIZE(CORENEUTRALS_OUT%rho_tor)
    NRHO2      = SIZE(CORENEUTRALS_IN%rho_tor)

    ALLOCATE (RHO1(NRHO1))
    ALLOCATE (RHO2(NRHO2))

    RHO1       = CORENEUTRALS_OUT%rho_tor
    RHO2       = CORENEUTRALS_IN%rho_tor

    CALL GET_COMP_DIMENSIONS  (CORENEUTRALS_OUT%COMPOSITIONS, NNUCL1, NION1,  NIMP1,  NZIMP1, NNEUT1, NTYPE1, NCOMP1)
    CALL GET_COMP_DIMENSIONS  (CORENEUTRALS_IN%COMPOSITIONS,  NNUCL2, NION2,  NIMP2,  NZIMP2, NNEUT2, NTYPE2, NCOMP2)




! +++ NEUTRALS

    DO INEUT1 = 1, NNEUT1
       DO ICOMP1 = 1, NCOMP1(INEUT1)
          INUCL1      = CORENEUTRALS_OUT%COMPOSITIONS%NEUTRALSCOMP(INEUT1)%NEUTCOMP(ICOMP1)%nucindex

          DO INEUT2 = 1, NNEUT2
             DO ICOMP2 = 1, NCOMP2(INEUT2)
                INUCL2   = CORENEUTRALS_IN%COMPOSITIONS%NEUTRALSCOMP(INEUT2)%NEUTCOMP(ICOMP2)%nucindex

                IF (INUCL2.LE.0 .OR. INUCL2.GT.SIZE(CORENEUTRALS_IN%COMPOSITIONS%NUCLEI)) GOTO 7


                IF (ABS(CORENEUTRALS_OUT%COMPOSITIONS%NUCLEI(INUCL1)%amn     - CORENEUTRALS_IN%COMPOSITIONS%NUCLEI(INUCL2)%amn)    .LE. 0.25   .AND.  &
                    ABS(CORENEUTRALS_OUT%COMPOSITIONS%NUCLEI(INUCL1)%zn      - CORENEUTRALS_IN%COMPOSITIONS%NUCLEI(INUCL2)%zn )    .LE. 0.25   .AND.  &
                       (CORENEUTRALS_OUT%COMPOSITIONS%NEUTRALSCOMP(INEUT1)%NEUTCOMP(ICOMP1)%multiplicity .EQ.                                         &
                        CORENEUTRALS_IN%COMPOSITIONS%NEUTRALSCOMP(INEUT2)%NEUTCOMP(ICOMP2)%multiplicity)  )                         THEN

                    DO ITYPE1 = 1, NTYPE1(INEUT1)
                       CORENEUTRALS_OUT%PROFILES(INEUT1)%neutraltype(ITYPE1)%n0%value(:)                          = 0.0_R8
                       CORENEUTRALS_OUT%PROFILES(INEUT1)%neutraltype(ITYPE1)%t0%value(:)                          = 0.0_R8
                       CORENEUTRALS_OUT%PROFILES(INEUT1)%neutraltype(ITYPE1)%v0%toroidal%value(:)                 = 0.0_R8
                       CORENEUTRALS_OUT%PROFILES(INEUT1)%neutraltype(ITYPE1)%v0%poloidal%value(:)                 = 0.0_R8
                       CORENEUTRALS_OUT%PROFILES(INEUT1)%neutraltype(ITYPE1)%v0%radial%value(:)                   = 0.0_R8


                       DO ITYPE2 = 1, NTYPE2(INEUT2)

                          IF (CORENEUTRALS_IN%COMPOSITIONS%NEUTRALSCOMP(INEUT2)%TYPE(ITYPE2)%flag .EQ. &
                              CORENEUTRALS_OUT%COMPOSITIONS%NEUTRALSCOMP(INEUT1)%TYPE(ITYPE1)%flag )  THEN

                             CALL L3interp(CORENEUTRALS_IN%PROFILES(INEUT2)%neutraltype(ITYPE2)%n0%value(:),              RHO2, NRHO2,&
                                           CORENEUTRALS_OUT%PROFILES(INEUT1)%neutraltype(ITYPE1)%n0%value(:),             RHO1, NRHO1)

                             CALL L3interp(CORENEUTRALS_IN%PROFILES(INEUT2)%neutraltype(ITYPE2)%t0%value(:),              RHO2, NRHO2,&
                                           CORENEUTRALS_OUT%PROFILES(INEUT1)%neutraltype(ITYPE1)%t0%value(:),             RHO1, NRHO1)

                             CALL L3interp(CORENEUTRALS_IN%PROFILES(INEUT2)%neutraltype(ITYPE2)%v0%toroidal%value(:),     RHO2, NRHO2,&
                                           CORENEUTRALS_OUT%PROFILES(INEUT1)%neutraltype(ITYPE1)%v0%toroidal%value(:),    RHO1, NRHO1)
   
                             CALL L3interp(CORENEUTRALS_IN%PROFILES(INEUT2)%neutraltype(ITYPE2)%v0%poloidal%value(:),     RHO2, NRHO2,&
                                           CORENEUTRALS_OUT%PROFILES(INEUT1)%neutraltype(ITYPE1)%v0%poloidal%value(:),    RHO1, NRHO1)

                             CALL L3interp(CORENEUTRALS_IN%PROFILES(INEUT2)%neutraltype(ITYPE2)%v0%radial%value(:),       RHO2, NRHO2,&
                                           CORENEUTRALS_OUT%PROFILES(INEUT1)%neutraltype(ITYPE1)%v0%radial%value(:),      RHO1, NRHO1)
                     
                          END IF
                       END DO
                    END DO

                 END IF

 7               CONTINUE
                 
              END DO
           END DO
        END DO
     END DO




    IF(ALLOCATED(NZIMP1)) DEALLOCATE (NZIMP1)
    IF(ALLOCATED(NCOMP1)) DEALLOCATE (NCOMP1)
    IF(ALLOCATED(NTYPE1)) DEALLOCATE (NTYPE1)
    IF(ALLOCATED(NZIMP2)) DEALLOCATE (NZIMP2)
    IF(ALLOCATED(NCOMP2)) DEALLOCATE (NCOMP2)
    IF(ALLOCATED(NTYPE2)) DEALLOCATE (NTYPE2)
    IF(ALLOCATED(RHO1))   DEALLOCATE (RHO1)
    IF(ALLOCATED(RHO2))   DEALLOCATE (RHO2)



    RETURN

  END SUBROUTINE INTERPOLATE_NEUTRALS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  SUBROUTINE INTERPOLATE_NEOCLASSIC(NEOCLASSIC_IN, NEOCLASSIC_OUT)

    USE EUITM_SCHEMAS
    USE ITM_TYPES
    USE ALLOCATE_DEALLOCATE

    IMPLICIT NONE

    TYPE (TYPE_NEOCLASSIC)  :: NEOCLASSIC_IN
    TYPE (TYPE_NEOCLASSIC)  :: NEOCLASSIC_OUT

    INTEGER                :: NRHO1, NRHO2
    INTEGER                :: IRHO1, IRHO2
    INTEGER                :: NNUCL1,INUCL1
    INTEGER                :: NNUCL2,INUCL2
    INTEGER                :: NION1, IION1
    INTEGER                :: NION2, IION2
    INTEGER                :: NIMP1, IIMP1        
    INTEGER                :: NIMP2, IIMP2
    INTEGER,   ALLOCATABLE :: NZIMP1(:)
    INTEGER,   ALLOCATABLE :: NZIMP2(:)
    INTEGER                :: IZIMP1,IZIMP2
    INTEGER                :: NNEUT1,INEUT1
    INTEGER                :: NNEUT2,INEUT2
    INTEGER,   ALLOCATABLE :: NCOMP1(:)
    INTEGER,   ALLOCATABLE :: NCOMP2(:)
    INTEGER,   ALLOCATABLE :: NTYPE1(:)
    INTEGER,   ALLOCATABLE :: NTYPE2(:)

    REAL (R8), ALLOCATABLE :: RHO1(:), RHO2(:)
    REAL (R8)              :: ZMIN1,   ZMIN2
    REAL (R8)              :: ZMAX1,   ZMAX2

!==============================================



    NRHO1      = SIZE(NEOCLASSIC_OUT%rho_tor)
    NRHO2      = SIZE(NEOCLASSIC_IN%rho_tor)

    ALLOCATE (RHO1(NRHO1))
    ALLOCATE (RHO2(NRHO2))

    RHO1       = NEOCLASSIC_OUT%rho_tor
    RHO2       = NEOCLASSIC_IN%rho_tor

    CALL GET_COMP_DIMENSIONS  (NEOCLASSIC_OUT%COMPOSITIONS, NNUCL1, NION1,  NIMP1,  NZIMP1, NNEUT1, NTYPE1, NCOMP1)
    CALL GET_COMP_DIMENSIONS  (NEOCLASSIC_IN%COMPOSITIONS,  NNUCL2, NION2,  NIMP2,  NZIMP2, NNEUT2, NTYPE2, NCOMP2)



! +++ Nullify initial profiles
    NEOCLASSIC_OUT%sigma                    =  0.0_R8
    NEOCLASSIC_OUT%jboot                    =  0.0_R8
    NEOCLASSIC_OUT%er                       =  0.0_R8
    NEOCLASSIC_OUT%vpol                     =  0.0_R8
    NEOCLASSIC_OUT%Ne_neo%diff_eff          =  0.0_R8
    NEOCLASSIC_OUT%Ne_neo%vconv_eff         =  0.0_R8
    NEOCLASSIC_OUT%Te_neo%diff_eff          =  0.0_R8
    NEOCLASSIC_OUT%Te_neo%vconv_eff         =  0.0_R8
    NEOCLASSIC_OUT%Mtor_neo%diff_eff        =  0.0_R8
    NEOCLASSIC_OUT%Mtor_neo%vconv_eff       =  0.0_R8



! sigma
    IF(ASSOCIATED(NEOCLASSIC_IN%sigma))                                        & 
       CALL L3interp(NEOCLASSIC_IN%sigma,                      RHO2,   NRHO2,  &
                     NEOCLASSIC_OUT%sigma,                     RHO1,   NRHO1)

! jboot
    IF(ASSOCIATED(NEOCLASSIC_IN%jboot))                                        & 
       CALL L3interp(NEOCLASSIC_IN%jboot,                      RHO2,   NRHO2,  &
                     NEOCLASSIC_OUT%jboot,                     RHO1,   NRHO1)

! er
    IF(ASSOCIATED(NEOCLASSIC_IN%er))                                           & 
       CALL L3interp(NEOCLASSIC_IN%er,                         RHO2,   NRHO2,  &
                     NEOCLASSIC_OUT%er,                        RHO1,   NRHO1)

! vpol
    IF(ASSOCIATED(NEOCLASSIC_IN%vpol))                                         & 
       CALL L3interp(NEOCLASSIC_IN%vpol,                       RHO2,   NRHO2,  &
                     NEOCLASSIC_OUT%vpol,                      RHO1,   NRHO1)

! Ne
    IF(ASSOCIATED(NEOCLASSIC_IN%Ne_neo%diff_eff))                              &
       CALL L3interp(NEOCLASSIC_IN%Ne_neo%diff_eff,            RHO2,   NRHO2,  &
                     NEOCLASSIC_OUT%Ne_neo%diff_eff,           RHO1,   NRHO1)
    IF(ASSOCIATED(NEOCLASSIC_IN%Ne_neo%vconv_eff))                             &
       CALL L3interp(NEOCLASSIC_IN%Ne_neo%vconv_eff,           RHO2,   NRHO2,  &
                     NEOCLASSIC_OUT%Ne_neo%vconv_eff,          RHO1,   NRHO1)

! Te 
    IF(ASSOCIATED(NEOCLASSIC_IN%Te_neo%diff_eff))                              &
       CALL L3interp(NEOCLASSIC_IN%Te_neo%diff_eff,            RHO2,   NRHO2,  &
                     NEOCLASSIC_OUT%Te_neo%diff_eff,           RHO1,   NRHO1)
    IF(ASSOCIATED(NEOCLASSIC_IN%Te_neo%vconv_eff))                             &
       CALL L3interp(NEOCLASSIC_IN%Te_neo%vconv_eff,           RHO2,   NRHO2,  &
                     NEOCLASSIC_OUT%Te_neo%vconv_eff,          RHO1,   NRHO1)
! Mtor
    IF(ASSOCIATED(NEOCLASSIC_IN%mtor_neo%diff_eff))                            &  
       CALL L3interp(NEOCLASSIC_IN%mtor_neo%diff_eff,          RHO2,  NRHO2,   &
                     NEOCLASSIC_OUT%mtor_neo%diff_eff,         RHO1,  NRHO1)
    IF(ASSOCIATED(NEOCLASSIC_IN%mtor_neo%vconv_eff))                           &  
       CALL L3interp(NEOCLASSIC_IN%mtor_neo%vconv_eff,         RHO2,  NRHO2,   &
                     NEOCLASSIC_OUT%mtor_neo%vconv_eff,        RHO1,  NRHO1)



! +++ IONS
    NEOCLASSIC_OUT%ni_neo%diff_eff        =  0.0_R8
    NEOCLASSIC_OUT%ni_neo%vconv_eff       =  0.0_R8
    NEOCLASSIC_OUT%Ti_neo%diff_eff        =  0.0_R8
    NEOCLASSIC_OUT%Ti_neo%vconv_eff       =  0.0_R8



    OUTPUT_ION_LOOP: DO IION1 = 1, NION1
       INUCL1      = NEOCLASSIC_OUT%COMPOSITIONS%IONS(IION1)%nucindex
       INPUT_ION_LOOP: DO IION2 = 1, NION2
          INUCL2   = NEOCLASSIC_IN%COMPOSITIONS%IONS(IION2)%nucindex 

          IF (INUCL2.LE.0 .OR. INUCL2.GT.SIZE(NEOCLASSIC_IN%COMPOSITIONS%NUCLEI)) GOTO 5

          CHECK_FOR_IONS_CONSISTENCY: IF &
             (ABS(NEOCLASSIC_OUT%COMPOSITIONS%NUCLEI(INUCL1)%amn   - NEOCLASSIC_IN%COMPOSITIONS%NUCLEI(INUCL2)%amn)  .LE. 0.25  .AND.  &
              ABS(NEOCLASSIC_OUT%COMPOSITIONS%NUCLEI(INUCL1)%zn    - NEOCLASSIC_IN%COMPOSITIONS%NUCLEI(INUCL2)%zn )  .LE. 0.25  .AND.  &
              ABS(NEOCLASSIC_OUT%COMPOSITIONS%IONS(IION1)%zion     - NEOCLASSIC_IN%COMPOSITIONS%IONS(IION2)%zion  )  .LE. 0.25) THEN
! Ni
              IF(ASSOCIATED(NEOCLASSIC_IN%ni_neo%diff_eff))                                     &                           
                    CALL L3interp(NEOCLASSIC_IN%ni_neo%diff_eff(:,iion2),        RHO2,  NRHO2,  &
                                  NEOCLASSIC_OUT%ni_neo%diff_eff(:,iion1),       RHO1,  NRHO1)
              IF(ASSOCIATED(NEOCLASSIC_IN%ni_neo%vconv_eff))                                    & 
                    CALL L3interp(NEOCLASSIC_IN%ni_neo%vconv_eff(:,iion2),       RHO2,  NRHO2,  &
                                  NEOCLASSIC_OUT%ni_neo%vconv_eff(:,iion1),      RHO1,  NRHO1)
       
! Ti
              IF(ASSOCIATED(NEOCLASSIC_IN%ti_neo%diff_eff))                                     &  
                 CALL L3interp(NEOCLASSIC_IN%ti_neo%diff_eff(:,iion2),           RHO2,  NRHO2,  &
                               NEOCLASSIC_OUT%ti_neo%diff_eff(:,iion1),          RHO1,  NRHO1)
              IF(ASSOCIATED(NEOCLASSIC_IN%ti_neo%vconv_eff))                                    &  
                 CALL L3interp(NEOCLASSIC_IN%ti_neo%vconv_eff(:,iion2),          RHO2,  NRHO2,  &
                               NEOCLASSIC_OUT%ti_neo%vconv_eff(:,iion1),         RHO1,  NRHO1)

          END IF CHECK_FOR_IONS_CONSISTENCY

 5        CONTINUE

       END DO INPUT_ION_LOOP
    END DO OUTPUT_ION_LOOP




! +++ IMPURITY
    IF (NIMP1*NIMP2.LE.0) GOTO 8
    OUTPUT_IMPURITY_LOOP: DO IIMP1 = 1, NIMP1
       INUCL1      = NEOCLASSIC_OUT%COMPOSITIONS%IMPURITIES(IIMP1)%nucindex

       NEOCLASSIC_OUT%Nz_neo(IIMP1)%diff_eff         =  0.0_R8
       NEOCLASSIC_OUT%Nz_neo(IIMP1)%vconv_eff        =  0.0_R8
       NEOCLASSIC_OUT%Tz_neo(IIMP1)%diff_eff         =  0.0_R8
       NEOCLASSIC_OUT%Tz_neo(IIMP1)%vconv_eff        =  0.0_R8

       INPUT_IMPURITY_LOOP: DO IIMP2 = 1, NIMP2
          INUCL2   = NEOCLASSIC_IN%COMPOSITIONS%IMPURITIES(IIMP2)%nucindex

          IF (INUCL2.LE.0 .OR. INUCL2.GT.SIZE(NEOCLASSIC_IN%COMPOSITIONS%NUCLEI)) GOTO 7

          CHECK_FOR_IMPURITY_CONSISTENCY: IF &
             (ABS(NEOCLASSIC_OUT%COMPOSITIONS%NUCLEI(INUCL1)%amn - NEOCLASSIC_IN%COMPOSITIONS%NUCLEI(INUCL2)%amn) .LE. 0.25   .AND.  &
              ABS(NEOCLASSIC_OUT%COMPOSITIONS%NUCLEI(INUCL1)%zn  - NEOCLASSIC_IN%COMPOSITIONS%NUCLEI(INUCL2)%zn ) .LE. 0.25  ) THEN

              OUTPUT_IONIZATION_STATES: DO IZIMP1 = 1, NZIMP1(IIMP1)
                 INPUT_IONIZATION_STATES: DO IZIMP2 = 1, NZIMP2(IIMP2)

                    ZMIN1    = NEOCLASSIC_OUT%COMPOSITIONS%IMPURITIES(IIMP1)%zmin(IZIMP1)
                    ZMAX1    = NEOCLASSIC_OUT%COMPOSITIONS%IMPURITIES(IIMP1)%zmax(IZIMP1)
                    ZMIN2    = NEOCLASSIC_IN%COMPOSITIONS%IMPURITIES(IIMP2)%zmin(IZIMP2)
                    ZMAX2    = NEOCLASSIC_IN%COMPOSITIONS%IMPURITIES(IIMP2)%zmax(IZIMP2)

                    IF(ABS((ZMAX1+ZMIN1)/2.0 - (ZMAX2+ZMIN2)/2.0).LE. 0.25) THEN
! nz
                       IF(ASSOCIATED(NEOCLASSIC_IN%Nz_neo))               THEN
                          IF(ASSOCIATED(NEOCLASSIC_IN%Nz_neo(IIMP2)%diff_eff))                             &
                             CALL L3interp(NEOCLASSIC_IN%Nz_neo(IIMP2)%diff_eff(:,izimp2),  RHO2, NRHO2,   &
                                           NEOCLASSIC_OUT%Nz_neo(IIMP1)%diff_eff(:,izimp1), RHO1, NRHO1)
                          IF(ASSOCIATED(NEOCLASSIC_IN%Nz_neo(IIMP2)%vconv_eff))                            & 
                             CALL L3interp(NEOCLASSIC_IN%Nz_neo(IIMP2)%vconv_eff(:,izimp2),  RHO2, NRHO2,  &
                                           NEOCLASSIC_OUT%Nz_neo(IIMP1)%vconv_eff(:,izimp1), RHO1, NRHO1)
                       END IF
! Tz
                       IF(ASSOCIATED(NEOCLASSIC_IN%Tz_neo))               THEN
                          IF(ASSOCIATED(NEOCLASSIC_IN%Tz_neo(IIMP2)%diff_eff))                             &
                             CALL L3interp(NEOCLASSIC_IN%Tz_neo(IIMP2)%diff_eff(:,izimp2),  RHO2, NRHO2,   &
                                           NEOCLASSIC_OUT%Tz_neo(IIMP1)%diff_eff(:,izimp1), RHO1, NRHO1)
                          IF(ASSOCIATED(NEOCLASSIC_IN%Tz_neo(IIMP2)%vconv_eff))                            &
                             CALL L3interp(NEOCLASSIC_IN%Tz_neo(IIMP2)%vconv_eff(:,izimp2),  RHO2, NRHO2,  &
                                           NEOCLASSIC_OUT%Tz_neo(IIMP1)%vconv_eff(:,izimp1), RHO1, NRHO1)
                       END IF
                   ENDIF

                END DO INPUT_IONIZATION_STATES
             END DO OUTPUT_IONIZATION_STATES

          END IF CHECK_FOR_IMPURITY_CONSISTENCY

 7        CONTINUE 

       END DO INPUT_IMPURITY_LOOP
    END DO OUTPUT_IMPURITY_LOOP





 8  IF(ALLOCATED(NZIMP1)) DEALLOCATE (NZIMP1)
    IF(ALLOCATED(NCOMP1)) DEALLOCATE (NCOMP1)
    IF(ALLOCATED(NTYPE1)) DEALLOCATE (NTYPE1)
    IF(ALLOCATED(NZIMP2)) DEALLOCATE (NZIMP2)
    IF(ALLOCATED(NCOMP2)) DEALLOCATE (NCOMP2)
    IF(ALLOCATED(NTYPE2)) DEALLOCATE (NTYPE2)
    IF(ALLOCATED(RHO1))   DEALLOCATE (RHO1)
    IF(ALLOCATED(RHO2))   DEALLOCATE (RHO2)



    RETURN

  END SUBROUTINE INTERPOLATE_NEOCLASSIC
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


!AF - 25.Apr.2016 - to get the profile derivatives at the start of the run by derivating the profiles (and ignoring derivatives in the input run if they exist)

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> These subroutines calculate first and second derivatives, DY1 and DY2, of function Y respect to argument X  
!>
!> These subroutines have been extracted from RITM code
!> and consist of derivation and integration routines
!>
!> \author D.Kalupin, R.Stankiewicz
!>
!> \version "$Id: main_plasma.f90 1587 2014-09-26 09:49:33Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

SUBROUTINE DERIVN_START(N,X,Y,DY1)

!-------------------------------------------------------!
!       These subroutines calculate first and second    !
!       derivatives, DY1 and DY2, of function Y respect !
!       to argument X                                   !
!-------------------------------------------------------!

  use itm_types
  IMPLICIT NONE

  INTEGER :: N                                          ! number of radial points (input)
  INTEGER :: I

  REAL (R8) :: X(N), &                                  ! argument array (input)
       Y(N), &                                  ! function array (input)
       DY1(N)                                   ! function derivative array (output)
  REAL (R8) :: H(N),DY2(N)

  REAL (R8) :: DDY !AF 6.Oct.2011

  DO I=1,N-1
     H(I)=X(I+1)-X(I)
  END DO

  DO I=2,N-1
     DY1(I)=((Y(I+1)-Y(I))*H(I-1)/H(I)+(Y(I)-Y(I-1))*H(I)/H(I-1)) &
          /(H(I)+H(I-1))
!     DY2(I)=2.e0_R8*((Y(I-1)-Y(I))/H(I-1)+(Y(I+1)-Y(I))/H(I)) & !AF 6.Oct.2011
!          /(H(I)+H(I-1))
  END DO

!  DY1(1)=DY1(2)-DY2(2)*H(1) !AF 6.Oct.2011
!  DY1(N)=DY1(N-1)+DY2(N-1)*H(N-1) !AF 6.Oct.2011

   DDY = 2.e0_R8*((Y(1)-Y(2))/H(1)+(Y(3)-Y(2))/H(2))/(H(2)+H(1))
   DY1(1) = DY1(2)-DDY*H(1)
   DDY = 2.e0_R8*((Y(N-2)-Y(N-1))/H(N-2)+(Y(N)-Y(N-1))/H(N-1))/(H(N-1)+H(N-2))
   DY1(N) = DY1(N-1)+DDY*H(N-1)

  RETURN
END SUBROUTINE DERIVN_START


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  SUBROUTINE INTERPOLATE_DELTA(COREDELTA_IN, COREDELTA_OUT)

    USE EUITM_SCHEMAS
    USE ITM_TYPES
    USE ALLOCATE_DEALLOCATE

    IMPLICIT NONE

    TYPE (TYPE_COREDELTA)  :: COREDELTA_IN
    TYPE (TYPE_COREDELTA)  :: COREDELTA_OUT

    INTEGER                :: NVAL1, NVAL2
    INTEGER                :: IVAL
    INTEGER                :: NRHO1, NRHO2
    INTEGER                :: IRHO1, IRHO2
    INTEGER                :: NNUCL1,INUCL1
    INTEGER                :: NNUCL2,INUCL2
    INTEGER                :: NION1, IION1
    INTEGER                :: NION2, IION2
    INTEGER                :: NIMP1, IIMP1        
    INTEGER                :: NIMP2, IIMP2
    INTEGER,   ALLOCATABLE :: NZIMP1(:)
    INTEGER,   ALLOCATABLE :: NZIMP2(:)
    INTEGER                :: IZIMP1,IZIMP2
    INTEGER                :: NNEUT1,INEUT1
    INTEGER                :: NNEUT2,INEUT2
    INTEGER,   ALLOCATABLE :: NCOMP1(:)
    INTEGER,   ALLOCATABLE :: NCOMP2(:)
    INTEGER,   ALLOCATABLE :: NTYPE1(:)
    INTEGER,   ALLOCATABLE :: NTYPE2(:)

    REAL (R8), ALLOCATABLE :: FUN(:)
    REAL (R8), ALLOCATABLE :: RHO1(:), RHO2(:)
    REAL (R8)              :: ZMIN1,   ZMIN2
    REAL (R8)              :: ZMAX1,   ZMAX2
!==============================================

    NVAL1              = 1
    NVAL2              = SIZE(COREDELTA_IN%VALUES)


    NRHO1              = SIZE(COREDELTA_OUT%VALUES(1)%rho_tor)

    ALLOCATE (FUN(NRHO1))
    ALLOCATE (RHO1(NRHO1))

    RHO1               = COREDELTA_OUT%VALUES(1)%rho_tor
 
    CALL GET_COMP_DIMENSIONS  (COREDELTA_OUT%COMPOSITIONS, NNUCL1, NION1,  NIMP1,  NZIMP1, NNEUT1, NTYPE1, NCOMP1)
    CALL GET_COMP_DIMENSIONS  (COREDELTA_IN%COMPOSITIONS,  NNUCL2, NION2,  NIMP2,  NZIMP2, NNEUT2, NTYPE2, NCOMP2)

    IF(.NOT.ASSOCIATED(COREDELTA_IN%VALUES)) GOTO 10


! +++ Nulify initial profiles:
    COREDELTA_OUT%VALUES(1)%delta_psi                      =  0.0_R8
    COREDELTA_OUT%VALUES(1)%delta_te                       =  0.0_R8
    COREDELTA_OUT%VALUES(1)%delta_ti                       =  0.0_R8
    COREDELTA_OUT%VALUES(1)%delta_ne                       =  0.0_R8
    COREDELTA_OUT%VALUES(1)%delta_ni                       =  0.0_R8
    COREDELTA_OUT%VALUES(1)%delta_vtor                     =  0.0_R8
    IF (NIMP1.GE.1)    THEN
       DO IIMP1 = 1, NIMP1
          COREDELTA_OUT%VALUES(1)%IMPURITY(IIMP1)%delta_nz =  0.0_R8
          COREDELTA_OUT%VALUES(1)%IMPURITY(IIMP1)%delta_tz =  0.0_R8
       END DO
    END IF

! +++ Check information saved to different VALUES:
    LOOP_on_VALUES: DO IVAL = 1, NVAL2
       NRHO2   = SIZE(COREDELTA_IN%VALUES(IVAL)%rho_tor)
       ALLOCATE (RHO2(NRHO2))
       RHO2    = COREDELTA_IN%VALUES(IVAL)%rho_tor



! psi
       FUN(:)                                      =  0.0_R8
       IF(ASSOCIATED(COREDELTA_IN%VALUES(IVAL)%delta_psi))     THEN                  
          CALL L3interp(COREDELTA_IN%VALUES(IVAL)%delta_psi,   RHO2,   NRHO2,  &
                        FUN,                                   RHO1,   NRHO1)
          COREDELTA_OUT%VALUES(1)%delta_psi           = COREDELTA_OUT%VALUES(1)%delta_psi + FUN
       END IF

! Te
       FUN(:)                                      =  0.0_R8
       IF(ASSOCIATED(COREDELTA_IN%VALUES(IVAL)%delta_te))      THEN                  
          CALL L3interp(COREDELTA_IN%VALUES(IVAL)%delta_te,    RHO2,   NRHO2,  &
                        FUN,                                   RHO1,   NRHO1)
          COREDELTA_OUT%VALUES(1)%delta_te            = COREDELTA_OUT%VALUES(1)%delta_te + FUN
       END IF

! Ne
       FUN(:)                                      =  0.0_R8
       IF(ASSOCIATED(COREDELTA_IN%VALUES(IVAL)%delta_ne))      THEN                  
          CALL L3interp(COREDELTA_IN%VALUES(IVAL)%delta_ne,    RHO2,   NRHO2,  &
                        FUN,                                   RHO1,   NRHO1)
          COREDELTA_OUT%VALUES(1)%delta_ne            = COREDELTA_OUT%VALUES(1)%delta_ne + FUN
       END IF



! +++ IONS
       OUTPUT_ION_LOOP: DO IION1 = 1, NION1
          INUCL1      = COREDELTA_OUT%COMPOSITIONS%IONS(IION1)%nucindex
          INPUT_ION_LOOP:DO IION2 = 1, NION2
             INUCL2   = COREDELTA_IN%COMPOSITIONS%IONS(IION2)%nucindex 

             IF (INUCL2.LE.0 .OR. INUCL2.GT.SIZE(COREDELTA_IN%COMPOSITIONS%NUCLEI)) GOTO 5

             CHECK_FOR_IONS_CONSISTENCY: IF &
                 (ABS(COREDELTA_OUT%COMPOSITIONS%NUCLEI(INUCL1)%amn   - COREDELTA_IN%COMPOSITIONS%NUCLEI(INUCL2)%amn)  .LE. 0.25  .AND.  &
                  ABS(COREDELTA_OUT%COMPOSITIONS%NUCLEI(INUCL1)%zn    - COREDELTA_IN%COMPOSITIONS%NUCLEI(INUCL2)%zn )  .LE. 0.25  .AND.  &
                  ABS(COREDELTA_OUT%COMPOSITIONS%IONS(IION1)%zion     - COREDELTA_IN%COMPOSITIONS%IONS(IION2)%zion  )  .LE. 0.25)  THEN
! Ni
                FUN(:)                                            =  0.0_R8
                IF(ASSOCIATED(COREDELTA_IN%VALUES(IVAL)%delta_ni)) THEN                                                    
                   CALL L3interp(COREDELTA_IN%VALUES(IVAL)%delta_ni(:,iion2),  RHO2,  NRHO2,  &
                                 FUN,                                          RHO1,  NRHO1)
                   COREDELTA_OUT%VALUES(1)%delta_ni(:,iion1)         = COREDELTA_OUT%VALUES(1)%delta_ni(:,iion1) + FUN
                END IF
       
! Ti
                FUN(:)                                            =  0.0_R8
                IF(ASSOCIATED(COREDELTA_IN%VALUES(IVAL)%delta_ti)) THEN                                                    
                   CALL L3interp(COREDELTA_IN%VALUES(IVAL)%delta_ti(:,iion2),  RHO2,  NRHO2,  &
                                 FUN,                                          RHO1,  NRHO1)
                   COREDELTA_OUT%VALUES(1)%delta_ti(:,iion1)         = COREDELTA_OUT%VALUES(1)%delta_ti(:,iion1) + FUN
                END IF
! Vtor
                FUN(:)                                            =  0.0_R8
                IF(ASSOCIATED(COREDELTA_IN%VALUES(IVAL)%delta_vtor)) THEN                                                    
                   CALL L3interp(COREDELTA_IN%VALUES(IVAL)%delta_vtor(:,iion2),RHO2,  NRHO2,  &
                                 FUN,                                          RHO1,  NRHO1)
                   COREDELTA_OUT%VALUES(1)%delta_vtor(:,iion1)       = COREDELTA_OUT%VALUES(1)%delta_vtor(:,iion1) + FUN
                END IF
             END IF CHECK_FOR_IONS_CONSISTENCY


5            CONTINUE

          END DO INPUT_ION_LOOP
       END DO OUTPUT_ION_LOOP




!      IMPURITY:
       IF (NIMP1*NIMP2.LE.0) GOTO 8
       OUTPUT_IMPURITY_LOOP: DO IIMP1 = 1, NIMP1
          INUCL1      = COREDELTA_OUT%COMPOSITIONS%IMPURITIES(IIMP1)%nucindex

          INPUT_IMPURITY_LOOP: DO IIMP2 = 1, NIMP2
             INUCL2   = COREDELTA_IN%COMPOSITIONS%IMPURITIES(IIMP2)%nucindex

             IF (INUCL2.LE.0 .OR. INUCL2.GT.SIZE(COREDELTA_IN%COMPOSITIONS%NUCLEI)) GOTO 7

             CHECK_FOR_IMPURITY_CONSISTENCY: IF &
                 (ABS(COREDELTA_OUT%COMPOSITIONS%NUCLEI(INUCL1)%amn - COREDELTA_IN%COMPOSITIONS%NUCLEI(INUCL2)%amn)  .LE. 0.25   .AND.  &
                  ABS(COREDELTA_OUT%COMPOSITIONS%NUCLEI(INUCL1)%zn  - COREDELTA_IN%COMPOSITIONS%NUCLEI(INUCL2)%zn )  .LE. 0.25  ) THEN

                OUTPUT_IONIZATION_STATES: DO IZIMP1 = 1, NZIMP1(IIMP1)
                   INPUT_IONIZATION_STATES: DO IZIMP2 = 1, NZIMP2(IIMP2)

                      ZMIN1    = COREDELTA_OUT%COMPOSITIONS%IMPURITIES(IIMP1)%zmin(IZIMP1)
                      ZMAX1    = COREDELTA_OUT%COMPOSITIONS%IMPURITIES(IIMP1)%zmax(IZIMP1)
                      ZMIN2    = COREDELTA_IN%COMPOSITIONS%IMPURITIES(IIMP2)%zmin(IZIMP2)
                      ZMAX2    = COREDELTA_IN%COMPOSITIONS%IMPURITIES(IIMP2)%zmax(IZIMP2)

                      IF(ABS((ZMAX1+ZMIN1)/2.0 - (ZMAX2+ZMIN2)/2.0).LE. 0.25) THEN
                         IF(ASSOCIATED(COREDELTA_IN%VALUES(IVAL)%IMPURITY))        THEN                 
! nz
                            FUN(:)                                            =  0.0_R8
                            IF(ASSOCIATED(COREDELTA_IN%VALUES(IVAL)%IMPURITY(IIMP2)%delta_nz))        THEN                 
                               CALL L3interp(COREDELTA_IN%VALUES(IVAL)%IMPURITY(IIMP2)%delta_nz(:,IZIMP2),  RHO2, NRHO2,   &
                                             FUN,                                                           RHO1, NRHO1)
                               COREDELTA_OUT%VALUES(1)%IMPURITY(IIMP1)%delta_nz(:,izimp1) = COREDELTA_OUT%VALUES(1)%IMPURITY(IIMP1)%delta_nz(:,IZIMP1) + FUN
                            END IF
! tz
                            FUN(:)                                            =  0.0_R8
                            IF(ASSOCIATED(COREDELTA_IN%VALUES(IVAL)%IMPURITY(IIMP2)%delta_tz))        THEN                 
                               CALL L3interp(COREDELTA_IN%VALUES(IVAL)%IMPURITY(IIMP2)%delta_tz(:,IZIMP2),  RHO2, NRHO2,   &
                                             FUN,                                                           RHO1, NRHO1)
                               COREDELTA_OUT%VALUES(1)%IMPURITY(IIMP1)%delta_tz(:,IZIMP1) = COREDELTA_OUT%VALUES(1)%IMPURITY(IIMP1)%delta_tz(:,IZIMP1) + FUN
                            END IF
                         END IF
                      END IF

                   END DO INPUT_IONIZATION_STATES
                END DO OUTPUT_IONIZATION_STATES

             END IF CHECK_FOR_IMPURITY_CONSISTENCY

7            CONTINUE

          END DO INPUT_IMPURITY_LOOP
       END DO OUTPUT_IMPURITY_LOOP

8      IF(ALLOCATED(RHO2))   DEALLOCATE (RHO2)

    ENDDO LOOP_on_VALUES




! +++ Deallocate internal variables:
    IF(ALLOCATED(NZIMP1)) DEALLOCATE (NZIMP1)
    IF(ALLOCATED(NCOMP1)) DEALLOCATE (NCOMP1)
    IF(ALLOCATED(NTYPE1)) DEALLOCATE (NTYPE1)
    IF(ALLOCATED(NZIMP2)) DEALLOCATE (NZIMP2)
    IF(ALLOCATED(NCOMP2)) DEALLOCATE (NCOMP2)
    IF(ALLOCATED(NTYPE2)) DEALLOCATE (NTYPE2)
    IF(ALLOCATED(RHO1))   DEALLOCATE (RHO1)
    IF(ALLOCATED(FUN))    DEALLOCATE (FUN)




  10  RETURN

  END SUBROUTINE INTERPOLATE_DELTA
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  









  END MODULE INTERPOLATE_CPO

