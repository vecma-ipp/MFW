SUBROUTINE STEPON
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2S01 LEAD THE CALCULATION                                          *
  !                                                                     *
  !**********************************************************************
  !
  !
  USE globals
  USE interpol
  use interpos_module
  IMPLICIT NONE
  INTERFACE
     SUBROUTINE COCOS(KCOCOS,Kexp_Bp,Ksigma_Bp,Ksigma_RphiZ,Ksigma_rhothetaphi,Ksign_q_pos,Ksign_pprime_pos)
       !
       ! return values of exp_Bp, sigma_Bp, sigma_rhothetaphi, sign_q_pos, sign_pprime_pos
       ! from the input value KCOCOS and according to O. Sauter and S. Yu. Medvevdev paper and Table I
       ! (see paper in chease directory)
       !
       IMPLICIT NONE
       INTEGER, intent(in) :: KCOCOS
       INTEGER, intent(out) :: Kexp_Bp, Ksigma_Bp, Ksigma_RphiZ, Ksigma_rhothetaphi, Ksign_q_pos, Ksign_pprime_pos
     END SUBROUTINE COCOS
     SUBROUTINE metrictoitm(Kexp_Bp,Ksigma_Bp,Ksigma_RphiZ,Ksigma_rhothetaphi,Ksign_q_pos,Ksign_pprime_pos)
       !
       USE globals
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: Kexp_Bp, Ksigma_Bp, Ksigma_RphiZ, Ksigma_rhothetaphi, Ksign_q_pos, Ksign_pprime_pos
     END SUBROUTINE metrictoitm
     SUBROUTINE EQCHEASE_MKSA(Kexp_Bp,Ksigma_Bp,Ksigma_RphiZ,Ksigma_rhothetaphi,Ksign_q_pos,Ksign_pprime_pos)
  !
       USE globals
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: Kexp_Bp, Ksigma_Bp, Ksigma_RphiZ, Ksigma_rhothetaphi, Ksign_q_pos, Ksign_pprime_pos
     END SUBROUTINE EQCHEASE_MKSA
  END INTERFACE
  !
  REAL(RKIND)      ::     ZWORK(NPISOEFF), TENS_DEF
  INTEGER          ::     JEND
  INTEGER          ::     J5
  INTEGER          ::     J4, ICHI
  INTEGER          ::     J3
  INTEGER          ::     IOUT9
  INTEGER :: iexp_Bp, isigma_Bp, isigma_RphiZ, isigma_rhothetaphi, isign_q_pos, isign_pprime_pos
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  ! PRINT NAMELIST
  IF ((NOPT .NE. 1) .AND. (NVERBOSE .GE. 1)) CALL OUTPUT(1)
  !
  IF (NOPT.GE.-2 .AND. NOPT.LE.-1) THEN
    !
    ! NOPT= -2 OR -1 : GUESS FROM OLD EQUIL, READ QUANTITIES FROM NIN
    NDIM(1) = NS
    NDIM(2) = NT
    NDIM(3) = NISO
    NCHI1  = NCHI + 1
    NPSI1  = NPSI + 1
    NV1    = NV + 1
    ! READ FILE NIN
    CALL IODISK(1)
    NS1    = NS + 1
    NT1    = NT + 1
    NT2    = NT + 2
    CALL OLDNEW
    CALL MESH(4)
    !
  ENDIF
  !
  IF (NOPT .EQ. 1) THEN
    ! NOPT = 1 : READ FULL EQUILIBRIUM FROM NIN
    !
    ! READ FILE NIN
    CALL IODISK(1)
    !     WRITE NAMELIST WITH VALUES FROM NIN
    WRITE(6,'(///" NAMELIST AFTER READ FROM NIN (NOPT=1)")')
    CALL OUTPUT(1)
    !
    NS1    = NS + 1
    NT1    = NT + 1
    NT2    = NT + 2
    NCHI1  = NCHI + 1
    NPSI1  = NPSI + 1
    NV1    = NV + 1
    CALL MESH(4)
    !
    IF (NBLOPT .NE. 0 .AND. CPRESS .NE. 1._RKIND) THEN
      ! IF BALLOONING OPTIMIZATION HAS BEEN DONE AND CPRESS.NE.0. RECOMPUTE
      ! EQUILIBRIUM SUCH THAT
      !       P'- NEW = CPRESS P' OF BALLOONING AND MERCIER OPTIMIZED
      !                 EQUILIBRIUM
      CALL DSCAL(NPPR+1,CPRESS,RPRM,1)
      CALL SPLINE(NPPR+1,PCSM,RPRM,D2RPRM,ZWORK)
      WRITE(6,2501) 100._RKIND * CPRESS
      CALL BALLIT
    ENDIF
  ELSE
    ! NOPT = 0, -1 OR -2 : COMPUTE EQUILIBRIUM FROM SCRATCH OR RESTART
    !
    IF (NANAL .EQ. 0) THEN
      ! NANAL = 0 : NUMERIC EQUILIBRIUM
      CALL BALLIT
    ELSE
      ! NANAL = 1 : ANALYTIC SOLOVEV EQUILIBRIUM
      NS1    = NS + 1
      NT1    = NT + 1
      NT2    = NT + 2
      NCHI1  = NCHI + 1
      NPSI1  = NPSI + 1
      NV1    = NV + 1
      NSTMAX = NS1 * NT
      CALL INITIA
      IF (NVERBOSE .GE. 0) CALL OUTPUT(3)
      CALL MESH(1)
      CALL MESH(2) 
      CALL MESH(4)
      IF (NVERBOSE .GE. 1) CALL OUTPUT(5)
      IF (NVERBOSE .GE. 1) CALL OUTPUT(6)
      CALL SOLOVEV
      IF (NVERBOSE .GE. 1) CALL RUNTIM
    ENDIF
    !
    ! STORE EQUILIBRIUM ON FILE NOUT
    CALL IODISK(2)
    !
    ! STOP IF ITERATION OVER CURRENT PROFILE DID NOT CONVERGE
    IF (NCON .EQ. -2) PRINT *,'DID NOT CONVERGE:  NCON= ',NCON
    IF (NCON .EQ. -2) RETURN
  ENDIF
  !
  !  COMPUTE S MESH REQUIRED FOR STABILITY (WITH OR WITHOUT PACKINGS)   *
  CALL MESH(3)
  IF (NIDEAL .EQ. 4) CALL MESH(7)
  IF (NIDEAL .EQ. 5) CALL MESH(8)
  !%OS         IF (NIDEAL .EQ. 6) CALL MESH(9)
  !
  !        COMPUTE (R,Z) OF PLASMA BOUNDARY (USED FOR EXPEQ.OUT AND EQDSK)
  CALL RZBOUND
  !
  !**********************************************************************
  ! PREMAP : COMPUTE MAPPING MESH AND PROFILES
  ! NOREPT : SCALE EQUILIBRIUM
  ! MAPPIN : COMPUTE SURFACE QUANTITIES
  !**********************************************************************
  !
  IF (NVERBOSE .GE. 1) THEN
    WRITE(6,'(/," BEFORE MAPPING: ")')
    CALL RUNTIM
  END IF
  !
  ! Do Mapping
  !
  IF (NIDEAL .EQ. 1 .OR. NIDEAL .EQ. 2) THEN
    ! ERATO OR LION
    CALL PREMAP(3)
    CALL NOREPT(NISO1EFF,1)
    CALL MAPPIN(1)
    !
  ELSE IF (NIDEAL .EQ. 0 .OR. NIDEAL .EQ. 3) THEN
    ! MARS OR NOVA-W
    CALL PREMAP(4)
    CALL NOREPT(NISO1EFF,1)
    IF (REXT .GT. 1._RKIND) CALL MESH(5)
    CALL MAPPIN(2)
    !
  ELSE IF (NIDEAL .EQ. 4) THEN
    ! PENN
    CALL IODISK(4)
    CALL PREMAP(5)
    CALL NOREPT(NISO1EFF,1)
    CALL MAPPIN(3)
    !
  ELSE IF (NIDEAL .EQ. 5) THEN
    ! XTOR
    CALL IODISK(5)
    CALL PREMAP(4)
    CALL NOREPT(NISO1EFF,1)
    CALL MAPPIN(2)
    !
  ELSE IF (NIDEAL .EQ. 6) THEN
    ! BASIC (SAME AS ERATO, BUT NO REQUIREMENTS ON MRSCAL AND NTMF0)
    CALL PREMAP(3)
    CALL NOREPT(NISO1EFF,1)
    CALL MAPPIN(1)
    !
  ELSE IF (NIDEAL .EQ. 7) THEN
    ! GYROKINETIC CODES USING EQCIN
    CALL PREMAP(3)
    CALL NOREPT(NISO1EFF,1)
    CALL MAPPIN(1)
    CALL OUTGYRO(NISO1EFF)
    !
  ELSE IF (NIDEAL .EQ. 8) THEN
    ! ELITE
    CALL PREMAP(3)
    CALL NOREPT(NISO1EFF,1)
    CALL MAPPIN(1)
    !   as uses same mapping mesh as erato, needs to call oiutelit after chipsi, below
    !            CALL OUTELIT
    !
  ELSE IF (NIDEAL .EQ. 9) THEN
    ! ORB5, GENE, GENERIC FOR GYROKINETIC CODES WITH STRAIGHT FIELD LINE JACOBIAN
    ! CALL PREMAP(6)       This cannot be used without modifying the interpolation 
    !in ogyropsi.f90 which assumes PSIISO on CSM
    ! only premap 3 allowed
    CALL PREMAP(3) 
    CALL NOREPT(NISO1EFF,1)
    CALL MAPPIN(1)
    !
    ! output to hdf5 files called at end after arrays changed to SI units
    !
  ELSE IF (NIDEAL .EQ. 10) THEN
    ! GKW, NEOART
    CALL PREMAP(3)
    CALL NOREPT(NISO1EFF,1)
    CALL MAPPIN(1)
    !
    ! output to files called at end after arrays changed to SI units
    !
    !
  ENDIF
  !
  !        ALWAYS SAVE EXPEQ.OUT
  IF (NVERBOSE .GE. 1) CALL IODISK(6)
  !
  !     SAVE TE, NE, ZEFF, AND/OR TI ON EXPTNZ.OUT
  IF (NVERBOSE .GE. 1) CALL BSEXPEQ(2)
  !
  ! Computes box and psi on R,Z
  CALL PSIBOX(NISO1EFF)
  !
!!$  IF (NEQDSK.NE.0 .OR. NIDEAL.EQ.6) THEN
    !     SAVE EQDSK FILE (uses COCOS_OUT, could be put at the end, but stayed here to be written earlier)
    IF (NVERBOSE .GE. 1) CALL IODISK(7)
!!$  ENDIF
  !
  IF (NVERBOSE .GE. 1) THEN
    WRITE(6,'(/," AFTER MAPPING: ")')
    CALL RUNTIM
  END IF
  !
  ! POLOIDAL MAGNETIC FIELD ENERGY (WITH NOPT = 0 ONLY) (OS: SHOULD CHECK WHEN NEEDED AND PUT SPECIFIC OPTION)
  IF (NOPT .EQ. 0) CALL ENERGY
  ! TEST SOLOVEV SOLUTION
  IF (NTEST .EQ. 1) CALL TEST
  !
  ! OUTPUT RELATED TO SOLUTION QUALITY
  IF (NVERBOSE .GE. 1) CALL OUTPUT(2)
  ! PRINT CHI/CHIM VALUES
  IF (NVERBOSE .GE. 2) CALL OUTPUT(8)
  !   MOVED OUTPUT(7) AFTER BALOON, SO THAT CAN BE USED IN "Q SUMMARY"
  !   => FLAG IOUT9 TO CALL OUTPUT(9) IN SAME ORDER AS BEFORE
  IOUT9 = 0
  !
  !**********************************************************************
  !                                                                     *
  ! OUTPUT FOR STABILITY CODES, BALLOONING AND MERCIER STABILITY, PLOT  *
  ! QUANTITIES                                                          *
  !%OS* (NOT FOR NIDEAL=6 AS CSM(1)=0.0 AND ERDATA AND BALOON NEED 1/CSM)   *
  !                                                                     *
  !**********************************************************************
  !
  ! now NISO1EFF could be 2*npsi1, might need to restrict below to nideal's which have NISO1EFF=npsi1
  ! except for erato stuff and outelit, useful for ballooning only?
  IF ((NIDEAL .NE. 0 .OR. NBAL .EQ. 1 .OR. NPLOT .EQ. 1)) THEN
    DO J3=1,NISO1EFF
      CALL CHIPSI(NISO1EFF,J3)
    END DO
    IF (NIDEAL .NE. 0 .OR. NPLOT .EQ. 1) THEN
      ! write(0,*) 'ncurv= ',ncurv
      ncurv=0 ! make sure does not add from previous call to chease subroutine
      DO J4=1,NISO1EFF
        CALL ERDATA(J4)
      END DO
      ! Above loop filled quantities J4_eff=2:NISO1EFF+1. Add on-axis values here when relevant
      IF (NDEQ .GE. 3) THEN
        EQ( 1,:,1) = 0._RKIND
        EQ( 3,:,1) = EQ( 1,:,2)
        EQ( 5,:,1) = 0.5 * (EQ( 1,:,1) + EQ( 3,:,1))
        EQ( 2,:,1) = EQ( 2,:,2)
        EQ( 4,:,1) = EQ( 4,:,2)
        EQ(6:8,:,1) = EQ( 6:8,:,2)
        EQ( 9,:,1) = T0
        EQ(10,:,1) = EQ( 10,:,2)
        EQ(11,:,1) = 1.0_RKIND
        EQ(14,:,1) = RMAG
        EQ(16,:,1) = 0.0_RKIND
        EQ(23,:,1) = 0.0_RKIND
        IF (NDEQ .GT. 25) THEN
          EQ(26,:,1) = DTTP0 / T0
          EQ(27,:,1) = - RMAG * DPDP0 - DTTP0 / RMAG
        ENDIF
        ! Chi dependent part for extrapolation at s=0:
        TENS_DEF = -0.1_RKIND
        DO ICHI=1,NCHI
          CALL INTERPOS(EQ( 1,ICHI,2:NISO1EFF1),EQ(12,ICHI,2:NISO1EFF1),N=NISO1EFF1-1,TENSION=TENS_DEF, &
               & XSCAL=EQ( 1,ICHI,1),yscal=EQ(12,ICHI,1), &
               & nbcscal=(/1, 2/), ybcscal=(/1.0E+32_RKIND, EQ(12,ICHI,NISO1EFF1)/))
          CALL INTERPOS(EQ( 1,ICHI,2:NISO1EFF1),EQ(13,ICHI,2:NISO1EFF1),N=NISO1EFF1-1,TENSION=TENS_DEF, &
               & XSCAL=EQ( 1,ICHI,1),yscal=EQ(13,ICHI,1), &
               & nbcscal=(/1, 2/), ybcscal=(/1.0E+32_RKIND, EQ(13,ICHI,NISO1EFF1)/))
          CALL INTERPOS(EQ( 1,ICHI,2:NISO1EFF1),EQ(15,ICHI,2:NISO1EFF1),N=NISO1EFF1-1,TENSION=TENS_DEF, &
               & XSCAL=EQ( 1,ICHI,1),yscal=EQ(15,ICHI,1), &
               & nbcscal=(/1, 2/), ybcscal=(/1.0E+32_RKIND, EQ(15,ICHI,NISO1EFF1)/))
          ! EQ(17): interpolate on s*EQ(17) then divide by s in LION but for s=0, cannot find value like this
          CALL INTERPOS(EQ( 1,ICHI,2:NISO1EFF1),EQ(17,ICHI,2:NISO1EFF1),N=NISO1EFF1-1,TENSION=TENS_DEF, &
               & XSCAL=EQ( 1,ICHI,1),yscal=EQ(17,ICHI,1), &
               & nbcscal=(/1, 2/), ybcscal=(/1.0E+32_RKIND, EQ(17,ICHI,NISO1EFF1)/))
          CALL INTERPOS(EQ( 1,ICHI,2:NISO1EFF1),EQ(18,ICHI,2:NISO1EFF1),N=NISO1EFF1-1,TENSION=TENS_DEF, &
               & XSCAL=EQ( 1,ICHI,1),yscal=EQ(18,ICHI,1), &
               & nbcscal=(/1, 2/), ybcscal=(/1.0E+32_RKIND, EQ(18,ICHI,NISO1EFF1)/))
          CALL INTERPOS(EQ( 1,ICHI,2:NISO1EFF1),EQ(19,ICHI,2:NISO1EFF1),N=NISO1EFF1-1,TENSION=TENS_DEF, &
               & XSCAL=EQ( 1,ICHI,1),yscal=EQ(19,ICHI,1), &
               & nbcscal=(/1, 2/), ybcscal=(/1.0E+32_RKIND, EQ(19,ICHI,NISO1EFF1)/))
          CALL INTERPOS(EQ( 1,ICHI,2:NISO1EFF1),EQ(20,ICHI,2:NISO1EFF1),N=NISO1EFF1-1,TENSION=TENS_DEF, &
               & XSCAL=EQ( 1,ICHI,1),yscal=EQ(20,ICHI,1), &
               & nbcscal=(/1, 2/), ybcscal=(/1.0E+32_RKIND, EQ(20,ICHI,NISO1EFF1)/))
          CALL INTERPOS(EQ( 1,ICHI,2:NISO1EFF1),EQ(21,ICHI,2:NISO1EFF1),N=NISO1EFF1-1,TENSION=TENS_DEF, &
               & XSCAL=EQ( 1,ICHI,1),yscal=EQ(21,ICHI,1), &
               & nbcscal=(/1, 2/), ybcscal=(/1.0E+32_RKIND, EQ(21,ICHI,NISO1EFF1)/))
          CALL INTERPOS(EQ( 1,ICHI,2:NISO1EFF1),EQ(22,ICHI,2:NISO1EFF1),N=NISO1EFF1-1,TENSION=TENS_DEF, &
               & XSCAL=EQ( 1,ICHI,1),yscal=EQ(22,ICHI,1), &
               & nbcscal=(/1, 2/), ybcscal=(/1.0E+32_RKIND, EQ(22,ICHI,NISO1EFF1)/))
          CALL INTERPOS(EQ( 1,ICHI,2:NISO1EFF1),EQ(24,ICHI,2:NISO1EFF1),N=NISO1EFF1-1,TENSION=TENS_DEF, &
               & XSCAL=EQ( 1,ICHI,1),yscal=EQ(24,ICHI,1), &
               & nbcscal=(/1, 2/), ybcscal=(/1.0E+32_RKIND, EQ(24,ICHI,NISO1EFF1)/))
          CALL INTERPOS(EQ( 1,ICHI,2:NISO1EFF1),EQ(24,ICHI,2:NISO1EFF1),N=NISO1EFF1-1,TENSION=TENS_DEF, &
               & XSCAL=EQ( 1,ICHI,1),yscal=EQ(24,ICHI,1), &
               & nbcscal=(/1, 2/), ybcscal=(/1.0E+32_RKIND, EQ(24,ICHI,NISO1EFF1)/))
          IF (NDEQ .GT. 25) THEN
            CALL INTERPOS(EQ( 1,ICHI,2:NISO1EFF1),EQ(25,ICHI,2:NISO1EFF1),N=NISO1EFF1-1,TENSION=TENS_DEF, &
                 & XSCAL=EQ( 1,ICHI,1),yscal=EQ(25,ICHI,1), &
                 & nbcscal=(/1, 2/), ybcscal=(/1.0E+32_RKIND, EQ(25,ICHI,NISO1EFF1)/))
            ! EQ(28): interpolate on s*EQ(28) then divide by s in LION but for s=0, cannot find value like this
            CALL INTERPOS(EQ( 1,ICHI,2:NISO1EFF1),EQ(28,ICHI,2:NISO1EFF1),N=NISO1EFF1-1,TENSION=TENS_DEF, &
                 & XSCAL=EQ( 1,ICHI,1),yscal=EQ(28,ICHI,1), &
                 & nbcscal=(/1, 2/), ybcscal=(/1.0E+32_RKIND, EQ(28,ICHI,NISO1EFF1)/))
            ! EQ(29): interpolate on s*EQ(29) then divide by s in LION but for s=0, cannot find value like this
            CALL INTERPOS(EQ( 1,ICHI,2:NISO1EFF1),EQ(29,ICHI,2:NISO1EFF1),N=NISO1EFF1-1,TENSION=TENS_DEF, &
                 & XSCAL=EQ( 1,ICHI,1),yscal=EQ(29,ICHI,1), &
                 & nbcscal=(/1, 2/), ybcscal=(/1.0E+32_RKIND, EQ(29,ICHI,NISO1EFF1)/))
          ENDIF
        END DO
        !
      END IF
      CR(:,1) = R0
      CZ(:,1) = RZ0
      !
      RINOR = RITOT / (ASPCT * TMF(NISO1EFF))
      QCYL  = 2 * AREA / (RITOT * RMAG**2)
      !
      IF (NIDEAL.EQ.1 .OR. NIDEAL.EQ.2) CALL NERAT
      !
      IF (NIDEAL .EQ. 8) CALL OUTELIT
    ENDIF
    !
    IF (NBAL .EQ. 1 .OR. NPLOT .EQ. 1) THEN
      !
      ! COULD SKIP SURFACES IF NISO1EFF>NPSI1
      DO J5=1,NISO1EFF,NPPSBAL
        JEND = MIN(J5+NPPSBAL-1,NISO1EFF)
        CALL BALOON(J5,JEND,SMISO)
      ENDDO
      IOUT9 = 1
    ENDIF
  ENDIF
  !
  ! MAIN OUTPUT OF AVERAGE, ON-AXIS VALUES AND ARRAYS
  IF (NVERBOSE .GE. 2) CALL OUTPUT(7)
  ! MERCIER AND BALLOONING
  IF (IOUT9 .EQ. 1 .AND. NVERBOSE .GE. 2) CALL OUTPUT(9)
  ! VALUES ON RATIONAL Q (SINCE PACKED ON Q)
  IF (NMESHA .EQ. 2) CALL PRIQQU
  !
  IF (NVERBOSE .GE. 1) THEN
    WRITE(6,'(/," AFTER ERATO STUFF: ")')
    CALL RUNTIM
  END IF
  !
  ! OS: should add input param if need R,Z info on specific q value (qshave for example)
  ! CALL SHAVE
  !
  IF (NIDEAL .EQ. 1 .OR. NIDEAL .EQ. 2) THEN
    CALL IODISK(8) ! write EQ on MEQ
    CALL IODISK(10) ! write vacuum quantities for ERATO on NVAC
  ENDIF
  IF (NIDEAL .EQ. 0)  CALL IODISK(11) ! write quantities for MARS on EQU01 and EQU02
  !
  CALL IODISK(9) ! write plot stuff on NDES
  !
  IF (REXT .GT. 1._RKIND) THEN
    CALL IODISK(12) ! write vacuum quantities for linear code on ETAVAC
  ENDIF
  !
  IF ((NPLOT .EQ. 1) .AND. (NVERBOSE.GE.1)) THEN
    CALL WRTBIN
    CALL WRTPLOT
  ENDIF
  !
  ! iodisk(13): save jsolver (OS: to test when needed)
  ! needs to add an input to decide if/when needed: if (jsolver.eq.1) CALL IODISK(13)
  !
  ! LAST OUTPUT: SOME USEFUL GLOBAL VALUES AND THEIR MKSA VALUES ON UNIT=6 WITH HEADER
  IF (NVERBOSE .GE. 0) CALL OUTMKSA(6,2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 9. NOW CHANGE VARIABLES TO SI UNITS AND COCOS_OUT CONVENTION
  ! 9.0 GET RELEVANT SIGNS FROM COCOS_OUT
  ! 9.1 CHANGE TO SI AND COCOS_OUT
  ! 9.2 CALL OUTPUT SUBROUTINES WHICH USE SI UNITS AND COCOS_OUT CONVENTION
  !
  ! 9.0 CALL COCOS
  !
  CALL COCOS(COCOS_OUT,iexp_Bp,isigma_Bp,isigma_RphiZ,isigma_rhothetaphi,isign_q_pos,isign_pprime_pos)
  !
  ! 9.1.1 change eqchease_out... arrays to mksa and COCOS_OUT conventions
  CALL METRICTOITM(iexp_Bp,isigma_Bp,isigma_RphiZ,isigma_rhothetaphi,isign_q_pos,isign_pprime_pos)
  ! 
  ! Then change eqchease_out_add_1d and _2d into SI units and COCOS_OUT conventions
  CALL EQCHEASE_MKSA(iexp_Bp,isigma_Bp,isigma_RphiZ,isigma_rhothetaphi,isign_q_pos,isign_pprime_pos)
  !
  ! CALL OUTPUTS WHICH NEED SI UNITS, LIKE FOR GENE/ORB5 INTERFACE OR ITM
  ! NOTE THAT THE ITM OUTPUT IS THROUGH eqchease_out ONLY ON RETURN TO THIS SUBROUTINE
  !
  IF (NIDEAL .EQ. 9) CALL OGYROPSI
  !
  IF (NIDEAL .EQ. 10) THEN
    CALL HAMADA
    CALL NEOART
  ENDIF 
  RETURN
  !
2501 FORMAT(///,10X,'*******************************************', &
       &           //,10X,'P'' = ',1F10.4,' % OF BALOONING OPTIMIZED P'' ' &
       &           //,10X,'*******************************************')
  !
END SUBROUTINE STEPON
