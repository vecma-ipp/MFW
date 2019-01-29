!*DECK C2SC01 
!*CALL PROCESS 
SUBROUTINE MAGAXE
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SC01 COMPUTE POSITION OF MAGNETIC AXIS. FINDS THE MINIMUM OF PSI  *
  !        BY CONJUGATE GRADIENT METHOD                                 *
  !        (SEE NUMERICAL RECIPES P.303-306)                            *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZDPDZ0
  REAL(RKIND)      ::     ZDPDR0
  REAL(RKIND)      ::     ZGAM
  REAL(RKIND)      ::     ZDGG
  REAL(RKIND)      ::     ZGG
  REAL(RKIND)      ::     ZDPDXIM
  REAL(RKIND)      ::     ZPSIM
  REAL(RKIND)      ::     ZDPDZM
  REAL(RKIND)      ::     ZDPDRM
  REAL(RKIND)      ::     ZZM
  REAL(RKIND)      ::     ZRM
  INTEGER          ::     J4
  REAL(RKIND)      ::     ZDPDXI1
  REAL(RKIND)      ::     ZDPDZ1
  REAL(RKIND)      ::     ZDPDR1
  INTEGER          ::     J2
  REAL(RKIND)      ::     ZDPDXI0
  REAL(RKIND)      ::     ZZ1
  REAL(RKIND)      ::     ZR1
  REAL(RKIND)      ::     ZSCL
  INTEGER          ::     J8
  REAL(RKIND)      ::     ZZOLD
  REAL(RKIND)      ::     ZROLD
  REAL(RKIND)      ::     ZH
  REAL(RKIND)      ::     ZG
  REAL(RKIND)      ::     ZPSI0
  REAL(RKIND)      ::     ZPSI1
  REAL(RKIND)      ::     ZXI
  REAL(RKIND)      ::     ZZ0
  REAL(RKIND)      ::     ZR0
  REAL(RKIND)      ::     ZDIV
  INTEGER          ::     NDIVS
  DIMENSION  ZG(2),  ZH(2), ZXI(2)
  parameter (ndivs = 20)
  zdiv = aspct/REAL(ndivs,RKIND)
  !
  IF (NMAG .EQ. 0) THEN
     !
     ZR0  = R0 + .1_RKIND * ASPCT
     ZZ0  = RZ0
     NMAG = 1
     !
  ELSE IF (NMAG .EQ. 1) THEN
     !
     ZR0 = RMAG
     ZZ0 = RZMAG
     !
  ENDIF
  !
  IF (RMAG .EQ. R0) ZR0 = R0 + .05_RKIND * ASPCT
  !
  ! INITIALIZATION FOR CONJUGATE GRADIENT METHOD
  !
  CALL EVLATE(1,ZR0,ZZ0,ZXI(1),ZXI(2),ZPSI1)
  CALL EVLATE(2,ZR0,ZZ0,ZXI(1),ZXI(2),ZPSI0)
  if (zpsi1.GE.0._RKIND) THEN
     write(*,*) ' psivalue ',zpsi1
     stop 'psi>=0'
  END if
  !
  ZG(1)  = - ZXI(1)
  ZG(2)  = - ZXI(2)
  ZH(1)  = ZG(1)
  ZH(2)  = ZG(2)
  ZXI(1) = ZH(1)
  ZXI(2) = ZH(2)
  ZROLD  = ZR0
  ZZOLD  = ZZ0
  !
  DO J8=1,50
     !
     ! BRACKET OUT THE MINIMUM IN DIRECTION ZXI
     !
     IF (ABS(ZXI(1)) .LT. RC1M14 .AND. ABS(ZXI(2)) .LT. RC1M14) THEN
        !
        GOTO 10
        !
     ENDIF
     !
     ZSCL = SQRT(ZXI(1)**2 + ZXI(2)**2)
     ZR1  = ZR0 + zdiv * ZXI(1) / ZSCL 
     ZZ1  = ZZ0 + zdiv * ZXI(2) / ZSCL
     !
     ZDPDXI0 = -ZSCL
     !
     DO J2=1,ndivs
        !
        CALL EVLATE(2,ZR1,ZZ1,ZDPDR1,ZDPDZ1,ZPSI1)
        !
        ZDPDXI1 = (ZDPDR1 * ZXI(1) + ZDPDZ1 * ZXI(2)) / ZSCL
        !
        IF (ZDPDXI1 * ZDPDXI0 .GT. 0._RKIND) THEN
           !
           ZR0     = ZR1
           ZZ0     = ZZ1
           ZDPDXI0 = ZDPDXI1
           ZR1     = ZR0 + zdiv * ZXI(1) / ZSCL 
           ZZ1     = ZZ0 + zdiv * ZXI(2) / ZSCL
           !
        ELSE
           !
           GOTO 3
           !
        ENDIF
        !
     END DO
     !
     PRINT*,'BRACKETING NOT SUCCEEDED'
     STOP
     !
3    CONTINUE 
     !
     ! COMPUTE THE MINIMUM IN DIRECTION ZXI BY BISSECTION 
     !
     DO J4=1,100
        !
        ZRM = .5_RKIND * (ZR0 + ZR1)
        ZZM = .5_RKIND * (ZZ0 + ZZ1)
        !
        CALL EVLATE(2,ZRM,ZZM,ZDPDRM,ZDPDZM,ZPSIM)
        !
        ZDPDXIM = (ZDPDRM * ZXI(1) + ZDPDZM * ZXI(2)) / ZSCL
        !
        IF ((ABS(ZR1 - ZR0) .LT. RC1M12 .AND. &
             &        ABS(ZZ1 - ZZ0) .LT. RC1M12) .OR. &
             &        ABS(ZDPDXIM) .LT. RC1M14) GOTO 5
        !
        IF (ZDPDXI0 * ZDPDXIM .LT. 0._RKIND) THEN
           !
           ZR1     = ZRM
           ZZ1     = ZZM
           ZDPDXI1 = ZDPDXIM
           !
        ELSE
           !
           ZR0     = ZRM
           ZZ0     = ZZM
           ZDPDXI0 = ZDPDXIM
           !
        ENDIF
        !
     END DO
     !
     PRINT*,'BISSECTION NOT SUCCEEDED'
     STOP
     !
5    CONTINUE 
     !
     ZR0 = ZRM
     ZZ0 = ZZM
     !
     IF ((ABS(ZROLD - ZR0) .LT. RC1M12 .AND. &
          &        ABS(ZZOLD - ZZ0) .LT. RC1M12) .OR. &
          &       (ABS(ZDPDRM) .LT. RC1M14 .AND. &
          &        ABS(ZDPDZM) .LT. RC1M14)) GOTO 10
     !
     ! INITIALIZATION FOR NEXT STEP OF CONJUGATE GRADIENT METHOD
     !
     ZROLD = ZR0
     ZZOLD = ZZ0
     !
     CALL EVLATE(2,ZR0,ZZ0,ZXI(1),ZXI(2),ZPSI0)
     !
     ZGG  = ZG(1)**2 + ZG(2)**2
     !C
     !C FLETCHER-REEVES
     !C
     !         ZDGG = ZXI(1)**2 + ZXI(2)**2 
     !
     ! POLAK-RIBIERE
     !
     ZDGG = ZXI(1) * (ZG(1) + ZXI(1)) + ZXI(2) * (ZG(2) + ZXI(2))
     !
     IF (ZGG .EQ. 0._RKIND) GOTO 10
     !
     ZGAM = ZDGG / ZGG
     !
     ZG(1)  = - ZXI(1)
     ZG(2)  = - ZXI(2)
     ZH(1)  = ZG(1) + ZGAM * ZH(1)
     ZH(2)  = ZG(2) + ZGAM * ZH(2)
     ZXI(1) = ZH(1)
     ZXI(2) = ZH(2)
     !
  END DO
  !
  CALL RVAR('ZROLD - ZR0',ZROLD - ZR0)
  CALL RVAR('ZZOLD - ZZ0',ZZOLD - ZZ0)
  CALL RVAR('ZDPDRM',ZDPDRM)
  CALL RVAR('ZDPDZM',ZDPDZM)
  PRINT*,'CONJUGATE GRADIENT NOT CONVERGED'
  write(*,*) ' warning magnetic axis not found'
  goto 20
  !        STOP
  !
10 CONTINUE 
  !
  CALL EVLATE(1,ZR0,ZZ0,ZDPDR0,ZDPDZ0,SPSIM)
  !
  RMAG  = ZR0
  RZMAG = ZZ0  
  !
  if (nsym.eq.0) return
  !ab
  !ab    sometimes the magnetic axis moves off the midplane after
  !ab    interpolation from small to large grid.  Then, under-relaxation
  !ab    usually helps it to go back to z = 0
  !ab
  if (abs(rzmag).lt.ezmag) return
  write(*,*) ' rzmag=',rzmag
  rzmag = 0._RKIND
20 relax = relax + 0.01_RKIND
  if (relax.lt.0.80_RKIND) return
  relax = 0.80_RKIND
  write(*,*) ' warning, relaxation factor =',relax
  !
  RETURN
END SUBROUTINE MAGAXE
