!*DECK C2S03
!*CALL PROCESS
SUBROUTINE INITIA
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2S03 INITIALIZE VERTICAL MATRIX NUMBERING AND GAUSS QUADRATURE     *
  !       POINTS FOR THE INTEGRATION OF THE VARIATIONAL FORM OF THE     *
  !       EQUILIBRIUM (SEE EQ. (27) IN THE PUBLICATION)                 *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     KPOINT
  INTEGER          ::     J8
  INTEGER          ::     J9
  INTEGER          ::     J7
  INTEGER          ::     J6
  REAL(RKIND)      ::     ZWGTT
  REAL(RKIND)      ::     ZRACT
  REAL(RKIND)      ::     ZWGTS
  REAL(RKIND)      ::     ZRACS
  INTEGER          ::     JT
  INTEGER          ::     I
  INTEGER          ::     MPLA4
  INTEGER          ::     MPLA3
  INTEGER          ::     MPLA2
  INTEGER          ::     MPLA1
  DIMENSION &
       &   MPLA1(4,4),   MPLA2(4,4),   MPLA3(4,4),   MPLA4(4,4), &
       &   ZRACS(NPSGS+1), ZRACT(NPTGS+1), ZWGTS(NPSGS+1), ZWGTT(NPTGS+1)
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  CALL RESETI(MPLA1,16,0)
  CALL RESETI(MPLA2,16,0)
  CALL RESETI(MPLA3,16,0)
  CALL RESETI(MPLA4,16,0)
  !
  !**********************************************************************
  !                                                                     *
  ! 1. DEFINE HORIZONTAL BAND MATRIX ADRESSES                           *
  !                                                                     *
  !**********************************************************************
  !
  MPLA1(1,1) = 1
  MPLA1(2,1) = NT1
  MPLA1(3,1) = 2
  MPLA1(4,1) = NT + 2
  !
  MPLA1(2,2) = 1
  MPLA1(4,2) = 2
  !
  MPLA1(2,3) = NT
  MPLA1(3,3) = 1
  MPLA1(4,3) = NT1
  !
  MPLA1(4,4) = 1
  !
  MPLA2(1,1) = 1
  MPLA2(2,1) = NT1
  MPLA2(3,1) = 3
  MPLA2(4,1) = NT + 3
  !
  MPLA2(2,2) = 1
  MPLA2(4,2) = 3
  !
  MPLA2(2,3) = NT - 1
  MPLA2(3,3) = 1
  MPLA2(4,3) = NT1
  !
  MPLA2(4,4) = 1
  !
  MPLA3(1,1) = 1
  MPLA3(2,1) = NT1
  MPLA3(4,1) = NT - 1
  !
  MPLA3(1,3) = 3
  MPLA3(2,3) = NT + 3
  MPLA3(3,3) = 1
  MPLA3(4,3) = NT1
  !
  MPLA3(2,2) = 1
  !
  MPLA3(2,4) = 3
  MPLA3(4,4) = 1
  !
  MPLA4(1,1) = 1
  MPLA4(2,1) = NT1
  MPLA4(4,1) = NT
  !
  MPLA4(1,3) = 2
  MPLA4(2,3) = NT + 2
  MPLA4(3,3) = 1
  MPLA4(4,3) = NT1
  !
  MPLA4(2,2) = 1
  !
  MPLA4(2,4) = 2
  MPLA4(4,4) = 1
  !
  DO I=1,4
     DO JT=1,NT
        !
        IF (JT .EQ. 1) THEN
           !
           MPLA(JT,I,1) = MPLA1(I,1)
           MPLA(JT,I,2) = MPLA1(I,2)
           MPLA(JT,I,3) = MPLA1(I,3)
           MPLA(JT,I,4) = MPLA1(I,4)
           !
        ELSE IF (JT .GE. 2 .AND. JT .LE. NT/2) THEN
           !
           MPLA(JT,I,1) = MPLA2(I,1)
           MPLA(JT,I,2) = MPLA2(I,2)
           MPLA(JT,I,3) = MPLA2(I,3)
           MPLA(JT,I,4) = MPLA2(I,4)
           !
        ELSE IF (JT .GE. NT/2+2 .AND. JT .LE. NT) THEN
           !
           MPLA(JT,I,1) = MPLA3(I,1)
           MPLA(JT,I,2) = MPLA3(I,2)
           MPLA(JT,I,3) = MPLA3(I,3)
           MPLA(JT,I,4) = MPLA3(I,4)
           !
        ELSE IF (JT .EQ. NT/2+1) THEN
           !
           MPLA(JT,I,1) = MPLA4(I,1)
           MPLA(JT,I,2) = MPLA4(I,2)
           MPLA(JT,I,3) = MPLA4(I,3)
           MPLA(JT,I,4) = MPLA4(I,4)
           !
        ENDIF
        !           
     END DO
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 2. DEFINE ISOPARAMETRIC INTEGRATION POINTS FOR GAUSSIAN QUADRATURE  *
  !    (16 POINTS FORMULA)                                              *
  !                                                                     *
  !**********************************************************************
  !
  CALL GAUSS(NSGAUS,ZRACS,ZWGTS)
  CALL GAUSS(NTGAUS,ZRACT,ZWGTT)
  !
  DO J6=1,NSGAUS
     !
     ZRACS(J6) = .5_RKIND * (1._RKIND + ZRACS(J6))
     !
  END DO
  !
  DO J7=1,NTGAUS
     !
     ZRACT(J7) = .5_RKIND * (1._RKIND + ZRACT(J7))
     !
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 3.DZETA CONTAINS VALUES (X ; Y) ON THE 16 GAUSSIAN QUADRATURE       *
  !   POINTS. CW CONTAINS INTEGRATION WEIGHTS                           *
  !                                                                     *
  !**********************************************************************
  !
  DO J9=1,NTGAUS
     !
     DO J8=1,NSGAUS
        !
        KPOINT          = (J9 - 1) * NSGAUS + J8
        DZETA(KPOINT,1) = ZRACS(J8)
        DZETA(KPOINT,2) = ZRACT(J9)
        CW(KPOINT)      = ZWGTS(J8) * ZWGTT(J9)
        !
     END DO
  END DO
  !
  RETURN
END SUBROUTINE INITIA
