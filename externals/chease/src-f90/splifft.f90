!*DECK C2SM24
!*CALL PROCESS
SUBROUTINE SPLIFFT(PCHI,PFUN,KCHI,PPERIOD,PD2FUN,PWORK,PFFTFUN, &
     &                      KNFFT,KINDEX,PA,PB,PC,PD)
  !        ###############################################################
  !
  !                                        AUTHOR O. SAUTER, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM24 COMPUTE CUBIC SPLINE WITH PERIODIC B.C. AND FAST FOURIER     *
  !        TRANSFORM ASSUMING PCHI REAL                                 *
  !                                                                     *
  !**********************************************************************
  !
  USE prec_const
  IMPLICIT NONE
  REAL(RKIND)      ::     PD
  REAL(RKIND)      ::     PC
  REAL(RKIND)      ::     PB
  INTEGER          ::     KINDEX
  REAL(RKIND)      ::     PA
  REAL(RKIND)      ::     PFFTFUN
  INTEGER          ::     I
  REAL(RKIND)      ::     PWORK
  REAL(RKIND)      ::     PD2FUN
  REAL(RKIND)      ::     PPERIOD
  REAL(RKIND)      ::     PFUN
  REAL(RKIND)      ::     PCHI
  INTEGER          ::     KNFFT
  INTEGER          ::     KCHI
  DIMENSION PCHI(KCHI), PFUN(KCHI), PD2FUN(KCHI), PWORK(KCHI,3), &
       &     PFFTFUN(KNFFT+2), KINDEX(KNFFT), &
       &     PA(KNFFT), PB(KNFFT), PC(KNFFT), PD(KNFFT)
  !.......................................................................
  !
  !     COMPUTE CUBIC SPLINE OF PFUN USING PA, PB, PC, PD PRECOMPUTED
  !
  CALL SPLCY(PCHI,PFUN,KCHI,PPERIOD,PD2FUN,PWORK(1,1),PWORK(1,2), &
       &     PWORK(1,3))
  DO I=1,KNFFT
     PFFTFUN(I) = PA(I)*PFUN(KINDEX(I)) + &
          &       PB(I)*PFUN(KINDEX(I)+1) + &
          &       PC(I)*PD2FUN(KINDEX(I)) + PD(I)*PD2FUN(KINDEX(I)+1)
  ENDDO
  !
  !     COMPUTE FAST FOURIER TRANSFORM AND REARRANGE COEFFICIENT FOR M=N/2
  !
  CALL REALFT(PFFTFUN,KNFFT/2,1)
  PFFTFUN(KNFFT+1) = PFFTFUN(2)
  PFFTFUN(2) = 0.0_RKIND
  PFFTFUN(KNFFT+2) = 0.0_RKIND
  !
  RETURN
END SUBROUTINE SPLIFFT
