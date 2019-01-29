!*DECK C2SF01
!*CALL PROCESS
SUBROUTINE NOREPT(KN,KSHIFT)
  !        ############################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SF01 EQUILIBRIUM TRANSFORMATION (SEE SECTIONS 2.3, 5.3 AND 6.4.4  *
  !        IN PUBLICATION)                                              *
  !                                                                     *
  !**********************************************************************
  !
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: KN, KSHIFT
  !
  INTEGER          ::     ICS, J1, J2, J3, J5, J6, J7, J8
  REAL(RKIND)      ::     ZQ(KN), ZS(KN)
  REAL(RKIND)      ::     QICS, TICS, ZCSHFT, ZX1, ZX2
  ! FUNCTION
  INTEGER          ::     ISRCHFGE
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  RRAXIS = RMAG
  RZAXIS = RZMAG
  !
  !**********************************************************************
  ! TOKAMAK EQUILIBRIUM                                                 *
  !**********************************************************************
  !
  IF (NRFP .EQ. 0) THEN
     !
     IF (NCSCAL .EQ. 1) THEN
        !
        DO J1=1,KN
           ZS(J1) = SQRT(1._RKIND - PSIISO(J1) / SPSIM)
           ZQ(J1) = .5_RKIND * TMF(J1) * CIDQ(J1) / CPI
        END DO
        !
        ICS = ISRCHFGE(KN,ZS,1,CSSPEC) - 1
        IF (ICS .LT. 2)      ICS = 2
        IF (ICS .GE. KN - 1) ICS = KN - 2
        !
        QICS = FCCCC0(ZQ(ICS-1),ZQ(ICS),ZQ(ICS+1),ZQ(ICS+2), &
             &                    ZS(ICS-1),ZS(ICS),ZS(ICS+1),ZS(ICS+2),CSSPEC)
        TICS = FCCCC0(TMF(ICS-1),TMF(ICS),TMF(ICS+1),TMF(ICS+2), &
             &                    ZS(ICS-1),ZS(ICS), ZS(ICS+1),ZS(ICS+2),CSSPEC)
        T0   = FCCCC0(TMF(1),TMF(2),TMF(3),TMF(4), &
             &                    ZS(1),ZS(2),ZS(3),ZS(4),RC0P)
        !
        ZCSHFT = TICS**2 * ((QSPEC / QICS)**2 - 1._RKIND)
        !
        CALL TSHIFT(ZCSHFT,KN)
        !
        IF (NTMF0 .EQ. 0) THEN
           SCALE = 1._RKIND / TMF(KN)
        ELSE IF (NTMF0 .EQ. 1) THEN
           SCALE = 1._RKIND / T0
        ENDIF
        !
        CALL PRNORM(SCALE,KN)
        !
        IF (((NSTTP.EQ.3) .OR. (NSTTP.EQ.4)) .AND. (NOPT .EQ. 0 .OR. (NOPT .EQ. 1 .AND. &
             &          (NBLOPT .NE. 0 .AND. CPRESS .NE. 1._RKIND)))) THEN
           !
           IF (NFUNC .EQ. 1) THEN
              CALL DSCAL(NSOUR,SCALE,AT,1)
              !
           ELSE IF (NFUNC .EQ. 2) THEN
              CALL DSCAL(5,SCALE,AT(3),1)
              CALL DSCAL(5,SCALE,AT2(3),1)
              CALL DSCAL(5,SCALE,AT3(3),1)
              !
              AT4(3) = SCALE * AT4(3)
              !
           ELSE IF (NFUNC .EQ. 3) THEN
              SCALE = SCALE**SCEXP
              AT(1) = SCALE * AT(1)
              !
           ELSE IF (NFUNC .EQ. 4) THEN
              CALL DSCAL(NPPF+1,SCALE,RFUN,1)
              !
           ENDIF
           !
           SCALAC = SCALAC * SCALE
           !
           IF (NVERBOSE .GE. 1) CALL RVAR('SCALE            ',SCALE)
           IF (NVERBOSE .GE. 1) CALL RVAR('ACCUMULATED SCALE',SCALAC)
        ENDIF
        !
     ELSE IF (NCSCAL .EQ. 2) THEN
        T0 = FCCCC0(TMF(1),TMF(2),TMF(3),TMF(4), &
             &                  PSIISO(1),PSIISO(2),PSIISO(3),PSIISO(4),SPSIM)
        !
        !            IF (NSTTP .LE. 2 .OR. (NSTTP .EQ. 3 .AND. NOPT .EQ. 1 .AND.
        !     &          (NBLOPT .EQ. 0 .OR. CPRESS .EQ. 1.))) THEN
        !
        IF (NSURF .EQ. 1) THEN
           SCALE = 1._RKIND
        ELSE
           SCALE = CURRT / CUROLD
        ENDIF
        !
        IF (NSTTP.LE.2) THEN
           CALL PRNORM(SCALE,KN)
           !
           IF (NTMF0 .EQ. 0) THEN
              ZCSHFT = 1._RKIND - TMF(KN)**2
           ELSE IF (NTMF0 .EQ. 1) THEN
              ZCSHFT = 1._RKIND - T0**2
           ENDIF
           !
           CALL TSHIFT(ZCSHFT,KN)
        ELSE 
           !
           !               PRINT*,'NOREPT : NSTTP=3/4; NCSCAL=2; NOPT=',NOPT,';'
           !               PRINT*,'NBLOPT=',NBLOPT,'; CPRESS=',CPRESS,';'
           !               PRINT*,'THIS OPTION IS NOT POSSIBLE'
           !               STOP               
           scale = scale**scexp
           AT(1) = SCALE  * AT(1)
           SCALAC = SCALAC * SCALE
           IF (NVERBOSE .GE. 1) CALL RVAR('SCALE            ',SCALE)
           IF (NVERBOSE .GE. 1) CALL RVAR('ACCUMULATED SCALE',SCALAC)
        ENDIF
        !
     ELSE IF (NCSCAL .EQ. 3) THEN
        ZS(1) = (1._RKIND - PSIISO(1) / SPSIM) * ABS(SPSIM) * CIDR(1)
        ZQ(1) = .5_RKIND * TMF(1) * CIDQ(1) / CPI
        DO J2=2,KN
           ZX1 = SQRT(1._RKIND - PSIISO(J2-1) / SPSIM)
           ZX2 = SQRT(1._RKIND - PSIISO(J2  ) / SPSIM)
           ZS(J2) = ZS(J2-1) + ABS(SPSIM) * (ZX2 - ZX1) * &
                &               (ZX2 * CIDR(J2) + ZX1 * CIDR(J2-1))
           ZQ(J2) = .5_RKIND * TMF(J2) * CIDQ(J2) / CPI
        END DO
        !
        DO J3=1,KN
           ZS(J3) = SQRT(ZS(J3) / ZS(KN))
        END DO
        !
        ICS = ISRCHFGE(KN,ZS,1,CSSPEC) - 1
        IF (ICS .LT. 2)      ICS = 2
        IF (ICS .GE. KN - 1) ICS = KN - 2
        !
        QICS = FCCCC0(ZQ(ICS-1),ZQ(ICS),ZQ(ICS+1),ZQ(ICS+2), &
             &                    ZS(ICS-1),ZS(ICS),ZS(ICS+1),ZS(ICS+2),CSSPEC)
        TICS = FCCCC0(TMF(ICS-1),TMF(ICS),TMF(ICS+1),TMF(ICS+2), &
             &                    ZS(ICS-1),ZS(ICS), ZS(ICS+1),ZS(ICS+2),CSSPEC)
        T0   = FCCCC0(TMF(1),TMF(2),TMF(3),TMF(4), &
             &                    ZS(1),ZS(2),ZS(3),ZS(4),RC0P)
        !
        ZCSHFT = TICS**2 * ((QSPEC / QICS)**2 - 1._RKIND)
        !
        CALL TSHIFT(ZCSHFT,KN)
        !
        IF (NTMF0 .EQ. 0) THEN
           SCALE = 1._RKIND / TMF(KN)
        ELSE IF (NTMF0 .EQ. 1) THEN
           SCALE = 1._RKIND / T0
        ENDIF
        !
        CALL PRNORM(SCALE,KN)
        !
        IF (((NSTTP.EQ.3) .OR. (NSTTP.EQ.4)) .AND. (NOPT .EQ. 0 .OR. (NOPT .EQ. 1 .AND. &
             &          (NBLOPT .NE. 0 .AND. CPRESS .NE. 1._RKIND)))) THEN
           !
           IF (NFUNC .EQ. 1) THEN
              CALL DSCAL(NSOUR,SCALE,AT,1)
              !
           ELSE IF (NFUNC .EQ. 2) THEN
              CALL DSCAL(5,SCALE,AT(3),1)
              CALL DSCAL(5,SCALE,AT2(3),1)
              CALL DSCAL(5,SCALE,AT3(3),1)
              !
              AT4(3) = SCALE * AT4(3)
              !
           ELSE IF (NFUNC .EQ. 3) THEN
              scale = scale**scexp
              AT(1) = SCALE * AT(1)
              !
           ELSE IF (NFUNC .EQ. 4) THEN
              CALL DSCAL(NPPF+1,SCALE,RFUN,1)
              !
           ENDIF
           !
           SCALAC = SCALAC * SCALE
           !
           IF (NVERBOSE .GE. 1) CALL RVAR('SCALE            ',SCALE)
           IF (NVERBOSE .GE. 1) CALL RVAR('ACCUMULATED SCALE',SCALAC)
        ENDIF
     ENDIF
     !
     !**********************************************************************
     !                                                                     *
     ! REVERSED FIELD PINCH EQUILIBRIUM                                    *
     !                                                                     *
     !**********************************************************************
     !
  ELSE IF (NRFP .EQ. 1) THEN
     !
     IF (NSURF .EQ. 1) THEN
        SCALE = 1._RKIND
     ELSE
        SCALE = CURRT / CUROLD
     ENDIF
     !
     T0 = FCCCC0(TMF(1),TMF(2),TMF(3),TMF(4), &
          &                  PSIISO(1),PSIISO(2),PSIISO(3),PSIISO(4),SPSIM)
     CALL PRNORM(SCALE,KN)
  ENDIF
  !
  PSI0   = SPSIM
  CPSRF  = ABS(SPSIM)
  !
  IF (NRSCAL .EQ. 1) CALL RSCALE(RRAXIS,KN)
  !
  IF (KSHIFT.EQ.0) RETURN
  !
  DO J5=1,NSNT
     CPSICL(4*J5-3) = CPSICL(4*J5-3) + CPSRF
  END DO
  !
  DO J6=1,KN
     PSIISO(J6) = PSIISO(J6) + CPSRF
  END DO
  !
  if (shift_p==1._rkind) then
     call pressure_piedestal
  endif
  !
  RETURN
END SUBROUTINE NOREPT
SUBROUTINE pressure_piedestal
  !        #####################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SJ02  COMPUTES T, T-TPRIME, PRESSURE AND P-PRIME PROFILES AT     *
  !          CONSTANT FLUX SURFACES                                     *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  ! For XTOR, pressure is shifted to match Spitzer's law resistivity = eta(T)
  ! on the axis and at the plasma edge. This could be  modified for other usages.
  ! NB: This is used only by old XTOR (eta-chi MHD) and bifluid MHD version of XTOR-2F
  !     of 2010 JCP. Using this with XTOR-K and full 2F XTOR is wrong.
  !
  predge=rodabyrod0*cpr(1)/((snumber/slimit)**(2._rkind/3._rkind)-1._rkind)
  !
  cpr = cpr+predge
  !
  RETURN
END SUBROUTINE pressure_piedestal
