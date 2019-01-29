!*DECK C2SA06
!*CALL PROCESS
SUBROUTINE PACKMEP(KN,KPOID,PMESH,PPLACE,PWIDTH,PSOLPD,KMESH,KMESHPOLEXP)
  !        ==============================================================
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !                                        O. SAUTER,   CRPP-EPFL
  !
  USE globals
  USE interpos_module
  IMPLICIT NONE
  interface
    SUBROUTINE bndfit(RIN,ZIN,KIN,RFIT,ZFIT,NBFIT,TENSION,R0,RZ0,KOPTION)
      !
      ! Perform periodic spline interpolation with tension on rho(theta) to smooth plasma boundary
      ! Use 0.5*[Rmax+Rmin; Zmax+Zmin] as origin for the theta, rho mesh
      !
      ! koption = 1: (optional, default if not provided) provide new R,Z points in [rfit(i),zfit(i)], i=1,nbfit
      !           2: return theta, rho in rfit, zfit respectively (used for mesh packing)
      !
      USE prec_const
      USE globals, ONLY : NVERBOSE
      USE interpos_module
      IMPLICIT NONE
      INTEGER, intent(in) :: KIN, NBFIT
      INTEGER, optional :: KOPTION
      REAL(RKIND), intent(in) :: RIN(KIN), ZIN(KIN)
      REAL(RKIND), intent(out) :: RFIT(NBFIT), ZFIT(NBFIT) 
      REAL(RKIND), intent(in), optional :: TENSION, R0, RZ0
    end SUBROUTINE bndfit
  end interface
  !
  INTEGER          ::     KMESH, KMESHPOLEXP
  INTEGER          ::     J5
  INTEGER          ::     I
  REAL(RKIND)      ::     ZF
  REAL(RKIND)      ::     ZDP
  REAL(RKIND)      ::     PMESH
  INTEGER          ::     J3
  REAL(RKIND)      ::     PSOLPD
  REAL(RKIND)      ::     ZC
  REAL(RKIND)      ::     PPLACE
  REAL(RKIND)      ::     PWIDTH
  REAL(RKIND)      ::     ZZ
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZW
  REAL(RKIND)      ::     ZS
  INTEGER          ::     J2
  REAL(RKIND)      ::     ZM, TENSION_eff, ZTHETA, ZRHO
  INTEGER          ::     KPOID
  INTEGER          ::     KN
  INTEGER          ::     IM
  PARAMETER (IM = 401)
  !
  DIMENSION &
       &   PMESH(KN),   PPLACE(KPOID),   PWIDTH(KPOID),   ZW(IM), ZTHETA(IM), ZRHO(IM)
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  !**********************************************************************
  !                                                                     *
  ! 1. STEP FOR EQUIDISTANT THETA'-MESH                                 *
  !                                                                     *
  !**********************************************************************
  !
  ZM = 2._RKIND * CPI / REAL(IM - 1,RKIND)
  !
  if (kmesh .eq. 1) then
    ! 2.1 FILL IN DENSITY FUNCTION                                         *
    DO J2=1,IM
      !
      ZS     = (J2 - 1) * ZM
      ZW(J2) = 0._RKIND
      !
      DO J1=1,KPOID
        !
        ZZ = SQRT(PWIDTH(J1)**2+1)
        !
        ZW(J2) = ZW(J2) + 2._RKIND/(ZZ*PWIDTH(J1))*( &
             &            ATAN2(PWIDTH(J1)*TAN(.25_RKIND*(ZS-PPLACE(J1))),ZZ+1)+ &
             &            ATAN2(PWIDTH(J1)*TAN(.25_RKIND*(ZS-PPLACE(J1))),ZZ-1)+ &
             &            ATAN2(PWIDTH(J1)*TAN(.25_RKIND*(   PPLACE(J1))),ZZ+1)+ &
             &            ATAN2(PWIDTH(J1)*TAN(.25_RKIND*(   PPLACE(J1))),ZZ-1))
        !
      END DO
    END DO
  elseif (kmesh .eq. 2) then
    TENSION_eff = -0.1_rkind
    call bndfit(RRBPS,RZBPS,NBPS,ZTHETA,ZRHO,IM,TENSION_eff,R0,RZ0,kmesh)
    ! integrate rho(theta) to get zw
    ! assume ztheta starts from 0
    call interpos(ZTHETA,ZRHO**kmeshpolexp,IM,tension=TENSION_eff, &
         & youtint=zw,nbc=-1,ybc=twopi)
  else
    print *,'warning, option not defined, should not be here in packmep'
    ZM = 2._RKIND * CPI / REAL(IM - 1,RKIND)
    pmesh(1:KN) = (/ (REAL(I - 1,RKIND)*zm, I=1,kn) /)
    return
  end if
  !     
  !**********************************************************************
  !                                                                     *
  ! 3. NORMALIZE IT TO ONE                                              *
  !                                                                     *
  !**********************************************************************
  !
  ZC = 2._RKIND* CPI * (1 - PSOLPD) / ZW(IM)
  !
  DO J3=1,IM
     !
     ZS     = (J3 - 1) * ZM
     ZW(J3) = ZS * PSOLPD + ZC * ZW(J3)
     !
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 4. FIND MESH POSITIONS                                              *
  !                                                                     *
  !**********************************************************************
  !
  PMESH( 1) = 0._RKIND
  PMESH(KN) = 2._RKIND*CPI
  !
  ZDP = 2._RKIND*CPI / REAL(KN - 1,RKIND)
  ZF  = ZDP
  I   = 1
  !
  DO J5=2,IM
     !
4    CONTINUE 
     !
     IF (ZW(J5) .LE. ZF) GOTO 5
     !
     I        = I + 1
     ZS       = (J5 - 2) * ZM
     PMESH(I) = ZS + (ZF - ZW(J5-1)) * ZM / (ZW(J5) - ZW(J5-1))
     ZF       = ZF + ZDP
     !
     GOTO 4
     !
5    CONTINUE 
  END DO
  !
  RETURN
END SUBROUTINE PACKMEP
