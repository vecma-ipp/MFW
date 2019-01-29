!*DECK C2SA02
!*CALL PROCESS
SUBROUTINE PACKME(KN,KPOID,PMESH,PPLACE,PWIDTH,PSOLPD)
  !        =======================================================
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  USE globals
  IMPLICIT NONE
  INTEGER          ::     J5
  INTEGER          ::     I
  REAL(RKIND)      ::     ZF
  REAL(RKIND)      ::     ZDP
  REAL(RKIND)      ::     PMESH
  INTEGER          ::     J3
  REAL(RKIND)      ::     PSOLPD
  REAL(RKIND)      ::     ZC
  REAL(RKIND)      ::     PWIDTH
  REAL(RKIND)      ::     PPLACE
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZW
  REAL(RKIND)      ::     ZS
  INTEGER          ::     J2
  REAL(RKIND)      ::     ZM
  INTEGER          ::     KPOID
  INTEGER          ::     KN
  INTEGER          ::     IM
  PARAMETER (IM = 401)
  !
  DIMENSION &
       &   PMESH(KN),   PPLACE(KPOID),   PWIDTH(KPOID),   ZW(IM)
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  !
  !**********************************************************************
  !     PMESH                                                           *
  !      ^                                                              *
  !    1-!          ### : LORENTZIANS                          ++       *
  !      !                                                +++++         *
  !      !          +++ : ZW(I)                       ++++              *
  !      !                                         +++                  *
  !      !                                        +                     *
  !      !                                      ++                      *
  !      !                                    ++                        *
  !      !                               +++++                          *
  !      !                          +++++                               *
  !      !                     +++++   !                                *
  !      !                +++++        !SLOPE PSOLPD                    *
  !      !            ++++                                              *
  !      !         +++                                                  *
  !      !        +                                                     *
  !      !       +###                              ###                  *
  !      !      +# ! #                            # ! #                 *
  !      !   +++#  !  #                          #  !  #                *
  !      !+++ ##   !   ##                      ##   !   ##              *
  !    0-!---------!--------------------------------!-----------!-> S'  *
  !      0     PPLACE(1)                         PPLACE(2)      1       *
  !                                                                     *
  !**********************************************************************
  !                                                                     *
  ! 1. STEP FOR EQUIDISTANT S'-MESH                                     *
  !                                                                     *
  !**********************************************************************
  !
  ZM = 1._RKIND / REAL(IM - 1,RKIND)
  !
  !**********************************************************************
  !                                                                     *
  ! 2. FILL IN DENSITY FUNCTION                                         *
  !                                                                     *
  !**********************************************************************
  !
  DO J2=1,IM
     !
     ZS     = (J2 - 1) * ZM
     ZW(J2) = 0._RKIND
     !
     DO J1=1,KPOID
        !
        ZW(J2) = ZW(J2) + ATAN((ZS - PPLACE(J1)) / PWIDTH(J1)) &
             &                   + ATAN(      PPLACE(J1)  / PWIDTH(J1))
        !
     END DO
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 3. NORMALIZE IT TO ONE                                              *
  !                                                                     *
  !**********************************************************************
  !
  ZC = (1 - PSOLPD) / ZW(IM)
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
  PMESH(KN) = 1._RKIND
  !
  ZDP = 1._RKIND / REAL(KN - 1,RKIND)
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
END SUBROUTINE PACKME


SUBROUTINE PACKME1(KN,KPOID,PMESH,PPLACE,PDENSITY)
  !        =======================================================
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  !
  INTEGER          ::     J5
  INTEGER          ::     I
  REAL(RKIND)      ::     ZF
  REAL(RKIND)      ::     ZDP
  REAL(RKIND)      ::     PMESH
  INTEGER          ::     J3
  REAL(RKIND)      ::     PSOLPD
  REAL(RKIND)      ::     ZC
  REAL(RKIND)      ::     PDENSITY
  REAL(RKIND)      ::     PPLACE
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZW
  REAL(RKIND)      ::     ZS
  INTEGER          ::     J2
  REAL(RKIND)      ::     ZM
  INTEGER          ::     KPOID
  INTEGER          ::     KN
  INTEGER          ::     IM
  PARAMETER (IM = 501)
  !
  DIMENSION &
       &   PMESH(KN),   PPLACE(KPOID),   PDENSITY(KPOID),   ZW(IM)
  INTEGER :: J4, J7
  REAL(RKIND) :: ZDS, ZX1, ZX2
  !
  !
  !**********************************************************************
  !                                                                     *
  ! 1. STEP FOR EQUIDISTANT S'-MESH                                     *
  !                                                                     *
  !**********************************************************************
  !
  IF (KPOID.LE.1) THEN
     PRINT*,'EQUIDISTANT MESH IN PACKME1'
     RETURN
  ENDIF
  !
  zds = 1._RKIND/REAL(im-1,RKIND)
  zw(1) = 0._RKIND
  !
  if (kpoid.eq.2) then
     do j1=2,im
        zs = (j1-1)*zds
        zx1 = .9_RKIND*pplace(1)
        zx2 = pplace(1)+.1_RKIND*(1._RKIND-pplace(1))
        if (zs.le.zx1) then
           zw(j1) = zw(j1-1)+zds*pdensity(1)
        else if (zs.gt.zx1.and.zs.lt.zx2) then
           zw(j1) = zw(j1-1)+zds*fcdcd0(zx1,pdensity(1),0._RKIND, &
                & zx2,pdensity(2),0._RKIND,zs)
        else if (zs.ge.zx2) then
           zw(j1) = zw(j1-1)+zds*pdensity(2)
        endif
     END DO
  else if (kpoid.ge.3) then
     j4=1
     do j3=2,im
        zs = (j3-1)*zds
        if (j4.eq.1) then
           zx1 = .9_RKIND*pplace(1)
           zx2 = pplace(1)+.1_RKIND*(pplace(2)-pplace(1))
           if (zs.le.zx1) then
              zw(j3) = zw(j3-1)+zds*pdensity(1)
           else if (zs.gt.zx1.and.zs.lt.zx2) then
              zw(j3) = zw(j3-1)+zds*fcdcd0(zx1,pdensity(1),0._RKIND, &
                   & zx2,pdensity(2),0._RKIND,zs)
           else if (zs.ge.zx2) then
              zw(j3) = zw(j3-1)+zds*pdensity(2)
              j4=j4+1
           endif
        else if (j4.eq.kpoid) then
           zw(j3) = zw(j3-1)+zds*pdensity(kpoid)
        else if (j4.ge.2.and.j4.le.kpoid-1) then
           zx1 = pplace(j4)-.1_RKIND*(pplace(j4)-pplace(j4-1))
           zx2 = pplace(j4)+.1_RKIND*(pplace(j4)-pplace(j4-1))
           if (zs.le.zx1) then
              zw(j3) = zw(j3-1)+zds*pdensity(j4)
           else if (zs.gt.zx1.and.zs.lt.zx2) then
              zw(j3) = zw(j3-1)+zds*fcdcd0(zx1,pdensity(j4),0._RKIND, &
                   & zx2,pdensity(j4+1),0._RKIND,zs)
           else if (zs.ge.zx2) then
              zw(j3) = zw(j3-1)+zds*pdensity(j4+1)
              j4=j4+1
           endif
        endif
     END DO
  endif
  !
  do j5=2,im
     zw(j5)=zw(j5)/zw(im)
  END do
  !
  !
  !**********************************************************************
  !                                                                     *
  ! 4._RKIND FIND MESH POSITIONS                                              *
  !                                                                     *
  !**********************************************************************
  !
  PMESH( 1) = 0._RKIND
  PMESH(KN) = 1._RKIND
  !
  ZDP = 1._RKIND / REAL(KN - 1,RKIND)
  ZF  = ZDP
  I   = 1
  !
  DO J7=2,IM
     !
6    CONTINUE 
     !
     IF (ZW(J7) .LE. ZF) GOTO 7
     !
     I        = I + 1
     ZS       = (J7 - 2) * ZDS
     PMESH(I) = ZS + (ZF - ZW(J7-1)) * ZDS / (ZW(J7) - ZW(J7-1))
     ZF       = ZF + ZDP
     !
     GOTO 6
     !
7    CONTINUE 
  END DO
  !     
  return
end SUBROUTINE PACKME1

SUBROUTINE PACKME2(KN,KPOID,PMESH,PPLACE,PWIDTH,PSOLPD)
  !        =======================================================
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  USE globals
  IMPLICIT NONE
  INTEGER          ::     J5
  INTEGER          ::     I
  REAL(RKIND)      ::     ZF
  REAL(RKIND)      ::     ZDP
  REAL(RKIND)      ::     PMESH
  INTEGER          ::     J3
  REAL(RKIND)      ::     PSOLPD
  REAL(RKIND)      ::     ZC
  REAL(RKIND)      ::     PWIDTH
  REAL(RKIND)      ::     PPLACE
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZW
  REAL(RKIND)      ::     ZS
  INTEGER          ::     J2
  REAL(RKIND)      ::     ZM
  INTEGER          ::     KPOID
  INTEGER          ::     KN
  REAL(RKIND)      ::   tmp1, tmp2, ZWPL, Step
  INTEGER         ::   IS, ISMIN, NPL, KK, JJ, Npts
  INTEGER          ::     IM
  PARAMETER (IM = 401)
  !
  DIMENSION &
       &   PMESH(KN),   PPLACE(KPOID),   PWIDTH(KPOID),   ZW(IM), &
       &   tmp1(KPOID), tmp2(0:KPOID+1),   ZWPL(0:KPOID+1), Npts(0:KPOID+1), &
       &   ZDP(KN+KPOID-1),   Step(0:KPOID+1)
  !
  ! Packs and put a grid point on APLACE values (called when NMESHA=4)
  !
  !**********************************************************************
  !                                                                     *
  ! 1. STEP FOR EQUIDISTANT S'-MESH                                     *
  !                                                                     *
  !**********************************************************************
  !
  ZM = 1._RKIND / REAL(IM - 1,RKIND)
  !
  !**********************************************************************
  !                                                                     *
  ! 2. FILL IN DENSITY FUNCTION                                         *
  !                                                                     *
  !**********************************************************************
  !
  ! sort APLACE and remove multiple values
  ! put result in tmp2 with NPL number of non zero, non equal APLACE values
  tmp1=PPLACE(1:KPOID)
  NPL=0
  tmp2=0._RKIND
  DO KK=1,KPOID
     IS = ISMIN(KPOID,tmp1,1)
     IF ((tmp1(IS).LE.0._RKIND) .OR. (tmp1(IS).LE.tmp2(NPL))) THEN
        tmp1(IS)=10.
     ELSE 
        tmp2(NPL+1)=tmp1(IS)
        NPL=NPL+1
        tmp1(IS)=10.
     END IF
  END DO
  !
  ! Calculate local density function at APLACE values
  DO KK=1,NPL
     ZWPL(KK) = 0._RKIND
     DO JJ=1,KPOID
        ZWPL(KK) = ZWPL(KK) + ATAN((tmp2(KK) - PPLACE(JJ)) / PWIDTH(JJ)) &
             &                   + ATAN(      PPLACE(JJ)  / PWIDTH(JJ))
     END DO
  END DO
  !
  ! standard density function
  DO J2=1,IM
     !
     ZS     = (J2 - 1) * ZM
     ZW(J2) = 0._RKIND
     !
     DO J1=1,KPOID
        !
        ZW(J2) = ZW(J2) + ATAN((ZS - PPLACE(J1)) / PWIDTH(J1)) &
             &                   + ATAN(      PPLACE(J1)  / PWIDTH(J1))
        !
     END DO
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 3. NORMALIZE IT TO ONE                                              *
  !                                                                     *
  !**********************************************************************
  !
  ZC = (1 - PSOLPD) / ZW(IM)
  !
  ! local density function
  ZWPL(0)=0._RKIND
  ZWPL(NPL+1)=1._RKIND
  DO KK=1,NPL
     ZWPL(KK)=tmp2(KK) * PSOLPD + ZC * ZWPL(KK)
  END DO
  !
  ! standard density function
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
  PMESH(KN) = 1._RKIND
  !
  ! split the ZF domain into NPL+1 intervals
  ! Npts = nb of points per interval (boundaries included)
  ! Step = starting size of ZDP
  Npts(0)=1
  Npts(NPL+1)=KN+NPL
  DO KK=1,NPL
     Npts(KK)=FLOOR(KN*(ZWPL(KK)-ZWPL(KK-1)))
     IF (Npts(KK).GT.1.) THEN
        Step(KK)=(ZWPL(KK)-ZWPL(KK-1))/(Npts(KK)-1)
        Npts(0)=Npts(0)+Npts(KK)-1
        Npts(NPL+1)=Npts(NPL+1)-Npts(KK)
     ELSE
        Step(KK)=ZWPL(KK)-ZWPL(KK-1)
        Npts(0)=Npts(0)+1
        Npts(NPL+1)=Npts(NPL+1)-2
     END IF
  END DO
  Step(NPL+1)=(ZWPL(NPL+1)-ZWPL(NPL))/(Npts(NPL+1)-1)
  !
  ! linear variation of ZDP to avoid jumps
  Npts(0)=1
  ZDP(Npts(0):Npts(0)+Npts(1)-2)=Step(1)
  Npts(0)=Npts(0)+MAX(Npts(1)-1,1)
  DO KK=2,NPL+1
     Step(KK)=2*Step(KK)-Step(KK-1)
     DO JJ=0,Npts(KK)-2
        ZDP(Npts(0)+JJ)=((Npts(KK)-JJ-2)*Step(KK-1)+JJ*Step(KK))/MAX(Npts(KK)-2,1)
     END DO
     Npts(0)=Npts(0)+MAX(Npts(KK)-1,1)
  END DO
  !
  ! backward correspondence
  I   = 1
  JJ  = 1
  ZF  = ZDP(JJ)
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
     JJ = JJ+1
     ZF = ZF + ZDP(JJ)
     !
     GOTO 4
     !
5    CONTINUE 
  END DO
  !
  RETURN
END SUBROUTINE PACKME2
