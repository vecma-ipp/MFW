SUBROUTINE outastro(KPSI1)
  !
  !   Construct equilibrium table A_EQU on a (R,Z) box
  !   for astro simulations 
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: KPSI1
  !
  INTEGER, PARAMETER :: NA_EQU = 16
  REAL(RKIND), allocatable, DIMENSION(:,:,:) :: A_EQU
  !
  INTEGER :: I, IGMAX, IND2, IP, IR, IR0, IRMAX, IRMIN, IS, ISHIFTZ, IT,&
       & IZ0, IZMAX, IZMIN, J
  REAL(RKIND) :: FPSIN, PDPDR, PDPDR0, PDPDR1, PDPDR1_3, PDPDR2_3, PDPDZ,&
       & PDPDZ0, PDPDZ1, PDPDZ1_3, PDPDZ2_3, RMAJOR, RMINOR, Z1, Z2, Z3, Z4,&
       & ZB, ZBND, ZBOTTOM, ZBOXLFT, ZCOST, ZDPDS0, ZDPDS1, ZDPDS1_3, ZDPDS2_3,&
       & ZDPDT0, ZDPDT1, ZDPDT1_3, ZDPDT2_3, ZDR, ZDRT, ZDSDR, ZDSDZ, ZDTDR, ZDTDZ,&
       & ZDZ, ZELONG, ZEPS, ZGER_MAX, ZGER_MIN, ZGEZ_MAX, ZGEZ_MIN, ZGGPR,&
       & ZPSI0, ZPSI1, ZR, ZRBOXLEN, ZRBOXLFT, ZRHO, ZRMAX, ZRMIN, ZRS1, ZRS2, ZRS3,&
       & ZRS4, ZS1, ZS2, ZSIG, ZSIG0, ZSIG1, ZSIG1_3, ZSIG2_3, ZSINT, ZT1, ZT2,&
       & ZTET, ZZ, ZZBOXLEN, ZZMAX, ZZMIN, ZZZLEN, aa, bb
  REAL(RKIND), DIMENSION(1,16) :: ZCPSI, ZDFDS, ZDFDT, ZF        
  REAL(RKIND), allocatable, DIMENSION(:,:) :: ZDPDS, ZDPDT, ZPSI
  REAL(RKIND) :: PS, ZDPSIS, ZDR2, ZDZ2, ZDSUM, ZDTTDR, ZDTTDZ, ZTT
  INTEGER :: ISMAX, ISMIN, ISRCHFGE, ISRCHFGT
  INTEGER :: JPOP
  REAL(RKIND) :: DDOT
  !----------------------------------------------------------------------
  !              1.   Prologue
  !
  !   Check that plasma boundary is inside box
  !
  IGMAX = NMGAUS * NT1
  IRMAX = ISMAX(IGMAX,RRISO(1,KPSI1),1)
  IRMIN = ISMIN(IGMAX,RRISO(1,KPSI1),1)
  IZMAX = ISMAX(IGMAX,RZISO(1,KPSI1),1)
  IZMIN = ISMIN(IGMAX,RZISO(1,KPSI1),1)
  ZRMIN = RRISO(IRMIN,KPSI1)
  ZRMAX = RRISO(IRMAX,KPSI1)
  ZZMIN = RZISO(IZMIN,KPSI1)
  ZZMAX = RZISO(IZMAX,KPSI1)
  !
  ZRBOXLEN = 1.1_RKIND*(ZRMAX-ZRMIN)
  ZRBOXLFT = (ZRMIN + ZRMAX)*0.5_RKIND - ZRBOXLEN*0.5_RKIND
  ZZZLEN = MAX(ABS(ZZMIN),ABS(ZZMAX)) * 2._RKIND
  ZZBOXLEN = 1.1_RKIND*ZZZLEN
  !
  !   Shift z-axis such that rzmag = 0.0
  !
  ISHIFTZ = 0
  ZBOTTOM = -0.5_RKIND*ZZBOXLEN
  !
  IF (RZMAG .NE. 0.0_RKIND) THEN
     ISHIFTZ = 1
     ZBOTTOM = RZMAG - 0.5_RKIND*ZZBOXLEN
  ENDIF
  !----------------------------------------------------------------------
  !              2.   Compute s, d/dR(psi) and d/dZ(psi)
  !
  !  COMPUTE PSI VALUE. NOTE THAT CPSICL WAS SHIFTED BY CPSRF SO SHIFT IT BACK
  !
  DO JPOP=1,NPOPULATIONS
     !
     allocate(A_EQU(NA_EQU,NRBOX_XTOR(JPOP), NZBOX_XTOR(JPOP)))
     allocate(zdpds(NRBOX_XTOR(JPOP),NZBOX_XTOR(JPOP)), &
              zdpdt(NRBOX_XTOR(JPOP),NZBOX_XTOR(JPOP)), &
              zpsi(NRBOX_XTOR(JPOP),NZBOX_XTOR(JPOP)))
     !
     a_equ = 0._rkind
     zdpds = 0._rkind
     zdpdt = 0._rkind
     zpsi  = 0._rkind
     !
     ZDR = ZRBOXLEN / REAL(NRBOX_XTOR(JPOP)-1,RKIND)
     ZDZ = ZZBOXLEN / REAL(NZBOX_XTOR(JPOP)-1,RKIND)
     !
     DO J=1,NZBOX_XTOR(JPOP)
        ZZ = ZBOTTOM + REAL(J-1,RKIND)*ZDZ
        !
        DO I=1,NRBOX_XTOR(JPOP)
           ZR = ZRBOXLFT + REAL(I-1,RKIND)*ZDR
           !
           ZRHO = SQRT((ZR - R0)**2 + (ZZ - RZ0)**2)
           ZTET = ATAN2(ZZ - RZ0,ZR - R0)
           IF (ZTET .LT. CT(1)) ZTET = ZTET + 2._RKIND * CPI
           !
           CALL BOUND(1,ZTET,ZBND)
           IT = ISRCHFGT(NT1,CT,1,ZTET) - 1
           IF (IT .LT. 1)  IT = 1
           IF (IT .GT. NT) IT = NT
           ZT1 = CT(IT)
           ZT2 = CT(IT+1)
           !
           ZSIG = ZRHO / ZBND
           !
           !     EVALUATE DRHOS/DTHETA
           ZEPS = 1.E-3_RKIND
           Z1 = ZTET - 2._RKIND * ZEPS
           Z2 = ZTET -      ZEPS
           Z3 = ZTET +      ZEPS
           Z4 = ZTET + 2._RKIND * ZEPS
           !
           CALL BOUND(1,Z1,ZRS1)
           CALL BOUND(1,Z2,ZRS2)
           CALL BOUND(1,Z3,ZRS3)
           CALL BOUND(1,Z4,ZRS4)
           ZDRT = (ZRS1 + 8._RKIND*(ZRS3 - ZRS2) -ZRS4) / (12._RKIND * ZEPS)
           !
           ZCOST = COS(ZTET)
           ZSINT = SIN(ZTET)
           ZDSDR = (ZDRT * ZSINT + ZBND * ZCOST) / ZBND**2
           ZDSDZ = (ZBND * ZSINT - ZDRT * ZCOST) / ZBND**2
           IF (ZSIG .NE. 0.0_RKIND) THEN
              ZDTDR = - ZSINT / ZRHO
              ZDTDZ = ZCOST / ZRHO
           ELSE
              ZDTDR = 0._RKIND
              ZDTDZ = 0._RKIND
           ENDIF
           !
           IF (ZSIG .LE. 1.0_RKIND) THEN
              !        INSIDE POINT
              
              IS = ISRCHFGT(NS1,CSIG,1,ZSIG)  - 1
              IF (IS .LT. 1)  IS = 1
              IF (IS .GT. NS) IS = NS
              !
              ZS1 = CSIG(IS)
              ZS2 = CSIG(IS+1)
              !        
              CALL PSICEL(IS,IT,1,1,ZCPSI,CPSICL)
              CALL BASIS1(1,1,ZS1,ZS2,ZT1,ZT2,ZSIG,ZTET,ZF)
              CALL BASIS2(1,1,ZS1,ZS2,ZT1,ZT2,ZSIG,ZTET,ZDFDS,ZDFDT)
              !
              ZPSI(I,J)      = DDOT(16,ZF,1,ZCPSI,1)
              ZDPDS(I,J)     = DDOT(16,ZDFDS,1,ZCPSI,1)
              ZDPDT(I,J)     = DDOT(16,ZDFDT,1,ZCPSI,1)
              !
              PDPDR = ZDPDS(I,J) * ZDSDR + ZDPDT(I,J) * ZDTDR
              PDPDZ = ZDPDS(I,J) * ZDSDZ + ZDPDT(I,J) * ZDTDZ
              !
              PS = SQRT(ABS(ZPSI(I,J)/CPSRF))
              !
              !
              !  COMPUTE FIRST DERIVATIVES OF THETA-TILD WITH
              !  RESPECT TO R AND Z
              !
              ZDPSIS = 2._RKIND * PS * CPSRF
              
              ZDZ2   = ZZ - RZMAG
              ZDR2   = ZR - RMAG
              ZDSUM  = ZDR2**2 + ZDZ2**2
              ZTT    = ATAN2(ZDZ2,ZDR2)
              IF (ZTT .LT. 0._RKIND) ZTT = ZTT + 2._RKIND * CPI
              !
              ZDTTDR  = - ZDZ2 / ZDSUM
              ZDTTDZ  =   ZDR2 / ZDSUM
              !
              A_EQU(1,I,J) = ZR
              A_EQU(2,I,J) = ZZ
              A_EQU(3,I,J) = PS
              A_EQU(4,I,J) = ZTT
              A_EQU(5,I,J) = PDPDR
              A_EQU(6,I,J) = PDPDZ
              !
              A_EQU(12,I,J) = (PDPDR * ZDTTDZ - PDPDZ * ZDTTDR) / (ZR*ZDPSIS)
              A_EQU(13,I,J) = (PDPDR**2 + PDPDZ**2) / ZDPSIS**2
              A_EQU(14,I,J) = ZDTTDR**2 + ZDTTDZ**2
              A_EQU(15,I,J) = 1._RKIND/ ZR**2
              A_EQU(16,I,J) = (PDPDR * ZDTTDR + PDPDZ * ZDTTDZ) / ZDPSIS
              
           ELSE IF (ZSIG .GT. 1.0_RKIND) THEN
              !
              ! set everything to zero outside sigma=1.
              !
              A_EQU(1,I,J) = ZR
              A_EQU(2,I,J) = ZZ
              A_EQU(3,I,J) = -1._rkind
              A_EQU(4,I,J) = 0._rkind
              A_EQU(5,I,J) = 0._rkind
              A_EQU(6,I,J) = 0._rkind
              !
              A_EQU(12,I,J) = 0._RKIND
              A_EQU(13,I,J) = 0._RKIND
              A_EQU(14,I,J) = 0._RKIND
              A_EQU(15,I,J) = 1._RKIND/ ZR**2
              A_EQU(16,I,J) = 0._RKIND
           ENDIF
        END DO
     END DO
     !
     !
     !
     !----------------------------------------------------------------------
     !              4.   Compute T,T',p,p',q
     !
     DO J = 1,NZBOX_XTOR(JPOP)
        DO I = 1,NRBOX_XTOR(JPOP)
           if (A_EQU(3,I,J).ne.0._rkind) then
              IP = ISRCHFGE(KPSI1,PSIISO,1,ZPSI(I,J))
              IF (IP .LE. 1)    IP = 2
              IF (IP .GE. KPSI1-1) IP = KPSI1 - 2
              A_EQU(7,I,J) = FCCCC0(TMF(IP-1),TMF(IP),TMF(IP+1),TMF(IP+2),&
                   & PSIISO(IP-1),PSIISO(IP),PSIISO(IP+1),PSIISO(IP+2),&
                   & ZPSI(I,J))
              ZGGPR = FCCCC0(TTP(IP-1),TTP(IP),TTP(IP+1),TTP(IP+2),&
                   & PSIISO(IP-1),PSIISO(IP),PSIISO(IP+1),PSIISO(IP+2),ZPSI(I,J))
              A_EQU(8,I,J) = ZGGPR / A_EQU(6,I,J)
              A_EQU(9,I,J) = FCCCC0(CPR(IP-1),CPR(IP),CPR(IP+1),CPR(IP+2),&
                   & PSIISO(IP-1),PSIISO(IP),PSIISO(IP+1),PSIISO(IP+2),&
                   & ZPSI(I,J))
              A_EQU(10,I,J) = FCCCC0(CPPR(IP-1),CPPR(IP),CPPR(IP+1),CPPR(IP+2),&
                   & PSIISO(IP-1),PSIISO(IP),PSIISO(IP+1),PSIISO(IP+2),&
                   & ZPSI(I,J))
              A_EQU(11,I,J) = FCCCC0(QPSI(IP-1),QPSI(IP),QPSI(IP+1),QPSI(IP+2),&
                   & PSIISO(IP-1),PSIISO(IP),PSIISO(IP+1),PSIISO(IP+2),&
                   & ZPSI(I,J))
           else
              A_EQU(7,I,J) = 0._rkind
              A_EQU(8,I,J) = 0._rkind
              A_EQU(9,I,J) = 0._rkind
              A_EQU(10,I,J) = 0._rkind
              A_EQU(11,I,J) = 0._rkind
           end if
        END DO
     END DO
     !----------------------------------------------------------------------
     !              5.   Output to file ASTRO
     !
     WRITE(NXTOR) NRBOX_XTOR(JPOP)
     WRITE(NXTOR) NZBOX_XTOR(JPOP)
     DO I=1,NA_EQU
        WRITE(NXTOR) A_EQU(I,1:NRBOX_XTOR(JPOP),1:NZBOX_XTOR(JPOP))
     END DO
     DEALLOCATE(a_equ,zdpds,zdpdt,zpsi)
  END DO
  !
END SUBROUTINE outastro
