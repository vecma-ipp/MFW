SUBROUTINE outgyro(KPSI1)
  !
  !   Construct equilibrium table A_EQU on a (R,Z) box
  !   for gyrokinetic simulations (NIDEAL=7)
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: KPSI1
  !
  INTEGER, PARAMETER :: NA_EQU = 8
  REAL(RKIND), allocatable, DIMENSION(:,:,:) :: A_EQU
  CHARACTER(LEN=32) :: FNAME = 'EQCIN'
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
  REAL(RKIND), DIMENSION(NZBOX,NRBOX) :: X11, Y1, YD2P1, ZA1, ZB1
  REAL(RKIND), DIMENSION(NRBOX,NZBOX) :: ZPSI, ZDPDS, ZDPDT
  REAL(RKIND), DIMENSION(2*NRBOX,NZBOX) :: X22, Y2, YD2P2, ZA2, ZB2
  REAL(RKIND), DIMENSION(NZBOX) :: YP1, YPN
  REAL(RKIND), DIMENSION(2*NRBOX) :: YP2, YP2N
  REAL(RKIND) :: ZWORK(3*NRBOX), Z2WORK(6*NRBOX)
  INTEGER :: ISMAX, ISMIN, ISRCHFGE, ISRCHFGT
  REAL(RKIND) :: DDOT
  !----------------------------------------------------------------------
  !              1.   Prologue
  !
  allocate(A_EQU(NA_EQU,NRBOX, NZBOX))
  !
  IF (NRBOX .GT. NPISOEFF) THEN
    PRINT *,'NRBOX=',NRBOX,' .GT. NPISOEFF=',NPISOEFF
    PRINT *,'CHANGE NRBOX OR OUTGYRO'
    STOP 'NRBOX'
  END IF
  IF (NZBOX .GT. NPISOEFF) THEN
    PRINT *,'NZBOX=',NZBOX,' .GT. NPISOEFF=',NPISOEFF
    PRINT *,'CHANGE NZBOX OR OUTGYRO'
    STOP 'NZBOX'
  END IF
  !
  A_EQU(1:NA_EQU,1:NRBOX,1:NZBOX) = 0.0_RKIND
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
  ZDR = ZRBOXLEN / REAL(NRBOX-1,RKIND)
  ZDZ = ZZBOXLEN / REAL(NZBOX-1,RKIND)
  !
  !        CONSTRUCT THE MESH SO THAT THE MESH CENTER IS LOCATED ON  RRMAG,RZMAG
  !
  IR0     = (RMAG - ZRBOXLFT)/ZDR
  ZRMAX   = ZRBOXLFT + REAL(NRBOX-1,RKIND) * (RMAG - ZRBOXLFT)/REAL(IR0,RKIND)
  ZRBOXLEN= ZRMAX  - ZRBOXLFT
  ZDR     = ZRBOXLEN / REAL(NRBOX-1,RKIND)
  !
  IZ0     = (RZMAG - ZBOTTOM)/ZDZ
  ZZMAX   = ZBOTTOM + REAL(NZBOX-1,RKIND) * (RZMAG - ZBOTTOM)/REAL(IZ0,RKIND)
  ZZBOXLEN=  ZZMAX - ZBOTTOM 
  ZDZ = ZZBOXLEN / REAL(NZBOX-1,RKIND)
  !

  DO J=1,NZBOX
     ZZ = ZBOTTOM + REAL(J-1,RKIND)*ZDZ
     !
     DO I=1,NRBOX
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
           A_EQU(1,I,J) = SQRT(ABS(DDOT(16,ZF,1,ZCPSI,1)/CPSRF))
           A_EQU(2,I,J) = PDPDR
           A_EQU(3,I,J) = PDPDZ

        ELSE IF (ZSIG .GT. 1.0_RKIND) THEN
           !
           !        CUBIC
           !     EVALUATE DPSI/DSIGMA AT SIGMA = (CSIG(NS)+1.)*1/3 AND (CSIG(NS)+1.)*2/3
           !
           ZSIG1_3 = (CSIG(NS)+1._RKIND)*1._RKIND/3._RKIND
           ZSIG1_3 = (CSIG(NS)*2._RKIND+1._RKIND)*1._RKIND/3._RKIND
           IS = NS
           ZS1 = CSIG(IS)
           ZS2 = CSIG(IS+1)
           CALL PSICEL(IS,IT,1,1,ZCPSI,CPSICL)
           CALL BASIS2(1,1,ZS1,ZS2,ZT1,ZT2,ZSIG1_3,ZTET,ZDFDS,ZDFDT)
           ZDPDS1_3 = DDOT(16,ZDFDS,1,ZCPSI,1)
           ZDPDT1_3 = DDOT(16,ZDFDT,1,ZCPSI,1)
           !
           ZSIG2_3 = (CSIG(NS)+2._RKIND)*1._RKIND/3._RKIND                 
           CALL PSICEL(IS,IT,1,1,ZCPSI,CPSICL)
           CALL BASIS2(1,1,ZS1,ZS2,ZT1,ZT2,ZSIG2_3,ZTET,ZDFDS,ZDFDT)
           ZDPDS2_3 = DDOT(16,ZDFDS,1,ZCPSI,1)
           ZDPDT2_3 = DDOT(16,ZDFDT,1,ZCPSI,1)
           !
           !        EVALUATE PSI AT SIGMA = CSIG(NS)
           ZSIG0 = CSIG(NS)
           IS = NS
           ZS1 = CSIG(IS)
           ZS2 = CSIG(IS+1)
           CALL PSICEL(IS,IT,1,1,ZCPSI,CPSICL)
           CALL BASIS1(1,1,ZS1,ZS2,ZT1,ZT2,ZSIG0,ZTET,ZF)
           ZPSI0 = DDOT(16,ZF,1,ZCPSI,1)
           !        EVALUATE DPSI/DSIGMA AT SIGMA = CSIG(NS)
           CALL BASIS2(1,1,ZS1,ZS2,ZT1,ZT2,ZSIG0,ZTET,ZDFDS,ZDFDT)
           ZDPDS0 = DDOT(16,ZDFDS,1,ZCPSI,1)
           ZDPDT0 = DDOT(16,ZDFDT,1,ZCPSI,1)
           !        
           !        EVALUATE PSI AT SIGMA = 1.0
           ZSIG1 = 1.0_RKIND
           CALL PSICEL(IS,IT,1,1,ZCPSI,CPSICL)
           CALL BASIS1(1,1,ZS1,ZS2,ZT1,ZT2,ZSIG1,ZTET,ZF)
           ZPSI1 = DDOT(16,ZF,1,ZCPSI,1)
           !        
           !        EVALUATE DPSI/DSIGMA AT SIGMA = 1.0
           CALL BASIS2(1,1,ZS1,ZS2,ZT1,ZT2,ZSIG1,ZTET,ZDFDS,ZDFDT)
           ZDPDS1 = DDOT(16,ZDFDS,1,ZCPSI,1)
           ZDPDT1 = DDOT(16,ZDFDT,1,ZCPSI,1)
           !        
           !        EVALUATE PSI AT ZSIG WITH CUBIC
           !
           !******** FOR KUA MODIFICATION  1.96
           !
           aa = (ZPSI1-ZPSI0)/(ZSIG1-ZSIG0)
           bb = (ZPSI1-aa*ZSIG1)
           ZPSI(I,J) = aa*ZSIG + bb
           A_EQU(1,I,J)=SQRT(ABS(ZPSI(I,J)/CPSRF))
           !        
           !        EVALUATE DPSI/DR,DPSI/DZ AT ZSIG WITH CUBIC
           !

           PDPDR0   = ZDPDS0    * ZDSDR + ZDPDT0    * ZDTDR
           PDPDR1_3 = ZDPDS1_3  * ZDSDR + ZDPDT1_3  * ZDTDR
           PDPDR2_3 = ZDPDS2_3  * ZDSDR + ZDPDT2_3  * ZDTDR
           PDPDR1   = ZDPDS1    * ZDSDR + ZDPDT1    * ZDTDR
           !
           PDPDZ0   = ZDPDS0    * ZDSDZ + ZDPDT0    * ZDTDZ
           PDPDZ1_3 = ZDPDS1_3  * ZDSDZ + ZDPDT1_3  * ZDTDZ
           PDPDZ2_3 = ZDPDS2_3  * ZDSDZ + ZDPDT2_3  * ZDTDZ
           PDPDZ1   = ZDPDS1    * ZDSDZ + ZDPDT1    * ZDTDZ

           !
           PDPDR   = FCCCC0(PDPDR0,PDPDR1_3,PDPDR2_3,PDPDR1,ZSIG0,&
                & ZSIG1_3,ZSIG2_3 ,ZSIG1,ZSIG)
           PDPDZ   = FCCCC0(PDPDZ0,PDPDZ1_3,PDPDZ2_3,PDPDZ1,ZSIG0,&
                & ZSIG1_3,ZSIG2_3 ,ZSIG1,ZSIG)

           !
           A_EQU(2,I,J) = PDPDR
           A_EQU(3,I,J) = PDPDZ
        ENDIF
     END DO
  END DO
  !
  !----------------------------------------------------------------------
  !              3.   Compute d2psi/dr2, d2psi/dz2, d2psi/drz 
  !                   using bicubic splines
  !
  DO I=1,NRBOX
     DO J=1,NZBOX
        X11(J,I) = ZRBOXLFT + REAL(I-1,RKIND)*ZDR
        Y1(J,I)  = A_EQU(2,I,J)
     END DO
  END DO
  !         
  DO J=1,NZBOX
     YP1(J)= FQQQ1(A_EQU(2,2,J),A_EQU(2,3,J),&
          & A_EQU(2,4,J),X11(J,2),X11(J,3),X11(J,4),&
          & X11(J,1))
     YPN(J) = FQQQ1(A_EQU(2,NRBOX-1,J),A_EQU(2,NRBOX-2,J),&
          & A_EQU(2,NRBOX-3,J),X11(J,NRBOX-1),X11(J,NRBOX-2),&
          & X11(J,NRBOX-3),X11(J,NRBOX))
     !
     A_EQU(4,1,J) = YP1(J)
  END DO
  !
  CALL MSPLINE(X11,Y1,NRBOX,NZBOX,NZBOX,YP1,YPN,YD2P1,ZA1,ZB1,ZWORK)
  DO I=2,NRBOX
     IR = I-1
     DO J=1,NZBOX
        A_EQU(4,I,J) =&
             & (Y1(J,I)-Y1(J,IR))/(X11(J,I) - X11(J,IR)) +&
             & YD2P1(J,IR) * (X11(J,I) - X11(J,IR)) / 6._RKIND +&
             & YD2P1(J,I) * (X11(J,I) - X11(J,IR)) / 3._RKIND 
     END DO
  END DO
  !
  DO J=1,NZBOX
     DO I=1,NRBOX
        X22(I,J)       = ZBOTTOM + REAL(J-1,RKIND)*ZDZ
        X22(I+NRBOX,J) = ZBOTTOM + REAL(J-1,RKIND)*ZDZ
        Y2(I,J)        = A_EQU(3,I,J)
        Y2(I+NRBOX,J)  = A_EQU(2,I,J)
     END DO
  END DO
  !         
  DO I=1,NRBOX
     YP2(I)= FQQQ1(A_EQU(3,I,2),A_EQU(3,I,3),&
          & A_EQU(3,I,4),X22(I,2),X22(I,3),X22(I,4),&
          & X22(I,1))
     YP2N(I)= FQQQ1(A_EQU(3,I,NZBOX-1),&
          & A_EQU(3,I,NZBOX-2),A_EQU(3,I,NZBOX-3),&
          & X22(I,NZBOX-1),X22(I,NZBOX-2),X22(I,NZBOX-3),&
          & X22(I,NZBOX))
     !
     YP2(I+NRBOX)= FQQQ1(A_EQU(2,I,2),A_EQU(2,I,3),&
          & A_EQU(2,I,4),X22(I,2),X22(I,3),X22(I,4),&
          & X22(I,1))
     YP2N(I+NRBOX)= FQQQ1(A_EQU(2,I,NZBOX-1),&
          & A_EQU(2,I,NZBOX-2),A_EQU(2,I,NZBOX-3),&
          & X22(I,NZBOX-1),X22(I,NZBOX-2),X22(I,NZBOX-3),&
          & X22(I,NZBOX))
     !
     A_EQU(5,I,1) = YP2(I)
     A_EQU(6,I,1) = YP2(I+NRBOX)
  END DO
  !
  CALL MSPLINE(X22,Y2,NZBOX,2*NRBOX,2*NRBOX,YP2,YP2N,YD2P2,ZA2,ZB2,Z2WORK)
  !
  DO J=2,NZBOX
     IS = J-1
     DO I=1,NRBOX
        IND2 = I+NRBOX
        A_EQU(5,I,J) =&
             & (Y2(I,J)-Y2(I,IS))/(X22(I,J) - X22(I,IS))&
             & + YD2P2(I,IS) * (X22(I,J) - X22(I,IS)) / 6._RKIND&
             & +YD2P2(I,J) * (X22(I,J) - X22(I,IS)) / 3._RKIND 
        A_EQU(6,I,J) =&
             & (Y2(IND2,J)-Y2(IND2,IS))/(X22(IND2,J) - X22(IND2,IS))&
             & + YD2P2(IND2,IS) * (X22(IND2,J) - X22(IND2,IS)) / 6._RKIND +&
             & YD2P2(IND2,J) * (X22(IND2,J) - X22(IND2,IS)) / 3._RKIND 
     END DO
  END DO
  !
  !----------------------------------------------------------------------
  !              4.   Compute T,T'
  !
  DO J = 1,NZBOX
     DO I = 1,NRBOX
        IP = ISRCHFGE(NISO1EFF,PSIISO,1,ZPSI(I,J))
        IF (IP .LE. 1)    IP = 2
        IF (IP .GE. NISO1EFF) IP = NISO1EFF - 1
        A_EQU(7,I,J) = FCCCC0(TMF(IP-1),TMF(IP),TMF(IP+1),TMF(IP+2),&
             & PSIISO(IP-1),PSIISO(IP),PSIISO(IP+1),PSIISO(IP+2),&
             & ZPSI(I,J))
        ZGGPR = FCCCC0(TTP(IP-1),TTP(IP),TTP(IP+1),TTP(IP+2),&
             & PSIISO(IP-1),PSIISO(IP),PSIISO(IP+1),PSIISO(IP+2),ZPSI(I,J))
        A_EQU(8,I,J) = ZGGPR / A_EQU(7,I,J)
     END DO
  END DO
  !----------------------------------------------------------------------
  !              5.   Output to file EQCIN
  !
!!! Notes: In this version NCORD and NRHOT are not defined (TTM)
  !
  ZGER_MIN =  ZRBOXLFT
  ZGER_MAX =  ZGER_MIN + ZRBOXLEN
  ZGEZ_MIN =  ZBOTTOM
  ZGEZ_MAX =  ZGEZ_MIN + ZZBOXLEN
  !
  IRMAX  = ISMAX(NBPSOUT,RRBPSOU,1)
  IRMIN  = ISMIN(NBPSOUT,RRBPSOU,1)
  IZMAX  = ISMAX(NBPSOUT,RZBPSOU,1)
  IZMIN  = ISMIN(NBPSOUT,RZBPSOU,1)
  ZRMIN  = RRBPSOU(IRMIN)
  ZRMAX  = RRBPSOU(IRMAX)
  ZZMIN  = RZBPSOU(IZMIN)
  ZZMAX  = RZBPSOU(IZMAX)
  RMAJOR = 0.5_RKIND*(ZRMIN+ZRMAX)
  RMINOR = 0.5_RKIND*(ZRMAX-ZRMIN)
  ZELONG = (ZZMAX -ZZMIN)/(ZRMAX-ZRMIN)
  !
  IF (NSURF .EQ.2 .OR. NSURF.EQ.4) THEN
     ZB = TRIANG
  ELSE
     ZB     = 0._RKIND
  ENDIF
  !
  FPSIN=ABS(SPSIM)
  !
  OPEN(NUCIN, FILE=TRIM(FNAME), FORM='UNFORMATTED')
  WRITE(NUCIN) NA_EQU, NRBOX, NZBOX, 1, KPSI1, KPSI1
  WRITE(NUCIN) RMAG, RZMAG, R0, RZ0
  WRITE(NUCIN) ZGER_MIN, ZGER_MAX, ZGEZ_MIN, ZGEZ_MAX
  WRITE(NUCIN) RMAJOR, RMINOR, ZELONG, ZB, T0, Q0, FPSIN
  WRITE(NUCIN) BETA, BETAP, BETAS, BETAX, Q0, QPSI(NISO1EFF1),&
       &  ARATIO(NISO1EFF1), VOLUME, AREA, CP0
  DO I=1,NA_EQU
     WRITE(NUCIN) A_EQU(I,1:NRBOX,1:NZBOX)
  END DO
  WRITE(NUCIN) SMISO(1:KPSI1)
  WRITE(NUCIN) QPSI(1:KPSI1)
  WRITE(NUCIN) CPR(1:KPSI1)
  WRITE(NUCIN) CPPR(1:KPSI1)
  CLOSE(NUCIN)
  !
  DEALLOCATE(A_EQU)
  !
END SUBROUTINE outgyro
