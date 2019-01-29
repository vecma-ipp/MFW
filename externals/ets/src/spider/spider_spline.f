      module spider_spline

      contains

      SUBROUTINE INTRPTAU(PXIN,PYIN,PYINNEW,PY2,KNIN,PXOUT,PYOUT,PYOUTP,
     +  PYOUTPP,KNOUT,KOPT,PTAUS,PWORK,PAMAT,MDAMAT,PBCLFT,PBCRGT)
C     =================================================================
C
C     NOTE: THIS ROUTINE INCLUDES THE STANDARD CUBIC SPLINE IF PTAUS=0:
C           THEN PYINNEW IS NOT USED AND MDAMAT=3 IS SUFFICIENT
C           (=> PYINNEW(1) OR PYINNEW=PYIN IS OK)
C
C     Interpolate (pxin,pyin) on (pxout,pyout) using
C     Hirshman fitted cubic spline with ptaus value or
C     standard cubic spline if PTAUS=0
C
C     KOPT = 0: ONLY INTERPOLATE FUNCTION INTO PYOUT
C     KOPT = 1: INTERPOLATE FUNCTION INTO PYOUT AND 1ST DER. INTO PYOUTP
C     KOPT = 2: AS KOPT=1 PLUS 2ND DER. INTO PYOUTPP
C
C     BOUNDARY CONDITIONS FOR CUBIC SPLINE: PBCLFT AND PBCRGT
C     THEN IF
C     = 2  : B.C.: 2ND DERIVATIVE (NATURAL)
C     = 1  : B.C.: 1ST DERIVATIVE FROM CUBIC LAGRANGE INTERPOLATION
C     = 0  : B.C.: 1ST DERIVATIVE = 0.0
C     = 3  : B.C.: 1ST DERIVATIVE FROM LINEAR INTERPOLATION
C     OTHERWISE: B.C: 1ST DERIVATIVE = PBCLFT OR PBCRGT
C

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION PXIN(KNIN), PYIN(KNIN), PXOUT(KNOUT), PYOUT(KNOUT),
     +  PYOUTP(KNOUT), PYOUTPP(KNOUT), PY2(KNIN), PWORK(KNIN),
     +  PAMAT(MDAMAT,KNIN), PYINNEW(KNIN)
C-----------------------------------------------------------------------
C
      ZEPS = 1.0D-12
C
CL    1. PREPARE SPLINE (DEFAULT <=> PKBCOPT.EQ.0)
C
C     LEFT
C
      ZYP1 = PBCLFT
      IF (ABS(PBCLFT - 1.D0) .LE. ZEPS) ZYP1 = -1.D+32
      IF (ABS(PBCLFT - 2.D0) .LE. ZEPS) ZYP1 =  1.D+32
      IF (ABS(PBCLFT - 3.D0) .LE. ZEPS) ZYP1 = -1.D+34
C
C     RIGHT
C
      ZYPN = PBCRGT
      IF (ABS(PBCRGT - 1.D0) .LE. ZEPS) ZYPN = -1.D+32
      IF (ABS(PBCRGT - 2.D0) .LE. ZEPS) ZYPN=  1.D+32
      IF (ABS(PBCRGT - 3.D0) .LE. ZEPS) ZYPN = -1.D+34
C
      CALL CUBSPLFIT(PXIN,PYIN,PYINNEW,KNIN,ZYP1,ZYPN,PY2,PTAUS
     +  ,PWORK,PAMAT,MDAMAT)
C
CL    2. COMPUTE INTERPOLATED VALUE AT EACH PXOUT
C
      DO I=1,KNOUT
        IF (PTAUS .EQ. 0.D0) THEN
c%OS        CALL SPLINT(PXIN,PYIN,PY2,KNIN,PXOUT(I),ZY,ZYP,ZYPP)
          CALL SPLINTEND(PXIN,PYIN,PY2,KNIN,PXOUT(I),ZY,ZYP,ZYPP,1)
        ELSE
c%OS        CALL SPLINT(PXIN,PYINNEW,PY2,KNIN,PXOUT(I),ZY,ZYP,ZYPP)
          CALL SPLINTEND(PXIN,PYINNEW,PY2,KNIN,PXOUT(I),ZY,ZYP,ZYPP,1)
        ENDIF
        PYOUT(I) = ZY
        IF (KOPT .GE. 1) PYOUTP(I) = ZYP
        IF (KOPT .EQ. 2) PYOUTPP(I) = ZYPP
      END DO
C
      RETURN
      END SUBROUTINE
C-----------------------------------------------------------------------
      SUBROUTINE CUBSPLFIT(X,Y,YNEW,N,YP1,YPN,Y2,TAUS,WORK,AMAT,
     +  MDAMAT)
C
C     PREPARE SECOND DERIVATIVE OF CUBIC SPLINE INTERPOLATION AND NEW
C     VALUES OF Y AT NODES YNEW FITTED SUCH THAT CHI**2 + TAUS*F''**2
C     IS MINIMIZED ACCORDING TO HIRSHMAN ET AL, PHYS. PLASMAS 1 (1994) 2280.
C     TAUS = TAU*SIGMA_K OF PAPER ASSUMING SIGMA_K CONSTANT.
C
C     SETTING TAUS=0., ONE FINDS THE USUAL CUBIC SPLINE INT. WITH CHI**2=0
C     TAUS LARGE => FIT CLOSER TO STRAIGHT LINE (SECOND DERIV.=0)
C
C     IF LAPACK ROUTINES NOT AVAILABLE, USE NONSYM.F AND REMOVE "c%nonsym"
C
C     IF TAUS=0, YNEW NOT USED => YNEW(1) OR YNEW=Y IS OK
C

      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER(MNONSYM=501)
      DIMENSION X(N), Y(N), Y2(N), WORK(N),
     +  AMAT(MDAMAT,N), YNEW(N)
     +  ,ANONSYM(MNONSYM*7), IPIVOT(MNONSYM)
c%nonsym
c%OS         INCLUDE 'CUCCCC.inc'
c.......................................................................
c.......................................................................
C*COMDECK CUCCCC
C ----------------------------------------------------------------------
C --     STATEMENT FUNCTION FOR CUBIC INTERPOLATION                   --
C --                         23.04.88            AR        CRPP       --
C --                                                                  --
C -- CUBIC INTERPOLATION OF A FUNCTION F(X)                           --
C -- THE EIGHT ARGUMENTS A1,A2,A3,A4,B1,B2,B3,B4 ARE DEFINED BY:      --
C -- F(B1) = A1 , F(B2) = A2 , F(B3) = A3 , F(B4) = A4                --
C ----------------------------------------------------------------------
C
         FA3(A1,A2,A3,A4,B1,B2,B3,B4) =
     F        (A1-A2) / ((B1-B2)*(B2-B4)*(B2-B3)) +
     F        (A1-A3) / ((B4-B3)*(B3-B1)*(B3-B2)) +
     F        (A1-A4) / ((B1-B4)*(B2-B4)*(B3-B4))
         FA2(A1,A2,A3,A4,B1,B2,B3,B4) =
     F        (A1-A2) / ((B2-B1)*(B3-B2)) +
     F        (A3-A1) / ((B3-B1)*(B3-B2)) -
     F        (B1+B2+B3) * FA3(A1,A2,A3,A4,B1,B2,B3,B4)
         FA1(A1,A2,A3,A4,B1,B2,B3,B4) =
     F        (A1-A2) / (B1-B2) -
     F        (B1+B2) * FA2(A1,A2,A3,A4,B1,B2,B3,B4) -
     F        (B1*B1+B1*B2+B2*B2) * FA3(A1,A2,A3,A4,B1,B2,B3,B4)
         FA0(A1,A2,A3,A4,B1,B2,B3,B4) =
     F        A1 -
     F        B1 * (FA1(A1,A2,A3,A4,B1,B2,B3,B4) +
     F              B1 * (FA2(A1,A2,A3,A4,B1,B2,B3,B4) +
     F                    B1 * FA3(A1,A2,A3,A4,B1,B2,B3,B4)))
C ----------------------------------------------------------------------
C -- FCCCC0 GIVES THE VALUE OF THE FUNCTION AT POINT PX:              --
C -- FCCCC0(......,PX) = F(PX)                                        --
C ----------------------------------------------------------------------
        FCCCC0(A1,A2,A3,A4,B1,B2,B3,B4,PX) =
     F              FA0(A1,A2,A3,A4,B1,B2,B3,B4) +
     F              PX * (FA1(A1,A2,A3,A4,B1,B2,B3,B4) +
     F                    PX * (FA2(A1,A2,A3,A4,B1,B2,B3,B4) +
     F                          PX * FA3(A1,A2,A3,A4,B1,B2,B3,B4)))
C ----------------------------------------------------------------------
C -- FCCCC1 GIVES THE VALUE OF THE FIRST DERIVATIVE OF F(X) AT PX:    --
C -- FCCCC1(......,PX) = DF/DX (PX)                                   --
C ----------------------------------------------------------------------
        FCCCC1(A1,A2,A3,A4,B1,B2,B3,B4,PX) =
     F              FA1(A1,A2,A3,A4,B1,B2,B3,B4) +
     F              PX * (2.D0 * FA2(A1,A2,A3,A4,B1,B2,B3,B4) +
     F                    3.D0 * PX * FA3(A1,A2,A3,A4,B1,B2,B3,B4))
C ----------------------------------------------------------------------
C -- FCCCC2 GIVES THE VALUE OF THE SECOND DERIVATIVE OF F(X) AT PX:   --
C -- FCCCC2(......,PX) = D2F/DX2 (PX)                                 --
C ----------------------------------------------------------------------
         FCCCC2(A1,A2,A3,A4,B1,B2,B3,B4,PX) =
     F             2.D0 * FA2(A1,A2,A3,A4,B1,B2,B3,B4) +
     F             6.D0 * FA3(A1,A2,A3,A4,B1,B2,B3,B4) * PX
C ----------------------------------------------------------------------
C -- FCCCC3 GIVES THE VALUE OF THE THIRD DERIVATIVE OF F(X) AT PX:     -
C -- FCCCC3(......,PX) = D3F/DX3 (PX)                                  -
C ----------------------------------------------------------------------
         FCCCC3(A1,A2,A3,A4,B1,B2,B3,B4,PX) =
     F                      6.D0* FA3(A1,A2,A3,A4,B1,B2,B3,B4)
c.......................................................................
c.......................................................................
C
C-----------------------------------------------------------------------
C
      DO I=1,N
        Y2(I) = 0.D0
      END DO
      DO I=1,MDAMAT
        DO J=1,N
          AMAT(I,J) = 0.D0
        END DO
      END DO
C
C     PREPARE 1 / H_K
C
      DO K=1,N-1
        WORK(K) = 1.D0/ (X(K+1) - X(K))
      END DO
C
      ZTAUEFF = TAUS
C
C     PREPARE BAND WIDTH
C
      IUP = 2
      IF (ZTAUEFF .EQ. 0.D0) IUP = 1
      IBAND = 2*IUP + 1
c%OS      IDIAG = IUP + 1
c%OS      IF (MDAMAT .LT. IUP+1) THEN
c%OS        PRINT *,' MDAMAT= ',MDAMAT,' < IUP+1= ',IUP
c%OS        STOP
c%OS      ENDIF
c%nonsym
      IDIAG = IBAND
      IF (MNONSYM*7 .LT. (3*IUP+1)*N) THEN
        PRINT *,' MMNONSYM*7= ',MNONSYM*7,' < (3*IUP+1)*N= ',(3*IUP+1)*N
        STOP
      ENDIF
c%nonsym
C
C     CONSTRUCT MATRIX AND R.H.S
C
c.......................................................................
C     AS MATRIX SYMMETRIC, COMPUTE ONLY UPPER PART
C
C     K=2,N-2 (BULK PART)
C
      DO K=2,N-2
C     A(K,K)
        AMAT(IDIAG,K) = (1.D0/WORK(K)+1.D0/WORK(K-1)) / 3.D0
     +    + 2.D0*ZTAUEFF*(WORK(K)**2 + WORK(K)*WORK(K-1)+WORK(K-1)**2)
C     A(K,K+1)
        AMAT(IDIAG-1,K+1) = 1.D0/WORK(K)/6.D0
     +    - ZTAUEFF*WORK(K)*(WORK(K+1)+2.D0*WORK(K)+WORK(K-1))
C     A(K,K+2)
        IF (IUP .EQ. 2) AMAT(IDIAG-2,K+2) = ZTAUEFF*WORK(K+1)*WORK(K)
C     B(K)
        Y2(K) = (Y(K+1)-Y(K))*WORK(K) - (Y(K)-Y(K-1))*WORK(K-1)
C
      END DO
C
C     FOR K=1, N-1, N: SAME AS ABOVE WITH WORK1/2(-1;0;N;N+1) = 0
C
C     K=1
C
      K = 1
      AMAT(IDIAG,K) = 1.D0/WORK(K) / 3.D0
     +  + 2.D0*ZTAUEFF*WORK(K)**2
      AMAT(IDIAG-1,K+1) = 1.D0/WORK(K)/6.D0
     +  - ZTAUEFF*WORK(K)*(WORK(K+1)+2.D0*WORK(K))
      IF (IUP .EQ. 2) AMAT(IDIAG-2,K+2) = ZTAUEFF*WORK(K+1)*WORK(K)
      Y2(K) = (Y(K+1)-Y(K))*WORK(K)
C
C     K=N-1
C
      K = N-1
      AMAT(IDIAG,K) = (1.D0/WORK(K)+1.D0/WORK(K-1)) / 3.D0
     +  + 2.D0*ZTAUEFF*(WORK(K)**2 + WORK(K)*WORK(K-1)+WORK(K-1)**2)
      AMAT(IDIAG-1,K+1) = 1.D0/WORK(K)/6.D0
     +  - ZTAUEFF*WORK(K)*(2.D0*WORK(K)+WORK(K-1))
      Y2(K) = (Y(K+1)-Y(K))*WORK(K) - (Y(K)-Y(K-1))*WORK(K-1)
C
C     K = N
C
      K = N
      AMAT(IDIAG,K) = 1.D0/WORK(K-1) / 3.D0
     +    + 2.D0*ZTAUEFF*WORK(K-1)**2
      Y2(K) = - (Y(K)-Y(K-1))*WORK(K-1)
C
C.......................................................................
C     BOUNDARY CONDITIONS
C
      IF (YP1 .GT. .99D30) THEN
C     SECOND DERIVATIVE = 0 (NATURAL B.C.) AND SYMMETRIZE
        Y2(1) = 0.D0
        AMAT(IDIAG,1) = 1.D0
        AMAT(IDIAG-1,2) = 0.D0
        IF (IUP .EQ. 2) AMAT(IDIAG-2,3) = 0.D0
        ZYP1 = 0.D0
      ELSE IF (-YP1 .GT. .99D+34) THEN
        ZYP1 = (Y(2)-Y(1))/(X(2)-X(1))
      ELSE IF (-YP1 .GT. .99D+30) THEN
        ZYP1 = FCCCC1(Y(1),Y(2),Y(3),Y(4),X(1),X(2),X(3),X(4),X(1))
      ELSE
        ZYP1 = YP1
      ENDIF
      IF (YPN .GT. .99D30) THEN
C     SECOND DERIVATIVE = 0 (NATURAL B.C.) AND SYMMETRIZE
        Y2(N) = 0.D0
        AMAT(IDIAG,N) = 1.D0
        AMAT(IDIAG-1,N) = 0.D0
        IF (IUP .EQ. 2) AMAT(IDIAG-2,N) = 0.D0
        ZYPN = 0.D0
      ELSE IF (-YPN .GT. .99D+34) THEN
        ZYPN = (Y(N)-Y(N-1))/(X(N)-X(N-1))
      ELSE IF (-YPN .GT. .99D+30) THEN
        ZYPN = FCCCC1(Y(N-3),Y(N-2),Y(N-1),Y(N),
     +    X(N-3),X(N-2),X(N-1),X(N),X(N))
      ELSE
        ZYPN = YPN
      ENDIF
C
      Y2(1) = Y2(1) - ZYP1
      Y2(N) = Y2(N) + ZYPN
C
C     SOLVE SYSTEM
C
      IDIMA = MDAMAT
      IDIMRHS = N
      IRHS = 1
c%OSc   CRAY
c%OSc%OS      CALL SPBTRF('U',N,IUP,AMAT,IDIMA,INFO)
c%OSC   SUN, SGI
c%OS      CALL DPBTRF('U',N,IUP,AMAT,IDIMA,INFO)
c%OS      IF (INFO .EQ. 0) THEN
c%OSc   CRAY
c%OSc%OS        CALL SPBTRS('U',N,IUP,IRHS,AMAT,IDIMA,Y2,IDIMRHS,INFO2)
c%OSC   SUN, SGI
c%OS        CALL DPBTRS('U',N,IUP,IRHS,AMAT,IDIMA,Y2,IDIMRHS,INFO2)
c%OS      ELSE
c%OS        PRINT *,' ERROR IN SPBTRF: INFO = ',INFO
c%OS        STOP 'INFO'
c%OS      ENDIF
C
c%nonsym
      DO I=1,IBAND*N
        ANONSYM(I) = 0.0
      ENDDO
      DO I=1,N
C     UPPER PART
        DO J=max(1,i),min(i+IUP,n)
          ANONSYM((I-1)*IBAND+J-I+IUP+1) = AMAT(IDIAG+I-J,J)
        ENDDO
C     LOWER PART
        DO J=max(1,i-IUP),min(i-1,n-1)
          ANONSYM((I-1)*IBAND+J-I+IUP+1) = AMAT(IDIAG+J-I,I)
        ENDDO
      ENDDO
      CALL NONSYM(ANONSYM,AMAT,Y2,N,IUP,IUP,1.0D-06,INFO2)
c%nonsym
C
C     ADAPT Y AT NODES
C
      IF (ZTAUEFF .NE. 0.D0) THEN
C
        DO K=2,N-1
          YNEW(K) = Y(K) - ZTAUEFF* ((Y2(K+1)-Y2(K))*WORK(K)
     +      - (Y2(K)-Y2(K-1))*WORK(K-1))
        ENDDO
        YNEW(1) = Y(1) - ZTAUEFF * (Y2(2)-Y2(1))*WORK(1)
        YNEW(N) = Y(N) + ZTAUEFF * (Y2(N)-Y2(N-1))*WORK(N-1)
C
      ENDIF

      IF (INFO2 .LT. 0) THEN
        PRINT *,' ERROR IN SPBTRS: INFO2 = ',INFO2
        STOP 'INFO2'
      ENDIF
C
      RETURN
      END SUBROUTINE
C-----------------------------------------------------------------------
      SUBROUTINE SPLINE(X,Y,N,YP1,YPN,Y2)

      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER (NMAX=100)
      DIMENSION X(N),Y(N),Y2(N),U(NMAX)
C
      IF (N .GT.NMAX) THEN
        PRINT *,' NMAX TOO SMALL IN SPLINE: N,NMAX= ',N,NMAX
        STOP
      ENDIF
      IF (YP1.GT..99D30) THEN
        Y2(1)=0.D0
        U(1)=0.D0
      ELSE
C%OS
        IF (-YP1 .GT. .99D+30) THEN
          YP1 = (Y(2)-Y(1))/(X(2)-X(1))
        ENDIF
C%OS
        Y2(1)=-0.5D0
        U(1)=(3.D0/(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      ENDIF
      DO 11 I=2,N-1
        SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
        P=SIG*Y2(I-1)+2.D0
        Y2(I)=(SIG-1.D0)/P
        U(I)=(6.D0*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))
     *      /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
11    CONTINUE
      IF (YPN.GT..99D30) THEN
        QN=0.D0
        UN=0.D0
      ELSE
C%OS
        IF (-YPN .GT. .99D+30) THEN
          YPN = (Y(N)-Y(N-1))/(X(N)-X(N-1))
        ENDIF
C%OS
        QN=0.5D0
        UN=(3./(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      ENDIF
      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.)
      DO 12 K=N-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+U(K)
12    CONTINUE
      RETURN
      END SUBROUTINE
C-----------------------------------------------------------------------
      SUBROUTINE SPLINT(XA,YA,Y2A,N,X,Y,YP,YPP)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION XA(N),YA(N),Y2A(N)
      KLO=1
      KHI=N
1     IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2
        IF(XA(K).GT.X)THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
      GOTO 1
      ENDIF
      H=XA(KHI)-XA(KLO)
      IF (H.EQ.0.) STOP 'BAD XA INPUT.'
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+
     *      ((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.
      YP=(YA(KHI)-YA(KLO))/H -
     -  ( (3.*A*A-1.)*Y2A(KLO) - (3.*B*B-1.)*Y2A(KHI) )*H/6.
      YPP=A*Y2A(KLO)+B*Y2A(KHI)
      RETURN
      END SUBROUTINE
C-----------------------------------------------------------------------
      SUBROUTINE SPLINTEND(XA,YA,Y2A,N,X,Y,YP,YPP,KOPT)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION XA(N),YA(N),Y2A(N)
C     KOPT = 0: NOTHING SPECIAL IF X OUT OF BOUND
C     KOPT = 1: USE QUADRATIC INTERPOLATION IF X OUT OF BOUND
C     KOPT = 2: USE CUBIC INTERPOLATION IF X OUT OF BOUND
C-----------------------------------------------------------------------
C*COMDECK CUCDCD
C ----------------------------------------------------------------------
C --     STATEMENT FUNCTION FOR CUBIC INTERPOLATION                   --
C --                         19.01.87            AR        CRPP       --
C --                                                                  --
C -- CUBIC INTERPOLATION OF A FUNCTION F(X)                           --
C -- THE SIX ARGUMENTS X1,F1,P1,X2,F2,P2 ARE DEFINED AS FOLLOWS:      --
C -- F(X1) = F1 , F(X2) = F2 , DF/DX(X1) = P1 , DF/DX(X2) = P2        --
C ----------------------------------------------------------------------
C
         FC3(X1,F1,P1,X2,F2,P2) =
     =      (2.D0* (F2 - F1) / (X1 - X2) + (P1 + P2)) / 
     /      ((X1 - X2) * (X1 - X2))
         FC2(X1,F1,P1,X2,F2,P2) =
     =      (3.D0* (X1 + X2) * (F1 - F2) / (X1 - X2) - 
     *       P1 * (X1 + 2.D0* X2) - P2 * (X2 + 2.D0* X1)) /
     /      ((X1 - X2) * (X1 - X2))
         FC1(X1,F1,P1,X2,F2,P2) =
     =      (6.D0* X1 * X2 * (F2 - F1) / (X1 - X2) + 
     *       X2 * P1 * (2 * X1 + X2) + X1 * P2 * (X1 + 2.D0* X2)) /
     /      ((X1 - X2) * (X1 - X2))
         FC0(X1,F1,P1,X2,F2,P2) =
     =      (F1 * X2**2 + F2 * X1**2 - X1 * X2 * (X2 * P1 + X1 * P2) +
     +       2.D0* X1 * X2 * (F1 * X2 - F2 * X1) / (X1 - X2)) /
     /      ((X1 - X2) * (X1 - X2))
C ----------------------------------------------------------------------
C -- FCDCD0 GIVES THE VALUE OF THE FUNCTION AT POINT PX               --
C -- FCDCD0(......,PX) = F(PX)                                        --
C ----------------------------------------------------------------------
         FCDCD0(X1,F1,P1,X2,F2,P2,PX) =
     =              FC0(X1,F1,P1,X2,F2,P2) +
     +              PX * (FC1(X1,F1,P1,X2,F2,P2) +
     +                    PX * (FC2(X1,F1,P1,X2,F2,P2) +
     +                          PX * FC3(X1,F1,P1,X2,F2,P2)))
C ----------------------------------------------------------------------
C -- FCDCD1 GIVES THE VALUE OF THE FIRST DERIVATIVE OF F(X) AT PX:    --
C -- FCDCD1(......,PX) = DF/DX (PX)                                   --
C ----------------------------------------------------------------------
         FCDCD1(X1,F1,P1,X2,F2,P2,PX) =
     =              FC1(X1,F1,P1,X2,F2,P2) +
     +              PX * (2.D0* FC2(X1,F1,P1,X2,F2,P2) +
     +                    3.D0* PX * FC3(X1,F1,P1,X2,F2,P2))
C ----------------------------------------------------------------------
C -- FCDCD2 GIVES THE VALUE OF THE SECOND DERIVATIVE OF F(X) AT PX:   --
C -- FCDCD2(......,PX) = D2F/DX2 (PX)                                 --
C ----------------------------------------------------------------------
         FCDCD2(X1,F1,P1,X2,F2,P2,PX) =
     =             2.D0* FC2(X1,F1,P1,X2,F2,P2) +
     +             6.D0* FC3(X1,F1,P1,X2,F2,P2) * PX
C ----------------------------------------------------------------------
C -- FCDCD3 GIVES THE VALUE OF THE THIRD DERIVATIVE OF F(X) AT PX:    --
C -- FCDCD3(......,PX) = D3F/DX3 (PX)                                 --
C ----------------------------------------------------------------------
         FCDCD3(X1,F1,P1,X2,F2,P2,PX) =
     =                      6.D0* FC3(X1,F1,P1,X2,F2,P2)
C-----------------------------------------------------------------------
C*COMDECK QUAQDQ
C ----------------------------------------------------------------------
C --     STATEMENT FUNCTION FOR QUADRATIC INTERPOLATION               --
C --                         19.01.87            AR        CRPP       --
C --                                                                  --
C -- QUADRATIC INTERPOLATION OF A FUNCTION F(X)                       --
C -- THE FIVE PARAMETERS X1,F1,P1,X2,F2    ARE DEFINED AS FOLLOWS:    --
C -- F(X1) = F1 , DF/DX(X1) = P1 , F(X2) = F2                         --
C ----------------------------------------------------------------------
C
         FD2(X1,F1,P1,X2,F2) = ((F2-F1)/(X2-X1) - P1) / (X2-X1)
         FD1(X1,F1,P1,X2,F2) = P1 - 2.*X1*FD2(X1,F1,P1,X2,F2)
         FD0(X1,F1,P1,X2,F2) = F1 - X1*(X1*FD2(X1,F1,P1,X2,F2) +
     +                                     FD1(X1,F1,P1,X2,F2))
C ----------------------------------------------------------------------
C -- FQDQ0 GIVES THE VALUE OF THE FUNCTION AT POINT PX                --
C -- FQDQ0(......,PX) = F(PX)                                         --
C ----------------------------------------------------------------------
         FQDQ0(X1,F1,P1,X2,F2,PX) = FD0(X1,F1,P1,X2,F2) +
     F                              PX * (FD1(X1,F1,P1,X2,F2) +
     F                                    PX * FD2(X1,F1,P1,X2,F2))
C ----------------------------------------------------------------------
C -- FQDQ1 GIVES THE VALUE OF THE FIRST DERIVATIVE OF F(X) AT PX      --
C -- FQDQ1(......,PX) = DF/DX (PX)                                    --
C ----------------------------------------------------------------------
         FQDQ1(X1,F1,P1,X2,F2,PX) = FD1(X1,F1,P1,X2,F2) +
     F                              2.* PX * FD2(X1,F1,P1,X2,F2)
C ----------------------------------------------------------------------
C -- FQDQ2 GIVES THE VALUE OF THE SECOND DERIVATIVE OF F(X) AT PX     --
C -- FQDQ2(......,PX) = D2F/DX2 (PX)                                  --
C ----------------------------------------------------------------------
         FQDQ2(X1,F1,P1,X2,F2) = 2.D0* FD2(X1,F1,P1,X2,F2)
C-----------------------------------------------------------------------

      KLO=1
      KHI=N
1     IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2
        IF(XA(K).GT.X)THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
      GOTO 1
      ENDIF
      H=XA(KHI)-XA(KLO)
      IF (H.EQ.0.) STOP 'BAD XA INPUT.'
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+
     *      ((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.
      YP=(YA(KHI)-YA(KLO))/H -
     -  ( (3.*A*A-1.)*Y2A(KLO) - (3.*B*B-1.)*Y2A(KHI) )*H/6.
      YPP=A*Y2A(KLO)+B*Y2A(KHI)

      IF (KOPT .EQ. 0) RETURN
C
C     ZEPSILON: DISTANCE OUTSIDE INTERVAL RELATIVE TO H
C
      ZEPSILON = 1.0D-05
c
c     x outside interval: use quadratic with y and yp at edge and y-1
c
      IF (KOPT .EQ. 1) THEN
C     LEFT END
        if (B.lt. -ZEPSILON) then
          print *,' warning points outside interval at left.',
     +      ' Use quadratic interpolation'
          A=1.D0
          B=0.D0
          YPAKLO=(YA(KHI)-YA(KLO))/H -
     -    ( (3.D0*A*A-1.D0)*Y2A(KLO) - (3.D0*B*B-1.D0)*Y2A(KHI) )*H/6.D0
          A=0.D0
          B=1.D0
          YPAKHI=(YA(KHI)-YA(KLO))/H -
     -    ( (3.D0*A*A-1.D0)*Y2A(KLO) - (3.D0*B*B-1.D0)*Y2A(KHI) )*H/6.D0
          Y=FQDQ0(XA(KLO),YA(KLO),YPAKLO,XA(KHI),YA(KHI),X)
          YP=FQDQ1(XA(KLO),YA(KLO),YPAKLO,XA(KHI),YA(KHI),X)
          YPP=FQDQ2(XA(KLO),YA(KLO),YPAKLO,XA(KHI),YA(KHI))
        endif
C     RIGHT END
        if (A .lt. -ZEPSILON) then
          print *,' warning points outside interval at right.',
     +      ' Use quadratic interpolation'
          A=1.D0
          B=0.D0
          YPAKLO=(YA(KHI)-YA(KLO))/H -
     -    ( (3.D0*A*A-1.D0)*Y2A(KLO) - (3.D0*B*B-1.D0)*Y2A(KHI) )*H/6.D0
          A=0.D0
          B=1.D0
          YPAKHI=(YA(KHI)-YA(KLO))/H -
     -    ( (3.D0*A*A-1.D0)*Y2A(KLO) - (3.D0*B*B-1.D0)*Y2A(KHI) )*H/6.D0
          Y=FQDQ0(XA(KHI),YA(KHI),YPAKHI,XA(KLO),YA(KLO),X)
          YP=FQDQ1(XA(KHI),YA(KHI),YPAKHI,XA(KLO),YA(KLO),X)
          YPP=FQDQ2(XA(KHI),YA(KHI),YPAKHI,XA(KLO),YA(KLO))
        endif

        RETURN
      ENDIF
c
c     x outside interval: use CUBIC with y and yp
c
      IF (KOPT .EQ. 2) THEN
C     LEFT OR RIGHT END
        if (a.lt.-ZEPSILON .OR. B.LT.-ZEPSILON) then
          print *,' warning points outside interval.',
     +      ' Use cubic interpolation'
          A=1.D0
          B=0.D0
          YPAKLO=(YA(KHI)-YA(KLO))/H -
     -    ( (3.D0*A*A-1.D0)*Y2A(KLO) - (3.D0*B*B-1.D0)*Y2A(KHI) )*H/6.D0
          A=0.D0
          B=1.D0
          YPAKHI=(YA(KHI)-YA(KLO))/H -
     -    ( (3.D0*A*A-1.D0)*Y2A(KLO) - (3.D0*B*B-1.D0)*Y2A(KHI) )*H/6.D0
          Y=FCDCD0(XA(KLO),YA(KLO),YPAKLO,XA(KHI),YA(KHI),YPAKHI,X)
          YP=FCDCD1(XA(KLO),YA(KLO),YPAKLO,XA(KHI),YA(KHI),YPAKHI,X)
          YPP=FCDCD2(XA(KLO),YA(KLO),YPAKLO,XA(KHI),YA(KHI),YPAKHI,X)
        endif
        RETURN
      ENDIF

      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE NONSYM(A,X,C,N,MLEFT,MRIGHT,EPS,NCOND)
C     *************************************************
C
C     5.3  SOLVES A REAL VALUED NONSYMMETRIC LINEAR SYSTEM  A . X  =  C
C
C     VERSION 1               APRIL 1988       KA        LAUSANNE
C     VERSION 2               MAI 1992         AJ        LAUSANNE
C
C-----------------------------------------------------------------------

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION   A(N*(MLEFT+MRIGHT+1)),    X(N),    C(N)
CMPLX COMPLEX     A,       X,       C,        ZPIVOT,   ZSUM,     ZTOP
      REAL*8        A,       X,       C,        ZPIVOT,   ZSUM,     ZTOP
      DATA        IMESS  / 0 /
C-----------------------------------------------------------------------
C
C     A IS A BANDMATRIX OF RANK N WITH MLEFT/MRIGHT OFF-DIAGONAL
C     ELEMENTS TO THE LEFT/RIGHT.
C     :A: AND :C: ARE DESTROYED BY :NONCYM: 
C     AT THE RETURN :C: CONTAINS THE SOLUTION VECTOR, :X: THE SQUARE
C     ROOT OF THE DIAGONAL ELEMENTS
C
C     THE ELEMENTS ARE HORIZONTALLY NUMBERED ROW AFTER ROW
C
C     FOR EASY NUMBERING, ZERO ELEMENTS HAVE BEEN INTRODUCED IN THE
C     UPPER LEFT-HAND CORNER AND IN THE LOWER RIGHT-HAND CORNER.
C     THESE ELEMENTS
C     M U S T  B E  S E T  T O  Z E R O
C     BEFORE CALLING :NONCYM: 
C
C     EXAMPLE FOR NUMBERING   MLEFT=2,  MRIGHT=3
C     -------
C
C     A(1)   A(2) I A(3)   A(4)   A(5)   A(6)
C     A(7) I A(8)   A(9)   A(10)  A(11)  A(12)
C     I                 .
C     I                        .
C
C     A PIVOT IS CONSIDERED TO BE BAD IF IT IS BY A FACTOR :EPS: SMALLER
C     THAN ITS OFF-DIAGONAL ELEMENTS. IF A BAD PIVOT IS ENCOUNTERED
C     A MESSAGE IS ISSUED (AT MOST TEN TIMES IN A RUN).
C     THE FLAG :NCOND: IS SET TO -1, OTHERWISE 0. 
C
C-----------------------------------------------------------------------
CL    0.        INITIALIZATION
C
      MOFFDI=MLEFT+MRIGHT
      MBAND=MOFFDI+1
      RATIO=0.D0
      NCOND=0
C     PRELIMINARY CHECK OF PIVOTS
      IPIV1=MLEFT+1
      IPIV2=IPIV1+(N-1)*MBAND
      DO 10 J=IPIV1,IPIV2,MBAND
CMPLX IF(CABS(A(J)) .EQ. 0.D0) GO TO 510
        IF( ABS(A(J)) .EQ. 0.D0) GO TO 510
 10   CONTINUE
C
C-----------------------------------------------------------------------
CL    1.        PRECONDITIONNING: GET ONES ON THE DIAGONAL
C
      DO 130 JP=1,N
        JPABS=(MLEFT+1)+(JP-1)*MBAND
CMPLX X(JP)=SQRT(CABS(A(JPABS)))
        X(JP)=SQRT(ABS(A(JPABS)))
C
C     DIVIDE THE EQUATIONS BY THE SQUARE ROOT OF THE PIVOT
        DO 110 J=-MLEFT,MRIGHT
          A(JPABS+J)=A(JPABS+J)/X(JP)
 110    CONTINUE
        C(JP)=C(JP)/X(JP)
C
C     CHANGE VARIABLES, DIVIDE COLUMN BY SQUARE ROOT OF THE PIVOT
        DO 120 JA=JPABS-MRIGHT*MOFFDI,JPABS+MLEFT*MOFFDI,MOFFDI
          IF (JA.GE.IPIV1 .AND. JA.LE.IPIV2) A(JA)=A(JA)/X(JP)
 120    CONTINUE
 130  CONTINUE
C
C-----------------------------------------------------------------------
CL    2.        CONSTRUCT UPPER TRIANGULAR MATRIX 
C
C     INITIALIZATION OF VERTICAL COUNTER
      IDOWN=MLEFT
C     PIVOT INDEX FOR FIRST INCOMPLETE COLUMN
      INCMPL=(N-MLEFT)*MBAND+MLEFT+1 
C     FIRST PIVOT AND LAST BUT ONE
      IPIV1=MLEFT+1
      IPIV2=IPIV1+(N-2)*MBAND
C
      DO 220 JPIVOT=IPIV1,IPIV2,MBAND
C     COLUMN LENGTH 
        IF(JPIVOT.GE.INCMPL) IDOWN=IDOWN-1
        ZPIVOT=A(JPIVOT)
C     FIRST AND LAST HORIZONTAL ELEMENT INDEX
        IHOR1=JPIVOT+1
        IHOR2=JPIVOT+MRIGHT
C     CHECK THE PIVOT
        ZMAX=0.D0
        DO 205 JHORIZ=IHOR1,IHOR2
CMPLX ZMAX=AMAX1(ZMAX,CABS(A(JHORIZ)))
          ZMAX=AMAX1(ZMAX, ABS(A(JHORIZ)))
 205    CONTINUE
CMPLX IF(CABS(ZPIVOT).EQ.0.D0) GO TO 510
        IF( ABS(ZPIVOT).EQ.0.D0) GO TO 510
CMPLX RATIO=AMAX1(RATIO,ZMAX/CABS(ZPIVOT))
        RATIO=AMAX1(RATIO,ZMAX/ ABS(ZPIVOT))
C
        DO 210 JHORIZ=IHOR1,IHOR2
          ZTOP=-A(JHORIZ)/ZPIVOT
          A(JHORIZ)=ZTOP
C     INITIALIZATION OF ELEMENT INDEX
          IELEM=JHORIZ
C     INITIALIZATION OF RECTANGULAR RULE CORNER ELEMENT
          ICORN=JPIVOT
C     LOOP DOWN THE COLUMN
          DO 211 JDOWN=1,IDOWN 
            IELEM=IELEM+MOFFDI
            ICORN=ICORN+MOFFDI
            A(IELEM)=A(IELEM)+ZTOP*A(ICORN)
 211      CONTINUE
 210    CONTINUE
C     TREAT THE CONSTANTS
C     INDEX OF THE FIRST ONE
        ICONST=JPIVOT/MBAND+1
        ZTOP=-C(ICONST)/ZPIVOT
        C(ICONST)=ZTOP
C     INITIALIZATION OF RECTANGULAR RULE CORNER ELEMENT
        ICORN=JPIVOT
C     LOOP DOWN THE CONSTANTS 
        DO 221 JDOWN=1,IDOWN 
          ICONST=ICONST+1
          ICORN=ICORN+MOFFDI
          C(ICONST)=C(ICONST)+ZTOP*A(ICORN)
 221    CONTINUE
 220  CONTINUE
C
C-----------------------------------------------------------------------
CL    3.        BACKSUBSTITUTION
C
 300  CONTINUE
C     INITIALIZATION OF SOLUTION INDEX
      ISOLUT=N
C     LAST PIVOT
      JPIVOT=IPIV2+MBAND
C     CHECK LAST PIVOT
CMPLX IF(CABS(A(JPIVOT)).EQ.0.D0) RATIO=1.D100
c%OS  IF( ABS(A(JPIVOT)).EQ.0.D0) RATIO=1.D100
      IF( ABS(A(JPIVOT)).EQ.0.D0) go to 510
C     LAST UNKNOWN
      C(ISOLUT)=C(ISOLUT)/A(JPIVOT)
C     INITIALIZATION OF HORIZONTAL RANGE
      IHOR1=JPIVOT+1
      IHOR2=JPIVOT+MRIGHT
C
      INM1=N-1
      DO 320 J=1,INM1
        ISOLUT=ISOLUT-1
        IHOR1=IHOR1-MBAND
        IHOR2=IHOR2-MBAND
        ZSUM=-C(ISOLUT)
        IKNOWN=ISOLUT
        DO 310 JHORIZ=IHOR1,IHOR2
          IKNOWN=IKNOWN+1
          IF(IKNOWN.GT.N) GO TO 310
          ZSUM=ZSUM+A(JHORIZ)*C(IKNOWN)
 310    CONTINUE
        C(ISOLUT)=ZSUM
 320  CONTINUE
C
C-----------------------------------------------------------------------
CL    4.        PRECONDITIONNING: BACK TO ORIGINAL VARIABLES
C
      DO 410 J=1,N
        C(J)=C(J)/X(J)
 410  CONTINUE
C
C-----------------------------------------------------------------------
CL    5.        HAVE WE ENCOUNTERED A BAD PIVOT ? 
C
C%OS  IF(1./RATIO .GT. EPS) RETURN
      IF(RATIO .LT. 1.D0/EPS) RETURN
      NCOND=-1
C     COUNT THE NUMBER OF MESSAGES ISSUED
      IMESS=IMESS+1
      IF(IMESS.LE.10) PRINT 9500, RATIO
      RETURN
C
C     ZERO PIVOT ENCOUNTERED
 510  CONTINUE
      PRINT 9510 
      STOP 'ZERO PIVOT IN NONCYM'
C
C-----------------------------------------------------------------------
CL    9.        FORMATS
C

 9500 FORMAT(/////20X,10(1H*),21H  MESSAGE FROM NONCYM,2X,10(1H*)///
     +  20X,'BAD PIVOT ENCOUNTERED, RATIO: ',1PE12.3///
     +  20X,43(1H*)///)
 9510 FORMAT(/////20X,10(1H*),'   ZERO PIVOT IN NONCYM'///)
C
      END SUBROUTINE

      end module spider_spline
