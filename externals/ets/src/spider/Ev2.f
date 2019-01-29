C************************************************************
C
C       NVAR=1 - SQUARED FORM OF SURFACES IS USED:
C       A1*X**2+A2*X*Y+A3*Y**2+A4*X+A5*Y+A6=0
C
C       NVAR=2 - BILINEAR FORM OF SURFACES IS USED
C
C       NVAR = 11 - TEST FOR NVAR=1
C--------------------------------------------------------
        SUBROUTINE FUN(X,F)
C
        IMPLICIT REAL*8(A-H,O-Z)
C
        COMMON/CPCOOR/ NVAR,IFAILF,RCP,ZCP,UU(5),DF
        COMMON/YSCR/ Y
C
        SQRT(XXX) = DSQRT(XXX)
C
        IF( NVAR .EQ. 1 ) THEN
C
            A1 = 0.5D0*UU(3)
            A2 =       UU(4)
            A3 = 0.5D0*UU(5)
            A4 =       UU(1)
            A5 =       UU(2)
            A6 = DF
C
            ALF = 1.D0
            IFAILF = 0
C
            B = (A2*X+A5) / (2.D0*A3)
            DET = B**2 - (A1*X**2+A4*X+A6)/A3
            IF( DET .LT. 0 ) THEN
C                WRITE(6,*) ' PROGRAM TERMINATED: DET= ',DET
C                STOP
                 DET = - DET
                 ALF = 1.D+3
                 IFAILF = 1
            ENDIF
            DET = SQRT( DET ) * ALF
            Y  =  -B + DET
            Y2 =  -B - DET
C
            F  = (X-RCP)**2 + (Y -ZCP)**2
            F2 = (X-RCP)**2 + (Y2-ZCP)**2
C
            IF (F2 .LT. F ) THEN
              F = F2
              Y = Y2
            ENDIF
C
        ENDIF
C
C
        IF( NVAR .EQ. 11 ) THEN
C..TEST
          A1 = 1.D0
          A2 = 0.D0
          A3 = 1.D0
          A4 = 0.D0
          A5 = 0.D0
          A6 = -1.D0
C......
            B = (A2*X+A5) / (2.D0*A3)
            DET = SQRT( B**2 - (A1*X**2+A4*X+A6)/A3 )
            Y  =  -B + DET
            Y2 =  -B - DET
C
            F  = (X-RCP)**2 + (Y -ZCP)**2
            F2 = (X-RCP)**2 + (Y2-ZCP)**2
C
            IF (F2 .LT. F )  F = F2
C
        ENDIF
C
        RETURN
        END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C***************************************************************
C Definition of (IC,JC)-cell, wich contain given (RC,ZC)-point
C---------------------------------------------------------------
C
          SUBROUTINE IJMESH( NR,R,NZ,Z, RC,ZC, IC,JC)
C
         IMPLICIT REAL*8(A-H,O-Z)
C
         DIMENSION R(1),Z(1)
C
          NOUT  = 17
          NTER  = 6
C
         IF( (RC.LT.R(1)) .OR. (R(NR).LT.RC) ) THEN
           ! WRITE(NTER,*) 'PROGRAM WAS INTERRUPTED IN SUBR. "IJMESH" '
           ! WRITE(NTER,*) 'POINT "RC" IS OUT OF "BOX" : RC =', RC
            !WRITE(NOUT,*) 'PROGRAM WAS INTERRUPTED IN SUBR. "IJMESH" '
            !WRITE(NOUT,*) 'POINT "RC" IS OUT OF "BOX" : RC =', RC
            STOP
         END IF
         IF( (ZC.LT.Z(1)) .OR. (Z(NZ).LT.ZC) ) THEN
           ! WRITE(NTER,*) 'PROGRAM WAS INTERRUPTED IN SUBR. "IJMESH" '
           ! WRITE(NTER,*) 'POINT "ZC" IS OUT OF "BOX" : ZC =', ZC
            !WRITE(NOUT,*) 'PROGRAM WAS INTERRUPTED IN SUBR. "IJMESH" '
            !WRITE(NOUT,*) 'POINT "ZC" IS OUT OF "BOX" : ZC =', ZC
            STOP
         END IF
C
         DO 1 I=1,NR
            IF( R(I) .GT. RC ) THEN
                IC = I - 1
                GO TO 2
            ENDIF
 1       CONTINUE
 2       CONTINUE
C
         DO 3 J=1,NZ
            IF( Z(J) .GT. ZC ) THEN
                JC = J - 1
                GO TO 4
            ENDIF
 3       CONTINUE
 4       CONTINUE
         RETURN
         END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      SUM OF ANGLES FOR POINT (UC,VC) AND
C          CLOSED  CURVE U,V(N)
C      ASSUMING    U,V(N) .NE. U,V(1) !!
C----------------------------------------------------------
C
         REAL*8 FUNCTION SUMANG (N,U,V,UC,VC)
C
         INCLUDE'double.inc'
C
         DIMENSION U(1:N), V(1:N)
C
         SUMANG=0.
         REG=1.D-10
C
           DO 1 I =1,N
           DU1=U(I)-UC
           DV1=V(I)-VC
         IF(I.NE.N) THEN
           DU2=U(I+1)-UC
           DV2=V(I+1)-VC
         ELSE
           DU2=U(1)-UC
           DV2=V(1)-VC
         ENDIF
         ANG=DACOS((DU1*DU2+DV1*DV2)/(DSQRT((DU1*DU1+DV1*DV1)*
     &                                (DU2*DU2+DV2*DV2))+REG))
         IF(DU1*DV2-DU2*DV1.GT.0.) THEN
          SUMANG=SUMANG+ANG
         ELSE
          SUMANG=SUMANG-ANG
         ENDIF
 1        CONTINUE
C
         RETURN
         END
C**********************************************************
C     MIN. DISTANCE FOR POINT (UC,VC) AND
C              CURVE U,V(N)
C----------------------------------------------------------
         REAL*8 FUNCTION DISTAN(N,U,V,UC,VC)
C
         INCLUDE'double.inc'
C
         DIMENSION U(1:N), V(1:N)
C
         SQRT(XXX) = DSQRT(XXX)
C
         DISTAN = 100.D0
C
         DO 1 I =1,N
            TTT = (U(I)-UC)**2 + (V(I)-VC)**2
            TTT = SQRT(TTT)
            IF(TTT.LT.DISTAN) DISTAN = TTT
 1       CONTINUE
C
         RETURN
         END
C
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C   POLOIDAL mag. field companents Bp_r, Bp_z  and
C   POLOIDAL mag. field projection on direction FIPRO
C   COMPUTE FOR GIVEN "PF_PROBE" POINTS:
C-----------------------------------------------------------------------
C
        SUBROUTINE BP_PROB( NOUT,   NTER,   KEYPRI,
     *                      NPRO,   RPRO,   ZPRO,   FIPRO,
     *                      BRPRO,  BZPRO,  BPCOM )
C
        INCLUDE 'double.inc'
C
        DIMENSION RPRO(*),  ZPRO(*),  FIPRO(*),
     *            BRPRO(*), BZPRO(*), BPCOM(*)
C-----------------------------------------------------------------------
C
      IF( NPRO.NE.0 )  THEN
         do 1455 i=1,npro
            call probe( rpro(i), zpro(i), fipro(i),
     *                  brpr, bzpr, bpnum )
            brpro(i) = brpr
            bzpro(i) = bzpr
            bpcom(i) = bpnum
 1455    continue
C
C        WRITE(NOUT, * )  '   '
C        WRITE(NOUT, * ) '--------------------------------------------'
C        WRITE(NOUT, * ) 'Poloidal mag. field probe projection        '
C        WRITE(NOUT, * ) '  BPCOM(i), i=1,...,NPRO = ', NPRO
C        WRITE(NOUT, * ) '--------------------------------------------'
C        WRITE(NOUT,101) (BPCOM(I), I=1,NPRO)
C        WRITE(NOUT, * ) '   '
C
      END IF
C	 	  
  101  FORMAT(2X,5E14.7)
C
         RETURN
         END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C  Compute POLOIDAL mag. field companents Bp_r, Bp_z
C  and POLOIDAL mag. field projection on direction FPR
C-----------------------------------------------------------------------
C
        SUBROUTINE PROBE( RPR, ZPR, FPR, BRPR, BZPR, BPNUM )
C
        INCLUDE 'double.inc'
        INCLUDE 'param.inc'
        INCLUDE 'comblc.inc'
C
        SIN(XXXX) = DSIN(XXXX)
        COS(XXXX) = DCOS(XXXX)
C-----------------------------------------------------------------------
C
          CALL IJMESH( NI,R,NJ,Z, RPR,ZPR, IC,JC )
C
          DRDZ  = ( R(IC+1) - R(IC) )*( Z(JC+1) - Z(JC) )
C
          DPSDR = ( (U(IC+1,JC  ) - U(IC,JC  )) * (Z(JC+1) - ZPR  ) +
     *              (U(IC+1,JC+1) - U(IC,JC+1)) * (ZPR     - Z(JC))  )
     *          / DRDZ
          DPSDZ = ( (U(IC  ,JC+1) - U(IC  ,JC)) * (R(IC+1) - RPR  ) +
     *              (U(IC+1,JC+1) - U(IC+1,JC)) * (RPR     - R(IC))  )
     *          / DRDZ
          BRPR  = - DPSDZ / RPR
          BZPR  =   DPSDR / RPR
C
          BPNUM = BRPR*COS(FPR) + BZPR*SIN(FPR)
C
          BBB   = 1.D0/(2.D0*pi)
C
          BRPR  = BRPR  
          BZPR  = BZPR  
          BPNUM = BPNUM 
C
        RETURN
        END
C
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C    Compute full poloidal flux in given "loop" points
C-----------------------------------------------------------------------
C
        SUBROUTINE FL_LOO( NOUT,   NTER,   KEYPRI,
     *                     NLOO,   RLOO,   ZLOO,
     *                     PSLOO )
C
        INCLUDE 'double.inc'
C
        DIMENSION RLOO(1), ZLOO(1), PSLOO(1)
C-----------------------------------------------------------------------
C
      IF( NLOO.NE.0 )  THEN
         do i=1,nloo
C
            call floop( rloo(i), zloo(i), psnum )
C
            if( i.eq.1 ) then
		    psloo(i) = psnum
            else
              psloo(i) = psnum - psloo(1)
            end if
C
         end do
C
C        WRITE(NOUT, * ) '   '
C        WRITE(NOUT, * ) '--------------------------------------------'
C        WRITE(NOUT, * ) 'Poloidal mag. flux loop values              '
C        WRITE(NOUT, * ) '  PSLOO(i), i=1,...,NLOO = ', NLOO
C        WRITE(NOUT, * ) '--------------------------------------------'
C        WRITE(NOUT,101) (psloo(i), i=1,nloo)
C        WRITE(NOUT, * ) '   '
C
      END IF
C	 	  
  101  FORMAT(2X,5E14.7)
C
         RETURN
         END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C    Compute full poloidal flux in given "loop" point
C-----------------------------------------------------------------------
        SUBROUTINE FLOOP( RPR, ZPR, PSPR )
C
        INCLUDE 'double.inc'
        INCLUDE 'param.inc'
        INCLUDE 'comblc.inc'
C
C-----------------------------------------------------------------------
C
          CALL IJMESH( NI,R,NJ,Z, RPR,ZPR, IC,JC )
C
          DRDZ  = ( R(IC+1) - R(IC) )*( Z(JC+1) - Z(JC) )
C
          PSPR  = ( U(IC  ,JC  ) * (R(IC+1) - RPR)*(Z(JC+1) - ZPR) +
     *              U(IC+1,JC  ) * ( RPR - R(IC) )*(Z(JC+1) - ZPR) +
     *              U(IC  ,JC+1) * (R(IC+1) - RPR)*( ZPR - Z(JC) ) +
     *              U(IC+1,JC+1) * ( RPR - R(IC) )*( ZPR - Z(JC) )   )
     *          / DRDZ
C
          BBB   = 1.D0/(2.D0*pi)
C
          PSPR   = PSPR / BBB
C
        RETURN
        END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!







