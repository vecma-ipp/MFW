C***************************************************************
C  SUBROUTINE "EVSLV" SOLVES THE MATRIX CIRCUIT EQUATION
C***************************************************************
C  INPUT DATE:
C  **********
C  NLES, NREG, TSTEP0, TSTEP, SIGM, NJ,
C  VOLK(NJ), VOLKP1(NJ), RES(NJ),
C  PSPK(NJ), PSPKP1(NJ), CRPK(NJ), CRPKP1(NJ), CRPKP(NJ),
C  NOUT, NTER, KEYPRI,
C
C  PPIND(NJ,NJ)  - from 'comevl.inc'
C  DPSIDJ(NJ,NJ) - from 'comevl.inc' only for NREG=1
C
C
C  OUTPUT DATE:
C  ***********
C  CRPKP1(NJ) - solution array - currents
C  EREVE      - "nevyazka" of circuit equation solution
C               ( without DPSIDJ*(CRPKP1 - CRPKP) term )
C***************************************************************
C
        SUBROUTINE EVSLV( NLES, NREG,   TSTEP0, TSTEP,  SIGM,
     *                    NJ,   VOLK,   VOLKP1, RES,
     *                    PSPK, PSPKP1, CRPK,   CRPKP1, CRPKP,
     *                    NOUT, NTER,   KEYPRI, EREVE )

        IMPLICIT REAL*8( A-H, O-Z )

        INCLUDE 'prm.inc'
        INCLUDE 'comevl.inc'

        DIMENSION   BZ(NJLIM)

        !DIMENSION   RES(*),  VOLK(*),   VOLKP1(*)
        !DIMENSION   PSPK(*), PSPKP1(*)
        !DIMENSION   CRPK(*), CRPKP1(*), CRPKP(*)

        DIMENSION   RES(NJLIM),  VOLK(NJLIM),   VOLKP1(NJLIM)
        DIMENSION   PSPK(NJLIM), PSPKP1(NJLIM)
        DIMENSION   CRPK(NJLIM), CRPKP1(NJLIM), CRPKP(NJLIM)

        REAL*8      A(NNLIM),   RSP(NSP),  B(NJLIM)
        INTEGER     PP(NJLIM),  P(NJLIM),  IP(NJLIM),
     *              IA(NJLIM1), JA(NNLIM), ISP(NSP)

        COMMON /SPA_MAT/ A, IA, JA
        COMMON /SPArsp/ RSP, PP, P, IP

        EQUIVALENCE (RSP(1),ISP(1))

C***************************************************************

        ABS(XXX)  = DABS(XXX)
        nflag=0
        NN = NJ * NJ
C---------------------------------------------------------------

       IF( NLES.EQ.0 ) THEN
          NPATH = 1

          DO I=1,NJ
             PP(I)  = I
              P(I)  = I
             IP(I)  = I
          enddo

          IA(1)    = 1
          DO I=2,NJ
            IA(I) = IA(I-1) + NJ
          enddo
          IA(NJ+1) = NN+1

          DO 3 I=1,NJ
             IA1 = IA(I)
             IA2 = IA(I+1) - 1
             DO 4 J=IA1,IA2
                JAJ   = J - IA1 + 1
                JA(J) = JAJ
                A(J)  = PPIND(I,JAJ)
                IF( NREG.EQ.  1 ) A(J) = A(J) + DPSIDJ(I,JAJ)
                IF( I   .EQ.JAJ ) A(J) = A(J) + TSTEP*SIGM*RES(I)
    4        CONTINUE
C
    3     CONTINUE
       END IF
C--------------------------------------------------------------------
C
       IF( NLES.EQ.1 ) THEN
          NPATH = 1
          DO 10 I=1,NJ
             IA1 = IA(I)
             IA2 = IA(I+1) - 1
             DO 11 J=IA1,IA2
                JAJ   = JA(J)
                A(J)  = PPIND(I,JAJ)
                IF( NREG.EQ.  1 ) A(J) = A(J) + DPSIDJ(I,JAJ)
                IF( I   .EQ.JAJ ) A(J) = A(J) + TSTEP*SIGM*RES(I)
   11        CONTINUE
   10     CONTINUE
       END IF
C--------------------------------------------------------------------
C
       IF( NLES.EQ.2 ) THEN
          NPATH = 3
          !NPATH = 1
       END IF
C--------------------------------------------------------------------
C
      IF((NLES.NE.0).AND.(NLES.NE.1).AND.(NLES.NE.2)) THEN
          WRITE(*,*) 'PARAMETER NLES = ', NLES
          WRITE(*,*) 'IT IS WRONG. PROGRAM INTERRUPT'
          STOP
      END IF
C******************************************************************

         DO 6 I=1,NJ

           RS1 = 0.d0
           RS2 = 0.d0
           DO 7 J=1,NJ
                              RS1 = RS1 + CRPK(J)  * PPIND(I,J)
              IF( NREG.EQ.1 ) RS2 = RS2 + CRPKP(J) * DPSIDJ(I,J)
    7      CONTINUE

           B(I)  = - TSTEP*RES(I)*(1.D0-SIGM)*CRPK(I)
     *             + TSTEP*(SIGM*VOLKP1(I)+(1.D0-SIGM)*VOLK(I))
     *             + RS1
     *             - (PSPKP1(I) - PSPK(I)) * TSTEP/TSTEP0

           IF( NREG.EQ.1 ) B(I) = B(I) + RS2

           BZ(I) = B(I)

    6   CONTINUE

C******************************************************************


          call sdrvd(nj,p,ip,ia,ja,a,b,CRPKP1,nsp,
     *              isp,rsp,nesp,npath,nflag)



!      CALL CDRVD( NJ,  PP,P,IP,  IA,JA,A,  B,CRPKP1,
 !    *            NSP,ISP,RSP,  NESP,NPATH,NFLAG )
C
C.      WRITE(NOUT,*) '****************************************** '
C.      WRITE(NOUT,*) 'FROM SUBROUTINE "EVSLV" AFTER "CALL CDRVD" '
C.      WRITE(NOUT,*) 'NSP  =',NSP,' ESP  =',NESP,' FLAF =',NFLAG
C.      WRITE(NOUT,*) 'PATH =',NPATH,' NLES =',NLES
C.      WRITE(NOUT,*) 'NREG =',NREG
C
C       CALL TCONTR( 0, A,JA,IA,NJ,CRPKP1,B, 0,0, BZ, ERR)
C
C       IF( KEYPRI.EQ.1 )  THEN
C       WRITE(NOUT,*) 'NEVYAZKA OF LINEAR  SYSTEM:  ERR   =', ERR
C       WRITE(NTER,*) 'NEVYAZKA OF LINEAR  SYSTEM:  ERR   =', ERR
C       END IF
C
          EREVE = 0.0D0
          DO 66 I=1,NJ
C
             RS1 = 0.d0
             DO 777 J=1,NJ
                RS1 = RS1 + (CRPKP1(J) - CRPK(J)) * PPIND(I,J)
  777        CONTINUE
C
             BZZ   = -   RES(I)*TSTEP*(1.D0 - SIGM)* CRPK(I)
     *               -   RES(I)*TSTEP*        SIGM * CRPKP1(I)
     *               +          TSTEP*(1.D0 - SIGM)* VOLK(I)
     *               +          TSTEP*        SIGM * VOLKP1(I)
     *               - RS1
     *               - (PSPKP1(I) - PSPK(I)) * TSTEP / TSTEP0
C
             TTT   = ABS(BZZ)
             IF( TTT.GT.EREVE )   EREVE = TTT
   66     CONTINUE
C
C       IF( KEYPRI.EQ.1 )  THEN
C       WRITE(NOUT,*) 'NEVYAZKA OF EVOL. EQUATION:  EREVE =', EREVE
C       WRITE(NOUT,*) '****************************************** '
C       END IF
C       WRITE(NTER,*) 'NEVYAZKA OF EVOL. EQUATION:  EREVE =', EREVE
C       WRITE(NTER,*) '****************************************** '
C
      IF( NESP.LT.0 ) THEN
          WRITE(*,*) 'ATTENTION ESP(SDRVD) = ', NESP
          WRITE(*,*) 'IT IS WRONG. PROGRAM INTERRUPT'
          !WRITE(NOUT,*) 'PARAMETER PATH(SDRVD) = ', NPATH
          !WRITE(NTER,*) 'ATTENTION ESP(SDRVD) = ', NESP
          !WRITE(NTER,*) 'IT IS WRONG. PROGRAM INTERRUPT'
          !WRITE(NTER,*) 'PARAMETER PATH(SDRVD) = ', NPATH
          STOP
      END IF
      IF( NFLAG.NE.0 ) THEN
          WRITE(*,*) 'ATTENTION FLAG(SDRVD) = ', NFLAG
          WRITE(*,*) 'IT IS WRONG. PROGRAM INTERRUPT'
          !WRITE(NOUT,*) 'PARAMETER PATH(SDRVD) = ', NPATH
          !WRITE(NTER,*) 'ATTENTION FLAG(SDRVD) = ', NFLAG
          !WRITE(NTER,*) 'IT IS WRONG. PROGRAM INTERRUPT'
          !WRITE(NTER,*) 'PARAMETER PATH(SDRVD) = ', NPATH
          STOP
      END IF
C
      RETURN
      END
C***************************************************************
