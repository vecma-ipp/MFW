C***************************************************************
C
          SUBROUTINE  DIFFER( AOLD, ANEW, N, ERRLOC, ERRVEC,
     *                        ABSMAX, ABSMIN, NOUT, NTER )
C
       include 'double.inc'
C
          DIMENSION AOLD(*), ANEW(*)
C
          ABS(X) = DABS(X)
          SQRT(X) = DSQRT(X)
C
          ERRLOC = 0.D0
          ABSMAX = ABS( ANEW(1) )
          ABSMIN = ABS( ANEW(1) )
C
          DO 1 I=1,N
	     ABSA = ABS( ANEW(I) )
	     IF( ABSA.LT.ABSMIN )  ABSMIN = ABSA
             IF( ABSA.GT.ABSMAX )  ABSMAX = ABSA
             ABSD = ABS( ANEW(I) - AOLD(I) )
             IF( ABSA.GT.1.0D-12 )  THEN
                 ABSR = ABSD / ABSA
                 IF( ABSR.GT.ERRLOC )  ERRLOC = ABSR
	     END IF
    1     CONTINUE
C
          S1 = 0.D0
          S2 = 0.D0
          DO 2 L=1,N
             S1 = S1 + ( ANEW(L) - AOLD(L) )**2
             S2 = S2 +   ANEW(L)**2
    2     CONTINUE
          S1 = SQRT( S1 )
          S2 = SQRT( S2 )
C
          IF( S2 .GT. 1.0D-12 ) THEN
              ERRVEC = S1 / S2
          ELSE
	      ERRVEC = 0.D0
          END IF
C
          RETURN
          END
C***************************************************************
C
          SUBROUTINE  DIFTIM( AK, AKP1S, AKP1N, N, ERRLOC, ERRVEC,
     *                        ABSMAX, ABSMIN, NOUT, NTER )
C
       include 'double.inc'
C
          DIMENSION AK(*), AKP1S(*), AKP1N(*)
C
          ABS(X) = DABS(X)
          SQRT(X) = DSQRT(X)
C
          ERRLOC = 0.D0
          ABSMAX = ABS( AKP1N(1) - AK(1) )
          ABSMIN = ABS( AKP1N(1) - AK(1) )
C
          DO 1 I=1,N
             ABSA = ABS( AKP1N(I) - AK(I) )
             IF( ABSA.LT.ABSMIN )  ABSMIN = ABSA
             IF( ABSA.GT.ABSMAX )  ABSMAX = ABSA
	     ABSD = ABS( AKP1N(I) - AKP1S(I) )
             IF( ABSA.GT.1.0D-12 )  THEN
                 ABSR = ABSD / ABSA
                 IF( ABSR.GT.ERRLOC ) ERRLOC = ABSR
	     END IF
    1     CONTINUE
C
          S1 = 0.D0
          S2 = 0.D0
          DO 2 L=1,N
             S1 = S1 + ( AKP1N(L) - AKP1S(L) )**2
             S2 = S2 + ( AKP1N(L) - AK(L)    )**2
    2     CONTINUE
C
          S1 = SQRT( S1 )
          S2 = SQRT( S2 )
C
          IF( S2 .GT. 1.0D-12 ) THEN
              ERRVEC = S1 / S2
          ELSE
	      ERRVEC = 0.D0
          END IF
C
          RETURN
          END
C***************************************************************
C
          SUBROUTINE  DIFGEO( RM, ZM, RAXPR, ZAXPR, RMAOLD, ZMAOLD,
     *                        DELAXA, DELAXR, NOUT, NTER )
C
       include 'double.inc'
C
          SQRT(X) = DSQRT(X)
C
          DELAXA = SQRT( (RMAOLD-RM)**2 + (ZMAOLD-ZM)**2 )
          DELAXT = SQRT( (RAXPR -RM)**2 + (ZAXPR -ZM)**2 )
          IF( DELAXT.GT.1.0D-12 )  THEN
              DELAXR   = DELAXA / DELAXT
          ELSE
              DELAXR = 0.D0
          END IF
C
          RETURN
          END
C***************************************************************
C***************************************************************
C  NTYPE=1 - CONDUCTOR WITH "LINEAR"      CROSS-SECTION
C  NTYPE=2 - CONDUCTOR WITH "RECTANGULAR" CROSS-SECTION
C  NTYPE=3 - CONDUCTOR WITH "ROUND"       CROSS-SECTION
C  RC      - CILINDR. "R" COORDINATE  OF  CROSS-SECTION CENTRE
C  VC      - VERTICAL (OR RADIUS for NTYPE=3) SIZE OF CROSS-SECTION
C  HC      - HORIZONTAL                       SIZE OF CROSS-SECTION
C
          REAL*8 FUNCTION SELIND( NTYPE, RC, VC, HC )
C
       include 'double.inc'
C
          ALOG(X) = DLOG(X)
          SQRT(X) = DSQRT(X)
          ATAN(X) = DATAN(X)
C
          NOUT = 17
          NTER = 6
          PI2   = 2.D0 * 3.14159265399D0
C********************************************************************
C
      IF( NTYPE .EQ. 1 ) THEN

          VCL=SQRT(HC**2+VC**2)

C--- FROM A.KAVIN
C
C+++      SELIND = RC * (ALOG( 8D0*RC/VCL ) - 0.5D0) / PI2/1.0d0
C
          SELIND=0.d0
          do i=-1,1
          do j=-1,i
          if(i .eq. j) then
          SELIND = SELIND+ (RC+HC*i/3.d0)
     >     * (ALOG( 8.D0*(RC+HC*i/3.d0)/(VCL/3.d0) ) - 0.5D0) / PI2
c     > * (ALOG( 8D0*(RC+HC*i/3.)/(0.2236*(VCL/3.+0.06)) ) - 2D0) / PI2
          else
          SELIND=SELIND+
     >    GREENI( RC+HC*i/3.d0,VC*i/3.d0,RC+HC*j/3.d0,VC*j/3.d0)*2d0 
     >     /(PI2/2.d0)
          endif
          enddo
          enddo
C
          SELIND = SELIND/9.d0
C
C--- FROM R.HYRUTDINOV
C...        SQVC   = SQRT(VCL)
C...        SELIND = RC * (ALOG( 16.D0*RC/SQVC ) - 0.5D0) / PI2
      END IF
C********************************************************************
C
          IF( NTYPE .EQ. 2 ) THEN
C--- FROM A.KAVIN
            SELIND = RC*(ALOG(8.D0*RC/(0.2236d0*(VC+HC)))-2.D0) / PI2
C--- FROM R.HYRUTDINOV
C...        SQVC   = SQRT(VC+HC)
C...        SELIND = RC*(  ALOG(16.D0*RC/SQVC) - 0.5D0 *
C... *                     ( (VC/HC)*ATAN(HC/VC) +
C... *                       (HC/VC)*ATAN(VC/HC) )       ) / PI2
          END IF
C********************************************************************
C
          IF( NTYPE .EQ. 3 ) THEN
C--- FROM A.KAVIN
            SELIND = RC*(ALOG(8.D0*RC/VC)-1.75D0) / PI2
          END IF
C********************************************************************
C
       IF((NTYPE.NE.1).AND.(NTYPE.NE.2).AND.(NTYPE.NE.3)) THEN
          !WRITE(NTER,*) '"NTYPE" DEFINITION IS WRONG : NTYPE =',NTYPE
          !WRITE(NOUT,*) '"NTYPE" DEFINITION IS WRONG : NTYPE =',NTYPE
          !WRITE(NTER,*) '!!! PROGRAM WAS INTERRUPTED IN "SELIND" '
          !WRITE(NOUT,*) '!!! PROGRAM WAS INTERRUPTED IN "SELIND" '
C
          STOP
C
       END IF
C
       RETURN
       END
C***************************************************************
C  NTYPE=1 - CONDUCTOR WITH "LINEAR"      CROSS-SECTION
C  NTYPE=2 - CONDUCTOR WITH "RECTANGULAR"  CROSS-SECTION
C  NTYPE=3 - CONDUCTOR WITH "ROUND"        CROSS-SECTION
C  RC,ZC   - CILINDR. "R,Z" COORDINATES OF CROSS-SECTION CENTRE
C  VC      - VERTICAL (OR RADIUS for NTYPE=3) SIZE OF CROSS-SECTION
C  HC      - HORIZONTAL                       SIZE OF CROSS-SECTION
C
C  INDEX "1" MEANS THE FIRST  CONDUCTOR
C  INDEX "2" MEANS THE SECOND CONDUCTOR
C
          REAL*8 FUNCTION BETIND( NTYPE1, RC1, ZC1, VC1, HC1,
     *                            NTYPE2, RC2, ZC2, VC2, HC2 )
C
       include 'double.inc'
C
          PI = 3.14159265399D0
C
C*************************************************************
      IF( NTYPE1 .EQ. 1 .and. NTYPE2 .EQ. 1) THEN
          BETIND=0.d0
          do i=-1,1
          do j=-1,1
          BETIND=BETIND+
     >    GREENI( RC1+HC1*i/3.d0,ZC1+VC1*i/3.d0,RC2+HC2*j/3.d0,ZC2
     >    +VC2*j/3.d0)
          enddo
          enddo
          BETIND = BETIND/PI/9.d0
      ENDIF
C
       IF( NTYPE1 .EQ. 1 .and. NTYPE2 .EQ. 2) THEN
          BETIND=0.d0
          do i=-1,1
          BETIND=BETIND+
     >    GREENI( RC1+HC1*i/3.d0,ZC1+VC1*i/3.d0,RC2,ZC2)
          enddo
          BETIND = BETIND/PI/3.d0
       ENDIF
C
         IF( NTYPE1 .EQ. 2 .and. NTYPE2 .EQ. 1) THEN
          BETIND=0.d0
          do j=-1,1
          BETIND=BETIND+
     >    GREENI( RC1,ZC1,RC2+HC2*j/3.d0,ZC2+VC2*j/3.d0)
          enddo
          BETIND = BETIND/PI/3.d0
         ENDIF
C
         IF( NTYPE1 .EQ. 2 .and. NTYPE2 .EQ. 2) THEN
            BETIND = GREENI( RC1, ZC1, RC2, ZC2 ) / PI
         ENDIF
C
         IF( NTYPE1 .EQ. 3 .OR.  NTYPE2 .EQ. 3) THEN
            BETIND = GREENI( RC1, ZC1, RC2, ZC2 ) / PI
         ENDIF
C
          RETURN
          END
C**********************************************************
C**********************************************************
C
       SUBROUTINE  GAVPAR( NOUT    , NTER    , KEYPRI  ,
     *                     KSTEP   , TSTEP   , TIMEV   ,
     *                     NCEQUI  , VOLKP1  ,
     *                     k_contr , tau_p   , KSTEP_C , TSTEP_C ,
     *                     v_contr , v0_contr, v_prog  , v_full  ,
     *                     res_comp, res_extr, pfc_ref )
C
       include 'double.inc'
C
       INCLUDE 'prm.inc'
       INCLUDE 'comevl.inc'
C---------------------------------------------------------------
C
       DIMENSION  VOLKP1(*)
C
       DIMENSION   v_contr(n_volt_m), v_prog(n_volt_m),
     *            v0_contr(n_volt_m), v_full(n_volt_m)
       DIMENSION  res_comp(n_volt_m), res_extr(n_volt_m)
       DIMENSION   pfc_ref(n_volt_m), tau_p(n_volt_m)
C
       DIMENSION  TMCUR(100), VE1(100), VE8(100)

       INTEGER    kp(100)
C
C----------------------------------------------------------------------
C**********************************************************************
C----------------------------------------------------------------------
C   COMPUTING PFC EQUIV. GROUPS VOLTAGES in [ VOLT ] ON NEW TIME-LEVEL
C   T(KSTEP) = TIMEV  AND THEY TIME DERIVATIVES:
C
          PI  = 3.14159265359D0
          aaa = 1000000.d0     !!! for transfer of currents into [A]
          BBB = 1.D0 / ( 2.D0*PI )
          KSTEPR = KSTEP - 1
C
C  -----  [ VOLKP1(L) ] = [ VOLT ] * BBB - the PET code dimension
C  -----  PFVOL2 (as they first appear) are defined in [ VOLT ]
C
C         ------- from previous time-level -------
C
      IF( k_contr .eq. 0 ) THEN
C
       DO L=1,NEQUI
          PFVOL2(L) = 0.0d0
       END DO
C
C +++ VREMYANKA +++ for equilibrium perturbation during VDE evolution
C     in vertical direction for ITER device
C
           n_pert =  8
       if( n_pert.ne.0 ) then
C
          k_star = 5
          k_pert1= 5
          k_pert2= 5
          k_rest = 5
C
	    dvolt  = 300.d0  !!! [Volt}
C  
C
          kp(1)  = k_star
          do i=2,n_pert
            kp(i) = kp(i-1) + k_pert1 + k_pert2 + k_rest
          end do
C
          do i=1,n_pert
	     kk1 = kp(i)+k_pert1
           kk2 = kp(i)+k_pert1+k_pert2
           if((kp(i).lt.kstep).and.(kstep.le.kk1))then
              PFVOL2(6)  =  dvolt
              PFVOL2(11) = -dvolt
           end if
           if((kk1.lt.kstep).and.(kstep.le.kk2))then
              PFVOL2(6)  = -490.d0
              PFVOL2(11) =  490.d0
           end if         
          end do
       end if
C
C           DO 21 L=1,NEQUI
C   21         PFVOL2(L) = PFVOL1(L)
C  
C +++ VREMYANKA +++ for initial equilibrium perturbation
C     in vertical direction for TCV device
C
C           IF(KSTEP.EQ.1) THEN
C              PFVOL2(3)  =  10000.0d0
C              PFVOL2(10) = -10000.0d0
C              PFVOL2(3)  =  5000.0d0
C              PFVOL2(10) = -5000.0d0
C           ELSE
C              PFVOL2(3)  = 0.0d0
C              PFVOL2(10) = 0.0d0
C           END IF
C +++++++++++++++++
C
      ELSE   !!! for  IF( k_contr .eq. 0 ) THEN
C
          IF( (KSTEPR/KSTEP_C)*KSTEP_C .EQ. KSTEPR ) THEN
C
C+++    coef_p = 0.0d0
C+++    coef_p = 0.1d0
C+++    coef_p = 0.5d0
        coef_p = 1.0d0
C
C+++    c2     = 0.0d0
        c2     = 1.0d0
C----------------------------------------------------------------------
C======================================================================
        c3 = 0.0d0
C+++    c3 = 1.0d0
C
      IF( c3 .GT. 0.0001d0 ) THEN
C
        TMCUR(1) = 0.449d0
        TMCUR(2) = 0.450d0
        TMCUR(3) = 0.451d0
        TMCUR(4) = 0.452d0
C
          VE1(1) =    0.d0
          VE1(2) =   75.d0
          VE1(3) =   75.d0
          VE1(4) =    0.d0
C
          VE8(1) =    0.d0
          VE8(2) =  -75.d0
          VE8(3) =  -75.d0
          VE8(4) =    0.d0
C
       IF( TIMEV .LE. TMCUR(1) ) THEN
           v0_contr(3)  = VE1(1)
           v0_contr(10) = VE8(1)
       END IF
       IF( TIMEV .GE. TMCUR(4) ) THEN
           v0_contr(3)  = VE1(4)
           v0_contr(10) = VE8(4)
       END IF
C
       IF( (TIMEV .GT. TMCUR(1)) .AND. (TIMEV .LT. TMCUR(4)) ) THEN
            DO  I=1,3
               IF((TIMEV.GE.TMCUR(I)).AND.(TIMEV.LT.TMCUR(I+1))) THEN
                    v0_contr(3)  = ( VE1(I)  *(TMCUR(I+1) - TIMEV) +
     *                               VE1(I+1)*(TIMEV - TMCUR(I)  ) ) /
     *                                        (TMCUR(I+1) - TMCUR(I) )
                    v0_contr(10) = ( VE8(I)  *(TMCUR(I+1) - TIMEV) +
     *                               VE8(I+1)*(TIMEV - TMCUR(I)  ) ) /
     *                                        (TMCUR(I+1) - TMCUR(I) )
               END IF
            END DO
       END IF
      END IF
C----------------------------------------------------------------------
C======================================================================
C
        do i=1,NEQUI
C           
          c0 = tau_p(i)/TSTEP_C
          c0 = c0*coef_p
C
          c1 = 1.d0 / ( 1.d0 + c0 )
C
          PFVOL2(i) = c1*( c0*PFVOL1(i) + c2*v_contr(i) + c3*v0_contr(i)
     *                   + v_prog(i)
     *                   + res_comp(i)*pfceqw(i)*aaa
     *                   + res_extr(i)*(pfc_ref(i) - pfceqw(i)*aaa) )
C
C+++       PFVOL2(i) = v_contr(i)
C+++       PFVOL2(i) = v_full(i)
C+++       PFVOL2(i) = PFVOL1(i)
C
        end do
c
          END IF  !!! for " IF( (KSTEPR/KSTEP_C)*KSTEP_C .EQ. KSTEPR ) "
      END IF   !!! for  " if( k_contr .eq. 0 ) then "
C
        !WRITE(NOUT,*) '-----------------------------------------'
        !write(NOUT,*) 'From "GAVPAR" => PFVOL2(NEQUI) for time =',timev
        !write(NOUT,*) '                 in [Volt]    for kstep =',kstep
        !write(NOUT,*) '                             for coef_p =',coef_p
        !write(NOUT,101) (PFVOL2(k), k=1,NEQUI)
        !WRITE(NTER,*) '--------------------------------------'
        !write(NTER,*) 'From "GAVPAR" => PFVOL2(NEQUI) for time =',timev
        !write(NTER,*) '                  in [Volt]   for kstep =',kstep
        !write(NTER,*) '                             for coef_p =',coef_p
        !write(NTER,101) (PFVOL2(k), k=1,NEQUI)
        !write(NTER,*) '     '
C
  101  FORMAT(2X,5E15.7)
C
C-----------------------------------------------------------------------
C  -----  TIME DERIVATIVES COMPUTING [in Volt/sec]
C
          DO 22 L=1,NEQUI
   22        DPFVDT(L) = ( PFVOL2(L) - PFVOL1(L) ) / TSTEP
C----------------------------------------------------------------
C----------------------------------------------------------------
C  -----  PFVOL2 are transformed in the PET code dimension
C
          DO 23 L=1,NEQUI
             VOLKP1(L) = PFVOL2(L) * BBB
   23     CONTINUE
C
C----------------------------------------------------------------
C----------------------------------------------------------------
C
          RETURN
          END
C****************************************************************
C****************************************************************
C  TRANSFORM OF "FULL" MATRIX "P" ( "NC*NC" SIZE ) TO
C  "EQUIVALENT" MATRIX "P" ( "NCEQUI*NCEQUI" SIZE ).
C  HERE   NCEQUI = NC - NCPFC + NEQUI.
C**********************************************************
C
         SUBROUTINE TRAMAT( P, NJLIM, NC, NCPFC, NEQUI,
     *                      NECON, WECON )
C
       include 'double.inc'
C
         DIMENSION  P(NJLIM,NJLIM)
         DIMENSION  T(NJLIM)
         DIMENSION  WECON(*)
         INTEGER    NECON(*)
C
C----------------------------------------------------------
C
        NCEQUI = NC - NCPFC + NEQUI
C
C  SUMMING OF COLUMNS !!!
C  ----------------------
        DO 1 L=1,NEQUI
           DO 2 I=1,NC
              T(I) = 0.D0
    2      CONTINUE
           DO 3 J=1,NCPFC
              IF( NECON(J).EQ.L ) THEN
                  DO 4 I=1,NC
                     T(I) = T(I) + P(I,J)*WECON(J)
    4             CONTINUE
              END IF
    3      CONTINUE
           DO 5 I=1,NC
              P(I,L) = T(I)
    5      CONTINUE
    1   CONTINUE
C
        DO 6 J=NCPFC+1,NC
        DO 6 I=1,NC
           P(I,NEQUI + J - NCPFC) = P(I,J)
    6   CONTINUE
C
C  SUMMING OF ROWS !!!
C  -------------------
        DO 11 L=1,NEQUI
           DO 12 J=1,NCEQUI
              T(J) = 0.D0
   12      CONTINUE
           DO 13 I=1,NCPFC
              IF( NECON(I).EQ.L ) THEN
                  DO 14 J=1,NCEQUI
                     T(J) = T(J) + P(I,J)*WECON(I)
   14             CONTINUE
              END IF
   13      CONTINUE
           DO 15 J=1,NCEQUI
              P(L,J) = T(J)
   15      CONTINUE
   11   CONTINUE
C
        DO 16 I=NCPFC+1,NC
        DO 16 J=1,NCEQUI
           P(NEQUI + I - NCPFC,J) = P(I,J)
   16   CONTINUE
C
         RETURN
         END
C*************************************************************
C  TRANSFORM OF "FULL" VECTOR "V" ( "NC" SIZE ) TO
C  "EQUIVALENT" VECTOR "V" ( "NCEQUI" SIZE ).
C  HERE   NCEQUI = NC - NCPFC + NEQUI.
C*************************************************************
C
       SUBROUTINE TRAVEC (VINP, VOUT, NC, NCPFC, NEQUI, NECON, WECON)
C
       include 'double.inc'
C
         DIMENSION  VINP(*), VOUT(*), WECON(*)
         INTEGER    NECON(*)
C
C----------------------------------------------------------
C
        DO 1 L=1,NEQUI
           T = 0.D0
           DO 2 I=1,NCPFC
              IF( NECON(I).EQ.L ) THEN
                  T = T + VINP(I)*WECON(I)
              END IF
    2      CONTINUE
           VOUT(L) = T
    1   CONTINUE
        DO 3 I=NCPFC+1,NC
           VOUT(NEQUI+I-NCPFC) = VINP(I)
    3   CONTINUE
C
       RETURN
       END
C
C***********************************************************************
C  TRANSFORM OF EDDY CURRENT VECTOR "PJKP1" ("NCEQUI" SIZE)
C  TO VECTOR "PC" ("NC" SIZE) FOR EQUILIBRIUM and to vector "PFCEQW"
C  HERE   NCEQUI = NC - NCPFC + NEQUI.
C***********************************************************************
C
       SUBROUTINE GRETA ( PJKP1, NCEQUI, PC )
C
       include 'double.inc'
C
       INCLUDE 'prm.inc'
       INCLUDE 'comevl.inc'
C
       DIMENSION  PJKP1(*), PC(*)
C----------------------------------------------------------
C
          DO 1 I=1,NPFC

             PFCW2(I)  = PJKP1( NEPFC(I) )
             PFCUR2(I) = PFCW2(I)  * NTURN(I)
             PFCD2(I)  = PFCUR2(I) / NDIV(I)

             NN1 = NLOC(I-1) + 1
             NN2 = NLOC(I)
             DO 2 L=NN1,NN2
                PC(L) = PFCD2(I)
    2        CONTINUE
    1     CONTINUE
          NCPFC = NLOC(NPFC)
          DO 3 I=NEQUI+1,NCEQUI
             PC( NCPFC+I-NEQUI ) = PJKP1(I)
    3     CONTINUE
C
          DO 4 I=1,NEQUI
             PFCEQW(I) = PJKP1(I)
    4     CONTINUE
C
       RETURN
       END
C*********************************************************************
C  
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       SUBROUTINE PRINTA( NOUT  , NTER  , KEYPRI, NGAV1 ,
     *                    KSTEP , TSTEP , TIMEV , KNEL  ,
     *                    BETPOL, ZLI3  , DELBET, DELZLI,
     *                    TOKOUT, PSIOUT, PSIBOU, PSIDEL,
     *                    ALF0  , ALF1  , ALF2  , BET0  , BET1 , BET2,
     *                    RM    , ZM    , RX0   , ZX0   ,
     *                    DELRMA, DELZMA, DELRMB, DELZMB,
     *                    DELRXP, DELZXP, DELRXB, DELZXB,
     *                    NCTRL , ALP   , ALPNEW, NUMLIM,
     *                    fwcurr, bpcurr, vvcurr,
     *                    pvolum, helout )
C
       include 'double.inc'
C
         INCLUDE 'param.inc'
C
         common /comus1/ rus1(nbndp2),  zus1(nbndp2), nus1
         common /comus2/ rus2(nbndp2),  zus2(nbndp2), nus2
         common /comlop/  rxb(nbndp2),   zxb(nbndp2), nxb
C
         common /equili/ bettot, rmx,    zzrmx,  rmn,   zzrmn,
     *                           zmx,    rrzmx,  zmn,   rrzmn,
     *                           r0cen,  z0cen,  radm,  aspect,
     *                           Eupper, Elower, DELup, DELlw, Bfvakc
         common /volpla/ Vol_pl
C.......................................................................
C
         pvolum = 2.d0*PI*Vol_pl
C
      IF( KEYPRI.EQ.1 ) THEN
          WRITE(NOUT,*) '.............................................'
          WRITE(NOUT,*) '  AFTER "EQ" TIME  =',TIMEV
          WRITE(NOUT,*) '             KSTEP =',KSTEP ,'  KNEL  =',KNEL
          WRITE(NOUT,*) '             NGAV1 =',NGAV1 ,'  NCTRL =',NCTRL
          WRITE(NOUT,*) '.............................................'
C         WRITE(NOUT,*) '  BETTOT =',BETTOT,'  B*R0   =',Bfvakc
          WRITE(NOUT,*) '  BETPOL =',BETPOL,'  LI3    =',ZLI3
          WRITE(NOUT,*) '  TOKOUT =',TOKOUT,'  PSIout =',PSIOUT
          WRITE(NOUT,*) '  PSIdel =',PSIDEL,'  PSIbou =',PSIBOU
          WRITE(NOUT,*) '  HELOUT =',HELOUT
          WRITE(NOUT,*) '  NUMLIM =',NUMLIM
          WRITE(NOUT,*) '  ALP    =',ALP   ,'  ALPNEW =',ALPNEW
          WRITE(NOUT,*) '  ALF0   =',ALF0  ,'  BET0   =',BET0
          WRITE(NOUT,*) '  ALF1   =',ALF1  ,'  BET1   =',BET1
          WRITE(NOUT,*) '  ALF2   =',ALF2  ,'  BET2   =',BET2
          WRITE(NOUT,*) '.............................................'
          WRITE(NOUT,*) '  Rax    =',RM    ,'  Zax    =',ZM
          WRITE(NOUT,*) '  Rxp    =',RX0   ,'  Zxp    =',ZX0
          WRITE(NOUT,*) '  Rmax   =',RMX   ,'  ZRmax  =',ZZRMX
          WRITE(NOUT,*) '  Rmin   =',RMN   ,'  ZRmin  =',ZZRMN
          WRITE(NOUT,*) '  Zmax   =',ZMX   ,'  RZmax  =',RRZMX
          WRITE(NOUT,*) '  Zmin   =',ZMN   ,'  RZmin  =',RRZMN
          WRITE(NOUT,*) '  Aspect =',ASPECT,'  Rminor =',RADM
          WRITE(NOUT,*) '  Eupper =',Eupper,'  Elower =',Elower
          WRITE(NOUT,*) '  Tupper =',DELup ,'  Tlower =',DELlw
C
C          WRITE(NOUT,*) '                                   '
C          WRITE(NOUT,*) 'Toroidal plasma current     (MA) =',tokout
C          WRITE(NOUT,*) 'Tor. first wall  current    (MA) =',fwcurr
C          WRITE(NOUT,*) 'Tor. back plate  current    (MA) =',bpcurr
C          WRITE(NOUT,*) 'Tor. vac. vessel current    (MA) =',vvcurr
C          WRITE(NOUT,*) 'Plasma volume             (m**3) =',pvolum
C          WRITE(NOUT,*) 'Current centroid coord.   Z (m ) =',z0cen
C          WRITE(NOUT,*) 'Helicity                         =',helout
C          WRITE(NOUT,*) '                                   '
C          WRITE(NOUT,*) '--------------------------------------------'
C          WRITE(NOUT,*) 'PLASMA BOUNDARY => RXB(L), L=1,NXB : NXB=',NXB
C          WRITE(NOUT,101)  (RXB(L), L=1,NXB)
C          WRITE(NOUT,*) 'PLASMA BOUNDARY => ZXB(L), L=1,NXB : NXB=',NXB
C          WRITE(NOUT,101)  (ZXB(L), L=1,NXB)
C
C         CALL  ARCLE( NXB, RXB, ZXB, ARC, ARCMAX )
C
C          WRITE(NOUT,*) 'PLASMA BOUNDARY => ARC(L), L=1,NXB : NXB=',NXB
C          WRITE(NOUT,*) '                   ARCMAX = ',ARCMAX
C...       WRITE(NOUT,101)  (ARC(L), L=1,NXB)
C          WRITE(NOUT,*) '                                            '
C          WRITE(NOUT,*) '--------------------------------------------'
C          WRITE(NOUT,*) '(R,Z)-COORDINATES OF THE SEPARATRIX BRANCHS '
C          WRITE(NOUT,*) '                                            '
C          WRITE(NOUT,*) '   RUS1(I), I=1,..,NUS1 = ', NUS1
C          WRITE(NOUT,101)  (RUS1(I), I=1,NUS1)
C          WRITE(NOUT,*) '   ZUS1(I), I=1,..,NUS1 = ', NUS1
C          WRITE(NOUT,101)  (ZUS1(I), I=1,NUS1)
C          WRITE(NOUT,*) '   RUS2(I), I=1,..,NUS2 = ', NUS2
C          WRITE(NOUT,101)  (RUS2(I), I=1,NUS2)
C          WRITE(NOUT,*) '   ZUS2(I), I=1,..,NUS2 = ', NUS2
C          WRITE(NOUT,101)  (ZUS2(I), I=1,NUS2)
C          WRITE(NOUT,*) '                                            '
C
C          IF( KSTEP.EQ.0 )  THEN
C		    CALL PR_Q_TAB( NOUT )
C          ELSE
C              CALL PR_Q( NOUT )
C          END IF

          IF( KSTEP .NE. 0 )  THEN
c          WRITE(NOUT,*) 'TIME-STEP    SHIFT    OF MAG.AXES AND X-POINT'
c          WRITE(NOUT,*) '  DELRMA =',DELRMA,'  DELZMA =',DELZMA
c          WRITE(NOUT,*) '  DELRXP =',DELRXP,'  DELZXP =',DELZXP
c          WRITE(NOUT,*) 'TIME-STEP CONVERGENCE OF MAG.AXES AND X-POINT'
c          WRITE(NOUT,*) '  DELRMB =',DELRMB,'  DELZMB =',DELZMB
c          WRITE(NOUT,*) '  DELRXB =',DELRXB,'  DELZXB =',DELZXB
c          WRITE(NOUT,*) 'TIME-STEP CONVERGENCE OF  BETAP   AND   LI3  '
c          WRITE(NOUT,*) '  DELBET =',DELBET,'  DELZLI =',DELZLI
          END IF
      END IF
C***********************************************************************
C
          !WRITE(NTER,*) '.............................................'
          !WRITE(NTER,*) '  AFTER "EQ" TIME  =',TIMEV
          !WRITE(NTER,*) '             KSTEP =',KSTEP ,'  KNEL  =',KNEL
          !WRITE(NTER,*) '             NGAV1 =',NGAV1 ,'  NCTRL =',NCTRL
          !WRITE(NTER,*) '.............................................'
          !WRITE(NTER,*) '  BETTOT =',BETTOT,'  B*R0   =',Bfvakc
          !WRITE(NTER,*) '  BETPOL =',BETPOL,'  LI3    =',ZLI3
          !WRITE(NTER,*) '  TOKOUT =',TOKOUT,'  PSIout =',PSIOUT
          !WRITE(NTER,*) '  PSIdel =',PSIDEL,'  PSIbou =',PSIBOU
          !WRITE(NTER,*) '  HELOUT =',HELOUT
          !WRITE(NTER,*) '  NUMLIM =',NUMLIM
          !WRITE(NTER,*) '  ALP    =',ALP   ,'  ALPNEW =',ALPNEW
          !WRITE(NTER,*) '  ALF0   =',ALF0  ,'  BET0   =',BET0
          !WRITE(NTER,*) '  Rax    =',RM    ,'  Zax    =',ZM
          !WRITE(NTER,*) '  Rxp    =',RX0   ,'  Zxp    =',ZX0

C          IF( KSTEP.EQ.0 )  THEN
C		    CALL PR_Q_TAB( NTER )
C          ELSE
C              CALL PR_Q( NTER )
C          END IF

C          IF( KSTEP .NE. 0 )  THEN
C          WRITE(NTER,*) 'TIME-STEP    SHIFT    OF MAG.AXES AND X-POINT'
C          WRITE(NTER,*) '  DELRMA =',DELRMA,'  DELZMA =',DELZMA
C          WRITE(NTER,*) '  DELRXP =',DELRXP,'  DELZXP =',DELZXP
C          WRITE(NTER,*) 'TIME-STEP CONVERGENCE OF MAG.AXES AND X-POINT'
C          WRITE(NTER,*) '  DELRMB =',DELRMB,'  DELZMB =',DELZMB
C          WRITE(NTER,*) '  DELRXB =',DELRXB,'  DELZXB =',DELZXB
C          END IF
C-----------------------------------------------------------------------
C
  101  FORMAT(2X,5E14.7)
C
       RETURN
       END
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       SUBROUTINE PRINTB(NOUT , NTER  , KEYPRI, KSTEP , TSTEP , TIMEV,
     *                  NGAV1 , NCTRL ,
     *                  BETPLX, FTOK  , PSIAX ,
     *                  ALF0  , ALF1  , ALF2  , BET0  , BET1  , BET2 )
C
       include 'double.inc'
C
        common/comhel/  helinp, helout
C.....................................................................
C
      IF( KEYPRI.EQ.1 ) THEN
          WRITE(NOUT,*) '.............................................'
          WRITE(NOUT,*) '  KSTEP  =',KSTEP
          WRITE(NOUT,*) '  TIME   =',TIMEV ,'  TSTEP  =',TSTEP
          WRITE(NOUT,*) '.............................................'
C          WRITE(NOUT,*) '  NGAV1  =',NGAV1 ,'  NCTRL  =',NCTRL
C          WRITE(NOUT,*) '  BETPLX =',BETPLX,'  HELINP =',HELINP
C          WRITE(NOUT,*) '  FTOK   =',FTOK  ,'  PSIAX  =',PSIAX
C          WRITE(NOUT,*) '  ALF0   =',ALF0  ,'  BET0   =',BET0
C          WRITE(NOUT,*) '  ALF1   =',ALF1  ,'  BET1   =',BET1
C          WRITE(NOUT,*) '  ALF2   =',ALF2  ,'  BET2   =',BET2
C          WRITE(NOUT,*) '.............................................'
      END IF
C
          !WRITE(NTER,*) '.............................................'
          !WRITE(NTER,*) '  KSTEP  =',KSTEP
          !!WRITE(NTER,*) '  TIME   =',TIMEV ,'  TSTEP  =',TSTEP
          !WRITE(NTER,*) '.............................................'
          !WRITE(NTER,*) '  NGAV1  =',NGAV1 ,'  NCTRL  =',NCTRL
          !WRITE(NTER,*) '  BETPLX =',BETPLX,'  HELINP =',HELINP
          !WRITE(NTER,*) '  FTOK   =',FTOK  ,'  PSIAX  =',PSIAX
          !WRITE(NTER,*) '  ALF0   =',ALF0  ,'  BET0   =',BET0
          !WRITE(NTER,*) '  ALF1   =',ALF1  ,'  BET1   =',BET1
          !WRITE(NTER,*) '  ALF2   =',ALF2  ,'  BET2   =',BET2
          !WRITE(NTER,*) '.............................................'
C---------------------------------------------------------------
C
       RETURN
       END
C-----------------------------------------------------------------------
C***********************************************************************
C
        SUBROUTINE eq_par( z0cen_e, alp_e,    alpnew_e, qcen_e,
     *                     nctrl_e, numlim_e, up_e,
     *                     rm_e,    zm_e,     rx0_e,    zx0_e   )
C
       include 'double.inc'
C
        INCLUDE 'param.inc'
        INCLUDE 'comblc.inc'
C
        common /equili/  bettot, rmx,    zzrmx,  rmn,   zzrmn,
     *                           zmx,    rrzmx,  zmn,   rrzmn,
     *                           r0cen,  z0cen,  radm,  aspect,
     *                           Eupper, Elower, DELup, DELlw, Bfvakc
C  ---------------------------------------------------------------------
C
        z0cen_e  = z0cen
        alp_e    = alp
        alpnew_e = alpnew
        qcen_e   = qcen

        nctrl_e  = nctrl
        numlim_e = numlim
        up_e     = up
        
        rm_e     = rm
        zm_e     = zm
        rx0_e    = rx0
        zx0_e    = zx0
C
        RETURN
        END
C***********************************************************************
C***********************************************************************
C--- INPUT PARAMETERS OF PFC SYSTEM AND PASSIV CONDUCTORS
C
       SUBROUTINE CONDUC( NC, NCEQUI, NCPFC, NFW, NBP, NVV,
     *                    RC,ZC, PC, VC,HC, NTYPE,
     *                    RC1,ZC1,  RC2,ZC2,  RC3,ZC3,  RC4,ZC4,
     *                    RES, VOLK, VOLKP1,
     *                    NECON, WECON,
     *                    NOUT, NTER, NINFW, ngra1 )
C
       include 'double.inc'
C
       INCLUDE 'prm.inc'
       INCLUDE 'comevl.inc'
C
C-----------------------------------------------------------------------
C
       DIMENSION  RC(*),  ZC(*),  PC(*), VC(NCLIM),  HC(NCLIM)
C
       DIMENSION  RC1(*), ZC1(*), RC2(*), ZC2(*),
     *            RC3(*), ZC3(*), RC4(*), ZC4(*)
C
       INTEGER    NECON(*), NTYPE(*)
       DIMENSION  WECON(*)
C
       DIMENSION  RES(*), VOLK(*), VOLKP1(*)
C.....................................................................
C
      INTEGER    KDFW(NPLIM), KDBP(NPLIM), KDVV(NPLIM)
C
      DIMENSION  RP1FW(NPLIM), ZP1FW(NPLIM), RP2FW(NPLIM), ZP2FW(NPLIM),
     *           RFWSEG(NPLIM), CFWSEG(NPLIM)
      DIMENSION  RP1BP(NPLIM), ZP1BP(NPLIM), RP2BP(NPLIM), ZP2BP(NPLIM),
     *           RBPSEG(NPLIM), CBPSEG(NPLIM)
      DIMENSION  RP1VV(NPLIM), ZP1VV(NPLIM), RP2VV(NPLIM), ZP2VV(NPLIM),
     *           RVVSEG(NPLIM), CVVSEG(NPLIM)
C
       DIMENSION  RFW(NPLIM),   ZFW(NPLIM),   DFW(NPLIM),  HFW(NPLIM),
     *            RESFW(NPLIM), CURFW(NPLIM), NTYFW(NPLIM)
       DIMENSION  RFW1(NPLIM),  ZFW1(NPLIM),  RFW2(NPLIM), ZFW2(NPLIM)
C
       DIMENSION  RBP(NPLIM),   ZBP(NPLIM),   DBP(NPLIM),  HBP(NPLIM),
     *            RESBP(NPLIM), CURBP(NPLIM), NTYBP(NPLIM)
       DIMENSION  RBP1(NPLIM),  ZBP1(NPLIM),  RBP2(NPLIM), ZBP2(NPLIM)
C
       DIMENSION  RVV(NPLIM),   ZVV(NPLIM),   DVV(NPLIM),  HVV(NPLIM),
     *            RESVV(NPLIM), CURVV(NPLIM), NTYVV(NPLIM)
       DIMENSION  RVV1(NPLIM),  ZVV1(NPLIM),  RVV2(NPLIM), ZVV2(NPLIM)
C
C***********************************************************************
          PI    = 3.14159265359d0
          BBB   = 1.D0 / (2.D0*PI)
C***********************************************************************
C
C--- INPUT OF PFC SYSTEM PARAMETERS
C
          CALL TRECUR( NCPFC, RC, ZC, PC, NTYPE, NECON, WECON,
     *                 HC, VC, NOUT, NTER )
C



!	PAUSE 'PAUSE AFTER "CALL TRECUR" '
C
          DO 1800 L=1,NEQUI
             RES(L)    = PFRES(L)  * BBB
 1800        VOLK(L)   = PFVOL1(L) * BBB
C
          NC     = NCPFC
          NCEQUI = NEQUI
C !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C***********************************************************************
C--- INPUT OF PASSIVE CONDUCTORS PARAMETERS
C
	  CALL  TREFW( NOUT, NTER, NINFW, NSEGFW,
     *               KDFW, RP1FW, ZP1FW, RP2FW, ZP2FW, RFWSEG, CFWSEG,
     *               NFW,  NTYFW,
     *               RFW,  ZFW,  DFW,   HFW,  RESFW, CURFW,
     *               RFW1, ZFW1, RFW2,  ZFW2 )
C
!	  PAUSE 'PAUSE AFTER "CALL TREFW" '
C
c        CALL  FW_tcv( NOUT, NTER, NINFW, NSEGFW,
c     *                KDFW, RP1FW, ZP1FW, RP2FW, ZP2FW, RFWSEG, CFWSEG,
c     *                NFW,  NTYFW,
c     *                RFW,  ZFW,  DFW,   HFW,  RESFW, CURFW,
c     *                RFW1, ZFW1, RFW2,  ZFW2 )
C
!	  PAUSE 'PAUSE AFTER "CALL FW_tcv" '
C
        IF( NFW.NE.0 )  THEN
           DO 136 I=1,NFW
                RC(NC+I) = RFW(I)
                ZC(NC+I) = ZFW(I)
                PC(NC+I) = CURFW(I)
             NTYPE(NC+I) = NTYFW(I)
                VC(NC+I) = DFW(I)
                HC(NC+I) = HFW(I)
               RC1(NC+I) = RFW1(I)
               ZC1(NC+I) = ZFW1(I)
               RC2(NC+I) = RFW2(I)
               ZC2(NC+I) = ZFW2(I)
C
           RES(NCEQUI+I) = RESFW(I) * BBB   !!!!!!!!!!   * 1.d+10   
          VOLK(NCEQUI+I) = 0.00D0   * BBB
C+++      VOLK(NCEQUI+I) = RES(NCEQUI+I)*CURFW(I)
  136     CONTINUE
C
          NC     = NC     + NFW
          NCEQUI = NCEQUI + NFW
        END IF
C---------------------------------------------------------------
C
	  CALL  TREBP( NOUT, NTER, NINFW, NSEGBP,
     *               KDBP, RP1BP, ZP1BP, RP2BP, ZP2BP, RBPSEG, CBPSEG,
     *               NBP,  NTYBP,
     *               RBP,  ZBP,  DBP,   HBP,  RESBP,  CURBP,
     *               RBP1, ZBP1, RBP2,  ZBP2 )
C
!	  PAUSE 'PAUSE AFTER "CALL TREBP" '
C
        IF( NBP.NE.0 )  THEN
           DO 137 I=1,NBP
                RC(NC+I) = RBP(I)
                ZC(NC+I) = ZBP(I)
                PC(NC+I) = CURBP(I)
             NTYPE(NC+I) = NTYBP(I)
                VC(NC+I) = DBP(I)
                HC(NC+I) = HBP(I)
               RC1(NC+I) = RBP1(I)
               ZC1(NC+I) = ZBP1(I)
               RC2(NC+I) = RBP2(I)
               ZC2(NC+I) = ZBP2(I)
C
           RES(NCEQUI+I) = RESBP(I) * BBB
          VOLK(NCEQUI+I) = 0.00D0   * BBB
  137     CONTINUE
C
          NC     = NC     + NBP
          NCEQUI = NCEQUI + NBP
        END IF
C---------------------------------------------------------------
C
	  CALL  TREVV( NOUT, NTER, NINFW, NSEGVV,
     *               KDVV, RP1VV, ZP1VV, RP2VV, ZP2VV, RVVSEG, CVVSEG,
     *               NVV,  NTYVV,
     *               RVV,  ZVV,  DVV,   HVV,  RESVV, CURVV,
     *               RVV1, ZVV1, RVV2,  ZVV2 )
C
!	  PAUSE 'PAUSE AFTER "CALL TREVV" '
C
       IF( NVV.NE.0 )  THEN
           DO 138 I=1,NVV
                RC(NC+I) = RVV(I)
                ZC(NC+I) = ZVV(I)
                PC(NC+I) = CURVV(I)
             NTYPE(NC+I) = NTYVV(I)
                VC(NC+I) = DVV(I)
                HC(NC+I) = HVV(I)
               RC1(NC+I) = RVV1(I)
               ZC1(NC+I) = ZVV1(I)
               RC2(NC+I) = RVV2(I)
               ZC2(NC+I) = ZVV2(I)
C
           RES(NCEQUI+I) = RESVV(I) * BBB
          VOLK(NCEQUI+I) = 0.00D0   * BBB
  138     CONTINUE
C
          NC     = NC     + NVV
          NCEQUI = NCEQUI + NVV
        END IF
C----------------------------------------------------------------
          !WRITE(NOUT,*) '                                        '
          !WRITE(NOUT,*) '****************************************'
          !WRITE(NOUT,*) 'NUMBER OF EXTERNAL TURNS FOR EQUILIBRIUM'
          !WRITE(NOUT,*) '       NC      =', NC
          !WRITE(NOUT,*) '       NCPFC   =', NCPFC
          !WRITE(NOUT,*) '       NFW     =', NFW
          !WRITE(NOUT,*) '       NBP     =', NBP
          !WRITE(NOUT,*) '       NVV     =', NVV
          !WRITE(NOUT,*) 'NUMBER OF EXTERNAL TURNS FOR EDDY CURRENTS'
          !WRITE(NOUT,*) '       NCEQUI  =', NCEQUI
          !WRITE(NOUT,*) '       NEQUI   =', NEQUI
          !WRITE(NOUT,*) '       NPFC    =', NPFC
          !WRITE(NOUT,*) '----------------------------------------'
          !WRITE(NOUT,*) '                                        '
        write(fname,'(a,a)') path(1:kname),'pascon.wr'
        open(1,file=fname,form='formatted')
          !open(1,file='pascon.wr')
             write(1,*) nc, ncpfc, nfw, nbp, nvv
             write(1,*) (rc(i), i=1,nc), (zc(i), i=1,nc)
          close(1)
C.......................................................................
C
          DO i=1,NCEQUI
            VOLKP1(i) = VOLK(i)
          END DO
C
       RETURN
       END
C***********************************************************************
C***********************************************************************











