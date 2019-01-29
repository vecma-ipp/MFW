!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C       VV(I)  - VERTICAL   (Z) SIZE  OF THIN PLATE
C       HH(I)  - HORISONTAL (R) SIZE  OF THIN PLATE
C
        SUBROUTINE TREFW(  NOUT, NTER, NINFW, NP,
     *                     KD, RP1, ZP1, RP2, ZP2, RESSEG, CURSEG,
     *                     ND, NTYD,
     *                     RD, ZD, VV, HH, RESD, CURD,
     *                     R1, Z1, R2, Z2 )
C
       include 'double.inc'
C
        INTEGER    KD(*), NTYD(*)
        DIMENSION  RP1(*), ZP1(*), RP2(*), ZP2(*), RESSEG(*), CURSEG(*)
C
        DIMENSION  RD(*), ZD(*), VV(*), HH(*), RESD(*), CURD(*),
     *             R1(*), Z1(*), R2(*), Z2(*)
C
        SQRT(X) = DSQRT(X)
C
C***************************************************************
C
        write(fname,'(a,a)') path(1:kname),'blanfw.dat'
        open(NINFW,file=fname,form='formatted')
      !OPEN( NINFW, FILE='blanfw.dat')
            READ(NINFW,*)  RCEFW
            READ(NINFW,*)  NP
            !WRITE(NOUT,*) '  '
            !WRITE(NOUT,*) '***********************************'
            !WRITE(NOUT,*) 'READING FROM FILE "BlanFW.DAT" ===>'
            !WRITE(NOUT,*) '  '
            !WRITE(NOUT,*) 'One-turn wall resistance (micro*Om) =',RCEFW
            !WRITE(NOUT,*) 'Number of "basic wall segments"     =',NP
C
       ND = 0
C-----------------------------------------------------------------------
       IF( NP.NE.0 )  THEN
C
         DO 33 L=1,NP
   33    READ(NINFW,*) KD(L), RP1(L), ZP1(L), RP2(L), ZP2(L), RESSEG(L),
     *                                                        CURSEG(L)
C  
          DO 1 L=1,NP
             I = ND + 1
             DELR  = (RP2(L) - RP1(L)) / KD(L)
             DELZ  = (ZP2(L) - ZP1(L)) / KD(L)
             RD(I) = RP1(L) + 0.5D0*DELR
             ZD(I) = ZP1(L) + 0.5D0*DELZ
             VV(I) = DELZ
             HH(I) = DELR
             DDI   = SQRT( HH(I)**2 + VV(I)**2 )
             R1(I) = RP1(L)
             Z1(I) = ZP1(L)
             R2(I) = RP1(L) + DELR
             Z2(I) = ZP1(L) + DELZ
C
             SSSFW = DDI/RD(I)
             IF( KD(L) .GT. 1 ) THEN
                  DO 2 K=1,KD(L)-1
                     RD(I+K) = RD(I+K-1) + DELR
                     ZD(I+K) = ZD(I+K-1) + DELZ
                     VV(I+K) = VV(I)
                     HH(I+K) = HH(I)
                     R1(I+K) = R1(I+K-1) + DELR
                     Z1(I+K) = Z1(I+K-1) + DELZ
                     R2(I+K) = R2(I+K-1) + DELR
                     Z2(I+K) = Z2(I+K-1) + DELZ
                     SSSFW   = SSSFW + DDI/RD(I+K)
    2             CONTINUE
             END IF
C
             SSSFW = SSSFW * RESSEG(L)
C
		   DO 7 K=1,KD(L)
                RESD(I+K-1) = SSSFW * RD(I+K-1)/DDI
                NTYD(I+K-1) = 1
                CURD(I+K-1) = CURSEG(L) / KD(L)
    7        CONTINUE
C
             ND = ND + KD(L)
    1     CONTINUE
C-----------------------------------------------------------------------

            fr_cam=0.d0
           do i=1,np
            fr_cam=fr_cam+1.d0/RESSEG(i)
           enddo
        fr_cam=1.d0/fr_cam




          IF( RCEFW.GT.0 )  THEN
              SSSFW = 0.0D0
              DO 333 L=1,ND
  333            SSSFW = SSSFW + SQRT( HH(L)**2 + VV(L)**2 )/RD(L)
              SSSFW = SSSFW * RCEFW
			DO 77 L=1,ND
   77            RESD(L) = SSSFW * RD(L) / SQRT( HH(L)**2 + VV(L)**2 )
          END IF
C-----------------------------------------------------------------------
C--- COMPUTING OF THE ONE-TURN RESISTANCE OF BlanFW.DAT (in micro*Om)
C
          RESCE = 0.D0
          DO 8 I=1,ND
  8          RESCE = RESCE + 1.D0 / RESD(I)
             RESCE = 1.D0 / RESCE
        !WRITE(NOUT,*) '  '
        !WRITE(NOUT,*) 'Control one-turn wall resistance (micro*Om)'
        !WRITE(NOUT,*) '        RESCE = ', RESCE
C-----------------------------------------------------------------------
C
          !WRITE(NOUT,*) '  '
          !WRITE(NOUT,*) 'RFW(L), ZFW(L), DFW(L), RESFW(L), CURFW(L)'
          !WRITE(NOUT,*) 'L = 1,..,ND =', ND
C
          DO 22 L=1,ND
             DD = SQRT( HH(L)**2 + VV(L)**2 )
           !WRITE(NOUT,122) RD(L), ZD(L), DD, RESD(L), CURD(L)
   22        continue
  122     FORMAT(2X,5E15.7)
C
          !WRITE(NOUT,*) '  '
          !WRITE(NOUT,*) 'R1(L), Z1(L), R2(L), Z2(L), RESFW(L)'
          !WRITE(NOUT,*) 'L = 1,..,ND =', ND
C
          DO 34 L=1,ND
           !WRITE(NOUT,722) R1(L), Z1(L), R2(L), Z2(L), RESD(L)
   34        continue
  722     FORMAT(2X,5E15.7)
          !WRITE(NOUT,*) '  '
C
       END IF   !!! ( for IF(NP.NE.0) )
C
       CLOSE (NINFW)
C
       RETURN
       END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C       DD(I)  - VERTICAL   (Z) SIZE OF "FILAMENT" CROSS-SECTION
C       HH(I)  - HORISONTAL (R) SIZE OF "FILAMENT" CROSS-SECTION
C
        SUBROUTINE FW_tcv( NOUT, NTER, NINFW, NP,
     *                     KD, RP1, ZP1, RP2, ZP2, RESSEG, CURSEG,
     *                     ND, NTYD,
     *                     RD, ZD, DD, HH, RESD, CURD,
     *                     R1, Z1, R2, Z2 )
C
           include 'double.inc'
C
        INTEGER    KD(1), NTYD(1)
        DIMENSION  RP1(1), ZP1(1), RP2(1), ZP2(1), RESSEG(1), CURSEG(1)
C
        DIMENSION  RD(1), ZD(1), DD(1), HH(1), RESD(1), CURD(1),
     *             R1(1), Z1(1), R2(1), Z2(1)
C
C WORKING ARRAYS
        DIMENSION  RS(200),  ZS(200)
C**********************************************************************
C
        PI  = 3.14159265358d0
        PI2 = PI*2.D0
C
C**********************************************************************
C
        write(fname,'(a,a)') path(1:kname),'fw_curr.dat'
        open(NINFW,file=fname,form='formatted')
      !OPEN(NINFW, FILE='fw_curr.dat')
C
            READ(NINFW,*)  NP
C
            !WRITE(NOUT,*) '  '
            !WRITE(NOUT,*) '***********************************'
            !WRITE(NOUT,*) 'READING FROM FILE "FW_curr.dat" ===>'
            !WRITE(NOUT,*) '  '
            !WRITE(NOUT,*) 'Number of "basic wall segments" =', NP
C
         IF( NP.NE.0 )  THEN
            READ(NINFW,*) (CURSEG(I),I=1,NP)
C----------------------------------------------
C           TRANSFORM FROM [A] TO [MA]
C
            DO 4433 I=1,NP
               CURSEG(I) = CURSEG(I) * 1.0d-06
 4433       CONTINUE
C----------------------------------------------
         END IF
C
      CLOSE(NINFW)
C**********************************************************************
        write(fname,'(a,a)') path(1:kname),'fw_geom.dat'
        open(1,file=fname,form='formatted')
      !OPEN(NINFW, FILE='fw_geom.dat')
C
            READ(1,*)  RTY_FW
            READ(1,*)  NP
C
            !WRITE(NOUT,*) '  '
            !WRITE(NOUT,*) '***********************************'
            !WRITE(NOUT,*) 'READING FROM FILE "FW_geom.dat" ===>'
            !WRITE(NOUT,*) '  '
            !WRITE(NOUT,*) 'Wall resistivity (micro*Om*m)   =', RTY_FW
            !WRITE(NOUT,*) 'Number of "basic wall segments" =', NP
C
       ND = 0
C-----------------------------------------------------------------------
       IF( NP.NE.0 )  THEN
C
         DO 33 L=1,NP
            READ(1,*) KD(L), RP1(L), ZP1(L), RP2(L), ZP2(L)
   33    CONTINUE
C   
C        RESSEG(L), CURSEG(L)
C-----------------------------
         DO 7733 I=1,NP
C
            CALL DIVPAR( RP1(I), ZP1(I), RP2(I), ZP2(I), 0.D0, 90.D0,
     *                   CURSEG(I), KD(I),
     *                   NDIVRE, NDIVW, NDIVH, RS, ZS, PS, VERC, HORC )
C
            DO 3 J=1,NDIVRE
                    L  = ND + J
                 RD(L) = RS(J)
                 ZD(L) = ZS(J)
               CURD(L) = CURSEG(I)/NDIVRE
                 HH(L) = HORC
                 DD(L) = VERC
               NTYD(L) = 2
                 R1(L) = 0.D0
                 Z1(L) = 0.D0
                 R2(L) = 0.D0
                 Z2(L) = 0.D0
    3       CONTINUE
C
          ND = ND + NDIVRE            

 7733    CONTINUE
C***********************************************************************
C-----------------------------------------------------------------------
C--- COMPUTING OF THE ONE-TURN RESISTANCE (in micro*Om)
C    RESD(i) - for FW_vessel segments - i=1,...,ND
C    RESCE   - for FW_vessel
C
          RESCE = 0.D0
          DO 8 I=1,ND
             RESD(I) = RTY_FW * PI2*RD(I) / ( HH(I)*DD(I) )
             RESCE   = RESCE + 1.D0 / RESD(I)
    8     CONTINUE
          RESCE = 1.D0 / RESCE
C
        !WRITE(NOUT,*) '  '
        !WRITE(NOUT,*) 'Control one-turn wall resistance (micro*Om)'
        !WRITE(NOUT,*) '        RESCE = ', RESCE
C-----------------------------------------------------------------------
C
      !WRITE(NOUT,*) '  '
      !WRITE(NOUT,*) 'RFW(L), ZFW(L), DFW(L), HFW(L)'
      !WRITE(NOUT,*) 'L = 1,..,ND =', ND
C
C        DO  L=1,ND
C          WRITE(NOUT,122) RD(L), ZD(L), DD(L), HH(L)
C        END DO
  122   FORMAT(2X,4E15.7)
C
      !WRITE(NOUT,*) '  '
      !WRITE(NOUT,*) 'RESFW(L),L = 1,..,ND =', ND
      !WRITE(NOUT,*) '  '
      !WRITE(NOUT,722) (RESD(L),l=1,ND)
      !WRITE(NOUT,*) '  '
      !WRITE(NOUT,*) 'CURFW(L),L = 1,..,ND =', ND
      !WRITE(NOUT,*) '  '
      !WRITE(NOUT,722) (CURD(L),l=1,ND)
      !WRITE(NOUT,*) '  '
C
  722   FORMAT(2X,6E15.7)
C
       END IF   !!! ( for IF(NP.NE.0) )
C
       CLOSE (1)
C
       RETURN
       END
C
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C       VV(I)  - VERTICAL   (Z) SIZE  OF THIN PLATE
C       HH(I)  - HORISONTAL (R) SIZE  OF THIN PLATE
C
C
        SUBROUTINE TREBP(  NOUT, NTER, NINFW, NP,
     *                     KD, RP1, ZP1, RP2, ZP2, RESSEG, CURSEG,
     *                     ND, NTYD,
     *                     RD, ZD, VV, HH, RESD, CURD, 
     *                     R1, Z1, R2, Z2 )
C
       include 'double.inc'
C
        INTEGER    KD(1), NTYD(1)
        DIMENSION  RP1(1), ZP1(1), RP2(1), ZP2(1), RESSEG(1), CURSEG(1)
C
        DIMENSION  RD(1), ZD(1), VV(1), HH(1), RESD(1), CURD(1),
     *             R1(1), Z1(1), R2(1), Z2(1)
C
        SQRT(X) = DSQRT(X)
C
C***************************************************************
C
        write(fname,'(a,a)') path(1:kname),'blanbp.dat'
        open(NINFW,file=fname,form='formatted')
      !OPEN(NINFW, FILE='blanbp.dat')
            READ(NINFW,*)  RCEBP
            READ(NINFW,*)  NP
            !WRITE(NOUT,*) '  '
            !WRITE(NOUT,*) '***********************************'
            !WRITE(NOUT,*) 'READING FROM FILE "BlanBP.DAT" ===>'
            !WRITE(NOUT,*) '  '
            !WRITE(NOUT,*) 'One-turn wall resistance (micro*Om) =',RCEBP
            !WRITE(NOUT,*) 'Number of "basic wall segments"     =',NP
       ND = 0
C-----------------------------------------------------------------------
       IF( NP.NE.0 )  THEN
C
         DO 33 L=1,NP
   33    READ(NINFW,*) KD(L), RP1(L), ZP1(L), RP2(L), ZP2(L), RESSEG(L),
     *                                                        CURSEG(L)
C  
          DO 1 L=1,NP
             I = ND + 1
             DELR  = (RP2(L) - RP1(L)) / KD(L)
             DELZ  = (ZP2(L) - ZP1(L)) / KD(L)
             RD(I) = RP1(L) + 0.5D0*DELR
             ZD(I) = ZP1(L) + 0.5D0*DELZ
             VV(I) = DELZ
             HH(I) = DELR
             DDI   = SQRT( DELR**2 + DELZ**2 )
             R1(I) = RP1(L)
             Z1(I) = ZP1(L)
             R2(I) = RP1(L) + DELR
             Z2(I) = ZP1(L) + DELZ
C
             SSSFW = DDI/RD(I)
             IF( KD(L) .GT. 1 ) THEN
                  DO 2 K=1,KD(L)-1
                     RD(I+K) = RD(I+K-1) + DELR
                     ZD(I+K) = ZD(I+K-1) + DELZ
                     VV(I+K) = DELZ
                     HH(I+K) = DELR
                     R1(I+K) = R1(I+K-1) + DELR
                     Z1(I+K) = Z1(I+K-1) + DELZ
                     R2(I+K) = R2(I+K-1) + DELR
                     Z2(I+K) = Z2(I+K-1) + DELZ
                     SSSFW   = SSSFW + DDI/RD(I+K)
    2             CONTINUE
             END IF
C
             SSSFW = SSSFW * RESSEG(L)
C
		   DO 7 K=1,KD(L)
                RESD(I+K-1) = SSSFW * RD(I+K-1)/DDI
                NTYD(I+K-1) = 1
                CURD(I+K-1) = CURSEG(L) / KD(L)
    7        CONTINUE
C
             ND = ND + KD(L)
    1     CONTINUE
C-----------------------------------------------------------------------
C
          IF( RCEBP.GT.0 )  THEN
              SSSFW = 0.0D0
              DO 333 L=1,ND
  333            SSSFW = SSSFW + SQRT( HH(L)**2 + VV(L)**2 )/RD(L)
              SSSFW = SSSFW * RCEBP
			DO 77 L=1,ND
   77            RESD(L) = SSSFW * RD(L) / SQRT( HH(L)**2 + VV(L)**2 )
          END IF
C-----------------------------------------------------------------------
C--- COMPUTING OF THE ONE-TURN RESISTANCE OF BlanBP.DAT (in micro*Om)
C
          RESCE = 0.D0
          DO 8 I=1,ND
  8          RESCE = RESCE + 1.D0 / RESD(I)
             RESCE = 1.D0 / RESCE
        !WRITE(NOUT,*) '  '
        !WRITE(NOUT,*) 'Control one-turn wall resistance (micro*Om)'
        !WRITE(NOUT,*) '        RESCE = ', RESCE
C-----------------------------------------------------------------------
C
          !WRITE(NOUT,*) '  '
          !WRITE(NOUT,*) 'RBP(L), ZBP(L), DBP(L), RESBP(L), CURBP(L)'
          !WRITE(NOUT,*) 'L = 1,..,ND =', ND
C
          DO 22 L=1,ND
	       DD = SQRT( HH(L)**2 + VV(L)**2 )
           !WRITE(NOUT,*) RD(L), ZD(L), DD, RESD(L), CURD(L)
   22        continue
C
          !WRITE(NOUT,*) '  '
          !WRITE(NOUT,*) 'R1(L), Z1(L), R2(L), Z2(L), RESBP(L), CURBP(L)'
          !WRITE(NOUT,*) 'L = 1,..,ND =', ND
C
          DO 34 L=1,ND
           !WRITE(NOUT,*) R1(L), Z1(L), R2(L), Z2(L), RESD(L), CURD(L)
   34        continue
          !WRITE(NOUT,*) '  '
C
       END IF   !!! ( for IF(NP.NE.0) )
C
       CLOSE (NINFW)
C
       RETURN
       END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C       VV(I)  - VERTICAL   (Z) SIZE  OF THIN PLATE
C       HH(I)  - HORISONTAL (R) SIZE  OF THIN PLATE
C
C
        SUBROUTINE TREVV(  NOUT, NTER, NINFW, NP,
     *                     KD, RP1, ZP1, RP2, ZP2, RESSEG, CURSEG, 
     *                     ND, NTYD,
     *                     RD, ZD, VV, HH, RESD, CURD,  
     *                     R1, Z1, R2, Z2 )
C
       include 'double.inc'
C
        INTEGER    KD(*), NTYD(*)
        DIMENSION  RP1(*), ZP1(*), RP2(*), ZP2(*), RESSEG(*), CURSEG(*)
C
        DIMENSION  RD(*), ZD(*), VV(*), HH(*), RESD(*), CURD(*),
     *             R1(*), Z1(*), R2(*), Z2(*)
C
        SQRT(X) = DSQRT(X)
C
C***************************************************************
C          
        write(fname,'(a,a)') path(1:kname),'vacves.dat'
        open(NINFW,file=fname,form='formatted')
      !OPEN(NINFW, FILE='vacves.dat')
            READ(NINFW,*)  RCEVV
            READ(NINFW,*)  NP
            !WRITE(NOUT,*) '  '
            !WRITE(NOUT,*) '***********************************'
            !WRITE(NOUT,*) 'READING FROM FILE "VacVes.DAT" ===>'
            !WRITE(NOUT,*) '  '
            !WRITE(NOUT,*) 'One-turn wall resistance (micro*Om) =',RCEVV
            !WRITE(NOUT,*) 'Number of "basic wall segments"     =',NP
C
       ND = 0
C-----------------------------------------------------------------------
       IF( NP.NE.0 )  THEN
C
         DO 33 L=1,NP
   33    READ(NINFW,*) KD(L), RP1(L), ZP1(L), RP2(L), ZP2(L), RESSEG(L),
     *                                                        CURSEG(L)
C  
          DO 1 L=1,NP
             I = ND + 1
             DELR  = (RP2(L) - RP1(L)) / KD(L)
             DELZ  = (ZP2(L) - ZP1(L)) / KD(L)
             RD(I) = RP1(L) + 0.5D0*DELR
             ZD(I) = ZP1(L) + 0.5D0*DELZ
             VV(I) = DELZ
             HH(I) = DELR
             DDI   = SQRT( DELR**2 + DELZ**2 )
             R1(I) = RP1(L)
             Z1(I) = ZP1(L)
             R2(I) = RP1(L) + DELR
             Z2(I) = ZP1(L) + DELZ
C
             SSSFW = DDI/RD(I)
             IF( KD(L) .GT. 1 ) THEN
                  DO 2 K=1,KD(L)-1
                     RD(I+K) = RD(I+K-1) + DELR
                     ZD(I+K) = ZD(I+K-1) + DELZ
                     VV(I+K) = DELZ
                     HH(I+K) = DELR
                     R1(I+K) = R1(I+K-1) + DELR
                     Z1(I+K) = Z1(I+K-1) + DELZ
                     R2(I+K) = R2(I+K-1) + DELR
                     Z2(I+K) = Z2(I+K-1) + DELZ
                     SSSFW   = SSSFW + DDI/RD(I+K)
    2             CONTINUE
             END IF
C
             SSSFW = SSSFW * RESSEG(L)
C
		   DO 7 K=1,KD(L)
                RESD(I+K-1) = SSSFW * RD(I+K-1)/DDI
                NTYD(I+K-1) = 1
                CURD(I+K-1) = CURSEG(L) / KD(L)
    7        CONTINUE
C
             ND = ND + KD(L)
    1     CONTINUE
C-----------------------------------------------------------------------
C
          IF( RCEVV.GT.0 )  THEN
              SSSFW = 0.0D0
              DO 333 L=1,ND
  333            SSSFW = SSSFW + SQRT( HH(L)**2 + VV(L)**2 )/RD(L)
              SSSFW = SSSFW * RCEVV
			DO 77 L=1,ND
   77            RESD(L) = SSSFW * RD(L) / SQRT( HH(L)**2 + VV(L)**2 )
          END IF
C-----------------------------------------------------------------------
C--- COMPUTING OF THE ONE-TURN RESISTANCE OF VacVes.DAT (in micro*Om)
C
          RESCE = 0.D0
          DO 8 I=1,ND
  8          RESCE = RESCE + 1.D0 / RESD(I)
             RESCE = 1.D0 / RESCE
        !WRITE(NOUT,*) '  '
        !WRITE(NOUT,*) 'Control one-turn wall resistance (micro*Om)'
        !WRITE(NOUT,*) '        RESCE = ', RESCE
C-----------------------------------------------------------------------
C
          !WRITE(NOUT,*) '  '
          !WRITE(NOUT,*) 'RVV(L), ZVV(L), DVV(L), RESVV(L), CURVV(L)'
          !WRITE(NOUT,*) 'L = 1,..,ND =', ND
C
          DO 22 L=1,ND
             DD = SQRT( HH(L)**2 + VV(L)**2 )
           !WRITE(NOUT,*) RD(L), ZD(L), DD, RESD(L), CURD(L)
   22        continue
C
          !WRITE(NOUT,*) '  '
          !WRITE(NOUT,*) 'R1(L), Z1(L), R2(L), Z2(L), RESVV(L), CURVV(L)'
          !WRITE(NOUT,*) 'L = 1,..,ND =', ND
C
          DO 34 L=1,ND
           !WRITE(NOUT,*) R1(L), Z1(L), R2(L), Z2(L), RESD(L), CURD(L)
   34        continue
          !WRITE(NOUT,*) '  '
C
       END IF   !!! ( for IF(NP.NE.0) )
C
       CLOSE (NINFW)
C
       RETURN
       END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        SUBROUTINE SEGCUR( NOUT, NTER, NSTART, NSEG, KD, BBB, PJK, RES,
     *                     CURSEG, RESSEG )
C
       include 'double.inc'
C
        INTEGER    KD(*)
        DIMENSION  PJK(*), RES(*)
	  DIMENSION  CURSEG(*), RESSEG(*)
C
        L = NSTART
        DO 1 I=1,NSEG
	     IF(I.GT.1) L=L+KD(I-1)
           CRTSEG = 0.D0
           RSTSEG = 0.D0
           DO 2 J=1,KD(I)
	        CRTSEG = CRTSEG +     PJK(L+J)
	        RSTSEG = RSTSEG + BBB/RES(L+J)
    2      CONTINUE
           CURSEG(I) = CRTSEG
           RESSEG(I) = 1.D0 / RSTSEG
    1   CONTINUE
C
      !WRITE(NOUT,*) '    '
      !WRITE(NOUT,*) 'FW SEGMENT EDDY CURRENTS (mega*Am) = CFWSEG(I):'
      !WRITE(NOUT,*) 'I=1,...,NSEGFW = ', NSEG
C      WRITE(NOUT,101) ( CURSEG(I), I=1,NSEG )
      !!WRITE(NOUT,*) 'FW SEGMENT RESISTANCES  (micro*Om) = RFWSEG(I):'
      !WRITE(NOUT,*) 'I=1,...,NSEGFW = ', NSEG
C      WRITE(NOUT,101) ( RESSEG(I), I=1,NSEG )
      !WRITE(NOUT,*) '    '
C
  101  FORMAT(2X,5E14.7)
C
       RETURN
       END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




