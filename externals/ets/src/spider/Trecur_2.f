C***********************************************************************
C    SUBROUTINE  FOR  READING  AND  TREATMENT
C    PFC  PARAMETERS  ( THEIR POSITIONS, CURRENTS, RESISTANCE,
C                       NUMBER OF TURNS, NUMBER OF DIVIDING,
C                       EQUIVALENT SIGNS )
C***********************************************************************
C  OUTPUT DATE:
C  ----------
C   NPFC      - NUMBER OF PF-COILS
C
C   PFCUR1(L) - VALUES OF TOTAL CURRENT OF PFC CROSS-SECTION (in MA)
C               L = 1,2,...,NPFC
C   NEPFC(L)  - EQUIVALENT "SIGNS"  OF PF-COILS
C               L = 1,2,...,NPFC
C   WEPFC(L)  - EQUIVALENT "WEIGHTS" OF PF-COILS
C               L = 1,2,...,NPFC
C   NTURN(L)  - NUMBER OF REAL TURNS  OF PF-COILS
C               L = 1,2,...,NPFC
C   NDIV(L)   - NUMBER OF DIVIDING TURNS OF PF-COILS
C               L = 1,2,...,NPFC
C   NLOC(L)   - LOCATION ARRAY OF DIVIDING TURNS OF PF-COILS
C
C               !!! L = 0,1,2,...,NPFC !!! :
C               NLOC(0) = 0, NLOC(1) = NDIV(1),
C               NLOC(2) = NDIV(1) + NDIV(2),...,
C               NLOC(NPFC) = NDIV(1) + NDIV(2) +...+ NDIV(NPFC) = NCPFC
C  ---------------------------------------------------------------------
C   NEQUI     - NUMBER OF EQUIVALENT PF-COIL GROUPS ( NEQUI .LE. NPFC )
C
C   PFRES(L)  - RESISTANCE OF EQUIVALENT PF-COIL GROUPS; in [micro*Ohm]
C               L = 1,2,...,NEQUI
C   PFVOL1(L) - VOLTAGE    OF EQUIVALENT PF-COIL GROUPS; in [V]
C               L = 1,2,...,NEQUI
C   PFCEQW(L) - CURRENT    OF EQUIVALENT PF-COIL GROUPS; in [MA]
C               L = 1,2,...,NEQUI
C  ---------------------------------------------------------------------
C   NCPFC    - NUMBER OF PFC-CURRENTS (AFTER DIVIDING)
C
C   PC(L)    - VALUES (IN MA) OF PFC-CURRENTS (AFTER DIVIDING)
C              L=1,...,NCPFC.
C   RI(L),ZI(L) - CILINDER COORDINATES (IN M) OF PFC-CURRENTS
C                 (AFTER DIVIDING)  L = 1,2,...,NCPFC
C   NTYPE(L) - NUMBER OF TYPE OF PFC-CURRENT CROSS-SECTIONS
C              (AFTER DIVIDING)  L = 1,2,...,NCPFC
C   VERS(L)  - VERTICAL   SIZE OF PFC-CURRENT CROSS-SECTIONS
C              (AFTER DIVIDING)  L = 1,2,...,NCPFC
C   HORS(L)  - HORIZONTAL SIZE OF PFC-CURRENT CROSS-SECTIONS
C              (AFTER DIVIDING)  L = 1,2,...,NCPFC
C   NECON(L) - EQUIVALENT "SIGNS"  OF PF-CURRENTS
C              (AFTER DIVIDING)  L = 1,2,...,NCPFC
C   WECON(L) - EQUIVALENT "WEIGHTS" OF PF-CURRENTS
C              (AFTER DIVIDING)  L = 1,2,...,NCPFC
C
C***********************************************************************
C
      SUBROUTINE TRECUR( NCPFC, RI, ZI, PC, NTYPE, NECON, WECON,
     *                   HORS, VERS, NPRI, NTER )
C
       include 'double.inc'
C
         INCLUDE 'prm.inc'
         INCLUDE 'comevl.inc'
C
      DIMENSION  RI(*), ZI(*), PC(*), HORS(*), VERS(*), WECON(*)
      INTEGER    NTYPE(*), NECON(*)
C
C WORK ARRAYS
      DIMENSION  RS(700),  ZS(700), DELZ(300), DELR(300)
      INTEGER    NDW(300), NDH(300)
C
C***********************************************************************
C
      NINP  = 1
C
      !WRITE(NPRI,*) '                                      '
      !WRITE(NPRI,*) '**************************************'
      !WRITE(NPRI,*) '*** ENTRANCE OF SUBROUT. TRECUR ***'
C-------------------------------------
        write(fname,'(a,a)') path(1:kname),'coilres.dat'
        open(ninp,file=fname,form='formatted')
      !open(ninp, file='coilres.dat')
C
         READ(NINP,*)  NEQUI
C
         !WRITE(NPRI,*) '**************************************'
         !WRITE(NPRI,*) '  NUMBER OF EQUIV. COIL GROUPS NEQUI =', NEQUI
         !WRITE(NPRI,*) '**************************************'
C
         DO 288 I=1,NEQUI
            READ(NINP,*)       L
            READ(NINP,*) PFRES(L)
C+++                     PFVOL1(L) = 0.d0   now it's located below
  288    CONTINUE
C
      close(ninp)
C-------------------------------------
        write(fname,'(a,a)') path(1:kname),'coil.dat'
        open(1,file=fname,form='formatted')
      !open(1, file='coil.dat')
      !open(ninp, file='coil.dat')
C
        ! READ(NINP,*)  NPFC, KEYCUR
         READ(1,*)  NPFC, KEYCUR


c	print *,' npfc ==',npfc
c	read (*,*)

C
         !WRITE(NPRI,*) '**************************************'
         !WRITE(NPRI,*) '  NUMBER OF PF-COILS            NPFC =', NPFC
         !WRITE(NPRI,*) '  KEY OF CURRENT DEFINITION   KEYCUR =', KEYCUR
         !WRITE(NPRI,*) '**************************************'
C
   11  FORMAT(/,2X,I3,2X,5E14.7)
   16  FORMAT(7X,I3,11X,2E14.7,E12.4,2X,I7)
   10  FORMAT(/,3X,'NUM.',2X,'RC (M)',8X,'ZC (M)',8X,'WC (M)',
     *          8X,'HC (M)', 8X,'CURRENT (MA)')
   15  FORMAT(  9X,'NDI ',10X,'AWC',11X,'AHC',11X,'NTURN  ',
     *          7X,' PF-COIL SIGN ')
C
      !WRITE(NPRI,*) '**************************************'
      !WRITE(NPRI,*) '*** PFC INPUT INFORMATION ***'
      !WRITE(NPRI,10)
      !WRITE(NPRI,15)
C
C*****************************************************
C
       NCPFC   = 0
       NLOC(0) = 0
C
       DO 2 I=1,NPFC
C
       !READ(NINP,*) NDI
       READ(1,*) NDI
       !READ(NINP,*) RC,ZC, WC,HC, AWC,AHC, PFCUR1(I), NTURN(I), NEPFC(I)
       READ(1,*) RC,ZC, WC,HC, AWC,AHC, PFCUR1(I), NTURN(I), NEPFC(I)
                      NTIPE = 2
C------------------------------------------------------
C EXPERIMENT FOR TCV PF FAST COILS (numbers 24,...,29)
C+++   IF( I.GE.24 )  NTIPE = 3
C------------------------------------------------------
C
       IF( KEYCUR .EQ. 1 )  PFCUR1(I) = PFCUR1(I) * NTURN(I)
C
          !WRITE(NPRI,11) I, RC, ZC, WC, HC, PFCUR1(I)
          !WRITE(NPRI,16) NDI, AWC, AHC, NTURN(I), NEPFC(I)
C
          CALL DIVPAR( RC, ZC, WC, HC, AWC, AHC, PFCUR1(I), NDI,
     *                 NDIVRE, NDIVW, NDIVH, RS, ZS, PS, VERC, HORC )
C
          NDIV(I)   = NDIVRE
          WEPFC(I)  = NTURN(I)  / NDIV(I)
C
          PFCUR2(I) = PFCUR1(I) 
          PFCW1(I)  = PFCUR1(I) / NTURN(I)
          PFCD1(I)  = PFCUR1(I) / NDIV(I)
C
          DO 3 J=1,NDIVRE
                   L  = NCPFC + J
                RI(L) = RS(J)
                ZI(L) = ZS(J)
                PC(L) = PFCD1(I)
              HORS(L) = HORC
              VERS(L) = VERC
             NTYPE(L) = NTIPE
             NECON(L) = NEPFC(I)
             WECON(L) = WEPFC(I)
    3     CONTINUE
C
          NCPFC     = NCPFC + NDIVRE
          NLOC(I)   = NCPFC
C
          NDW(I)    = NDIVW
          NDH(I)    = NDIVH
          DELZ(I)   = VERC
          DELR(I)   = HORC
C
    2  CONTINUE
    
!      open(1,file='pfc_nturn.wr',form='formatted')
!       do i=1,NPFC
!        write(1,'(e13.5)' ) PFCUR2(I) 
!       enddo
!       pause 'pfc_nturn.wr'
    
C
       !close(ninp)
       close(1)
C
       DO 4 I=1,NEQUI

          DO 5 J=1,NPFC
C
             IF( NEPFC(J) .EQ. I )  THEN
                 PFCEQW(I) = PFCW1(J)
                 GO TO 5
              END IF
C
    5     CONTINUE
C
C            [Volt] = [micro*Ohm]*[mega*Amper]
C
C+++      PFVOL1(I) = 0.d0
          PFVOL1(I) = PFRES(I)*PFCEQW(I)

    4  CONTINUE
C
C*****************************************************
C
        !WRITE(NPRI,*) '**************************************'
	!WRITE(NPRI,*) 'EQ.SIGN NUM. =>   PFRES(L),  PFVOL1(L),  PFCEQW(L)'
 4444 FORMAT(2X,I8,4X,E13.6,5X,E13.6,5X,E13.6)
C
	  !DO 1937 L=1,NEQUI
	!WRITE(NPRI,4444) L, PFRES(L), PFVOL1(L), PFCEQW(L)
 !1937     CONTINUE
C---------------------------------------------------------------------
C
   42  FORMAT(/,2X,'PFC NUM.',4X,'PFC SIGN',6X,'PFC WEIGHT  ',
     *          5X,'TURN CURRENT' )
   44  FORMAT(2X,I8,4X,I8,5X,E13.6,5X,E13.6)
C
      !WRITE(NPRI,*) '**************************************'
      !WRITE(NPRI,*) '*** PFC TURN CURRENT INFORMATION ***'
      !WRITE(NPRI,42)
C
      !DO 41 I=1,NPFC
         !WRITE(NPRI,44) I, NEPFC(I), WEPFC(I), PFCW1(I)
   !41 CONTINUE
C*****************************************************
C
   12  FORMAT(/,2X,'NUM.',2X,'NDIVRE',2X,'NDIVW',2X,'NDIVH',2X,
     *          'CELL CURRENT',4X,'CELL Z-SIZE',4X,'CELL R-SIZE')
   14  FORMAT(2X,I3,2X,I6,2X,I5,2X,I5,2X,E13.5,2X,E13.5,2X,E13.5)
C
      !WRITE(NPRI,*) '**********************************************'
      !WRITE(NPRI,*) '*** DIVIDING PROCEDURE RESULTS INFORMATION ***'
      !WRITE(NPRI,12)
      !WRITE(NTER,*) '**********************************************'
      !WRITE(NTER,*) '*** DIVIDING PROCEDURE RESULTS INFORMATION ***'
      !WRITE(NTER,12)
C
!      DO 444 I=1,NPFC
!         WRITE(NPRI,14) I, NDIV(I), NDW(I), NDH(I), PFCD1(I),
!     *                     DELZ(I), DELR(I)
!         WRITE(NTER,14) I, NDIV(I), NDW(I), NDH(I), PFCD1(I),
!     *                     DELZ(I), DELR(I)
!  444 CONTINUE
C
      !WRITE(NTER,*) '**************************************'
      !WRITE(NTER,*) '*** NUMBER OF MAT. PFC-CURR. NCPFC =', NCPFC
      !WRITE(NTER,*) '                                      '
      !WRITE(NPRI,*) '**************************************'
      !WRITE(NPRI,*) '*** NUMBER OF MAT. PFC-CURR. NCPFC =', NCPFC
      !WRITE(NPRI,*) '                                      '
C
  111  FORMAT( 2X, 10I5 )
  222  FORMAT( 2X, 7E11.4 )
       !WRITE(NPRI,*) '***** NECON(I), I=1,NCPFC =', NCPFC,'*****'
C+++   WRITE(NPRI,111) ( NECON(I), I=1,NCPFC )
       !WRITE(NPRI,*) '***** WECON(I), I=1,NCPFC =', NCPFC,'*****'
C+++   WRITE(NPRI,222) ( WECON(I), I=1,NCPFC )
C*****************************************************
C
      !WRITE(NPRI,*) '**************************************'
      !WRITE(NPRI,*) '*** EXIT FROM SUBROUT. TRECUR ***'
      !WRITE(NPRI,*) '**************************************'
      !WRITE(NPRI,*) '                                      '
C
      RETURN
      END
C**************************************************************
C        SUBROUTINE FOR DIVIDING OF PARALLELOGRAM
C                ( FOR PFC CROSS-SECTION )
C**************************************************************
C  INPUT DATE:
C  ----------
C   RC,ZC - CILINDER COORDINATES OF CENTER OF PARALLELOGRAM
C   WC    - PROJECTION OF THE FIRST SIDE OF PARALLELOGRAM
C           ON AXIS "R" (IN METER)
C   HC    - PROJECTION OF THE SECOND SIDE OF PARALLELOGRAM
C           ON AXIS "Z" (IN METER)
C   AWC   - ANGLE BETWEEN THE FIRST SIDE OF PARAL. AND
C           AXIS "R" (IN DEGREES, IT MUST NOT BE EQUAL 90 ,
C                                 IT MUST BE :  -90 < AWC < 90 )
C   AHC   - ANGLE BETWEEN THE SECOND SIDE OF PARAL. AND
C           AXIS "R" (IN DEGREES, IT MUST NOT BE EQUAL 0 ,
C                                 IT MUST BE :  0 < AHC < 180 )
C   CURC  - CURRENT OF PARALLELOGRAM (IN MA)
C   NDIV  - APPROXIMATE NUMBER OF CELLS OF DIVIDING:
C           NDIV=0 - A SPECIAL CASE: AUTOMATICALLY NDIVRE=1,
C                    RS(1)=RC, ZS(1)=ZC, PS=CURC
C           IF NDIV > 0 THEN WE HAVE THE MOST TOTAL ALGORITHM OF
C                            DIVIDING
C           IF NDIV < 0 THEN NDIVW AND NDIVH ARE CUT OFF BY ABS(NDIV)
C
C  OUTPUT DATE:
C  ----------
C   NDIVRE      - REAL NUMBER OF CELLS OF DIVIDING ( = NDIVW*NDIVH )
C   NDIVW       - NUMBER OF DIVIDING OF THE FIRST  SIDE OF PARAL.
C   NDIVH       - NUMBER OF DIVIDING OF THE SECOND SIDE OF PARAL.
C   RS(L),ZS(L) - CILINDER COORDINATES OF CENTERS OF CELLS OF DIVIDING
C                 L = 1,2,...,NDIVRE ! (IN METER)
C   PS          - CURRENT OF EVERY CELL OF DIVIDING  (IN MA)
C   VERS - VERTICAL (OR LINEAR, OR RADIUS) SIZE OF CELL CROSS-S.
C   HORS - HORIZONTAL SIZE OF CELL CROSS-SECTION
C
C**************************************************************
C
        SUBROUTINE DIVPAR( RC, ZC, WC, HC, AWC, AHC, CURC, NDIV,
     *                     NDIVRE, NDIVW, NDIVH, RS, ZS, PS,
     *                     VERS, HORS )
C
C
       include 'double.inc'
C
        DIMENSION  RS(1), ZS(1)
C
C**************************************************************
                SIN(X) = DSIN(X)
                COS(X) = DCOS(X)
               ATAN(X) = DATAN(X)
               SQRT(X) = DSQRT(X)
C**************************************************************
C
       IF(NDIV.EQ.0) THEN
          NDIVW  = 1
          NDIVH  = 1
          NDIVRE = 1
          RS(1)  = RC
          ZS(1)  = ZC
          PS     = CURC
          VERS   = HC
          HORS   = WC
          RETURN
       END IF
C***************************************
C
       IF(AHC.LT.0) AHC = AHC + 180.
C
       IF(NDIV.GE.0) THEN
          NDIVA =  NDIV
       ELSE
          NDIVA = -NDIV
       END IF
C***************************************
C   PARAMETERS OF PARALLELOGRAM
C
       XX    = 1.
       PI    = 4.*ATAN(XX)
C
       AWCR  = AWC * PI /180.
       AHCR  = AHC * PI /180.
C
       R0    = RC - 0.5*( WC + HC * COS(AHCR)/SIN(AHCR) )
       Z0    = ZC - 0.5*( HC + WC * SIN(AWCR)/COS(AWCR) )
C
       WSIZE = WC / COS(AWCR)
       HSIZE = HC / SIN(AHCR)
C
       WR    = WC
       WZ    = WC * SIN(AWCR) / COS(AWCR)
       HR    = HC * COS(AHCR) / SIN(AHCR)
       HZ    = HC
C***************************************
C   CALCULATION  NDIVW, NDIVH, NDIVRE, PS
C
       SW    = SQRT( NDIVA*WSIZE/HSIZE )
       SH    = SQRT( NDIVA*HSIZE/WSIZE )
       SW    = SW + 0.5
       SH    = SH + 0.5
C
       NDIVW = IDINT(SW)
       NDIVH = IDINT(SH)
C
       IF(NDIVW.EQ.0)  NDIVW = 1
       IF(NDIVH.EQ.0)  NDIVH = 1
C
       IF((NDIV.LT.0).AND.(NDIVW.GT.NDIVA)) NDIVW = NDIVA
       IF((NDIV.LT.0).AND.(NDIVH.GT.NDIVA)) NDIVH = NDIVA
C
       NDIVRE = NDIVW * NDIVH
       PS     = CURC / NDIVRE
C***************************************
C   CALCULATION  RS(L), ZS(L) : L = 1,2,...,NDIVRE
C
       WR  = WR / NDIVW
       WZ  = WZ / NDIVW
       HR  = HR / NDIVH
       HZ  = HZ / NDIVH
C
       HORS = WR
       VERS = HZ
C
       RS(1) = R0 + 0.5*(WR + HR)
       ZS(1) = Z0 + 0.5*(WZ + HZ)
C
      DO 1 I=1,NDIVW
      DO 1 J=1,NDIVH
         L     = (I-1)*NDIVH + J
         RS(L) = RS(1) + (I-1)*WR + (J-1)*HR
         ZS(L) = ZS(1) + (I-1)*WZ + (J-1)*HZ
    1 CONTINUE
C***************************************
C
        RETURN
        END







