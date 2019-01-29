
C-----------------------------------------------------------------------

         SUBROUTINE GRAPHE(PXMAX,PXMIN,PYMAX,PYMIN,PX,PY,KN,PXOR,PYOR,
     S   PXLE,PYLE,KCTX,KCTY,KTITL,KROX,KROY)
C
C  KROX = 0 FIRST VALUE OF THE X-AXIS IS NOT A ROUNDED VALUE
C         1 FIRST VALUE OF THE X-AXIS IS A ROUNDED VALUE
C  MOD(KROY,10) SAME BUT FOR THE Y AXIS
C  KROY/10 = 0: DO NOT ALLOW FOR ASINH Y-AXIS
C  KROY/10 = 1: DO     ALLOW FOR ASINH Y-AXIS
C
C
         DIMENSION PX(KN),PY(KN)
         CHARACTER KCTX*(*),KCTY*(*),KTITL*(*),ZYTITLE*80
c.......................................................................
C
CL       0. Compute length of character strings
C
         DO I=1,80
           IF (KCTX(I:I) .EQ. '$') GO TO 10
         ENDDO
 10      CONTINUE
         KCTXLE=I-1

         DO I=1,80
           IF (KCTY(I:I) .EQ. '$') GO TO 11
         ENDDO
 11      CONTINUE
         KCTYLE=I-1

         DO I=1,80
           IF (KTITL(I:I) .EQ. '$') GO TO 12
         ENDDO
 12      CONTINUE
         KTITLE=I-1
C
CL       1. DETERMINE EVENTUAL ROUNDOFFS FOR X/YMAX/MIN
C
         IF (PYMAX.EQ.PYMIN.AND.PYMIN.EQ.0.0) THEN
           PYMIN = -1.0
           PYMAX =  1.0
         ENDIF
         IF (PYMAX.EQ.PYMIN.AND.PYMIN.NE.0.0) THEN
           IF (PYMIN.LT.0.0) THEN
             PYMAX = 0.0
             PYMIN = 2.*PYMIN
           ENDIF
           IF (PYMIN.GT.0.0) THEN
             PYMAX = 2.*PYMAX
             PYMIN = 0.0
           ENDIF
         ENDIF
         ZXMIN = PXMIN
         ZXMAX = PXMAX
         ZYMIN = PYMIN
         ZYMAX = PYMAX
c%OS         IF (KROX .EQ. 1) THEN
c%OS           CALL RNDMNLG(ZXMIN)
c%OS           CALL RNDMXLG(ZXMAX)
c%OS         ENDIF
c%OS         IF (KROY .EQ. 1) THEN
c%OS           CALL RNDMNLG(ZYMIN)
c%OS           CALL RNDMXLG(ZYMAX)
c%OS         ENDIF
C
         ZYTITLE = KCTY(1:KCTYLE+1)
         KYTITLE = KCTYLE
         IF (KCTYLE .EQ. 0) THEN
           ZYTITLE = KTITL(1:KTITLE+1)
           KYTITLE = KTITLE
         ENDIF
C
         ZBETA = 15.
         ZSUM = 0.0
         DO I=1,KN
           ZSUM = ZSUM + ABS(PY(I))
         ENDDO
         ZSUM = ZSUM / FLOAT(KN)
         IF (ZSUM.GE.ZYMAX/ZBETA .OR. ZYTITLE(1:2).EQ.'Q$' .OR.
     +     KROY/10.EQ.0) THEN
           CALL GRAPHMB(PX,PY,KN,0.1*PXOR,0.1*PXLE,0.1*PYOR,0.1*PYLE
     +     ,0.2,KCTX,ZYTITLE,KCTXLE,KYTITLE,1.0,0,0,ZXMIN,ZXMAX,ZYMIN,
     +     ZYMAX)
         ELSE
           print *,' using asinh for ',ZYTITLE(1:KYTITLE)
           print *,ZYTITLE(1:KYTITLE),' zsum, zymax/zbeta= ',zsum,
     +     zymax/zbeta
           CALL GLASHMB(PX,PY,KN,0.1*PXOR,0.1*PXLE,0.1*PYOR,0.1*PYLE
     +     ,0.2,KCTX,ZYTITLE,KCTXLE,KYTITLE,1.0,0,0,ZXMIN,ZXMAX,ZYMIN,
     +     ZYMAX)
         ENDIF
C
         RETURN
         END
        SUBROUTINE GRAPHEN(PXMAX,PXMIN,PYMAX,PYMIN,PX,PY,MDKN,KN,
     S  KPLOT,PXOR,PYOR,PXLE,PYLE,KCTX,KCTY,KCTITL,KROX,KROY)
C
C     PLOT N CURVES: PY(I,N)
C
C  KROX = 0 FIRST VALUE OF THE X-AXIS IS NOT A ROUNDED VALUE
C         1 FIRST VALUE OF THE X-AXIS IS A ROUNDED VALUE
C  KROY   SAME BUT FOR THE Y AXIS
C
C     KCTY IS DUMMY (NOT USED)
C
         DIMENSION PX(KN),PY(MDKN,KPLOT), KNTITL(11)
         CHARACTER*(*) KCTX,KCTY
         CHARACTER*80 KCTITL(KPLOT+1)
c.......................................................................
C
CL       0. Compute length of character strings
C
         DO I=1,80
           IF (KCTX(I:I) .EQ. '$') GO TO 10
         ENDDO
 10      CONTINUE
         KCTXLE=I-1

         DO IPLOT=1,KPLOT+1
           DO I=1,80
             IF (KCTITL(IPLOT)(I:I) .EQ. '$') GO TO 12
           ENDDO
 12        CONTINUE
           KNTITL(IPLOT)=I-1
         ENDDO
C
CL       1. DETERMINE EVENTUAL ROUNDOFFS FOR X/YMAX/MIN
C
         ZXMIN = PXMIN
         ZXMAX = PXMAX
         ZYMIN = PYMIN
         ZYMAX = PYMAX
         IF (ZYMAX.EQ.ZYMIN.AND.ZYMIN.EQ.0.0) THEN
           ZYMIN = -1.0
           ZYMAX =  1.0
         ENDIF
         IF (ZYMAX.EQ.ZYMIN.AND.ZYMIN.NE.0.0) THEN
           IF (ZYMIN.LT.0.0) THEN
             ZYMAX = 0.0
             ZYMIN = 2.*ZYMIN
           ENDIF
           IF (ZYMIN.GT.0.0) THEN
             ZYMAX = 2.*ZYMAX
             ZYMIN = 0.0
           ENDIF
         ENDIF
c%OS         IF (KROX .EQ. 1) THEN
c%OS           CALL RNDMNLG(ZXMIN)
c%OS           CALL RNDMXLG(ZXMAX)
c%OS         ENDIF
c%OS         IF (KROY .EQ. 1) THEN
c%OS           CALL RNDMNLG(ZYMIN)
c%OS           CALL RNDMXLG(ZYMAX)
c%OS         ENDIF
C
         ZBETA = 15.
         ZSUM = 0.0
         DO I=1,KN
           ZSUM = ZSUM + ABS(PY(I,1))
         ENDDO
         ZSUM = ZSUM / FLOAT(KN)
         IF (ZSUM.GE.ZYMAX/ZBETA .OR. KCTITL(KPLOT+1)(1:2).EQ.'Q$') THEN
           CALL GRAPNMB(PX,PY,MDKN,KN,KPLOT,0.1*PXOR,0.1*PXLE,0.1*PYOR,
     +       0.1*PYLE,0.2,KCTX,KCTITL,KCTXLE,KNTITL,1.0,0,0,ZXMIN,ZXMAX,
     +       ZYMIN,ZYMAX)
         ELSE
           print *,'using asinh for ',KCTITL(KPLOT+1)(1:KNTITL(KPLOT+1))
           print *,KCTITL(KPLOT+1)(1:KNTITL(KPLOT+1)),
     +       ' zsum, zymax/zbeta= ',zsum,zymax/zbeta
           CALL GLASNMB(PX,PY,MDKN,KN,KPLOT,0.1*PXOR,0.1*PXLE,0.1*PYOR,
     +       0.1*PYLE,0.2,KCTX,KCTITL,KCTXLE,KNTITL,1.0,0,0,ZXMIN,ZXMAX,
     +       ZYMIN,ZYMAX)
         ENDIF
C
         RETURN
         END


         SUBROUTINE GRAPHMB(PX,PY,KN,PX0,PDX,PY0,PDY,PHCHAR,PXIBCD,
     +                      PYIBCD,KX,KY,PSIZLN,KCADRE,KGRID,
     +                      PXMIN,PXMAX,PYMIN,PYMAX)
C        ***************************************************************
C
C   WARNING: PX0,PY0,PDX,PDY REFER TO AXIS DIMENSION RATHER THAN FRAME
C
C     DRAW THE GRAPH OF A FONCTION GIVEN BY KN POINTS (PX(I),PY(I))
C     IN A RECTANGULAR BOX ((PX0,PY0) ; (PX0+PDX,PY0+PDY)
C
C        PX         : VECTOR OF X-COORDINATES, OF DIMENSION KN
C        PY         : VECTOR OF Y-COORDINATES, OF DIMENSION KN
C        (PX0,PY0)  : BOTTOM LEFT CORNER OF THE BOX
C        (PDX,PDY)  : LENGTH IN X- AND Y-DIRECTION OF THE BOX
C        PHCHAR     : CHARACTER HEIGHT (TYPICAL VALUE: 0.30)
C        PXIBCD     : TITLE OF X-AXIS
C        PYIBCD     : TITLE OF Y-AXIS
C        KX,KY      : NUMBER OF CHARACTERS IN PXIBCD,PYIBCD
C        PSIZLN     : LINE WIDTH OF CURVE
C        KCADRE     : DRAW BOX OPTION :(0=NO, 1=YES,CONT., 2=YES,ONLY CORNERS)
C        KGRID      : DRAW GRID OPTION : (0=NO, 1=Y-GRID, 2=X-GRID, 3=XY-GRID)
C
      CHARACTER PXIBCD*(*),PYIBCD*(*)
      DIMENSION  PX(KN),PY(KN)
C
C%%      CALL GSLN(1)
C%%      CALL GSLWSC(2.0)
         CALL GDASH(0)
         CALL GWICOL(-2.0,1)
C%%   CALL GSLWSC(1.0)
      CALL GWICOL(-1.0,1)
C%%
C     CACUL DES EXTREMAS
C
      CALL MAXAR(PX,KN,ZZXMAX,ZZXMIN)

      CALL MAXAR(PY,KN,ZZYMAX,ZZYMIN)
C
      IZERO = 0
      IF (ZZXMAX.EQ.0.0 .OR. ZZXMIN.EQ.0.0) IZERO = 1
      IF ( (ABS(ZZXMAX-ZZXMIN).LT.1.0E-12*ABS(ZZXMAX) .AND. IZERO.EQ.0)
     +      .OR.  (ZZXMAX.EQ.ZZXMIN       .AND. IZERO.EQ.1)       ) THEN
         IF(ZZXMAX .NE. 0.0) ZZXMIN = ZZXMIN/2.
         IF(ZZXMAX .NE. 0.0) ZZXMAX = ZZXMAX + ZZXMIN
         IF(ZZXMAX .EQ. 0.0) ZZXMIN = -1.
         IF(ZZXMAX .EQ. 0.0) ZZXMAX =  1.
      ENDIF
C
      IZERO = 0
      IF (ZZYMAX.EQ.0.0 .OR. ZZYMIN.EQ.0.0) IZERO = 1
      IF ( (ABS(ZZYMAX-ZZYMIN).LT.1.0E-12*ABS(ZZYMAX) .AND. IZERO.EQ.0)
     +      .OR.  (ZZYMAX.EQ.ZZYMIN       .AND. IZERO.EQ.1)       ) THEN
         IF(ZZYMAX .NE. 0.0) ZZYMIN = ZZYMIN/2.
         IF(ZZYMAX .NE. 0.0) ZZYMAX = ZZYMAX + ZZYMIN
         IF(ZZYMAX .EQ. 0.0) ZZYMIN = -1.
         IF(ZZYMAX .EQ. 0.0) ZZYMAX =  1.
      ENDIF

C     X EXTREMA
      IF (ABS(PXMIN+99.9) .LE. 1.0E-10) THEN
        ZXMIN = ZZXMIN
      ELSE
        ZXMIN = PXMIN
      ENDIF
      IF (ABS(PXMAX+99.9) .LE. 1.0E-10) THEN
        ZXMAX = ZZXMAX
      ELSE
        ZXMAX = PXMAX
      ENDIF
      IF (ZXMIN .GT. ZXMAX) THEN
        ZXMIN = ZZXMIN
        ZXMAX = ZZXMAX
      ENDIF
C     Y EXTREMA
      IF (ABS(PYMIN+99.9) .LE. 1.0E-10) THEN
        ZYMIN = ZZYMIN
      ELSE
        ZYMIN = PYMIN
      ENDIF
      IF (ABS(PYMAX+99.9) .LE. 1.0E-10) THEN
        ZYMAX = ZZYMAX
      ELSE
        ZYMAX = PYMAX
      ENDIF
      IF (ZYMIN .GT. ZYMAX) THEN
        ZYMIN = ZZYMIN
        ZYMAX = ZZYMAX
      ENDIF
C
      ZDX = ZXMAX-ZXMIN
      ZDY = ZYMAX-ZYMIN
C
C     LONGUEUR DES AXES
         ZHCHMN =       PHCHAR
c%OS         ZLX=PDX - 11.5 * ZHCHMN
         ZLX=PDX - 10.5 * ZHCHMN
         IF (ABS(ZDY) .LT. 0.05*AMAX1(ABS(ZYMIN),ABS(ZYMAX)))
     +                                             ZLX = ZLX - 2.*ZHCHMN
         ZLY=PDY - 7.0 * ZHCHMN
         zdeltax = (pdx - zlx)
         zdeltay = (pdy - zly)
         zcx0 = px0 - 0.75*zdeltax
         zcy0 = py0 - 0.75*zdeltay
         zlx = pdx
         zly = pdy
C%%
      IF (KCADRE .NE. 0) CALL CADRE(zcX0*10.,zcY0*10.,(zdeltax+zlx)*10.,
     +     (zdeltaY+zly)*10.,KCADRE)
C
C     DESSIN DES AXES
c%OS         ZX0=PX0+ 8.0 * ZHCHMN
         ZX0=PX0
         IF (ABS(ZDY) .LT. 0.05*AMAX1(ABS(ZYMIN),ABS(ZYMAX)))
     +                                             ZX0 = ZX0 + 2.*ZHCHMN
c%OS         ZY0=PY0+ 5.0 * ZHCHMN
         ZY0=PY0
C
C     ECHELLES
C
      ZECHX = ZLX/ZDX
      ZECHY = ZLY/ZDY
C
C     GRID OPTION
         ZGRIDX = 0.0
         ZGRIDY = 0.0
         IF (KGRID.EQ.1 .OR. KGRID.EQ.3) ZGRIDY =  ZLX
         IF (KGRID.EQ.2 .OR. KGRID.EQ.3) ZGRIDX =  ZLY
C
         CALL AXES(ZX0*10.,ZY0*10.,ZLX*10.,ZXMIN,ZXMAX, 0.,PHCHAR*10.,
     +             PXIBCD, KX, 0, 0,-ZLY*10.,-ZGRIDX*10.)
         CALL AXES(ZX0*10.,ZY0*10.,ZLY*10.,ZYMIN,ZYMAX,90.,PHCHAR*10.,
     +             PYIBCD,-KY, 1, 0, ZLX*10., ZGRIDY*10.)
C     
      DO 100 J=1,KN 
         PX(J) = ((PX(J)-ZXMIN)*ZECHX + ZX0) *10.
         PY(J) = ((PY(J)-ZYMIN)*ZECHY + ZY0) *10.
 100  CONTINUE
C
C%%      CALL GSLN(1)
C%%      CALL GSLWSC(PSIZLN)
         CALL GDASH(0)
         CALL GWICOL(-PSIZLN,1)
C%%         CALL GPL(KN,PX,PY)
         CALL GVECT(PX,PY,KN)
C
      DO 110 J=1,KN 
         PX(J) = (PX(J)/10.-ZX0)/ZECHX + ZXMIN
         PY(J) = (PY(J)/10.-ZY0)/ZECHY + ZYMIN
 110  CONTINUE
C
      IF (ZYMIN*ZYMAX .LT. 0.0) THEN
         CALL GDASH(1)
         CALL GWICOL(-1.0,1)
         CALL GPLOT(ZX0*10.      ,(ZY0-ZYMIN*ZECHY)*10.,3)
         CALL GPLOT((ZX0+ZLX)*10.,(ZY0-ZYMIN*ZECHY)*10.,2)
         CALL GDASH(0)
      ENDIF
C
C%%      CALL GSLWSC(1.)
         CALL GWICOL(-1.0,1)
C
C     DEFINE GRAF AREA
CC         CALL BGRAF(ZX0*10.,ZY0*10.,ZLX*10.,ZLY*10.)
C     NUMBER OF DECIMAL POINTS
CC         NDEC = 2
C     EXPONENT
CC         ZIM = 0.
CC         IF (ZXMIN .NE. 0.0) ZIM = ALOG10(ABS(ZXMIN))
CC         ZIMM = 0.
CC         IF (ZXMAX .NE. 0.0) ZIMM = ALOG10(ABS(ZXMAX))
CC         IF (ZIMM.GT.ZIM .AND. ZIMM.NE.0.0) ZIM = ZIMM
CC         IF (ZIM .EQ. 0.0) ZIM = ZIMM
CC         IF (ZIM   .GE. 0.0) IEXP = INT(ZIM  )
CC         IF (ZIM   .LT. 0.0) IEXP = INT(ZIM  ) - 1
CC         CALL BAXLAB(PHCHAR*10.,PHCHAR*10.,NDEC,IEXP)
C     TICK PLACE
CC         NTIC = 2
CC         CALL BTICKJ(NTIC)
C     X,Y INTERVALS
CC         INTX = 5
CC         INTY = 5
C     X-AXIS
CC         CALL BAXIS(1,ZXMIN,(ZXMAX-ZXMIN)/INTX,ZXMAX,PXIBCD)
C     Y-AXIS
CC         ZIM = 0.
CC         IF (ZYMIN .NE. 0.0) ZIM = ALOG10(ABS(ZYMIN))
CC         ZIMM = 0.
CC         IF (ZYMAX .NE. 0.0) ZIMM = ALOG10(ABS(ZYMAX))
CC         IF (ZIMM.GT.ZIM .AND. ZIMM.NE.0.0) ZIM = ZIMM
CC         IF (ZIM .EQ. 0.0) ZIM = ZIMM
CC         IF (ZIM   .GE. 0.0) IEXP = INT(ZIM  )
CC         IF (ZIM   .LT. 0.0) IEXP = INT(ZIM  ) - 1
CC         CALL BAXLAB(PHCHAR*10.,PHCHAR*10.,NDEC,IEXP)
CC         CALL BTICKJ(NTIC)
CC         CALL BAXIS(2,ZYMIN,(ZYMAX-ZYMIN)/INTY,ZYMAX,PYIBCD)
C     FRAME WIDTH
CC         ZFRAWTH = -2.0
CC         CALL BFRAME(ZFRAWTH)
C     CURVE WIDTH
CC         ZCURWTH = -PSIZLN
CC         CALL BLIWDH(ZCURWTH)
CC         CALL BLINE(PX,PY,KN)
C     GRID LINE KIND AND WIDTH
CC         IGRID = 2
CC         ZGRIWTH = -1.0
CC         CALL BDASH(IGRID)
CC         CALL BLIWDH(ZGRIWTH)
C     DEFINE GRID STYLE IN BOTH DIRECTION
CC         IGRISTX = 0
CC         IGRISTY = 0
CC         IF (KGRID .EQ. 1) IGRISTY =  1
CC         IF (KGRID .EQ. 2) IGRISTX =  1
CC         IF (KGRID .EQ. 3) IGRISTX =  1
CC         IF (KGRID .EQ. 3) IGRISTY =  1
CC         CALL BGRID(IGRISTX,IGRISTY)
C
C     ECHELLES
C
C%%   ZECHX = ZLX/ZDX
C%%   ZECHY = ZLY/ZDY
C
C%%   IF (ZYMIN*ZYMAX .LT. 0.0) THEN
C%%      CALL GPLOT(ZX0    ,ZY0-ZYMIN*ZECHY,3)
C%%      CALL GPLOT(ZX0+ZLX,ZY0-ZYMIN*ZECHY,2)
C%%   ENDIF
C
C%%   CALL GSLWSC(1.)
         CALL GWICOL(-1.0,1)
C
      RETURN
      END


         SUBROUTINE GRAPNMB(PX,PY,MDKN,KN,KPLOT,PX0,PDX,PY0,PDY,PHCHAR,
     +                      PXIBCD,PYIBCD,KX,KY,PSIZLN,KCADRE,KGRID,
     +                      PXMIN,PXMAX,PYMIN,PYMAX)
C        ***************************************************************
C
C     PLOT KPLOT CURVE, OTHERWISE, SAME AS GRAPHMB
C
C   WARNING: PX0,PY0,PDX,PDY REFER TO AXIS DIMENSION RATHER THAN FRAME
C
C     DRAW THE GRAPH OF A FONCTION GIVEN BY KN POINTS (PX(I),PY(I))
C     IN A RECTANGULAR BOX ((PX0,PY0) ; (PX0+PDX,PY0+PDY)
C
C        PX         : VECTOR OF X-COORDINATES, OF DIMENSION KN
C        PY         : VECTOR OF Y-COORDINATES, OF DIMENSION KN
C        (PX0,PY0)  : BOTTOM LEFT CORNER OF THE BOX
C        (PDX,PDY)  : LENGTH IN X- AND Y-DIRECTION OF THE BOX
C        PHCHAR     : CHARACTER HEIGHT (TYPICAL VALUE: 0.30)
C        PXIBCD     : TITLE OF X-AXIS
C        PYIBCD(I)  : TITLE OF CURVE + Y-AXIS
C        KX,KY(I)   : NUMBER OF CHARACTERS IN PXIBCD,PYIBCD
C        PSIZLN     : LINE WIDTH OF CURVE
C        KCADRE     : DRAW BOX OPTION :(0=NO, 1=YES,CONT., 2=YES,ONLY CORNERS)
C        KGRID      : DRAW GRID OPTION : (0=NO, 1=Y-GRID, 2=X-GRID, 3=XY-GRID)
C
      CHARACTER*(*) PXIBCD,PYIBCD(KPLOT+1)
      DIMENSION  PX(KN),PY(MDKN,KPLOT), KY(KPLOT+1), ZXLAB(2), ZYLAB(2)
C
C%%      CALL GSLN(1)
C%%      CALL GSLWSC(2.0)
         CALL GDASH(0)
         CALL GWICOL(-2.0,1)
C%%   CALL GSLWSC(1.0)
      CALL GWICOL(-1.0,1)
C%%
C     CACUL DES EXTREMAS
C
      CALL MAXAR(PX,KN,ZZXMAX,ZZXMIN)

      CALL MAXAR(PY(1,1),KN,ZZYMAX,ZZYMIN)
      DO IPLOT=2,KPLOT
        CALL MAXAR(PY(1,IPLOT),KN,ZZYMAXI,ZZYMINI)
        ZZYMAX = MAX(ZZYMAX,ZZYMAXI)
        ZZYMIN = MIN(ZZYMIN,ZZYMINI)
      ENDDO
C
      IZERO = 0
      IF (ZZXMAX.EQ.0.0 .OR. ZZXMIN.EQ.0.0) IZERO = 1
      IF ( (ABS(ZZXMAX-ZZXMIN).LT.1.0E-12*ABS(ZZXMAX) .AND. IZERO.EQ.0)
     +      .OR.  (ZZXMAX.EQ.ZZXMIN       .AND. IZERO.EQ.1)       ) THEN
         IF(ZZXMAX .NE. 0.0) ZZXMIN = ZZXMIN/2.
         IF(ZZXMAX .NE. 0.0) ZZXMAX = ZZXMAX + ZZXMIN
         IF(ZZXMAX .EQ. 0.0) ZZXMIN = -1.
         IF(ZZXMAX .EQ. 0.0) ZZXMAX =  1.
      ENDIF
C
      IZERO = 0
      IF (ZZYMAX.EQ.0.0 .OR. ZZYMIN.EQ.0.0) IZERO = 1
      IF ( (ABS(ZZYMAX-ZZYMIN).LT.1.0E-12*ABS(ZZYMAX) .AND. IZERO.EQ.0)
     +      .OR.  (ZZYMAX.EQ.ZZYMIN       .AND. IZERO.EQ.1)       ) THEN
         IF(ZZYMAX .NE. 0.0) ZZYMIN = ZZYMIN/2.
         IF(ZZYMAX .NE. 0.0) ZZYMAX = ZZYMAX + ZZYMIN
         IF(ZZYMAX .EQ. 0.0) ZZYMIN = -1.
         IF(ZZYMAX .EQ. 0.0) ZZYMAX =  1.
      ENDIF

C     X EXTREMA
      IF (ABS(PXMIN+99.9) .LE. 1.0E-10) THEN
        ZXMIN = ZZXMIN
      ELSE
        ZXMIN = PXMIN
      ENDIF
      IF (ABS(PXMAX+99.9) .LE. 1.0E-10) THEN
        ZXMAX = ZZXMAX
      ELSE
        ZXMAX = PXMAX
      ENDIF
      IF (ZXMIN .GT. ZXMAX) THEN
        ZXMIN = ZZXMIN
        ZXMAX = ZZXMAX
      ENDIF
C     Y EXTREMA
      IF (ABS(PYMIN+99.9) .LE. 1.0E-10) THEN
        ZYMIN = ZZYMIN
      ELSE
        ZYMIN = PYMIN
      ENDIF
      IF (ABS(PYMAX+99.9) .LE. 1.0E-10) THEN
        ZYMAX = ZZYMAX
      ELSE
        ZYMAX = PYMAX
      ENDIF
      IF (ZYMIN .GT. ZYMAX) THEN
        ZYMIN = ZZYMIN
        ZYMAX = ZZYMAX
      ENDIF
C
      ZDX = ZXMAX-ZXMIN
      ZDY = ZYMAX-ZYMIN
C
C     LONGUEUR DES AXES
         ZHCHMN =       PHCHAR
c%OS         ZLX=PDX - 11.5 * ZHCHMN
         ZLX=PDX - 10.5 * ZHCHMN
         IF (ABS(ZDY) .LT. 0.05*AMAX1(ABS(ZYMIN),ABS(ZYMAX)))
     +                                             ZLX = ZLX - 2.*ZHCHMN
         ZLY=PDY - 7.0 * ZHCHMN
         zdeltax = (pdx - zlx)
         zdeltay = (pdy - zly)
         zcx0 = px0 - 0.75*zdeltax
         zcy0 = py0 - 0.75*zdeltay
         zlx = pdx
         zly = pdy
C%%
      IF (KCADRE .NE. 0) CALL CADRE(zcX0*10.,zcY0*10.,(zdeltax+zlx)*10.,
     +     (zdeltaY+zly)*10.,KCADRE)
C
C     DESSIN DES AXES
c%OS         ZX0=PX0+ 8.0 * ZHCHMN
         ZX0=PX0
         IF (ABS(ZDY) .LT. 0.05*AMAX1(ABS(ZYMIN),ABS(ZYMAX)))
     +                                             ZX0 = ZX0 + 2.*ZHCHMN
c%OS         ZY0=PY0+ 5.0 * ZHCHMN
         ZY0=PY0
C
C     ECHELLES
C
      ZECHX = ZLX/ZDX
      ZECHY = ZLY/ZDY
C
C     GRID OPTION
         ZGRIDX = 0.0
         ZGRIDY = 0.0
         IF (KGRID.EQ.1 .OR. KGRID.EQ.3) ZGRIDY =  ZLX
         IF (KGRID.EQ.2 .OR. KGRID.EQ.3) ZGRIDX =  ZLY
C
         CALL AXES(ZX0*10.,ZY0*10.,ZLX*10.,ZXMIN,ZXMAX, 0.,PHCHAR*10.,
     +             PXIBCD, KX, 0, 0,-ZLY*10.,-ZGRIDX*10.)
         CALL AXES(ZX0*10.,ZY0*10.,ZLY*10.,ZYMIN,ZYMAX,90.,PHCHAR*10.,
     +         PYIBCD(KPLOT+1),-KY(KPLOT+1), 1, 0, ZLX*10., ZGRIDY*10.)
C
         DO J=1,KN 
           PX(J) = ((PX(J)-ZXMIN)*ZECHX + ZX0) *10.
         ENDDO
         DO IP=1,KPLOT
           DO J=1,KN 
             PY(J,IP) = ((PY(J,IP)-ZYMIN)*ZECHY + ZY0) *10.
           ENDDO
         ENDDO
C
C%%      CALL GSLN(1)
C%%      CALL GSLWSC(PSIZLN)
         CALL GDASH(0)
         CALL GWICOL(-PSIZLN,1)
         DO IP=1,KPLOT
           CALL GDASH(MOD(IP-1,8))
           CALL GVECT(PX,PY(1,IP),KN)
         ENDDO
C
C
CL    LABEL THE CURVES
C
         ZY = ZY0 + ZLY + 0.5
         ZYLAB(1) = ZY*10.
         ZYLAB(2) = ZY*10.
         ZX = ZCX0 + PHCHAR
      DO 107 IP=1,KPLOT
         ZXLAB(1) = ZX*10.
         ZXLAB(2) = (ZX + 0.8) * 10.
         CALL GDASH(MOD(IP-1,8))
         CALL GVECT(ZXLAB,ZYLAB,2)
         CALL GCHAR(PYIBCD(IP),ZXLAB(2)+0.15*10.,ZY*10.,PHCHAR*10.)
C
c%OS         ZDEL = 0.8 + KY(IP) * PHCHAR * 0.90 ! UNIRAS
         ZDEL = 0.8 + KY(IP) * PHCHAR * 1.00 ! NCAR
         ZX = ZX + ZDEL
 107  CONTINUE
C
      DO 110 J=1,KN 
         PX(J) = (PX(J)/10.-ZX0)/ZECHX + ZXMIN
 110  CONTINUE
C
      DO 111 IP=1,KPLOT
      DO 111 J=1,KN 
         PY(J,IP) = (PY(J,IP)/10.-ZY0)/ZECHY + ZYMIN
 111  CONTINUE
C
      IF (ZYMIN*ZYMAX .LT. 0.0) THEN
         CALL GDASH(1)
         CALL GWICOL(-1.0,1)
         CALL GPLOT(ZX0*10.      ,(ZY0-ZYMIN*ZECHY)*10.,3)
         CALL GPLOT((ZX0+ZLX)*10.,(ZY0-ZYMIN*ZECHY)*10.,2)
         CALL GDASH(0)
      ENDIF
C
C%%      CALL GSLWSC(1.)
         CALL GWICOL(-1.0,1)
C
      RETURN
      END


      SUBROUTINE GLASHMB(PX,PY,KN,PX0,PDX,PY0,PDY,PHCHAR,PXIBCD,
     +                      PYIBCD,KX,KY,PSIZLN,KCADRE,KGRID,
     +                      PXMIN,PXMAX,PYMIN,PYMAX)
C        ***************************************************************
C
C
C     Y AXIS IS ASINH SCALING
C
C   WARNING: PX0,PY0,PDX,PDY REFER TO AXIS DIMENSION RATHER THAN FRAME
C
C     DRAW THE GRAPH OF A FONCTION GIVEN BY KN POINTS (PX(I),PY(I))
C     IN A RECTANGULAR BOX ((PX0,PY0) ; (PX0+PDX,PY0+PDY)
C
C        PX         : VECTOR OF X-COORDINATES, OF DIMENSION KN
C        PY         : VECTOR OF Y-COORDINATES, OF DIMENSION KN
C        (PX0,PY0)  : BOTTOM LEFT CORNER OF THE BOX
C        (PDX,PDY)  : LENGTH IN X- AND Y-DIRECTION OF THE BOX
C        PHCHAR     : CHARACTER HEIGHT (TYPICAL VALUE: 0.30)
C        PXIBCD     : TITLE OF X-AXIS
C        PYIBCD     : TITLE OF Y-AXIS
C        KX,KY      : NUMBER OF CHARACTERS IN PXIBCD,PYIBCD
C        PSIZLN     : LINE WIDTH OF CURVE
C        KCADRE     : DRAW BOX OPTION :(0=NO, 1=YES,CONT., 2=YES,ONLY CORNERS)
C        KGRID      : DRAW GRID OPTION : (0=NO, 1=Y-GRID, 2=X-GRID, 3=XY-GRID)
C
      CHARACTER PXIBCD*(*),PYIBCD*(*)
      DIMENSION  PX(KN),PY(KN)
C
C%%      CALL GSLN(1)
C%%      CALL GSLWSC(2.0)
         CALL GDASH(0)
         CALL GWICOL(-2.0,1)
C%%   CALL GSLWSC(1.0)
      CALL GWICOL(-1.0,1)
C%%
C     CACUL DES EXTREMAS
C
      CALL MAXAR(PX,KN,ZZXMAX,ZZXMIN)

      CALL MAXAR(PY,KN,ZZYMAX,ZZYMIN)
C
      IZERO = 0
      IF (ZZXMAX.EQ.0.0 .OR. ZZXMIN.EQ.0.0) IZERO = 1
      IF ( (ABS(ZZXMAX-ZZXMIN).LT.1.0E-12*ABS(ZZXMAX) .AND. IZERO.EQ.0)
     +      .OR.  (ZZXMAX.EQ.ZZXMIN       .AND. IZERO.EQ.1)       ) THEN
         IF(ZZXMAX .NE. 0.0) ZZXMIN = ZZXMIN/2.
         IF(ZZXMAX .NE. 0.0) ZZXMAX = ZZXMAX + ZZXMIN
         IF(ZZXMAX .EQ. 0.0) ZZXMIN = -1.
         IF(ZZXMAX .EQ. 0.0) ZZXMAX =  1.
      ENDIF
C
      IZERO = 0
      IF (ZZYMAX.EQ.0.0 .OR. ZZYMIN.EQ.0.0) IZERO = 1
      IF ( (ABS(ZZYMAX-ZZYMIN).LT.1.0E-12*ABS(ZZYMAX) .AND. IZERO.EQ.0)
     +      .OR.  (ZZYMAX.EQ.ZZYMIN       .AND. IZERO.EQ.1)       ) THEN
         IF(ZZYMAX .NE. 0.0) ZZYMIN = ZZYMIN/2.
         IF(ZZYMAX .NE. 0.0) ZZYMAX = ZZYMAX + ZZYMIN
         IF(ZZYMAX .EQ. 0.0) ZZYMIN = -1.
         IF(ZZYMAX .EQ. 0.0) ZZYMAX =  1.
      ENDIF

C     X EXTREMA
      IF (ABS(PXMIN+99.9) .LE. 1.0E-10) THEN
        ZXMIN = ZZXMIN
      ELSE
        ZXMIN = PXMIN
      ENDIF
      IF (ABS(PXMAX+99.9) .LE. 1.0E-10) THEN
        ZXMAX = ZZXMAX
      ELSE
        ZXMAX = PXMAX
      ENDIF
      IF (ZXMIN .GT. ZXMAX) THEN
        ZXMIN = ZZXMIN
        ZXMAX = ZZXMAX
      ENDIF
C     Y EXTREMA
      IF (ABS(PYMIN+99.9) .LE. 1.0E-10) THEN
        ZYMIN = ZZYMIN
      ELSE
        ZYMIN = PYMIN
      ENDIF
      IF (ABS(PYMAX+99.9) .LE. 1.0E-10) THEN
        ZYMAX = ZZYMAX
      ELSE
        ZYMAX = PYMAX
      ENDIF
      IF (ZYMIN .GT. ZYMAX) THEN
        ZYMIN = ZZYMIN
        ZYMAX = ZZYMAX
      ENDIF
C
      ZDX = ZXMAX-ZXMIN
C
C     LONGUEUR DES AXES
         ZHCHMN =       PHCHAR
c%OS         ZLX=PDX - 11.5 * ZHCHMN
         ZLX=PDX - 10.5 * ZHCHMN
C         IF (ABS(ZDY) .LT. 0.05*AMAX1(ABS(ZYMIN),ABS(ZYMAX)))
C     +                                             ZLX = ZLX - 2.*ZHCHMN
         ZLY=PDY - 7.0 * ZHCHMN
         zdeltax = (pdx - zlx)
         zdeltay = (pdy - zly)
         zcx0 = px0 - 0.75*zdeltax
         zcy0 = py0 - 0.75*zdeltay
         zlx = pdx
         zly = pdy
C%%
      IF (KCADRE .NE. 0) CALL CADRE(zcX0*10.,zcY0*10.,(zdeltax+zlx)*10.,
     +     (zdeltaY+zly)*10.,KCADRE)
C
C     DESSIN DES AXES
c%OS         ZX0=PX0+ 8.0 * ZHCHMN
c%OS         ZX0=PX0
         ZX0=PX0 + 2.2*ZHCHMN
C         IF (ABS(ZDY) .LT. 0.05*AMAX1(ABS(ZYMIN),ABS(ZYMAX)))
C     +                                             ZX0 = ZX0 + 2.*ZHCHMN
c%OS         ZY0=PY0+ 5.0 * ZHCHMN
         ZY0=PY0
C
C     ECHELLES
C
      ZECHX = ZLX/ZDX
      ZDYA = ASINH(ZYMAX) - ASINH(ZYMIN)
      ZECHY = ZLY/ZDYA
C
C     GRID OPTION
         ZGRIDX = 0.0
         ZGRIDY = 0.0
         IF (KGRID.EQ.1 .OR. KGRID.EQ.3) ZGRIDY =  ZLX
         IF (KGRID.EQ.2 .OR. KGRID.EQ.3) ZGRIDX =  ZLY
C
         CALL AXES(ZX0*10.,ZY0*10.,ZLX*10.,ZXMIN,ZXMAX, 0.,PHCHAR*10.,
     +             PXIBCD, KX, 0, 0,-ZLY*10.,-ZGRIDX*10.)
         CALL AXASNH(ZX0*10.,ZY0*10.,ZLY*10.,ZYMIN,ZYMAX,90.,PHCHAR*10.,
     +        PYIBCD,-KY, 1, 0, ZLX*10., ZGRIDY*10.)
C     
      DO 100 J=1,KN 
         PX(J) = ((PX(J)-ZXMIN)*ZECHX + ZX0) *10.
 100  CONTINUE
C
      ZYMNAS = ASINH(ZYMIN)
      DO 101 J=1,KN 
         PY(J) = ((ASINH(PY(J))-ZYMNAS)*ZECHY + ZY0) *10.
 101  CONTINUE
C
C%%      CALL GSLN(1)
C%%      CALL GSLWSC(PSIZLN)
         CALL GDASH(0)
         CALL GWICOL(-PSIZLN,1)
C%%         CALL GPL(KN,PX,PY)
         CALL GVECT(PX,PY,KN)
C
      DO 110 J=1,KN 
         PX(J) = (PX(J)/10.-ZX0)/ZECHX + ZXMIN
         PY(J) = SINH((PY(J)/10.-ZY0)/ZECHY + ZYMNAS)
 110  CONTINUE
C
      IF (ZYMIN*ZYMAX .LT. 0.0) THEN
         CALL GWICOL(-1.0,1)
         CALL GDASH(1)
         CALL GPLOT(ZX0*10.      ,(ZY0-ZYMNAS*ZECHY)*10.,3)
         CALL GPLOT((ZX0+ZLX)*10.,(ZY0-ZYMNAS*ZECHY)*10.,2)
         CALL GDASH(0)
      ENDIF
C
C%%      CALL GSLWSC(1.)
         CALL GWICOL(-1.0,1)
C
      RETURN
      END


         SUBROUTINE GLASNMB(PX,PY,MDKN,KN,KPLOT,PX0,PDX,PY0,PDY,PHCHAR,
     +                      PXIBCD,PYIBCD,KX,KY,PSIZLN,KCADRE,KGRID,
     +                      PXMIN,PXMAX,PYMIN,PYMAX)
C        ***************************************************************
C
C     PLOT KPLOT CURVE, OTHERWISE, SAME AS GLASHMB
C
C     PX0,PY0,PDX,PDY REFER TO AXIS DIMENSION RATHER THAN FRAME
C
C     DRAW THE GRAPH OF A FONCTION GIVEN BY KN POINTS (PX(I),PY(I))
C     IN A RECTANGULAR BOX ((PX0,PY0) ; (PX0+PDX,PY0+PDY)
C
C        PX         : VECTOR OF X-COORDINATES, OF DIMENSION KN
C        PY         : VECTOR OF Y-COORDINATES, OF DIMENSION KN
C        (PX0,PY0)  : BOTTOM LEFT CORNER OF THE BOX
C        (PDX,PDY)  : LENGTH IN X- AND Y-DIRECTION OF THE BOX
C        PHCHAR     : CHARACTER HEIGHT (TYPICAL VALUE: 0.30)
C        PXIBCD     : TITLE OF X-AXIS
C        PYIBCD(I)  : TITLE OF CURVE + Y-AXIS
C        KX,KY(I)   : NUMBER OF CHARACTERS IN PXIBCD,PYIBCD
C        PSIZLN     : LINE WIDTH OF CURVE
C        KCADRE     : DRAW BOX OPTION :(0=NO, 1=YES,CONT., 2=YES,ONLY CORNERS)
C        KGRID      : DRAW GRID OPTION : (0=NO, 1=Y-GRID, 2=X-GRID, 3=XY-GRID)
C
      CHARACTER*(*) PXIBCD,PYIBCD(KPLOT+1)
      DIMENSION  PX(KN),PY(MDKN,KPLOT), KY(KPLOT+1), ZXLAB(2), ZYLAB(2)
C
C%%      CALL GSLN(1)
C%%      CALL GSLWSC(2.0)
         CALL GDASH(0)
         CALL GWICOL(-2.0,1)
C%%   CALL GSLWSC(1.0)
      CALL GWICOL(-1.0,1)
C%%
C     CACUL DES EXTREMAS
C
      CALL MAXAR(PX,KN,ZZXMAX,ZZXMIN)

      CALL MAXAR(PY(1,1),KN,ZZYMAX,ZZYMIN)
      DO IPLOT=2,KPLOT
        CALL MAXAR(PY(1,IPLOT),KN,ZZYMAXI,ZZYMINI)
        ZZYMAX = MAX(ZZYMAX,ZZYMAXI)
        ZZYMIN = MIN(ZZYMIN,ZZYMINI)
      ENDDO
C
      IZERO = 0
      IF (ZZXMAX.EQ.0.0 .OR. ZZXMIN.EQ.0.0) IZERO = 1
      IF ( (ABS(ZZXMAX-ZZXMIN).LT.1.0E-12*ABS(ZZXMAX) .AND. IZERO.EQ.0)
     +      .OR.  (ZZXMAX.EQ.ZZXMIN       .AND. IZERO.EQ.1)       ) THEN
         IF(ZZXMAX .NE. 0.0) ZZXMIN = ZZXMIN/2.
         IF(ZZXMAX .NE. 0.0) ZZXMAX = ZZXMAX + ZZXMIN
         IF(ZZXMAX .EQ. 0.0) ZZXMIN = -1.
         IF(ZZXMAX .EQ. 0.0) ZZXMAX =  1.
      ENDIF
C
      IZERO = 0
      IF (ZZYMAX.EQ.0.0 .OR. ZZYMIN.EQ.0.0) IZERO = 1
      IF ( (ABS(ZZYMAX-ZZYMIN).LT.1.0E-12*ABS(ZZYMAX) .AND. IZERO.EQ.0)
     +      .OR.  (ZZYMAX.EQ.ZZYMIN       .AND. IZERO.EQ.1)       ) THEN
         IF(ZZYMAX .NE. 0.0) ZZYMIN = ZZYMIN/2.
         IF(ZZYMAX .NE. 0.0) ZZYMAX = ZZYMAX + ZZYMIN
         IF(ZZYMAX .EQ. 0.0) ZZYMIN = -1.
         IF(ZZYMAX .EQ. 0.0) ZZYMAX =  1.
      ENDIF

C     X EXTREMA
      IF (ABS(PXMIN+99.9) .LE. 1.0E-10) THEN
        ZXMIN = ZZXMIN
      ELSE
        ZXMIN = PXMIN
      ENDIF
      IF (ABS(PXMAX+99.9) .LE. 1.0E-10) THEN
        ZXMAX = ZZXMAX
      ELSE
        ZXMAX = PXMAX
      ENDIF
      IF (ZXMIN .GT. ZXMAX) THEN
        ZXMIN = ZZXMIN
        ZXMAX = ZZXMAX
      ENDIF
C     Y EXTREMA
      IF (ABS(PYMIN+99.9) .LE. 1.0E-10) THEN
        ZYMIN = ZZYMIN
      ELSE
        ZYMIN = PYMIN
      ENDIF
      IF (ABS(PYMAX+99.9) .LE. 1.0E-10) THEN
        ZYMAX = ZZYMAX
      ELSE
        ZYMAX = PYMAX
      ENDIF
      IF (ZYMIN .GT. ZYMAX) THEN
        ZYMIN = ZZYMIN
        ZYMAX = ZZYMAX
      ENDIF
C
      ZDX = ZXMAX-ZXMIN
      ZDY = ZYMAX-ZYMIN
C
C     LONGUEUR DES AXES
         ZHCHMN =       PHCHAR
c%OS         ZLX=PDX - 11.5 * ZHCHMN
         ZLX=PDX - 10.5 * ZHCHMN
         IF (ABS(ZDY) .LT. 0.05*AMAX1(ABS(ZYMIN),ABS(ZYMAX)))
     +                                             ZLX = ZLX - 2.*ZHCHMN
         ZLY=PDY - 7.0 * ZHCHMN
         zdeltax = (pdx - zlx)
         zdeltay = (pdy - zly)
         zcx0 = px0 - 0.75*zdeltax
         zcy0 = py0 - 0.75*zdeltay
         zlx = pdx
         zly = pdy
C%%
      IF (KCADRE .NE. 0) CALL CADRE(zcX0*10.,zcY0*10.,(zdeltax+zlx)*10.,
     +     (zdeltaY+zly)*10.,KCADRE)
C
C     DESSIN DES AXES
c%OS         ZX0=PX0+ 8.0 * ZHCHMN
         ZX0=PX0 + 2.2*ZHCHMN
         IF (ABS(ZDY) .LT. 0.05*AMAX1(ABS(ZYMIN),ABS(ZYMAX)))
     +                                             ZX0 = ZX0 + 2.*ZHCHMN
c%OS         ZY0=PY0+ 5.0 * ZHCHMN
         ZY0=PY0
C
C     ECHELLES
C
      ZECHX = ZLX/ZDX
      ZDYA = ASINH(ZYMAX) - ASINH(ZYMIN)
      ZECHY = ZLY/ZDYA
C
C     GRID OPTION
         ZGRIDX = 0.0
         ZGRIDY = 0.0
         IF (KGRID.EQ.1 .OR. KGRID.EQ.3) ZGRIDY =  ZLX
         IF (KGRID.EQ.2 .OR. KGRID.EQ.3) ZGRIDX =  ZLY
C
         CALL AXES(ZX0*10.,ZY0*10.,ZLX*10.,ZXMIN,ZXMAX, 0.,PHCHAR*10.,
     +             PXIBCD, KX, 0, 0,-ZLY*10.,-ZGRIDX*10.)
         CALL AXASNH(ZX0*10.,ZY0*10.,ZLY*10.,ZYMIN,ZYMAX,90.,PHCHAR*10.,
     +        PYIBCD(KPLOT+1),-KY(KPLOT+1), 1, 0, ZLX*10., ZGRIDY*10.)
C
         DO J=1,KN 
           PX(J) = ((PX(J)-ZXMIN)*ZECHX + ZX0) *10.
         ENDDO
         ZYMNAS = ASINH(ZYMIN)
         DO IP=1,KPLOT
           DO J=1,KN 
             PY(J,IP) = ((ASINH(PY(J,IP))-ZYMNAS)*ZECHY + ZY0) *10.
           ENDDO
         ENDDO
C
C%%      CALL GSLN(1)
C%%      CALL GSLWSC(PSIZLN)
         CALL GDASH(0)
         CALL GWICOL(-PSIZLN,1)
         DO IP=1,KPLOT
           CALL GDASH(MOD(IP-1,8))
           CALL GVECT(PX,PY(1,IP),KN)
         ENDDO
C
C
CL    LABEL THE CURVES
C
         ZY = ZY0 + ZLY + 0.5
         ZYLAB(1) = ZY*10.
         ZYLAB(2) = ZY*10.
         ZX = ZCX0 + PHCHAR
      DO 107 IP=1,KPLOT
         ZXLAB(1) = ZX*10.
         ZXLAB(2) = (ZX + 0.8) * 10.
         CALL GDASH(MOD(IP-1,8))
         CALL GVECT(ZXLAB,ZYLAB,2)
         CALL GCHAR(PYIBCD(IP),ZXLAB(2)+0.15*10.,ZY*10.,PHCHAR*10.)
C
c%OS         ZDEL = 0.8 + KY(IP) * PHCHAR * 0.90 ! UNIRAS
         ZDEL = 0.8 + KY(IP) * PHCHAR * 1.00 ! NCAR
         ZX = ZX + ZDEL
 107  CONTINUE
C
      DO 110 J=1,KN 
         PX(J) = (PX(J)/10.-ZX0)/ZECHX + ZXMIN
 110  CONTINUE
C
      DO 111 IP=1,KPLOT
      DO 111 J=1,KN 
         PY(J,IP) = SINH((PY(J,IP)/10.-ZY0)/ZECHY + ZYMIN)
 111  CONTINUE
C
      IF (ZYMIN*ZYMAX .LT. 0.0) THEN
         CALL GDASH(1)
         CALL GWICOL(-1.0,1)
         CALL GPLOT(ZX0*10.      ,(ZY0-ZYMNAS*ZECHY)*10.,3)
         CALL GPLOT((ZX0+ZLX)*10.,(ZY0-ZYMNAS*ZECHY)*10.,2)
         CALL GDASH(0)
      ENDIF
C
C%%      CALL GSLWSC(1.)
         CALL GWICOL(-1.0,1)
C
      RETURN
      END
         SUBROUTINE AXES(X,Y,SIZE,XMIN,XMAX,THETA,PHCHAR,IBCD,N
     +                  ,KCHDIR,KTITDIR,PDIST,SIZ2)
C        ******************************************************
C     DRAWS A LINEAR AXIS
C
C     X,Y     WORLD COORDINATES AT BEGINNING OF AXIS
C     SIZE    AXIS LENGTH
C     XMIN    VALUE OF THE VARIABLE AT THE BEGINNING OF THE AXIS
C     XMAX      "   "   "     "     "   "  END       OF THE AXIS
C     THETA   ANGLE IN DEGREES
C     PHCHAR  HEIGHT OF CHARACTERS
C     IBCD    NAME OF AXIS
C     N       LENGTH OF NAME (NB OF CHARACTERS)
C             IF POSITIVE, NAME WILL BE PRINTED ON THE RIGHT SIDE
C             IF NEGATIVE,  "    "        "            LEFT   "
C     KCHDIR  DIRECTION OF AXIS NUMBERS (VERTICAL AXIS ONLY)
C             = 0 : CHARACTER PERP. TO AXIS (I.E. WRITING VERTICALLY  )
C             = 1 :    "   PARALLEL TO AXIS (I.E.    "    HORIZONTALLY)
C     KTITDIR DIRECTION OF CHARACTERS FOR THE TITLE (VERTICAL AXIS ONLY)
C             = 0 : CHARACTER PERP. TO AXIS (I.E. WRITING VERTICALLY  )
C             = 1 :    "   PARALLEL TO AXIS (I.E.    "    HORIZONTALLY)
C     PDIST   DISTANCE IN PERPEND. DIRECT. AT WHICH AXIS AND TICKS ARE RE-DRAWN
C             PDIST > 0  => ON THE RIGHT SIDE OF THE AXIS
C             PDIST < 0  => ON THE LEFT  SIDE "   "   "
C             IN GENERAL PDIST=SIZ2 IF (SIZ2 .NE. 0.)
C     SIZ2    GRID LENGTH
C             SIZ2  > 0  => ON THE RIGHT SIDE OF THE AXIS
C             SIZ2  < 0  => ON THE LEFT  SIDE "   "   "
C
C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C
C     FOR A SIMPLE AXIS FILL IN THE FIRST SIX PARAMETERS AS WELL AS IBCD
C     AND N, AND SET THE OTHERS TO THE FOLLOWING VALUES :
C           PHCHAR  = 0.30  OR  0.25
C           PDIST   = 0.0 OR SIZE OF OTHER AXIS
C           SIZ2    = 0.0
C           KCHDIR  = 0  FOR THETA=0.(ALWAYS)
C                   = 1  FOR THETA=90.
C           KTITDIR = 0  (ALWAYS FOR THETA=0)
C    !!! PDIST AND SIZ2 SHOULD HAVE SAME SIGN AND OPPOSITE TO SIGN OF N
C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C
C
      DIMENSION ZXN(2),      ZYN(2)
      CHARACTER TEXT*10 ,IBCD*(*)
C     DATA SIZEN,SIZEL,TICK/0.02,0.04,0.02/
      DX = XMAX - XMIN
      S = SIZE
C
CL       DEFINE BASIC ATTRIBUTES
C
C                      FOR TEXT
C&    CALL GSTXFP(2,2)
C%%   CALL GSCHSP(0.0)
C%%   CALL GSCHXP(1.0)
C%%   CALL GSTXP(0)
C%%   CALL GSTXAL(0,0)
C%%   CALL GSCHH(PHCHAR)
C                      FOR POLYLINE
C%%   CALL GSLN(1)
      CALL GDASH(0)
C%%   CALL GSLWSC(2.)
      CALL GWICOL(-2.0,1)
CC     CALL GQTXX(1,X,Y,'ABCDEFGHIJ',IERR,CPX,CPY,TX,TY)
C     SIZEW = (TX(3)-TX(1))/10.
C     SIZEL = (TY(3)-TY(1))*0.5
      SIZEW = 0.947*PHCHAR
      SIZEH = PHCHAR
      TICK  = SIZEH/2.0
      ZTHETA = 0.
C%%
      IANGLE = 0
      IF (THETA .EQ. 90.) ZTHETA = -1.
C%%   IF (KCHDIR .EQ. 0) CALL GSCHUP(0.+ZTHETA,1.+ZTHETA)
      IF (KCHDIR.EQ.0 .AND. THETA.EQ.90.) IANGLE = 90
      I1MKCH = 1 - KCHDIR
      I1MKTIT = 1 - KTITDIR
C     ADX=ABS(DX)
C%      ADX = AMAX1(ABS(XMIN),ABS(DX),ABS(XMIN+DX))
      ADX = AMAX1(ABS(XMIN),ABS(XMIN+DX))
      NFLOAT = 2
      IF (ABS(DX) .LT. 0.05 *ADX) NFLOAT = 3
      IF (ABS(DX) .LT. 0.005*ADX) NFLOAT = 4
C--------------RETURN IF DX=0
      IF(ADX.NE.0.) GO TO 30
      GO TO 99
30    CONTINUE
      ANG=THETA*0.017453292
      CTH=COS(ANG)
      STH=SIN(ANG)
C--    FIND THE RIGHT SCALE
      EX=0.0
3     CONTINUE
      IF(ADX.LT.10.0)GO TO 5
      ADX=ADX*0.10
      EX=EX+1.0
      GO TO 3
    6 ADX=ADX*10.0
      EX=EX-1.0
    5 IF(ADX.LT.1.00) GO TO 6
      ADX=10.0**(-EX)
      X0=XMIN*ADX
      ADX=DX*ADX
C
C  (NTIC-1) = NUMBER OF INTERVALS
      NTIC = 11
      CHDEC = SIZE / (SIZEW*(NFLOAT+3.)*I1MKCH + PHCHAR*(1.+2.*KCHDIR))
      IF (CHDEC.GE.45.                   ) NTIC = 51
      IF (CHDEC.GE.35. .AND. CHDEC.LT.45.) NTIC = 41
      IF (CHDEC.GE.25. .AND. CHDEC.LT.35.) NTIC = 31
      IF (CHDEC.GE.15. .AND. CHDEC.LT.25.) NTIC = 21
      IF (CHDEC.GE.3.8 .AND. CHDEC.LT.4.8) NTIC = 9
      IF (CHDEC.GE.2.8 .AND. CHDEC.LT.3.8) NTIC = 7
      IF (CHDEC.GE.1.8 .AND. CHDEC.LT.2.8) NTIC = 5
      IF (                   CHDEC.LT.1.8) NTIC = 3
      KN=N
      A=-1.0
C--------------POSITIVE N FOR ANNOTATION ON CLOCKWISE SIDE OF AXIS
      IF(ISIGN(1,KN).GE.0) GO TO 2
      A=-A
      KN=-KN
C--------------DETERMINE POSITION OF ANNOTATION
 2    ZCTH = CTH*S/(NTIC-1.)
      ZSTH = STH*S/(NTIC-1.)
      DXN=-SIZEW*2.0
C        ZA0 = 1 IF TITLE ON LEFT SIDE, 0 OTHERWISE
      ZA0 = (A+1.)/2.
      IF(X0.GE.10.)DXN=DXN-SIZEW
      IF(X0.GE.100.)DXN=DXN-SIZEW
      DYN= 1.5*TICK*A - (1.-ZA0)*SIZEH
C
      XN = X +  DXN*CTH - (DYN             )*STH -
     -     (ZA0*(NFLOAT+2) + (1.-ZA0)*2.5)*KCHDIR*STH*SIZEW
      IF (XMIN .LT. 0.0) XN = XN + STH*(1.-ZA0)*SIZEW*1.05*KCHDIR
C
      YN = Y + (DYN*CTH+DXN*STH)*I1MKCH -
     -         SIZEH/2.*KCHDIR*STH
      NT=NTIC/2
C--------------WRITE NUMERIC   INTERVALS
      ZDX0 = ADX/(NTIC-1.)
      DO 8 I=1,NTIC,2
      WRITE(TEXT,'(F7.4)') X0
      IF (TEXT(2:2) .EQ. ' ') TEXT(2:2) = '0'
C
CL       SPECIAL TREATMENT FOR CYBER ONLY
C
C&    IF (TEXT(2:2) .EQ. '-') THEN
C&       TEXT(1:1) = '-'
C&       TEXT(2:2) = '0'
C&    ENDIF
C        END OF SPECIAL TREATMENT FOR CYBER
C
C     RATIO WIDTH '-' OVER ' '
      ZRAT = 18./35.
      IF (TEXT(1:1) .EQ. '-') XN = XN - ZRAT*SIZEW*(CTH+KCHDIR)
      IF (TEXT(1:1) .EQ. '-') YN = YN - ZRAT*SIZEW*STH*I1MKCH
C%%   CALL GTX(XN,YN,TEXT(1:NFLOAT+3))
      ITLEN = NFLOAT+4
      TEXT(ITLEN:ITLEN) = '$'
      CALL GCHARA(IANGLE)
      CALL GCHAR(TEXT,XN,YN,PHCHAR)
      IF (TEXT(1:1) .EQ. '-') XN = XN + ZRAT*SIZEW*(CTH+KCHDIR)
      IF (TEXT(1:1) .EQ. '-') YN = YN + ZRAT*SIZEW*STH*I1MKCH
      X0=X0+2.*ZDX0
      XN=XN+2.*ZCTH
      YN=YN+2.*ZSTH
 8    CONTINUE
C--------------WRITE AXIS LABEL
      Z=KN
      IF(EX.NE.0.0) Z=Z+7.
      ZZ = KN
      Z=Z/2.
      DXB=S*0.5-Z*SIZEW
      DYB=(0.8*TICK+SIZEW*3.5)*A -0.5*SIZEW
      ZDYY = FLOAT(NFLOAT-2)*0.4*SIZEW * A * KCHDIR
C
      XT= X+DXB*CTH-(DYB-2.*ZA0*SIZEW+ZDYY+(1.-ZA0)*(SIZEW-2.*TICK))*STH
     -   - (ZA0*SIZEW*(ZZ-3.*KCHDIR) + (1.-ZA0)*1.0*SIZEW)*KTITDIR*STH -
     -                (A*(NFLOAT+3)*SIZEW+(1.-ZA0)*3.2*SIZEW)*KCHDIR*STH
      XT = XT - I1MKCH*SIZEW*(1.4+0.5*A-(3.20+0.20*A)*SIZEW*KTITDIR)*STH
      XT = XT + I1MKTIT*STH*SIZEW*(I1MKCH*(1.-ZA0) + KCHDIR*ZA0*1.2)
      IF (XMIN .LT. 0.0) XT = XT - A*KCHDIR*SIZEW*STH
C
      YT = Y + DYB*CTH + DXB*STH *I1MKTIT + (0.5*S-SIZEH/2.)*KTITDIR*STH
C%%
      IANGLE = 0
      IF (THETA.EQ.90.  .AND.  KTITDIR.EQ.0) IANGLE = 90
C%%   CALL GSCHUP(0.+ZTHETA*I1MKTIT,1.+ZTHETA*I1MKTIT)
C%%   IF(KN.GT.0) CALL GTX(XT,YT,IBCD(1:KN))
      CALL GCHARA(IANGLE)
      IF(KN.GT.0) CALL GCHAR(IBCD,XT,YT,PHCHAR)
      IF(EX.EQ.0.0) GO TO 9
C--------------WRITE DECIMAL EXPONENT
      Z=KN
      ZDLKTIT = KTITDIR * (Z/2. - 4.) * SIZEW * STH
      IF (KTITDIR.EQ.1 .AND. Z.LE.7.) ZDLKTIT = (Z-7.)*SIZEW * STH
C
      XT=XT+Z*SIZEW*CTH + ZDLKTIT
      YT=YT+Z*SIZEW*STH*I1MKTIT - KTITDIR*STH*2.5*SIZEW
C
C%%   CALL GTX (XT,YT,' * 10')
      CALL GCHARA(IANGLE)
      CALL GCHAR(' * 10$',XT,YT,PHCHAR)
      XT=XT+5.1*SIZEW*(CTH+KTITDIR*STH) - 0.5*SIZEH*STH*I1MKTIT
      YT=YT+5.1*SIZEW*STH*I1MKTIT + 0.5*SIZEH*(CTH+KTITDIR*STH)
      KEX = -EX
      WRITE(TEXT,'(I3)') KEX
C&&&& CALL GSCHH(0.9*PHCHAR)
      IF (TEXT(1:2) .EQ. '  ') TEXT(1:2) = '+ '
      IF (TEXT(1:2) .EQ. ' -') TEXT(1:2) = '- '
      IF (TEXT(1:1) .EQ. ' ') TEXT(1:1) = '+'
      IF (TEXT(2:2) .EQ. ' ') THEN
           TEXT(2:2) = TEXT(3:3)
           TEXT(3:3) = ' '
      ENDIF
C%%   CALL GTX(XT,YT,TEXT)
      TEXT(4:4) = '$'
      CALL GCHARA(IANGLE)
      CALL GCHAR(TEXT,XT,YT,PHCHAR)
CC    CALL GSCHH(PHCHAR)
   9  CONTINUE
C--------------DRAW TICK MARKS AND AXIS
      ZXN(1) =X
      ZYN(1) =Y
      ZXN(2) =X+ S*CTH
      ZYN(2) =Y+ S*STH
C%%   CALL GPL(2,ZXN,ZYN)
      CALL GVECT(ZXN,ZYN,2)
      DXB= TICK*A*STH
      DYB=-TICK*A*CTH
      ZXN(1) = ZXN(1) - ZCTH
      ZYN(1) = ZYN(1) - ZSTH
      DO 13 I=1,NTIC
      ZXN(1) = ZXN(1) + ZCTH
      ZYN(1) = ZYN(1) + ZSTH
      ZXN(2) = ZXN(1) + DXB
      ZYN(2) = ZYN(1) + DYB
C%%   CALL GPL(2,ZXN,ZYN)
      CALL GVECT(ZXN,ZYN,2)
   13 CONTINUE
C
C                      DRAW GRID
      IF (SIZ2 .EQ. 0.0) GO TO  20
C%%   CALL GSLN(3)
C%%   CALL GSLWSC(1.)
      CALL GWICOL(-1.0,1)
      DGX= SIZ2 * STH
      DGY=-SIZ2 * CTH
      ZXN(1) = X - ZCTH
      ZYN(1) = Y - ZSTH
      DO 25 I=1,NTIC
      ZXN(1) = ZXN(1) + ZCTH
      ZYN(1) = ZYN(1) + ZSTH
      ZXN(2) = ZXN(1) + DGX
      ZYN(2) = ZYN(1) + DGY
C%%   CALL GPL(2,ZXN,ZYN)
      CALL GDASH(2)
      CALL GVECT(ZXN,ZYN,2)
   25 CONTINUE
C%%   CALL GSLN(1)
      CALL GDASH(0)
C%%   CALL GSLWSC(2.)
      CALL GWICOL(-2.0,1)
   20 CONTINUE
C
C--------------RE-DRAW THE MARKS AND AXIS AT PDIST IN PERPENDICULAR DIR.
      IF (PDIST .EQ. 0.0) GO TO 15
      ZXN(1) =X + PDIST*STH
      ZYN(1) =Y - PDIST*CTH
      ZXN(2) =ZXN(1) + S*CTH
      ZYN(2) =ZYN(1) + S*STH
C%%   CALL GPL(2,ZXN,ZYN)
      CALL GVECT(ZXN,ZYN,2)
      DXB=-TICK*A*STH
      DYB= TICK*A*CTH
      ZXN(1) = ZXN(1) - ZCTH
      ZYN(1) = ZYN(1) - ZSTH
      DO 14 I=1,NTIC
      ZXN(1) = ZXN(1) + ZCTH
      ZYN(1) = ZYN(1) + ZSTH
      ZXN(2) = ZXN(1) + DXB
      ZYN(2) = ZYN(1) + DYB
C%%   CALL GPL(2,ZXN,ZYN)
      CALL GVECT(ZXN,ZYN,2)
   14 CONTINUE
C
 15   CONTINUE
C%%   CALL GSCHUP(0.,1.)
C%%   CALL GSLN(1)
      CALL GDASH(0)
C%%   CALL GSLWSC(1.0)
      CALL GWICOL(-1.0,1)
C     CALL GSTXFP(1,2)
   99 RETURN
      END
         SUBROUTINE MAXAR(PA,KN,PMAX,PMIN)
C        *******************************
C
C     CALCULATES THE EXTREMAL VALUES OF PA(I),I=1,KN
C
      DIMENSION PA(KN)
      Z=PA(1)
      ZZ=PA(1)
      DO 10 I=2,KN
      IF (PA(I).GE.Z) Z=PA(I)
      IF (PA(I).LT.ZZ) ZZ=PA(I)
 10   CONTINUE
      PMAX=Z
      PMIN=ZZ
      RETURN
      END
         SUBROUTINE CADRE(PX,PY,PLX,PLY,KCONT)
C        *************************************
C
C     KCONT = 1 : CADRE COMPLET
C             2 : COINS DU CADRE UNIQUEMENT
C             0 : PAS DE CADRE
C---------------------------------------------------------
         IF (KCONT .EQ. 0) RETURN
         GO TO (100,200) KCONT
C
CL    DESSINE UN CADRE COMPLET
C
 100     CALL GPLOT(PX    ,PY    ,3)
         CALL GPLOT(PX    ,PY+PLY,2)
         CALL GPLOT(PX+PLX,PY+PLY,2)
         CALL GPLOT(PX+PLX,PY    ,2)
         CALL GPLOT(PX    ,PY    ,2)
C
         RETURN
C
CL    LEFT/BOTTOM CORNER
C
 200     CALL GPLOT(PX       ,PY+1.    ,3)
         CALL GPLOT(PX       ,PY       ,2)
         CALL GPLOT(PX+1.    ,PY       ,2)
C     LEFT/TOP CORNER
         CALL GPLOT(PX       ,PY+PLY-1.,3)
         CALL GPLOT(PX       ,PY+PLY   ,2)
         CALL GPLOT(PX+1.    ,PY+PLY   ,2)
C     RIGHT/TOP CORNER
         CALL GPLOT(PX+PLX-1.,PY+PLY   ,3)
         CALL GPLOT(PX+PLX   ,PY+PLY   ,2)
         CALL GPLOT(PX+PLX   ,PY+PLY-1.,2)
C     RIGHT/BOTTOM CORNER
         CALL GPLOT(PX+PLX   ,PY+1.    ,3)
         CALL GPLOT(PX+PLX   ,PY       ,2)
         CALL GPLOT(PX+PLX-1.,PY       ,2)
C
         RETURN
         END
         SUBROUTINE GPLOT(X,Y,I)
C        ***********************
C
C- FROM  USUAL PLOT SUBROUTINE IN VERSAPLOT PACKAGE (PLOTGKS)
C%
C%    INTERFACE FOR UNIRAS
C
C     (X,Y)            : POINT TO JOIN
C       I   =  1 OR 2  : DRAW THE SEGMENT
C       I   =  3       : DO NOT DRAW THE SEGMENT
C       I   <  0       : MOVE THE ORIGIN
C       I   =  999     : CALL GCLRWK(1,1) , I.E. SEPERATES PLOTS
C
C * POSITION ET ORIGINE COURANTES, EXTREMITES DU DESSIN (ABSOLUES)
      REAL XGKS(2),YGKS(2)
      SAVE   XOR,YOR,XGKS,YGKS
      DATA   XOR,YOR/0.0,0.0/
C
C * CAS DU TRAIT
      IPLUS=IABS(I)
      IF(IPLUS.EQ.999) GO TO 2
      XA=XOR+X
      YA=YOR+Y
      IF(IPLUS.EQ.3) THEN
        XGKS(1)=XA
        YGKS(1)=YA
      ELSE
        XGKS(2)=XA
        YGKS(2)=YA
C%%     CALL GPL(2,XGKS,YGKS)
        IF (IPLUS .NE. 1) THEN
          CALL GVECT(XGKS,YGKS,2)
        ELSE
C     AVOID HAVING GVECT(.,.,1) CALLING GPLOT, CALLING GVECT, ETC.
C     WHEN USING GVECT IN uni_to_ncar.f
C     UNIRAS: CALL GVECT(XGKS,YGKS,2)
C     NCAR:   CALL CURVED(XGKS,YGKS,2)
C          CALL CURVED(XGKS,YGKS,2)
          CALL GVECT(XGKS,YGKS,2)
        END IF
        XGKS(1)=XA
        YGKS(1)=YA
      ENDIF
C * CHANGER D'ORIGINE SI I EST NEGATIF
      IF(I.GT.0) RETURN
      XOR=XA
      YOR=YA
      RETURN
2     CONTINUE
C * CAS DE LA FIN DE DESSIN
      XOR=0.
      YOR=0.
C%%   CALL GCLRWK(1,1)
      CALL GCLEAR
      RETURN
      END
         SUBROUTINE RNDMXLG(PMAX)
C***********************************************************************
C
C        ROUND PMAX TO NEXT ((N*10)**K) INTEGER VALUE
C
         IF (PMAX .LE. 0.0) THEN
            WRITE(6,*) ' WARNING IN RNDMXLG: PMAX .LE. 0.0 : ',PMAX
            RETURN
         ENDIF
C
         ZMAXLOG = ALOG10(PMAX)
         IF (ZMAXLOG .GT. 0.0) ZMAXLOG = INT(ZMAXLOG) + 1
         IF (ZMAXLOG .LT. 0.0) ZMAXLOG = INT(ZMAXLOG)
C
         PMAX = 10**ZMAXLOG
C
         RETURN
         END
         SUBROUTINE RNDMNLG(PMIN)
C
C        ROUND PMIN TO PREVIOUS " INTEGER " LOGARITHM VALUE
C
         IF (PMIN .LE. 0.0) THEN
            WRITE(6,*) ' WARNING IN RNDMNLG: PMIN .LE. 0.0 : ',PMIN
            RETURN
         ENDIF
C
         ZMINLOG = ALOG10(PMIN)
         IF (ZMINLOG .GT. 0.0) ZMINLOG = INT(ZMINLOG)
         IF (ZMINLOG .LT. 0.0) ZMINLOG = INT(ZMINLOG) - 1.
C
         PMIN = 10**ZMINLOG
C
CC       WRITE(NNIN,'(" MISTAKE IN RNDMIN ")')
         RETURN
         END

      SUBROUTINE RTXHEI(PHCHAR)
C
      COMMON/RTX_UNI/RTX_UNI_H
C
      RTX_UNI_H = PHCHAR
C
      RETURN
      END

      SUBROUTINE RTX(KOPT,TITLE,PX0,PY0)
C
      CHARACTER*(*) TITLE
      CHARACTER*80 ZTITLE
      COMMON /RTX_UNI/  RTX_UNI_H
C
C   ADD $ SIGN AT END
      ILEN = LEN(TITLE)
      DO I=ILEN,1,-1
        IF (TITLE(I:I) .NE. ' ') GO TO 10
      END DO
   10 CONTINUE
      TITLE(I+1:I+1) = '$'
C
c%OS      DO I=1,80
c%OS        IF (TITLE(I:I) .NE. ' ') GO TO 10
c%OS      ENDDO
c%OS 10   I1 = I
c%OSC     DON'T PUSH LEFT LINE WITH 0.....(DPLACE,ETC)
c%OS      IF (TITLE(I1:I1) .EQ. '0') I1=1
c%OS      DO I=I1,80
c%OS        IF (TITLE(I:I) .EQ. '$') GO TO 11
c%OS      ENDDO
c%OS 11   I2 =I
c%OS      ZTITLE = TITLE(I1:I2)
c%OS      CALL GCHAR(ZTITLE,PX0,PY0,RTX_UNI_H)
      CALL GCHAR(TITLE,PX0,PY0,RTX_UNI_H)
C
      RETURN
      END

      SUBROUTINE GCHARDOL(TITLE,PX0,PY0,PHCHAR)
C
      CHARACTER*(*) TITLE
C
C   ADD $ SIGN AT END OF TITLE BEFORE CALLING GCHAR
C
      ILEN = LEN(TITLE)
      DO I=ILEN,1,-1
        IF (TITLE(I:I) .NE. ' ') GO TO 10
      END DO
   10 CONTINUE
      TITLE(I+1:I+1) = '$'
C
      CALL GCHAR(TITLE,PX0,PY0,PHCHAR)
C
      RETURN
      END

      SUBROUTINE RRECT (PX0,PY0,PX1,PY1,KCOLOR,KWIDTH)
C
      ZX0 = PX0
      ZY0 = PY0
      ZXLEN = (PX1-PX0)
      ZYLEN = (PY1-PY0)
      CALL CADRE(ZX0,ZY0,ZXLEN,ZYLEN,1)
C
      RETURN
      END

         FUNCTION ASINH(PX)
C        ******************
C
C     COMPUTE HYPERBOLIC ARG SINUS
C
         IF (PX.GE.0) THEN
            ASINH =   LOG( PX + SQRT(PX*PX+1.0))
         ELSE
            ASINH = - LOG(-PX + SQRT(PX*PX+1.0))
         ENDIF
C
         RETURN
         END

         SUBROUTINE AXASNH(X,Y,SIZE,XMIN,XMAX,THETA,PHCHAR,IBCD,N
     +                  ,KCHDIR,KTITDIR,PDIST,SIZ2)
C        ******************************************************
C     DRAWS A ASINH AXIS FROM ASINH(XMIN) TO ASINH(XMAX), LINEARLY
C
C     X,Y     WORLD COORDINATES AT BEGINNING OF AXIS
C     SIZE    AXIS LENGTH
C     XMIN    VALUE OF THE VARIABLE AT THE BEGINNING OF THE AXIS
C     XMAX      "   "   "     "     "   "  END       OF THE AXIS
C     THETA   ANGLE IN DEGREES
C     PHCHAR  HEIGHT OF CHARACTERS
C     IBCD    NAME OF AXIS
C     N       LENGTH OF NAME (NB OF CHARACTERS)
C             IF POSITIVE, NAME WILL BE PRINTED ON THE RIGHT SIDE
C             IF NEGATIVE,  "    "        "            LEFT   "
C     KCHDIR  DIRECTION OF AXIS NUMBERS (VERTICAL AXIS ONLY)
C             = 0 : CHARACTER PERP. TO AXIS (I.E. WRITING VERTICALLY  )
C             = 1 :    "   PARALLEL TO AXIS (I.E.    "    HORIZONTALLY)
C     KTITDIR DIRECTION OF CHARACTERS FOR THE TITLE (VERTICAL AXIS ONLY)
C             = 0 : CHARACTER PERP. TO AXIS (I.E. WRITING VERTICALLY  )
C             = 1 :    "   PARALLEL TO AXIS (I.E.    "    HORIZONTALLY)
C     PDIST   DISTANCE IN PERPEND. DIRECT. AT WHICH AXIS AND TICKS ARE RE-DRAWN
C             PDIST > 0  => ON THE RIGHT SIDE OF THE AXIS
C             PDIST < 0  => ON THE LEFT  SIDE "   "   "
C             IN GENERAL PDIST=SIZ2 IF (SIZ2 .NE. 0.)
C     SIZ2    GRID LENGTH
C             SIZ2  > 0  => ON THE RIGHT SIDE OF THE AXIS
C             SIZ2  < 0  => ON THE LEFT  SIDE "   "   "
C
C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C
C     FOR A SIMPLE AXIS FILL IN THE FIRST SIX PARAMETERS AS WELL AS IBCD
C     AND N, AND SET THE OTHERS TO THE FOLLOWING VALUES :
C           PHCHAR  = 0.30  OR  0.25
C           PDIST   = 0.0 OR SIZE OF OTHER AXIS
C           SIZ2    = 0.0
C           KCHDIR  = 0  FOR THETA=0.(ALWAYS)
C                   = 1  FOR THETA=90.
C           KTITDIR = 0  (ALWAYS FOR THETA=0)
C    !!! PDIST AND SIZ2 SHOULD HAVE SAME SIGN AND OPPOSITE TO SIGN OF N
C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C-C
C
      DIMENSION ZXN(2),      ZYN(2)
      CHARACTER TEXT*10 ,IBCD*(*)
C     DATA SIZEN,SIZEL,TICK/0.02,0.04,0.02/
      ZXMAX = XMAX
      ZXMIN = XMIN
      DX = ZXMAX - ZXMIN
      DXASH = ASINH(ZXMAX) - ASINH(ZXMIN)
      S = SIZE
C
CL       DEFINE BASIC ATTRIBUTES
C
C                      FOR POLYLINE
      CALL GDASH(0)
      CALL GWICOL(-2.0,1)
C     CALL GQTXX(1,X,Y,'ABCDEFGHIJ',IERR,CPX,CPY,TX,TY)
C     SIZEW = (TX(3)-TX(1))/10.
C     SIZEL = (TY(3)-TY(1))*0.5
      SIZEW = 0.947*PHCHAR
      SIZEH = PHCHAR
      TICK  = SIZEH/2.0
      ZTHETA = 0.
C%%
      IANGLE = 0
      IF (THETA .EQ. 90.) ZTHETA = -1.
      IF (KCHDIR.EQ.0 .AND. THETA.EQ.90.) IANGLE = 90
      I1MKCH = 1 - KCHDIR
      I1MKTIT = 1 - KTITDIR
      ADX = AMAX1(ABS(XMIN),ABS(DX),ABS(XMIN+DX))
C--------------RETURN IF DX=0
      IF(ADX.NE.0.) GO TO 30
      GO TO 99
30    CONTINUE
      ANG=THETA*0.017453292
      CTH=COS(ANG)
      STH=SIN(ANG)
C
C  (NTIC-1) = NUMBER OF INTERVALS
      NTIC = 11
      CHDEC = SIZE / (SIZEW*(9.)*I1MKCH + PHCHAR*(1.+2.*KCHDIR))
      IF (CHDEC.GE.45.                   ) NTIC = 51
      IF (CHDEC.GE.35. .AND. CHDEC.LT.45.) NTIC = 41
      IF (CHDEC.GE.25. .AND. CHDEC.LT.35.) NTIC = 31
      IF (CHDEC.GE.15. .AND. CHDEC.LT.25.) NTIC = 21
      IF (CHDEC.GE.3.8 .AND. CHDEC.LT.4.8) NTIC = 9
      IF (CHDEC.GE.2.8 .AND. CHDEC.LT.3.8) NTIC = 7
      IF (CHDEC.GE.1.8 .AND. CHDEC.LT.2.8) NTIC = 5
      IF (                   CHDEC.LT.1.8) NTIC = 3
      KN=N
      A=-1.0
C--------------POSITIVE N FOR ANNOTATION ON CLOCKWISE SIDE OF AXIS
      IF(ISIGN(1,KN).GE.0) GO TO 2
      A=-A
      KN=-KN
C--------------DETERMINE POSITION OF ANNOTATION
 2    ZCTH = CTH*S/(NTIC-1.)
      ZSTH = STH*S/(NTIC-1.)
      DXN=-SIZEW*4.0
C        ZA0 = 1 IF TITLE ON LEFT SIDE, 0 OTHERWISE
      ZA0 = (A+1.)/2.
      DYN= 1.5*TICK*A - (1.-ZA0)*SIZEH + 3.*SIZEW*KCHDIR
C
      XN = X +  DXN*CTH - (DYN             )*STH -
     -     (ZA0*(4.) + (1.-ZA0)*2.5)*KCHDIR*STH*SIZEW
      IF (XMIN .LT. 0.0) XN = XN + STH*(1.-ZA0)*SIZEW*1.05*KCHDIR
C
      YN = Y + (DYN*CTH+DXN*STH)*I1MKCH -
     -         SIZEH/2.*KCHDIR*STH
      NT=NTIC/2
C--------------WRITE NUMERIC   INTERVALS
      X0 = ZXMIN
      ZDX0 = DXASH/(NTIC-1.)
      DO 8 I=1,NTIC,2
      WRITE(TEXT,'(1PE8.1,"$")') X0
C
C     RATIO WIDTH '-' OVER ' '
      ZRAT = 18./35.
      IF (TEXT(1:1) .EQ. '-') XN = XN - ZRAT*SIZEW*(CTH+KCHDIR)
      IF (TEXT(1:1) .EQ. '-') YN = YN - ZRAT*SIZEW*STH*I1MKCH
C
C     RATIO WIDTH '1' OVER '2'
      ZRAT12 = 1./4.
      IF (TEXT(2:2) .EQ. '1') XN = XN + ZRAT12*SIZEW*(CTH+KCHDIR)
      IF (TEXT(2:2) .EQ. '1') YN = YN + ZRAT12*SIZEW*STH*I1MKCH
C
      CALL GCHARA(IANGLE)
      CALL GCHAR(TEXT,XN,YN,PHCHAR)
      IF (TEXT(1:1) .EQ. '-') XN = XN + ZRAT*SIZEW*(CTH+KCHDIR)
      IF (TEXT(1:1) .EQ. '-') YN = YN + ZRAT*SIZEW*STH*I1MKCH
      IF (TEXT(2:2) .EQ. '1') XN = XN - ZRAT12*SIZEW*(CTH+KCHDIR)
      IF (TEXT(2:2) .EQ. '1') YN = YN - ZRAT12*SIZEW*STH*I1MKCH
      X0 = SINH(ASINH(X0) + 2.*ZDX0)
      XN=XN+2.*ZCTH
      YN=YN+2.*ZSTH
 8    CONTINUE
C--------------WRITE AXIS LABEL
      Z=KN
      ZZ = KN
      Z=Z/2.
      DXB=S*0.5-Z*SIZEW
      DYB=(0.8*TICK+SIZEW*3.5)*A -0.5*SIZEW
      ZDYY = 7.*0.4*SIZEW * A * KCHDIR
C
      XT= X+DXB*CTH-(DYB-2.*ZA0*SIZEW+ZDYY+(1.-ZA0)*(SIZEW-2.*TICK))*STH
     -   - (ZA0*SIZEW*(ZZ-3.*KCHDIR) + (1.-ZA0)*1.0*SIZEW)*KTITDIR*STH -
     -                (A*(5.)*SIZEW+(1.-ZA0)*3.2*SIZEW)*KCHDIR*STH
      XT = XT - I1MKCH*SIZEW*(1.4+0.5*A-(3.20+0.20*A)*SIZEW*KTITDIR)*STH
      XT = XT + I1MKTIT*STH*SIZEW*(I1MKCH*(1.-ZA0) + KCHDIR*ZA0*1.2)
      IF (XMIN .LT. 0.0) XT = XT - A*KCHDIR*SIZEW*STH
C
      YT = Y + DYB*CTH + DXB*STH *I1MKTIT + (0.5*S-SIZEH/2.)*KTITDIR*STH
C%%
      IANGLE = 0
      IF (THETA.EQ.90.  .AND.  KTITDIR.EQ.0) IANGLE = 90
      CALL GCHARA(IANGLE)
      IF(KN.GT.0) CALL GCHAR(IBCD,XT,YT,PHCHAR)
C--------------DRAW TICK MARKS AND AXIS
      ZXN(1) =X
      ZYN(1) =Y
      ZXN(2) =X+ S*CTH
      ZYN(2) =Y+ S*STH
      CALL GVECT(ZXN,ZYN,2)
      DXB= TICK*A*STH
      DYB=-TICK*A*CTH
      ZXN(1) = ZXN(1) - ZCTH
      ZYN(1) = ZYN(1) - ZSTH
      DO 13 I=1,NTIC
      ZXN(1) = ZXN(1) + ZCTH
      ZYN(1) = ZYN(1) + ZSTH
      ZXN(2) = ZXN(1) + DXB
      ZYN(2) = ZYN(1) + DYB
      CALL GVECT(ZXN,ZYN,2)
   13 CONTINUE
C
C                      DRAW GRID
      IF (SIZ2 .EQ. 0.0) GO TO  20
      CALL GWICOL(-1.0,1)
      DGX= SIZ2 * STH
      DGY=-SIZ2 * CTH
      ZXN(1) = X - ZCTH
      ZYN(1) = Y - ZSTH
      DO 25 I=1,NTIC
      ZXN(1) = ZXN(1) + ZCTH
      ZYN(1) = ZYN(1) + ZSTH
      ZXN(2) = ZXN(1) + DGX
      ZYN(2) = ZYN(1) + DGY
      CALL GDASH(2)
      CALL GVECT(ZXN,ZYN,2)
   25 CONTINUE
      CALL GDASH(0)
      CALL GWICOL(-2.0,1)
   20 CONTINUE
C
C--------------RE-DRAW THE MARKS AND AXIS AT PDIST IN PERPENDICULAR DIR.
      IF (PDIST .EQ. 0.0) GO TO 15
      ZXN(1) =X + PDIST*STH
      ZYN(1) =Y - PDIST*CTH
      ZXN(2) =ZXN(1) + S*CTH
      ZYN(2) =ZYN(1) + S*STH
      CALL GVECT(ZXN,ZYN,2)
      DXB=-TICK*A*STH
      DYB= TICK*A*CTH
      ZXN(1) = ZXN(1) - ZCTH
      ZYN(1) = ZYN(1) - ZSTH
      DO 14 I=1,NTIC
      ZXN(1) = ZXN(1) + ZCTH
      ZYN(1) = ZYN(1) + ZSTH
      ZXN(2) = ZXN(1) + DXB
      ZYN(2) = ZYN(1) + DYB
      CALL GVECT(ZXN,ZYN,2)
   14 CONTINUE
C
 15   CONTINUE
      CALL GDASH(0)
      CALL GWICOL(-1.0,1)
C     CALL GSTXFP(1,2)
   99 RETURN
      END
