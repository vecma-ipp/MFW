C
C     INTERFACE FOR UNIRAS CODES TO BE ABLE TO RUN WITH NCAR
C
C     ROUTINES USED IN PCHEASE_UNI
C
C     GRAPHMB
C     GLASHMB
C     GRAPNMB
C     GLASNMB
C     
C     FOLLOWING ROUTINES ARE DEFINED:
C
C     GVECT(PX,PY,KN)
C     GDOT(PX,PY,KN)
C     GCHAR(CTEXT,PX,PY,PHCHAR)
C     GDASH(K)
C     GCHARA(KANGLE)
C     GWICOL(PSIZE,KCOL)
C     GROUTE(PTEXT)
C     GOPEN
C     GCLOSE
C     GSEGCR(KSEG)
C     GSEGCL(KSEG)
C     GCLEAR
C


      SUBROUTINE GRAPHMB(PX,PY,KN,PX0,PDX,PY0,PDY,PHCHAR,PXIBCD,PYIBCD
     +  ,KX,KY,PSIZLN,KCADRE,KGRID,
     +  PXMIN,PXMAX,PYMIN,PYMAX)
C        ***************************************************************
C
C        DRAW THE GRAPH OF A FONCTION GIVEN BY KN POINTS (PX(I),PY(I))
C        IN A RECTANGULAR BOX ((PX0,PY0) ; (PX0+PDX,PY0+PDY)
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
C
         CHARACTER PXIBCD*(*),PYIBCD*(*)
         DIMENSION  PX(KN),PY(KN)
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
         WRITE(NPLOT,'(A)') 'GRAPHMB'
C
         WRITE(NPLOT,*) KN , PX0 , PDX , PY0 , PDY , PHCHAR
         WRITE(NPLOT,*) PXMIN,PXMAX,PYMIN,PYMAX
C
         DO 100 II=1,KN
            IF (ABS(PY(II)).LE.1.0E-500 .AND. PY(II).NE.0.0) 
     +          PY(II) = SIGN(1.,PY(II)) * 1.0E-500
 100     CONTINUE
C
         WRITE(NPLOT,'(1P10E13.5)') (PX(I),I=1,KN)
         WRITE(NPLOT,'(1P10E13.5)') (PY(I),I=1,KN)
         WRITE(NPLOT,'(A)') PXIBCD
         WRITE(NPLOT,'(A)') PYIBCD
         WRITE(NPLOT,*) KX , KY , PSIZLN , KCADRE , KGRID
C
         RETURN
         END

      SUBROUTINE GLASHMB(PX,PY,KN,PX0,PDX,PY0,PDY,PHCHAR,PXIBCD,PYIBCD
     +  ,KX,KY,PSIZLN,KCADRE,KGRID,
     +  PXMIN,PXMAX,PYMIN,PYMAX)
C        ***************************************************************
C
C        DRAW THE GRAPH OF A FONCTION GIVEN BY KN POINTS (PX(I),PY(I))
C        IN A RECTANGULAR BOX ((PX0,PY0) ; (PX0+PDX,PY0+PDY)
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
C
         CHARACTER PXIBCD*(*),PYIBCD*(*)
         DIMENSION  PX(KN),PY(KN)
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
         WRITE(NPLOT,'(A)') 'GLASHMB'
C
         WRITE(NPLOT,*) KN , PX0 , PDX , PY0 , PDY , PHCHAR
         WRITE(NPLOT,*) PXMIN,PXMAX,PYMIN,PYMAX
C
         DO 100 II=1,KN
            IF (ABS(PY(II)).LE.1.0E-500 .AND. PY(II).NE.0.0) 
     +          PY(II) = SIGN(1.,PY(II)) * 1.0E-500
 100     CONTINUE
C
         WRITE(NPLOT,'(1P10E13.5)') (PX(I),I=1,KN)
         WRITE(NPLOT,'(1P10E13.5)') (PY(I),I=1,KN)
         WRITE(NPLOT,'(A)') PXIBCD
         WRITE(NPLOT,'(A)') PYIBCD
         WRITE(NPLOT,*) KX , KY , PSIZLN , KCADRE , KGRID
C
         RETURN
         END

      SUBROUTINE GRAPNMB(PX,PY,MDKN,KN,KPLOT,PX0,PDX,PY0,PDY,PHCHAR,
     +  PXIBCD,PYIBCD,KX,KY,PSIZLN,KCADRE,KGRID,
     +  PXMIN,PXMAX,PYMIN,PYMAX)
C        ***************************************************************
C
C     PLOT KPLOT CURVES
C
C        DRAW THE GRAPH OF A FONCTION GIVEN BY KN POINTS (PX(I),PY(I))
C        IN A RECTANGULAR BOX ((PX0,PY0) ; (PX0+PDX,PY0+PDY)
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
C
         CHARACTER*(*) PXIBCD,PYIBCD(KPLOT+1)
         DIMENSION  PX(KN),PY(MDKN,KPLOT), KY(KPLOT+1)
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
         WRITE(NPLOT,'(A)') 'GRAPNMB'
C
         WRITE(NPLOT,*) KN , KPLOT , PX0 , PDX , PY0 , PDY , PHCHAR
         WRITE(NPLOT,'(1P10E13.5)') (PX(I),I=1,KN)
C
         DO 100 IP=1,KPLOT
            DO 101 I=1,KN
               IF (ABS(PY(I,IP)).LE.1.0E-500 .AND. PY(I,IP).NE.0.0) 
     +             PY(I,IP) = SIGN(1.,PY(I,IP)) * 1.0E-500
 101        CONTINUE
 100     CONTINUE
C
         DO 110 IP=1,KPLOT
            WRITE(NPLOT,'(1P10E13.5)') (PY(I,IP),I=1,KN)
 110     CONTINUE
         WRITE(NPLOT,'(A)') PXIBCD
         WRITE(NPLOT,'(A)') (PYIBCD(I),I=1,KPLOT+1)
         WRITE(NPLOT,*) KX, (KY(I),I=1,KPLOT+1), PSIZLN, KCADRE,KGRID
         WRITE(NPLOT,*) PXMIN,PXMAX,PYMIN,PYMAX
C
         RETURN
         END



      SUBROUTINE GLASNMB(PX,PY,MDKN,KN,KPLOT,PX0,PDX,PY0,PDY,PHCHAR,
     +  PXIBCD,PYIBCD,KX,KY,PSIZLN,KCADRE,KGRID,
     +  PXMIN,PXMAX,PYMIN,PYMAX)
C        ***************************************************************
C
C     PLOT KPLOT CURVES
C
C        DRAW THE GRAPH OF A FONCTION GIVEN BY KN POINTS (PX(I),PY(I))
C        IN A RECTANGULAR BOX ((PX0,PY0) ; (PX0+PDX,PY0+PDY)
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
C
         CHARACTER*(*) PXIBCD,PYIBCD(KPLOT+1)
         DIMENSION  PX(KN),PY(MDKN,KN), KY(KPLOT+1)
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
         WRITE(NPLOT,'(A)') 'GLASNMB'
C
         WRITE(NPLOT,*) KN , KPLOT , PX0 , PDX , PY0 , PDY , PHCHAR
         WRITE(NPLOT,'(1P10E13.5)') (PX(I),I=1,KN)
C
         DO 100 IP=1,KPLOT
            DO 101 I=1,KN
               IF (ABS(PY(I,IP)).LE.1.0E-500 .AND. PY(I,IP).NE.0.0) 
     +             PY(I,IP) = SIGN(1.,PY(I,IP)) * 1.0E-500
 101        CONTINUE
 100     CONTINUE
C
         DO 110 IP=1,KPLOT
            WRITE(NPLOT,'(1P10E13.5)') (PY(I,IP),I=1,KN)
 110     CONTINUE
         WRITE(NPLOT,'(A)') PXIBCD
         WRITE(NPLOT,'(A)') (PYIBCD(I),I=1,KPLOT+1)
         WRITE(NPLOT,*) KX, (KY(I),I=1,KPLOT+1), PSIZLN, KCADRE,KGRID
         WRITE(NPLOT,*) PXMIN,PXMAX,PYMIN,PYMAX
C
         RETURN
         END

         SUBROUTINE GVECT(PX,PY,KN)
C
         DIMENSION PX(KN), PY(KN)
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
         WRITE(NPLOT,'(A)') 'GVECT'
C
         WRITE(NPLOT,*) KN
         WRITE(NPLOT,'(1P10E13.5)') (PX(I),I=1,KN)
         WRITE(NPLOT,'(1P10E13.5)') (PY(I),I=1,KN)
C
         RETURN
         END
         SUBROUTINE GDOT(PX,PY,KN)
C
         DIMENSION PX(KN), PY(KN)
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
         WRITE(NPLOT,'(A)') 'GDOT'
C
         WRITE(NPLOT,*) KN
         WRITE(NPLOT,'(1P10E13.5)') (PX(I),I=1,KN)
         WRITE(NPLOT,'(1P10E13.5)') (PY(I),I=1,KN)
C
         RETURN
         END
         SUBROUTINE GCHAR(PCTEXT,PX0,PY0,PHCHAR)
         CHARACTER*(*) PCTEXT
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
         WRITE(NPLOT,'(A)') 'GCHAR'
C
         WRITE(NPLOT,*) PX0 , PY0, PHCHAR
         WRITE(NPLOT,'(A)') PCTEXT
C
         RETURN
         END
         SUBROUTINE GDASH(K)
C
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
         WRITE(NPLOT,'(A)') 'GDASH'
C
         WRITE(NPLOT,*) K
C
         RETURN
         END
         SUBROUTINE GCHARA(KANGLE)
C        -------------------------
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
         WRITE(NPLOT,'(A)') 'GCHARA'
C
         WRITE(NPLOT,*) KANGLE
C
         RETURN
         END
         SUBROUTINE GWICOL(PSIZE,KCOL)
C        -----------------------------
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
         WRITE(NPLOT,'(A)') 'GWICOL'
C
         WRITE(NPLOT,*) PSIZE,KCOL
C
         RETURN
         END

      SUBROUTINE GROUTE(PTEXT)
C
      CHARACTER*(*)PTEXT
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
c%OS         WRITE(NPLOT,'(A)') 'GROUTE'
c%OSC
c%OS         WRITE(NPLOT,'(A)') PTEXT
C
      RETURN
      END

      SUBROUTINE GOPEN
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
c%OS         WRITE(NPLOT,'(A)') 'GOPEN'
C
      RETURN
      END
      SUBROUTINE GCLOSE
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
         WRITE(NPLOT,'(A)') 'GCLOSE'
C
      RETURN
      END

      SUBROUTINE GSEGCR(KSEG)
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
c%OS         WRITE(NPLOT,'(A)') 'GSEGCR'
c%OSC
c%OS         WRITE(NPLOT,*) KSEG
C
      RETURN
      END

      SUBROUTINE GSEGCL(KSEG)
C
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
c%OS         WRITE(NPLOT,'(A)') 'GSEGCL'
c%OSC
c%OS         WRITE(NPLOT,*) KSEG
C
      RETURN
      END

      SUBROUTINE GCLEAR
C
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
         WRITE(NPLOT,'(A)') 'GCLEAR'
C
      RETURN
      END

      SUBROUTINE GLIMIT(P1,P2,P3,P4,P5,P6)
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
         DATA    ICALL/0/
         SAVE ICALL
C-----------------------------------------------------------------------
C
         IF (ICALL .EQ. 1) RETURN
C
         ICALL = 1
         WRITE(NPLOT,'(A)') 'PLOTS'
C
         WRITE(NPLOT,*) P2/10.,P4/10.
C
      RETURN
      END

      SUBROUTINE GWBOX(P1,P2,P3)
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
         WRITE(NPLOT,'(A)') 'GWBOX'
C
         WRITE(NPLOT,*) P1,P2,P3
C
      RETURN
      END

      SUBROUTINE GVPORT(PX0,PY0,PX1,PY1)
C-----------------------------------------------------------------------
         DATA    NPLOT/8/
C-----------------------------------------------------------------------
C
         WRITE(NPLOT,'(A)') 'GVPORT'
C
         WRITE(NPLOT,*) PX0,PY0,PX1,PY1
C
      RETURN
      END
