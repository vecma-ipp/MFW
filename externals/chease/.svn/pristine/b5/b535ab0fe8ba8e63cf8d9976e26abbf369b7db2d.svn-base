C
C     INTERFACE FOR UNIRAS CODES TO BE ABLE TO RUN WITH NCAR
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


         SUBROUTINE GVECT(PX,PY,KN)
C     PLOT A POLYLINE
C
         DIMENSION PX(KN),   PY(KN)
C
         IF (KN .GE. 2) THEN
           CALL CURVED(PX,PY,KN)
         ELSE IF (KN .EQ. 0) THEN
           CALL GPLOT(PX,PY,3)
         ELSE IF (KN .EQ. 1) THEN
           CALL GPLOT(PX,PY,1)
         ENDIF
C
         RETURN
         END
         SUBROUTINE GDOT(PX,PY,KN)
C        -------------------------
C
C     PLOT MARKS ON EACH COORDINATE
C
         DIMENSION PX(KN),   PY(KN)
C
c%OS         CALL GWICOL(2.0,0)
         CALL GSMKSC (0.3)
         CALL GSPMCI (1)
         CALL POINTS(PX,PY,KN,-4,0) !  (empty circles)
c         CALL NGDOTS(PX,PY,KN,1.5,1) ! (for filled dots)
C
         RETURN
         END
         SUBROUTINE GCHAR(CTEXT,PX,PY,PHCHAR)
C        ------------------------------------
C
C     DRAW STRING CTEXT
C
         CHARACTER*(*)       CTEXT
         CHARACTER*90        ZTXT
C
         COMMON/CHRSPE/NANGLE, AHCHAR
C
C  REMOVE FIRST $ SIGN IF ANY
         ILEN = LEN(CTEXT)
         DO 10 I=1,ILEN
            IF (CTEXT(I:I) .EQ. '$') GO TO 11
 10      CONTINUE
         I = ILEN + 1
 11      ILAST = I - 1
         ZTXT = CTEXT(1:ILAST)
C%         CALL GSCHH(0.012)
c%OS         AHCHAR = MAX(2.0,PHCHAR)
         AHCHAR = PHCHAR
c%OS
c%OS          CALL WTSTR(PX,PY,ZTXT,INT(AHCHAR/2.2*8.),NANGLE,-1)
         CALL WTSTR(PX,PY,ZTXT,INT(AHCHAR/2.5*8.),NANGLE,-1)
C     RTURN TO DEFAULT VALUE FOR THE ANGLE
         NANGLE = 0.0
C
         RETURN
         END
         SUBROUTINE GDASH(K)
C        -------------------
C
C     CHOOSE TYPE OF LINE
C
         DIMENSION IPAT(0:7)
C
C
C Set a solid dash pattern,  1111111111111111 (BINARY).
C Boolean operations (using locally-implemented support
C routines) are used to support porting to hosts with 16
C bit integers.
C
C     ISHIFT MULITPIES BY 2 (SHIFTS PATTERN TO LEFT)
C     IOR ADDS 1 IF NUMBER WAS EVEN (SETS ONE AS LAST BIT)
C     THUS IPAT(1) = 32767*2+1 IN DECIMAL
C
      IPAT(0) = IOR (ISHIFT (32767,1), 1)
C
C Array IPAT contains 5 different 16-BIT dash patterns.  The patterns
C are constructed with boolean operations as shown above.
C The binary representations of the patterns are
C        1010101010101010
C        0001110001111111
C        1111110011111100
C        1100110011001100
C        1111000011110000
C        1111111111111100
C        1111111100000000
C
      IPAT(1) = ISHIFT (21845,1)
      IPAT(2) = IOR (ISHIFT ( 3647,1), 1)
      IPAT(3) = ISHIFT (32382,1)
      IPAT(4) = ISHIFT (26214,1)
      IPAT(5) = ISHIFT (30840,1)
      IPAT(6) = ISHIFT (32766,1)
      IPAT(7) = ISHIFT (32640,1)
C
         CALL DASHDB(IPAT(MOD(K,8)))
C
         RETURN
         END
         SUBROUTINE GCHARA(KANGLE)
C        -------------------------
C
C     DEFINE ANGLE FOR TEXT
C
         COMMON/CHRSPE/NANGLE, AHCHAR
C
         NANGLE = KANGLE
C
         RETURN
         END
         SUBROUTINE GWICOL(PSIZE,KCOL)
C        -----------------------------
C
C     SPECIFIES WIDTH OF LINE
C
cccc         CALL SETUSV('LW',INT(ABS(PSIZE+1.)))
         CALL GSLWSC(ABS(PSIZE))
C
         RETURN
         END

      SUBROUTINE GROUTE(PTEXT)
C
      CHARACTER*(*)PTEXT
C
      RETURN
      END

      SUBROUTINE GOPEN
C
      CALL GOPKS(6,1)
      CALL GOPWK(1,2,1)
      CALL GACWK(1)
C
      RETURN
      END

      SUBROUTINE GCLOSE
C
      CALL GDAWK(1)
      CALL GCLWK(1)
      CALL GCLKS
C
      RETURN
      END

      SUBROUTINE GSEGCR(KSEG)
C
      RETURN
      END

      SUBROUTINE GSEGCL(KSEG)
C
c%OS      CALL FRAME
C
      RETURN
      END

      SUBROUTINE GCLEAR
C
      CALL FRAME
C
      RETURN
      END

      SUBROUTINE GLIMIT(P1,P2,P3,P4,P5,P6)
C
      RETURN
      END

      SUBROUTINE GWBOX(P1,P2,P3)
C
      RETURN
      END

      SUBROUTINE GVPORT(PX0,PY0,PX1,PY1)
C
      DATA ICALL/0/
      SAVE ICALL
C
      IF (ICALL .EQ. 0) THEN
        ZXNORM=1.0
        ZYNORM=(PY1-PY0)/(PX1-PX0)
        IF (ZYNORM .GT. 1.0) THEN
          ZXNORM=1./ZYNORM
          ZYNORM=1.
        ENDIF
        CALL SET(0.,ZXNORM,0.,ZYNORM,PX0,PX1,PY0,PY1,1)
      ENDIF
      ICALL = 1
C
      RETURN
      END
