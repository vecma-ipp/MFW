      SUBROUTINE PPP
C
C***********************************************************************
C     DUMMY HEADING FOR THE FORTRAN SOURCE OF LIBRARY PPPLIB.  "PPP"   *
C STANDS FOR "PLASMA PHYSICS PLOTTING (PACKAGE)".  IT IS DERIVED FROM  *
C THE SIMILAR PLOTTING PACKAGE P4, DEVELOPED AT LOS ALAMOS SCIENTIFIC  *
C LABORATORY BY CLAIR NIELSON, BRENDAN GODFREY, DENNIS HEWETT, DEBORAH *
C HYMAN, AND ROBERT MALONE, STARTING FROM BASIC LASL PLOTTING ROUTINES *
C WHICH WERE WRITTEN BY R.M. FRANK, GENE WILLBANKS, AND OTHERS (WHOSE  *
C NAMES WE WERE NOT ABLE TO TRACE).  THE PACKAGE HAS BEEN OBTAINED IN  *
C AUG '83 FROM DENNIS HEWETT, WHO DOES NOT WANT TO BE HELD RESPONSIBLE *
C THOUGH FOR PROBLEMS ARISING WITH ITS USE.                            *
C                                                                      *
C     PPPLIB HAS BEEN ADAPTED IN '84 AND '85 FOR USE ON THE CYBERS 750 *
C AND 205 OF SARA AT AMSTERDAM BY HANS GOEDBLOED AND DICK HOGEWEIJ.    *
C THE ORIGINAL TEXT REFERING TO THE MFECC CRAY1 COMPUTERS AT LIVERMORE *
C HAS BEEN CONSERVED BY ENCLOSING IT BETWEEN THE REVISE DIRECTIVES     *
C "*IF MFE", OR "*ELSEIF MFE", AND "*ENDIF".                           *
C                                                                      *
C     THE CALLS OF THE LOWEST LEVEL GRAPHICS SUPPORT ROUTINES, WHICH   *
C ARE NECESSARILY SYSTEM DEPENDENT, APPEAR AT THE END OF THE PACKAGE.  *
C FOR USE ON THE MFECC CRAY1 COMPUTERS THE PRIMITIVE PLOTTING ROUTINES *
C ARE TAKEN FROM THE LIBRARY TV80LIB.  FOR USE ON THE SARA COMPUTERS   *
C THE PRIMITIVE PLOTTING ROUTINES ARE TAKEN FROM THE CALCOMP LIBRARY.  *
C                                                                      *
C     MODIFICATION BY HANS GOEDBLOED 31/10/85: TRANSITION TO STANDARD  *
C     FORTRAN 77, REPLACING ALL OPERATIONS INVOLVING HOLLERITHS,       *
C     WORD LENGTH, AND OCTAL REPRESENTATIONS BY MACHINE-INDEPENDENT    *
C     MANIPULATIONS AND DECIMAL INTEGER REPRESENTATIONS.               *
C                                                                      *
C     GUIDO HUYSMANS, STEFAAN POEDTS, AND HANS GOEDBLOED 1/09/91:      *
C     ADAPTATION TO THE IBM 3090 COMPUTERS AT SARA AND KUL.            *
C                                                                      *
C     HANS GOEDBLOED, GUIDO HUYSMANS, AND EGBERT WESTERHOF 11/11/91:   *
C     SEPARATE BRANCH CREATING LASERWRITER POSTSCRIPT FILES.           *
C                                                                      *
C     GUIDO HUYSMANS 27/7/99                                           *
C     CONVERTED PLOT COORDINATES TO REAL TO IMPROVE RESOLUTION         *
C     ADDED NEW ROUTINE CPLOTM (DERIVED FROM CPLOT) FOR CONTOUR PLOTS  *
C     ON IRREGULAR BUT ORDERED GRIDS.
C                                                                      *
C***********************************************************************
C
      use itm_types
      implicit none
      WRITE(*,10)
   10 FORMAT(/1X,'LIBRARY PPPLIB'/1X,'VERSION 15, D.D. 7/12/91')
      RETURN
      END
C
      SUBROUTINE LPLOT6(MX,MY,X,Y,NPTS,TITLE)
C
C***********************************************************************
C     LPLOT6 PLOTS THE VALUES IN THE ARRAYS X AND Y AND CONNECTS THEM  *
C WITH A  LINE.  IT DRAWS A BOX AROUND THE PLOT WITH LINEAR-LINEAR     *
C SCALING AND PLACES LABELS ON LEFT AND BOTTOM AXES.  NPTS IS THE      *
C DIMENSION OF THE PLOTTED ARRAYS.  TITLE CONTAINS A TITLE OF AN ARBI- *
C TRARY NUMBER OF CHARACTERS TO BE DRAWN AT THE TOP OF THE GRAPH.  THE *
C POSITION OF THE PLOT ON THE PAGE IS DETERMINED BY MX,MY (SEE LPLOT). *
C                                                                      *
C     WRITTEN BY DEBBY HYMAN, 7-79                                     *
C     REMOVED NTITLE FROM ARGUMENT LIST, HGO 4/12/85.                  *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) X(*),Y(*)
      integer MX,MY,NPTS
      CHARACTER*(*) TITLE
C
      CALL LPLOT(MX,MY,1,X,Y,NPTS,1,TITLE,LEN(TITLE),'X',1,'Y',1)
      RETURN
      END
C
      SUBROUTINE HPLOT6(MX,MY,X,Y,NPTS,TITLE)
C
C***********************************************************************
C     HPLOT6 DRAWS A HISTOGRAM OF THE VALUES IN THE ARRAYS X AND Y     *
C WHICH BOTH CONTAIN NPTS POINTS.  TITLE CONTAINS A TITLE OF AN ARBI-  *
C TRARY NUMBER OF CHARACTERS TO BE DRAWN AT THE TOP OF THE GRAPH.  THE *
C POSITION OF THE PLOT ON THE PAGE IS DETERMINED BY MX,MY (SEE LPLOT). *
C                                                                      *
C     WRITTEN BY DEBBY HYMAN, 7-79                                     *
C     REMOVED NTITLE FROM ARGUMENT LIST, HGO 4/12/85.                  *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) X(*),Y(*)
      integer MX,MY,NPTS
      CHARACTER*(*) TITLE
C
      CALL HPLOT(MX,MY,1,X,Y,NPTS,1,TITLE,LEN(TITLE),'X',1,'Y',1)
      RETURN
      END
C
      SUBROUTINE CPLOT8(MX,MY,X,Y,NX,NY,Z,TITLE)
C
C***********************************************************************
C     A CONTOUR PLOT DISPLAYS THE SHAPE OF A SURFACE Z = F(X,Y) BY     *
C TRACING OUT LINES THAT CONNECT POINTS OF EQUAL VALUE ON THE SURFACE. *
C THERE ARE TWO CONTOUR PLOTTING SUBROUTINES IN PPPLIB.  IN THE HIGH-  *
C LEVEL (EASIER TO USE) ROUTINE CPLOT8, THE MATRIX Z(I,J) IS DIMEN-    *
C SIONED NX,NY.  IT CONTAINS THE VALUES OF THE FUNCTION F(X(I),Y(J)),  *
C WHERE I=1,2,..NX, AND J=1,2,..,NY.  TITLE CONTAINS A TITLE OF AN     *
C ARBITRARY NUMBER OF CHARACTERS TO BE DRAWN AT THE TOP OF THE GRAPH.  *
C CPLOT8 SCALES THE PLOT DYNAMICALLY AND TRACES THE SURFACE WITH NC    *
C CONTOUR LINES, WHERE NC IS FIXED IN THE PARAMETER STATEMENT BELOW.   *
C THE POSITION OF THE PLOT ON THE PAGE IS DETERMINED BY MX,MY (SEE     *
C LPLOT FOR MORE DETAILS).                                             *
C     WARNING: IN CPLOT8 THE FIRST DIMENSION OF Z IS ASSUMED TO EXTEND *
C OVER THE MAXIMUM RANGE DECLARED (CALLED NDIM IN THE LOWER LEVEL MAIN *
C ROUTINE CPLOT).  USE CPLOT WHEN THIS IS NOT THE CASE!                *
C                                                                      *
C     WRITTEN BY DEBBY HYMAN, 7-79                                     *
C     ADDED PARAMETER NC, HGO 17/12/85.                                *
C***********************************************************************
C
      use itm_types
      implicit none
      integer nc
      PARAMETER (NC=5)
C
      real (r8) X(*),Y(*),Z(NX,*),ZC(NC)
      integer MX,MY,NX,NY
      CHARACTER*(*) TITLE
C
      CALL CPLOT(MX,MY,1,X,Y,NX,NY,1,1,Z,NX,ZC,-NC,
     A           TITLE,LEN(TITLE),'X',1,'Y',1)
      RETURN
      END
C
      SUBROUTINE VPLOT9(MX,MY,X,Y,NX,NY,VX,VY,TITLE)
C
C***********************************************************************
C     A VECTOR PLOT GIVES A REPRESENTATION OF A TWO-DIMENSIONAL VECTOR *
C FIELD VX = F(X,Y), VY = G(X,Y) BY DRAWING VECTORS STARTING FROM DOTS *
C AT EACH DATA LOCATION.  THERE ARE TWO VECTOR PLOTTING SUBROUTINES IN *
C PPPLIB.  THE HIGH-LEVEL (EASIER TO USE) ROUTINE VPLOT9 PROCESSES     *
C NX*NY ELEMENTS OF THE TWO-DIMENSIONAL ARRAYS VX(I,J) = F(X(I),Y(J))  *
C AND VY(I,J) = G(X(I),Y(J)), CONTAINING THE HORIZONTAL AND VERTICAL   *
C FIELD COMPONENTS TO BE PLOTTED.  NX IS THE FIRST DIMENSION OF BOTH   *
C VX AND VY IN THE DIMENSION STATEMENT OF THE CALLING PROGRAM.  TITLE  *
C CONTAINS A TITLE OF AN ARBITRARY NUMBER OF CHARACTERS TO BE DRAWN AT *
C THE TOP OF THE GRAPH.  THE POSITION OF THE PLOT ON THE PAGE IS       *
C DETERMINED BY MX,MY (SEE LPLOT FOR MORE DETAILS).                    *
C     WARNING: IN VPLOT9 THE FIRST DIMENSION OF VX AND VY IS ASSUMED   *
C TO EXTEND OVER THE MAXIMUM RANGE DECLARED (CALLED NDIM IN THE LOWER  *
C LEVEL MAIN ROUTINE VPLOT).  USE VPLOT WHEN THIS IS NOT THE CASE!     *
C                                                                      *
C     WRITTEN BY DEBBY HYMAN, 7-79                                     *
C     ADDED ARGUMENTS X AND Y, HGO 17/12/85.                           *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) X(*),Y(*),VX(NX,*),VY(NX,*)
      integer MX,MY,NX,NY
      CHARACTER*(*) TITLE
C
      CALL VPLOT(MX,MY,111,X,Y,NX,NY,1,1,VX,VY,NX,.9_R8,20,
     A           TITLE,LEN(TITLE),'X',1,'Y',1)
      RETURN
      END
C
      SUBROUTINE SPLOT9(MX,MY,IS,YX,ZXY,NX,NY,Z,TITLE)
C
C***********************************************************************
C     THE SECTION PLOT ROUTINES PRODUCE ONE-DIMENSIONAL CROSS-SECTION  *
C PLOTS OF A TWO-DIMENSIONAL FUNCTION AT CONSTANT VALUE OF ONE OF THE  *
C TWO COORDINATES X AND Y.                                             *
C     SPLOT9 IS THE HIGH-LEVEL SECTION PLOT SUBROUTINE.  FOR IS=1 IT   *
C PRODUCES NS X-SECTION PLOTS AT EQUIDISTANT VALUES OF X.  IN THE SAME *
C WAY, FOR IS=2  SPLOT9 PRODUCES NS Y-SECTION PLOTS.  THE VALUE OF NS  *
C IS FIXED IN THE PARAMETER STATEMENT BELOW.  NX IS THE ACTUAL FIRST   *
C DIMENSION OF Z, AND NY IS THE ACTUAL SECOND DIMENSION.  TITLE CON-   *
C TAINS A TITLE OF AN ARBITRARY NUMBER OF CHARACTERS TO BE DRAWN AT    *
C THE TOP OF THE GRAPH.  THE POSITION OF THE PLOT ON THE PAGE IS DE-   *
C TERMINED BY MX,MY (SEE LPLOT FOR MORE DETAILS).                      *
C     WARNING: IN SPLOT9 THE FIRST DIMENSION OF Z IS ASSUMED TO EXTEND *
C OVER THE MAXIMUM RANGE DECLARED (CALLED NDIM IN THE LOWER LEVEL MAIN *
C ROUTINE SPLOT).  USE SPLOT WHEN THIS IS NOT THE CASE!                *
C                                                                      *
C     WRITTEN BY DEBBY HYMAN, 7-79                                     *
C     MERGED X- AND Y-SECTION SUBROUTINES BY MEANS OF THE ADDITIONAL   *
C     ARGUMENT IS, HGO 23/12/85.                                       *
C***********************************************************************
C
      use itm_types
      implicit none
      integer ns
      PARAMETER (NS=3)
C
      real (r8) YX(*),ZXY(*),Z(NX,*)
      integer MX,MY,IS,NX,NY
      integer IJARR(NS)
      CHARACTER*(*) TITLE
C
      IF(IS.EQ.1) CALL SPLOT(MX,MY,1,30971,YX,ZXY,NX,NY,1,Z,NX,IJARR,
     A                       -NS,TITLE,LEN(TITLE),'Y',1,' ',1)
      IF(IS.EQ.2) CALL SPLOT(MX,MY,2,30971,YX,ZXY,NX,NY,1,Z,NX,IJARR,
     A                       -NS,TITLE,LEN(TITLE),'X',1,' ',1)
      RETURN
      END
C
      SUBROUTINE APLOT9(MX,MY,IA,YX,AVXY,NX,NY,Z,TITLE)
C
C***********************************************************************
C     THE AVERAGE PLOT ROUTINES AVERAGE A TWO-DIMENSIONAL ARRAY IN ONE *
C DIRECTION AND PLOT THE RESULT WITH RESPECT TO THE OTHER DIRECTION.   *
C     APLOT9 IS THE HIGH-LEVEL AVERAGE PLOT SUBROUTINE.  FOR IA=1 IT   *
C AVERAGES THE MATRIX Z IN THE X-DIRECTION.  IN THE SAME WAY, FOR IA=2 *
C APLOT9 PRODUCES A Y-AVERAGE PLOT.  NX IS THE ACTUAL FIRST DIMENSION  *
C OF Z, AND NY IS THE ACTUAL SECOND DIMENSION.  TITLE CONTAINS A TITLE *
C OF AN ARBITRARY NUMBER OF CHARACTERS TO BE DRAWN AT THE TOP OF THE   *
C GRAPH.  THE POSITION OF THE PLOT ON THE PAGE IS DETERMINED BY MX,MY  *
C (SEE LPLOT FOR MORE DETAILS).                                        *
C     WARNING: IN APLOT9 THE FIRST DIMENSION OF Z IS ASSUMED TO EXTEND *
C OVER THE MAXIMUM RANGE DECLARED (CALLED NDIM IN THE LOWER LEVEL MAIN *
C ROUTINE APLOT).  USE APLOT WHEN THIS IS NOT THE CASE!                *
C                                                                      *
C     WRITTEN BY DEBBY HYMAN, 7-79                                     *
C     MERGED X- AND Y-SECTION SUBROUTINES BY MEANS OF THE ADDITIONAL   *
C     ARGUMENT IA, HGO 23/12/85.                                       *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) YX(*),AVXY(*),Z(NX,*)
      integer MX,MY,IA,NX,NY
      CHARACTER*(*) TITLE
C
      IF(IA.EQ.1) CALL APLOT(MX,MY,1,YX,AVXY,NX,NY,1,Z,NX,1,NX,
     A                       TITLE,LEN(TITLE),'Y',1,' ',1)
      IF(IA.EQ.2) CALL APLOT(MX,MY,2,YX,AVXY,NX,NY,1,Z,NX,1,NY,
     A                       TITLE,LEN(TITLE),'X',1,' ',1)
      RETURN
      END
C
      SUBROUTINE LPLOT(MX,MY,IOP,X,Y,NPTS,INC,
     A                 TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)
C
C***********************************************************************
C     THIS SUBROUTINE DRAWS A LINE PLOT OF THE NPTS VALUES IN X AND Y. *
C LPLOT DETERMINES THE RANGES OF X AND Y ,SUBSEQUENTLY CALLS NFRAME TO *
C DRAW A BOX AROUND THE PLOT, TO SCALE THE X- AND Y-AXES, AND TO PLACE *
C A TITLE AND LABELS ALONG THE AXES, AND FINALLY PUTS THE CURVE ON THE *
C PLOT.  THIS SEQUENCE MAY BE SPLIT BY THE USE OF NEGATIVE VALUES OF   *
C THE ARGUMENTS IOP (TO SUPPRESS PLOTTING OF THE CURVE) AND NPTS (TO   *
C SUPPRESS PLOTTING OF THE FRAME AND SCALES).                          *
C                                                                      *
C     THE PLOT'S POSITION ON THE PAGE IS DETERMINED BY IMX,IMY.  IMY=1 *
C SPECIFIES THAT THE Y-COORDINATE RANGE SPANS A FULL PAGE; IMY=2 AND 3 *
C SPECIFY THE UPPER AND LOWER HALVES OF THE PAGE; AND IMY=4, 5, AND 6  *
C SPECIFY THE UPPER, MIDDLE, AND LOWER THIRDS OF THE PAGE.  IMX=1 SPE- *
C CIFIES THAT THE X-COORDINATE RANGE SPANS A FULL PAGE; IMX=2 AND 3    *
C SPECIFY THE LEFT AND RIGHT HALVES OF THE PAGE; WHILE IMX=4, 5, AND 6 *
C ARE NOT ALLOWED.                                                     *
C     FOR EXAMPLE, (IMX,IMY)=(1,1) SPECIFIES A PLOT FILLING THE FULL   *
C PAGE, AND (3,3) SPECIFIES A PLOT IN THE LOWER RIGHT-HAND QUADRANT.   *
C A MAXIMUM OF SIX PLOTS ON A PAGE IS POSSIBLE IF THE PAIRS (2,4),     *
C (3,4), (2,5), (3,5), (2,6), AND (3,6) ARE USED.  PAGE ADVANCE IS     *
C AUTOMATIC WITH THE FIRST PLOT THAT EXTENDS INTO THE UPPER LEFT-HAND  *
C CORNER OF THE PAGE.  SUCH A PLOT MUST BE THE FIRST IN ANY PLOT       *
C SEQUENCE INTENDED TO APPEAR ON ONE PAGE.                             *
C                                                                      *
C     THE PLOT'S RANGE USUALLY IS EXPANDED TO A "ROUND" DECIMAL NUMBER *
C BY THE AUTOMATIC SCALING ROUTINES FROM THE MINIMUM RANGE IMPLIED BY  *
C THE DATA.  EXPANSION MAY BE PREVENTED BY APPENDING A '1' IN FRONT OF *
C THE IMX AND IMY VALUES IN ANY PLOT CALL (I.E., ISX=1).  FOR EXAMPLE, *
C IF XMIN=0.17, XMAX=359.78, AND ISX=0 FOR AUTOMATIC SCALING, THE X-   *
C SCALE GOES FROM 0.0 TO 400.0.  NOW, IF XMIN AND XMAX STAY THE SAME   *
C AND ISX=1 FOR EXACT SCALING, THE X-SCALE GOES FROM 0.0 TO 360.0.     *
C                                                                      *
C     ARGUMENTS:                                                       *
C                                                                      *
C MX     - DEFINES THE GRAPH AREA AND THE SCALING IN THE X-DIRECTION   *
C          ACCORDING TO THE FORMULA                                    *
C             IABS(MX) = IIX*1000 + IAX*100 + ISX*10 + IMX ,           *
C          WHERE IMX DETERMINES THE HORIZONTAL EXTENSION OF THE PLOT:  *
C             IMX = 1 - FULL PAGE                                      *
C                   2 - LEFT HALF OF THE PAGE                          *
C                   3 - RIGHT HALF OF THE PAGE,                        *
C          AND ISX DETERMINES THE SCALING ALONG THE X-AXIS:            *
C             ISX = 0 - AUTOMATIC SCALING WITH EXPANSION (DEFAULT)     *
C                   1 - EXACT SCALING (NO ROUNDING)                    *
C                   2 - EQUIDISTANT SCALING WITH THE X-SCALE ADAPTED   *
C                       TO THE LENGTHS ALONG Y (SEE NOTE IN NFRAME),   *
C          AND IAX PROVIDES AN ADDITIONAL OPTION:                      *
C             IAX = 0 - NO ACTION (DEFAULT)                            *
C                   1 - X=0 AXIS IS DRAWN  (IF IT LIES IN THE RANGE)   *
C                   2 - X=0 AXIS IS DASHED (IF IT LIES IN THE RANGE),  *
C          AND IIX OVERRULES THE DEFAULT NUMBER OF SCALE INTERVALS:    *
C             IIX = 0 - 4 INTERVALS FOR SCALES AND TICKMARKS (DEFAULT) *
C             IIX > 0 - IIX INTERVALS (NOT FOR AUTOMATIC SCALING).     *
C          MX < 0 : PLOTTING OF SCALES AND TICK MARKS SUPPRESSED.      *
C MY     - DEFINES THE GRAPH AREA AND THE SCALING IN THE Y-DIRECTION,  *
C          ANALOGOUS TO THE ABOVE EXPRESSIONS WITH X REPLACED BY Y,    *
C          WHERE IMY DETERMINES THE VERTICAL EXTENSION OF THE PLOT:    *
C             IMY = 1 - FULL PAGE                                      *
C                   2 - TOP HALF OF THE PAGE                           *
C                   3 - BOTTOM HALF OF THE PAGE                        *
C                   4 - TOP THIRD OF THE PAGE                          *
C                   5 - MIDDLE THIRD OF THE PAGE                       *
C                   6 - BOTTOM THIRD OF THE PAGE.                      *
C IOP    - PROVIDES DIFFERENT OPTIONS FOR THE X-Y SCALES, THE SYMBOLS  *
C          PLOTTED, AND THE CURVE DRAWN, ACCORDING TO THE FORMULA      *
C             IABS(IOP) = N*10000 + IC*10 + JOP,                       *
C          WHERE JOP DETERMINES THE SCALES ALONG THE X- AND Y-AXES:    *
C             JOP = 1 - LINEAR X-AXIS, LINEAR Y-AXIS                   *
C                   2 - LINEAR X-AXIS, LOG Y-AXIS                      *
C                   3 - LOG X-AXIS, LINEAR Y-AXIS                      *
C                   4 - LOG X-AXIS, LOG Y-AXIS                         *
C                   5 - LINEAR X-AXIS, LINEAR Y-AXIS (BUT PLOTTING OF  *
C                       FRAME, SCALES, AND TICK MARKS SUPPRESSED),     *
C          AND IC INDICATES THE ASCII CHARACTER TO BE PLACED AT THE    *
C          POINTS:                                                     *
C             IC = 0 (DEFAULT) - NO CHARACTER PLACED                   *
C             32 (192) <= IC <= 126 (254)                              *
C                              - CHARACTER FROM TABLE OF DLCH,         *
C          AND N DETERMINES THE SPACING BETWEEN THE PLOTTED CHARACTERS *
C          AND WHETHER A CURVE IS TO BE DRAWN THROUGH THEM:            *
C             N = 0 (DEFAULT) - SYMBOL SPECIFIED BY IC PLACED AT EACH  *
C                               POINT; THE POINTS ARE NOT CONNECTED    *
C             N > 0           - A SYMBOL PLACED AT EVERY N'TH POINT;   *
C                               ALL POINTS ARE CONNECTED BY A CURVE.   *
C          IOP < 0: THE FRAME IS DRAWN AND THE AXES ARE SCALED, BUT    *
C          THE CURVE IS NOT DRAWN.  THIS AMOUNTS TO JUST A CALL OF     *
C          NFRAME WITH AUTOMATIC DETERMINATION OF THE EXTREME VALUES   *
C          OF X AND Y BY LPLOT.  (IF THESE VALUES ARE ALREADY KNOWN,   *
C          IT IS MORE EFFICIENT TO CALL NFRAME DIRECTLY).              *
C X      - THE TABLE OF ABSCISSA VALUES TO BE PLOTTED.                 *
C Y      - THE TABLE OF ORDINATE VALUES TO BE PLOTTED.                 *
C NPTS   - IABS(NPTS) IS THE NUMBER OF X/Y ELEMENTS.                   *
C          NPTS < 0: A CURVE IS DRAWN ONTO A FRAME PREVIOUSLY SET UP   *
C          BY A CALL TO NFRAME OR LPLOT WITH IOP < 0.                  *
C INC    - IABS(INC) IS THE SPACING BETWEEN THE X/Y ELEMENTS PLOTTED.  *
C          INC < 0: THE Y-ELEMENTS PLOTTED ARE PAIRED WITH ABSCISSA    *
C          VALUES DETERMINED BY THE TWO VALUES XMIN=X(1) AND DX=X(2),  *
C          WHICH THE USER SHOULD INSERT IN X.                          *
C TITLE  - TITLE FOR THE GRAPH.                                        *
C NTITLE - THE NUMBER OF CHARACTERS IN NTITLE.                         *
C XNAME  - LABEL FOR THE X-AXIS.                                       *
C NXNAME - NUMBER OF CHARACTERS IN XNAME.                              *
C YNAME  - LABEL FOR THE Y-AXIS.                                       *
C NYNAME - NUMBER OF CHARACTERS IN YNAME.                              *
C          THE ABOVE THREE CHARACTER STRINGS ARE AUTOMATICALLY TRUN-   *
C          CATED TO FIT ALONGSIDE THE CHOSEN FRAME.  THE FONT CAN BE   *
C          CHANGED ACCORDING TO THE RULES GIVEN IN DLCH.               *
C                                                                      *
C     ENTRY HPLOT DRAWS A HISTOGRAM OF THE VALUES IN X AND Y.  THE     *
C ARGUMENTS ARE THE SAME AS FOR LPLOT.                                 *
C                                                                      *
C     WRITTEN BY CLAIR NIELSON.                                        *
C     MODIFIED BY DENNIS HEWETT 2-78, FOR RANGE PRINTED ON TOP RIGHT.  *
C     MODIFIED BY BOB MALONE 3-78, FOR CHARACTERS ON THE CURVE.        *
C     MODIFIED BY DEBBY HYMAN 4-80, FOR INCREMENTATION TO WORK,        *
C     FOR RNG TO BE PRINTED ONLY WHEN VERY SMALL.                      *
C     MODIFIED BY HANS GOEDBLOED 14/11/85, FOR ADAPTATION TO NEW DLCH, *
C     SHIFT OF THE TITLE WHEN RNG IS PRINTED.                          *
C     MODIFIED BY GUIDO HUYSMANS 1/07/89, FOR CLIPPING LINES TO FIT    *
C     THE FRAME.                                                       *
C     MODIFIED BY RONALD VAN DER LINDEN 6/90, TO MAKE IT POSSIBLE      *
C     TO HAVE 100 INSTEAD OF 10 DATA POINTS BETWEEN PRINTED SYMBOLS.   *
C***********************************************************************
C
      use itm_types
      implicit none
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      real (r8) X(*),Y(*)
      integer MX,MY,IOP,NPTS,INC,NTITLE,NXNAME,NYNAME
      CHARACTER*(*) TITLE,XNAME,YNAME
      CHARACTER TITLE1*80,RANGE*14
      real (r8) xmn, xmx, ymn, ymx, rng, xfac, yfac, 
     &     hx, xjs, xj, alog19, zix1, yj, ziy1, zix, ziy, zidx1,
     &     zidy1, zidx, zidy
      integer iopa, jop, ntot, inca, n, ic, imx, imy, icharsize,
     &     idum, nb, ntitl1, j
      LOGICAL FHIST,FLOGX,FLOGY,FCONN,FCHAR
C
C     * FLAG FOR HISTOGRAM.
      FHIST=.FALSE.
C
   10 continue
!DPC Added to try and prevent the routine from taking forever when NaNs are present
!the following code seems to work for g95 but not for pgi (perhaps wrongly optimized away?)
      do j=1, iabs(npts)
         if(x(j) .ne. x(j)) then
            write(*,*) 'lplot: x range includes NaN'
            return
         endif
         if(y(j) .ne. y(j)) then
            write(*,*) 'lplot: y range includes NaN'
            return
         endif
      enddo
      IOPA=IABS(IOP)
      JOP=MOD(IOPA,10)
      FLOGX=.FALSE.
      FLOGY=.FALSE.
      IF(JOP.EQ.3.OR.JOP.EQ.4) FLOGX=.TRUE.
      IF(JOP.EQ.2.OR.JOP.EQ.4) FLOGY=.TRUE.
      NTOT=IABS(NPTS)
      INCA=IABS(INC)
C
C     * SCHEME FOR CHARACTERS ON THE CURVE BY BOB MALONE, 3/78
C     * SET DEFAULTS FOR OPERATION WITHOUT CHARACTERS ON CURVES.
      FCONN=.TRUE.
      FCHAR=.FALSE.
      N=1
C
C     * DETERMINE WHETHER CHARACTERS ARE DESIRED.
      IC=MOD(IOPA/10,1000)
      IF(IC.NE.0) THEN
         FCHAR=.TRUE.
         N=MOD(IOPA/10000,100)
         IF(N.EQ.0) THEN
            FCONN=.FALSE.
            N=1
         ENDIF
      ENDIF
C
C     * REDUCE CHARACTERSIZE FOR SMALLEST PLOTS
      IMX=MOD(IABS(MX),10)
      IMY=MOD(IABS(MY),10)
      ICHARSIZE=2
      IF ((IMX.GE.2).AND.(IMY.GE.2)) ICHARSIZE=1
C
C     * DRAW THE FRAME.
      IF(NPTS.GT.0) THEN
         IF(INC.LT.0) THEN
            XMN=X(1)
            XMX=X(1)+(NTOT-1)*X(2)/INCA
         ELSE
            CALL MAXV(X,NTOT,INCA,XMX,IDUM)
            CALL MINV(X,NTOT,INCA,XMN,IDUM)
         ENDIF
         CALL MAXV(Y,NTOT,INCA,YMX,IDUM)
         CALL MINV(Y,NTOT,INCA,YMN,IDUM)
         NB=0
         RNG=ABS(YMX-YMN)
         IF(RNG.LT.(.02*ABS(YMX))) THEN
            WRITE(RANGE,'(''RNG ='',1PE9.2)') RNG
            NB=10
         ENDIF
         TITLE1=TITLE
         NTITL1=ISIGN(MIN(IABS(NTITLE)+NB,80),NTITLE)
         CALL NFRAME(MX,MY,JOP,XMN,XMX,YMN,YMX,
     A               TITLE1,NTITL1,XNAME,NXNAME,YNAME,NYNAME)
         IF(NB.NE.0) CALL DLCH(IXR-120,IYT+8,RANGE,14,1)
         IF(IOP.LT.0) RETURN
      ELSE
         CALL OFRAME(MX,MY)
      ENDIF
C
C     * DRAW THE CURVE.
      XFAC=REAL(IXR-IXL,R8)/(XR-XL)
      YFAC=REAL(IYT-IYB,R8)/(YT-YB)
      HX=0._R8
      IF(INC.LT.0) HX=X(2)
      XJS=X(1)
      XJ=XJS
      IF(FLOGX) XJ=ALOG19(XJ)
      ZIX1=REAL(IXL,R8)+(XJ-XL)*XFAC
      YJ=Y(1)
      IF(FLOGY) YJ=ALOG19(YJ)
      ZIY1=REAL(IYB,R8)+(YJ-YB)*YFAC
      IF(FHIST) CALL DRV(ZIX1,REAL(IYB,R8),ZIX1,ZIY1)
      IF(FCHAR.AND.N.EQ.1) CALL DLCH(INT(ZIX1),-INT(ZIY1),' ',IC,1)
      DO 20 J=1+INCA,NTOT,INCA
         XJS=XJS+HX
         IF(INC.GT.0) XJS=X(J)
         XJ=XJS
         IF(FLOGX) XJ=ALOG19(XJ)
	 ZIX=REAL(IXL,R8)+(XJ-XL)*XFAC
         YJ=Y(J)
         IF(FLOGY) YJ=ALOG19(YJ)
         ZIY=REAL(IYB,R8)+(YJ-YB)*YFAC
         IF(FHIST) THEN
C           * HISTOGRAM DRAWN BY THESE CALLS TO DRV.
            CALL DRV(ZIX1,ZIY1,ZIX,ZIY1)
            CALL DRV(ZIX,ZIY1,ZIX,ZIY)
         ELSE
            IF(FCONN) THEN
               ZIDX1=ZIX1
               ZIDY1=ZIY1
               ZIDX=ZIX
               ZIDY=ZIY
               CALL CLIP(ZIDX1,ZIDY1,ZIDX,ZIDY)
            ENDIF
            IF(FCHAR.AND.MOD(J,N).EQ.0) THEN
               IF((ZIX.GT.REAL(IXL,R8).AND.ZIX.LT.REAL(IXR,R8)).AND.
     A            (ZIY.GT.REAL(IYB,R8).AND.ZIY.LT.REAL(IYT,R8)))
     B         CALL DLCH(INT(ZIX),-INT(ZIY),' ',IC,-ICHARSIZE)
            ENDIF
         ENDIF
         ZIX1=ZIX
         ZIY1=ZIY
   20 CONTINUE
      RETURN
C
C     * ENTRY FOR DRAWING A HISTOGRAM.
      ENTRY HPLOT(MX,MY,IOP,X,Y,NPTS,INC,
     A            TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)
      FHIST=.TRUE.
      GOTO 10
      END
C
      SUBROUTINE CLIP(ZIX1,ZIY1,ZIX2,ZIY2)
C
C***********************************************************************
C     SUBROUTINE TO CLIP LINE TO LINEPIECE WITHIN PLOT WINDOW.         *
C                                SOURCE : INTERACTIVE GRAPHICS, P. 88  *
C     ADDED BY GUIDO HUYSMANS 1/07/89.                                *
C***********************************************************************
C
      use itm_types
      implicit none
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      real (r8) ZIX1,ZIY1,ZIX2,ZIY2
      real (r8) zix, ziy
      INTEGER C,C1,C2
C
      CALL CODE(ZIX1,ZIY1,C1)
      CALL CODE(ZIX2,ZIY2,C2)
   10 IF((C1.GT.1).OR.(C2.GT.1)) THEN
         IF(MOD(44100/(C1*C2),210).NE.0) RETURN
         C = C1
         IF(C.LE.1) C = C2
         IF(MOD(C,5).EQ.0) THEN
            ZIY = ZIY1 + (ZIY2-ZIY1)*(REAL(IXL,R8)-ZIX1)/(ZIX2-ZIX1)
            ZIX = REAL(IXL,R8)
         ELSE
            IF(MOD(C,7).EQ.0) THEN
               ZIY = ZIY1 + (ZIY2-ZIY1)*(REAL(IXR,R8)-ZIX1)/(ZIX2-ZIX1)
               ZIX = REAL(IXR,R8)
            ELSE
               IF(MOD(C,3).EQ.0) THEN
                 ZIX = ZIX1 + 
     &                 (ZIX2-ZIX1)*(REAL(IYB,R8)-ZIY1)/(ZIY2-ZIY1)
                 ZIY = REAL(IYB,R8)
               ELSE
                 IF(MOD(C,2).EQ.0) THEN
                 ZIX = ZIX1 + 
     &                   (ZIX2-ZIX1)*(REAL(IYT,R8)-ZIY1)/(ZIY2-ZIY1)
                 ZIY = REAL(IYT,R8)
                 ENDIF
               ENDIF
            ENDIF
         ENDIF
         IF(C.EQ.C1) THEN
            ZIX1 = ZIX
            ZIY1 = ZIY
            CALL CODE(ZIX,ZIY,C1)
         ELSE
            ZIX2 = ZIX
            ZIY2 = ZIY
            CALL CODE(ZIX,ZIY,C2)
         ENDIF
         GOTO 10
      ENDIF
      CALL DRV(ZIX1,ZIY1,ZIX2,ZIY2)
      RETURN
      END
C
      SUBROUTINE CODE(ZIX,ZIY,C)
C
C***********************************************************************
C                                                                      *
C     ADDED BY GUIDO HUYSMANS 1/07/89.                                 *
C***********************************************************************
C
      use itm_types
      implicit none
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      real (r8) ZIX,ZIY
      INTEGER C
C
      C = 1
      IF(ZIX.LT.REAL(IXL,R8)) THEN
         C = 5*C
      ELSE
         IF(ZIX.GT.REAL(IXR,R8)) C = 7*C
      ENDIF
      IF(ZIY.LT.REAL(IYB,R8)) THEN
         C = 3*C
      ELSE
         IF(ZIY.GT.REAL(IYT,R8)) C = 2*C
      ENDIF
      RETURN
      END
C
      SUBROUTINE PPLOT(MX,MY,X,Y,NPTS,INC)
C
C***********************************************************************
C     SUBROUTINE PPLOT PLOTS THE VALUES IN X AND Y.  EACH POINT IS RE- *
C PRESENTED BY A PLOTTING DOT, AND ADJACENT POINTS ARE NOT CONNECTED.  *
C ENTRY PPLOTC PROVIDES A CONDITIONAL POINT PLOT OF THOSE POINTS FOR   *
C WHICH THE Z VALUE SATISFIES ZMIN < Z < ZMAX.  THE ROUTINES HAVE BEEN *
C OPTIMIZED TO PLOT MANY PARTICLES AS DOTS.                            *
C     BOTH SUBROUTINES ASSUME THAT THE FRAME, SCALE, AND LABELS FOR    *
C THIS (IMX,IMY) PLOT HAVE BEEN GENERATED BY A PREVIOUS CALL OF LPLOT  *
C WITH IOP = -1 OR A DIRECT CALL OF NFRAME.  ONLY LINEAR-LINEAR SCA-   *
C LING IS ALLOWED.  IF PPLOT IS CALLED WITHOUT A PRECEDING LPLOT CALL, *
C IT WILL USE THE SCALING LEFT IN COMMON BLOCK CJE07 FOR THAT FRAME.   *
C                                                                      *
C     ARGUMENTS:                                                       *
C                                                                      *
C MX/MY - SEE LPLOT.                                                   *
C X     - THE TABLE OF ABSCISSA VALUES.                                *
C Y     - THE TABLE OF ORDINATE VALUES.                                *
C NPTS  - THE NUMBER OF ELEMENTS IN THE ARRAYS X, Y, AND Z.            *
C INC   - IABS(INC) IS THE SPACING BETWEEN THE X/Y ELEMENTS PLOTTED.   *
C                                                                      *
C     ADDITIONAL ARGUMENTS FOR PPLOTC:                                 *
C                                                                      *
C Z     - A FUNCTION OF X AND Y.                                       *
C ZMIN  - THE SMALLEST VALUE TO BE PLOTTED.                            *
C ZMAX  - THE LARGEST VALUE TO BE PLOTTED.                             *
C                                                                      *
C     WRITTEN BY CLAIR NIELSON.                                        *
C***********************************************************************
C
      use itm_types
      implicit none
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      real (r8) X(*),Y(*),Z(*)
      integer MX,MY,NPTS,INC
      real (r8) ZMIN,ZMAX
      real (r8) xfac, yfac, zix, ziy
      integer j
C
      CALL OFRAME(MX,MY)
      XFAC=(IXR-IXL)/(XR-XL)
      YFAC=(IYT-IYB)/(YT-YB)
      DO 10 J=1,NPTS,IABS(INC)
         ZIX=MIN(MAX(REAL(IXL,R8),REAL(IXL,R8)+(X(J)-XL)*XFAC),
     &        REAL(IXR,R8))
         ZIY=MIN(MAX(REAL(IYB,R8),REAL(IYB,R8)+(Y(J)-YB)*YFAC),
     &        REAL(IYT,R8))
         CALL DRP(ZIX,ZIY)
   10 CONTINUE
      RETURN
C
      ENTRY PPLOTC(MX,MY,X,Y,NPTS,INC,Z,ZMIN,ZMAX)
C
      CALL OFRAME(MX,MY)
      XFAC=(IXR-IXL)/(XR-XL)
      YFAC=(IYT-IYB)/(YT-YB)
      DO 20 J=1,NPTS,IABS(INC)
         IF(Z(J).LT.ZMIN) GOTO 20
         IF(Z(J).GT.ZMAX) GOTO 20
         ZIX=MIN(MAX(REAL(IXL,R8),REAL(IXL,R8)+(X(J)-XL)*XFAC),
     &        REAL(IXR,R8))
         ZIY=MIN(MAX(REAL(IYB,R8),REAL(IYB,R8)+(Y(J)-YB)*YFAC),
     &        REAL(IYT,R8))
         CALL DRP(ZIX,ZIY)
   20 CONTINUE
      RETURN
      END
C
      SUBROUTINE DPLOT(MX,MY,X,Y,NPTS,INC,L1,L2)
C
C***********************************************************************
C     DPLOT DRAWS A DASHED OR DOTTED CURVE THROUGH THE POINTS          *
C                   X(I),Y(I), I=1,NPTS,INC,                           *
C WHERE L1 AND L2 ARE THE LENGTHS OF THE STROKES AND SPACES OF THE     *
C LINE.  E.G., IF L1=0, A DOTTED CURVE IS PRODUCED WITH DISTANCES L2   *
C BETWEEN THE DOTS.  IF L2=0 THE CURVE IS FULLY DRAWN (OF COURSE, ONE  *
C SHOULD NOT USE DPLOT BUT LPLOT IN THAT CASE).                        *
C     THIS SUBROUTINE ASSUMES A PREVIOUS CALL OF NFRAME OR LPLOT WITH  *
C IOP = -1 TO SET UP THE FRAME AND SCALING.                            *
C                                                                      *
C     ARGUMENTS:                                                       *
C                                                                      *
C MX/MY - SEE LPLOT.                                                   *
C X     - THE TABLE OF ABSCISSA VALUES.                                *
C Y     - THE TABLE OF ORDINATE VALUES.                                *
C NPTS  - THE NUMBER OF ELEMENTS IN THE ARRAYS X AND Y.                *
C INC   - IABS(INC) IS THE SPACING BETWEEN THE X/Y ELEMENTS USED.      *
C L1    - LENGTH OF THE STROKES IN PLOTTING COORDINATES.               *
C L2    - LENGTH OF THE SPACES IN PLOTTING COORDINATES.                *
C                                                                      *
C     WRITTEN HGO 28/01/86                                             *
C***********************************************************************
C
      use itm_types
      implicit none
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      real (r8) X(*),Y(*)
      integer MX,MY,NPTS,INC,L1,L2
      real (r8) xfac, yfac, zix1, ziy1, zix, ziy
      integer l, inca, j, ll
C
      CALL OFRAME(MX,MY)
      XFAC=REAL(IXR-IXL,R8)/(XR-XL)
      YFAC=REAL(IYT-IYB,R8)/(YT-YB)
      ZIX1=MIN(MAX(REAL(IXL,R8),REAL(IXL,R8)+(X(1)-XL)*XFAC),
     &     REAL(IXR,R8))
      ZIY1=MIN(MAX(REAL(IYB,R8),REAL(IYB,R8)+(Y(1)-YB)*YFAC),
     &     REAL(IYT,R8))
      L=0
      INCA=IABS(INC)
      DO 10 J=1+INCA,NPTS,INCA
       ZIX=MIN(MAX(REAL(IXL,R8),REAL(IXL,R8)+(X(J)-XL)*XFAC),
     &     REAL(IXR,R8))
       ZIY=MIN(MAX(REAL(IYB,R8),REAL(IYB,R8)+(Y(J)-YB)*YFAC),
     &     REAL(IYT,R8))
       CALL DASH(ZIX1,ZIY1,ZIX,ZIY,L1,L2,L,LL)
       L=LL
       ZIX1=ZIX
       ZIY1=ZIY
   10 CONTINUE
      RETURN
      END
C
      SUBROUTINE DASH(ZIX1,ZIY1,ZIX2,ZIY2,L1,L2,L,LL)
C
C***********************************************************************
C     THIS ROUTINE DRAWS A DASHED LINE FROM (IX1,IY1) TO (IX2,IY2).    *
C THE ARGUMENTS L1 AND L2 ARE THE LENGTHS IN PLOTTING COORDINATES OF   *
C THE STROKES AND SPACES OF THE LINE, RESP.  L IS THE INITIAL POSITION *
C (INPUT) AND LL IS THE FINAL POSITION (OUTPUT) OF THE POINTER ON THE  *
C PLOTTING STRIP (0,L1+L2).                                            *
C                                                                      *
C     WRITTEN HGO 28/01/86                                             *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) ZIX1,ZIY1,ZIX2,ZIY2
      integer L1,L2,L,LL
      real (r8) r, xfac, yfac, dx, dy, zix, ziy
      integer ir, ltot, l11, l22
      LL=L
      R=SQRT(REAL((ZIX2-ZIX1)**2+(ZIY2-ZIY1)**2,R8))
      IF(R.NE.0._R8) THEN
         XFAC=(ZIX2-ZIX1)/R
         YFAC=(ZIY2-ZIY1)/R
      ELSE
         XFAC=1._R8
         YFAC=1._R8
      ENDIF
      IR=INT(R)
      LTOT=0
      DX=0._R8
      DY=0._R8
      ZIX=ZIX1
      ZIY=ZIY1
      CALL MOVABS(ZIX,ZIY)
   10 IF(LL.EQ.0.AND.L1.EQ.0) CALL DRP(ZIX,ZIY)
      IF(LL.LT.L1) THEN
         L11=MIN(L1-LL,IR-LTOT)
         DX=DX+L11*XFAC
         DY=DY+L11*YFAC
         ZIX=ZIX1+DX
         ZIY=ZIY1+DY
ccc         IF(L1.NE.0) CALL DRWABS(ZIX,ZIY)
         IF(L1.NE.0) CALL CLIP(ZIX1,ZIY1,ZIX,ZIY)
         IF(L1.EQ.0) CALL DRP(ZIX,ZIY)
         LTOT=LTOT+L11
         LL=LL+L11
      ELSE
         L22=MIN(L1+L2-LL,IR-LTOT)
         DX=DX+L22*XFAC
         DY=DY+L22*YFAC
         ZIX=ZIX1+DX
         ZIY=ZIY1+DY
         CALL MOVABS(ZIX,ZIY)
         LTOT=LTOT+L22
         LL=LL+L22
         IF(LL.GE.L1+L2) LL=0
      ENDIF
      IF(LTOT.LT.IR) GOTO 10
      RETURN
      END
C
      SUBROUTINE CPLOT(MX,MY,ILAB,X,Y,NX,NY,INCX,INCY,Z,NDIM,ZC,NC,
     A                 TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)
C
C***********************************************************************
C     SUBROUTINE CPLOT DRAWS NC CONTOURS OF THE FUNCTION Z = F(X,Y).   *
C THIS FUNCTION SHOULD BE STORED AS A TWO-DIMENSIONAL ARRAY Z(I,J),    *
C COMPUTED AT THE POINTS   X(I), I=1,IABS(NX),IABS(INCX),              *
C                          Y(J), J=1,IABS(NY),IABS(INCY).              *
C ENTRY CPLOTX IS AN EXTENSION FOR DRAWING POLAR PLOTS AND LOG10 CON-  *
C TOURS.                                                               *
C                                                                      *
C     ARGUMENTS:                                                       *
C                                                                      *
C MX/MY - SEE LPLOT.                                                   *
C ILAB  - CONTROLS THE ABSENCE/PRESENCE (ILAB=0/1) OF ALPHABETIC       *
C         LABELS ON THE CONTOURS.  THE CHOICE OF THE LABELS IS FIXED   *
C         IN THE PARAMETER STATEMENT BELOW TO BE UPPER CASE (N1=65),   *
C         LOWER CASE (N1=97), OR GREEK (N1=225).                       *
C X     - TABLE OF ABSCISSA VALUES.                                    *
C Y     - TABLE OF ORDINATE VALUES.                                    *
C NX    - IABS(NX) IS THE NUMBER OF POINTS IN X TO BE USED.            *
C         NX < 0 : ONLY THE FRAME AND SCALES FOR THE PLOT ARE DRAWN.   *
C NY    - IABS(NY) IS THE NUMBER OF POINTS IN Y TO BE USED.            *
C         NY < 0 : CONTOURS ARE DRAWN ON A FRAME PREVIOUSLY CREATED    *
C         BY A CALL TO CPLOT WITH NX < 0 OR A DIRECT CALL OF NFRAME    *
C         (IN ORDER TO SPECIFY A FRAME SIZE DIFFERENT FROM THE ONE     *
C         IMPLIED BY THE RANGES OF X AND Y).                           *
C INCX  - IABS(INCX) IS THE SKIP PARAMETER IN A ROW.                   *
C         INCX < 0 : XMIN = X(1) AND HX = X(2).                        *
C INCY  - IABS(INCY) IS THE SKIP PARAMETER IN A COLUMN.                *
C         INCY < 0 : YMIN = Y(1) AND HY = Y(2).                        *
C Z     - THE TWO-DIMENSIONAL FUNCTION TO BE CONTOURED; Z SHOULD BE    *
C         STORED SO THAT Z(I,J) IS THE VALUE OF Z AT X(I),Y(J).        *
C NDIM  - LENGTH OF A ROW OF Z (1ST DIMENSION OF THE 2-D ARRAY).       *
C         HENCE, ONE SHOULD OBSERVE: NX <= NDIM.                       *
C ZC    - THE TABLE OF CONTOUR VALUES, WHICH SHOULD BE DIMENSIONED AT  *
C         LEAST AS ZC(NC) IN THE CALLING PROGRAM.                      *
C NC    - NUMBER OF CONTOURS TO BE PLOTTED; MAXIMUM OF 26.             *
C         NC < 0 : CPLOT AUTOMATICALLY FILLS ZC WITH NC VALUES.        *
C         NC > 0 : ZC IS SUPPLIED BY THE USER; VALUES MUST BE STORED   *
C         IN INCREASING ORDER IN ZC.                                   *
C TITLE                - TITLE FOR THE GRAPH.                          *
C XNAME/YNAME          - LABEL FOR THE X/Y-AXIS.                       *
C NTITLE/NXNAME/NYNAME - NUMBER OF CHARACTERS IN TITLE/XNAME/YNAME.    *
C                                                                      *
C     ADDITIONAL ARGUMENTS FOT CPLOTX:                                 *
C                                                                      *
C RMAX  - MAXIMUM RADIUS FOR A POLOR PLOT.                             *
C         = 0 : CARTESIAN PLOT.                                        *
C         > 0 : X/Y CORRESPONDS TO R/THETA (IN RADIANS).               *
C         < 0 : X/Y CORRESPONDS TO R/COS(THETA).                       *
C IQUAD - TOTAL NUMBER OF QUADRANTS (FOR RMAX.NE.0 ONLY).              *
C LGZ   - IABS(ILGZ) CONTROLS THE NUMBER OF LOG10 CONTOURS.            *
C         LGZ = 0 : SCALAR CONTOURS.                                   *
C         ILGZ = 1,2,3,4 : LOG10 CONTOURS AT II = 1,9,ILGZ INTVLS/DEC. *
C         LGZ > 0 : THIS NUMBER II IS AUTOMATICALLY OVERRIDDEN (DOWN   *
C         TO 1 INT/DEC, DEPENDING ON THE NUMBER OF DECADES COMPUTED    *
C         FROM THE RANGE OF Z) TO GET A REASONABLE NUMBER OF CONTOURS. *
C         LGZ < 0 : THE AUTOMATIC OVERRIDE IS SWITCHED OFF, BUT THE    *
C         NUMBER OF DECADES IS LIMITED TO 8.                           *
C                                                                      *
C     WRITTEN BY CLAIR NIELSON.                                        *
C     MODIFIED BY DENNIS HEWETT 2/8/78, FOR CONTOLLING CONTOUR LABELS, *
C     ADDED VARIABLE NLAB BOTH HERE AND IN TRICJ3.                     *
C     EXTENDED BY DENNIS HEWETT 12/8/82, WITH CPLOTX AND TRICJ3 FOR    *
C     LOG10 CONTOURS AND POLAR PLOTS.                                  *
C     ADDED ARGUMENT ILAB (=NLAB), HGO 6/12/85.                        *
C***********************************************************************
C
      use itm_types
      implicit none
      integer n1
      PARAMETER (N1=97)
C
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      COMMON /CPLCOM/XFAC,YFAC,FX0,FY0,ISYM(26),NLAB,N1C
      real (r8) XFAC,YFAC,FX0,FY0
      integer ISYM,NLAB,N1C
      real (r8) X(*),Y(*),Z(NDIM,*),ZC(*)
      CHARACTER*(*) TITLE,XNAME,YNAME
      real (r8) ZT(4)
      CHARACTER*19 AMIN,AMAX
      CHARACTER*80 TITLE1
      real (r8) RMAX,rmx,xmn,ymn,xmx,ymx,hx,hy,zmin,zmax,delz,alog19,
     &     step,zct,y1,y2,x1,x2,xbar,ybart,ybarb
      integer MX,MY,ILAB,NX,NY,INCX,INCY,NDIM,NC,NTITLE,NXNAME,NYNAME,
     &     IQUAD,LGZ,I,ILGZ,ICORD,nnx,nny,inx,iny,nb,ntitl1,noc,icps,
     &     idum,jdum,lgmx,lgmn,ldec,ic,ii,id,iqud,j
      LOGICAL FLGZ
C
C     * INITIALIZE N1 AND ISYM FOR USE IN TRICJ3.
      N1C=N1
      DO 5 I=1,26
    5    ISYM(I)=0
C
C     * INITIALIZE FOR SCALAR CONTOURS.
      ILGZ=0
C     * FLAG DOWN FOR OVERRIDING THE AUTOMATIC DETERMINATION OF THE
C     * NUMBER OF CONTOURS PER DECADE IN THE CASE OF LOG10 CONTOURS.
      FLGZ=.FALSE.
C
C     * INPUT PARAMETERS.
   10 ICORD=0
      RMX=0._R8
      NNX=IABS(NX)
      NNY=IABS(NY)
      IF(NNY.LE.1) RETURN
      NLAB=IABS(ILAB)
      INX=IABS(INCX)
      INY=IABS(INCY)
      XMN=X(1)
      YMN=Y(1)
      IF(INCX.LE.0) THEN
         HX=X(2)
         XMX=X(1)+(NNX-1)*X(2)/INX
      ELSE
         XMX=X(NNX)
      ENDIF
      IF(INCY.LE.0) THEN
         HY=Y(2)
         YMX=Y(1)+(NNY-1)*Y(2)/INY
      ELSE
         YMX=Y(NNY)
      ENDIF
C
C     * DRAW THE FRAME.
   20 IF(NY.GE.0) THEN
         NB=0
         IF(IABS(ILAB).EQ.1) THEN
            NB=8
            IF(ILGZ.NE.0) NB=14
         ENDIF
         TITLE1=TITLE
         NTITL1=ISIGN(MIN(IABS(NTITLE)+NB,80),NTITLE)
         CALL NFRAME(MX,MY,1,XMN,XMX,YMN,YMX,
     A               TITLE1,NTITL1,XNAME,NXNAME,YNAME,NYNAME)
         IF(NX.LT.0) RETURN
      ELSE
         CALL OFRAME(MX,MY)
      ENDIF

C
C     * PARAMETERS FOR COMMON /CPLCOM/ SHARED WITH SUBROUTINE TRICJ3.
C     * INT(FX0) AND INT(FY0) ARE THE INTEGER LOCATIONS OF X=0 AND Y=0.
      XFAC=REAL(IXR-IXL,R8)/(XR-XL)
      YFAC=REAL(IYT-IYB,R8)/(YT-YB)
      FX0=REAL(IXL,R8)-XL*XFAC
      FY0=REAL(IYB,R8)-YB*YFAC
C
C     * DETERMINE CONTOUR VALUES AND NUMBER OF CONTOURS.
C      NOC=MIN(26,IABS(NC))
      NOC=ABS(NC)
      ICPS = NOC
      IF (ILAB.LT.0) THEN
        ICPS = -NOC
      ENDIF
      IF(NC.LE.0) THEN
         CALL MINM(Z,NDIM,NNX,NNY,INX,INY,ZMIN,IDUM,JDUM)
         CALL MAXM(Z,NDIM,NNX,NNY,INX,INY,ZMAX,IDUM,JDUM)
         IF(ILGZ.EQ.0) THEN
            DELZ=(ZMAX-ZMIN)/NOC
            DO 30 IC=1,NOC
   30          ZC(IC)=ZMIN+(REAL(IC,R8)-.5)*DELZ
         ELSE
            LGMX=ALOG19(ZMAX)
            IF(ZMAX.LT.1._R8) LGMX=LGMX-1
            LGMN=ALOG19(ZMIN)
            IF(ZMIN.LT.1._R8) LGMN=LGMN-1
            LGMN=MAX(LGMN,LGMX-25)
            LDEC=LGMX-LGMN+1
            IF(FLGZ) THEN
               IF(ILGZ.EQ.1) LDEC=MIN(LDEC,2)
               IF(ILGZ.EQ.2) LDEC=MIN(LDEC,5)
               LDEC=MIN(LDEC,8)
               LGMN=LGMX-LDEC+1
            ELSE
               IF(LDEC.GT.2.AND.ILGZ.LT.2) ILGZ=2
               IF(LDEC.GT.5.AND.ILGZ.LT.3) ILGZ=4
               IF(LDEC.GT.8) ILGZ=10
            ENDIF
            IC=0
            STEP=10._R8**LGMN
            DO 50 ID=1,LDEC
               DO 40 II=1,9,ILGZ
                  ZCT=REAL(II,R8)*STEP
                  IF((IC.EQ.NOC).OR.(ZCT.GT.ZMAX)) GOTO 60
                  IC=IC+1
   40             ZC(IC)=ZCT
   50          STEP=STEP*10._R8
   60       NOC=IC
         ENDIF
      ENDIF
C
C     * PUT EXTREME PARAMETERS ALONG THE TOP OF THE GRAPH.
      IF(ABS(ILAB).EQ.1) THEN
         IF(ILGZ.EQ.0) THEN
            WRITE(AMIN,'(''='',1PE9.2)') ZC(1)
            WRITE(AMAX,'(''='',1PE9.2)') ZC(NOC)
            CALL DLCH(IXR-90,IYT+18,' ',N1,1)
            CALL DLCH(IXR-75,IYT+18,AMIN,10,1)
            CALL DLCH(IXR-90,IYT+4,' ',N1+NOC-1,1)
            CALL DLCH(IXR-75,IYT+4,AMAX,10,1)
         ELSE
            WRITE(AMIN,'(''='',1PE9.2,'' ILGZ ='',I2)') ZC(1),ILGZ
            WRITE(AMAX,'(''='',1PE9.2,'' LDEC ='',I2)') ZC(NOC),LDEC
            CALL DLCH(IXR-170,IYT+18,' ',N1,1)
            CALL DLCH(IXR-155,IYT+18,AMIN,19,1)
            CALL DLCH(IXR-170,IYT+4,' ',N1+NOC-1,1)
            CALL DLCH(IXR-155,IYT+4,AMAX,19,1)
         ENDIF
      ENDIF
C
C     * DRAW THE CONTOURS BY CALLING TRICJ3 FOR THE TWO TRIANGLES
C     * WITHIN A MESH OF THE GRID.
      Y1=Y(1)
      DO 80 J=1+INY,NNY,INY
         IF(INCY.GT.0) HY=Y(J)-Y(J-INY)
         Y2=Y1+HY
         X1=X(1)
         DO 70 I=1+INX,NNX,INX
            IF(INCX.GT.0) HX=X(I)-X(I-INX)
            X2=X1+HX
            ZT(1)=Z(I-INX,J-INY)
            ZT(2)=Z(I,J-INY)
            ZT(3)=Z(I,J)
            ZT(4)=Z(I-INX,J)
            IF(ABS(ZT(3)-ZT(1)).GE.ABS(ZT(4)-ZT(2))) THEN
              CALL TRICJ3(X1,Y1,HX,HY,ICPS,ZC,ZT(2),ZT(1),ZT(4),ICORD)
              CALL TRICJ3(X2,Y2,-HX,-HY,ICPS,ZC,ZT(4),ZT(3),ZT(2),ICORD)
            ELSE
              CALL TRICJ3(X2,Y1,-HX,HY,ICPS,ZC,ZT(1),ZT(2),ZT(3),ICORD)
              CALL TRICJ3(X1,Y2,HX,-HY,ICPS,ZC,ZT(3),ZT(4),ZT(1),ICORD)
            ENDIF
            X1=X2
   70    CONTINUE
         Y1=Y2
   80 CONTINUE
c-------------------------- postscript extension add colorbar
      IF (ILAB.EQ.-1) THEN
	XBAR  = REAL(IXR,R8)
	YBART = REAL(IYT,R8)
	YBARB = REAL(IYB,R8)
        CALL COLORBAR(ZC,NOC,XBAR,YBART,YBARB)
      ENDIF

C
      RETURN
C
C     * ENTRY FOR POLAR PLOTS AND LOG10 CONTOURS.
      ENTRY CPLOTX(MX,MY,ILAB,X,Y,NX,NY,INCX,INCY,Z,NDIM,ZC,NC,
     A             TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME,
     B             RMAX,IQUAD,LGZ)
C
      ILGZ=MIN(IABS(LGZ),4)
      IF(ILGZ.LT.0) FLGZ=.TRUE.
      IF(RMAX.EQ.0._R8) GOTO 10
C
      ICORD=1
      IF(RMAX.LT.0._R8) ICORD=2
      RMX=ABS(RMAX)
      NNX=IABS(NX)
      NNY=IABS(NY)
      IF(NNY.LE.1) RETURN
      NLAB=IABS(ILAB)
      INX=IABS(INCX)
      INY=IABS(INCY)
      IF(INCX.LT.0) HX=X(2)
      IF(INCY.LT.0) HY=Y(2)
      XMN=0._R8
      YMN=0._R8
      XMX=RMX
      YMX=RMX
      IQUD=MAX(IQUAD,1)
      IF(IQUD.GT.2.AND.RMAX.LT.0.) IQUD=2
      IF(IQUD.EQ.2.) THEN
         XMN=-RMX
         XMX=RMX
         YMX=RMX
      ELSEIF(IQUD.EQ.3.OR.IQUD.EQ.4) THEN
         XMN=-RMX
         YMN=-RMX
      ENDIF
      GOTO 20
C
      END
C
      SUBROUTINE TRICJ3(XV,YV,DX,DY,NOC,ZC,ZX,ZV,ZY,ICORD)
C
C***********************************************************************
C     THIS SUBROUTINE IS CALLED FROM CPLOT TO DETERMINE THE PARTS OF   *
C THE CONTOURS THAT LIE WITHIN A TRIANGLE OF THE GRID MESH.  TRICJ3    *
C FINDS THE INTERSECTIONS OF THE CONTOURS WITH THE TWO SIDES OF THE    *
C TRIANGLE AND DRAWS LINES BETWEEN THOSE POINTS.                       *
C     IF NLABEL=1 IN COMMON /CPLCOM/, ALPHABETIC LABELS ARE WRITTEN    *
C EVERY #(ISKIP+1) CALL OF TRICJ3.  ISKIP IS FIXED IN THE PARAMETER    *
C STATEMENT BELOW.                                                     *
C                                                                      *
C     MODIFIED BY D.W. HEWETT 12-82, FOR THE DIFFERENT COORDINATES     *
C     X,Y (ICORD=0), R,THETA (ICORD=1), AND  R,COS(THETA) (ICORD=2).   *
C     ADDED PARAMETER ISKIP, ADDED CHECK ON RANGE Y1/2, HGO 9/12/85.   *
C***********************************************************************
C
      use itm_types
      implicit none
      integer iskip
      PARAMETER (ISKIP=9)
C
      real (r8) XV,YV,DX,DY,ZX,ZV,ZY
      integer NOC,ICORD
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      COMMON /CPLCOM/XFAC,YFAC,FX0,FY0,ISYM(26),NLAB,N1C
      real (r8) XFAC,YFAC,FX0,FY0
      integer ISYM,NLAB,N1C
      real (r8) ZC(*)
      real (r8) X(3),Y(3),Z(3),XP(3),YP(3)
      real (r8) pi,tpi,flp1,tx1,frac,x1,y1,x2,y2,flp2,tx2,
     &     zix1,ziy1,zix2,ziy2
      integer n1,ix,icps,noc2,iv,iy,i,ic,icc,idx,idy,ix11,iy11
C
      PI=3.1415926535898_R8
      TPI=2._R8*PI
      N1=N1C
      IX=1
      ICPS=0
      IF (NOC.LE.0) ICPS=1
      NOC2 = ABS(NOC)
      IF(ZV.LT.ZX) THEN
         IX=2
         IF(ZY.LT.ZX) IX=3
         IV=1
         IY=5-IX
         IF(ZY.LE.ZV) THEN
            IV=5-IX
            IY=1
         ENDIF
      ELSE
         IF(ZY.LT.ZX) IX=2
         IV=3-IX
         IY=3
         IF(ZY.LE.ZV) THEN
            IV=3
            IY=3-IX
         ENDIF
      ENDIF
C
      X(IX)=XV+DX
      X(IV)=XV
      X(IY)=XV
      Y(IX)=YV
      Y(IV)=YV
      Y(IY)=YV+DY
      Z(IX)=ZX
      Z(IV)=ZV
      Z(IY)=ZY
C-----------------------------------------------------------------------
C Postscript extension using gradient fill, Guido Huysmans 15/11/2000
C-----------------------------------------------------------------------
      IF (ICPS.EQ.1) THEN
        DO I=1,3
           XP(I) = X(I)
	   YP(I) = Y(I)
    	   IF(ICORD.NE.0) THEN
             FLP1=1._R8
             IF(ICORD.EQ.1) THEN
                IF(YP(I).GT.PI.AND.YP(I).LT.TPI) FLP1=-1._R8
                YP(I)=COS(YP(I))
             ENDIF
             YP(I)=MIN(MAX(-1.0_R8,YP(I)),1.0_R8)
             TX1=YP(I)*XP(I)
             YP(I)=FLP1*XP(I)*SQRT(1._R8-YP(I)*YP(I))
             XP(I)=TX1
           ENDIF
	 XP(I) = FX0 + XP(I)* XFAC
	 YP(I) = FY0 + YP(I)* YFAC
        ENDDO
      CALL FILLTRIA(XP,YP,Z,ZC(1),ZC(NOC2))
      RETURN
      ENDIF
C-----------------------------------------------------------------------

      IF(Z(1).EQ.Z(3)) RETURN
C
      DO 10 IC=1,NOC2
         IF(ZC(IC).LT.Z(1)) GOTO 10
         IF(ZC(IC).GT.Z(3)) GOTO 20
         FRAC=(ZC(IC)-Z(1))/(Z(3)-Z(1))
         X1=X(1)+(X(3)-X(1))*FRAC
         Y1=Y(1)+(Y(3)-Y(1))*FRAC
         IF(ZC(IC).LE.Z(2).AND.Z(1).NE.Z(2)) THEN
            FRAC=(ZC(IC)-Z(1))/(Z(2)-Z(1))
            X2=X(1)+FRAC*(X(2)-X(1))
            Y2=Y(1)+FRAC*(Y(2)-Y(1))
         ELSE
            FRAC=(ZC(IC)-Z(2))/(Z(3)-Z(2))
            X2=X(2)+FRAC*(X(3)-X(2))
            Y2=Y(2)+FRAC*(Y(3)-Y(2))
         ENDIF
         IF(ICORD.NE.0) THEN
            FLP1=1._R8
            FLP2=1._R8
            IF(ICORD.EQ.1) THEN
               IF(Y1.GT.PI.AND.Y1.LT.TPI) FLP1=-1._R8
               IF(Y2.GT.PI.AND.Y2.LT.TPI) FLP2=-1._R8
               Y1=COS(Y1)
               Y2=COS(Y2)
            ENDIF
            Y1=MIN(MAX(-1.0_R8,Y1),1.0_R8)
            Y2=MIN(MAX(-1.0_R8,Y2),1.0_R8)
            TX1=Y1*X1
            Y1=FLP1*X1*SQRT(1.0_R8-Y1*Y1)
            X1=TX1
            TX2=Y2*X2
            Y2=FLP2*X2*SQRT(1.0_R8-Y2*Y2)
            X2=TX2
         ENDIF
         ZIX1=FX0+X1*XFAC
         ZIY1=FY0+Y1*YFAC
         ZIX2=FX0+X2*XFAC
         ZIY2=FY0+Y2*YFAC
         CALL LINCOL(0)
	 CALL DRV(ZIX1,ZIY1,ZIX2,ZIY2)
         ISYM(IC)=ISYM(IC)+NLAB
         IF(ISYM(IC).GE.1) THEN
            ICC=IC+N1-1
            IDX=8
            IDY=0
            IF(ABS(ZIX2-ZIX1).GE.ABS(ZIY2-ZIY1)) THEN
               IDX=0
               IDY=8
            ENDIF
            IX11=MIN(MAX(IXL+5,INT(ZIX1)+IDX),IXR-5)
            IY11=MIN(MAX(IYB+5,INT(ZIY1)+IDY),IYT-5)
            CALL DLCH(IX11,-IY11,' ',ICC,1)
            ISYM(IC)=-ISKIP
         ENDIF
   10 CONTINUE
C
   20 RETURN
      END

************************************************************************
      SUBROUTINE CPLOTM(MX,MY,ILAB1,X,Y,NX,NY,INCX,INCY,Z,NDIM,ZC,NC,
     A                   TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)
C
C***********************************************************************
C SUBROUTINE CPLOTM FOR CONTOUR PLOTS ON NON-EQUIDISTANT GRIDS IN X,Y  *
C     see CPLOT except X and Y are 2-D arrays with the position at     *
C     every grid point of Z.                                           *
C     Positions of contour are calculated with a linear interpolation  *
C     of X and Y.                                                      *
C     ILAB=10 or 11  : plot the irregular grid                         *
C ENTRY CPLOTXM uses the (r,theta)  coordinate system                  *
C                                                                      *
C Guido Huysmans 21-7-99                                               *
C***********************************************************************
C
      use itm_types
      implicit none
      integer n1
      PARAMETER (N1=97)
C
      real (r8) rmax
      integer MX,MY,ILAB1,NX,NY,INCX,INCY,NDIM,NC,
     A     NTITLE,NXNAME,NYNAME,iquad,lgz
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      COMMON /CPLCOM/XFAC,YFAC,FX0,FY0,ISYM(26),NLAB,N1C
      real (r8) XFAC,YFAC,FX0,FY0
      integer ISYM,NLAB,N1C
      real (r8) X(NDIM,*),Y(NDIM,*),Z(NDIM,*),ZC(*)
      CHARACTER*(*) TITLE,XNAME,YNAME
      real (r8) ZT(4),XZ(4),YZ(4),XP(NDIM),YP(NDIM)
      CHARACTER*19 AMIN,AMAX
      CHARACTER*80 TITLE1
      LOGICAL FLGZ
      real (r8) rmx,xmn,ymn,xmx,ymx,delz,zmax,zmin,alog19,step,zct,
     &     xbar,ybart,ybarb,xtt,ytt
      integer ilab,i,ilgz,icord,nnx,nny,inx,iny,j,nb,ntitl1,noc,icps,
     &     idum,jdum,lgmx,ic,lgmn,ldec,id,ii,iqud,iplgr
C
C     * INITIALIZE N1 AND ISYM FOR USE IN TRICJ3.
      N1C=N1
      ILAB  = MOD(ILAB1,10)
      DO 5 I=1,26
    5    ISYM(I)=0
C
C     * INITIALIZE FOR SCALAR CONTOURS.
      ILGZ=0
C     * FLAG DOWN FOR OVERRIDING THE AUTOMATIC DETERMINATION OF THE
C     * NUMBER OF CONTOURS PER DECADE IN THE CASE OF LOG10 CONTOURS.
      FLGZ=.FALSE.
C
C     * INPUT PARAMETERS.
   10 ICORD=0
      RMX=0._R8
      NNX=IABS(NX)
      NNY=IABS(NY)
      IF(NNY.LE.1) RETURN
      NLAB=ILAB
      INX=IABS(INCX)
      INY=IABS(INCY)
      XMN = 1.e20_R8
      XMX = -XMN
      YMN =  XMN
      YMX =  XMX
      DO I=1,NX,INX
        DO J=1,NY,INY
	  IF (X(I,J).GT.XMX) XMX=X(I,J)
	  IF (X(I,J).LT.XMN) XMN=X(I,J)
	  IF (Y(I,J).GT.YMX) YMX=Y(I,J)
	  IF (Y(I,J).LT.YMN) YMN=Y(I,J)
        ENDDO
      ENDDO
C
C     * DRAW THE FRAME.
   20 IF(NY.GE.0) THEN
         NB=0
         IF(ILAB.EQ.1) THEN
            NB=8
            IF(ILGZ.NE.0) NB=14
         ENDIF
         TITLE1=TITLE
         NTITL1=ISIGN(MIN(IABS(NTITLE)+NB,80),NTITLE)
         CALL NFRAME(MX,MY,1,XMN,XMX,YMN,YMX,
     A               TITLE1,NTITL1,XNAME,NXNAME,YNAME,NYNAME)
         IF(NX.LT.0) RETURN
      ELSE
         CALL OFRAME(MX,MY)
      ENDIF
C
C     PLOT GRID
C
      IF (ILAB1.GT.9) THEN
        DO I=1,NY
          IF (ICORD.EQ.0) THEN
            DO J=1,NX
              XP(J) = X(J,I)
              YP(J) = Y(J,I)
            ENDDO
	  ELSE
            DO J=1,NX
              XP(J) = X(J,I)*COS(Y(J,I))
	      YP(J) = X(J,I)*SIN(Y(J,I))
	    ENDDO
	  ENDIF
          CALL DPLOT(MX,MY,XP,YP,NX,1,2,8)
        ENDDO
        DO I=1,NX
          IF (ICORD.EQ.0) THEN
            DO J=1,NY
              XP(J) = X(I,J)
              YP(J) = Y(I,J)
	    ENDDO
	  ELSE
            DO J=1,NY
              XP(J) = X(I,J)*COS(Y(I,J))
	      YP(J) = X(I,J)*SIN(Y(I,J))
	    ENDDO
	  ENDIF
          CALL DPLOT(MX,MY,XP,YP,NY,1,2,8)
        ENDDO
      ENDIF
C
C     * PARAMETERS FOR COMMON /CPLCOM/ SHARED WITH SUBROUTINE TRICJ3.
C     * INT(FX0) AND INT(FY0) ARE THE INTEGER LOCATIONS OF X=0 AND Y=0.
      XFAC=REAL(IXR-IXL,R8)/(XR-XL)
      YFAC=REAL(IYT-IYB,R8)/(YT-YB)
      FX0=REAL(IXL,R8)-XL*XFAC
      FY0=REAL(IYB,R8)-YB*YFAC
C
C     * DETERMINE CONTOUR VALUES AND NUMBER OF CONTOURS.
C      NOC=MIN(26,IABS(NC))
      NOC=ABS(NC)

      ICPS = NOC
      IF (ILAB.LT.0) THEN
        ICPS = -NOC
      ENDIF
      IF(NC.LE.0) THEN
         CALL MINM(Z,NDIM,NNX,NNY,INX,INY,ZMIN,IDUM,JDUM)
         CALL MAXM(Z,NDIM,NNX,NNY,INX,INY,ZMAX,IDUM,JDUM)
         IF(ILGZ.EQ.0) THEN
            DELZ=(ZMAX-ZMIN)/NOC
            DO 30 IC=1,NOC
   30          ZC(IC)=ZMIN+(REAL(IC,R8)-.5)*DELZ
         ELSE
            LGMX=ALOG19(ZMAX)
            IF(ZMAX.LT.1._R8) LGMX=LGMX-1
            LGMN=ALOG19(ZMIN)
            IF(ZMIN.LT.1._R8) LGMN=LGMN-1
            LGMN=MAX(LGMN,LGMX-25)
            LDEC=LGMX-LGMN+1
            IF(FLGZ) THEN
               IF(ILGZ.EQ.1) LDEC=MIN(LDEC,2)
               IF(ILGZ.EQ.2) LDEC=MIN(LDEC,5)
               LDEC=MIN(LDEC,8)
               LGMN=LGMX-LDEC+1
            ELSE
               IF(LDEC.GT.2.AND.ILGZ.LT.2) ILGZ=2
               IF(LDEC.GT.5.AND.ILGZ.LT.3) ILGZ=4
               IF(LDEC.GT.8) ILGZ=10
            ENDIF
            IC=0
            STEP=10._R8**LGMN
            DO 50 ID=1,LDEC
               DO 40 II=1,9,ILGZ
                  ZCT=REAL(II,R8)*STEP
                  IF((IC.EQ.NOC).OR.(ZCT.GT.ZMAX)) GOTO 60
                  IC=IC+1
   40             ZC(IC)=ZCT
   50          STEP=STEP*10._R8
   60       NOC=IC
         ENDIF
      ENDIF
C
C     * PUT EXTREME PARAMETERS ALONG THE TOP OF THE GRAPH.
      IF(ILAB.EQ.1) THEN
         IF(ILGZ.EQ.0) THEN
            WRITE(AMIN,'(''='',1PE9.2)') ZC(1)
            WRITE(AMAX,'(''='',1PE9.2)') ZC(NOC)
            CALL DLCH(IXR-90,IYT+18,' ',N1,1)
            CALL DLCH(IXR-75,IYT+18,AMIN,10,1)
            CALL DLCH(IXR-90,IYT+4,' ',N1+NOC-1,1)
            CALL DLCH(IXR-75,IYT+4,AMAX,10,1)
         ELSE
            WRITE(AMIN,'(''='',1PE9.2,'' ILGZ ='',I2)') ZC(1),ILGZ
            WRITE(AMAX,'(''='',1PE9.2,'' LDEC ='',I2)') ZC(NOC),LDEC
            CALL DLCH(IXR-170,IYT+18,' ',N1,1)
            CALL DLCH(IXR-155,IYT+18,AMIN,19,1)
            CALL DLCH(IXR-170,IYT+4,' ',N1+NOC-1,1)
            CALL DLCH(IXR-155,IYT+4,AMAX,19,1)
         ENDIF
      ENDIF
C
C     * DRAW THE CONTOURS BY CALLING TRICJ3 FOR THE TWO TRIANGLES
C     * WITHIN A MESH OF THE GRID.
      DO 80 J=1+INY,NNY,INY
         DO 70 I=1+INX,NNX,INX
            ZT(1)=Z(I-INX,J-INY)
            ZT(2)=Z(I,J-INY)
            ZT(3)=Z(I,J)
            ZT(4)=Z(I-INX,J)
	    XZ(1)=X(I-INX,J-INY)
            XZ(2)=X(I,J-INY)
            XZ(3)=X(I,J)
            XZ(4)=X(I-INX,J)
	    YZ(1)=Y(I-INX,J-INY)
            YZ(2)=Y(I,J-INY)
            YZ(3)=Y(I,J)
            YZ(4)=Y(I-INX,J)
            IF(ABS(ZT(3)-ZT(1)).GE.ABS(ZT(4)-ZT(2))) THEN
              CALL TRICJ3M(XZ(2),YZ(2),XZ(1),YZ(1),XZ(4),YZ(4),ICPS,ZC,
     >                     ZT(2),ZT(1),ZT(4),ICORD)
              CALL TRICJ3M(XZ(4),YZ(4),XZ(3),YZ(3),XZ(2),YZ(2),ICPS,ZC,
     >                     ZT(4),ZT(3),ZT(2),ICORD)
            ELSE
              CALL TRICJ3M(XZ(1),YZ(1),XZ(2),YZ(2),XZ(3),YZ(3),ICPS,ZC,
     >                     ZT(1),ZT(2),ZT(3),ICORD)
              CALL TRICJ3M(XZ(3),YZ(3),XZ(4),YZ(4),XZ(1),YZ(1),ICPS,ZC,
     >                     ZT(3),ZT(4),ZT(1),ICORD)
            ENDIF
   70    CONTINUE
   80 CONTINUE
C
c-------------------------- postscript extension ad colorbar
      IF (ILAB.EQ.-1) THEN
	XBAR  = REAL(IXR,R8)
	YBART = REAL(IYT,R8)
	YBARB = REAL(IYB,R8)
        CALL COLORBAR(ZC,NOC,XBAR,YBART,YBARB)
      ENDIF
      RETURN
C
C     * ENTRY FOR POLAR PLOTS AND LOG10 CONTOURS.
      ENTRY CPLOTXM(MX,MY,ILAB1,X,Y,NX,NY,INCX,INCY,Z,NDIM,ZC,NC,
     A             TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME,
     B             RMAX,IQUAD,LGZ)
C
      IPLGR = ISIGN(ILAB1,1)
      ILAB  = IABS(ILAB1)
      ILGZ=MIN(IABS(LGZ),4)
      IF(ILGZ.LT.0) FLGZ=.TRUE.
      IF(RMAX.EQ.0._R8) GOTO 10
C
      ICORD=1
      IF(RMAX.LT.0._R8) ICORD=2
      RMX=ABS(RMAX)
      NNX=IABS(NX)
      NNY=IABS(NY)
      IF(NNY.LE.1) RETURN
      NLAB=ILAB
      INX=IABS(INCX)
      INY=IABS(INCY)
      XMN = 1.e20_R8
      XMX = -XMN
      YMN =  XMN
      YMX =  XMX
      DO I=1,NX,INX
        DO J=1,NY,INY
	  XTT = X(I,J)*COS(Y(I,J))
	  YTT = X(I,J)*SIN(Y(I,J))
	  IF (XTT.GT.XMX) XMX=XTT
	  IF (XTT.LT.XMN) XMN=XTT
	  IF (YTT.GT.YMX) YMX=YTT
	  IF (YTT.LT.YMN) YMN=YTT
        ENDDO
      ENDDO
      IQUD=MAX(IQUAD,1)
      IF(IQUD.GT.2.AND.RMAX.LT.0._R8) IQUD=2
      IF (IQUD.EQ.1) XMN=0._R8
      IF (IQUD.LE.2) YMN=0._R8
      GOTO 20
C
      END
C
************************************************************************
      SUBROUTINE CPLOTFE(MX,MY,ILAB1,X,Y,Z,NX,INC,ZC,NC,
     A                   TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)
C
C***********************************************************************
C SUBROUTINE CPLOTFE FOR CONTOUR PLOTS ON IRREGULAR GRID IN X,Y        *
C     see CPLOT except X and Y and Z are given as a set of 'squares'   *
C     X(i,1:4),Y(i,1:4),Z(i,1:4)                                       *
C     Positions of contour are calculated with a linear interpolation  *
C     of X and Y.                                                      *
C     ILAB=10 or 11  : plot the irregular grid                         *
C ENTRY CPLOTXM uses the (r,theta)  coordinate system                  *
C                                                                      *
C Guido Huysmans 21-7-99                                               *
C***********************************************************************
C
      use itm_types
      implicit none
      integer n1
      PARAMETER (N1=97)
C
      integer MX,MY,ILAB1,NX,INC,NC,NTITLE,NXNAME,NYNAME
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      COMMON /CPLCOM/XFAC,YFAC,FX0,FY0,ISYM(26),NLAB,N1C
      real (r8) XFAC,YFAC,FX0,FY0
      integer ISYM,NLAB,N1C
      real (r8) X(4,*),Y(4,*),Z(4,*),ZC(*)
      CHARACTER*(*) TITLE,XNAME,YNAME
      real (r8) ZT(4),XZ(4),YZ(4),XP(5),YP(5)
      CHARACTER*19 AMIN,AMAX
      CHARACTER*80 TITLE1
      LOGICAL FLGZ
      real (r8) rmx,xmn,xmx,ymn,ymx,zmin,zmax,delz,alog19,
     &     step,zct,xbar,ybart,ybarb
      integer ilab,i,ilgz,icord,nnx,inx,j,nb,ntitl1,j1,noc,
     &     icps,idum,jdum,ic,lgmx,lgmn,ldec,id,ii
C
C     * INITIALIZE N1 AND ISYM FOR USE IN TRICJ3.
      N1C=N1
      ILAB  = MOD(ILAB1,10)
      DO I=1,26
         ISYM(I)=0
      ENDDO
C
C     * INITIALIZE FOR SCALAR CONTOURS.
      ILGZ=0
C     * FLAG DOWN FOR OVERRIDING THE AUTOMATIC DETERMINATION OF THE
C     * NUMBER OF CONTOURS PER DECADE IN THE CASE OF LOG10 CONTOURS.
      FLGZ=.FALSE.
C
C     * INPUT PARAMETERS.
   10 ICORD=0
      RMX=0._R8
      NNX=IABS(NX)
      NLAB=ILAB
      INX=IABS(INC)
      XMN = 1.e20_R8
      XMX = -XMN
      YMN =  XMN
      YMX =  XMX
      DO I=1,NNX,INC
        DO J=1,4
	  IF (X(J,I).GT.XMX) XMX=X(J,I)
	  IF (X(J,I).LT.XMN) XMN=X(J,I)
	  IF (Y(J,I).GT.YMX) YMX=Y(J,I)
	  IF (Y(J,I).LT.YMN) YMN=Y(J,I)
        ENDDO
      ENDDO
C
C     * DRAW THE FRAME.
      IF(NX.GE.0) THEN
         NB=0
         IF(ILAB.EQ.1) THEN
            NB=8
            IF(ILGZ.NE.0) NB=14
         ENDIF
         TITLE1=TITLE
         NTITL1=ISIGN(MIN(IABS(NTITLE)+NB,80),NTITLE)
         CALL NFRAME(MX,MY,1,XMN,XMX,YMN,YMX,
     A               TITLE1,NTITL1,XNAME,NXNAME,YNAME,NYNAME)
      ELSE
         CALL OFRAME(MX,MY)
      ENDIF
      NX = ABS(NX)

C
C     PLOT GRID
C
      IF (ILAB1.GT.9) THEN
        DO I=1,NX
            DO J=1,5
	      J1 = MOD(J-1,4) + 1
              XP(J) = X(J1,I)
              YP(J) = Y(J1,I)
            ENDDO
!	  CALL LINCOL(2)
	  CALL DPLOT(MX,MY,XP,YP,4,1,2,8)	
!          CALL LPLOT6(MX,MY,XP,YP,-4,' ')
	ENDDO
      ENDIF
!      CALL LINCOL(3)
C
C     * PARAMETERS FOR COMMON /CPLCOM/ SHARED WITH SUBROUTINE TRICJ3.
C     * INT(FX0) AND INT(FY0) ARE THE INTEGER LOCATIONS OF X=0 AND Y=0.
      XFAC=REAL(IXR-IXL,R8)/(XR-XL)
      YFAC=REAL(IYT-IYB,R8)/(YT-YB)
      FX0=REAL(IXL,R8)-XL*XFAC
      FY0=REAL(IYB,R8)-YB*YFAC
C
C     * DETERMINE CONTOUR VALUES AND NUMBER OF CONTOURS.
C     * DETERMINE CONTOUR VALUES AND NUMBER OF CONTOURS.
C      NOC=MIN(26,IABS(NC))
      NOC=ABS(NC)

      ICPS = NOC
      IF (ILAB.LT.0) THEN
        ICPS = -NOC
      ENDIF
      IF(NC.LE.0) THEN
         CALL MINM(Z,4,4,NX,1,INC,ZMIN,IDUM,JDUM)
         CALL MAXM(Z,4,4,NX,1,INC,ZMAX,IDUM,JDUM)
         IF(ILGZ.EQ.0) THEN
            DELZ=(ZMAX-ZMIN)/NOC
            DO IC=1,NOC
               ZC(IC)=ZMIN+(REAL(IC,R8)-.5)*DELZ
            ENDDO
         ELSE
            LGMX=ALOG19(ZMAX)
            IF(ZMAX.LT.1._R8) LGMX=LGMX-1
            LGMN=ALOG19(ZMIN)
            IF(ZMIN.LT.1._R8) LGMN=LGMN-1
            LGMN=MAX(LGMN,LGMX-25)
            LDEC=LGMX-LGMN+1
            IF(FLGZ) THEN
               IF(ILGZ.EQ.1) LDEC=MIN(LDEC,2)
               IF(ILGZ.EQ.2) LDEC=MIN(LDEC,5)
               LDEC=MIN(LDEC,8)
               LGMN=LGMX-LDEC+1
            ELSE
               IF(LDEC.GT.2.AND.ILGZ.LT.2) ILGZ=2
               IF(LDEC.GT.5.AND.ILGZ.LT.3) ILGZ=4
               IF(LDEC.GT.8) ILGZ=10
            ENDIF
            IC=0
            STEP=10._R8**LGMN
            DO ID=1,LDEC
               DO II=1,9,ILGZ
                  ZCT=REAL(II,R8)*STEP
                  IF((IC.EQ.NOC).OR.(ZCT.GT.ZMAX)) GOTO 60
                  IC=IC+1
                  ZC(IC)=ZCT
               ENDDO
               STEP=STEP*10._R8
            ENDDO
   60       NOC=IC
         ENDIF
      ENDIF
C
C     * PUT EXTREME PARAMETERS ALONG THE TOP OF THE GRAPH.
      IF(ILAB.EQ.1) THEN
         IF(ILGZ.EQ.0) THEN
            WRITE(AMIN,'(''='',1PE9.2)') ZC(1)
            WRITE(AMAX,'(''='',1PE9.2)') ZC(NOC)
            CALL DLCH(IXR-90,IYT+18,' ',N1,1)
            CALL DLCH(IXR-75,IYT+18,AMIN,10,1)
            CALL DLCH(IXR-90,IYT+4,' ',N1+NOC-1,1)
            CALL DLCH(IXR-75,IYT+4,AMAX,10,1)
         ELSE
            WRITE(AMIN,'(''='',1PE9.2,'' ILGZ ='',I2)') ZC(1),ILGZ
            WRITE(AMAX,'(''='',1PE9.2,'' LDEC ='',I2)') ZC(NOC),LDEC
            CALL DLCH(IXR-170,IYT+18,' ',N1,1)
            CALL DLCH(IXR-155,IYT+18,AMIN,19,1)
            CALL DLCH(IXR-170,IYT+4,' ',N1+NOC-1,1)
            CALL DLCH(IXR-155,IYT+4,AMAX,19,1)
         ENDIF
      ENDIF
C
C     * DRAW THE CONTOURS BY CALLING TRICJ3 FOR THE TWO TRIANGLES
C     * WITHIN A MESH OF THE GRID.
      DO I=1,NX,INC
         DO J=1,4
            ZT(J)=Z(J,I)
            XZ(J)=X(J,I)
	    YZ(J)=Y(J,I)
	  ENDDO	
          IF(ABS(ZT(3)-ZT(1)).GE.ABS(ZT(4)-ZT(2))) THEN
              CALL TRICJ3M(XZ(2),YZ(2),XZ(1),YZ(1),XZ(4),YZ(4),ICPS,ZC,
     >                     ZT(2),ZT(1),ZT(4),ICORD)
              CALL TRICJ3M(XZ(4),YZ(4),XZ(3),YZ(3),XZ(2),YZ(2),ICPS,ZC,
     >                     ZT(4),ZT(3),ZT(2),ICORD)
          ELSE
              CALL TRICJ3M(XZ(1),YZ(1),XZ(2),YZ(2),XZ(3),YZ(3),ICPS,ZC,
     >                     ZT(1),ZT(2),ZT(3),ICORD)
              CALL TRICJ3M(XZ(3),YZ(3),XZ(4),YZ(4),XZ(1),YZ(1),ICPS,ZC,
     >                     ZT(3),ZT(4),ZT(1),ICORD)
          ENDIF
   70 ENDDO
      CALL LINCOL(0)
c-------------------------- postscript extension ad colorbar
      IF (ICPS.LT.0) THEN
	XBAR  = REAL(IXR,R8)
	YBART = REAL(IYT,R8)
	YBARB = REAL(IYB,R8)
        CALL COLORBAR(ZC,NOC,XBAR,YBART,YBARB)
      ENDIF
C
      RETURN
      END
C
C
      SUBROUTINE TRICJ3M(XX,YX,XV,YV,XY,YY,NOC,ZC,ZX,ZV,ZY,ICORD)
C
C***********************************************************************
C     THIS SUBROUTINE IS CALLED FROM CPLOT TO DETERMINE THE PARTS OF   *
C THE CONTOURS THAT LIE WITHIN A TRIANGLE OF THE GRID MESH.  TRICJ3    *
C FINDS THE INTERSECTIONS OF THE CONTOURS WITH THE TWO SIDES OF THE    *
C TRIANGLE AND DRAWS LINES BETWEEN THOSE POINTS.                       *
C     IF NLABEL=1 IN COMMON /CPLCOM/, ALPHABETIC LABELS ARE WRITTEN    *
C EVERY #(ISKIP+1) CALL OF TRICJ3.  ISKIP IS FIXED IN THE PARAMETER    *
C STATEMENT BELOW.                                                     *
C                                                                      *
C     MODIFIED BY D.W. HEWETT 12-82, FOR THE DIFFERENT COORDINATES     *
C     X,Y (ICORD=0), R,THETA (ICORD=1), AND  R,COS(THETA) (ICORD=2).   *
C     ADDED PARAMETER ISKIP, ADDED CHECK ON RANGE Y1/2, HGO 9/12/85.   *
C***********************************************************************
C
      use itm_types
      implicit none
      integer iskip
      PARAMETER (ISKIP=9)
C
      real (r8) XX,YX,XV,YV,XY,YY,ZX,ZV,ZY
      integer NOC,ICORD
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      COMMON /CPLCOM/XFAC,YFAC,FX0,FY0,ISYM(26),NLAB,N1C
      real (r8) XFAC,YFAC,FX0,FY0
      integer ISYM,NLAB,N1C
      real (r8) ZC(*)
      real (r8) X(3),Y(3),Z(3),XP(3),YP(3)
      real (r8) flp1,pi,tpi,tx1,frac,x1,y1,x2,y2,flp2,tx2,
     &     zix1,ziy1,zix2,ziy2
      integer icps,noc2,n1,ix,iv,iy,i,ic,icc,idx,idy,ix11,iy11
C
      ICPS=0
      IF (NOC.LE.0) ICPS=1
      NOC2 = ABS(NOC)
      N1=N1C
      IX=1
      IF(ZV.LT.ZX) THEN
         IX=2
         IF(ZY.LT.ZX) IX=3
         IV=1
         IY=5-IX
         IF(ZY.LE.ZV) THEN
            IV=5-IX
            IY=1
         ENDIF
      ELSE
         IF(ZY.LT.ZX) IX=2
         IV=3-IX
         IY=3
         IF(ZY.LE.ZV) THEN
            IV=3
            IY=3-IX
         ENDIF
      ENDIF
C
      X(IX)=XX
      X(IV)=XV
      X(IY)=XY
      Y(IX)=YX
      Y(IV)=YV
      Y(IY)=YY
      Z(IX)=ZX
      Z(IV)=ZV
      Z(IY)=ZY

C-----------------------------------------------------------------------
C Postscript extension using gradient fill, Guido Huysmans 15/11/2000
C-----------------------------------------------------------------------
      IF (ICPS.EQ.1) THEN
        DO I=1,3
           XP(I) = X(I)
	   YP(I) = Y(I)
    	   IF(ICORD.NE.0) THEN
             FLP1=1._R8
             IF(ICORD.EQ.1) THEN
                IF(YP(I).GT.PI.AND.YP(I).LT.TPI) FLP1=-1._R8
                YP(I)=COS(YP(I))
             ENDIF
             YP(I)=MIN(MAX(-1.0_R8,YP(I)),1.0_R8)
             TX1=YP(I)*XP(I)
             YP(I)=FLP1*XP(I)*SQRT(1.0_R8-YP(I)*YP(I))
             XP(I)=TX1
           ENDIF
	 XP(I) = FX0 + XP(I)* XFAC
	 YP(I) = FY0 + YP(I)* YFAC
        ENDDO
      CALL FILLTRIA(XP,YP,Z,ZC(1),ZC(NOC2))
      RETURN
      ENDIF

      IF(Z(1).EQ.Z(3)) RETURN
C
      PI=3.1415926535898_R8
      TPI=2._R8*PI
      DO 10 IC=1,NOC
         IF(ZC(IC).LT.Z(1)) GOTO 10
         IF(ZC(IC).GT.Z(3)) GOTO 20
         FRAC=(ZC(IC)-Z(1))/(Z(3)-Z(1))
         X1=X(1)+(X(3)-X(1))*FRAC
         Y1=Y(1)+(Y(3)-Y(1))*FRAC
         IF(ZC(IC).LE.Z(2).AND.Z(1).NE.Z(2)) THEN
            FRAC=(ZC(IC)-Z(1))/(Z(2)-Z(1))
            X2=X(1)+FRAC*(X(2)-X(1))
            Y2=Y(1)+FRAC*(Y(2)-Y(1))
         ELSE
            FRAC=(ZC(IC)-Z(2))/(Z(3)-Z(2))
            X2=X(2)+FRAC*(X(3)-X(2))
            Y2=Y(2)+FRAC*(Y(3)-Y(2))
         ENDIF
         IF(ICORD.NE.0) THEN
            FLP1=1._R8
            FLP2=1._R8
            IF(ICORD.EQ.1) THEN
               IF(Y1.GT.PI.AND.Y1.LT.TPI) FLP1=-1._R8
               IF(Y2.GT.PI.AND.Y2.LT.TPI) FLP2=-1._R8
               Y1=COS(Y1)
               Y2=COS(Y2)
            ENDIF
            Y1=MIN(MAX(-1.0_R8,Y1),1.0_R8)
            Y2=MIN(MAX(-1.0_R8,Y2),1.0_R8)
            TX1=Y1*X1
            Y1=FLP1*X1*SQRT(1.0_R8-Y1*Y1)
            X1=TX1
            TX2=Y2*X2
            Y2=FLP2*X2*SQRT(1.0_R8-Y2*Y2)
            X2=TX2
         ENDIF
         ZIX1=FX0+X1*XFAC
         ZIY1=FY0+Y1*YFAC
         ZIX2=FX0+X2*XFAC
         ZIY2=FY0+Y2*YFAC
         CALL DRV(ZIX1,ZIY1,ZIX2,ZIY2)
         ISYM(IC)=ISYM(IC)+NLAB
         IF(ISYM(IC).GE.1) THEN
            ICC=IC+N1-1
            IDX=8
            IDY=0
            IF(ABS(ZIX2-ZIX1).GE.ABS(ZIY2-ZIY1)) THEN
               IDX=0
               IDY=8
            ENDIF
            IX11=MIN(MAX(IXL+5,INT(ZIX1)+IDX),IXR-5)
            IY11=MIN(MAX(IYB+5,INT(ZIY1)+IDY),IYT-5)
            CALL DLCH(IX11,-IY11,' ',ICC,1)
            ISYM(IC)=-ISKIP
         ENDIF
   10 CONTINUE
C
   20 RETURN
      END
C
      SUBROUTINE QCPLOT(NX,NY,INCX,INCY,Z,NDIM,ZC,NC,
     A                  TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME,
     B                  LGZ,IOUNIT)
C
C***********************************************************************
C     NONGRAPHICS CONTOUR PLOTTER.  QUICK TEST OF THE LAYOUT OF PLOTS  *
C TO BE MADE WITH CPLOT/CPLOTX.  WRITES AN ARRAY OF LETTERS TO FORM A  *
C "CONTOUR" PLOT ON PRINTED OUTPUT FROM UNIT IOUNIT.                   *
C                                                                      *
C     ARGUMENTS:                                                       *
C                                                                      *
C NX/NY  - NUMBER OF POINTS IN THE X/Y-DIRECTION TO BE PLOTTED.        *
C INCX   - SKIP PARAMETER IN A ROW.                                    *
C INCY   - SKIP PARAMETER IN A COLUMN.                                 *
C Z      - THE TWO-DIMENSIONAL FUNCTION TO BE CONTOURED; Z SHOULD BE   *
C          STORED SO THAT Z(I,J) IS THE VALUE OF Z AT X(I),Y(J).  THIS *
C          CORRESPONDS TO I ACROSS AND J ALONG THE PAGE.               *
C NDIM   - LENGTH OF A ROW OF Z (1ST DIMENSION OF THE 2-D ARRAY).      *
C ZC     - THE TABLE OF CONTOUR VALUES.                                *
C NC     - NUMBER OF CONTOURS TO BE PLOTTED; MAXIMUM OF 26.            *
C          NC < 0:  QCPLOT AUTOMATICALLY FILLS ZC WITH NC VALUES.      *
C          NC > 0:  ZC IS SUPPLIED BY THE USER; VALUES MUST BE STORED  *
C          IN INCREASING ORDER IN ZC.                                  *
C TITLE                - TITLE FOR THE GRAPH.                          *
C XNAME/YNAME          - LABEL FOR THE X/Y-AXIS.                       *
C NTITLE/NXNAME/NYNAME - NUMBER OF CHARACTERS IN TITLE/XNAME/YNAME.    *
C LGZ    - CONTROLS THE NUMBER OF LOG10 CONTOURS; SEE CPLOTX.          *
C IOUNIT - UNIT NUMBER FOR THE PRINTED OUTPUT.                         *
C                                                                      *
C     WRITTEN BY D.W. HEWETT 1/15/83                                   *
C***********************************************************************
C
      use itm_types
      implicit none
      integer NX,NY,INCX,INCY,NDIM,NC,NTITLE,NXNAME,NYNAME,
     B                  LGZ,IOUNIT
      CHARACTER*(*) TITLE,XNAME,YNAME
      real (r8) Z(NDIM,*),ZC(*)
      integer IROW(80)
      real (r8) zmin,zmax,delz,alog19,step,zct
      integer nnx,nny,ilgz,inx,iny,ncut,noc,idum,jdum,ic,lgmx, lgmn,
     &     ldec,id,ii,n1,j,i1,i

C
      NNX=IABS(NX)
      NNY=IABS(NY)
      IF(NNY.LE.1) RETURN
      ILGZ=IABS(LGZ)
      INX=IABS(INCX)
      INY=IABS(INCY)
   10 NCUT=NNX/INX
      IF(NCUT.GE.75) THEN
         INX=INX+INX
         WRITE(IOUNIT,11) INX
         GOTO 10
      ENDIF
C
C      NOC=MIN(26,IABS(NC))
      NOC=ABS(NC)
      IF(NC.LE.0) THEN
         CALL MINM(Z,NDIM,NNX,NNY,INX,INY,ZMIN,IDUM,JDUM)
         CALL MAXM(Z,NDIM,NNX,NNY,INX,INY,ZMAX,IDUM,JDUM)
         IF(ILGZ.EQ.0) THEN
            DELZ=(ZMAX-ZMIN)/NOC
            DO 30 IC=1,NOC
   30          ZC(IC)=ZMIN+(REAL(IC,R8)-.5)*DELZ
         ELSE
            LGMX=ALOG19(ZMAX)
            IF(ZMAX.LT.1._R8) LGMX=LGMX-1
            LGMN=ALOG19(ZMIN)
            IF(ZMIN.LT.1._R8) LGMN=LGMN-1
            LGMN=MAX(LGMN,LGMX-25)
            LDEC=LGMX-LGMN+1
            IF(LGZ.LT.0) THEN
               IF(ILGZ.EQ.1) LDEC=MIN(LDEC,2)
               IF(ILGZ.EQ.2) LDEC=MIN(LDEC,5)
               LDEC=MIN(LDEC,8)
               LGMN=LGMX-LDEC+1
            ELSE
               IF(LDEC.GT.2.AND.ILGZ.LT.2) ILGZ=2
               IF(LDEC.GT.5.AND.ILGZ.LT.3) ILGZ=4
               IF(LDEC.GT.8) ILGZ=10
            ENDIF
            IC=0
            STEP=10._R8**LGMN
            DO 50 ID=1,LDEC
               DO 40 II=1,9,ILGZ
                  ZCT=REAL(II,R8)*STEP
                  IF((IC.EQ.NOC).OR.(ZCT.GT.ZMAX)) GOTO 60
                  IC=IC+1
   40             ZC(IC)=ZCT
   50          STEP=STEP*10._R8
   60       NOC=IC
         ENDIF
      ENDIF
C
      WRITE(IOUNIT,61) TITLE
      WRITE(IOUNIT,62) XNAME(1:LEN(XNAME)),YNAME(1:LEN(YNAME))
      N1=ICHAR('A')
      WRITE(IOUNIT,63) CHAR(N1),ZC(1),CHAR(N1+NOC-1),ZC(NOC)
      IF(ILGZ.NE.0) WRITE(IOUNIT,64) NOC,ZMIN,ZMAX,LDEC,LGZ,ILGZ
      WRITE(IOUNIT,65)
C
      DO 100 J=NNY,1,-INY
         I1=0
         DO 90 I=1,NNX,INX
            I1=I1+1
            DO 70 IC=1,NOC
               IF(Z(I,J).LE.ZC(IC)) GOTO 80
   70       CONTINUE
            IC=NOC+1
   80       IROW(I1)=N1+IC-1
   90    CONTINUE
         WRITE(IOUNIT,91) J,(CHAR(IROW(I)),I=1,I1)
  100 CONTINUE
      I1=0
      DO 110 I=1,NNX,INX
         I1=I1+1
  110    IROW(I1)=MOD(I,10)
      WRITE(IOUNIT,111) (IROW(I),I=1,I1)
C
      RETURN
C
C     * FORMATS.
   11 FORMAT(1X,'** NNX GREATER THAN 75, INX CHANGED TO',I5)
   61 FORMAT(/1X,'QCPLOT: ',A)
   62 FORMAT(9X,A,' HORIZONTALLY, ',A,' VERTICALLY')
   63 FORMAT(9X,A1,' =',1PE9.2,4X,A1,' =',1PE9.2)
   64 FORMAT(9X,'NOC,ZMIN,ZMAX,LDEC,LGZ,ILGZ',I5,2E14.6,3I5)
   65 FORMAT(9X,'THE SYMBOL A MEANS: IN THAT LOCATION VALUE .LE. A'/)
   91 FORMAT(I3,1X,75A1)
  111 FORMAT(4X,75I1)
      END
C
      SUBROUTINE VPLOT(MX,MY,IVEC,X,Y,NX,NY,INCX,INCY,VX,VY,NDIM,SIZE,L,
     A                 TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)
C
C***********************************************************************
C     SUBROUTINE VPLOT DRAWS A REPRESENTION OF A 2-DIMENSIONAL VECTOR  *
C FIELD VX = F(X,Y), VY = G(X,Y).  THESE FUNCTIONS SHOULD BE STORED AS *
C 2-DIMENSIONAL ARRAYS VX(I,J), VY(I,J), COMPUTED AT THE OBSERVATION   *
C POINTS X(I), I=1,IABS(NX),IABS(INCX), Y(J), J=1,IABS(NY),IABS(INCY). *
C ENTRY VPLOTX IS AN EXTENSION FOR POLAR COORDINATES.                  *
C                                                                      *
C     ARGUMENTS:                                                       *
C                                                                      *
C MX/MY - SEE LPLOT.                                                   *
C IVEC  - PROVIDES DIFFERENT OPTIONS FOR THE PRESENTATION OF THE VEC-  *
C         TOR FIELD ACCORDING TO THE FORMULA                           *
C            IABS(IVEC) = ISUP*100 + IDOT*10 + JVEC,                   *
C         WHERE JVEC DETERMINES THE SHAPE OF THE ARROWHEADS:           *
C            JVEC = 1 - SIZE ARROWHEAD PROPORTIONAL TO VECTOR LENGTH   *
C                   2 - CONSTANT-SIZE ARROWHEAD,                       *
C         AND IDOT PROVIDES THE OPTION TO IDENTIFY THE DATA POINTS:    *
C            IDOT = 0 - NO ACTION (DEFAULT)                            *
C                   1 - DOT PLACED AT THE DATA LOCATIONS,              *
C         AND ISUP DETERMINES WHETHER SMALL VECTORS ARE DRAWN OR NOT:  *
C            ISUP = 0 - NO ACTION (DEFAULT)                            *
C                   1 - SUPPRESS DOT AND VECTOR IF BOTH VECTOR COMPO-  *
C                       NENTS <= EPS * MAXIMUM AMPLITUDE OF VX AND VY, *
C                       WHERE EPS IS FIXED IN THE PARAMETER STATEMENT. *
C X/Y   - TABLE OF THE ABSCISSA/ORDINATE VALUES.                       *
C NX    - IABS(NX) IS THE NUMBER OF POINTS IN X TO BE USED.            *
C         NX < 0 : ONLY THE FRAME AND SCALES FOR THE PLOT ARE DRAWN.   *
C NY    - IABS(NY) IS THE NUMBER OF POINTS IN Y TO BE USED.            *
C         NY < 0 : VECTORS ARE DRAWN ON A FRAME PREVIOUSLY CREATED     *
C         BY A CALL TO VPLOT WITH NX < 0 OR A DIRECT CALL OF NFRAME    *
C         (IN ORDER TO SPECIFY A FRAME SIZE DIFFERENT FROM THE ONE     *
C         IMPLIED BY THE RANGES OF X AND Y).                           *
C INCX  - IABS(INCX) IS THE SKIP PARAMETER IN A ROW.                   *
C         INCX < 0 : XMIN = X(1) AND HX =X(2).                         *
C INCY  - IABS(INCY) IS THE SKIP PARAMETER IN A COLUMN.                *
C         INCY < 0 : YMIN = Y(1) AND HY =Y(2).                         *
C VX/VY - THE TWO-DIMENSIONAL VECTOR COMPONENTS TO BE PLOTTED; STORED  *
C         SUCH THAT VX/VY(I,J) IS THE VALUE OF VX/VY AT X(I),Y(J).     *
C NDIM  - LENGTH OF A ROW OF VX/VY (1ST ARGUMENT OF THE 2-D ARRAYS).   *
C         HENCE, ONE SHOULD OBSERVE: NX <= NDIM.                       *
C SIZE  - THE VECTORS ARE PLOTTED WITH THEIR MAXIMUM AMPLITUDE AMP     *
C         (PRINTED ON TOP OF THE GRAPH IF ISUP=1) SCALED DOWN WITH A   *
C         FACTOR OF SIZE*STEP/AMP, WHERE STEP IS THE WIDTH OF THE      *
C         SMALLEST MESH OF THE GRID AND SIZE IS CHOSEN FOR CLARITY OF  *
C         PRESENTATION.  FOR A UNIFORM GRID, THE LARGEST VECTORS WILL  *
C         PRECISELY FIT THE MESH WHEN SIZE=1.  FOR A NON-UNIFORM GRID, *
C         SIZE HAS TO BE CHOSEN BY CONSIDERING THE SPACING OF A TYPI-  *
C         CAL MESH AS COMPARED TO THE SMALLEST ONE.                    *
C L     - LENGTH OF THE ARROWHEADS AS AN INTEGER PERCENTAGE OF THE     *
C         VECTOR LENGTH (FOR JVEC=1) OR AS AN ABSOLUTE VALUE IN TERMS  *
C         OF PLOTTING COORDINATES (FOR JVEC=2).                        *
C TITLE               - TITLE FOR THE GRAPH.                           *
C XNAME/YNAME         - LABEL FOR THE X/Y-AXIS.                        *
C NTITLE/NXNAME/YNAME - NUMBER OF CHARACTERS IN TITLE/XNAME/YNAME.     *
C                                                                      *
C     ADDITIONAL ARGUMENTS FOR VPLOTX:                                 *
C                                                                      *
C RMAX  - MAXIMUM RADIUS FOR A POLAR PLOT.                             *
C         = 0 : CARTESIAN PLOT.                                        *
C         > 0 : X/Y CORRESPONDS TO R/THETA (IN RADIANS),               *
C               VX/VY CORRESPONDS TO THE VECTOR COMPONENT VR/VT.       *
C IQUAD - TOTAL NUMBER OF QUADRANTS (FOR RMAX.NE.0 ONLY).              *
C                                                                      *
C     WRITTEN BY D. HEWETT 3/83 BY ADAPTING A VERSION OF CPLOT.        *
C     EXTENDED FOR NON-UNIFORM GRID, ADDED ARGUMENTS IVEC, SIZE, L,    *
C     ADDED PARAMETER EPS, ELIMINATED ICORD=2 OPTION OF CPLOT/TRICJ3,  *
C     HGO 16/12/85.                                                    *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) eps
      PARAMETER (EPS=.1)
C
      integer MX,MY,IVEC,NX,NY,INCX,INCY,NDIM,L,
     A     NTITLE,NXNAME,NYNAME
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      real (r8) X(*),Y(*),VX(NDIM,*),VY(NDIM,*)
      CHARACTER*(*) TITLE,XNAME,YNAME
      CHARACTER TITLE1*80,STRING*14
      real (r8) size,rmx,hx,hy,xmn,ymn,xmx,ymx,xfac,yfac,fx0,fy0,
     &     vxmx,vymx,amp,dxmn,dx,dymn,dy,step,vfac,thr,pi,tpi,
     &     y1sav,y1,x1sav,x1,x2,y2,c,flp,s,x2s,
     &     zix1,ziy1,zix2,ziy2,rmax
      integer icord,nnx,nny,inx,iny,jvec,idot,isup,nb,ntitl1,idum,jdum,
     &     i,j,iquad,iqud
C
   10 ICORD=0
      RMX=0._R8
      NNX=IABS(NX)
      NNY=IABS(NY)
      IF(NNY.LE.1) RETURN
      INX=IABS(INCX)
      INY=IABS(INCY)
      HX=X(2)-X(1)
      HY=Y(2)-Y(1)
      XMN=X(1)
      YMN=Y(1)
      XMX=X(NNX)
      YMX=Y(NNY)
      IF(INCX.LT.0) THEN
         HX=X(2)
         XMX=X(1)+(NNX-1)*X(2)/INX
      ENDIF
      IF(INCY.LT.0) THEN
         HY=Y(2)
         YMX=Y(1)+(NNY-1)*Y(2)/INY
      ENDIF
C
   20 JVEC=MOD(IABS(IVEC),10)
      IDOT=MOD(IABS(IVEC)/10,10)
      ISUP=MOD(IABS(IVEC)/100,10)
      IF(NY.GE.0) THEN
         NB=0
         IF(ISUP.EQ.1) NB=9
         TITLE1=TITLE
         NTITL1=ISIGN(MIN(IABS(NTITLE)+NB,80),NTITLE)
         CALL NFRAME(MX,MY,1,XMN,XMX,YMN,YMX,
     A               TITLE1,NTITL1,XNAME,NXNAME,YNAME,NYNAME)
         IF(NX.LT.0) RETURN
      ELSE
         CALL OFRAME(MX,MY)
      ENDIF
C
C     * INT(FX0) AND INT(FY0) ARE THE INTEGER LOCATIONS OF X=0 AND Y=0.
      XFAC=(IXR-IXL)/(XR-XL)
      YFAC=(IYT-IYB)/(YT-YB)
      FX0=IXL-XL*XFAC
      FY0=IYT-YT*YFAC
C
      CALL MAXAM(VX,NDIM,NNX,NNY,INX,INY,VXMX,IDUM,JDUM)
      CALL MAXAM(VY,NDIM,NNX,NNY,INX,INY,VYMX,IDUM,JDUM)
      VXMX=ABS(VXMX)
      VYMX=ABS(VYMX)
      AMP=MAX(VXMX,VYMX)
      IF(ISUP.EQ.1.OR.AMP.EQ.0.) THEN
         WRITE(STRING,'(''AMP ='',1PE9.2)') AMP
         CALL DLCH(IXR-110,IYT+18,STRING,14,1)
         WRITE(STRING,'(''EPS ='',1PE9.2)') EPS
         CALL DLCH(IXR-110,IYT+4,STRING,14,1)
         IF(AMP.EQ.0._R8) AMP=1._R8
      ENDIF
      DXMN=INX*ABS(HX)
      IF(INCX.GT.0) THEN
         DO 30 I=1+INX,NNX,INX
            DX=ABS(X(I)-X(I-INX))
   30       IF(DX.LT.DXMN) DXMN=DX
      ENDIF
      DYMN=INY*ABS(HY)
      IF(INCY.GT.0) THEN
         DO 40 J=1+INY,NNY,INY
            DY=ABS(Y(J)-Y(J-INY))
   40       IF(DY.LT.DYMN) DYMN=DY
      ENDIF
      STEP=MIN(DXMN,DYMN)
      VFAC=SIZE*STEP/AMP
      THR=EPS*SIZE*STEP
C
      PI=3.1415926535898_R8
      TPI=2._R8*PI
      Y1SAV=Y(1)-HY
      DO 60 J=1,NNY,INY
         Y1=Y1SAV+HY
         IF(INCY.GT.0) Y1=Y(J)
         Y1SAV=Y1
         X1SAV=X(1)-HX
         DO 50 I=1,NNX,INX
            X1=X1SAV+HX
            IF(INCX.GT.0) X1=X(I)
            X1SAV=X1
            Y1=Y1SAV
            X2=VFAC*VX(I,J)
            Y2=VFAC*VY(I,J)
            IF(ISUP.EQ.1.AND.ABS(X2).LT.THR.AND.ABS(Y2).LT.THR) GOTO 50
            IF(ICORD.NE.0) THEN
               C=COS(Y1)
               C=MIN(MAX(-1.0_R8,C),1.0_R8)
               FLP=1._R8
               IF(Y1.GT.PI.AND.Y1.LT.TPI) FLP=-1._R8
               S=FLP*SQRT(1.0_R8-C*C)
               Y1=X1*S
               X1=X1*C
               X2S=X2
               X2=X2S*C-Y2*S
               Y2=X2S*S+Y2*C
            ENDIF
            X2=X1+X2
            Y2=Y1+Y2
            ZIX1=FX0+X1*XFAC
            ZIY1=FY0+Y1*YFAC
            ZIX2=FX0+X2*XFAC
            ZIY2=FY0+Y2*YFAC
            IF(IDOT.EQ.1) CALL DLCH(int(ZIX1),-int(ZIY1),' ',46,1)
            IF(JVEC.EQ.1) CALL ARROW1(ZIX1,ZIY1,ZIX2,ZIY2,L)
            IF(JVEC.EQ.2) CALL ARROW2(ZIX1,ZIY1,ZIX2,ZIY2,L)
   50    CONTINUE
   60 CONTINUE
C
      RETURN
C
      ENTRY VPLOTX(MX,MY,IVEC,X,Y,NX,NY,INCX,INCY,VX,VY,NDIM,SIZE,L,
     A             TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME,
     B             RMAX,IQUAD)
C
      IF(RMAX.EQ.0._R8) GOTO 10
C
      ICORD=1
      RMX=ABS(RMAX)
      NNX=IABS(NX)
      NNY=IABS(NY)
      IF(NNY.LE.1) RETURN
      INX=IABS(INCX)
      INY=IABS(INCY)
      HX=X(2)-X(1)
      HY=Y(2)-Y(1)
      IF(INCX.LT.0) HX=X(2)
      IF(INCY.LT.0) HY=Y(2)
      XMN=0._R8
      YMN=0._R8
      XMX=RMX
      YMX=RMX
      IQUD=MAX(IQUAD,1)
      IF(IQUD.GT.2.AND.RMAX.LT.0._R8) IQUD=2
      IF(IQUD.EQ.2) THEN
         XMN=-RMX
         XMX=RMX
         YMX=RMX
      ELSEIF(IQUD.EQ.3.OR.IQUD.EQ.4) THEN
         XMN=-RMX
         YMN=-RMX
      ENDIF
      GOTO 20
C
      END
C
      SUBROUTINE FPLOT(MX,MY,IVEC,X,Y,NPTS,INC,VX,VY,VFAC,L,
     A                 TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)
C
C***********************************************************************
C     FPLOT IS A ONE-DIMENSIONAL VECTOR PLOTTING ROUTINE WHICH PLOTS   *
C THE TWO-DIMENSIONAL FLOW FIELD VX = F(X,Y), VY = G(X,Y) ALONG A ONE- *
C DIMENSIONAL CURVE X(I), Y(I), I=1,..,NPTS.  CONSEQUENTLY, THE VECTOR *
C COMPONENTS SHOULD BE GIVEN AS ONE-DIMENSIONAL ARRAYS VX(I), VY(I).   *
C THE AMPLITUDE OF THE VECTOR FIELD IS SCALED WITH THE FACTOR VFAC.    *
C                                                                      *
C     ARGUMENTS:                                                       *
C                                                                      *
C MX/MY - SEE LPLOT.                                                   *
C IVEC  - PROVIDES DIFFERENT OPTIONS FOR THE PRESENTATION OF THE VEC-  *
C         TOR FIELD ACCORDING TO THE FORMULA                           *
C            IABS(IVEC) = IDOT*10 + JVEC,                              *
C         WHERE JVEC DETERMINES THE SHAPE OF THE ARROWHEADS:           *
C            JVEC = 1 - SIZE ARROWHEAD PROPORTIONAL TO VECTOR LENGTH   *
C                   2 - CONSTANT-SIZE ARROWHEAD,                       *
C         AND IDOT PROVIDES THE OPTION TO IDENTIFY THE DATA POINTS:    *
C            IDOT = 0 - NO ACTION (DEFAULT)                            *
C                   1 - DOT PLACED AT THE DATA LOCATIONS.              *
C         IVEC < 0 : ONLY FRAME AND SCALES FOR THE PLOT ARE DRAWN.     *
C X/Y   - TABLE OF ABSCISSA/ORDINATE VALUES.                           *
C NPTS  - IABS(NPTS) IS THE NUMBER OF ELEMENTS IN THE ARRAYS X AND Y.  *
C         NPTS < 0: THE VECTORS ARE DRAWN ONTO A FRAME PREVIOUSLY SET  *
C         UP BY A CALL TO NFRAME OR FPLOT WITH IVEC < 0.               *
C INC   - IABS(INC) IS THE SPACING BETWEEN THE X/Y POSITIONS PLOTTED.  *
C         INC < 0: THE Y-POSITIONS PLOTTED ARE PAIRED WITH ABSCISSA    *
C         VALUES DETERMINED BY THE TWO VALUES XMIN=X(1) AND DX=X(2),   *
C         WHICH THE USER SHOULD INSERT IN X.                           *
C VX    - 1D ARRAY CONTAINING THE X-COMPONENTS OF THE VECTOR FIELD.    *
C VY    - 1D ARRAY CONTAINING THE Y-COMPONENTS OF THE VECTOR FIELD.    *
C VFAC  - MULTIPLICATIVE FACTOR FOR THE AMPLITUDE OF THE VECTORS.      *
C L     - LENGTH OF THE ARROWHEADS AS AN INTEGER PERCENTAGE OF THE     *
C         VECTOR LENGTH (FOR JVEC=1) OR AS AN ABSOLUTE VALUE IN TERMS  *
C         OF PLOTTING COORDINATES (FOR JVEC=2).                        *
C TITLE                - TITLE FOR THE GRAPH.                          *
C XNAME/YNAME          - LABEL FOR THE X/Y-AXIS.                       *
C NTITLE/NXNAME/NYNAME - NUMBER OF CHARACTERS IN TITLE/XNAME/YNAME.    *
C                                                                      *
C     WRITTEN BY HANS GOEDBLOED 29/08/85 BY ADAPTING LPLOT.            *
C***********************************************************************
C
      use itm_types
      implicit none
      integer MX,MY,IVEC,NPTS,INC,L,NTITLE,NXNAME,NYNAME
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      real (r8) X(*),Y(*),VX(*),VY(*)
      CHARACTER*(*) TITLE,XNAME,YNAME
      real (r8) vfac,xmn,xmx,ymx,ymn,xfac,yfac,hx,x1,y1,
     &     zix1,ziy1,x2,y2,zix2,ziy2,zixx,ziyy,ziyys
      integer jvec,idot,ntot,inca,idum,i


C
      JVEC=MOD(IABS(IVEC),10)
      IDOT=MOD(IABS(IVEC)/10,10)
      NTOT=IABS(NPTS)
      INCA=IABS(INC)
C
C     * DRAW THE FRAME.
      IF(NPTS.GT.0) THEN
         IF(INC.LT.0) THEN
            XMN=X(1)
            XMX=X(1)+(NTOT-1)*X(2)/INCA
         ELSE
            CALL MAXV(X,NTOT,INCA,XMX,IDUM)
            CALL MINV(X,NTOT,INCA,XMN,IDUM)
         ENDIF
         CALL MAXV(Y,NTOT,INCA,YMX,IDUM)
         CALL MINV(Y,NTOT,INCA,YMN,IDUM)
         CALL NFRAME(MX,MY,1,XMN,XMX,YMN,YMX,
     A               TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)
         IF(IVEC.LT.0) RETURN
      ELSE
         CALL OFRAME(MX,MY)
      ENDIF
C
C     * DRAW THE VECTOR FIELD.
      XFAC=(IXR-IXL)/(XR-XL)
      YFAC=(IYT-IYB)/(YT-YB)
      HX=0._R8
      IF(INC.LT.0) HX=X(2)
      X1=X(1)-HX
      DO 10 I=1,NTOT,INCA
         X1=X1+HX
         IF(INC.GT.0) X1=X(I)
         Y1=Y(I)
         ZIX1=MIN(MAX(REAL(IXL,R8),REAL(IXL,R8)+(X1-XL)*XFAC),
     &        REAL(IXR,R8))
         ZIY1=MIN(MAX(REAL(IYB,R8),REAL(IYB,R8)+(Y1-YB)*YFAC),
     &        REAL(IYT,R8))
         IF(IDOT.EQ.1) CALL DLCH(INT(ZIX1),-INT(ZIY1),' ',46,1)
         X2=X1+VX(I)*VFAC
         Y2=Y1+VY(I)*VFAC
         ZIX2=REAL(IXL,R8)+(X2-XL)*XFAC
         ZIY2=REAL(IYB,R8)+(Y2-YB)*YFAC
         IF(ZIX2.LT.REAL(IXL,R8).OR.ZIX2.GT.REAL(IXR,R8)
     >	   .OR.ZIY2.LT.REAL(IYB,R8).OR.ZIY2.GT.REAL(IYT,R8)) THEN
C           * IF VECTOR WOULD CROSS THE BOUNDARY, SUPPRESS ARROWHEAD
C           * AND PART OF THE VECTOR OUTSIDE THE DOMAIN.
            ZIXX=MIN(MAX(REAL(IXL,R8),ZIX2),REAL(IXR,R8))
            ZIYY=MIN(MAX(REAL(IYB,R8),ZIY2),REAL(IYT,R8))
            ZIYYS=ZIYY
            IF(ZIXX.NE.ZIX2) ZIYY=ZIY1+
     >                      (ZIY2-ZIY1)*(ZIXX-ZIX1)/(ZIX2-ZIX1)
            IF(ZIYYS.NE.ZIY2.AND.(ZIYY.LE.REAL(IYB,R8)
     >        .OR.ZIYY.GE.REAL(IYT,R8))) THEN
               ZIXX=ZIX1+(ZIX2-ZIX1)*(ZIYYS-ZIY1)/(ZIY2-ZIY1)
               ZIYY=ZIYYS
            ENDIF
            CALL DRV(ZIX1,ZIY1,ZIXX,ZIYY)
         ELSE
            IF(JVEC.EQ.1) CALL ARROW1(ZIX1,ZIY1,ZIX2,ZIY2,L)
            IF(JVEC.EQ.2) CALL ARROW2(ZIX1,ZIY1,ZIX2,ZIY2,L)
         ENDIF
   10 CONTINUE
      RETURN
      END
C
      SUBROUTINE ARROW1(ZIX1,ZIY1,ZIX2,ZIY2,L)
C***********************************************************************
C     SUBROUTINE ARROW1 DRAWS AN ARROW FROM (IX1,IY1) TO (IX2,IY2).    *
C THE HEIGHT AND THE WIDTH OF THE ARROWHEAD ARE FIXED RELATIVE TO THE  *
C LENGTH OF THE ARROW BY THE VARIABLES H AND W, WHERE H = L/100 (I.E., *
C THE ARGUMENT L PROVIDES THE LENGTH OF THE ARROWHEAD AS AN INTEGER    *
C PERCENTAGE OF THE VECTOR LENGTH R) AND W = WR*H (I.E., THE PARAMETER *
C WR PROVIDES THE WIDTH RELATIVE TO THE HEIGHT OF THE ARROWHEAD).      *
C     THROUGH ENTRY ARROW2 ARROWS WITH A CONSTANT LENGTH OF THE ARROW- *
C HEAD ARE DRAWN.  THIS LENGTH IS FIXED BY THE ARGUMENT L = H*R (I.E., *
C L PROVIDES THE ABSOLUTE LENGTH OF THE ARROWHEAD IN TERMS OF PLOTTING *
C COORDINATES).                                                        *
C                                                                      *
C     WRITTEN HGO 29/08/85                                             *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) wr
      PARAMETER (WR=.35)
      real (r8) ZIX1,ZIY1,ZIX2,ZIY2
      integer L
      real (r8) h,w,zihx,zihy,ziwx,ziwy,zix3,ziy3,zix4,ziy4,r
C
      H=REAL(L,R8)/100._R8
   10 CALL DRV(ZIX1,ZIY1,ZIX2,ZIY2)
      IF(L.EQ.0) RETURN
      W=WR*H
      ZIHX=H*(ZIX2-ZIX1)
      ZIHY=H*(ZIY2-ZIY1)
      ZIWX=W*(ZIX2-ZIX1)
      ZIWY=W*(ZIY2-ZIY1)
      ZIX3=ZIX2-ZIHX+ZIWY
      ZIY3=ZIY2-ZIWX-ZIHY
      ZIX4=ZIX2-ZIHX-ZIWY
      ZIY4=ZIY2+ZIWX-ZIHY
      CALL DRWABS(ZIX3,ZIY3)
      CALL DRWABS(ZIX4,ZIY4)
      CALL DRWABS(ZIX2,ZIY2)
      RETURN
C
C     * ENTRY FOR DRAWING CONSTANT-SIZE ARROWHEADS.
      ENTRY ARROW2(ZIX1,ZIY1,ZIX2,ZIY2,L)
      R=SQRT(REAL((ZIX2-ZIX1)**2+(ZIY2-ZIY1)**2,R8))
      IF(R.LT.REAL(L,R8)) THEN
         CALL DLCH(INT(ZIX1),-INT(ZIY1),' ',46,1)
         RETURN
      ENDIF
      H=REAL(L,R8)/R
      GOTO 10
C
      END
C
      SUBROUTINE SPLOT(MX,MY,IS,IOP,YX,ZXY,NX,NY,INCYX,Z,NDIM,IJARR,NS,
     A                 TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)
C
C***********************************************************************
C     SPLOT PLOTS THE ONE-DIMENSIONAL CROSS-SECTIONS OF THE TWO-DIMEN- *
C SIONAL FUNCTION Z(X(I),Y(J)) IN THE X- OR Y-DIRECTION, DEPENDING ON  *
C THE VALUE OF IS.  THE VALUES OF THE X- OR Y-INDICES MAY BE SPECIFIED *
C IN THE ARRAY IJARR(ISEC), WHERE ISEC=1,NS.                           *
C                                                                      *
C     ARGUMENTS:                                                       *
C                                                                      *
C MX/MY  - SEE LPLOT.                                                  *
C IS     - DETERMINES WHETHER X- OR Y-SECTIONS ARE PLOTTED.            *
C          IS = 1 : X-SECTION ZX PLOTTED AS A FUNCTION OF Y.           *
C          IS = 2 : Y-SECTION ZY PLOTTED AS A FUNCTION OF X.           *
C          HENCE, FOR EXPRESSIONS XY READ: X FOR IS=1, Y FOR IS=2,     *
C          AND VICE VERSA FOR YX.                                      *
C IOP    - SEE LPLOT.                                                  *
C          IF IOP > 10 (I.E., CHARACTERS ARE PLACED ON THE CURVES),    *
C          THE CHARACTER NUMBER IC IS AUTOMATICALLY INCREMENTED FOR    *
C          THE NS DIFFERENT CURVES SPECIFIED.  E.G., IF IOP=30971, A   *
C          LOWER CASE 'A' IS PLACED AT EVERY 3RD POINT ON THE FIRST    *
C          CURVE, A LOWER CASE 'B' ON THE SECOND CURVE, ETC.           *
C YX     - TABLE OF THE ABSCISSA VALUES FOR THE PLOTS.                 *
C ZXY    - AN ARRAY THAT HOLDS THE X/Y-SECTIONS OF THE FUNCTION Z.     *
C          IT SHOULD BE DIMENSIONED NY/NX IN THE CALLING PROGRAM.      *
C NX     - THE NUMBER OF POINTS IN THE X-DIRECTION, I.E. THE NUMBER OF *
C          POINTS IN A Y-SECTION PLOT.                                 *
C          NX < 0 : ONLY THE FRAME AND SCALES FOR THE PLOT ARE DRAWN.  *
C NY     - THE NUMBER OF POINTS IN THE Y-DIRECTION, I.E. THE NUMBER OF *
C          POINTS IN A X-SECTION PLOT.                                 *
C          NY < 0 : SECTIONS ARE DRAWN ON A FRAME PREVIOUSLY CREATED   *
C          BY A CALL TO SPLOT WITH NX < 0 OR A DIRECT CALL OF NFRAME   *
C          (IN ORDER TO SPECIFY A FRAME SIZE DIFFERENT FROM THE ONE    *
C          IMPLIED BY THE RANGES OF YX AND ZXY).                       *
C INCYX  - IABS(INCYX) IS THE SKIP PARAMATER FOR YX.                   *
C          INCYX < 0 : YXMIN = YX(1) AND DYX = YX(2).                  *
C Z      - THE TWO-DIMENSIONAL TABLE OF VALUES DIMENSIONED AT LEAST AS *
C          NX BY NY IN THE CALLING PROGRAM.                            *
C NDIM   - THE FIRST DIMENSION OF THE ARRAY Z.  HENCE: NX <= NDIM.     *
C IJARR  - CONTAINS THE X/Y INDICES AT WHICH TO TAKE SECTIONS AND      *
C          DIMENSIONED BY NS.                                          *
C NS     - THE NUMBER OF ROWS OR COLUMNS AT WHICH TO TAKE SECTIONS.    *
C          NS < 0 : SPLOT AUTOMATICALLY FILLS IJARR WITH NS VALUES.    *
C          NS > 0 : IJARR IS SUPPLIED BY THE USER.                     *
C TITLE                - TITLE FOR THE GRAPH.                          *
C XNAME/YNAME          - LABEL FOR THE X/Y-AXIS.                       *
C NTITLE/NXNAME/NYNAME - NUMBER OF CHARACTERS IN TITLE/XNAME/YNAME.    *
C                                                                      *
C     WRITTEN BY BRENDAN GODFREY                                       *
C     ADDED ARGUMENTS IS AND NDIM, HGO 23/12/85.                       *
C     CORRECTED ERROR IN CALCULATION IJARR, HGO 2/8/91                 *
C***********************************************************************
C
      use itm_types
      implicit none
      integer MX,MY,IS,IOP,NX,NY,INCYX,NDIM,NS,
     A     NTITLE,NXNAME,NYNAME
      real (r8) YX(*),ZXY(*),Z(NDIM,*)
      integer IJARR(*)
      CHARACTER*(*) TITLE,XNAME,YNAME
      real (r8) zxymin,zxymax,yxmin,yxmax,dxy
      integer nnx,nny,nnxy,nnyx,idum,jdum,nos,isec,ji,iop1
C
      NNX=IABS(NX)
      NNY=IABS(NY)
      IF(IS.EQ.1) THEN
         NNXY=NNX
         NNYX=NNY
      ELSE
         NNXY=NNY
         NNYX=NNX
      ENDIF
C
C     * DRAW FRAME AND SCALES.
      IF(NY.GT.0) THEN
C        * DETERMINE THE RANGE OF ALL POSSIBLE ZXY'S.
         CALL MINM(Z,NDIM,NNX,NNY,1,1,ZXYMIN,IDUM,JDUM)
         CALL MAXM(Z,NDIM,NNX,NNY,1,1,ZXYMAX,IDUM,JDUM)
C        * DETERMINE THE RANGE OF YX.
         YXMIN=YX(1)
         YXMAX=YX(1)+YX(2)*(NNYX-1)/IABS(INCYX)
         IF(INCYX.GT.0) YXMAX=YX(NNYX)
         CALL NFRAME(MX,MY,IABS(IOP),YXMIN,YXMAX,ZXYMIN,ZXYMAX,
     A               TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)
C        * IF NX < 0, DRAW FRAME AND SCALES ONLY.
         IF(NX.LT.0) RETURN
      ELSE
C        * DRAW SECTIONS ONTO PREVIOUSLY DRAWN FRAME.
         CALL OFRAME(MX,MY)
      ENDIF
C
C     * DETERMINE THE NUMBER AND INDICES OF THE SECTIONS.
      NOS=IABS(NS)
      IF(NS.LT.0) THEN
C        * STORE UNIFORMLY SPACED INDICES ALONG XY IN THE ARRAY IJARR.
         DXY=REAL(NNXY,R8)/REAL(NOS,R8)
         DO 10 ISEC=1,NOS
   10       IJARR(ISEC)=(ISEC-0.5_R8)*DXY+0.5_R8
      ENDIF
C
C     * FILL THE ARRAY ZXY AND PASS IT ONTO LPLOT.
      DO 30 ISEC=1,NOS
         DO 20 JI=1,NNYX
            IF(IS.EQ.1) ZXY(JI)=Z(IJARR(ISEC),JI)
            IF(IS.EQ.2) ZXY(JI)=Z(JI,IJARR(ISEC))
   20    CONTINUE
         IOP1=IOP
         IF(IOP/10.NE.0) IOP1=IOP+(ISEC-1)*10
         CALL LPLOT(MX,MY,IOP1,YX,ZXY,-NNYX,INCYX,
     A              TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)
   30 CONTINUE
C
      RETURN
      END
C
      SUBROUTINE APLOT(MX,MY,IA,YX,AVXY,NX,NY,INCYX,Z,NDIM,IJ1,IJ2,
     A                 TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)
C
C***********************************************************************
C     APLOT AVERAGES OVER THE TWO-DIMENSIONAL FUNCTION Z(X(I),Y(J)) IN *
C ONE DIRECTION (X OR Y, DEPENDING ON THE VALUE OF IA) AND PLOTS THE   *
C RESULT WITH RESPECT TO THE OTHER DIRECTION.  AVERAGING MAY BE LIMI-  *
C TED TO A SPECIFIED BAND OF INDICES IJ1-IJ2 IN THE X- OR Y-DIRECTION. *
C THIS SUBROUTINE ONLY PERFORMS THE AVERAGING CALCULATION; IT CALLS    *
C LPLOT TO DRAW THE RESULTING CURVE.                                   *
C                                                                      *
C     ARGUMENTS:                                                       *
C                                                                      *
C MX/MY  - SEE LPLOT.                                                  *
C IA     - DETERMINES WHETHER X- OR Y-AVERAGES ARE PLOTTED.            *
C          IA = 1 : X-AVERAGE AVX PLOTTED AS A FUNCTION OF Y.          *
C          IA = 2 : Y-AVERAGE AVY PLOTTED AS A FUNCTION OF X.          *
C          HENCE, FOR EXPRESSIONS XY READ: X FOR IA=1, Y FOR IA=2,     *
C          AND VICE VERSA FOR YX.                                      *
C YX     - TABLE OF THE ABSCISSA VALUES FOR THE PLOT.                  *
C AVXY   - AN ARRAY THAT HOLDS THE X/Y-AVERAGES OF THE FUNCTION Z.     *
C          IT SHOULD BE DIMENSIONED NY/NX IN THE CALLING PROGRAM.      *
C NX     - THE NUMBER OF POINTS IN THE X-DIRECTION, I.E. THE NUMBER OF *
C          POINTS IN A Y-AVERAGE PLOT.                                 *
C          NX < 0 : ONLY THE FRAME AND SCALES FOR THE PLOT ARE DRAWN.  *
C NY     - THE NUMBER OF POINTS IN THE Y-DIRECTION, I.E. THE NUMBER OF *
C          POINTS IN A X-AVERAGE PLOT.                                 *
C          NY < 0 : AVERAGES ARE DRAWN ON A FRAME PREVIOUSLY CREATED   *
C          BY A CALL TO APLOT WITH NX < 0 OR A DIRECT CALL OF NFRAME   *
C          (IN ORDER TO SPECIFY A FRAME SIZE DIFFERENT FROM THE ONE    *
C          IMPLIED BY THE RANGES OF YX AND AVXY).                      *
C INCYX  - IABS(INCYX) IS THE SKIP PARAMATER FOR YX.                   *
C          INCYX < 0 : YXMIN = YX(1) AND DYX = YX(2).                  *
C Z      - THE TWO-DIMENSIONAL TABLE OF VALUES DIMENSIONED AT LEAST AS *
C          NX BY NY IN THE CALLING PROGRAM.                            *
C NDIM   - THE FIRST DIMENSION OF THE ARRAY Z.  HENCE: NX <= NDIM.     *
C IJ1    - THE INDEX OF THE FIRST CELL IN THE AVERAGE CALCULATION.     *
C          IJ1 <= 0 : THE VALUE 1 FOR AVERAGING OVER THE WHOLE RANGE   *
C          IS TAKEN.                                                   *
C IJ2    - THE INDEX OF THE LAST CELL OF THE BAND TO AVERAGE OVER.     *
C          IJ2 <= 0 : THE VALUE NX/NY FOR AVERAGING OVER THE WHOLE     *
C          RANGE IS TAKEN.                                             *
C TITLE                - TITLE FOR THE GRAPH.                          *
C XNAME/YNAME          - LABEL FOR THE X/Y-AXIS.                       *
C NTITLE/NXNAME/NYNAME - NUMBER OF CHARACTERS IN TITLE/XNAME/YNAME.    *
C                                                                      *
C     WRITTEN BY DENNIS HEWETT                                         *
C     MODIFIED BY DEBBY HYMAN 5-80, FOR -NX OR -NY TO TRIGGER SEPARATE *
C     DRAWING OF THE FRAME AND THE CURVE.                              *
C     ADDED ARGUMENTS IA AND NDIM, SEPARATED X- AND Y-AVERAGE PLOTS,   *
C     IMPROVED HANDLING OF THE SKIP PARAMETER, HGO 23/12/85.           *
C***********************************************************************
C
      use itm_types
      implicit none
      integer MX,MY,IA,NX,NY,INCYX,NDIM,IJ1,IJ2,
     A     NTITLE,NXNAME,NYNAME
      real (r8) YX(*),AVXY(*),Z(NDIM,*)
      CHARACTER*(*) TITLE,XNAME,YNAME
      integer nnx,nny,nnxy,nnyx,ij11,ij22,ji,ij
C
      NNX=IABS(NX)
      NNY=IABS(NY)
      IF(IA.EQ.1) THEN
         NNXY=NNX
         NNYX=NNY
      ELSE
         NNXY=NNY
         NNYX=NNX
      ENDIF
C
C     * INSTALL BAND OF INDICES TO AVERAGE OVER, IF DESIRED.
      IJ11=1
      IF(IJ1.GT.0) IJ11=IJ1
      IJ22=NNXY
      IF(IJ2.GT.0) IJ22=IJ2
C
C     * COMPUTE THE AVERAGE AVXY.
      DO 20 JI=1,NNYX
         AVXY(JI)=0._R8
         DO 10 IJ=IJ11,IJ22
            IF(IA.EQ.1) AVXY(JI)=AVXY(JI)+Z(IJ,JI)
            IF(IA.EQ.2) AVXY(JI)=AVXY(JI)+Z(JI,IJ)
   10    CONTINUE
   20    AVXY(JI)=AVXY(JI)/REAL(IJ22-IJ11+1,R8)
C
C     * PLOT AVXY AS A FUNCTION OF YX.
      CALL LPLOT(MX,MY,ISIGN(1,NX),YX,AVXY,ISIGN(NNYX,NY),INCYX,
     A           TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)
      RETURN
      END
C
      SUBROUTINE TPLOT(MX,MY,IVERT,NX,NY,INCX,INCY,Z,NDIM,
     A                 TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)
C
C***********************************************************************
C     THIS ROUTINE PLOTS A FUNCTION OF TWO VARIABLES ON A RECTANGULAR  *
C GRID AS A SET OF VERTICAL LINES RISING FROM THE GRID POINTS.  THE    *
C HEIGHT OF THE LINES IS RELATED TO THE AMPLITUDE OF THE FUNCTION:     *
C          FMZ(I,J)/ZMAX                   FOR IVERT=1 (LINEAR),       *
C          GM(1+HM*ALOG10(Z(I,J)/ZMAX2))   FOR IVERT=2 (QUASI-LOG),    *
C WHERE FM,GM,HM ARE FIXED IN THE PARAMETER STATEMENT BELOW.  THE      *
C PARAMETERS DM,DN,EM DETERMINE THE LENGTH AND ORIENTATION OF THE X-   *
C AND Y-AXES.  TPLOT MAY BE USED, E.G., TO PLOT FOURIER COMPONENTS     *
C WITH A VERTICAL LOG SCALE.  IT PRODUCES A CRUDE 3-D PLOT.  ONLY      *
C POSITIVE VALUES OF Z ARE PLOTTED.                                    *
C                                                                      *
C     ARGUMENTS:                                                       *
C                                                                      *
C MX/MY  - SEE LPLOT.                                                  *
C IVERT  - DETERMINES WHETHER THE VERTICAL SCALE OF THE PLOT IS LINEAR *
C          OR QUASI-LOGARITHMIC.                                       *
C NX     - NUMBER OF POINTS IN THE X-DIRECTION.                        *
C NY     - NUMBER OF POINTS IN THE Y-DIRECTION.                        *
C INCX   - SKIP PARAMETER FOR X.                                       *
C INCY   - SKIP PARAMETER FOR Y.                                       *
C Z      - 2-D ARRAY OF VERTICAL HEIGHTS.                              *
C NDIM   - THE FIRST DIMENSION OF THE ARRAY Z.  HENCE: NX <= NDIM.     *
C TITLE                - TITLE FOR THE GRAPH.                          *
C XNAME/YNAME          - LABEL FOR THE X/Y-AXIS (REFERS TO INDICES!).  *
C NTITLE/NXNAME/NYNAME - NUMBER OF CHARACTERS IN TITLE/XNAME/YNAME.    *
C                                                                      *
C     WRITTEN BY DAVE FORSLUND                                         *
C     ADDED ARGUMENTS IVERT AND NDIM, INTRODUCED PARAMETERS DN AND FM, *
C     IMPROVED SCALES, HGO 24/12/85.                                   *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) dm, dn, em, fm, gm, hm
      PARAMETER (DM=.57,DN=.91,EM=.82,FM=2._R8,GM=.1,HM=.33)
C
      integer MX,MY,IVERT,NX,NY,INCX,INCY,NDIM,
     A     NTITLE,NXNAME,NYNAME
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      real (r8) Z(NDIM,*)
      CHARACTER*(*) TITLE,XNAME,YNAME
      CHARACTER*9 STRING
      real (r8) zmax,zmax2,dmax,emax,a,c,gme,e0,d0,d,e,alog19
      integer j,i,i0,j0,i1,i2,j1,nc,id,ie0,ie
C
      CALL NFRAME(MX,MY,5,0._R8,1._R8,0._R8,1._R8,
     &     TITLE,NTITLE,' ',1,' ',1)
C
C     * DETERMINE THE MAXIMUM OF Z (ZMAX2 IS THE NEXT LARGEST VALUE).
      ZMAX=0._R8
      ZMAX2=0._R8
      DO 10 J=1,NY,INCY
         DO 10 I=1,NX,INCX
            ZMAX2=MAX(ZMAX2,Z(I,J))
            IF(ZMAX2.GT.ZMAX) THEN
               ZMAX2=ZMAX
               ZMAX=Z(I,J)
            ENDIF
   10 CONTINUE
C
      CALL DLCH(IXL,IYT-20,'ZMAX =',6,2)
      WRITE(STRING,'(1PE9.2)') ZMAX
      CALL DLCH(IXL,IYT-40,STRING,9,2)
      WRITE(STRING,'(1PE9.2)') ZMAX2
      CALL DLCH(IXL,IYT-60,STRING,9,2)
C
C     * SCALE FACTORS.
      DMAX=(NX-1._R8)/DM
      EMAX=(NY-1._R8)/EM
      A=DMAX*(DN-DM)/(EMAX*EM)
C
C     * DRAW X- AND Y-AXIS.
      CALL CONVRT(0._R8,I0,0._R8,DMAX,IXL,IXR)
      CALL CONVRT(0._R8,J0,0._R8,EMAX,IYB,IYT)
      CALL CONVRT(NX-1._R8,I1,0._R8,DMAX,IXL,IXR)
      CALL CONVRT(NX-1._R8+A*(NY-1._R8),I2,0._R8,DMAX,IXL,IXR)
      CALL CONVRT(NY-1._R8,J1,0._R8,EMAX,IYB,IYT)
      CALL DRV(REAL(I0,R8),REAL(J0,R8),REAL(I1,R8),REAL(J0,R8))
      CALL DRV(REAL(I1,R8),REAL(J0,R8),REAL(I2,R8),REAL(J1,R8))
C
C     * SCALE AND LABEL X-AXIS.
      CALL DLCH(I0-6,J0-22,'1',1,2)
      IF(NX.LT.10) THEN
         NC=1
         WRITE(STRING,'(I1)') NX
      ELSEIF(NX.LT.100) THEN
         NC=2
         WRITE(STRING,'(I2)') NX
      ELSEIF(NX.LT.1000) THEN
         NC=3
         WRITE(STRING,'(I3)') NX
      ENDIF
      CALL DLCH(I1-NC*6,J0-22,STRING,NC,2)
      CALL DLCH((I0+I1)/2-NXNAME*6,J0-43,XNAME,NXNAME,2)
C
C     * SCALE AND LABEL Y-AXIS.
      CALL DRV(REAL(I1,R8),REAL(J0,R8),REAL(I1+15,R8),REAL(J0,R8))
      CALL DRV(REAL(I2,R8),REAL(J1,R8),REAL(I2+15,R8),REAL(J1,R8))
      CALL DLCH(I1+20,J0-8,'1',1,2)
      IF(NY.LT.10) THEN
         NC=1
         WRITE(STRING,'(I1)') NY
      ELSEIF(NY.LT.100) THEN
         NC=2
         WRITE(STRING,'(I2)') NY
      ELSEIF(NY.LT.1000) THEN
         NC=3
         WRITE(STRING,'(I3)') NY
      ENDIF
      CALL DLCH(I2+20,J1-8,STRING,NC,2)
      CALL DLCH((I1+I2)/2+50,(J0+J1)/2-8,YNAME,NYNAME,2)
C
C     * PLOT Z(I,J).
      C=0._R8
      IF(IVERT.EQ.1) THEN
         IF(ZMAX.GT.0._R8) C=FM/ZMAX
      ELSEIF(IVERT.EQ.2) THEN
         IF(ZMAX2.GT.0._R8) C=1._R8-HM*LOG10(ZMAX2)
         GME=GM*EMAX
      ENDIF
      DO 20 J=1,NY,INCY
         E0=REAL(J-1,R8)
         D0=A*E0
         DO 20 I=1,NX,INCX
            D=D0+REAL(I-1,R8)
            E=E0
            IF(IVERT.EQ.1) THEN
               IF(Z(I,J).GT.0._R8) E=E+MAX(0.0_R8,C*Z(I,J))
            ELSEIF(IVERT.EQ.2) THEN
               IF(Z(I,J).GT.0._R8) 
     &              E=E+MAX(0.0_R8,GME*(C+HM*ALOG19(Z(I,J))))
            ENDIF
            CALL CONVRT(D,ID,0.0_R8,DMAX,IXL,IXR)
            CALL CONVRT(E0,IE0,0.0_R8,EMAX,IYB,IYT)
            CALL CONVRT(E,IE,0.0_R8,EMAX,IYB,IYT)
            IF(IABS(IE-IE0).GT.1) THEN
               CALL DRV(REAL(ID,R8),REAL(IE,R8),
     &              REAL(ID,R8),REAL(IE0,R8))
            ELSE
               CALL DLCH(ID,-IE0,' ',46,2)
            ENDIF
   20 CONTINUE
C
      RETURN
      END
C
      SUBROUTINE P3PLOT(MX,MY,R,TH,NR,NTH,F,NDIM,THX,THY,TITLE,NTITLE)
C
C***********************************************************************
C     SUBROUTINE P3PLOT PRODUCES A THREE-DIMENSIONAL POLAR PLOT OF THE *
C FUNCTION F(R(I),TH(J)), I=1,..,NR, J=1,..,NTH.                       *
C                                                                      *
C     ARGUMENTS:                                                       *
C                                                                      *
C MX/MY  - SEE LPLOT.                                                  *
C R      - 1D ARRAY OF RADIAL COORDINATES.                             *
C TH     - 1D ARRAY OF ANGULAR COORDINATES (IN RADIANS).               *
C NR     - NUMBER OF POINTS IN THE RADIAL DIRECTION.                   *
C NTH    - NUMBER OF POINTS IN THE ANGULAR DIRECTION.                  *
C F      - 2D ARRAY OF FUNCTION VALUES.                                *
C NDIM   - THE FIRST DIMENSION OF THE ARRAY F.  HENCE: NR <= NDIM.     *
C THX    - ANGLE (IN DEGREES!) AT WHICH THE X-AXIS IS TO BE DRAWN.     *
C THY    - ANGLE (IN DEGREES!) BETWEEN THE X- AND Y-AXIS.              *
C TITLE  - TITLE FOR THE GRAPH.                                        *
C NTITLE - NUMBER OF CHARACTERS IN TITLE.                              *
C                                                                      *
C     WRITTEN BY R.M. FRANK, 1975.                                     *
C     ADAPTED TO PPPLIB, HGO 24/04/86.                                 *
C***********************************************************************
C
      use itm_types
      implicit none
      integer MX,MY,NR,NTH,NDIM,NTITLE
      real (r8) THX,THY
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      real (r8) R(*),TH(*),F(NDIM,*)
      CHARACTER*(*) TITLE
      integer JXRL(3),JYTB(6)
      real (r8) X(2),Y(2)
      CHARACTER*3 LAB(2,2)
      real (r8) pi,dgr,rmax,fmax,sx,cx,sxy,cxy,xmx,ymx,theta,q,t,
     &     xmn,ymn,s,ytest,xfac,yfac,ri,
     &     zix0,ziy0,zix1,ziy1,zix,ziy,ziy2,rmin
      integer irm,idum,jdum,j,i,imx,imy,mxx,myy,
     &     l,ll,rlab,k,ir0
C
      DATA JXRL/900,400,400/,JYTB/645,285,285,165,165,165/
      DATA LAB/' 0 ',' 90','180','270'/
C
      PI=3.1415926535898_R8
      DGR=PI/180._R8
C
C     * DETERMINE FRAME COORDINATES.
      CALL MAXV(R,NR,1,RMAX,IRM)
      CALL MAXAM(F,NDIM,NR,NTH,1,1,FMAX,IDUM,JDUM)
      SX=SIN(THX*DGR)
      CX=COS(THX*DGR)
      SXY=SIN((THY+THX)*DGR)
      CXY=COS((THY+THX)*DGR)
      XMX=RMAX*CX
      YMX=RMAX*SX
      DO 10 J=1,71
         THETA=J*5._R8*DGR
         Q=SIN(THETA)
         T=COS(THETA)
         XMX=MAX(XMX,RMAX*(T*CX+Q*CXY))
         YMX=MAX(YMX,RMAX*(T*SX+Q*SXY))
   10 CONTINUE
      XMX=XMX*1.125_R8
      XMN=-XMX
      YMN=-YMX
      S=ABS((YMX-YMN)/(2._R8*FMAX))
      DO 20 J=1,NTH
         Q=SIN(TH(J))*SXY+COS(TH(J))*SX
         DO 20 I=1,NR
            YTEST=R(I)*Q+S*F(I,J)
            YMX=MAX(YMX,YTEST)
            YMN=MIN(YMN,YTEST)
   20 CONTINUE
      YMX=YMX*1.125_R8
      YMN=YMN*1.125_R8
      IMX=MOD(IABS(MX),10)
      IMY=MOD(IABS(MY),10)
      IF((XMX-XMN)/(YMX-YMN).LT.REAL(JXRL(IMX),R8)/JYTB(IMY)) THEN
         MXX=20+IMX
         MYY=10+IMY
      ELSE
         MXX=10+IMX
         MYY=20+IMY
      ENDIF
      CALL NFRAME(MXX,MYY,5,XMN,XMX,YMN,YMX,TITLE,NTITLE,' ',1,' ',1)
C
      XFAC=REAL(IXR-IXL,R8)/(XR-XL)
      YFAC=REAL(IYT-IYB,R8)/(YT-YB)
      ZIX0=REAL(IXL,R8)-(XL*XFAC)
      ZIY0=REAL(IYB,R8)-(YB*YFAC)
C
C     * DRAW BOUNDARY CURVE.
      ZIX1=ZIX0+(RMAX*CX*XFAC)
      ZIY1=ZIY0+(RMAX*SX*YFAC)
      L=0
      DO 30 J=1,72
         THETA=J*5._R8*DGR
         Q=SIN(THETA)
         T=COS(THETA)
         ZIX=ZIX0+RMAX*(T*CX+Q*CXY)*XFAC
         ZIY=ZIY0+RMAX*(Q*SXY+T*SX)*YFAC
         CALL DASH(ZIX1,ZIY1,ZIX,ZIY,10,10,L,LL)
         L=LL
         ZIX1=ZIX
         ZIY1=ZIY
   30 CONTINUE
C
C     * DRAW AXES AND LABELS.
      RLAB=1._R8+60._R8*XMX/((IXR-IXL)*RMAX)
      DO 50 K=1,2
         DO 40 L=1,2
            THETA=(90._R8*(K-1)+180._R8*(L-1))*DGR
            Q=SIN(THETA)
            T=COS(THETA)
            X(L)=RMAX*(T*CX+Q*CXY)
            Y(L)=RMAX*(Q*SXY+T*SX)
   40    CONTINUE
         CALL DASH(ZIX0+(X(1)*XFAC),ZIY0+(Y(1)*YFAC),
     A             ZIX0+(X(2)*XFAC),ZIY0+(Y(2)*YFAC),10,10,0,IDUM)
         DO 50 L=1,2
            ZIX=ZIX0+X(L)*RLAB*XFAC
            ZIY=ZIY0+Y(L)*RLAB*YFAC
            CALL DLCH(INT(ZIX-12),-INT(ZIY),LAB(K,L),3,2)
   50 CONTINUE
C
C     * DRAW BOUNDARY VERTICALS.
      DO 60 J=1,NTH-1
         THETA=TH(J)
         Q=SIN(THETA)
         T=COS(THETA)
         ZIX =ZIX0+RMAX*(T*CX+Q*CXY)*XFAC
         ZIY1=ZIY0+RMAX*(Q*SXY+T*SX)*YFAC
         ZIY2=ZIY1+S*F(IRM,J)*YFAC
         IF(ABS(ZIY2-ZIY1).GT.5) CALL DASH(ZIX,ZIY1,ZIX,ZIY2,0,5,0,IDUM)
   60 CONTINUE
C
C     * DRAW CENTER LINE.
      CALL MINV(R,NR,1,RMIN,IR0)
      IF(RMIN.EQ.0._R8)
     A   CALL DASH(ZIX0,ZIY0,ZIX0,ZIY0+S*F(IR0,1)*YFAC,10,10,0,IDUM)
C
C     * DRAW ANGULAR GRID LINES.
      DO 70 I=1,NR
         RI=R(I)
         ZIX=ZIX0+RI*CX*XFAC
         ZIY=ZIY0+(RI*SX+S*F(I,1))*YFAC
         CALL MOVABS(ZIX,ZIY)
         DO 70 J=2,NTH
            THETA=TH(J)
            Q=SIN(THETA)
            T=COS(THETA)
            ZIX=ZIX0+RI*(T*CX+Q*CXY)*XFAC
            ZIY=ZIY0+(RI*(Q*SXY+T*SX)+S*F(I,J))*YFAC
            CALL DRWABS(ZIX,ZIY)
   70 CONTINUE
C
C     * DRAW RADIAL GRID LINES.
      DO 80 J=1,NTH-1
         THETA=TH(J)
         Q=SIN(THETA)
         T=COS(THETA)
         RI=R(1)
         ZIX=ZIX0+RI*(T*CX+Q*CXY)*XFAC
         ZIY=ZIY0+(RI*(Q*SXY+T*SX)+S*F(1,J))*YFAC
         CALL MOVABS(ZIX,ZIY)
            DO 80 I=2,NR
            RI=R(I)
            ZIX=ZIX0+RI*(T*CX+Q*CXY)*XFAC
            ZIY=ZIY0+(RI*(Q*SXY+T*SX)+S*F(I,J))*YFAC
            CALL DRWABS(ZIX,ZIY)
   80 CONTINUE
      RETURN
      END
C
      SUBROUTINE MAXV(A,N,INC,B,I)
C
C***********************************************************************
C     SUBROUTINE MAXV, AND ENTRIES MINV, MAXAV, MINAV DETERMINE THE    *
C MAXIMUM, MINIMUM, MAXIMUM ABSOLUTE, MINIMUM ABSOLUTE VALUES OF AN    *
C ARRAY OF REAL NUMBERS, RESPECTIVELY.  AS THE ARRAY IS SEARCHED, AN   *
C INDEX IS UPDATED EACH TIME A LARGER VALUE OF A (IN THE CASE OF MAXV) *
C IS ENCOUNTERED.  AFTER THE ARRAY IS SEARCHED, B IS SET TO THE VALUE  *
C OF A WITH THE CALCULATED INDEX.                                      *
C                                                                      *
C A   - ONE-DIMENSIONAL INPUT ARRAY OF REAL NUMBERS.                   *
C N   - NUMBER OF ELEMENTS IN THE ARRAY A.                             *
C INC - SPACING AT WHICH ELEMENTS ARE TO BE EXAMINED.                  *
C B   - MAXIMUM, MINIMUM, MAXIMUM ABSOLUTE OR MINIMUM ABSOLUTE VALUE   *
C       OF A RETURNED TO THE CALLER.                                   *
C I   - ELEMENT NUMBER OF MAXIMUM, MINIMUM, ETC. VALUE OF A (1<=I<=N). *
C                                                                      *
C     SEPARATED THE DIFFERENT ENTRIES, CHANGED THE MEANING OF N TO THE *
C     PRESENT ONE, HGO 9/1/86.                                         *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) A(*), B,s
      integer N,INC,I,k
C
      B=A(1)
      I=1
      DO 10 K=1,N,INC
         IF(A(K).GT.B) THEN
            B=A(K)
            I=K
         ENDIF
   10 CONTINUE
      RETURN
C
      ENTRY MINV(A,N,INC,B,I)
      B=A(1)
      I=1
      DO 20 K=1,N,INC
         IF(A(K).LT.B) THEN
            B=A(K)
            I=K
         ENDIF
   20 CONTINUE
      RETURN
C
      ENTRY MAXAV(A,N,INC,B,I)
      B=ABS(A(1))
      I=1
      DO 30 K=1,N,INC
         S=ABS(A(K))
         IF(S.GT.B) THEN
            B=S
            I=K
         ENDIF
   30 CONTINUE
      B=A(I)
      RETURN
C
      ENTRY MINAV(A,N,INC,B,I)
      B=ABS(A(1))
      I=1
      DO 40 K=1,N,INC
         S=ABS(A(K))
         IF(S.LT.B) THEN
            B=S
            I=K
         ENDIF
   40 CONTINUE
      B=A(I)
      RETURN
      END
C
      SUBROUTINE MAXM(A,IA,M,N,INCK,INCL,B,I,J)
C
C***********************************************************************
C     SUBROUTINE MAXM, AND ENTRIES MINM, MAXAM, MINAM DETERMINE THE    *
C MAXIMUM, MINIMUM, MAXIMUM ABSOLUTE, AND MINIMUM ABSOLUTE ELEMENT AND *
C THE INDICES OF THAT ELEMENT IN MATRIX A.                             *
C                                                                      *
C A    - TWO-DIMENSIONAL INPUT ARRAY.                                  *
C IA   - MAXIMUM LENGTH OF THE FIRST ARGUMENT OF A AS SPECIFIED IN THE *
C        DIMENSION STATEMENT, I.E. DIMENSION A(IA,JA).                 *
C M    - NUMBER OF COLUMNS (1ST ARGUMENT).                             *
C N    - NUMBER OF ROWS (2ND ARGUMENT).                                *
C INCK - SKIP PARAMETER FOR THE 1ST ARGUMENT.                          *
C INCL - SKIP PARAMETER FOR THE 2ND ARGUMENT.                          *
C B    - CONTAINS THE DESIRED ELEMENT.                                 *
C I    - FIRST INDEX TO THE RESULTANT ELEMENT.                         *
C J    - SECOND INDEX TO THE RESULTANT ELEMENT.                        *
C                                                                      *
C     SEPARATED THE DIFFERENT ENTRIES, ADDED ARGUMENTS INCK AND NCL,   *
C     HGO 9/1/86.                                                      *
C***********************************************************************
C
      use itm_types
      implicit none
      integer IA,M,N,INCK,INCL,I,J,k,l
      real (r8) A(IA,*),B,s
C
      B=A(1,1)
      I=1
      J=1
      DO 10 K=1,M,INCK
         DO 10 L=1,N,INCL
            IF(A(K,L).GT.B) THEN
               B=A(K,L)
               I=K
               J=L
            ENDIF
   10 CONTINUE
      RETURN
C
      ENTRY MINM(A,IA,M,N,INCK,INCL,B,I,J)
      B=A(1,1)
      I=1
      J=1
      DO 20 K=1,M,INCK
         DO 20 L=1,N,INCL
            IF(A(K,L).LT.B) THEN
               B=A(K,L)
               I=K
               J=L
            ENDIF
   20 CONTINUE
      RETURN
C
      ENTRY MAXAM(A,IA,M,N,INCK,INCL,B,I,J)
      B=ABS(A(1,1))
      I=1
      J=1
      DO 30 K=1,M,INCK
         DO 30 L=1,N,INCL
            S=ABS(A(K,L))
            IF(S.GT.B) THEN
               B=S
               I=K
               J=L
            ENDIF
   30 CONTINUE
      B=A(I,J)
      RETURN
C
      ENTRY MINAM(A,IA,M,N,INCK,INCL,B,I,J)
      B=ABS(A(1,1))
      I=1
      J=1
      DO 40 K=1,M,INCK
         DO 40 L=1,N,INCL
            S=ABS(A(K,L))
            IF(S.LT.B) THEN
               B=S
               I=K
               J=L
            ENDIF
   40 CONTINUE
      B=A(I,J)
      RETURN
      END
C
      FUNCTION ALOG19(ARG)
C
C***********************************************************************
C     SPECIAL ALOG10 TO PREVENT ERROR ON ZERO OR NEGATIVE ARGUMENT.    *
C     BOB MALONE, 12/08/78                                             *
C***********************************************************************
C
      use itm_types
      implicit none
      real (R8) arg, alog19
      IF(ARG.LT.1.E-50_R8) THEN
         ALOG19=-50._R8
      ELSE
         ALOG19=LOG10(ARG)
      ENDIF
      RETURN
      END
C
      BLOCK DATA LHEAD
C
C***********************************************************************
C     PROPERLY INITIALIZING THE TOP AND BOTTOM LABELS FOR NFRAME.      *
C                                                                      *
C     WRITTEN, HGO 2/8/91                                              *
C***********************************************************************
C
      COMMON /LHEAD1/LABTOP,LABBOT,D,T
      CHARACTER LABTOP*80,LABBOT*40,D*10,T*8
      COMMON /LHEAD2/NCT,NCB
C
      DATA LABTOP,LABBOT,D,T/' ',' ',' ',' '/
      DATA NCT,NCB/1,1/
C
      END
C
      SUBROUTINE LBLTOP(LABEL,NLABEL)
C
C***********************************************************************
C     SUBROUTINE LBLTOP ENABLES A USER TO SPECIFY AN 80-CHARACTER      *
C LABEL AT THE TOP OF A PAGE OF PLOTS.  ENTRY LBLBOT ENABLES A USER TO *
C SPECIFY A 40-CHARACTER LABEL AT THE BOTTOM OF A PLOTTING PAGE AND TO *
C WRITE DATE AND TIME IN THE LEFT CORNER OF THE PAGE.  THE LABELS ARE  *
C CENTERED WITH RESPECT TO THE WIDTH OF THE PAGE.  LBLTOP AND LBLBOT   *
C SHOULD BE CALLED BEFORE ANY OTHER PLOT CALLS FOR A PAGE.  ONCE A     *
C LABEL CALL IS GIVEN, IT REMAINS IN EFFECT FOR EACH SUCCEEDING PAGE   *
C UNTIL ANOTHER CALL WITH A DIFFERENT CHARACTER STRING IS GIVEN.       *
C***********************************************************************
C
      use itm_types
      implicit none
      integer nlabel
      COMMON /LHEAD1/LABTOP,LABBOT,D,T
      CHARACTER LABTOP*80,LABBOT*40,D*10,T*8
      COMMON /LHEAD2/NCT,NCB
      CHARACTER*(*) LABEL
      integer nct,ncb
C
      NCT=ISIGN(MIN(IABS(NLABEL),80),NLABEL)
      LABTOP=LABEL
      RETURN
C
      ENTRY LBLBOT(LABEL,NLABEL)
      NCB=ISIGN(MIN(IABS(NLABEL),40),NLABEL)
      LABBOT=LABEL
      CALL DATI(D,T)
      RETURN
      END
C
      BLOCK DATA POS
C
C***********************************************************************
C     INITIALIZING PLOT POSITION VECTOR KP(M). THIS VECTOR IS UPDATED  *
C IN THE SUBROUTINES NFRAME AND WRTEXT. OCCUPIED POSITIONS RESULT IN   *
C FRAME ADVANCE.                                                       *
C                                                                      *
C     WRITTEN, HGO 3/12/91                                             *
C***********************************************************************
C
      use itm_types
      implicit none
      integer kp,m
      COMMON /KPOS/KP(36)
C
C     * PLOT POSITIONS:
C M:       1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
C MX,MY:  11 12 13 14 15 16 21 22 23 24 25 26 31 32 33 34 35 36
      DATA (KP(M),M=1,36)
     >   / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     >     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /
C
      END
C
      SUBROUTINE NFRAME(MX,MY,IOP,XMIN,XMAX,YMIN,YMAX,
     A                  TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)
C
C***********************************************************************
C     NFRAME IS THE INTERFACE DRIVER FOR THE HIGH-LEVEL ROUTINES, SUCH *
C AS LPLOT, AND THE LOW-LEVEL ROUTINES, SUCH AS SBLIN, WHICH SCALES    *
C THE BOTTOM BOUNDARY OF THE PLOT LINEARLY.  NFRAME DEFINES THE GRAPH  *
C AREA AND THE SCALING ALONG X AND Y FROM THE MX AND MY VALUES PASSED  *
C TO IT.  IT HAS THE GRID, SPECIFIED IN THE IOP VALUE, DRAWN AND THE   *
C AXES SCALED ACCORDING TO THE MINIMUM AND MAXIMUM VALUES OF THE X AND *
C Y ARRAYS AND THE GIVEN IOP.  IT HANDLES PLACEMENT OF THE TITLES OF   *
C THE PLOT AND THE AXES.  IT ALSO PLACES THE TOP AND BOTTOM LABELS ON  *
C THE PAGE.  IT STORES THE SCALING INFORMATION IN COMMON BLOCK CJE07   *
C FOR FUTURE CALLS TO THE SAME (IMX,IMY) FRAME ON THE PAGE.  NFRAME    *
C AUTOMATICALLY ADVANCES A PAGE WITH THE FIRST PLOT THAT EXTENDS INTO  *
C THE UPPER LEFT-HAND CORNER.                                          *
C                                                                      *
C     ARGUMENTS:                                                       *
C                                                                      *
C MX     - DEFINES THE GRAPH AREA AND THE SCALING IN THE X-DIRECTION   *
C          ACCORDING TO THE FORMULA                                    *
C             IABS(MX) = IIX*1000 + IAX*100 + ISX*10 + IMX ,           *
C          WHERE IMX DETERMINES THE HORIZONTAL EXTENSION OF THE PLOT:  *
C             IMX = 1 - FULL PAGE                                      *
C                   2 - LEFT HALF OF THE PAGE                          *
C                   3 - RIGHT HALF OF THE PAGE,                        *
C          AND ISX DETERMINES THE SCALING ALONG THE X-AXIS:            *
C             ISX = 0 - AUTOMATIC SCALING WITH EXPANSION (DEFAULT)     *
C                   1 - EXACT SCALING (NO ROUNDING)                    *
C                   2 - EQUIDISTANT SCALING WITH THE X-SCALE ADAPTED   *
C                       TO THE LENGTHS ALONG Y (SEE NOTE IN NFRAME),   *
C          AND IAX PROVIDES AN ADDITIONAL OPTION:                      *
C             IAX = 0 - NO ACTION (DEFAULT)                            *
C                   1 - X=0 AXIS IS DRAWN  (IF IT LIES IN THE RANGE)   *
C                   2 - X=0 AXIS IS DASHED (IF IT LIES IN THE RANGE),  *
C          AND IIX OVERRULES THE DEFAULT NUMBER OF SCALE INTERVALS:    *
C             IIX = 0 - 4 INTERVALS FOR SCALES AND TICKMARKS (DEFAULT) *
C             IIX > 0 - IIX INTERVALS (NOT FOR AUTOMATIC SCALING).     *
C          MX < 0 : PLOTTING OF SCALES AND TICK MARKS SUPPRESSED.      *
C MY     - DEFINES THE GRAPH AREA AND THE SCALING IN THE Y-DIRECTION,  *
C          ANALOGOUS TO THE ABOVE EXPRESSIONS WITH X REPLACED BY Y,    *
C          WHERE IMY DETERMINES THE VERTICAL EXTENSION OF THE PLOT:    *
C             IMY = 1 - FULL PAGE                                      *
C                   2 - TOP HALF OF THE PAGE                           *
C                   3 - BOTTOM HALF OF THE PAGE                        *
C                   4 - TOP THIRD OF THE PAGE                          *
C                   5 - MIDDLE THIRD OF THE PAGE                       *
C                   6 - BOTTOM THIRD OF THE PAGE.                      *
C IOP    - EQUALS THE SCALING OPTION JOP OF SUBROUTINE LPLOT:          *
C             JOP = 1 - LINEAR X-AXIS, LINEAR Y-AXIS                   *
C                   2 - LINEAR X-AXIS, LOG Y-AXIS                      *
C                   3 - LOG X-AXIS, LINEAR Y-AXIS                      *
C                   4 - LOG X-AXIS, LOG Y-AXIS                         *
C                   5 - LINEAR X-AXIS, LINEAR Y-AXIS (BUT PLOTTING OF  *
C                       FRAME, SCALES, AND TICK MARKS SUPPRESSED).     *
C XMIN   - MINIMUM VALUE OF X.                                         *
C XMAX   - MAXIMUM VALUE OF X.                                         *
C YMIN   - MINIMUM VALUE OF Y.                                         *
C YMAX   - MAXIMUM VALUE OF Y.                                         *
C          THESE FOUR EXTREME VALUES ARE EITHER PRESCRIBED BY THE USER *
C          THROUGH A DIRECT CALL OF NFRAME (FOLLOWED BY CALLS TO LPLOT *
C          WITH NPTS < 0) OR DETERMINED AUTOMATICALLY BY LPLOT ITSELF. *
C TITLE  - TITLE FOR THE GRAPH.                                        *
C NTITLE - THE NUMBER OF CHARACTERS IN NTITLE.                         *
C XNAME  - LABEL FOR THE X-AXIS.                                       *
C NXNAME - NUMBER OF CHARACTERS IN XNAME.                              *
C YNAME  - LABEL FOR THE Y-AXIS.                                       *
C NYNAME - NUMBER OF CHARACTERS IN YNAME.                              *
C          THE ABOVE THREE CHARACTER STRINGS ARE AUTOMATICALLY TRUN-   *
C          CATED TO FIT ALONGSIDE THE CHOSEN FRAME.  THE FONT MAY BE   *
C          CHANGED ACCORDING TO THE RULES GIVEN IN DLCH.               *
C                                                                      *
C     NOTE:                                                            *
C THE SCALING ISX/Y=2 IS USED TO PRESERVE THE RELATIVE PROPORTIONS OF  *
C GEOMETRIC FIGURES.  E.G., A STANDING ELLIPSE X=COS(T), Y=1.5*SIN(T)  *
C IS PLOTTED ON THE LEFT HALF OF THE PAGE WITH THESE CALLS:            *
C     "CALL NFRAME(22,11,1,-1.,1.,-2.,2.,'ELLIPSE',7,'X',1,'Y',1)",    *
C     "CALL LPLOT(2,1,1,X,Y,-NPTS,1,' ',0,' ',0,' ',0)".               *
C SINCE THE RANGE OF X IS DETERMINED AUTOMATICALLY IN THIS CASE, THE   *
C PARAMETERS XMIN=-1.0 AND XMAX=1.0 ONLY FIX THE CENTRAL VALUE X=0.    *
C                                                                      *
C     OFRAME(MX,MY) IS AN ENTRY POINT INTO NFRAME; IT RESTORES THE     *
C PLOTTING COMMON CJE07 TO THE CONDITIONS OF THE PARTICULAR (IMX,IMY)  *
C PLOT DETERMINED BY THE LAST CALL TO NFRAME, TO PLOT A SECOND AND     *
C THIRD CURVE ON THE SAME PLOT.                                        *
C                                                                      *
C     SETADV(IA) IS ANOTHER ENTRY POINT INTO NFRAME; IT OVERRIDES THE  *
C AUTOMATIC ADVANCE AND PRINTING OF TOP AND BOTTOM LABELS (IN EFFECT   *
C FOR THE DEFAULT VALUE IA = 0).  IA = 1 / -1 : ADVANCE / NO ADVANCE,  *
C IRRESPECTIVE OF THE VALUES OF IMX AND IMY.                           *
C                                                                      *
C     WRITTEN BY CLAIR NIELSON.                                        *
C     MODIFIED BY DEBBY HYMAN 2/80 FOR SCALING AXES NICELY.            *
C     MODIFIED BY DICK HOGEWEIJ 21/06/84 FOR EQUIDISTANT SCALINGS.     *
C     MODIFIED BY HANS GOEDBLOED 14/11/85 FOR ADAPTATION TO NEW DLCH,  *
C     NEW MEANING OF ARGUMENTS MX,MY, IMPROVED EQUIDISTANT SCALING,    *
C     IMPROVED HANDLING OF THE LABELS.                                 *
C     NEW FRAME ADVANCE, ROUNDING OF LOG SCALES, HGO 4/11/91.          *
C***********************************************************************
C
      use itm_types
      implicit none
      integer MX,MY,IOP,NTITLE,NXNAME,NYNAME
      COMMON /KPOS/KP(36)
      integer kp
      COMMON /LHEAD1/LABTOP,LABBOT,D,T
      CHARACTER LABTOP*80,LABBOT*40,D*10,T*8
      COMMON /LHEAD2/NCT,NCB
      CHARACTER*(*) TITLE,XNAME,YNAME
      integer JXL(6),JXR(6),JYB(6),JYT(6),
     A        IXL(36),IXR(36),IYB(36),IYT(36),
     B        NPOS(36,36)
      real (r8) XL(36),XR(36),YB(36),YT(36)
      LOGICAL FLOGX,FLOGY
      INTEGER ASW
      REAL (R8) XMIN,XMAX,YMIN,YMAX
      SAVE IXL,IXR,IYB,IYT,XL,XR,YB,YT,ASW,NPOS
      integer nct,ncb,n,m
      real (r8) xln,xrn,alog19,ybn,ytn,xlx,xrx,ybx,ytx,
     &     xmid,fac,rmult,ymid,divx
      integer imx,imy,isx,isy,iax,iay,iix,iiy,jop,iadv,icen,nx,
     &     idum,jdum,idiff,ny,ixlx,ixrx,iybx,iytx,
     &     idivy,divy,mult,idivx,mcx,mcy,nxnam1,ixname,
     &     nynam1,iyname,ntitl1,ititle,icharsize,ia
C
C     * FRAME COORDINATES.
      DATA JXL/ 90, 90,590, 90, 423, 756/
      DATA JXR/990,490,990,323, 656, 990/
      DATA JYB/ 77,437, 77,557,317, 77/
      DATA JYT/722,722,362,722,482,242/
      DATA ASW/0/
C
C     * DECISION TABLE FOR FRAME ADVANCE.
C M:       1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
C MX,MY:  11 12 13 14 15 16 21 22 23 24 25 26 31 32 33 34 35 36
C
      DATA ((NPOS(N,M),N=1,18),M=1,18)
     1   / 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     2     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     3     1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1,
     4     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     5     1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0,
     6     1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
     7     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     8     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     9     1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0,
     *     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     1     1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0,
     2     1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0,
     3     1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
     4     1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0,
     5     1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1,
     6     1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0,
     7     1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0,
     8     1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1/
c
C M:      19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36
C MX,MY:  41 42 43 44 45 46 51 52 53 54 55 56 61 62 63 64 65 66
      DATA ((NPOS(N,M),N=1,18),M=19,36)
     9  /  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     *     1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0,
     1     1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1,
     2     1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1,
     3     1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1,
     4     1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0,
     5     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
     6     1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
     7     1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
     8     1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
     9     1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
     *     1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
     1     1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     2     1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0,
     3     1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1,
     4     1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0,
     5     1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0,
     6     1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1 /
      DATA ((NPOS(N,M),N=19,36),M=1,18)
C M:      19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36
C MX,MY:  41 42 43 44 45 46 51 52 53 54 55 56 61 62 63 64 65 66
     1  /  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     2     1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0,
     3     1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1,
     4     1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1,
     5     1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1,
     6     1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0,
     7     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
     8     1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
     9     1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
     *     1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
     1     1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
     2     1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
     3     1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     4     1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0,
     5     1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1,
     6     1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0,
     7     1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0,
     8     1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1/
      DATA ((NPOS(N,M),N=19,36),M=19,36)
     9  /  1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     *     1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     1     1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     2     1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     3     1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     4     1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     5     0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
     6     0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0,
     7     0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0,
     8     0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
     9     0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0,
     *     0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0,
     1     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
     2     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0,
     3     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1,
     4     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0,
     5     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0,
     6     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1/
C
C     * INPUT PARAMETERS.

      IMX=MOD(IABS(MX),10)
      IMY=MOD(IABS(MY),10)
      ISX=MOD(IABS(MX)/10,10)
      ISY=MOD(IABS(MY)/10,10)
      IAX=MOD(IABS(MX)/100,10)
      IAY=MOD(IABS(MY)/100,10)
      IIX=MOD(IABS(MX)/1000,10)
      IIY=MOD(IABS(MY)/1000,10)
      M=IMY+6*IMX-6
      IF(M.GT.36) STOP '*** NFRAME: IMX OR IMY TOO BIG ***'
      JOP=MOD(IABS(IOP),10)
      FLOGX=.FALSE.
      FLOGY=.FALSE.
      IF(JOP.EQ.3.OR.JOP.EQ.4) FLOGX=.TRUE.
      IF(JOP.EQ.2.OR.JOP.EQ.4) FLOGY=.TRUE.
C
C     * ADVANCE A PAGE AND DRAW TOP AND BOTTOM LABELS.
      IADV=0
      DO 10 N=1,36
   10    IADV=IADV+KP(N)*NPOS(N,M)
      IF((ASW.EQ.0.AND.IADV.NE.0).OR.(ASW.EQ.1)) THEN
         CALL ADV(1)
         DO 20 N=1,36
   20       KP(N)=0
      ENDIF
c      IF (MAXVAL(KP).GT.0) THEN
         ICEN=(JXR(1)+JXL(1))/2
         CALL DLCH(ICEN-6*IABS(NCT),766,LABTOP,NCT,-2)
         CALL DLCH(ICEN-9*IABS(NCB),0,LABBOT,NCB,-3)
         CALL DLCH(25,17,D,10,-1)
         CALL DLCH(25,2,T,8,-1)
c      ENDIF
      IF(ASW.NE.0) ASW=0
      KP(M)=1
C
C     * COMPUTE FRAME COORDINATES.
      IXL(M)=JXL(IMX)
      IXR(M)=JXR(IMX)
      IYB(M)=JYB(IMY)
      IYT(M)=JYT(IMY)
C     * B GODFREY'S IDEA TO FORCE NORMALIZATION:
      XLN=XMIN+0.0_R8
      XRN=XMAX+0.0_R8
      IF(XLN.EQ.XRN) THEN
         XLN=(1.0_R8-SIGN(0.5_R8,XLN))*XLN-1.0E-6_R8
         XRN=(1.0_R8+SIGN(0.5_R8,XRN))*XRN+1.0E-6_R8
      ENDIF
      IF(FLOGX) THEN
         XLN=ALOG19(XLN)
         XRN=ALOG19(XRN)
C        * LIMIT DECADES PLOTTED TO MOST SIGNIFICANT, HEWETT 6/9/83
         XLN=MAX(XLN,XRN-24._R8)
      ENDIF
      YBN=YMIN+0.0_R8
      YTN=YMAX+0.0_R8
      IF(YBN.EQ.YTN) THEN
         YBN=(1.0_R8-SIGN(0.5_R8,YBN))*YBN-1.0E-6_R8
         YTN=(1.0_R8+SIGN(0.5_R8,YTN))*YTN+1.0E-6_R8
      ENDIF
      IF(FLOGY) THEN
         YBN=ALOG19(YBN)
         YTN=ALOG19(YTN)
C        * LIMIT DECADES PLOTTED TO MOST SIGNIFICANT, HEWETT 6/9/83
         YBN=MAX(YBN,YTN-24._R8)
      ENDIF
      XL(M)=XLN
      XR(M)=XRN
      YB(M)=YBN
      YT(M)=YTN
C     * IF XMAX <= XMIN FOR A PARTICULAR FRAME,
C     * MAKE XMAX = XMIN + 1.0 AT LEAST.
      IF(XR(M).LE.XL(M)) XR(M)=XL(M)+MAX(1.0_R8,XL(M))
      IF(YT(M).LE.YB(M)) YT(M)=YB(M)+MAX(1.0_R8,YB(M))
C
C     * NUMBER OF INTERVALS NX FOR EXACT AND AUTOMATIC SCALING.
      NX=4
      IF(ISX.NE.0.AND.IIX.NE.0) NX=IIX
C     * AUTOMATIC SCALING.
      IF(ISX.EQ.0.AND.(.NOT.FLOGX)) THEN
         CALL ASCL(3,XL(M),XR(M),NX,IDUM,JDUM)
         IF(NX.GT.5) THEN
            IDIFF=XR(M)-XL(M)
            IF(MOD(IDIFF,5).EQ.0) NX=5
            IF(MOD(IDIFF,4).EQ.0) NX=4
            IF(MOD(IDIFF,3).EQ.0) NX=3
         ENDIF
      ENDIF
C     * ROUNDING OF LOG SCALES.
      IF(ISX.EQ.0.AND.(FLOGX)) THEN
         XL(M)=MIN(AINT(XL(M)),SIGN(AINT(ABS(XL(M))+.999),XL(M)))
         XR(M)=MAX(AINT(XR(M)),SIGN(AINT(ABS(XR(M))+.999),XR(M)))
      ENDIF
C
C     * NUMBER OF INTERVALS NY FOR EXACT AND AUTOMATIC SCALING.
      NY=4
      IF(ISY.NE.0.AND.IIY.NE.0) NY=IIY
C     * AUTOMATIC SCALING.
      IF(ISY.EQ.0.AND.(.NOT.FLOGY)) THEN
         CALL ASCL(3,YB(M),YT(M),NY,IDUM,JDUM)
         IF(NY.GT.5) THEN
            IDIFF=YT(M)-YB(M)
            IF(MOD(IDIFF,5).EQ.0) NY=5
            IF(MOD(IDIFF,4).EQ.0) NY=4
            IF(MOD(IDIFF,3).EQ.0) NY=3
         ENDIF
      ENDIF
C     * ROUNDING OF LOG SCALES.
      IF(ISY.EQ.0.AND.(FLOGY)) THEN
         YB(M)=MIN(AINT(YB(M)),SIGN(AINT(ABS(YB(M))+.999),YB(M)))
         YT(M)=MAX(AINT(YT(M)),SIGN(AINT(ABS(YT(M))+.999),YT(M)))
      ENDIF
C
C     * INITIALIZE COORDINATES OF THE EXTREME TICK MARKS.
      IXLX=IXL(M)
      IXRX=IXR(M)
      IYBX=IYB(M)
      IYTX=IYT(M)
      XLX=XL(M)
      XRX=XR(M)
      YBX=YB(M)
      YTX=YT(M)
C
C     * EQUIDISTANT SCALING WITH X-SCALE ADAPTED; ADDED 210684 GMDH.
      IF(ISX.EQ.2.AND.(.NOT.FLOGX)) THEN
         IF(ISY.EQ.2) STOP '*** NFRAME: ISX=ISY=2 FORBIDDEN ***'
C        * CENTER X-INTERVAL WITH RESPECT TO PLOTTING AREA; HGO 25/11/85
         XMID=.5*(XL(M)+XR(M))
         FAC=(YT(M)-YB(M))*(IXR(M)-IXL(M))/(IYT(M)-IYB(M))
         XL(M)=XMID-.5*FAC
         XR(M)=XMID+.5*FAC
         IDIVY=(IYT(M)-IYB(M))/NY
         DIVY=(YT(M)-YB(M))/NY
         RMULT=(XL(M)-YB(M))/DIVY
         MULT=RMULT
         IF(MULT.LT.RMULT) MULT=MULT+1
         XLX=YB(M)+MULT*DIVY
         IXLX=IXL(M)+(XLX-XL(M))*(IXR(M)-IXL(M))/(XR(M)-XL(M))
         NX=0
         XRX=XLX
   30    IF((XRX+DIVY).LE.XR(M)) THEN
            XRX=XRX+DIVY
            NX=NX+1
            GOTO 30
         ENDIF
         IXRX=IXLX+IDIVY*NX
      ENDIF
C
C     * EQUIDISTANT SCALING WITH Y-SCALE ADAPTED; ADDED 210684 GMDH.
      IF(ISY.EQ.2.AND.(.NOT.FLOGY)) THEN
C        * CENTER Y-INTERVAL WITH RESPECT TO PLOTTING AREA; HGO 25/11/85
         YMID=.5*(YB(M)+YT(M))
         FAC=(XR(M)-XL(M))*(IYT(M)-IYB(M))/(IXR(M)-IXL(M))
         YB(M)=YMID-.5*FAC
         YT(M)=YMID+.5*FAC
         IDIVX=(IXR(M)-IXL(M))/NX
         DIVX=(XR(M)-XL(M))/NX
         RMULT=(YB(M)-XL(M))/DIVX
         MULT=RMULT
         IF(MULT.LT.RMULT) MULT=MULT+1
         YBX=XL(M)+MULT*DIVX
         IYBX=IYB(M)+(YBX-YB(M))*(IYT(M)-IYB(M))/(YT(M)-YB(M))
         NY=0
         YTX=YBX
   40    IF((YTX+DIVX).LE.YT(M)) THEN
            YTX=YTX+DIVX
            NY=NY+1
            GOTO 40
         ENDIF
         IYTX=IYBX+IDIVX*NY
      ENDIF
C
C     * DEFINE THE GRAPH AREA AND EXTREME TICK MARKS.
      CALL DGA(IXL(M),IXR(M),IYB(M),IYT(M),XL(M),XR(M),YB(M),YT(M))
      CALL DGAX(IXLX,IXRX,IYBX,IYTX,XLX,XRX,YBX,YTX)
C
C     * SUPPRESS PLOTTING OF THE SCALES IF MX/Y < 0.
      IF(MX.LT.0) NX=0
      IF(MY.LT.0) NY=0
C
C     * DRAW FRAME, SCALES, AND TICK MARKS (EXCEPT FOT JOP=5).
      IF(JOP.EQ.1) THEN
         CALL DLNLN(NX,NY,1,IAX,IAY)
         CALL SBLIN(NX)
         CALL SLLIN(NY)
      ELSEIF(JOP.EQ.2) THEN
         CALL DLNLG(NX,NY)
         CALL SBLIN(NX)
         CALL SLLOG(NY)
      ELSEIF(JOP.EQ.3) THEN
         CALL DLGLN(NX,NY)
         CALL SBLOG(NX)
         CALL SLLIN(NY)
      ELSEIF(JOP.EQ.4) THEN
         CALL DLGLG(NX,NY)
         CALL SBLOG(NX)
         CALL SLLOG(NY)
      ELSEIF(JOP.EQ.5) THEN
C        * DRAW X/Y=0 AXIS WHEN IAX/Y.NE.0.
         CALL DLNLN(0,0,0,IAX,IAY)
      ENDIF
C
C     * DRAW TITLE AND LABELS OF THE AXES.
C     * MAXIMUM NUMBER OF CHARACTERS FITTING ALONG THE FRAME:
      MCX=33
      IF(IMX.EQ.1) MCX=75
      MCY=13
      IF(IMY.EQ.2.OR.IMY.EQ.3) MCY=23
      IF(IMY.EQ.1) MCY=53
C     * TRUNCATE IF THE STRING IS TOO LONG, WHILE ACCOUNTING FOR THE
C     * DIFFERENT MEANING OF THE ARGUMENTS FOR SINGLE CHARACTER CODING:
      NXNAM1=ISIGN(MIN(IABS(NXNAME),MCX-6),NXNAME)
      IXNAME=(IXL(M)+IXR(M))/2-6*IABS(NXNAM1)
      IF((LEN(XNAME).EQ.1).AND.(NXNAME.NE.1)) THEN
         NXNAM1=NXNAME
         IXNAME=(IXL(M)+IXR(M))/2-6
      ENDIF
      NYNAM1=ISIGN(MIN(IABS(NYNAME),MCY),NYNAME)
      IYNAME=(IYB(M)+IYT(M))/2-6*IABS(NYNAM1)
      IF((LEN(YNAME).EQ.1).AND.(NYNAME.NE.1)) THEN
         NYNAM1=NYNAME
         IYNAME=(IYB(M)+IYT(M))/2-6
      ENDIF
      NTITL1=ISIGN(MIN(IABS(NTITLE),MCX),NTITLE)
CGTA      ITITLE=(IXL(M)+IXR(M))/2-6*IABS(NTITL1)
      ITITLE = IXL(M)
      ICHARSIZE = 2
      IF ((IMX.GE.4).AND.(IMY.GE.4)) ICHARSIZE= 1
      CALL DLCH(IXNAME,IYB(M)-43,XNAME,NXNAM1,-2)
      CALL DLCV(IXL(M)-64,IYNAME,YNAME,NYNAM1,-2)
      CALL DLCH(ITITLE,IYT(M)+8,TITLE,NTITL1,-ICHARSIZE)
      RETURN
C
C     * ENTRY FOR RESTORING PLOTTING COMMON CJE07.
      ENTRY OFRAME(MX,MY)
      IMX=MOD(IABS(MX),10)
      IMY=MOD(IABS(MY),10)
      M=IMY+6*IMX-6
      IF(M.GT.36) STOP '*** OFRAME: IMX OR IMY TOO BIG ***'
      CALL DGA(IXL(M),IXR(M),IYB(M),IYT(M),XL(M),XR(M),YB(M),YT(M))
      RETURN
C
C     * ENTRY FOR MANUAL ADVANCE BEFORE NEXT PLOT.
      ENTRY SETADV(IA)
      ASW=IA
      RETURN
      END
C
      SUBROUTINE ASCL(M,ZMIN,ZMAX,MAJOR,MINOR,KF)
C
C***********************************************************************
C     THIS ROUTINE PROVIDES THE AUTOMATIC SCALING OF THE GRAPH BOUN-   *
C DARIES TO ROUNDED DECIMAL NUMBERS AND COMPUTES THE ASSOCIATED PARAM- *
C ETERS FOR THE LINEAR GRID DRAWING SUBROUTINES.                       *
C                                                                      *
C M     - ON INPUT, MINIMUM NUMBER OF MAJOR INTERVALS (1 <= M <= 20).  *
C         IT IS SUGGESTED THAT M BE FAIRLY SMALL (E.G. 4 OR 5) IN      *
C         ORDER TO PREVENT THE NUMERICAL SCALE FROM RUNNING TOGETHER.  *
C         THIS DEPENDS ON HOW MUCH OF THE PLOTTING AREA IS TO BE USED  *
C         AND ON THE NUMBER OF CHARACTERS WHICH WILL BE NEEDED FOR     *
C         EACH SCALE NUMBER.                                           *
C ZMIN  - ON INPUT, THE VALUE OF THE SMALLER ENDPOINT.                 *
C         ON OUTPUT, THE VALUE OF THE NEW SMALLER ENDPOINT.            *
C ZMAX  - ON INPUT, THE VALUE OF THE LARGER ENDPOINT.                  *
C         ON OUTPUT, THE VALUE OF THE NEW LARGER ENDPOINT.             *
C MAJOR - ON OUTPUT, THE NUMBER OF MAJOR INTERVALS AT WHICH TO PLACE   *
C         TICK MARKS AND A NUMERIC SCALE.                              *
C MINOR - ON OUTPUT, THE NUMBER OF MINOR INTERVALS AT WHICH TO PLACE   *
C         TICK MARKS AND A NUMERIC SCALE.                              *
C KF    - ON OUTPUT, THE FORMAT CODE DESCRIBING THE NUMBER OF DIGITS   *
C         NECESSARY TO DISPLAY THE SCALE NUMBERS UNIQUELY.  KF IS AN   *
C         INTEGER (0 <= KF <= 6 OR 10 <= KF <= 16) SUCH THAT THE UNITS *
C         DIGIT SPECIFIES THE NUMBER OF DIGITS TO BE PRINTED TO THE    *
C         RIGHT OF THE DECIMAL POINT.  A TENS DIGIT OF ZERO INDICATES  *
C         FIXED POINT FORMAT (F FORMAT) AND A TENS DIGIT OF ONE INDI-  *
C         CATES FLOATING POINT FORMAT (E FORMAT).  THIS FORMAT CODE    *
C         WAS USED PREVIOUSLY FOR PLACING A NUMERIC SCALE ALONG  THE   *
C         GRAPH BOUNDARY USING THE SCALE BOUNDARY ROUTINES SBLIN AND   *
C         SLLIN.  THE PRESENT VERSIONS OF THE LATTER SUBROUTINES DO    *
C         NOT HAVE THIS INPUT ARGUMENT ANYMORE.                        *
C***********************************************************************
C
      use itm_types
      implicit none
      integer M,MAJOR,MINOR,KF
      real (r8) ZMIN,ZMAX
      real (r8) z1,z2,am,zbar,z,p,tenk,dz,fn
      integer iflag,k,nm,n1,n2,j

      Z1=ZMIN
      Z2=ZMAX
      AM=M
C
C     * ZMAX <= ZMIN, M <= 0, AND M > 20 ARE INVALID VALUES: RETURN.
      IF((Z2.LE.Z1).OR.(M.LE.0.OR.M.GT.20)) THEN
         MAJOR=0
         MINOR=0
         KF=0
         RETURN
      ENDIF
C
      IF(Z2.NE.0.AND.Z1.NE.0) THEN
         ZBAR=Z2/Z1
         IF(ABS(ZBAR).GE.1000._R8) THEN
            Z1=0._R8
         ELSEIF(ABS(ZBAR).LE..001) THEN
            Z2=0._R8
         ELSEIF(ABS(ZBAR-1._R8).LE..000005*AM) THEN
            ZBAR=(Z2+Z1)/2._R8
            Z=.0000026*AM*ABS(ZBAR)
            Z2=ZBAR+Z
            Z1=ZBAR-Z
            GOTO 10
         ENDIF
      ENDIF
      IF(Z2-Z1.NE.AM) THEN
         Z2=Z2-.000001*ABS(Z2)
         Z1=Z1+.000001*ABS(Z1)
      ENDIF
   10 P=(Z2-Z1)/AM
      IFLAG=0
      TENK=1._R8
      K=0
      IF(P.LT.1._R8) THEN
         IFLAG=1
         P=1._R8/P
      ENDIF
   20 IF(P.GE.10000._R8) THEN
         P=P/10000._R8
         TENK=TENK*10000._R8
         K=K+4
         GOTO 20
      ENDIF
   30 IF(P.GE.10._R8) THEN
         P=P/10._R8
         TENK=TENK*10._R8
         K=K+1
         GOTO 30
      ENDIF
      IF(IFLAG.NE.0) THEN
         P=10._R8/P
         TENK=.1/TENK
         K=-K-1
      ENDIF
      IF(P.LT.2._R8) THEN
         P=1._R8
         NM=5
      ELSEIF(P.LT.5) THEN
         P=2._R8
         NM=4
      ELSEIF(P.GE.5._R8) THEN
         P=5._R8
         NM=5
      ENDIF
      DZ=P*TENK
      N1=Z1/DZ
      FN=N1
      Z=FN*DZ
      IF(Z.GT.Z1) THEN
         Z=Z-DZ
         N1=N1-1
      ENDIF
      Z1=Z
      N2=Z2/DZ
      FN=N2
      Z=FN*DZ
      IF(Z.LT.Z2) THEN
         N2=N2+1
         Z=Z+DZ
      ENDIF
      Z2=Z
      IF(K.LE.0.AND.K.GE.-5) THEN
         K=-K
         GOTO 50
      ENDIF
      IF(ABS(Z2).LE.ABS(Z1)) THEN
         Z=ABS(Z1)
      ELSE
         Z=ABS(Z2)
      ENDIF
      Z=Z/TENK
      J=0
   40 IF(Z.GE.10._R8) THEN
         Z=Z/10._R8
         J=J+1
         GOTO 40
      ENDIF
      IF(K.GE.0.AND.J+K.LE.5) THEN
         K=0
      ELSE
         K=10+J
         IF(K.LT.11) K=11
      ENDIF
C
   50 ZMIN=Z1
      ZMAX=Z2
      MAJOR=N2-N1
      MINOR=NM*MAJOR
      KF=K
      RETURN
      END
C
      SUBROUTINE DGA(IX1,IX2,IY1,IY2,X1,X2,Y1,Y2)
C
C***********************************************************************
C     THIS ROUTINE DEFINES THE GRAPH AREA.  THE FIRST FOUR ARGUMENTS   *
C DEFINE THE FRAME COORDINATES FOR THE BOUNDARIES OF THE GRAPH AREA.   *
C THE NEXT FOUR ARGUMENTS ARE THE FLOATING-POINT VALUES ASSIGNED TO    *
C THE BOUNDARIES.  IF IXL > IXR, AND SIMILARLY IF IYB > IYT, THEY ARE  *
C REVERSED.  THE BOUNDARY COORDINATES ARE TESTED FOR 0 < RANGE < 1023; *
C IF THEY ARE OUT OF RANGE, THEIR VALUES ARE SET TO THE APPROPRIATE    *
C MINIMUM OR MAXIMUM VALUE.  THERE ARE NO RESTRICTIONS ON XL, XR, YB,  *
C OR YT OTHER THAN NORMAL MACHINE LIMITS.  THE VALUES ARE STORED IN    *
C COMMON BLOCK CJE07.                                                  *
C     ENTRY DGAX FILLS COMMON /CJE07X/ WITH THE FRAME COORDINATES AND  *
C FLOATING-POINT VALUES BELONGING TO THE MINIMUM/MAXIMUM LOCATIONS OF  *
C THE TICK MARKS.                                                      *
C                                                                      *
C     MODIFIED 210684 GMDH: ADDED COMMON /CJE07X/ AND ENTRY DGAX.      *
C***********************************************************************
C
      use itm_types
      implicit none
      integer IX1,IX2,IY1,IY2
      real (r8) X1,X2,Y1,Y2
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      COMMON /CJE07X/XLX,XRX,YBX,YTX,IXLX,IXRX,IYBX,IYTX
      real (r8) xlx,xrx,ybx,ytx
      integer ixlx,ixrx,iybx,iytx
C
      IXL=MIN(MAX(0,MIN(IX1,IX2)),1023)
      IXR=MIN(MAX(0,MAX(IX1,IX2)),1023)
      IYB=MIN(MAX(0,MIN(IY1,IY2)),1023)
      IYT=MIN(MAX(0,MAX(IY1,IY2)),1023)
      XL=X1
      XR=X2
      YB=Y1
      YT=Y2
C
C     * ENTRY FOR EQUIDISTANT SCALING.
      ENTRY DGAX(IX1,IX2,IY1,IY2,X1,X2,Y1,Y2)
      IXLX=IX1
      IXRX=IX2
      IYBX=IY1
      IYTX=IY2
      XLX=X1
      XRX=X2
      YBX=Y1
      YTX=Y2
      RETURN
      END
C
      SUBROUTINE DLGLG(JX,JY)
C
C***********************************************************************
C     CALLING SEQUENCES: CALL DLNLN(NX,NY,IBOX,IAX,IAY)                *
C                        CALL DLNLG(NX,JY)                             *
C                        CALL DLGLN(JX,NY)                             *
C                        CALL DLGLG(JX,JY)                             *
C     THESE ARE THE CALLING SEQUENCES FOR DRAWING A FRAME WITH LINEAR- *
C LINEAR, LINEAR-LOG, LOG-LINEAR, AND LOG-LOG GRIDS, RESPECTIVELY.     *
C LINEAR-LOG MEANS THE GRID WILL BE DIVIDED LINEARLY IN THE X-DIREC-   *
C TION AND LOGARITHMICALLY IN THE Y-DIRECTION.  LOG-LINEAR MEANS THE   *
C REVERSE.  NX AND NY REFER TO THE NUMBER OF LINEAR INTERVALS IN THE   *
C X- AND Y-DIRECTIONS.                                                 *
C     THE NUMBER OF LOG CYCLES TO BE DRAWN IS DETERMINED BY THE        *
C FLOATING-POINT VALUES ASSIGNED TO THE APPROPRIATE GRAPH BOUNDARIES.  *
C THE VALUE ASSIGNED TO A SPECIFIC BOUNDARY IN THIS CASE IS THE POWER  *
C OF 10 ASSOCIATED WITH THAT BOUNDARY.  THE DIFFERENCE BETWEEN THE     *
C VALUES ASSIGNED TO THE BOUNDARIES IS THE NUMBER OF LOG CYCLES.  IF   *
C THE NUMBER OF CYCLES EXCEEDS 25, AN ERROR MESSAGE IS PRINTED, AND AN *
C EXIT IS PERFORMED WITHOUT DRAWING ANY CYCLES.  THE LINEAR GRID WILL  *
C BE COMPLETE THOUGH.                                                  *
C                                                                      *
C     MODIFIED HGO 13/11/85: ARGUMENTS JX,JY ADDED TO PERMIT SEPARATE  *
C     SUPPRESSION OF THE TICK MARKS FOR JX/Y=0.                        *
C***********************************************************************
C
      use itm_types
      implicit none
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      integer IXY(4)
      real (r8) XY(4)
      EQUIVALENCE (IXY,IXL),(XY,XL)
      real (r8) ALG(8)
      CHARACTER*14 MESS1,MESS2(2)
      real (r8) z1,z2,zmin,zmax,z11
      integer k,jx,jy,iex,itype,i1,i2,nz,irev,isl,izc,i,icz,ny,nx

C
      DATA (ALG(K),K=1,8) /.30102999566398,.47712125471966,
     A     .60205999132796,.69897000433602,.77815125038364,
     B     .84509804001426,.90308998699194,.95424250943933/
      DATA MESS1/'DECADES EXCEED'/
      DATA MESS2/'  25 NO OF X  ','  25 NO OF Y  '/
C
C     * ORDER OF EXECUTION IS: FIRST VERTICAL AXIS (ITYPE=2),
C     * THEN HORIZONTAL AXIS (ITYPE=1).
      IEX=1
      ITYPE=2
      CALL BOX(IXL,IXR,IYB,IYT)
C
C     * SKIP PLOTTING OF THE VERTICAL SCALE IF JY=0.
   10 IF(IEX.EQ.1.AND.JY.EQ.0) GOTO 40
      I1=2*ITYPE-1
      I2=2*ITYPE
      Z1=XY(I1)
      Z2=XY(I2)
      IF(Z1.EQ.Z2) Z2=Z2+.01
      ZMIN=MIN(Z1,Z2)
      ZMAX=MAX(Z1,Z2)
      ZMIN=MIN(AINT(ZMIN),SIGN(AINT(ABS(ZMIN)+.999),ZMIN))
      ZMAX=MAX(AINT(ZMAX),SIGN(AINT(ABS(ZMAX)+.999),ZMAX))
      Z1=ZMIN
      Z2=ZMAX
      NZ=ABS(Z1-Z2)
      IF(NZ.GT.25) THEN
         CALL DLCH(500,520,MESS1,14,2)
         CALL DLCH(500,500,MESS2(ITYPE),14,2)
         RETURN
      ENDIF
      IF(NZ.EQ.0) THEN
         Z11=Z1+1._R8
         IF(Z2.LT.Z1) Z11=Z1-1._R8
         NZ=1
         Z1=Z11
      ENDIF
      IF(XY(I2).GE.XY(I1)) THEN
         IREV=1
         XY(I1)=Z1
         XY(I2)=Z2
      ELSE
         IREV=2
         XY(I1)=Z2
         XY(I2)=Z1
      ENDIF
      ISL=(IXY(I2)-IXY(I1))/NZ
      IZC=IXY(I1)
      DO 30 I=1,NZ
         DO 20 K=1,8
            ICZ=IZC+(IREV-1+(3-IREV-IREV)*ALG(K))*ISL
            IF(ITYPE.EQ.1) THEN
               CALL DRV(REAL(ICZ,R8),REAL(IYT-15,R8),
     &              REAL(ICZ,R8),REAL(IYT,R8))
               CALL DRV(REAL(ICZ,R8),REAL(IYB,R8),
     &              REAL(ICZ,R8),REAL(IYB+15,R8))
            ELSE
               CALL DRV(REAL(IXL,R8),REAL(ICZ,R8),
     &              REAL(IXL+15,R8),REAL(ICZ,R8))
               CALL DRV(REAL(IXR-15,R8),REAL(ICZ,R8),
     &              REAL(IXR,R8),REAL(ICZ,R8))
            ENDIF
   20    CONTINUE
         IZC=IXY(I1)+(I*(IXY(I2)-IXY(I1)))/NZ
         IF(ITYPE.EQ.1) THEN
            CALL DRV(REAL(IZC,R8),REAL(IYT,R8),
     &              REAL(IZC,R8),REAL(IYT-25,R8))
            CALL DRV(REAL(IZC,R8),REAL(IYB+25,R8),
     &              REAL(IZC,R8),REAL(IYB,R8))
         ELSE
            CALL DRV(REAL(IXL,R8),REAL(IZC,R8),
     &              REAL(IXL+25,R8),REAL(IZC,R8))
            CALL DRV(REAL(IXR-25,R8),REAL(IZC,R8),
     &              REAL(IXR,R8),REAL(IZC,R8))
         ENDIF
   30 CONTINUE
      IF(IEX.EQ.2) RETURN
      GOTO 40
C
      ENTRY DLGLN(JX,NY)
      CALL DLNLN(0,NY,1,0,0)
   40 IF(JX.EQ.0) RETURN
      IEX=2
      ITYPE=1
c-------------------------- this some avoids a bad system call on DEC
      WRITE(*,*)
      GOTO 10
C
      ENTRY DLNLG(NX,JY)
      CALL DLNLN(NX,0,1,0,0)
      IF(JY.EQ.0) RETURN
      IEX=2
      ITYPE=2
      GOTO 10
      END
C
      SUBROUTINE DLNLN(NX,NY,IBOX,IAX,IAY)
C
C***********************************************************************
C     THIS ROUTINE DRAWS A FRAME WITH A LINEAR-LINEAR GRID CONSISTING  *
C OF NX EQUALLY SPACED INTERVALS IN THE X-DIRECTION AND NY EQUALLY     *
C SPACED INTERVALS IN THE Y-DIRECTION (0 < NX/Y <= 10).  THE INTERVALS *
C ARE MARKED OFF BY TICKS ON THE BOUNDARIES.                           *
C                                                                      *
C     MODIFIED 210684 GMDH: NUMBER OF TICK MARKS INCREASED WITH 1 ON   *
C     EACH SIDE OF THE RANGE; COMMON CJE07X ADDED.                     *
C     MODIFIED HGO 13/11/85: SUPPRESS TICK MARKS IF NX/Y=0; ADDED THE  *
C     ARGUMENTS IBOX,IAX,IAY TO SUPPRESS DRAWING OF THE BOX IF IBOX=0  *
C     AND TO DRAW THE X/Y=0 AXIS IF IAX/Y.NE.0                         *
C***********************************************************************
C
      use itm_types
      implicit none
      integer NX,NY,IBOX,IAX,IAY
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      COMMON /CJE07X/XLX,XRX,YBX,YTX,IXLX,IXRX,IYBX,IYTX
      real (r8) xlx,xrx,ybx,ytx,dx,dy
      integer ixlx,ixrx,iybx,iytx,ix0,idum,nxs,iiyb,iiyt,
     &     i,ixs,iy0,nys,iixr,iixl,iys

C
      IF(IBOX.NE.0) CALL BOX(IXL,IXR,IYB,IYT)
C
      IF(IAX.NE.0.AND.(XL.LT.0..AND.XR.GT.0.)) THEN
C        * DRAW X=0 AXIS.
         IX0=(IXL*XR-IXR*XL)/(XR-XL)
       IF(IAX.EQ.1)
     >      CALL DRV(REAL(IX0,R8),REAL(IYB,R8),
     &        REAL(IX0,R8),REAL(IYT,R8))
       IF(IAX.EQ.2)
     >      CALL DASH(REAL(IX0,R8),REAL(IYB,R8),
     &      REAL(IX0,R8),REAL(IYT,R8),
     >      10,10,0,IDUM)
      ENDIF
      IF(NX.NE.0) THEN
         NXS=MIN(IABS(NX),128)
         DX=REAL(IXRX-IXLX,R8)/NXS
         IIYB=IYB+20
         IIYT=IYT-20
         DO 10 I=0,NXS
            IXS=IXLX+I*DX
            CALL DRV(REAL(IXS,R8),REAL(IYB,R8),
     &           REAL(IXS,R8),REAL(IIYB,R8))
            CALL DRV(REAL(IXS,R8),REAL(IYT,R8),
     &           REAL(IXS,R8),REAL(IIYT,R8))
   10    CONTINUE
      ENDIF
C
      IF(IAY.NE.0.AND.(YB.LT.0..AND.YT.GT.0.)) THEN
C        * DRAW Y=0 AXIS.
         IY0=(IYB*YT-IYT*YB)/(YT-YB)
         IF(IAY.EQ.1)
     >      CALL DRV(REAL(IXL,R8),REAL(IY0,R8),
     &        REAL(IXR,R8),REAL(IY0,R8))
         IF(IAY.EQ.2) CALL DASH(REAL(IXL,R8),REAL(IY0,R8),REAL(IXR,R8),
     >                          REAL(IY0,R8),10,10,0,IDUM)
      ENDIF
      IF(NY.NE.0) THEN
         NYS=MIN(IABS(NY),128)
         DY=REAL(IYTX-IYBX,R8)/NYS
         IIXR=IXR-20
         IIXL=IXL+20
         DO 20 I=0,NYS
            IYS=IYBX+I*DY
            CALL DRV(REAL(IXL,R8),REAL(IYS,R8),
     &           REAL(IIXL,R8),REAL(IYS,R8))
            CALL DRV(REAL(IXR,R8),REAL(IYS,R8),
     &           REAL(IIXR,R8),REAL(IYS,R8))
   20    CONTINUE
      ENDIF
      RETURN
      END
C
      SUBROUTINE SBLIN(NX)
C
C***********************************************************************
C     THIS ROUTINE PRINTS A LINEAR NUMERIC SCALE ON THE BOTTOM BOUN-   *
C DARY OF A FRAME WITH NX EQUALLY SPACED INTERVALS DRAWN BY DLNLN OR   *
C DLNLG.  THE NUMBERS ARE PRINTED IN F5.2 FORMAT WITH AN ADDITIONAL    *
C POWER OF 10 (IF NEEDED) PRINTED SEPATATELY.  THE DATA FOR THE SCALE  *
C ARE OBTAINED FROM XLX,XRX,YBX,YTX OF COMMON BLOCK CJE07X.            *
C                                                                      *
C     MODIFIED BY DEBBY HYMAN 4/7/80: SCALE FACTOR KS CORRECTED.       *
C     MODIFIED 210684 GMDH: ADDED COMMON /CJE07X/.                     *
C     MODIFIED HGO 13/11/85: RETURN ON NX=0.                           *
C***********************************************************************
C
      use itm_types
      implicit none
      integer nx
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      COMMON /CJE07X/XLX,XRX,YBX,YTX,IXLX,IXRX,IYBX,IYTX
      CHARACTER*5 OUT
      real (r8) xlx,xrx,ybx,ytx,t,x,alog19,xll,xrr,dx,ddx,xc
      integer ixlx,ixrx,iybx,iytx,ks,fact,iyb1,nxa,i,ixc,j,ixr1,iyb2

C
      IF(NX.EQ.0) RETURN
C
C     * DETERMINE THE SCALE FACTOR KS OF 10.
      T=MAX(ABS(XLX),ABS(XRX))
      IF(ABS(T).LE.1.E-15_R8) T=1.E-15_R8
      X=ALOG19(T)
C     * FIX FOR -1.0E7 THAT RETURNS KS=6 INSTEAD OF KS=7:
      KS=X+SIGN(0.001_R8,X)
C
      FACT=10._R8**(-KS)
      XLL=XLX*FACT
      XRR=XRX*FACT
C
C     * WRITE XLL ONTO THE BOUNDARY.
      IYB1=IYB-18
      WRITE(OUT,'(F5.2)') XLL
      CALL DLCH(IXLX-18,IYB1,OUT,5,-2)
C
C     * DETERMINE THE NUMBER OF INTERVALS TO SCALE (0 < NX <= 10).
      NXA=MIN(10,IABS(NX))
      DX=(XRR-XLL)/NXA
      DDX=REAL(IXRX-IXLX,R8)/NXA
C
C     * WRITE THE SCALE ONTO THE BOUNDARY.
      DO 10 I=1,NXA
         IXC=IXLX+I*DDX-18
         XC=XLL+I*DX
         WRITE(OUT,'(F5.2)') XC
         CALL DLCH(IXC,IYB1,OUT,5,-2)
   10 CONTINUE
C
C     * WRITE THE SCALE FACTOR OF 10.
      IF(KS.EQ.0) RETURN
      IF(2.LE.KS.AND.KS.LE.9) J=1
      IF((-9.LE.KS.AND.KS.LE.-1).OR.(KS.GT.9)) J=2
      IF(KS.LE.-10) J=3
      IXR1=IXR-36
      IYB2=IYB-43
      CALL DLCH(IXR1,IYB2+1,'X',1,1)
      CALL DLCH(IXR1,IYB2,' 10',3,2)
      IF(KS.EQ.1) RETURN
      WRITE(OUT,'(I3)') KS
      CALL DLCH(IXR1+36,IYB2+8,OUT(4-J:3),J,1)
      RETURN
      END
C
      SUBROUTINE SLLIN(NY)
C
C***********************************************************************
C     THIS ROUTINE PRINTS A LINEAR NUMERIC SCALE ON THE LEFT BOUN-     *
C DARY OF A FRAME WITH NY EQUALLY SPACED INTERVALS DRAWN BY DLNLN OR   *
C DLGLN.  THE NUMBERS ARE PRINTED IN F5.2 FORMAT WITH AN ADDITIONAL    *
C POWER OF 10 (IF NEEDED) PRINTED SEPATATELY.  THE DATA FOR THE SCALE  *
C ARE OBTAINED FROM XLX,XRX,YBX,YTX OF COMMON BLOCK CJE07X.            *
C                                                                      *
C     MODIFIED BY DEBBY HYMAN 4/7/80: FIXED ALOG(X) BEING OFF FOR      *
C     SCALING IN SOME CASES.                                           *
C     MODIFIED 210684 GMDH: ADDED COMMON /CJE07X/.                     *
C     MODIFIED HGO 13/11/85: RETURN ON NY=0.                           *
C***********************************************************************
C
      use itm_types
      implicit none
      integer ny
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      COMMON /CJE07X/XLX,XRX,YBX,YTX,IXLX,IXRX,IYBX,IYTX
      CHARACTER*5 OUT
      real (r8) xlx,xrx,ybx,ytx,t,x,alog19,fact,ytt,ybb,dy,ddy,yc
      integer ixlx,ixrx,iybx,iytx,ks,ixl1,nya,i,iyc,j,iyt1
C
      IF(NY.EQ.0) RETURN
C
C     * DETERMINE THE SCALE FACTOR KS OF 10.
      T=MAX(ABS(YBX),ABS(YTX))
      IF(ABS(T).LE.1.E-15_R8) T=1.E-15_R8
      X=ALOG19(T)
C     * FIX FOR -1.0E7 THAT RETURNS KS=6 INSTEAD OF KS=7:
      KS=X+SIGN(0.001_R8,X)
C
      FACT=10._R8**(-KS)
      YTT=YTX*FACT
      YBB=YBX*FACT
C
C     * WRITE YBB ONTO THE BOUNDARY.
      IXL1=IXL-64
      IF(IXL1.LT.15) IXL1=15
      WRITE(OUT,'(F5.2)') YBB
      CALL DLCH(IXL1,IYBX-2,OUT,5,2)
C
C     * DETERMINE THE NUMBER OF INTERVALS TO SCALE (0 < NY <= 10).
      NYA=MIN(10,IABS(NY))
      DY=(YTT-YBB)/NYA
      DDY=REAL(IYTX-IYBX,R8)/NYA
C
C     * WRITE THE SCALE ONTO THE BOUNDARY.
      DO 10 I=1,NYA
         IYC=IYBX+I*DDY-6
         YC=YBB+I*DY
         WRITE(OUT,'(F5.2)') YC
         CALL DLCH(IXL1,IYC,OUT,5,2)
   10 CONTINUE
C
C     * WRITE THE SCALE FACTOR OF 10.
      IF(KS.EQ.0) RETURN
      IF(2.LE.KS.AND.KS.LE.9) J=1
      IF((-9.LE.KS.AND.KS.LE.-1).OR.(KS.GT.9)) J=2
      IF(KS.LE.-10) J=3
      IYT1=IYT+13
      CALL DLCH(IXL1,IYT1+1,'X',1,1)
      CALL DLCH(IXL1,IYT1,' 10',3,2)
      IF(KS.EQ.1) RETURN
      WRITE(OUT,'(I3)') KS
      CALL DLCH(IXL1+36,IYT1+8,OUT(4-J:3),J,1)
      RETURN
      END
C
      SUBROUTINE SBLOG(JX)
C
C***********************************************************************
C     THIS ROUTINE PRINTS A LOG NUMERIC SCALE ON THE BOTTOM BOUNDARY.  *
C THROUGH ENTRY SLLOG(JY) A LOG NUMERIC SCALE IS PRINTED ON THE LEFT   *
C BOUNDARY.                                                            *
C                                                                      *
C     MODIFIED HGO 13/11/85: RETURN UPON JX=0 AND JY=0.                *
C***********************************************************************
C
      use itm_types
      implicit none
      integer jx
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT
      real (r8) XL,XR,YB,YT
      integer IXL,IXR,IYB,IYT
      integer IXY(4)
      real (r8) XY(4)
      EQUIVALENCE (IXY,IXL),(XY,XL)
      CHARACTER*3 OUT
      integer iy,iydel,iydl,ix,ixdel,ixdl,i1,i2,jy,ixyv,
     &     nx,ixc,iyc,ixx,iyx,j,idxyv,i

C
      IF(JX.EQ.0) RETURN
      IY=IYB
      IYDEL=-23
      IYDL=8
      IX=IXL
      IXDEL=-16
      IXDL=23
      I1=1
      I2=2
      GOTO 10
C
C     * ENTRY FOR PLOTTING LOG NUMERIC SCALE ON THE LEFT BOUNDARY.
      ENTRY SLLOG(JY)
      IF(JY.EQ.0) RETURN
      IX=IXL
      IXDEL=-54
      IXDL=24
      IY=IYB
      IYDEL=-2
      IYDL=8
      I1=3
      I2=4
C
   10 IXYV=XY(I1)
      NX=MIN(ABS(XY(I1)-XY(I2)),25.0_R8)
      WRITE(OUT,'(I3)') IXYV
      IXC=IX+IXDEL
      IYC=IY+IYDEL
      IXX=IXC+IXDL
      IYX=IYC+IYDL
      CALL DLCH(IXC,IYC,'10',2,2)
      J=1
      IF(IXYV.LT.0) J=2
      IF(IXYV.LT.-9) J=3
      CALL DLCH(IXX,IYX,OUT(4-J:3),J,1)
      IF(NX.EQ.0) RETURN
      IDXYV=ISIGN(1,INT(XY(I2)-XY(I1)))
      DO 20 I=1,NX
         IXYV=IXYV+IDXYV
         WRITE(OUT,'(I3)') IXYV
         IF(I1.NE.1) THEN
            IYC=IY+IYDEL+(I*(IXY(I2)-IXY(I1)))/NX
            IYX=IYC+IYDL
         ELSE
            IXC=IX+IXDEL+(I*(IXY(I2)-IXY(I1)))/NX
            IXX=IXC+IXDL
         ENDIF
         CALL DLCH(IXC,IYC,'10',2,2)
         IF(IXYV.GE.-9) J=2
         IF(IXYV.GE.0) J=1
         CALL DLCH(IXX,IYX,OUT(4-J:3),J,1)
   20 CONTINUE
      RETURN
      END
C
      SUBROUTINE CONVRT(Z,IZ,Z1,Z2,IZ1,IZ2)
C
C***********************************************************************
C     CONVRT CONVERTS THE REAL NUMBER Z TO AN SC-4020 COORDINATE BASED *
C ON Z1 AND Z2 AS THE REAL USER-SCALED VALUES ASSOCIATED WITH THE PLOT *
C AREA BOUNDARIES IZ1 AND IZ2, RESPECTIVELY.  THE RESULT IS STORED IN  *
C IZ.  THE CONVERSION IS PERFORMED BY THE FORMULA:                     *
C                                                                      *
C     IZ = IZ1 +((Z -Z1)/(Z2 -Z1))*(IZ2 -IZ1)                          *
C                                                                      *
C IZ IS TESTED TO ENSURE THAT IT LIES WITHIN THE BOUNDARIES SPECIFIED  *
C BY IZ1 AND IZ2.  IF IT LIES OUTSIDE THESE LIMITS, IT IS SET EQUAL TO *
C THE APPROPRIATE LIMIT.  IF Z2 EQUALS Z1 ON INPUT, THEN IZ IS SET TO  *
C MAX(IZ1,IZ2).                                                        *
C                                                                      *
C Z       - REAL USER COORDINATE.                                      *
C IZ      - CONVERTED SC-4020 COORDINATE IN THE RANGE IZ1 TO IZ2.      *
C Z1/Z2   - REAL USER VALUES CORRESPONDING TO IZ1/IZ2.                 *
C IZ1/IZ2 - SC-4020 COORDINATES BOUNDS OF THE PLOT AREA ALONG ONE AXIS *
C           (0 <= IZ1 <= IZ2 <= 1023).                                 *
C                                                                      *
C EXAMPLE: "CALL CONVRT(1.,IX,0.,2.,100,900)".  UPON RETURN, IX=500.   *
C***********************************************************************
C
      use itm_types
      implicit none
      integer IZ,IZ1,IZ2
      real (r8) Z,Z1,Z2
      real (r8) f
      F=Z2-Z1
      IF(F.NE.0) F=(IZ2-IZ1)/F
      IZ=MIN(MAX(MIN(IZ1,IZ2),IZ1+INT((Z-Z1)*F)),MAX(IZ1,IZ2))
      RETURN
      END
C
      SUBROUTINE BOX(IX1,IX2,IY1,IY2)
C
C***********************************************************************
C     THIS ROUTINE DRAWS A BOX WITH VERTICAL SIDES AT IX1 AND IX2 AND  *
C HORIZONTAL SIDES AT IY1 AND IY2.                                     *
C                                                                      *
C     WRITTEN HGO 18/10/85                                             *
C***********************************************************************
C
      use itm_types
      implicit none
      integer IX1,IX2,IY1,IY2
      CALL DRV(REAL(IX1,R8),REAL(IY1,R8),REAL(IX1,R8),REAL(IY2,R8))
      CALL DRWABS(REAL(IX2,R8),REAL(IY2,R8))
      CALL DRWABS(REAL(IX2,R8),REAL(IY1,R8))
      CALL DRWABS(REAL(IX1,R8),REAL(IY1,R8))
      RETURN
      END
C
C ======================================================================
C ================= SYSTEM DEPENDENT PARTS BELOW =======================
C ======================================================================
C
      SUBROUTINE DATI(D,T)
C
C***********************************************************************
C     THIS SUBROUTINE WRITES DATE AND TIME ONTO THE VARIABLES D AND T. *
C                                                                      *
C     WRITTEN HGO 3/12/85                                              *
C     CHANGED NAME OF THE SUBROUTINE, HGO 2/8/91                       *
C***********************************************************************
C
      use itm_types
      implicit none
      CHARACTER D*10,T*8, DAT*20,TIM*20
      CHARACTER YEAR*4,MONTH*2,DAY*2,HOUR*2,MINUT*2

      CALL DATE_AND_TIME(DAT,TIM)
      YEAR  = DAT(1:4)
      MONTH = DAT(5:6)
      DAY   = DAT(7:8)
      HOUR  = TIM(1:2)
      MINUT = TIM(3:4)
      WRITE(D,1) DAY,MONTH,YEAR
      WRITE(T,2) HOUR,MINUT
    1 FORMAT(A2,'/',A2,'/',A4)
    2 FORMAT(A2,':',A2)
      RETURN
      END
C
      BLOCK DATA CALPOS
C
C***********************************************************************
C     ACTIVATING CALCOMP AND/OR POSTSCRIPT BRANCHES.                   *
C***********************************************************************
C
      use itm_types
      implicit none
      COMMON /LCLPS/LCAL,LPOS
      integer lcal,lpos
C
      DATA LCAL,LPOS / 0, 1 /
C
      END
C
      SUBROUTINE WRTEXT(IUNIT)
C
C***********************************************************************
C     BRANCHING TO WRTEXT1 (CALCOMP) / WRTEXT2 (POSTSCRIPT).           *
C***********************************************************************
C
      use itm_types
      implicit none
      integer iunit
      COMMON /KPOS/KP(36)
      COMMON /LCLPS/LCAL,LPOS
      integer lcal,lpos,kp
C
      IF(LCAL.EQ.1) CALL WRTEXT1(IUNIT)
      IF(LPOS.EQ.1) CALL WRTEXT2(IUNIT)
      KP(1) = 1
      RETURN
      END
C
      SUBROUTINE WRTEXT1(IUNIT)
C
C***********************************************************************
C     THIS ROUTINE READS A LOCAL FILE, THAT IS OPENED IN THE CALLING   *
C PROGRAM WITH THE UNIT NUMBER "IUNIT", AND WRITES IT TO THE GRAPHICS  *
C FILE.  WRTEXT STARTS WRITING ON A NEW FRAME, UNLESS IUNIT < 0 WHEN   *
C WRITING STARTS AT THE CURRENT IY POSITION OF THE DRAWING BEAM.  IT   *
C AUTOMATICALLY ADVANCES A FRAME IF THE FILE NEEDS AN ADDITIONAL PAGE. *
C TYPICAL USE FOR WRTEXT IS TO WRITE THE CURRENT UPDATE MODIFICATIONS  *
C OF THE SOURCE OR THE NAMELIST INPUT ONTO THE GRAPHICS FILE.          *
C                                                                      *
C     WRITTEN BY DEBBY HYMAN, 8-79                                     *
C     MODIFIED HGO 25/10/85: OPTION IUNIT < 0, IMPROVED LINE SPACING.  *
C***********************************************************************
C
      use itm_types
      implicit none
      integer iunit
      integer ispace
      PARAMETER (ISPACE=4)
      CHARACTER*80 LINE
      real (r8) zix,ziy
      integer my,iu,iy,l

C
      MY = 16+ISPACE
      IU = IABS(IUNIT)
      IF(IUNIT.LT.0) THEN
         CALL SEELOC1(ZIX,ZIY)
         IY = INT(ZIY)-MY
      ELSE
         CALL ADV1(1)
         IY = 780-MY
      ENDIF
C
*CRAY X-MP
*     CALL REWIND(IU)
*  10 READ(IU,'(A80)') LINE
*     IF(IEOF(IU).NE.0) GOTO 40
C
      REWIND IU
   10 READ(IU,'(A80)',END=40) LINE
C
      DO 20 L=80,1,-1
   20    IF(LINE(L:L).NE.' ') GOTO 30
   30 CALL DLCH1(20,IY,LINE(1:L),L,2)
      IY = IY-MY
C
      IF(IY.LT.0) THEN
C        * RESET IY FOR ANOTHER PAGE OF TEXT.
         CALL ADV1(1)
         IY = 780-MY
      ENDIF
      GOTO 10
C
   40 RETURN
      END
C
      SUBROUTINE WRTEXT2(IUNIT)
C
C***********************************************************************
C     POSTSCRIPT VERSION.                                              *
C***********************************************************************
C
      use itm_types
      implicit none
      integer iunit
      integer ispace
      PARAMETER (ISPACE=4)
      CHARACTER*80 LINE
      real (r8) zix,ziy
      integer my,iu,iy,l

C
      MY = 17+ISPACE
      IU = IABS(IUNIT)
      IF(IUNIT.LT.0) THEN
         CALL SEELOC2(ZIX,ZIY)
         IY = INT(ZIY)-MY
      ELSE
         CALL ADV2(1)
         IY = 780-MY
      ENDIF
C
*CRAY X-MP
*     CALL REWIND(IU)
*  10 READ(IU,'(A80)') LINE
*     IF(IEOF(IU).NE.0) GOTO 40
C
      REWIND IU
   10 READ(IU,'(A80)',END=40) LINE
C
      DO 20 L=80,1,-1
   20    IF(LINE(L:L).NE.' ') GOTO 30
   30 CALL DLCH2(20,IY,LINE(1:L),L,2)
      IY = IY-MY
C
      IF(IY.LT.0) THEN
C        * RESET IY FOR ANOTHER PAGE OF TEXT.
         CALL ADV2(1)
         IY = 780-MY
      ENDIF
      GOTO 10
C
   40 RETURN
      END
C
      BLOCK DATA EBDASC
C
C***********************************************************************
C     CONVERTS EBCDIC TO CORRESPONDING ASCII VALUES (AND VICE VERSA).  *
C***********************************************************************
C
      COMMON /NEBDASC/NEA(64:255)
      COMMON /NASCEBD/NAE(32:126)
C
      DATA (NEA(IE),IE=64,159)
     6   /                     32,   0,   0,   0,   0,   0,
     7      0,   0,   0,   0,   0,  46,  60,  40,  43, 124,
     8     38,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     9     33,  36,  42,  41,  59,  94,  45,  47,   0,   0,
     *      0,   0,   0,   0,   0,   0,   0,  44,  37,  95,
     1     62,  63,   0,   0,   0,   0,   0,   0,   0,   0,
     2      0,  96,  58,  35,  64,  39,  61,  34,   0,  97,
     3     98,  99, 100, 101, 102, 103, 104, 105,   0,   0,
     4      0,   0,   0,   0,   0, 106, 107, 108, 109, 110,
     5    111, 112, 113, 114,   0,   0,   0,   0,   0,   0 /
      DATA (NEA(IE),IE=160,255)
     6   /  0, 126, 115, 116, 117, 118, 119, 120, 121, 122,
     7      0,   0,   0,  91,   0,   0,   0,   0,   0,   0,
     8      0,   0,   0,   0,   0,   0,   0,   0,   0,  93,
     9      0,   0, 123,  65,  66,  67,  68,  69,  70,  71,
     *     72,  73,   0,   0,   0,   0,   0,   0, 125,  74,
     1     75,  76,  77,  78,  79,  80,  81,  82,   0,   0,
     2      0,   0,   0,   0,  92,   0,  83,  84,  85,  86,
     3     87,  88,  89,  90,   0,   0,   0,   0,   0,   0,
     4     48,  49,  50,  51,  52,  53,  54,  55,  56,  57,
     5      0,   0,   0,   0,   0,   0                     /
C
      DATA (NAE(IA),IA=32,126)
     3   /           64,  90, 127, 123,  91, 108,  80, 125,
     4     77,  93,  92,  78, 107,  96,  75,  97, 240, 241,
     5    242, 243, 244, 245, 246, 247, 248, 249, 122,  94,
     6     76, 126, 110, 111, 124, 193, 194, 195, 196, 197,
     7    198, 199, 200, 201, 209, 210, 211, 212, 213, 214,
     8    215, 216, 217, 226, 227, 228, 229, 230, 231, 232,
     9    233, 173, 224, 189,  95, 109, 121, 129, 130, 131,
     *    132, 133, 134, 135, 136, 137, 145, 146, 147, 148,
     1    149, 150, 151, 152, 153, 162, 163, 164, 165, 166,
     2    167, 168, 169, 192,  79, 208, 161                /
C
      END
C
      SUBROUTINE DLCH(IX,IY,STRING,NC,ISIZE)
C
C***********************************************************************
C     BRANCHING TO DLCH1 (CALCOMP) / DLCH2 (POSTSCRIPT).               *
C***********************************************************************
C
      use itm_types
      implicit none
      integer IX,IY,NC,ISIZE
      COMMON /LCLPS/LCAL,LPOS
      integer lcal,lpos
      CHARACTER*(*) STRING
C
      IF(LCAL.EQ.1) CALL DLCH1(IX,IY,STRING,NC,ISIZE)
      IF(LPOS.EQ.1) CALL DLCH2(IX,IY,STRING,NC,ISIZE)
      RETURN
C
      ENTRY DLCV(IX,IY,STRING,NC,ISIZE)
      IF(LCAL.EQ.1) CALL DLCV1(IX,IY,STRING,NC,ISIZE)
      IF(LPOS.EQ.1) CALL DLCV2(IX,IY,STRING,NC,ISIZE)
      RETURN
      END
C
      SUBROUTINE DLCH1(IX,IY,STRING,NC,ISIZE)
C
C***********************************************************************
C     THIS ROUTINE WILL PRINT ARBITRARILY LARGE CHARACTERS ON THE      *
C GRAPHICS FILE, EITHER HORIZONTALLY OR VERTICALLY (WITH ENTRY DLCV).  *
C HORIZONTAL PRINTING IS FROM LEFT TO RIGHT.  FOR VERTICAL PRINTING,   *
C CHARACTERS ARE ROTATED 90 DEGREES COUNTERCLOCKWISE AND PRINTED FROM  *
C BOTTOM TO TOP.  THE ROUTINE USES VECTORS TO DRAW CHARACTERS IN A     *
C BASIC 5 BY 7 MATRIX.                                                 *
C                                                                      *
C     THE NC CHARACTERS STORED IN STRING ARE WRITTEN WITH THE LOWER    *
C LEFT-HAND CORNER OF THE FIRST CHARACTER AT (IX,IY) FOR BOTH HORIZON- *
C TAL AND VERTICAL CHARACTERS.  CHARACTER AND LINE SPACING ARE AUTOMA- *
C TIC IN EITHER DIRECTION WITH CHARACTER SIZES GIVEN BY MX=ISIZE*6 AND *
C MY=ISIZE*8.  EACH LINE IS SPACED DOWN BY MY PLOTTING POSITIONS.  ON  *
C SUBSEQUENT CALLS IF IX < 0, PRINTING WILL CONTINUE WHERE IT LEFT OFF *
C ON THE PRECEDING PRINT.  IF IY < 0, THE FIRST CHARACTER IN STRING IS *
C CENTERED AT IX,IY.                                                   *
C                                                                      *
C     IF NC < 0, EACH OCCURRENCE OF THE CHARACTER '$' IN STRING CAUSES *
C THE FONT TO BE CHANGED FROM NORMAL (NFONT=1, IC=32,126) TO SYMBOL    *
C (NFONT=2, IC=192,254) AND VICE VERSA, WHERE '$' IS NOT COUNTED IN NC.*
C IF STRING=' ' AND NC.NE.1, ONE SINGLE CHARACTER IS DRAWN REPRESENTED *
C BY THE INTEGER CODE IC=NC.                                           *
C                                                                      *
C     FOR EXAMPLE,                                                     *
C THE LOWER CASE "a" IS PRINTED WITH:                                  *
C     "CALL DLCH(IX,IY,'a',1,ISIZE)" OR "DLCH(IX,IY,' ',97,ISIZE)",    *
C THE GREEK CHARACTER ALPHA WITH:                                      *
C     "CALL DLCH(IX,IY,'$a',-1,ISIZE)" OR "DLCH(IX,IY,' ',225,ISIZE)", *
C AND THE STRING "A single ALFA is printed" WITH:                      *
C     "CALL DLCH(IX,IY,'A single $A$ is printed',-21,ISIZE)".          *
C                                                                      *
C     METHOD:                                                          *
C THE CHARACTERS ARE DRAWN USING VECTORS WHICH ARE TAKEN FROM TABLE.   *
C TABLE IS THE COORDINATE MATRIX.  EACH CHARACTER IS DEFINED BY ONE    *
C WORD CONSISTING OF A SERIES OF DX,DY'S REPRESENTING A DISPLACEMENT   *
C IN A 5 BY 7 MATRIX.  THE STARTING AND ENDING OF THE VECTORS ARE      *
C DEFINED BY:                                                          *
C     X1=X+DX1, Y1=Y+DY1,                                              *
C     X2=X+DX2, Y2=Y+DY2.                                              *
C X2,Y2 BECOME THE NEW X1,Y1 UNLESS DX=0 IN WHICH CASE THE VECTOR IS   *
C STARTED ALL OVER WITH DY BECOMING THE NEXT DX.  IF DY=0, THE NEXT DX *
C BECOMES DY WITH RESPECT TO A Y SHIFTED DOWNWARD BY AN AMOUNT OF 1/4  *
C OF THE CHARACTER SIZE.  THE CHARACTER ORIGIN IS THE LOWER-LEFT COR-  *
C NER X,Y (THE PARAMETERS IX,IY).  ISIZE IS THE CHARACTER SIZE AS A    *
C MULTIPLICATIVE FACTOR OF THE DX,DY'S OF THE COORDINATE MATRIX.       *
C                                                                      *
C     MODIFIED BY HANS GOEDBLOED 18/10/85: REPLACING OCTAL CONVERSIONS *
C     BY MACHINE-INDEPENDENT CHARACTER AND INTEGER MANIPULATIONS.      *
C     MODIFIED BY GUIDO HUYSMANS 2/8/91: INTEGER COORDINATE TABLE OF   *
C     ASCII CHARACTERS ADAPTED TO WORD LENGTHS OF 32 BIT MACHINES.     *
C     MODIFIED BY HANS GOEDBLOED 11/11/91: EXTENDING CHARACTER TABLE   *
C     TO CORRESPOND TO THE SYMBOL FONT OF DLCH2.                       *
C     MODIFIED BY ELISABETH SCHWARZ 4/12/91: FINISHED CHARACTER TABLE. *
C***********************************************************************
C
      use itm_types
      implicit none
      integer IX,IY,NC,ISIZE
      COMMON /NEBDASC/NEA(64:255)
C
      CHARACTER*(*) STRING
      INTEGER TABLE(3,2,32:126)
      LOGICAL FVERT,FFONT,FSING
      real (r8) zisx,zisy
      integer nea,l,n,i,isx,isy,nfont,ic,nchr,jsize,mx,my,myd,
     &     isxold,m,icw,isya,j,istart,idx,ix1,idy,iy1

C
C     * TABLE COORDINATES ASCII CHARACTERS (HEBREW STYLE).
C
C      32. 33. 34. 35. 36. 37. 38. 39.
C           !   "   #   $   %   &   '
      DATA (((TABLE(L,N,I),L=1,3),N=1,1),I=32,39)
     2  /   0,00000000,00000000,
     3    222,31312220,72427372,
     4      7,57454750,72715272,
     5    353,10555101,47401272,
     6      7,31302124,35516265,
     7     15,24130736,27101175,
     8     35,13122131,64735115,
     9      0,00000000,53737453 /
C      40. 41. 42. 43. 44. 45. 46. 47.
C       (   )   *   +   ,   -   .   /
      DATA (((TABLE(L,N,I),L=1,3),N=1,1),I=40,47)
     *  /   0,00000000,74523214,
     1      0,00000000,72543412,
     2      0,00414506,22402264,
     3      0,00000004,14506323,
     4      0,00000000,13333413,
     5      0,00000000,00004145,
     6      0,00000023,13142423,
     7      0,00000000,00001175 /
C      48. 49. 50. 51. 52. 53. 54. 55.
C       0   1   2   3   4   5   6   7
      DATA (((TABLE(L,N,I),L=1,3),N=1,1),I=48,55)
     8  /   0,00417274,45141241,
     9      0,00000627,31301114,
     *      0,00617274,65211115,
     1    717,46544251,41104244,
     2      0,00000733,13501474,
     3      0,21121435,54517275,
     4      0,41443514,12417274,
     5      0,00000000,00717513 /
C      56. 57. 58. 59. 60. 61. 62. 63.
C       8   9   :   ;   <   =   >   ?
      DATA (((TABLE(L,N,I),L=1,3),N=1,1),I=56,63)
     6  /  74,55311214,35517274,
     7      0,12144574,72514245,
     8      2,43334240,64635464,
     9    133,43313063,53546463,
     *      0,00000000,00654125,
     1      0,00000003,53105551,
     2      0,00000000,00614521,
     3      1,32303355,65747261 /
C
C      64. 65. 66. 67. 68. 69. 70. 71.  192.193.194.195.196.197.198.199.
C       @   A   B   C   D   E   F   G    LE  AE  GE  CD DEL  IE PHI GAM
      DATA (((TABLE(L,N,I),L=1,3),N=1,2),I=64,71)
     4  /  34,43423113,35557351,             0,00000755,13501531,
     5      0,00000117,31503432,             4,53442310,65546251,
     6     41,44657471,11142544,             0,00000715,53101135,
     7      0,25141221,61727465,             0,72744514,12314245,
     8      0,00007174,45141171,             0,00000000,53151153,
     9      0,00075711,11504441,             0,00252104,54106561,
     *      0,00000757,11104144,       1373051,31222435,55646251,
     1   3335,25141221,61727465,             0,00000657,57107212 /
C      72. 73. 74. 75. 76. 77. 78. 79.  200.201.202.203.204.205.206.207.
C       H   I   J   K   L   M   N   O   DIA INT the DIA LAM MIN NAB DOT
      DATA (((TABLE(L,N,I),L=1,3),N=1,2),I=72,79)
     2  /   0,00711104,14507515,             0,00000041,23456341,
     3      0,00727407,31301214,             0,00000000,74632312,
     4      0,07375074,24131221,          4544,53736424,13225241,
     5      0,00711103,17504215,          4541,23456341,33455341,
     6      0,00000000,00711115,             0,00000000,00117315,
     7      0,00000011,71337515,             0,00000000,00004541,
     8      0,00000000,11711575,             0,00000000,51551351,
     9     72,74652514,12216172,        425452,44423344,54635242 /
C      80. 81. 82. 83. 84. 85. 86. 87.  208.209.210.211.212.213.214.215.
C       P   Q   R   S   T   U   V   W    PI THE SRT SIG PER  NE  PM OME
      DATA (((TABLE(L,N,I),L=1,3),N=1,2),I=80,87)
     *  /   0,00001171,74654441,            15,14740111,27207571,
     1    331,50141241,72744514,       4442051,31121435,55747251,
     2      1,17174654,44104315,             0,00000000,75134231,
     3      0,21121425,61727465,             0,00657571,44111525,
     4      0,00000007,17507313,             0,00000001,51101373,
     5      0,00007121,12142575,             0,00642203,53105551,
     6      0,00000000,00711375,             0,00414506,32302125,
     7      0,00000071,12531475,          1112,32417274,45341415 /
C      88. 89. 90. 91. 92. 93. 94. 95.  216.217.218.219.220.221.222.223.
C       X   Y   Z   [   \   ]   ^   _   KSI PSI INF ARL ARD ARR ARU BAR
      DATA (((TABLE(L,N,I),L=1,3),N=1,2),I=88,95)
     8  /   0,00000007,11501175,      34540444,20325207,57101511,
     9      0,00000714,31304375,            73,13014120,75454171,
     *      0,00000000,71751115,            41,31225445,35245241,
     1      0,00000000,74721214,             0,00000223,14203531,
     2      0,00000000,00002561,             0,00000342,33206323,
     3      0,00000000,72741412,             0,00000243,54403531,
     4      0,00000000,00547352,             0,00000546,35206323,
     5      0,00000000,00001511,             0,00000000,00003531 /
C
C      96. 97. 98. 99.100.101.102.103.  224.225.226.227.228.229.230.231.
C       `   a   b   c   d   e   f   g   ovs alf bet chi del eps phi gam
      DATA (((TABLE(L,N,I),L=1,3),N=1,2),I=96,103)
     6  /   0,00000000,73545373,             0,00000000,00007571,
     7   3532,21121425,15555241,             0,00005512,21415215,
     8      0,00007111,14355451,             1,16365434,20433513,
     9      0,00000055,52311215,             0,00000515,21501155,
     *      0,00007515,12315255,             0,75726125,14122143,
     1      0,00313554,52311215,             0,05552311,21503531,
     2     12,14052540,65746313,          1027,40543514,12315254,
     3    121,42575725,13234405,             0,00516224,13225465 /
C     104.105.106.107.108.109.110.111.  232.233.234.235.236.237.238.239.
C       h   i   j   k   l   m   n   o   eta iot phi kap lam  mu  nu omi
      DATA (((TABLE(L,N,I),L=1,3),N=1,2),I=104,111)
     4  /   0,00015455,45101171,          1524,65747362,03172701,
     5      0,12140737,37305313,             0,00000000,00431213,
     6      0,12132470,40747474,          1027,40543514,12315254,
     7      0,00000711,10143154,             0,00511103,15503115,
     8      0,00000727,31301214,             0,00000007,11504311,
     9    154,55453130,43525111,           443,34203544,75073101,
     *      0,01545545,24101151,             0,00000041,42122444,
     1      0,00543514,12315254,             0,00534121,13254553 /
C     112.113.114.115.116.117.118.119.  240.241.242.243.244.245.246.247.
C       p   q   r   s   t   u   v   w    pi the rho sig tau ups omb ome
      DATA (((TABLE(L,N,I),L=1,3),N=1,2),I=112,119)
     2  /   0,00031345,57471101,            41,52445504,41401252,
     3      0,00035325,17275105,             0,41727445,14124145,
     4      0,00045545,24101151,             0,04233456,57463101,
     5      0,00001114,25415255,             0,43352513,11415255,
     6      0,00000732,31405452,             0,00053231,41505551,
     7      0,00005525,14122151,             0,00000041,42121444,
     8      0,00000000,00511355,           454,10423112,33143544,
     9      0,00000051,12531455,             0,00423112,33143544 /
C     120.121.122.123.124.125.126.      248.249.250.251.252.253.254.
C       x   y   z   {   |   }   ~       ksi psi zet
      DATA (((TABLE(L,N,I),L=1,3),N=1,2),I=120,126)
     *  /   0,00000005,11501155,             6,56142440,31212515,
     1   1214,25750453,43241701,             0,00074120,52313455,
     2      0,00000000,51551115,          1420,51412216,20656271,
     3      0,00141333,42537374,             0,00000000,00000000,
     4      0,00000000,00001373,             0,00000000,00000000,
     5      0,00121333,44537372,             0,00000000,00000000,
     6      0,00000000,45345241,             0,00000000,00000000 /
C
C     * NC=0 MAY BE USED TO SWITCH OFF PRINTING OF STRING.
      IF(NC.EQ.0) RETURN
C
C     * FLAG FOR ROTATE (DRAW VERTICAL CHARACTERS).
      FVERT = .FALSE.
C
C     * CHECK FOR IX AND IY WITHIN THE RANGES 0-1023 AND 0-779.
      ISX = MIN(IABS(IX),1023)
      ISY = MIN(IABS(IY),779)
C     * IF IX<0, CONTINUE PRINTING AT PREVIOUS LOCATION.
      IF(IX.LT.0) THEN
        CALL SEELOC1(ZISX,ZISY)
        ISX=INT(ZISX)
        ISY=INT(ZISY)
      ENDIF
C
   10 CONTINUE
C
C     * FLAG FOR INTERPRET '$' AS CHANGE THE FONT.
      FFONT = .FALSE.
      IF(NC.LT.0) FFONT = .TRUE.
C     * DEFAULT FOR CHARACTER FONT.
      NFONT = 1
C
C     * FLAG FOR SINGLE CHARACTER.
      FSING = .FALSE.
      IF((LEN(STRING).EQ.1).AND.(NC.NE.1)) THEN
         IC = NC
         IF(IC.LT.32.OR.(126.LT.IC.AND.IC.LT.192).OR.IC.GT.254) RETURN
         FSING = .TRUE.
         NCHR = 1
      ELSE
C        * MAXIMUM FOR NC IS 80 CHARACTERS.
         NCHR = MIN(IABS(NC),80)
      ENDIF
C
C     * SET UP THE SPACING FOR 5 BY 7 CHARACTER MATRIX.
      JSIZE = IABS(ISIZE)
      IF(JSIZE.EQ.1) THEN
         MX = 10
         MY = 13
         MYD = 3
      ELSE
         MX = JSIZE*6
         MY = JSIZE*8
         MYD = JSIZE*2
      ENDIF
C
C     * SAVE ISX FOR LINE OVERFLOW.
      ISXOLD = ISX
C     * IF IY < 0, CENTER FIRST CHARACTER OF STRING AT IX,IY.
      IF(IY.LT.0) THEN
         ISX = ISX-MX/2
         ISY = ISY-MY/2
         IF(FVERT) ISY = ISY+MY
         IF(FSING) THEN
C           * ADDITIONAL POSITION CORRECTIONS FOR SINGLE CENTERED DOT
C           * (ONLY PRECISE IF ISIZE EVEN) AND LOWER CASE CHARACTERS.
            IF(IC.EQ.46) THEN
               ISX = ISX-MX/12
               ISY = ISY+5*MY/16
               IF(FVERT) ISY = ISY-5*MY/8
            ELSEIF((96.LE.IC.AND.IC.LE.126).OR.
     >            (224.LE.IC.AND.IC.LE.254)) THEN
               ISY = ISY+MY/8
               IF(FVERT) ISY = ISY-MY/4
            ENDIF
         ENDIF
      ENDIF
C
C     * GO THROUGH STRING PICKING OFF CHARACTERS ONE BY ONE.
C
      M = 0
      DO 60 N=1,NCHR
C
   20    M = M+1
         IF(FFONT.AND.(STRING(M:M).EQ.'$')) THEN
C           * THE FONT IS CHANGED.
            NFONT =  -NFONT+3
            GOTO 20
         ENDIF
         IF(FSING) THEN
            IF(IC.GE.192) THEN
               IC = IC-128
               NFONT = 2
            ENDIF
         ELSE
C           * GET ASCII CHARACTER VALUE.
            IC = ICHAR(STRING(M:M))
CMS         IC = NEA(IC)
            IF(NFONT.EQ.2.AND.IC.LT.64) RETURN
         ENDIF
C
C        * POSITION CORRECTION FOR OVERSTRIKE SYMBOL.
         IF(NFONT.EQ.2.AND.IC.EQ.96) ISX = ISX+MX
C
C        * CONTINUE PRINTING ON THE NEXT LINE IF STRING IS TOO LONG.
         IF(((ISX+MX.GE.1023).AND.(.NOT.FVERT)).OR.
     A      ((ISX+MX.GE. 779).AND.(     FVERT))) THEN
            ISX = ISXOLD
            IF(.NOT.FVERT) ISY = ISY-(MY+2*JSIZE)
            IF(FVERT)      ISY = ISY+(MY+2*JSIZE)
         ENDIF
C
C        * PICK UP CONTROL WORD FROM TABLE FOR DRAWING THE CHARACTER
C        * WITH VECTORS.
C
         ICW = TABLE(3,NFONT,IC)
         ISYA = ISY
         J = 1
         ISTART = 1
C
C        * SOME CHARACTERS TAKE MORE OR LESS STROKES.
C        * PICK UP ONE DIGIT AT A TIME FROM ICW UNTIL ICW=0.
C
   30    IF(J.EQ.9)  ICW = TABLE(2,NFONT,IC)
         IF(J.EQ.17) ICW = TABLE(1,NFONT,IC)
         IF((ICW.EQ.0).AND.(J.NE.8).AND.(J.NE.16)) GOTO 50
         IDX = MOD(ICW,10)
         ICW = ICW/10
         J = J+1
         IF(IDX.EQ.0) THEN
C           * START ANEW ON "0" IN DX LOCATION.
            ISTART = 1
            GOTO 30
         ENDIF
         IF(JSIZE.EQ.1) THEN
            IX1 = IDX/2+IDX
         ELSE
            IX1 = IDX*JSIZE
         ENDIF
   40    IF(J.EQ.9)  ICW = TABLE(2,NFONT,IC)
         IF(J.EQ.17) ICW = TABLE(1,NFONT,IC)
         IDY = MOD(ICW,10)
         ICW = ICW/10
         J = J+1
         IF(IDY.EQ.0) THEN
C           * DOWNWARD DISPLACEMENT ON "0" IN DY LOCATION FOR CHARACTERS
C           * LIKE L.C. Y AND G.
            ISYA = ISYA-MYD
            IF(FVERT) ISYA = ISYA+2*MYD
            GOTO 40
         ENDIF
         IF(JSIZE.EQ.1) THEN
            IY1 = IDY/2+IDY
         ELSE
            IY1 = IDY*JSIZE
         ENDIF
C
         IF(ISTART.EQ.1) THEN
C           * POSITION THE BEAM.
            IF(FVERT) THEN
C              * ROTATE FOR VERTICAL CHARACTERS.
               CALL MOVABS1(REAL(ISYA-IY1,R8),REAL(ISX+IX1,R8))
            ELSE
               CALL MOVABS1(REAL(ISX+IX1,R8),REAL(ISYA+IY1,R8))
            ENDIF
            ISTART = 0
         ELSE
C           * DRAW VECTOR.
            IF(FVERT) THEN
C              * ROTATE FOR VERTICAL CHARACTERS.
               CALL DRWABS1(REAL(ISYA-IY1,R8),REAL(ISX+IX1,R8))
            ELSE
               CALL DRWABS1(REAL(ISX+IX1,R8),REAL(ISYA+IY1,R8))
            ENDIF
         ENDIF
         GOTO 30
C
C        * POSITION FOR NEXT CHARACTER.
   50    IF(.NOT.(NFONT.EQ.2.AND.IC.EQ.96)) ISX = ISX+MX
C
   60 CONTINUE
C
C     * POST BEAM POSITION.
      IF(.NOT.FVERT) CALL MOVABS1(REAL(ISX,R8),REAL(ISY,R8))
      IF(FVERT) CALL MOVABS1(REAL(ISY,R8),REAL(ISX,R8))
      RETURN
C
C     * ENTRY FOR DRAWING VERTICALLY.
      ENTRY DLCV1(IX,IY,STRING,NC,ISIZE)
      IF(NC.EQ.0) RETURN
      FVERT = .TRUE.
      ISX = MIN(IABS(IY),779)
      ISY = MIN(IABS(IX),1023)
      IF(IX.LT.0) THEN
        CALL SEELOC1(ZISY,ZISX)
        ISX = INT(ZISX)
        ISY = INT(ZISY)
      ENDIF
      GOTO 10
      END
C
      SUBROUTINE DLCH2(IX,IY,STRING,NC,ISIZE)
C
C***********************************************************************
C     POST-SCRIPT VERSION, EXPLOITING THE LASERWRITER FONT HELVETICA   *
C (NFONT=1) AND AN ADAPTED VERSION OF THE SYMBOL FONT (NFONT=2).       *
C FOR ISIZE>0, THE CHARACTER SPACING HAS BEEN MODIFIED TO CONSTANT     *
C PITCH (TYPEWRITER STYLE).  FOR ISIZE<0, THE ORIGINAL PROPORTIONAL    *
C FONTS ARE EXPLOITED.                                                 *
C                                                                      *
C     WRITTEN BY GUIDO HUYSMANS, EGBERT WESTERHOF, AND HANS GOEDBLOED  *
C                                                            11/11/91  *
C***********************************************************************
C
      use itm_types
      implicit none
      integer IX,IY,NC,ISIZE
      integer ips
      PARAMETER (IPS=51)
C
      COMMON /ADVPAGE/ADVP
      LOGICAL ADVP
C
      COMMON /NEBDASC/NEA(64:255)
      integer nea
      COMMON /NASCEBD/NAE(32:126)
      integer nae
C
      CHARACTER*(*) STRING
      CHARACTER CHR*1, STROUT*81, FORM*17
      CHARACTER*4 OCT, SYMB(192:254)
      CHARACTER*1 BS1
      LOGICAL FVERT, FFONT, FSING, FCHANGE, FLINETL, FOCT
      real (r8) zisx,zisy
      integer nfont,magn,ic,isx,isy,jsize,mx,my,mx1,nchr,
     &     isxold,isyold,num,m,ilen,n
C
      SAVE NFONT, MAGN, BS1
C
      DATA NFONT, MAGN / 0, 0/
C
C     BACKSLASH ON IBM, THIS IS A ONE CHARARCTER VARIABLE. IT
      DATA BS1 /'\\'/
C
C     * OCTAL VALUES FOR SYMBOL FONT CHARACTERS.
C
      DATA (SYMB(IC),IC=192,223)
C    IC = 192     193     194     195     196     197     198     199
     >/'\\243','\\273','\\263','\\266','\\104','\\272','\\106','\\107',
C    IC = 200     201     202     203     204     205     206     207
     > '\\340','\\362','\\112','\\250','\\114','\\055','\\321','\\267',
C    IC = 208     209     210     211     212     213     214     215
     > '\\120','\\121','\\326','\\123','\\136','\\271','\\261','\\127',
C    IC = 216     217     218     219     220     221     222     223
     > '\\130','\\131','\\245','\\254','\\257','\\256','\\255','\\276'/
C
      DATA (SYMB(IC),IC=224,254)
C    IC = 224     225     226     227     228     229     230     231
     >/'\\040','\\141','\\142','\\143','\\144','\\145','\\146','\\147',
C    IC = 232     233     234     235     236     237     238     239
     > '\\150','\\151','\\152','\\153','\\154','\\155','\\156','\\157',
C    IC = 240     241     242     243     244     245     246     247
     > '\\160','\\161','\\162','\\163','\\164','\\165','\\166','\\167',
C    IC = 248     249     250     251     252     253     254
     > '\\170','\\171','\\172','\   ','\   ','\   ','\   ' /
C
C     * NC=0 MAY BE USED TO SWITCH OFF PRINTING OF STRING.
      IF(NC.EQ.0) RETURN
C
C     * FLAG FOR ROTATE (DRAW VERTICAL CHARACTERS).
      FVERT = .FALSE.
C
C     * CHECK FOR IX AND IY WITHIN THE RANGES 0-1023 AND 0-779.
      ISX = MIN(IABS(IX),1023)
      ISY = MIN(IABS(IY),779)
C     * IF IX<0, CONTINUE PRINTING AT PREVIOUS LOCATION.
      IF(IX.LT.0) THEN
        CALL SEELOC2(ZISX,ZISY)
        ISX = INT(ZISX)
        ISY = INT(ZISY)
      ENDIF
C
   10 CONTINUE
C
      FOCT = .FALSE.
      JSIZE = IABS(ISIZE)
      IF(JSIZE.EQ.1) THEN
         MX = 9
         MY = 13
      ELSE
         MX = JSIZE*6
         MY = JSIZE*8.5_R8
      ENDIF
      MX1 = MX
      IF (ISIZE.LT.0) MX1 = MX / 2
C
      IF((MAGN.NE.MY).OR.(NFONT.NE.1).OR.(ADVP)) THEN
         MAGN = MY
         NFONT = 1
         WRITE(IPS,'(I4,A5)') MY,' scaH'
         ADVP = .FALSE.
      ENDIF
      FCHANGE = .FALSE.
      FFONT = .FALSE.
      IF(NC.LT.0) FFONT = .TRUE.
C
C     * FLAG FOR SINGLE CHARACTER.
      FSING = .FALSE.
      IF((LEN(STRING).EQ.1).AND.(NC.NE.1)) THEN
         IC = NC
         IF(IC.LT.32.OR.(126.LT.IC.AND.IC.LT.192).OR.IC.GT.254) RETURN
         FSING = .TRUE.
         NCHR = 1
      ELSE
C        * MAXIMUM FOR NC IS 80 CHARACTERS.
         NCHR = MIN(IABS(NC),80)
      ENDIF
C
C     * SAVE ISX AND ISY FOR LINE OVERFLOW.
      ISXOLD = ISX
      ISYOLD = ISY
      IF(.NOT.FVERT) WRITE(IPS,'(I4,1X,I4,A2)') ISX,ISY,' m'
      IF(FVERT) WRITE(IPS,'(A9,I4,1X,I4,A21,I1,1X,I1,A2)')
     >        'st gsave ',ISY,ISX,' translate 90 rotate ',0,0,' m'
C
C     * DRAWING SINGLE CHARACTERS.
C
      IF(FSING) THEN
         IF(192.LE.IC.AND.IC.LE.254) THEN
            NFONT = 2
            WRITE(IPS,'(I4,A5)') MY,' scaS'
            FOCT = .FALSE.
            IF(IC.LE.223.OR.IC.GT.250) THEN
               OCT = SYMB(IC)
               FOCT = .TRUE.
            ENDIF
         ENDIF
         IF(IC.EQ.46.AND.IY.LT.0) THEN
C           * SIZE CORRECTION FOR SINGLE CENTERED DOT.
            WRITE(IPS,'(I4,A5)') MY+8,' scaH'
            MAGN = MY+8
         ENDIF
         IF(IC.LE.126.OR.IC.GE.224) THEN
            IF(IC.GE.224) IC = IC-128
CMS         IC = NAE(IC)
            CHR = CHAR(IC)
         ENDIF
         IF(IY.LT.0) THEN
            IF(FOCT) THEN
               WRITE(IPS,'(I4,1X,I7,A10)') ISX,ISY,' ('//OCT//') tc'
            ELSEIF(CHR.EQ.'('.OR.CHR.EQ.')'.OR.CHR.EQ.BS1) THEN
            WRITE(IPS,'(I4,1X,I4,A2,2A1,A4)')ISX,ISY,' (',BS1,CHR,') tc'
            ELSE
               WRITE(IPS,'(I4,1X,I4,A7)')  ISX,ISY,' ('//CHR//') tc'
            ENDIF
         ELSE
            IF(FOCT) THEN
               WRITE(IPS,'(I4,A10)') MX,' ('//OCT//') tw'
            ELSEIF(CHR.EQ.'('.OR.CHR.EQ.')'.OR.CHR.EQ.BS1) THEN
               WRITE(IPS,'(I4,A2,2A1,A4)')  MX,' (',BS1,CHR,') tw'
            ELSE
               WRITE(IPS,'(I4,A7)')  MX,' ('//CHR//') tw'
            ENDIF
         ENDIF
         FOCT = .FALSE.
         GOTO 50
      ENDIF
C
C     * DRAWING STRING OF CHARACTERS.
C
      NUM = 0
      M = 1
      FLINETL = .FALSE.
C
   20 CONTINUE
C
         STROUT = 'X'
         ILEN = 0
         N = 0
C
C        * PROCESSING PART OF STRING WITH THE SAME FONT.
   30    CONTINUE
            IF(NUM.GE.NCHR) GOTO 40
            IF(((ISX+(N+1)*MX1.GE.1100).AND.(.NOT.FVERT)).OR.
     >         ((ISX+(N+1)*MX.GE. 779).AND.(     FVERT))) THEN
               IF(NUM.EQ.0) RETURN
               FLINETL = .TRUE.
               GOTO 40
            ENDIF
            IF(FFONT.AND.(STRING(M:M).EQ.'$')) THEN
               M = M+1
               FCHANGE = .TRUE.
               GOTO 40
            ENDIF
            CHR = STRING(M:M)
            IF(NFONT.EQ.2.AND.CHR.NE.' ') THEN
               IC = ICHAR(CHR)
CMS            IC = NEA(IC)
               IF(IC.LT.64) RETURN
               FOCT = .FALSE.
               IF(IC.LE.95.OR.IC.GT.122) THEN
                  OCT = SYMB(IC+128)
                  FOCT = .TRUE.
               ENDIF
            ENDIF
            IF(CHR.EQ.'('.OR.CHR.EQ.')'.OR.CHR.EQ.BS1) THEN
C              * INTERCEPT SPECIAL POSTSCRIPT CHARACTERS.
               STROUT = STROUT(1:ILEN+1)//BS1//CHR
               ILEN = ILEN+2
            ELSEIF(FOCT) THEN
C              * INTERCEPT SPECIAL SYMBOLS.
               STROUT = STROUT(1:ILEN+1)//OCT
               ILEN = ILEN+4
               FOCT = .FALSE.
            ELSE
               STROUT = STROUT(1:ILEN+1)//CHR
               ILEN = ILEN+1
            ENDIF
            M = M+1
            NUM = NUM+1
            N = N+1
         GOTO 30
C
C        * WRITING PART OF STRING WITH THE SAME FONT.
   40    IF(ILEN.NE.0) THEN
            IF(ISIZE.GT.0) THEN
               IF(ILEN.LE.70) THEN
                  WRITE(FORM,'(A8,I3,A4)') '(I4,A2,A',ILEN,',A4)'
                  WRITE(IPS,FORM) MX,' (',STROUT(2:ILEN+1),') tw'
               ELSE
                  WRITE(IPS,'(I4,A2,A70,A4)')
     >                            MX,' (',STROUT(2:71),') tw'
                  WRITE(FORM,'(A8,I3,A4)') '(I4,A2,A',ILEN-70,',A4)'
                  WRITE(IPS,FORM) MX,' (',STROUT(72:ILEN+1),') tw'
               ENDIF
            ELSE
               IF(ILEN.LE.70) THEN
                  WRITE(FORM,'(A5,I3,A4)') '(A1,A',ILEN,',A4)'
                  WRITE(IPS,FORM) '(',STROUT(2:ILEN+1),') sh'
               ELSE
                  WRITE(IPS,'(A1,A70,A4)')
     >                            '(',STROUT(2:71),') sh'
                  WRITE(FORM,'(A5,I3,A4)') '(A1,A',ILEN-70,',A4)'
                  WRITE(IPS,FORM) '(',STROUT(72:ILEN+1),') sh'
               ENDIF
            ENDIF
         ENDIF
         ISX = ISX+ILEN*MX
         IF(FLINETL) THEN
            ISX = ISXOLD
            IF(.NOT.FVERT) ISY = ISY-(MY+2*JSIZE)
            IF(FVERT)      ISY = ISY+(MY+2*JSIZE)
            IF(ISY.LT.0) RETURN
            IF(.NOT.FVERT) WRITE(IPS,'(I4,1X,I4,A2)') ISX,ISY,' m'
            IF(FVERT)      WRITE(IPS,'(I4,1X,I4,A2)') 0,ISYOLD-ISY,' m'
            FLINETL = .FALSE.
         ENDIF
         IF(FCHANGE) THEN
            NFONT = -NFONT+3
            IF(NFONT.EQ.1) WRITE(IPS,'(I4,A5)') MY,' scaH'
            IF(NFONT.EQ.2) WRITE(IPS,'(I4,A5)') MY,' scaS'
            FCHANGE = .FALSE.
         ENDIF
C
      IF(NUM.LT.NCHR) GOTO 20
C
C     * POST BEAM POSITION.
   50 IF(.NOT.FVERT) CALL MOVABS2(REAL(ISX,R8),REAL(ISY,R8))
      IF(FVERT) WRITE(IPS,'(A8)') 'grestore'
      IF(FVERT) CALL MOVABS2(REAL(ISY,R8),REAL(ISX,R8))
      RETURN
C
C     * ENTRY FOR DRAWING VERTICALLY.
      ENTRY DLCV2(IX,IY,STRING,NC,ISIZE)
      IF(NC.EQ.0) RETURN
      FVERT = .TRUE.
      ISX = MIN(IABS(IY),779)
      ISY = MIN(IABS(IX),1023)
      IF(IX.LT.0) THEN
        CALL SEELOC2(ZISY,ZISX)
        ISX = INT(ZISX)
        ISY = INT(ZISY)
      ENDIF
      GOTO 10
      END
C
      SUBROUTINE BEGPLT(NAME)
C
C***********************************************************************
C     BRANCHING TO BEGPLT1 (CALCOMP) / BEGPLT2 (POSTSCRIPT).           *
C***********************************************************************
C
      use itm_types
      implicit none
      COMMON /LCLPS/LCAL,LPOS
      integer lcal,lpos
      CHARACTER*(*) NAME
C
      IF(LCAL.EQ.1) CALL BEGPLT1(NAME)
      IF(LPOS.EQ.1) CALL BEGPLT2(NAME)
      RETURN
      END
C
      SUBROUTINE BEGPLT1(NAME)
C
C***********************************************************************
C     BEGPLT INITIALIZES THE PLOTTING ROUTINES; IT MUST BE CALLED      *
C BEFORE ANY PLOTTING IS DONE.  THE PLOTTING FRAME COORDINATES ARE SET *
C TO 1024 BY 780.  THESE COORDINATES ARE CHOSEN SO THAT THE OUTPUT IS  *
C COMPATIBLE WITH THE 4014 TEKTRONIX SCREEN, THE VERSATEK PLOTTER, AND *
C MICROFICHE.                                                          *
C                                                                      *
C     AT THE CRAY1 AT MFECC BEGPLT CREATES A FILE NAMED "F3_NAME_OX"   *
C IN THE USER'S LOCAL FILE SPACE.  E.G., IF NAME='ABCD', THE GRAPHICS  *
C IS WRITTEN ONTO "F3ABCDOX".  IF ADDITIONAL FILE SPACE IS NEEDED, NEW *
C GRAPHICS FILES ARE AUTOMATICALLY CREATED BY INCREMENTING THE FOURTH  *
C LETTER AS FOLLOWS: "F3ABCEOX", "F3ABCFOX", ETC.                      *
C                                                                      *
C     WRITTEN BY CLAIR NIELSON                                         *
C     MODIFIED HGO 23/10/85: ELIMINATED PARAMETERS TITLE AND NTITLE.   *
C***********************************************************************
C
      use itm_types
      implicit none
      COMMON /GRNR1/IGR
      integer igr
      COMMON /LIB8X1/ZIXSAV,ZIYSAV
      real (r8) zixsav,ziysav
      CHARACTER*(*) NAME
      IGR = 1
      ZIXSAV = 0._R8
      ZIYSAV = 0._R8
      RETURN
      END
C
      SUBROUTINE BEGPLT2(NAME)
C
C***********************************************************************
C     POSTSCRIPT VERSION: WRITE HEADER AND DEFINITIONS.                *
C***********************************************************************
C
      use itm_types
      implicit none
      integer ips
      PARAMETER (IPS=51)
C
      CHARACTER*(*) NAME
      COMMON /GRNR2/IGR
      integer igr
      COMMON /LIB8X2/ZIXSAV,ZIYSAV
      real (r8) zixsav,ziysav
C
      OPEN(IPS,FILE=NAME)
      WRITE(IPS,'(A/A9,A8/A/A/A/A/A/A/A)')
     > '%!PS-Adobe-2.0',
     > '%%Title: ',NAME,
     > '%%Creator: PPPLIB',
     > '%%Pages: (atend)',
     > '%%BoundingBox: 50 400 50 400',
     > '%%For: PPPLIB',
     > '%%EndComments',
     > '%%EndProlog',
     > '%%Begin Setup'
      WRITE(IPS,'(7(A/),A)')
c     > '.60 .60 scale 900 180 translate 90 rotate',
     > '.5 .5 scale 100 100 translate',
     > '.1 setlinewidth',
     > '/l {lineto} def /m {moveto} def /sf {setrgbcolor fill} def',
     > '/rl {rlineto} def /rm {rmoveto} def',
     > '/sh {show} def /st {stroke} def',
     > '/pt {l .4 setlinewidth st .1 setlinewidth} def',
     > '/scaH {/Helvetica findfont exch scalefont setfont} def',
     > '/scaS {/Symbol findfont exch scalefont setfont} def'
      WRITE(IPS,'(17(A/),A)')
     > '/tw  % typewrite (str) with dx=skip.',
     > ' {/str exch def /skip exch def',
     > '  str {/charcode exch def /char ( ) dup 0 charcode put def',
     > '   skip 2 div 0 rm gsave',
     > '   char stringwidth pop 2 div neg 0 rm',
     > '   char show grestore skip 2 div 0 rm} forall} def',
     > '/tc  % type centered character.',
     > ' {/ch exch def /y exch def /x exch def',
     > '  gsave newpath 0 0 m',
     > '  ch true charpath flattenpath pathbbox',
     > '  /ury exch def /urx exch def /lly exch def /llx exch def',
     > '  urx llx add 2 div /dx exch def',
     > '  ury lly add 2 div /dy exch def grestore',
     > '  x dx sub y dy sub m ch sh} def'
      WRITE(IPS,*) '/mx {/v1 exch def v1 1 gt {1} {v1} ifelse} def'
      WRITE(IPS,*) '/mi {/v1 exch def v1 0 lt {0} {v1} ifelse} def'
      WRITE(IPS,*) '/rgb2 {/cc exch def'
      WRITE(IPS,*) '2 cc 4 mul sub              /bb exch def    %blue'
      WRITE(IPS,*) '2 4 cc mul 2 sub abs sub    /gg exch def    %green'
      WRITE(IPS,*) 'cc 4 mul 2 sub              /rr exch def    %red'
      WRITE(IPS,*) 'rr gg bb setrgbcolor'
      WRITE(IPS,*) '} def'
      WRITE(IPS,*) '/rgb {/cc exch def 1 cc sub /dd exch def'
      WRITE(IPS,*) '1 6 dd mul 3 sub abs sub mx mi /r2 exch def'
      WRITE(IPS,*) '6 dd mul 5 sub mx mi           /r3 exch def'
      WRITE(IPS,*) '1 12 dd mul sub mx mi          /r4 exch def'
      WRITE(IPS,*) 'dd 4 mul 2 sub mx mi r2 add    /bb exch def'
      WRITE(IPS,*) '2 4 dd mul 2 sub abs sub       /gg exch def'
      WRITE(IPS,*) '2 dd 4 mul sub mx mi '
     >             //'r2 add r3 add r4 sub /rr exch def'
      WRITE(IPS,*) 'rr gg bb setrgbcolor'
      WRITE(IPS,*) '} def'
      WRITE(IPS,*) '/tri { add add 3 div rgb'
      WRITE(IPS,*) ' moveto lineto lineto closepath fill '
      WRITE(IPS,*) '} def'
      WRITE(IPS,*) '0 /nql exch def'
      WRITE(IPS,*) '/ftr {/nq exch def'
      WRITE(IPS,*) '%recursive,lowest level,fill triangle with average'
      WRITE(IPS,*) 'nq 0 eq  { tri } if'
      WRITE(IPS,*) '% next level'
      WRITE(IPS,*) 'nq 0 gt {'
      WRITE(IPS,*) 'nq 1 sub /nq exch def'
      WRITE(IPS,*) '/c3 exch def /c2 exch def /c1 exch def'
      WRITE(IPS,*) '/y3 exch def /x3 exch def'
      WRITE(IPS,*) '/y2 exch def /x2 exch def'
      WRITE(IPS,*) '/y1 exch def /x1 exch def'
      WRITE(IPS,*) 'x1 x2 add 2 div /x12 exch def'
      WRITE(IPS,*) 'y1 y2 add 2 div /y12 exch def'
      WRITE(IPS,*) 'c1 c2 add 2 div /c12 exch def'
      WRITE(IPS,*) 'x1 x3 add 2 div /x13 exch def'
      WRITE(IPS,*) 'y1 y3 add 2 div /y13 exch def'
      WRITE(IPS,*) 'c1 c3 add 2 div /c13 exch def'
      WRITE(IPS,*) 'x2 x3 add 2 div /x23 exch def'
      WRITE(IPS,*) 'y2 y3 add 2 div /y23 exch def'
      WRITE(IPS,*) 'c2 c3 add 2 div /c23 exch def'
      WRITE(IPS,*) 'x1  y1     x12 y12    x13 y13   c1  c12 c13  nq'
      WRITE(IPS,*) 'x3  y3     x13 y13    x23 y23   c3  c13 c23  nq'
      WRITE(IPS,*) 'x2  y2     x12 y12    x23 y23   c2  c12 c23  nq'
      WRITE(IPS,*) 'x12 y12    x13 y13    x23 y23   c12 c13 c23  nq'
      WRITE(IPS,*) 'ftr ftr ftr ftr '
      WRITE(IPS,*) '} if'
      WRITE(IPS,*) '} def'
c      WRITE(IPS,*) '%%Title: GradFill.ps'
c      WRITE(IPS,*) '% Copyright (C) 1993, Carl W. Orthlieb, Adobe '
c      WRITE(IPS,*) '% Systems Incorporated. Permission to use and '
c      WRITE(IPS,*) '% modify this software and its documentation for '
c      WRITE(IPS,*) '% any purpose is hereby granted without fee'
c      WRITE(IPS,*) '% provided, however, that the above copyright  '
c      WRITE(IPS,*) '% notice appear in all copies, that both that '
c      WRITE(IPS,*) '% copyright notice and this permission notice '
c      WRITE(IPS,*) '% appear in supporting documentation. The author '
c      WRITE(IPS,*) '% makes no representations about the suitability '
c      WRITE(IPS,*) '% of this software for any purpose. It is provided'
c      WRITE(IPS,*) '% as is without express or implied warranty.'
c      WRITE(IPS,*) ' /setupgradfill {	% sr sg sb er eg eb steps angle'
c      WRITE(IPS,*) ' 	rotate'
c      WRITE(IPS,*) ' 	/numsteps exch def'
c      WRITE(IPS,*) ' 	% String to hold synthetic data'
c      WRITE(IPS,*) ' 	/gradstr numsteps 3 mul string def '
c      WRITE(IPS,*) ' 	3 index sub numsteps div /ib exch def'
c      WRITE(IPS,*) ' 	3 index sub numsteps div /ig exch def'
c      WRITE(IPS,*) ' 	3 index sub numsteps div /ir exch def'
c      WRITE(IPS,*) ' 	/sb exch def'
c      WRITE(IPS,*)  ' 	/sg exch def '
c      WRITE(IPS,*)  ' 	/sr exch def	'
c      WRITE(IPS,*)  ' 	%(SR SG SB: ) print sr == sb == sg =='
c      WRITE(IPS,*)  ' 	%(IR IG IB: ) print ir == ib == ig =='
c      WRITE(IPS,*)  ' 	% Create a synthetic data string'
c      WRITE(IPS,*)  ' 	0 1 numsteps 1 sub {	% loop'
c      WRITE(IPS,*)  ' 	 3 mul		% index'
c      WRITE(IPS,*)  ' 	 gradstr 1 index sr 255 mul round cvi put'
c      WRITE(IPS,*)  ' 	 gradstr 1 index 1 add sg 255 mul round cvi put'
c      WRITE(IPS,*)  ' 	 gradstr 1 index 2 add sb 255 mul round cvi put'
c      WRITE(IPS,*)  ' 		pop'
c      WRITE(IPS,*)  ' 		/sr sr ir add def'
c      WRITE(IPS,*)  ' 		/sg sg ig add def'
c      WRITE(IPS,*)  ' 		/sb sb ib add def'
c      WRITE(IPS,*)  ' 	} for'
c      WRITE(IPS,*)  ' } bind def'
c      WRITE(IPS,*)  ' /convbboxtoxywh {	% llx lly urx ury'
c      WRITE(IPS,*)  ' 	2 index sub exch 3 index sub exch'
c      WRITE(IPS,*)  ' } bind def'
c      WRITE(IPS,*)  ' /gradmatrix matrix def'
c      WRITE(IPS,*)  ' /gradfill {'
c      WRITE(IPS,*)  ' % startred startgreen startblue endred '
c      WRITE(IPS,*)  ' %          endgreen endblue steps angle'
c      WRITE(IPS,*)  ' 	save /mysave exch def'
c      WRITE(IPS,*)  ' 		setupgradfill'
c      WRITE(IPS,*)  ' 	pathbbox clip convbboxtoxywh	% x y w h'
c      WRITE(IPS,*)  ' 	0 0 3 -1 roll 6 -2 roll		% w 0 0 h x y'
c      WRITE(IPS,*)  ' 		gradmatrix astore concat'
c      WRITE(IPS,*)  ' 		numsteps 1 8 [numsteps 0 0 1 0 0]'
c      WRITE(IPS,*)  ' 		{'
c      WRITE(IPS,*)  ' 			gradstr'
c      WRITE(IPS,*)  ' 		}'
c      WRITE(IPS,*)  ' 		false 3 colorimage'
c      WRITE(IPS,*)  ' mysave restore'
c      WRITE(IPS,*)  ' } bind def'
      WRITE(IPS,'(A)')  '%%End Setup'
      WRITE(IPS,'(A)')  'newpath'
      WRITE(IPS,'(A)')  '%%Page: 1 1'
      WRITE(IPS,'(A)')  '%%start plotting'
C
      IGR = 1
      ZIXSAV = 0._R8
      ZIYSAV = 0._R8
      RETURN
      END
C
      SUBROUTINE BEGMOV(NAME,TITLE,NTITLE)
C
C***********************************************************************
C     THIS ROUTINE INITIALIZES THE PLOTTING ROUTINES FOR MAKING A      *
C MOVIE.  THE FRAME COORDINATES ARE INITIALIZED TO 1020 BY 1024. THESE *
C COORDINATES ARE CHOSEN SO THAT THE OUTPUT IS COMPATIBLE WITH 16-MM   *
C AND 35-MM FILM.  THE ARGUMENTS OF BEGMOV ARE THE SAME AS THOSE OF    *
C THE ORIGINAL VERSION OF BEGPLT WHERE "TITLE" WAS USED TO PRODUCE AN  *
C EYE-READABLE TITLE OF "NTITLE" CHARACTERS ON THE GRAPHICS FILE OR    *
C MICROFICHE.  THE USER MUST GIVE THE MOVIE FILE, "F6_NAME_0X", TO THE *
C SYSTEM FOR PROCESSING:  "GIVE F6_NAME_0X 999999 END".                *
C                                                                      *
C     WRITTEN BY DEBBY HYMAN 3-80                                      *
C     CALL HAS NO EFFECT AT SARA - 840106 GMDH                         *
C***********************************************************************
C
      use itm_types
      implicit none
      integer ntitle
      real (r8) title
      COMMON /GRNR1/IGR
      integer igr
      COMMON /LIB8X1/ZIXSAV,ZIYSAV
      real (r8) zixsav,ziysav
      CHARACTER*(*) NAME
      IGR = 0
      ZIXSAV = 0._R8
      ZIYSAV = 0._R8
      RETURN
      END
C
      SUBROUTINE FINPLT
C
C***********************************************************************
C     BRANCHING TO FINPLT1 (CALCOMP) / FINPLT2 (POSTSCRIPT).           *
C***********************************************************************
C
      use itm_types
      implicit none
      COMMON /LCLPS/LCAL,LPOS
      integer lcal,lpos
C
      IF(LCAL.EQ.1) CALL FINPLT1
      IF(LPOS.EQ.1) CALL FINPLT2
      RETURN
      END
C
      SUBROUTINE FINPLT1
C
C***********************************************************************
C     THIS ROUTINE IS CALLED AFTER ALL PLOTTING IN A CODE IS FINISHED. *
C IT FLUSHES THE BUFFERS AND CLOSES THE GRAPHICS FILES.                *
C***********************************************************************
C
C
      use itm_types
      implicit none
      RETURN
      END
C
      SUBROUTINE FINPLT2
C
C***********************************************************************
C     POSTSCRIPT VERSION.                                              *
C***********************************************************************
C
      use itm_types
      implicit none
      integer ips
      PARAMETER (IPS=51)
C
      COMMON /GRNR2/IGR
      integer igr
C
      WRITE(IPS,'(A/A)')
     > 'stroke gsave showpage grestore',
     > '%%Trailer'
      WRITE(IPS,'(A,I5)')  '%%Pages: ',IGR
      WRITE(IPS,'(A)')     '%%EOF'
      CLOSE(IPS)
C
      RETURN
      END
C
      SUBROUTINE ADV(N)
C
C***********************************************************************
C     BRANCHING TO ADV1 (CALCOMP) / ADV2 (POSTSCRIPT).                 *
C***********************************************************************
C
      use itm_types
      implicit none
      integer n
      COMMON /LCLPS/LCAL,LPOS
      integer lcal,lpos
C
      IF(LCAL.EQ.1) CALL ADV1(N)
      IF(LPOS.EQ.1) CALL ADV2(N)
      RETURN
      END
C
      SUBROUTINE ADV1(N)
C
C***********************************************************************
C     THIS ROUTINE ADVANCES N PLOTTING PAGES (ONLY SENSIBLE FOR N=1).  *
C***********************************************************************
C
      use itm_types
      implicit none
      integer n
      COMMON /GRNR1/IGR
      integer igr

      IGR = IGR+N
      RETURN
      END
C
      SUBROUTINE ADV2(N)
C
C***********************************************************************
C     POSTSCRIPT VERSION.                                              *
C***********************************************************************
C
      use itm_types
      implicit none
      integer n
      integer ips
      PARAMETER (IPS=51)
C
      COMMON /GRNR2/IGR
      integer igr
      COMMON /ADVPAGE/ADVP
      LOGICAL ADVP
      integer i
C
      DO 10 I=1,N
         IF(IGR.NE.0) WRITE(IPS,'(A30/A8,I3,I3)')
     >    'stroke gsave showpage grestore',
C    EEN TIJDELIJKE OPLOSSING OM TE ZORGEN DAT DE POSTSCRIPT INTERPRETER
C    WEET WELKE PAGINA DE KOMENDE IS. EIGENLIJK MOET ONDERSTAANDE REGEL
C    GEPLAATST WORDEN IN DE ROUTINE DIE HET BEGIN VAN EEN PAGINA AANMAAK
C    SEPT '93 SANDER BELIEN
     >    '%%Page: ',IGR+N,IGR+N
   10 CONTINUE
C
      IGR = IGR+N
      ADVP= .TRUE.
      RETURN
      END
C
      SUBROUTINE DRV(ZIX1,ZIY1,ZIX2,ZIY2)
C
C***********************************************************************
C     BRANCHING TO ADV1 (CALCOMP) / ADV2 (POSTSCRIPT).                 *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) zix1,ziy1,zix2,ziy2
      COMMON /LCLPS/LCAL,LPOS
      integer lcal,lpos
C
      IF(LCAL.EQ.1) CALL DRV1(ZIX1,ZIY1,ZIX2,ZIY2)
      IF(LPOS.EQ.1) CALL DRV2(ZIX1,ZIY1,ZIX2,ZIY2)
      RETURN
      END
C
      SUBROUTINE DRV1(ZIX1,ZIY1,ZIX2,ZIY2)
C
C***********************************************************************
C     THIS ROUTINE DRAWS A LINE VECTOR FROM (IX1,IY1) TO (IX2,IY2).    *
C***********************************************************************
C
C
      use itm_types
      implicit none
      real (r8) zix1,ziy1,zix2,ziy2
      COMMON /LIB8X1/ZIXSAV,ZIYSAV
      real (r8) zixsav,ziysav
C
      ZIXSAV = ZIX2
      ZIYSAV = ZIY2
      RETURN
      END
C
      SUBROUTINE DRV2(ZIX1,ZIY1,ZIX2,ZIY2)
C
C***********************************************************************
C     POSTSCRIPT VERSION.                                              *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) zix1,ziy1,zix2,ziy2
      integer ips
      PARAMETER (IPS=51)
C
      COMMON /LIB8X2/ZIXSAV,ZIYSAV
      real (r8) zixsav,ziysav
      integer numlin
C
      SAVE NUMLIN
      DATA NUMLIN / 0 /
C
      NUMLIN = NUMLIN+1
      IF(NUMLIN.GE.30) THEN
         WRITE(IPS,'(A2)') 'st'
         NUMLIN = 0
      ENDIF
      WRITE(IPS,'(F8.3,1X,F8.3,A3,F8.3,1X,F8.3,A2)')
     >             ZIX1,ZIY1,' m ',ZIX2,ZIY2,' l'
C
      ZIXSAV = ZIX2
      ZIYSAV = ZIY2
      RETURN
      END
C
      SUBROUTINE DRP(ZIX,ZIY)
C
C***********************************************************************
C     BRANCHING TO DRP1 (CALCOMP) / DRP2 (POSTSCRIPT).                 *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) zix,ziy
      COMMON /LCLPS/LCAL,LPOS
      integer lcal,lpos
C
      IF(LCAL.EQ.1) CALL DRP1(ZIX,ZIY)
      IF(LPOS.EQ.1) CALL DRP2(ZIX,ZIY)
      RETURN
      END
C
      SUBROUTINE DRP1(ZIX,ZIY)
C
C***********************************************************************
C     THIS ROUTINE DRAWS A POINT AT THE LOCATION (IX,IY).              *
C***********************************************************************
C
C
      use itm_types
      implicit none
      real (r8) zix,ziy
      COMMON /LIB8X1/ZIXSAV,ZIYSAV
      real (r8) zixsav,ziysav
C
      ZIXSAV = ZIX
      ZIYSAV = ZIY
      RETURN
      END
C
      SUBROUTINE DRP2(ZIX,ZIY)
C
C***********************************************************************
C     POSTSCRIPT VERSION.                                              *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) zix,ziy
      integer ips
      PARAMETER (IPS=51)
C
      COMMON /LIB8X2/ZIXSAV,ZIYSAV
      real (r8) zixsav,ziysav
C
      WRITE(IPS,'(a3,F8.3,1X,F8.3,A3,F8.3,1X,F8.3,A3)')
     >      'st ',ZIX+0.4,ZIY,' m ',ZIX,ZIY,' pt'
      ZIXSAV = ZIX
      ZIYSAV = ZIY
      RETURN
      END
C
      SUBROUTINE MOVABS(ZIX,ZIY)
C
C***********************************************************************
C     BRANCHING TO MOVABS1 (CALCOMP) / MOVABS2 (POSTSCRIPT).           *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) zix,ziy
      COMMON /LCLPS/LCAL,LPOS
      integer lcal,lpos
C
      IF(LCAL.EQ.1) CALL MOVABS1(ZIX,ZIY)
      IF(LPOS.EQ.1) CALL MOVABS2(ZIX,ZIY)
      RETURN
      END
C
      SUBROUTINE MOVABS1(ZIX,ZIY)
C
C***********************************************************************
C     THIS ROUTINE MOVES THE DRAWING BEAM TO THE LOCATION (IX,IY).     *
C***********************************************************************
C
C
      use itm_types
      implicit none
      real (r8) zix,ziy
      COMMON /LIB8X1/ZIXSAV,ZIYSAV
      real (r8) zixsav,ziysav
C
      ZIXSAV = ZIX
      ZIYSAV = ZIY
      RETURN
      END
C
      SUBROUTINE MOVABS2(ZIX,ZIY)
C
C***********************************************************************
C     POSTSCRIPT VERSION.                                              *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) zix,ziy
      integer ips
      PARAMETER (IPS=51)
C
      COMMON /LIB8X2/ZIXSAV,ZIYSAV
      real (r8) zixsav,ziysav
      integer numbin
C
      SAVE NUMBIN
      DATA NUMBIN / 0 /
C
      NUMBIN = NUMBIN+1
      IF(NUMBIN.GE.50) THEN
         WRITE(IPS,'(A2)') 'st'
         NUMBIN = 0
      ENDIF
      WRITE(IPS,'(F8.3,1X,F8.3,A2)') ZIX,ZIY,' m'
      ZIXSAV = ZIX
      ZIYSAV = ZIY
      RETURN
      END
C
      SUBROUTINE DRWABS(ZIX,ZIY)
C
C***********************************************************************
C     BRANCHING TO DRWABS1 (CALCOMP) / DRWABS2 (POSTSCRIPT).           *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) zix,ziy
      COMMON /LCLPS/LCAL,LPOS
      integer lcal,lpos
C
      IF(LCAL.EQ.1) CALL DRWABS1(ZIX,ZIY)
      IF(LPOS.EQ.1) CALL DRWABS2(ZIX,ZIY)
      RETURN
      END
C
      SUBROUTINE DRWABS1(ZIX,ZIY)
C
C***********************************************************************
C     THIS ROUTINE DRAWS A LINE VECTOR FROM THE CURRENT BEAM POSITION  *
C TO (IX,IY), WHICH BECOMES THE NEW BEAM POSITION.                     *
C***********************************************************************
C
C
      use itm_types
      implicit none
      real (r8) zix,ziy
      COMMON /LIB8X1/ZIXSAV,ZIYSAV
      real (r8) zixsav,ziysav
C
      ZIXSAV = ZIX
      ZIYSAV = ZIY
      RETURN
      END
C
      SUBROUTINE DRWABS2(ZIX,ZIY)
C
C***********************************************************************
C     POSTSCRIPT VERSION.                                              *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) zix,ziy
      integer ips
      PARAMETER (IPS=51)
C
      COMMON /LIB8X2/ZIXSAV,ZIYSAV
      real (r8) zixsav,ziysav
C
      IF(ZIX.EQ.ZIXSAV.AND.ZIY.EQ.ZIYSAV) THEN
      WRITE(IPS,'(A3,F8.3,1X,F8.3,A3,F8.3,1X,F8.3,A4,F8.3,1X,F8.3,A2)')
     >         'st ',ZIX+1.,ZIY,' m ',ZIX,ZIY,' pt ',ZIX,ZIY,' m'
      ELSE
         WRITE(IPS,'(F8.3,1X,F8.3,A2)') ZIX,ZIY,' l'
      ENDIF
      ZIXSAV = ZIX
      ZIYSAV = ZIY
      RETURN
      END
C
      SUBROUTINE SEELOC1(ZIX,ZIY)
C
C***********************************************************************
C     THIS ROUTINE LOOKS UP THE CURRENT POSITION OF THE DRAWING BEAM.  *
C COMMON /LIB8X1/ ITSELF SHOULD NOT BE USED FOR THIS PURPOSE SINCE ITS *
C CONTENTS SHOULD REMAIN SHARED AND AFFECTED ONLY BY THE LOWEST-LEVEL  *
C SYSTEM-DEPENDENT DRAWING ROUTINES.                                   *
C NOTE THAT THERE IS NO BRANCHING ROUTINE SEELOC SINCE THE OUTPUT VAR- *
C IABLES IX,IY OF SEELOC1 AND SEELOC2 MAY DIFFER IN PRINCIPLE.         *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) zix,ziy
      COMMON /LIB8X1/ZIXSAV,ZIYSAV
      real (r8) zixsav,ziysav
C
      ZIX = ZIXSAV
      ZIY = ZIYSAV
      RETURN
      END
C
      SUBROUTINE SEELOC2(ZIX,ZIY)
C
C***********************************************************************
C     POSTSCRIPT VERSION.                                              *
C***********************************************************************
C
      use itm_types
      implicit none
      real (r8) zix,ziy
      COMMON /LIB8X2/ZIXSAV,ZIYSAV
      real (r8) zixsav,ziysav
C
      ZIX = ZIXSAV
      ZIY = ZIYSAV
      RETURN
      END
C
      SUBROUTINE PLOT(A,B,I)
      use itm_types
      implicit none
      real (r8) a,b
      integer i
      RETURN
      END
C
      SUBROUTINE PLOTS(A,B,NAME)
      use itm_types
      implicit none
      real (r8) a,b
      CHARACTER*10 NAME
      RETURN
      END

      SUBROUTINE LINCOL(ICOL)
C***********************************************************************
C ROUTINE TO CHANGE COLOR IN POST-SCRIPT VERSION ONLY
C***********************************************************************
      use itm_types
      implicit none
      integer icol
      integer ips
      PARAMETER (IPS=51)
      write(IPS,*) ' stroke'
      if (icol.eq.0) write(IPS,*) ' 0.0 0.0 0.0 setrgbcolor'
      if (icol.eq.1) write(IPS,*) ' 1.0 0.0 0.0 setrgbcolor'
      if (icol.eq.2) write(IPS,*) ' 0.0 1.0 0.0 setrgbcolor'
      if (icol.eq.3) write(IPS,*) ' 0.0 0.0 1.0 setrgbcolor'
      if (icol.eq.4) write(IPS,*) ' 1.0 1.0 0.0 setrgbcolor'
      if (icol.eq.5) write(IPS,*) ' 0.0 1.0 1.0 setrgbcolor'
      if (icol.eq.6) write(IPS,*) ' 1.0 0.0 1.0 setrgbcolor'
      if (icol.eq.7) write(IPS,*) ' 1.0 0.6 0.0 setrgbcolor'
      if (icol.eq.8) write(IPS,*) ' 0.6 0.0 1.0 setrgbcolor'
      if (icol.eq.9) write(IPS,*) ' 0.0 1.0 0.6 setrgbcolor'
      return
      end

      SUBROUTINE FILLTRIA(X,Y,Z,ZMIN,ZMAX)
      use itm_types
      implicit none
      real (r8) zmin,zmax
      integer ips
      PARAMETER (IPS=51)
      REAL (R8) X(3),Y(3),Z(3)
      integer i

      DO I=1,3
         Z(I) = (Z(I)-ZMIN)/(ZMAX-ZMIN)
      ENDDO
      WRITE(IPS,901) X(1),Y(1),X(2),Y(2),X(3),Y(3),
     >               max(0.0_R8,min(1.0_R8,Z(1))),
     >               max(0.0_R8,min(1.0_R8,Z(2))),
     >               max(0.0_R8,min(1.0_R8,Z(3)))
 901  FORMAT(6f8.2,3f8.3,' nql ftr')
      RETURN
      END
      SUBROUTINE COLORBAR(ZC,NLAB,XR,YT,YB)
c-----------------------------------------------------------------------
c subroutine to add a colorbar next to a contour plot using the default
c colors defined in begplt2
c-----------------------------------------------------------------------
      use itm_types
      implicit none
      integer nlab
      real (r8) xr,yt,yb
      integer ips
      PARAMETER (IPS=51)
      REAL (R8) X(3),Y(3),Z(3),ZC(*)
      CHARACTER*19 ALAB
      real (r8) xwidth,xoff,zmax,zmin,z1,z3,y1,x3
      integer i,nlabc

      xwidth=15._R8
      xoff=10._R8
      NLABC = 51
      ZMAX = ZC(NLAB)
      ZMIN = ZC(1)
      DO I=1,NLABC-1
	Z1   = (ZMAX-ZMIN)*REAL(I-1,R8)/REAL(NLABC-1,R8)+ZMIN
	Z(2) = Z1
	Z3   = (ZMAX-ZMIN)*REAL(I,R8)/REAL(NLABC-1,R8)+ZMIN
	Z(3) = Z3
	Z(1) = Z1
	Y(1) = YB+XOFF + REAL(I-1,R8)/REAL(NLABC-1,R8)*(YT-YB-2*XOFF)
	Y(3) = YB+XOFF + REAL(I,R8)  /REAL(NLABC-1,R8)*(YT-YB-2*XOFF)
	Y(2) = Y(1)
	X(1) = XR + XOFF
	X(3) = XR + XOFF + XWIDTH
	X(2) = X(3)	
	CALL FILLTRIA(X,Y,Z,ZMIN,ZMAX)
	X(2) = X(1)
	Y(2) = Y(3)
	Z(1) = Z1
	Z(2) = Z3
	Z(3) = Z3	
        CALL FILLTRIA(X,Y,Z,ZMIN,ZMAX)
      ENDDO	
      WRITE(IPS,*)  '0. 0. 0. setrgbcolor'
      DO I=1,NLAB-1
	Z1 = ZC(I)
	Y1 = YB+XOFF + (ZC(I)-ZMIN)/(ZMAX-ZMIN)*(YT-YB-2*XOFF)
	X3 = XR + XOFF + XWIDTH
	WRITE(ALAB,'(1PE9.2)') Z1
   	CALL DLCH2(INT(X3+5._R8),INT(Y1),ALAB,19,-1)
	WRITE(IPS,11)  X3-xwidth/2._R8,Y1
	WRITE(IPS,12)  X3,Y1
      ENDDO	
      WRITE(ALAB,'(1PE9.2)') ZMAX
      CALL DLCH2(INT(X3+5._R8),INT(YT-XOFF),ALAB,19,-1)


      WRITE(IPS,11)  XR+xoff,YT-xoff
      WRITE(IPS,12)  XR+xoff+xwidth,YT-xoff
      WRITE(IPS,12)  XR+xoff+xwidth,YB+xoff
      WRITE(IPS,12)  XR+xoff,YB+xoff
      WRITE(IPS,12)  XR+xoff,YT-xoff
   11 format(2f8.2,' m')
   12 format(2f8.2,' l')
      return
      end
