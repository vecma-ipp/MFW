         PROGRAM UNIOUT
C  ********************************************************************
C  *  UNIOUT                  31/07/89                  A.JAUN
C  * 
C  *  Reads and uniras output file consisting in a assembly of several
C  *  different pictures and draws them on an output device.
C  *
C  ********************************************************************
C
      PARAMETER (MDSEG= 20)
      DIMENSION NSEGM(MDSEG)
      CHARACTER*8 TT
c.......................................................................
      IPAGE = 0
      ZALPHA = 0.0
      ZXIN = 210.
      ZYIN = 297.
      IRATIO = 0
      INFO = 1
C
C  Header
      PRINT *,'                     ***************************'
      PRINT *,'                     *       U N I O U T       *'
      PRINT *,'                     *       -----------       *'
      PRINT *,'                     * Multi-device output of  *'
      PRINT *,'                     *  Uniras plot files      *'
      PRINT *,'                     ***************************'
      PRINT *,' '
C
C  Select the driver
 10   CONTINUE
      PRINT *,'FIRST, SELECT THE OPTION :'
      PRINT *,'(Enter 1: for postscript, plot n first segments'
      PRINT *,'       2: for Tektronix 4014, plot n first segments'
      PRINT *,'       3: for xterm, plot n first segments'
      PRINT *,'       4: plot diff. segments.Assume A4 vert. -> A4 hor.'
      PRINT *,'       5: choose A4 vert.or hor., choose if ratio y/x'
     +       ,' kept constant and angle'
      PRINT *,'       6: choose input dim., if ratio cst. and angle'
      PRINT *,'       7: for sun window, plot n first segments'
      PRINT *,' '
      PRINT *,' 11, 12, 13, or 17: same as 1, 2, 3 or 7 but plot all',
     +  ' the segments'
      PRINT *,' '
      PRINT *,'      91: traceur versacolor A4 du SIC, driver GVC2766A4'
      PRINT *,'      99: Versatec coul. central (SIC), driver GVCE3236'
      PRINT *,'       0: to quit             ):'
      READ *, IDEV
      IALLSEG = 0
      IF (IDEV.GE.11 .AND. IDEV.LE.19) THEN
         IALLSEG = 1
         IDEV = IDEV - 10
      ENDIF
      IF (IDEV.EQ.1) THEN
         CALL GROUTE('SEL MPOST;EX')
      ELSE IF (IDEV.EQ.2) THEN
         CALL GROUTE('SEL MT40XX;EX')
      ELSE IF (IDEV.EQ.3) THEN 
         CALL GROUTE('SEL MX11;EX')
      ELSE IF (IDEV.GE.4 .AND. IDEV.LE.6) THEN
         GO TO 200
      ELSE IF (IDEV.EQ.7) THEN 
         CALL GROUTE('SEL MSVIEW;EX')
      ELSE IF (IDEV.EQ.91) THEN 
         CALL GROUTE('SEL GVC2766A4;EX')
      ELSE IF (IDEV.EQ.99) THEN 
         CALL GROUTE('SEL GVCE3236;EX')
      ELSE IF (IDEV.EQ.0) THEN 
         GO TO 999
      ELSE
         PRINT *,'>>> ERROR: selected option not available'
         GOTO 10
      ENDIF
C
      ISECTION = 1
C
CL    0. ALL SEGMENTS TO BE PLOTTED
C
      IF (IALLSEG .EQ. 1) GO TO 201
C
CL    1. DIRECT PLOT OF N FIRST SEGMENT
C
C  Subdivide the output page
      PRINT *,'ENTER THE NUMBER OF SEGMENTS TO BE DRAWN'
      PRINT *,'  HORIZONTALLY, AND VERTICALLY.'
      PRINT *,'  (enter "1 1" for one segment on one page):'
      READ  *, IXSUB, IYSUB

 101  CONTINUE

      ZXLEN = 1. / FLOAT(IXSUB)
      ZYLEN = 1. / FLOAT(IYSUB)
C
      IMAX = IXSUB*IYSUB
C
C  Messages of uniras are written on unit 2
c%OS      CALL GUNIT(1,2)
c%OS      CALL GUNIT(11,2)
C      CALL GSHMES('SUP','SUP')
C  Open uniras
      CALL GOPEN
C
C  Display header information: date, time, etc
C
C  Use the old segment file
      CALL GSEGWK(0)
C
C  Loop over the segments (y is the slowly varying parameter)
      ZALPHA = 0.0
      ZXIN = 210.
      ZYIN = 297.
      IRATIO = 0
      DO 100 I=0,IMAX-1
         ZXPOS = 1.*MOD(I,IXSUB)/IXSUB
         ZYPOS = 1. - ZYLEN - 1.*INT(I/IXSUB)/IYSUB
         CALL DISGGN(I+1,ZXPOS,ZYPOS,ZXLEN,ZYLEN,ZXIN,ZYIN,IDEV,
     +        ZALPHA,IRATIO,INFO)
         IF (INFO .LT. 1) GO TO 999
 100  CONTINUE
C
      IF (IDEV.EQ.2) THEN
C        TEKTRONIX --- WAIT FOR DUMMY INTEGER, TO KEEP SCREEN CLEAN
         CALL GCLOSE
         PRINT *,''
         READ (*,'(A)') TT
      ENDIF
C
      IPAGE = 1
      GO TO 999
C
CL    2. PLOT OF SPECIFIC SEGMENTS WITH DIFFERENT OPTIONS
C
 200  CONTINUE
      ISECTION = 2
      IOPT = IDEV
 205  PRINT *,' SELECT THE OUTPUT DEVICE DRIVER:'
      PRINT *,'  (Enter 1: for postscript'
      PRINT *,'         2: for Tektronix 4014'
      PRINT *,'         3: for xterm, plot n first segments'
      PRINT *,'         7: for sun window   '
      PRINT *,' '
      PRINT *,' 11, 12, 13, or 17: same as 1, 2, 3 or 7 but plot all',
     +  ' the segments'
      PRINT *,' '
      PRINT *,'      91: traceur versacolor A4 du SIC, driver GVC2766A4'
      PRINT *,'      99: Versatec coul. central (SIC), driver GVCE3236'
      READ *, IDEV
      IALLSEG = 0
      IF (IDEV.GE.11 .AND. IDEV.LE.19) THEN
         IALLSEG = 1
         IDEV = IDEV - 10
      ENDIF
      IF (IDEV.EQ.1) THEN
         CALL GROUTE('SEL MPOST; EX')
      ELSE IF (IDEV.EQ.2) THEN
         CALL GROUTE('SEL MT40XX;EX')
      ELSE IF (IDEV.EQ.3) THEN 
         CALL GROUTE('SEL MX11;EX')
      ELSE IF (IDEV.EQ.7) THEN 
         CALL GROUTE('SEL MSVIEW;EX')
      ELSE IF (IDEV.EQ.91) THEN 
         CALL GROUTE('SEL GVC2766A4;EX')
      ELSE IF (IDEV.EQ.99) THEN 
         CALL GROUTE('SEL GVCE3236;EX')
      ELSE
         PRINT *,'>>> ERROR: selected option not available'
         GOTO 205
      ENDIF
C
      IPAGE = 0
C
CL    2.0 ALL SEGMENTS TO BE PLOTTED
C
 201  CONTINUE
      IF (IALLSEG .EQ. 1) THEN
         PRINT *,' All the segments in the unipict file will be plotted'
     +        ,' on the device'
         PRINT *,' ENTER how many you want per page.'
         READ *,IALLPRPG
c%OS         PRINT *,' ENTER how many on horizontal(vertical) axis with',
c%OS     +        ' positive(negative) number'
c%OS         PRINT *,'     for ex:  2  => segments subdivide into 2 columns'
c%OS         PRINT *,'     for ex: -2  => segments subdivide into 2 rows'
c%OS         READ *,IALLHOR
         IALLHOR = INT(SQRT(1.*IALLPRPG) + 0.99)
         IALLHOR = MIN(IALLPRPG,IALLHOR)
         IF (IALLHOR .EQ. 0) GO TO 999
         IF (IALLHOR .GT. 0) THEN
            IXSUB = IALLHOR
            IYSUB = IALLPRPG/IXSUB
            IF (IXSUB*IYSUB .LT. IALLPRPG) IYSUB = IYSUB + 1
         ELSE
            IYSUB = IALLHOR
            IXSUB = IALLPRPG/IYSUB
            IF (IXSUB*IYSUB .LT. IALLPRPG) IXSUB = IXSUB + 1
         ENDIF
         IMAX = IXSUB*IYSUB
         IALLPRPG = IMAX
         NSEGTO = IMAX
         IF (ISECTION .EQ. 1) GO TO 300
         IF (ISECTION .EQ. 2) GO TO 230
      ENDIF
C
CL    2.1 STARTS THE LOOP. COUNTS THE NUMBER OF PAGES PLOTTED
C
 210  CONTINUE
C
CL    2.2 DETERMINE THE SEGMENTS TO BE DRAWN ON NEXT PAGE
C
      PRINT *,' ENTER THE TOTAL NUMBER OF SEGMENTS TO BE DRAWN ON NEXT',
     +        ' PAGE'
      PRINT *,' enter 0 to quit'
      READ *,NSEGTO
      IF (NSEGTO .LE. 0) GO TO 999
C
      PRINT *,' ENTER THE LIST OF THE SEGMENTS TO DRAW ON THE PAGE :'
      READ *,(NSEGM(I),I=1,NSEGTO)
C
      PRINT *,'ENTER THE NUMBER OF SEGMENTS ON THE HORIZONTAL AXIS'
      PRINT *,' (with respect to the plots)'
      READ  *, IXSUB
      IF (IXSUB .EQ. 0) GO TO 999
      IYSUB = NSEGTO / IXSUB
      IF (IXSUB*IYSUB .LT. NSEGTO) IYSUB = IYSUB + 1
      IMAX = IXSUB*IYSUB
C
CL    2.3 READ ORIENTATION OF PLOTS WITH RESPECT TO OUTPUT DEVICE
C         (FOR OPTIONS 5 AND 6)
C
 230  CONTINUE
      ZALPHA = 0.
      IF (IOPT.EQ.5 .OR. IOPT.EQ.6) THEN
         PRINT *,' ENTER ORIENTATION OF PLOTS ON OUTPUT DEVICE'
         PRINT *,' (0, 90, -90, or whatever)'
         READ  *, ZALPHA
CC         IF (ZALPHA .NE. 0. .AND. ZALPHA .NE. 90. .AND. ZALPHA.NE.-90.) 
CC     +    PRINT *,' WARNING: ORIENTATION SHOULD BE 0., 90. OR -90.'
      ENDIF
C
CL    2.4 CHOOSE INPUT PAGE ORIENTATION
C         (FOR OPTION 5)
C
      IHOR = 0
      IF (IOPT .EQ. 5) THEN
         PRINT *,' ENTER 0 OR 1 IF YOUR PLOT DIMENSIONS ARE A4 VERTICAL'
     +          ,' OR HORIZ.'
         READ *,IHOR
      ENDIF
C
CL    2.5 CHOOSE IF SIZE RATIO SHOULD BE KEPT CONSTANT AND WINDOW DIMENSIONS
C         (FOR OPTION 5 AND 6)
C
      IRATIO = 0
      IF (IOPT.EQ.5 .OR. IOPT.EQ.6) THEN
         PRINT *,' ENTER  0 IF RATIO Y/X NEED NOT TO BE KEPT CONSTANT'
         PRINT *,'        1 IF RATIO SHOULD BE KEPT CONSTANT'
         PRINT *,'       -1 TO KEEP CM AS CM,I.E. NO SCALING OF X AND Y'
         READ *,IRATIO
      ENDIF
C
CL    2.6 CHOOSE DIMENSIONS OF INPUT WINDOW
C         (FOR OPTION 6 AND IF (IRATIO.NE.-1 OR IMAX>1))
C
      ZXIN = 210.
      ZYIN = 297.
      IF (IHOR .EQ. 1) THEN
         ZXIN = 297.
         ZYIN = 210.
      ENDIF
C
CC      IF (IOPT.EQ.6 .OR. IRATIO.EQ.-1) THEN
CC         ZXIN = 0.0
CC         ZYIN = 0.0
CC      ENDIF
C
      IF (IOPT.EQ.6 .AND. IRATIO.NE.-1) THEN
         PRINT *,' ENTER DIMENSIONS X,Y OF INPUT WINDOW (in cm)'
         READ *,ZXIN,ZYIN
         ZXIN = 10. * ZXIN
         ZYIN = 10. * ZYIN
      ENDIF
C
CL    3. OPEN UNIRAS AND PLOT ON DEVICE
C        ATTENTION: THE TEKTRONIX PLOTS ON THE SCREEN ONLY 
C                   AFTER THE CALL TO GCLOSE
C
C
 300  CONTINUE
      IPAGE = IPAGE + 1
C        CLEAR SCREEN FOR NEXT PAGE
      IF (IPAGE.GE.2 .AND. IDEV.NE.2) CALL GCLEAR
C
      IF (IPAGE.GE.2 .AND. IDEV.NE.2) GO TO 310
C
C  Messages of uniras are written on unit 2
c%OS      CALL GUNIT(1,2)
c%OS      CALL GUNIT(11,2)
C
C  Open uniras
      CALL GOPEN
C
C  Display header information: date, time, etc
C
C  Use the old segment file
      CALL GSEGWK(0)
C
CL    3.1 DISPOSE SEGMENTS ON THE PAGE. THE VERTICAL LINE WITH RESPECT 
C         TO THE PLOTS IS THE SLOWLY VARYING PARAMETER.
C
 310  CONTINUE
C
      ZXLEN = 1. / FLOAT(IXSUB)
      ZYLEN = 1. / FLOAT(IYSUB)
      I = -1
      DO II=1,IMAX
         NSEGM(II) = II
      ENDDO
 311  CONTINUE
      I = I + 1
      IF (ZALPHA .EQ. 90.) THEN
         ZXPOS = 1.*INT(I/IYSUB)/IXSUB
         ZYPOS = 1.*MOD(I,IYSUB)/IYSUB
      ELSE IF (ZALPHA .EQ. -90.) THEN
         ZXPOS = 1. - ZXLEN - 1.*INT(I/IYSUB)/IXSUB
         ZYPOS = 1. - ZYLEN - 1.*MOD(I,IYSUB)/IYSUB
      ELSE
         ZXPOS = 1.*MOD(I,IXSUB)/IXSUB
         ZYPOS = 1. - ZYLEN - 1.*INT(I/IXSUB)/IYSUB
      ENDIF
C
      IF (NSEGM(I+1) .GT. 0) THEN
         CALL DISGGN(NSEGM(I+1),ZXPOS,ZYPOS,ZXLEN,ZYLEN,ZXIN,ZYIN,IDEV
     +        ,ZALPHA,IRATIO,INFO)
         IF (INFO .LT. 1) GO TO 999
      ENDIF
C     IF INFO.GE.1 => SEGMENT NSEGM(I+1) OK

C     311 LOOP UNTIL PAGE FINISHED
      IF (INFO.GE.1 .AND. I.LT.NSEGTO-1) GO TO 311
C
CL    3.1 WAIT FOR DUMMY INPUT TO KEEP TEKTRONIX LIKE SCREENS CLEAN
C
      IF (IDEV.EQ.2 .AND. IALLSEG.NE.1) THEN
C        TEKTRONIX --- WAIT FOR DUMMY INPUT, TO KEEP SCREEN CLEAN
         CALL GCLOSE
         PRINT *,''
         READ (*,'(A)') TT
      ENDIF
C
CL    3.1.2 CONTINUE SCROLLING PAGES IF IALLSEG=1
C
      IF (IALLSEG .EQ. 1) THEN
         IF (INFO .LT. 1) GO TO 999
         ILASTSEG = NSEGM(I+1)
         I = -1
         DO II=1,NSEGTO
            NSEGM(II) = ILASTSEG + II
         ENDDO
         CALL GSEGEX(NSEGM(1),INFO)
         IF (INFO .LT. 1) GO TO 999
         IPAGE = IPAGE + 1
         IF (IDEV .EQ. 2) THEN
C     TEKTRONIX --- WAIT FOR DUMMY INPUT, TO KEEP SCREEN CLEAN
            CALL GCLOSE
            PRINT *,''
            READ (*,'(A)') TT
            CALL GOPEN
            CALL GSEGWK(0)
         ELSE IF (IDEV .NE. 1) THEN
C     WAIT FOR RETURN BEFORE PREPARING NEXT PLOT
            READ (*,'(A)') TT
            CALL GCLEAR
         ELSE
            CALL GCLEAR
         ENDIF     
         GO TO 311
      ENDIF
C
CL    3.2 RESTART THE LOOP => PLOT OTHER SEGMENTS ON NEXT PAGE
C
      GO TO 210
C
CL    9. END OF PROGRAM
C
 999  CONTINUE
C
      CALL GCLOSE
      IF (IDEV .EQ. 2) THEN
C     TEKTRONIX --- WAIT FOR DUMMY INPUT, TO KEEP SCREEN CLEAN
         PRINT *,''
         READ (*,'(A)') TT
      ENDIF
C
      PRINT *,'  ',IPAGE,' PAGE(S) WERE PLOTTED'
      PRINT *,'STOP - Normal end of UNIOUT'
      STOP
      END
      SUBROUTINE DISGGN(K,PXPOS,PYPOS,PXLEN,PYLEN,PXIN,PYIN,KDEV,
     +     PANGLE,KRATIO,KINFO)
C  *********************************************************************
C  *  DISGGN                    5/09/90                   O.SAUTER     *
C  *                                                                   *
C  *  Creates the output page.                                         *
C  *  PXIN, PYIN : DIMENSIONS OF INPUT WINDOW                          *
C  *  PANGLE =  0. OR 90. ANGLE OF ROTATION OF THE PLOT BEFORE PRINTING*
C  *  KRATIO =  0 : DO NOT KEEP RATIO Y/X CONSTANT                     *
C  *         =  1 : KEEP SIZE RATIO Y/X CONSTANT                       *
C  *         = -1 : KEEP CM AS CM                                      *
C  *  KINFO : DEMANDS ON STATUS OF SEGMENT USING GSEGEX(K,KINFO)       *
C  *********************************************************************
C
         CALL GSEGEX(K,KINFO)
         IF (KINFO .LT. 1) RETURN

         ZANGLE = PANGLE
         ZXIN = PXIN
         ZYIN = PYIN
C
C  Ask for the lengths of the plotting area
         CALL GRPSIZ(ZXSIZE,ZYSIZE)
C
C  this shift may be necessary because of a bug in the
C  HPOSTA4 driver
C
         IF (KDEV .EQ. 1) THEN
            ZYSIZE = ZYSIZE - 4.
         ENDIF
C
CC         PRINT *,' DEVICE DIM.: X= ',ZXSIZE,'  Y= ',ZYSIZE
C
CL    0. SETUP TRANSFORMATION
C        NOTE : THE LASER CAN ONLY PLOT 19.6 CM LARGE.THEREFORE
C        ZREDU SHOULD BE SMALLER OR EQUAL TO 0.97 FOR MPOST.
C
         ZREDU = 0.97
         ZXDEV = PXLEN * ZXSIZE
         ZYDEV = PYLEN * ZYSIZE
         ZDELX = 0.01 * ZXSIZE
         ZDELY = 0.01 * ZYSIZE
         ZXSH0 = PXPOS * ZXSIZE
         ZYSH0 = PYPOS * ZYSIZE
         ZRAD = 3.14159265359 / 180. * ZANGLE
         ZSIN = SIN (ZRAD)
         ZCOS = COS(ZRAD)
         ZSINA = ABS(ZSIN)
         ZCOSA = ABS(ZCOS)
         IF (ZCOSA .LE. 1.0E-05) THEN
            ZCOS = 0.0
            ZCOSA = 0.0
         ENDIF
         IF (ZSINA .LE. 1.0E-05) THEN
            ZSIN = 0.0
            ZSINA = 0.0
         ENDIF
C
         IF (KRATIO .EQ.-1) GO TO 300
C
C THIS SCALING IS SUCH THAT THE FOUR CORNERS OF PLOT TOUCH THE 4 SIDES
C OF THE DEVICE VIEWPORT. IT GIVES STRANGE RESULTS FOR ANGLES AROUND 45.
C THERE, IT IS BETTER TO KEEP RATIO FIX
C
         IF (ABS(ZCOSA-ZSINA) .LE. 0.25) GO TO 200
C
         IF (ABS(ZCOSA-ZSINA) .GT. 1.0E-04) THEN
            ZDEN = ZCOSA**2 - ZSINA**2
            ZSCALX = (ZXDEV*ZCOSA-ZYDEV*ZSINA) / ZXIN / ZDEN
            ZSCALY = (ZYDEV*ZCOSA-ZXDEV*ZSINA) / ZYIN / ZDEN
         ELSE
C            ZSCALX = MIN(ZXDEV,ZYDEV) / ZXIN / (ZCOSA+ZSINA)
C            ZSCALY = ZSCALX * ZXIN / ZYIN
C     OR
            ZSCALX = MIN(ZXDEV,ZYDEV) / (ZXIN+ZYIN) / ZCOSA
            ZSCALY = ZSCALX
         ENDIF
C
         IF (KRATIO .EQ. 1) GO TO 200
C
CL    1. DO NOT KEEP SIZE RATIO PYIN/PXIN CONSTANT
C
         ZXNOR = ZREDU * ZSCALX
         ZYNOR = ZREDU * ZSCALY
C
         GO TO 999
C
CL    2. KEEP SIZE RATIO PYIN/PXIN CONSTANT, BUT EXPAND TO MAXIMUM OF
C        OUTPUT DEVICE PLOTTING AREA.
C
 200     CONTINUE
C
C
         ZSCALX = ZXDEV / (ZXIN*ZCOSA + ZYIN*ZSINA)
         ZSCALY = ZYDEV / (ZXIN*ZSINA + ZYIN*ZCOSA)
         ZXNOR = ZREDU * MIN(ZSCALX,ZSCALY)
         ZYNOR = ZXNOR
C
         GO TO 999
C
CL    3. KEEP CM AS CM
C
 300     CONTINUE
C
         ZXNOR = 1.0
         ZYNOR = 1.0
         IF (ABS(ZCOSA-ZSINA) .GT. 1.0E-04) THEN
            ZDEN = ZCOSA**2 - ZSINA**2
            ZXIN = (ZXDEV*ZCOSA-ZYDEV*ZSINA) / ZDEN
            ZYIN = (ZYDEV*ZCOSA-ZXDEV*ZSINA) / ZDEN
         ELSE
            ZXIN = MIN(ZXDEV,ZYDEV) / (ZCOSA+ZSINA)
            ZYIN = ZXIN
         ENDIF
C
CL    9. TRANSFORM SEGMENT AND COPY IT ON DEVICE
C
 999     CONTINUE
C
C TRANSFORMATION OF THE CORNERS : (0,ZYINSC); (ZXINSC,ZYINSC); (ZXINSC,0)
C
         ZXINSC = ZXNOR * ZXIN
         ZYINSC = ZYNOR * ZYIN
         ZXBXLN = ZXINSC*ZCOSA + ZYINSC * ZSINA
         ZYBXLN = ZXINSC*ZSINA + ZYINSC * ZCOSA
         ZX1 = - ZSIN * ZYINSC
         ZY1 =   ZCOS * ZYINSC
         ZX2 =   ZCOS*ZXINSC - ZSIN*ZYINSC
         ZY2 =   ZSIN*ZXINSC + ZCOS*ZYINSC
         ZX3 =   ZCOS * ZXINSC
         ZY3 =   ZSIN * ZXINSC
C     ORIGIN OF BOX
         ZXBXOR = MIN(0.,ZX1,ZX2,ZX3)
         ZYBXOR = MIN(0.,ZY1,ZY2,ZY3)
C
         ZXSH = ZXSH0 - ZXBXOR + ZDELX
         ZYSH = ZYSH0 - ZYBXOR + ZDELY
C
C  this shift is necessary for the HPOST.. driver, as the laser needs
C  a y shift of at least 5 mm.
C
         IF (KDEV .EQ. 1) THEN
            ZYSH = ZYSH + 4.
         ENDIF
C
C     CLIP VIEWPORT IF MORE THAN ONE SEGMENT IS DRAWN ON THE PAGE
C     (IN CASE OF CM KEPT AS CM)
         IF (KRATIO.EQ.-1 .AND. (PXLEN.NE.1.0 .OR. PYLEN.NE.1.0)) THEN
            CALL GCLIP
            ZXVPOR = ZXSH0
            ZYVPOR = ZYSH0
            ZXVPLN = 0.99 * ZXBXLN
            ZYVPLN = 0.99 * ZYBXLN
            CALL GVPORT(ZXVPOR,ZYVPOR,ZXVPLN,ZYVPLN)
         ENDIF
C
         CALL GSEGTR(K,ZXSH,ZYSH,ZXNOR,ZYNOR,ZANGLE)
         CALL GSEGWK(K)
C     RESET NO CLIPPING DEFAULT OPTION
         CALL GNCLIP
C
         RETURN
         END
