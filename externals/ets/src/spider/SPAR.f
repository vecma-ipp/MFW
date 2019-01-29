C                                                                1/15/81

C***********************************************************************

C  ODRV -- DRIVER FOR SPARSE MATRIX REORDERING ROUTINES

C***********************************************************************

        SUBROUTINE  ODRVD

     *     (N, IA,JA,A, P,IP, NSP,ISP, PATH, FLAG)

C

C  DESCRIPTION

C

C    ODRV FINDS A MINIMUM DEGREE ORDERING OF THE ROWS AND COLUMNS OF A

C    SYMMETRIC MATRIX M STORED IN (IA,JA,A) FORMAT (SEE BELOW).  FOR THE

C    REORDERED MATRIX, THE WORK AND STORAGE REQUIRED TO PERFORM GAUSSIAN

C    ELIMINATION IS (USUALLY) SIGNIFICANTLY LESS.

C

C    IF ONLY THE NONZERO ENTRIES IN THE UPPER TRIANGLE OF M ARE BEING

C    STORED, THEN ODRV SYMMETRICALLY REORDERS (IA,JA,A), (OPTIONALLY)

C    WITH THE DIAGONAL ENTRIES PLACED FIRST IN EACH ROW.  THIS IS TO

C    ENSURE THAT IF M(I,J) WILL BE IN THE UPPER TRIANGLE OF M WITH

C    RESPECT TO THE NEW ORDERING, THEN M(I,J) IS STORED IN ROW I (AND

C    THUS M(J,I) IS NOT STORED);  WHEREAS IF M(I,J) WILL BE IN THE

C    STRICT LOWER TRIANGLE OF M, THEN M(J,I) IS STORED IN ROW J (AND

C    THUS M(I,J) IS NOT STORED).

C

C

C  STORAGE OF SPARSE MATRICES

C

C    THE NONZERO ENTRIES OF THE MATRIX M ARE STORED ROW-BY-ROW IN THE

C    ARRAY A.  TO IDENTIFY THE INDIVIDUAL NONZERO ENTRIES IN EACH ROW,

C    WE NEED TO KNOW IN WHICH COLUMN EACH ENTRY LIES.  THESE COLUMN

C    INDICES ARE STORED IN THE ARRAY JA;  I.E., IF  A(K) = M(I,J),  THEN

C    JA(K) = J.  TO IDENTIFY THE INDIVIDUAL ROWS, WE NEED TO KNOW WHERE

C    EACH ROW STARTS.  THESE ROW POINTERS ARE STORED IN THE ARRAY IA;

C    I.E., IF M(I,J) IS THE FIRST NONZERO ENTRY (STORED) IN THE I-TH ROW

C    AND  A(K) = M(I,J),  THEN  IA(I) = K.  MOREOVER, IA(N+1) POINTS TO

C    THE FIRST LOCATION FOLLOWING THE LAST ELEMENT IN THE LAST ROW.

C    THUS, THE NUMBER OF ENTRIES IN THE I-TH ROW IS  IA(I+1) - IA(I),

C    THE NONZERO ENTRIES IN THE I-TH ROW ARE STORED CONSECUTIVELY IN

C

C            A(IA(I)),  A(IA(I)+1),  ..., A(IA(I+1)-1),

C

C    AND THE CORRESPONDING COLUMN INDICES ARE STORED CONSECUTIVELY IN

C

C            JA(IA(I)), JA(IA(I)+1), ..., JA(IA(I+1)-1).

C

C    SINCE THE COEFFICIENT MATRIX IS SYMMETRIC, ONLY THE NONZERO ENTRIES

C    IN THE UPPER TRIANGLE NEED BE STORED.  FOR EXAMPLE, THE MATRIX

C

C             ( 1  0  2  3  0 )

C             ( 0  4  0  0  0 )

C         M = ( 2  0  5  6  0 )

C             ( 3  0  6  7  8 )

C             ( 0  0  0  8  9 )

C

C    COULD BE STORED AS

C

C              1  2  3  4  5  6  7  8  9 10 11 12 13

C         ---+--------------------------------------

C         IA   1  4  5  8 12 14

C         JA   1  3  4  2  1  3  4  1  3  4  5  4  5

C          A   1  2  3  4  2  5  6  3  6  7  8  8  9

C

C    OR (SYMMETRICALLY) AS

C

C              1  2  3  4  5  6  7  8  9

C         ---+--------------------------

C         IA   1  4  5  7  9 10

C         JA   1  3  4  2  3  4  4  5  5

C          A   1  2  3  4  5  6  7  8  9          .

C

C

C  PARAMETERS

C

C    N    - ORDER OF THE MATRIX

C

C    IA   - INTEGER ONE-DIMENSIONAL ARRAY CONTAINING POINTERS TO DELIMIT

C           ROWS IN JA AND A;  DIMENSION = N+1

C

C    JA   - INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE COLUMN INDICES

C           CORRESPONDING TO THE ELEMENTS OF A;  DIMENSION = NUMBER OF

C           NONZERO ENTRIES IN (THE UPPER TRIANGLE OF) M

C

C    A    - REAL ONE-DIMENSIONAL ARRAY CONTAINING THE NONZERO ENTRIES IN

C           (THE UPPER TRIANGLE OF) M, STORED BY ROWS;  DIMENSION =

C           NUMBER OF NONZERO ENTRIES IN (THE UPPER TRIANGLE OF) M

C

C    P    - INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE PERMUTATION

C           OF THE ROWS AND COLUMNS OF M CORRESPONDING TO THE MINIMUM

C           DEGREE ORDERING;  DIMENSION = N

C

C    IP   - INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE INVERSE OF

C           THE PERMUTATION RETURNED IN P;  DIMENSION = N

C

C    NSP  - DECLARED DIMENSION OF THE ONE-DIMENSIONAL ARRAY ISP;  NSP

C           MUST BE AT LEAST  3N+4K,  WHERE K IS THE NUMBER OF NONZEROES

C           IN THE STRICT UPPER TRIANGLE OF M

C

C    ISP  - INTEGER ONE-DIMENSIONAL ARRAY USED FOR WORKING STORAGE;

C           DIMENSION = NSP

C

C    PATH - INTEGER PATH SPECIFICATION;  VALUES AND THEIR MEANINGS ARE -

C             1  FIND MINIMUM DEGREE ORDERING ONLY

C             2  FIND MINIMUM DEGREE ORDERING AND REORDER SYMMETRICALLY

C                  STORED MATRIX (USED WHEN ONLY THE NONZERO ENTRIES IN

C                  THE UPPER TRIANGLE OF M ARE BEING STORED)

C             3  REORDER SYMMETRICALLY STORED MATRIX AS SPECIFIED BY

C                  INPUT PERMUTATION (USED WHEN AN ORDERING HAS ALREADY

C                  BEEN DETERMINED AND ONLY THE NONZERO ENTRIES IN THE

C                  UPPER TRIANGLE OF M ARE BEING STORED)

C             4  SAME AS 2 BUT PUT DIAGONAL ENTRIES AT START OF EACH ROW

C             5  SAME AS 3 BUT PUT DIAGONAL ENTRIES AT START OF EACH ROW

C

C    FLAG - INTEGER ERROR FLAG;  VALUES AND THEIR MEANINGS ARE -

C               0    NO ERRORS DETECTED

C              9N+K  INSUFFICIENT STORAGE IN MD

C             10N+1  INSUFFICIENT STORAGE IN ODRV

C             11N+1  ILLEGAL PATH SPECIFICATION

C

C

C  CONVERSION FROM REAL TO DOUBLE PRECISION

C

C    CHANGE THE REAL DECLARATIONS IN ODRV AND SRO TO DOUBLE PRECISION

C    DECLARATIONS.

C

C-----------------------------------------------------------------------

C

        INTEGER  IA(N+1), JA(*),  P(N), IP(N),  ISP(NSP),  PATH,  FLAG,

     *     V, L, HEAD,  TMP, Q

C...    REAL  A(1)

        DOUBLE PRECISION  A(*)

        LOGICAL  DFLAG

C

C----INITIALIZE ERROR FLAG AND VALIDATE PATH SPECIFICATION

        FLAG = 0

        IF (PATH.LT.1 .OR. 5.LT.PATH)  GO TO 111

C

C----ALLOCATE STORAGE AND FIND MINIMUM DEGREE ORDERING

        IF ((PATH-1) * (PATH-2) * (PATH-4) .NE. 0)  GO TO 1

          MAX = (NSP-N)/2

          V    = 1

          L    = V     +  MAX

          HEAD = L     +  MAX

          NEXT = HEAD  +  N

          IF (MAX.LT.N)  GO TO 110

C

          CALL  MD

     *       (N, IA,JA, MAX,ISP(V),ISP(L), ISP(HEAD),P,IP, ISP(V), FLAG)

          IF (FLAG.NE.0)  GO TO 100

C

C----ALLOCATE STORAGE AND SYMMETRICALLY REORDER MATRIX

   1    IF ((PATH-2) * (PATH-3) * (PATH-4) * (PATH-5) .NE. 0)  GO TO 2

          TMP = (NSP+1) -      N

          Q   = TMP     - (IA(N+1)-1)

          IF (Q.LT.1)  GO TO 110

C

          DFLAG = PATH.EQ.4 .OR. PATH.EQ.5

          CALL SRO

     *       (N,  IP,  IA, JA, A,  ISP(TMP),  ISP(Q),  DFLAG)

C

   2    RETURN

C

C ** ERROR -- ERROR DETECTED IN MD

 100    RETURN

C ** ERROR -- INSUFFICIENT STORAGE

 110    FLAG = 10*N + 1

        RETURN

C ** ERROR -- ILLEGAL PATH SPECIFIED

 111    FLAG = 11*N + 1

        RETURN

        END

C***********************************************************************

C***********************************************************************

C  MD -- MINIMUM DEGREE ALGORITHM (BASED ON ELEMENT MODEL)

C***********************************************************************

        SUBROUTINE  MD

     *     (N, IA,JA, MAX, V,L, HEAD,LAST,NEXT, MARK, FLAG)

C

C  DESCRIPTION

C

C    MD FINDS A MINIMUM DEGREE ORDERING OF THE ROWS AND COLUMNS OF A

C    SYMMETRIC MATRIX M STORED IN (IA,JA,A) FORMAT.

C

C

C  ADDITIONAL PARAMETERS

C

C    MAX  - DECLARED DIMENSION OF THE ONE-DIMENSIONAL ARRAYS V AND L;

C           MAX MUST BE AT LEAST  N+2K,  WHERE K IS THE NUMBER OF

C           NONZEROES IN THE STRICT UPPER TRIANGLE OF M

C

C    V    - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = MAX

C

C    L    - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = MAX

C

C    HEAD - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N

C

C    LAST - INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE PERMUTATION

C           OF THE ROWS AND COLUMNS OF M CORRESPONDING TO THE MINIMUM

C           DEGREE ORDERING;  DIMENSION = N

C

C    NEXT - INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE INVERSE OF

C           THE PERMUTATION RETURNED IN LAST;  DIMENSION = N

C

C    MARK - INTEGER ONE-DIMENSIONAL WORK ARRAY (MAY BE THE SAME AS V);

C           DIMENSION = N

C

C    FLAG - INTEGER ERROR FLAG;  VALUES AND THEIR MEANINGS ARE -

C             0      NO ERRORS DETECTED

C             11N+1  INSUFFICIENT STORAGE IN MD

C

C

C  DEFINITIONS OF INTERNAL PARAMETERS

C

C    ---------+---------------------------------------------------------

C    V(S)       VALUE FIELD OF LIST ENTRY

C    ---------+---------------------------------------------------------

C    L(S)       LINK FIELD OF LIST ENTRY  (0 => END OF LIST)

C    ---------+---------------------------------------------------------

C    L(VI)      POINTER TO ELEMENT LIST OF UNELIMINATED VERTEX VI

C    ---------+---------------------------------------------------------

C    L(EJ)      POINTER TO BOUNDARY LIST OF ACTIVE ELEMENT EJ

C    ---------+---------------------------------------------------------

C    HEAD(D)    VJ => VJ HEAD OF D-LIST D

C                0 => NO VERTEX IN D-LIST D

C

C

C                                VI UNELIMINATED VERTEX

C                        VI IN EK                   VI NOT IN EK

C    ---------+-----------------------------+---------------------------

C    NEXT(VI)   UNDEFINED BUT NONNEGATIVE     VJ => VJ NEXT IN D-LIST

C                                              0 => VI TAIL OF D-LIST

C    ---------+-----------------------------+---------------------------

C    LAST(VI)   (NOT SET UNTIL MDP)           -D => VI HEAD OF D-LIST D

C              -VK => COMPUTE DEGREE          VJ => VJ LAST IN D-LIST

C               EJ => VI PROTOTYPE OF EJ       0 => VI NOT IN ANY D-LIST

C                0 => DO NOT COMPUTE DEGREE

C    ---------+-----------------------------+---------------------------

C    MARK(VI)   MARK(VK)                      NONNEGATIVE TAG < MARK(VK)

C

C

C                                 VI ELIMINATED VERTEX

C                    EI ACTIVE ELEMENT                  OTHERWISE

C    ---------+-----------------------------+---------------------------

C    NEXT(VI)   -J => VI WAS J-TH VERTEX      -J => VI WAS J-TH VERTEX

C                     TO BE ELIMINATED              TO BE ELIMINATED

C    ---------+-----------------------------+---------------------------

C    LAST(VI)    M => SIZE OF EI = M          UNDEFINED

C    ---------+-----------------------------+---------------------------

C    MARK(VI)   -M => OVERLAP COUNT OF EI     UNDEFINED

C                     WITH EK = M

C               OTHERWISE NONNEGATIVE TAG

C                     < MARK(VK)

C

C-----------------------------------------------------------------------

C

        INTEGER  IA(*), JA(*),  V(*), L(*),  HEAD(*), LAST(*), NEXT(*),

     *     MARK(*),  FLAG,  TAG, DMIN, VK,EK, TAIL

C

C----INITIALIZATION

        TAG = 0

        CALL  MDI

     *     (N, IA,JA, MAX,V,L, HEAD,LAST,NEXT, MARK,TAG, FLAG)

        IF (FLAG.NE.0)  RETURN

C

        K = 0

        DMIN = 1

C

C----WHILE  K < N  DO

   1    IF (K.GE.N)  GO TO 4

C

C------SEARCH FOR VERTEX OF MINIMUM DEGREE

   2      IF (HEAD(DMIN).GT.0)  GO TO 3

            DMIN = DMIN + 1

            GO TO 2

C

C------REMOVE VERTEX VK OF MINIMUM DEGREE FROM DEGREE LIST

   3      VK = HEAD(DMIN)

          HEAD(DMIN) = NEXT(VK)

          IF (HEAD(DMIN).GT.0)  LAST(HEAD(DMIN)) = -DMIN

C

C------NUMBER VERTEX VK, ADJUST TAG, AND TAG VK

          K = K+1

          NEXT(VK) = -K

          LAST(VK) = DMIN - 1

          TAG = TAG + LAST(VK)

          MARK(VK) = TAG

C

C------FORM ELEMENT EK FROM UNELIMINATED NEIGHBORS OF VK

          CALL  MDM

     *       (VK,TAIL, V,L, LAST,NEXT, MARK)

C

C------PURGE INACTIVE ELEMENTS AND DO MASS ELIMINATION

          CALL  MDP

     *       (K,VK,TAIL, V,L, HEAD,LAST,NEXT, MARK)

C

C------UPDATE DEGREES OF UNELIMINATED VERTICES IN EK

          CALL  MDU

     *       (VK,DMIN, V,L, HEAD,LAST,NEXT, MARK)

C

          GO TO 1

C

C----GENERATE INVERSE PERMUTATION FROM PERMUTATION

   4    DO 5 K=1,N

          NEXT(K) = -NEXT(K)

   5      LAST(NEXT(K)) = K

C

        RETURN

        END

C

C***********************************************************************

C  MDI -- INITIALIZATION

C***********************************************************************

        SUBROUTINE  MDI

     *     (N, IA,JA, MAX,V,L, HEAD,LAST,NEXT, MARK,TAG, FLAG)

        INTEGER  IA(*), JA(*),  V(*), L(*),  HEAD(*), LAST(*), NEXT(*),

     *     MARK(*), TAG,  FLAG,  SFS, VI,DVI, VJ

C

C----INITIALIZE DEGREES, ELEMENT LISTS, AND DEGREE LISTS

        DO 1 VI=1,N

          MARK(VI) = 1

          L(VI) = 0

   1      HEAD(VI) = 0

        SFS = N+1

C

C----CREATE NONZERO STRUCTURE

C----FOR EACH NONZERO ENTRY A(VI,VJ) IN STRICT UPPER TRIANGLE

        DO 3 VI=1,N

          JMIN = IA(VI)

          JMAX = IA(VI+1) - 1

          IF (JMIN.GT.JMAX)  GO TO 3

          DO 2 J=JMIN,JMAX

            VJ = JA(J)

            IF (VI.GE.VJ)  GO TO 2

              IF (SFS.GE.MAX)  GO TO 101

C

C------ENTER VJ IN ELEMENT LIST FOR VI

              MARK(VI) = MARK(VI) + 1

              V(SFS) = VJ

              L(SFS) = L(VI)

              L(VI) = SFS

              SFS = SFS+1

C

C------ENTER VI IN ELEMENT LIST FOR VJ

              MARK(VJ) = MARK(VJ) + 1

              V(SFS) = VI

              L(SFS) = L(VJ)

              L(VJ) = SFS

              SFS = SFS+1

   2        CONTINUE

   3      CONTINUE

C

C----CREATE DEGREE LISTS AND INITIALIZE MARK VECTOR

        DO 4 VI=1,N

          DVI = MARK(VI)

          NEXT(VI) = HEAD(DVI)

          HEAD(DVI) = VI

          LAST(VI) = -DVI

          IF (NEXT(VI).GT.0)  LAST(NEXT(VI)) = VI

   4      MARK(VI) = TAG

C

        RETURN

C

C ** ERROR -- INSUFFICIENT STORAGE

 101    FLAG = 9*N + VI

        RETURN

        END

C

C***********************************************************************

C  MDM -- FORM ELEMENT FROM UNELIMINATED NEIGHBORS OF VK

C***********************************************************************

        SUBROUTINE  MDM

     *     (VK,TAIL, V,L, LAST,NEXT, MARK)

        INTEGER  VK, TAIL,  V(*), L(*),   LAST(*), NEXT(*),   MARK(*),

     *     TAG, S,LS,VS,ES, B,LB,VB, BLP,BLPMAX

C

C----INITIALIZE TAG AND LIST OF UNELIMINATED NEIGHBORS

        TAG = MARK(VK)

        TAIL = VK

C

C----FOR EACH VERTEX/ELEMENT VS/ES IN ELEMENT LIST OF VK

        LS = L(VK)

   1    S = LS

        IF (S.EQ.0)  GO TO 5

          LS = L(S)

          VS = V(S)

          IF (NEXT(VS).LT.0)  GO TO 2

C

C------IF VS IS UNELIMINATED VERTEX, THEN TAG AND APPEND TO LIST OF

C------UNELIMINATED NEIGHBORS

            MARK(VS) = TAG

            L(TAIL) = S

            TAIL = S

            GO TO 4

C

C------IF ES IS ACTIVE ELEMENT, THEN ...

C--------FOR EACH VERTEX VB IN BOUNDARY LIST OF ELEMENT ES

   2        LB = L(VS)

            BLPMAX = LAST(VS)

            DO 3 BLP=1,BLPMAX

              B = LB

              LB = L(B)

              VB = V(B)

C

C----------IF VB IS UNTAGGED VERTEX, THEN TAG AND APPEND TO LIST OF

C----------UNELIMINATED NEIGHBORS

              IF (MARK(VB).GE.TAG)  GO TO 3

                MARK(VB) = TAG

                L(TAIL) = B

                TAIL = B

   3          CONTINUE

C

C--------MARK ES INACTIVE

            MARK(VS) = TAG

C

   4      GO TO 1

C

C----TERMINATE LIST OF UNELIMINATED NEIGHBORS

   5    L(TAIL) = 0

C

        RETURN

        END

C

C***********************************************************************

C  MDP -- PURGE INACTIVE ELEMENTS AND DO MASS ELIMINATION

C***********************************************************************

        SUBROUTINE  MDP

     *     (K,EK,TAIL, V,L, HEAD,LAST,NEXT, MARK)

        INTEGER  EK, TAIL,  V(*), L(*),  HEAD(*), LAST(*), NEXT(*),

     *     MARK(*),  TAG, FREE, LI,VI,LVI,EVI, S,LS,VS, ILP,ILPMAX

C

C----INITIALIZE TAG

        TAG = MARK(EK)

C

C----FOR EACH VERTEX VI IN EK

        LI = EK

        ILPMAX = LAST(EK)

        IF (ILPMAX.LE.0)  GO TO 12

        DO 11 ILP=1,ILPMAX

          I = LI

          LI = L(I)

          VI = V(LI)

C

C------REMOVE VI FROM DEGREE LIST

          IF (LAST(VI).EQ.0)  GO TO 3

            IF (LAST(VI).GT.0)  GO TO 1

              HEAD(-LAST(VI)) = NEXT(VI)

              GO TO 2

   1          NEXT(LAST(VI)) = NEXT(VI)

   2        IF (NEXT(VI).GT.0)  LAST(NEXT(VI)) = LAST(VI)

C

C------REMOVE INACTIVE ITEMS FROM ELEMENT LIST OF VI

   3      LS = VI

   4      S = LS

          LS = L(S)

          IF (LS.EQ.0)  GO TO 6

            VS = V(LS)

            IF (MARK(VS).LT.TAG)  GO TO 5

              FREE = LS

              L(S) = L(LS)

              LS = S

   5        GO TO 4

C

C------IF VI IS INTERIOR VERTEX, THEN REMOVE FROM LIST AND ELIMINATE

   6      LVI = L(VI)

          IF (LVI.NE.0)  GO TO 7

            L(I) = L(LI)

            LI = I

C

            K = K+1

            NEXT(VI) = -K

            LAST(EK) = LAST(EK) - 1

            GO TO 11

C

C------ELSE ...

C--------CLASSIFY VERTEX VI

   7        IF (L(LVI).NE.0)  GO TO 9

              EVI = V(LVI)

              IF (NEXT(EVI).GE.0)  GO TO 9

                IF (MARK(EVI).LT.0)  GO TO 8

C

C----------IF VI IS PROTOTYPE VERTEX, THEN MARK AS SUCH, INITIALIZE

C----------OVERLAP COUNT FOR CORRESPONDING ELEMENT, AND MOVE VI TO END

C----------OF BOUNDARY LIST

                  LAST(VI) = EVI

                  MARK(EVI) = -1

                  L(TAIL) = LI

                  TAIL = LI

                  L(I) = L(LI)

                  LI = I

                  GO TO 10

C

C----------ELSE IF VI IS DUPLICATE VERTEX, THEN MARK AS SUCH AND ADJUST

C----------OVERLAP COUNT FOR CORRESPONDING ELEMENT

   8              LAST(VI) = 0

                  MARK(EVI) = MARK(EVI) - 1

                  GO TO 10

C

C----------ELSE MARK VI TO COMPUTE DEGREE

   9              LAST(VI) = -EK

C

C--------INSERT EK IN ELEMENT LIST OF VI

  10        V(FREE) = EK

            L(FREE) = L(VI)

            L(VI) = FREE

  11      CONTINUE

C

C----TERMINATE BOUNDARY LIST

  12    L(TAIL) = 0

C

        RETURN

        END

C

C***********************************************************************

C  MDU -- UPDATE DEGREES OF UNELIMINATED VERTICES IN EK

C***********************************************************************

        SUBROUTINE  MDU

     *     (EK,DMIN, V,L, HEAD,LAST,NEXT, MARK)

        INTEGER  EK, DMIN,  V(*), L(*),  HEAD(*), LAST(*), NEXT(*),

     *     MARK(*),  TAG, VI,EVI,DVI, S,VS,ES, B,VB, ILP,ILPMAX,

     *     BLP,BLPMAX

C

C----INITIALIZE TAG

        TAG = MARK(EK) - LAST(EK)

C

C----FOR EACH VERTEX VI IN EK

        I = EK

        ILPMAX = LAST(EK)

        IF (ILPMAX.LE.0)  GO TO 11

        DO 10 ILP=1,ILPMAX

          I = L(I)

          VI = V(I)

          IF (LAST(VI))  1, 10, 8

C

C------IF VI NEITHER PROTOTYPE NOR DUPLICATE VERTEX, THEN MERGE ELEMENTS

C------TO COMPUTE DEGREE

   1        TAG = TAG + 1

            DVI = LAST(EK)

C

C--------FOR EACH VERTEX/ELEMENT VS/ES IN ELEMENT LIST OF VI

            S = L(VI)

   2        S = L(S)

            IF (S.EQ.0)  GO TO 9

              VS = V(S)

              IF (NEXT(VS).LT.0)  GO TO 3

C

C----------IF VS IS UNELIMINATED VERTEX, THEN TAG AND ADJUST DEGREE

                MARK(VS) = TAG

                DVI = DVI + 1

                GO TO 5

C

C----------IF ES IS ACTIVE ELEMENT, THEN EXPAND

C------------CHECK FOR OUTMATCHED VERTEX

   3            IF (MARK(VS).LT.0)  GO TO 6

C

C------------FOR EACH VERTEX VB IN ES

                B = VS

                BLPMAX = LAST(VS)

                DO 4 BLP=1,BLPMAX

                  B = L(B)

                  VB = V(B)

C

C--------------IF VB IS UNTAGGED, THEN TAG AND ADJUST DEGREE

                  IF (MARK(VB).GE.TAG)  GO TO 4

                    MARK(VB) = TAG

                    DVI = DVI + 1

   4              CONTINUE

C

   5          GO TO 2

C

C------ELSE IF VI IS OUTMATCHED VERTEX, THEN ADJUST OVERLAPS BUT DO NOT

C------COMPUTE DEGREE

   6        LAST(VI) = 0

            MARK(VS) = MARK(VS) - 1

   7        S = L(S)

            IF (S.EQ.0)  GO TO 10

              VS = V(S)

              IF (MARK(VS).LT.0)  MARK(VS) = MARK(VS) - 1

              GO TO 7

C

C------ELSE IF VI IS PROTOTYPE VERTEX, THEN CALCULATE DEGREE BY

C------INCLUSION/EXCLUSION AND RESET OVERLAP COUNT

   8        EVI = LAST(VI)

            DVI = LAST(EK) + LAST(EVI) + MARK(EVI)

            MARK(EVI) = 0

C

C------INSERT VI IN APPROPRIATE DEGREE LIST

   9      NEXT(VI) = HEAD(DVI)

          HEAD(DVI) = VI

          LAST(VI) = -DVI

          IF (NEXT(VI).GT.0)  LAST(NEXT(VI)) = VI

          IF (DVI.LT.DMIN)  DMIN = DVI

C

  10      CONTINUE

C

  11    RETURN

        END

C***********************************************************************

C***********************************************************************

C  SRO -- SYMMETRIC REORDERING OF SPARSE SYMMETRIC MATRIX

C***********************************************************************

        SUBROUTINE  SRO

     *     (N, IP, IA,JA,A, Q, R, DFLAG)

C

C  DESCRIPTION

C

C    THE NONZERO ENTRIES OF THE MATRIX M ARE ASSUMED TO BE STORED

C    SYMMETRICALLY IN (IA,JA,A) FORMAT (I.E., NOT BOTH M(I,J) AND M(J,I)

C    ARE STORED IF I NE J).

C

C    SRO DOES NOT REARRANGE THE ORDER OF THE ROWS, BUT DOES MOVE

C    NONZEROES FROM ONE ROW TO ANOTHER TO ENSURE THAT IF M(I,J) WILL BE

C    IN THE UPPER TRIANGLE OF M WITH RESPECT TO THE NEW ORDERING, THEN

C    M(I,J) IS STORED IN ROW I (AND THUS M(J,I) IS NOT STORED);  WHEREAS

C    IF M(I,J) WILL BE IN THE STRICT LOWER TRIANGLE OF M, THEN M(J,I) IS

C    STORED IN ROW J (AND THUS M(I,J) IS NOT STORED).

C

C

C  ADDITIONAL PARAMETERS

C

C    Q     - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N

C

C    R     - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = NUMBER OF

C            NONZERO ENTRIES IN THE UPPER TRIANGLE OF M

C

C    DFLAG - LOGICAL VARIABLE;  IF DFLAG = .TRUE., THEN STORE NONZERO

C            DIAGONAL ELEMENTS AT THE BEGINNING OF THE ROW

C

C-----------------------------------------------------------------------

C

        INTEGER  IP(*),  IA(*), JA(*),  Q(*), R(*)

C...    REAL  A(1),  AK

        DOUBLE PRECISION  A(*),  AK

        LOGICAL  DFLAG

C

C

C--PHASE 1 -- FIND ROW IN WHICH TO STORE EACH NONZERO

C----INITIALIZE COUNT OF NONZEROES TO BE STORED IN EACH ROW

        DO 1 I=1,N

  1       Q(I) = 0

C

C----FOR EACH NONZERO ELEMENT A(J)

        DO 3 I=1,N

          JMIN = IA(I)

          JMAX = IA(I+1) - 1

          IF (JMIN.GT.JMAX)  GO TO 3

          DO 2 J=JMIN,JMAX

C

C--------FIND ROW (=R(J)) AND COLUMN (=JA(J)) IN WHICH TO STORE A(J) ...

            K = JA(J)

            IF (IP(K).LT.IP(I))  JA(J) = I

            IF (IP(K).GE.IP(I))  K = I

            R(J) = K

C

C--------... AND INCREMENT COUNT OF NONZEROES (=Q(R(J)) IN THAT ROW

  2         Q(K) = Q(K) + 1

  3       CONTINUE

C

C

C--PHASE 2 -- FIND NEW IA AND PERMUTATION TO APPLY TO (JA,A)

C----DETERMINE POINTERS TO DELIMIT ROWS IN PERMUTED (JA,A)

        DO 4 I=1,N

          IA(I+1) = IA(I) + Q(I)

  4       Q(I) = IA(I+1)

C

C----DETERMINE WHERE EACH (JA(J),A(J)) IS STORED IN PERMUTED (JA,A)

C----FOR EACH NONZERO ELEMENT (IN REVERSE ORDER)

        ILAST = 0

        JMIN = IA(1)

        JMAX = IA(N+1) - 1

        J = JMAX

        DO 6 JDUMMY=JMIN,JMAX

          I = R(J)

          IF (.NOT.DFLAG .OR. JA(J).NE.I .OR. I.EQ.ILAST)  GO TO 5

C

C------IF DFLAG, THEN PUT DIAGONAL NONZERO AT BEGINNING OF ROW

            R(J) = IA(I)

            ILAST = I

            GO TO 6

C

C------PUT (OFF-DIAGONAL) NONZERO IN LAST UNUSED LOCATION IN ROW

  5         Q(I) = Q(I) - 1

            R(J) = Q(I)

C

  6       J = J-1

C

C

C--PHASE 3 -- PERMUTE (JA,A) TO UPPER TRIANGULAR FORM (WRT NEW ORDERING)

        DO 8 J=JMIN,JMAX

  7       IF (R(J).EQ.J)  GO TO 8

            K = R(J)

            R(J) = R(K)

            R(K) = K

            JAK = JA(K)

            JA(K) = JA(J)

            JA(J) = JAK

            AK = A(K)

            A(K) = A(J)

            A(J) = AK

            GO TO 7

  8       CONTINUE

C

        RETURN

        END

C                                                                1/15/81

C***********************************************************************

C  SDRV -- DRIVER FOR SPARSE SYMMETRIC POSITIVE DEFINITE MATRIX ROUTINES

C***********************************************************************

        SUBROUTINE SDRVD

     *     (N, P,IP, IA,JA,A, B, Z, NSP,ISP,RSP,ESP, PATH, FLAG)

C

C  DESCRIPTION

C

C    SDRV SOLVES SPARSE SYMMETRIC POSITIVE DEFINITE SYSTEMS OF LINEAR

C    EQUATIONS.  THE SOLUTION PROCESS IS DIVIDED INTO THREE STAGES --

C

C      SSF - THE COEFFICIENT MATRIX M IS FACTORED SYMBOLICALLY TO

C            DETERMINE WHERE FILLIN WILL OCCUR DURING THE NUMERIC

C            FACTORIZATION.

C

C      SNF - M IS FACTORED NUMERICALLY INTO THE PRODUCT UT-D-U, WHERE

C            D IS DIAGONAL AND U IS UNIT UPPER TRIANGULAR.

C

C      SNS - THE LINEAR SYSTEM  MX = B  IS SOLVED USING THE UT-D-U

C            FACTORIZATION FROM SNF.

C

C    FOR SEVERAL SYSTEMS WITH THE SAME COEFFICIENT MATRIX, SSF AND SNF

C    NEED BE DONE ONLY ONCE (FOR THE FIRST SYSTEM);  THEN SNS IS DONE

C    ONCE FOR EACH ADDITIONAL RIGHT-HAND SIDE.  FOR SEVERAL SYSTEMS

C    WHOSE COEFFICIENT MATRICES HAVE THE SAME NONZERO STRUCTURE, SSF

C    NEED BE DONE ONLY ONCE (FOR THE FIRST SYSTEM);  THEN SNF AND SNS

C    ARE DONE ONCE FOR EACH ADDITIONAL SYSTEM.

C

C

C  STORAGE OF SPARSE MATRICES

C

C    THE NONZERO ENTRIES OF THE MATRIX M ARE STORED ROW-BY-ROW IN THE

C    ARRAY A.  TO IDENTIFY THE INDIVIDUAL NONZERO ENTRIES IN EACH ROW,

C    WE NEED TO KNOW IN WHICH COLUMN EACH ENTRY LIES.  THESE COLUMN

C    INDICES ARE STORED IN THE ARRAY JA;  I.E., IF  A(K) = M(I,J),  THEN

C    JA(K) = J.  TO IDENTIFY THE INDIVIDUAL ROWS, WE NEED TO KNOW WHERE

C    EACH ROW STARTS.  THESE ROW POINTERS ARE STORED IN THE ARRAY IA;

C    I.E., IF M(I,J) IS THE FIRST NONZERO ENTRY (STORED) IN THE I-TH ROW

C    AND  A(K) = M(I,J),  THEN  IA(I) = K.  MOREOVER, IA(N+1) POINTS TO

C    THE FIRST LOCATION FOLLOWING THE LAST ELEMENT IN THE LAST ROW.

C    THUS, THE NUMBER OF ENTRIES IN THE I-TH ROW IS  IA(I+1) - IA(I),

C    THE NONZERO ENTRIES IN THE I-TH ROW ARE STORED CONSECUTIVELY IN

C

C            A(IA(I)),  A(IA(I)+1),  ..., A(IA(I+1)-1),

C

C    AND THE CORRESPONDING COLUMN INDICES ARE STORED CONSECUTIVELY IN

C

C            JA(IA(I)), JA(IA(I)+1), ..., JA(IA(I+1)-1).

C

C    SINCE THE COEFFICIENT MATRIX IS SYMMETRIC, ONLY THE NONZERO ENTRIES

C    IN THE UPPER TRIANGLE NEED BE STORED, FOR EXAMPLE, THE MATRIX

C

C             ( 1  0  2  3  0 )

C             ( 0  4  0  0  0 )

C         M = ( 2  0  5  6  0 )

C             ( 3  0  6  7  8 )

C             ( 0  0  0  8  9 )

C

C    COULD BE STORED AS

C

C              1  2  3  4  5  6  7  8  9 10 11 12 13

C         ---+--------------------------------------

C         IA   1  4  5  8 12 14

C         JA   1  3  4  2  1  3  4  1  3  4  5  4  5

C          A   1  2  3  4  2  5  6  3  6  7  8  8  9

C

C    OR (SYMMETRICALLY) AS

C

C              1  2  3  4  5  6  7  8  9

C         ---+--------------------------

C         IA   1  4  5  7  9 10

C         JA   1  3  4  2  3  4  4  5  5

C          A   1  2  3  4  5  6  7  8  9          .

C

C

C  REORDERING THE ROWS AND COLUMNS OF M

C

C    A SYMMETRIC PERMUTATION OF THE ROWS AND COLUMNS OF THE COEFFICIENT

C    MATRIX M (E.G., WHICH REDUCES FILLIN OR ENHANCES NUMERICAL

C    STABILITY) MUST BE SPECIFIED.  THE SOLUTION Z IS RETURNED IN THE

C    ORIGINAL ORDER.

C

C    TO SPECIFY THE TRIVIAL ORDERING (I.E., THE IDENTITY PERMUTATION),

C    SET  P(I) = IP(I) = I,  I=1,...,N.  IN THIS CASE, P AND IP CAN BE

C    THE SAME ARRAY.

C

C    IF A NONTRIVIAL ORDERING (I.E., NOT THE IDENTITY PERMUTATION) IS

C    SPECIFIED AND M IS STORED SYMMETRICALLY (I.E., NOT BOTH M(I,J) AND

C    M(J,I) ARE STORED FOR I NE J), THEN ODRV SHOULD BE CALLED (WITH

C    PATH = 3 OR 5) TO SYMMETRICALLY REORDER (IA,JA,A) BEFORE CALLING

C    SDRV.  THIS IS TO ENSURE THAT IF M(I,J) WILL BE IN THE UPPER

C    TRIANGLE OF M WITH RESPECT TO THE NEW ORDERING, THEN M(I,J) IS

C    STORED IN ROW I (AND THUS M(J,I) IS NOT STORED);  WHEREAS IF M(I,J)

C    WILL BE IN THE STRICT LOWER TRIANGLE OF M, THEN M(J,I) IS STORED IN

C    ROW J (AND THUS M(I,J) IS NOT STORED).

C

C

C  PARAMETERS

C

C    N    - NUMBER OF VARIABLES/EQUATIONS

C

C    P    - INTEGER ONE-DIMENSIONAL ARRAY SPECIFYING A PERMUTATION OF

C           THE ROWS AND COLUMNS OF M;  DIMENSION = N

C

C    IP   - INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE INVERSE OF THE

C           PERMUTATION SPECIFIED IN P;  I.E., IP(P(I)) = I, I=1,...,N;

C           DIMENSION = N

C

C    IA   - INTEGER ONE-DIMENSIONAL ARRAY CONTAINING POINTERS TO DELIMIT

C           ROWS IN JA AND A;  DIMENSION = N+1

C

C    JA   - INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE COLUMN INDICES

C           CORRESPONDING TO THE ELEMENTS OF A;  DIMENSION = NUMBER OF

C           NONZERO ENTRIES IN M STORED

C

C    A    - REAL ONE-DIMENSIONAL ARRAY CONTAINING THE NONZERO ENTRIES IN

C           THE COEFFICIENT MATRIX M, STORED BY ROWS;  DIMENSION =

C           NUMBER OF NONZERO ENTRIES IN M STORED

C

C    B    - REAL ONE-DIMENSIONAL ARRAY CONTAINING THE RIGHT-HAND SIDE B;

C           B AND Z CAN BE THE SAME ARRAY;  DIMENSION = N

C

C    Z    - REAL ONE-DIMENSIONAL ARRAY CONTAINING THE SOLUTION X;  Z AND

C           B CAN BE THE SAME ARRAY;  DIMENSION = N

C

C    NSP  - DECLARED DIMENSION OF THE ONE-DIMENSIONAL ARRAYS ISP AND

C           RSP;  NSP MUST BE (SUBSTANTIALLY) LARGER THAN  3N+2K,  WHERE

C           K = NUMBER OF NONZERO ENTRIES IN THE UPPER TRIANGLE OF M

C

C    ISP  - INTEGER ONE-DIMENSIONAL ARRAY USED FOR WORKING STORAGE;  ISP

C           AND RSP SHOULD BE EQUIVALENCED;  DIMENSION = NSP

C

C    RSP  - REAL ONE-DIMENSIONAL ARRAY USED FOR WORKING STORAGE;  RSP

C           AND ISP SHOULD BE EQUIVALENCED;  DIMENSION = NSP

C

C    ESP  - INTEGER VARIABLE;  IF SUFFICIENT STORAGE WAS AVAILABLE TO

C           PERFORM THE SYMBOLIC FACTORIZATION (SSF), THEN ESP IS SET TO

C           THE AMOUNT OF EXCESS STORAGE PROVIDED (NEGATIVE IF

C           INSUFFICIENT STORAGE WAS AVAILABLE TO PERFORM THE NUMERIC

C           FACTORIZATION (SNF))

C

C    PATH - INTEGER PATH SPECIFICATION;  VALUES AND THEIR MEANINGS ARE -

C             1  PERFORM SSF, SNF, AND SNS

C             2  PERFORM SNF AND SNS (ISP/RSP IS ASSUMED TO HAVE BEEN

C                  SET UP IN AN EARLIER CALL TO SDRV (FOR SSF))

C             3  PERFORM SNS ONLY (ISP/RSP IS ASSUMED TO HAVE BEEN SET

C                  UP IN AN EARLIER CALL TO SDRV (FOR SSF AND SNF))

C             4  PERFORM SSF

C             5  PERFORM SSF AND SNF

C             6  PERFORM SNF ONLY (ISP/RSP IS ASSUMED TO HAVE BEEN SET

C                  UP IN AN EARLIER CALL TO SDRV (FOR SSF))

C

C    FLAG - INTEGER ERROR FLAG;  VALUES AND THEIR MEANINGS ARE -

C               0     NO ERRORS DETECTED

C              2N+K   DUPLICATE ENTRY IN A  --  ROW = K

C              6N+K   INSUFFICIENT STORAGE IN SSF  --  ROW = K

C              7N+1   INSUFFICIENT STORAGE IN SNF

C              8N+K   ZERO PIVOT  --  ROW = K

C             10N+1   INSUFFICIENT STORAGE IN SDRV

C             11N+1   ILLEGAL PATH SPECIFICATION

C

C

C  CONVERSION FROM REAL TO DOUBLE PRECISION

C

C    CHANGE THE REAL DECLARATIONS IN SDRV, SNF, AND SNS TO DOUBLE

C    PRECISION DECLARATIONS;  AND CHANGE THE VALUE IN THE DATA STATEMENT

C    FOR THE INTEGER VARIABLE RATIO (IN SDRV) FROM 1 TO 2.

C

C-----------------------------------------------------------------------

C

        INTEGER  P(*), IP(*),  IA(*), JA(*),  ISP(*), ESP,  PATH,  FLAG,

     *     RATIO,  Q, MARK, D, U, TMP,  UMAX

C...    REAL  A(1),  B(1),  Z(1),  RSP(1)

C...    DATA  RATIO/1/

        DOUBLE PRECISION  A(*),  B(*),  Z(*),  RSP(*)

        DATA  RATIO/1/

C

C----VALIDATE PATH SPECIFICATION

        IF (PATH.LT.1 .OR. 6.LT.PATH)  GO TO 111

C

C----ALLOCATE STORAGE AND FACTOR M SYMBOLICALLY TO DETERMINE FILL-IN

        IJU   = 1

        IU    = IJU     +  N

        JL    = IU      + N+1

        JU    = JL      +  N

        Q     = (NSP+1) -  N

        MARK  = Q       -  N

        JUMAX = MARK    - JU

C

        IF ((PATH-1) * (PATH-4) * (PATH-5) .NE. 0)  GO TO 1

          IF (JUMAX.LE.0)  GO TO 110

          CALL SSF

     *       (N,  P, IP,  IA, JA,  ISP(IJU), ISP(JU), ISP(IU), JUMAX,

     *        ISP(Q), ISP(MARK), ISP(JL), FLAG)

          IF (FLAG.NE.0)  GO TO 100

C

C----ALLOCATE STORAGE AND FACTOR M NUMERICALLY

   1    IL   = JU      + ISP(IJU+(N-1))

        TMP  = ((IL-1)+(RATIO-1)) / RATIO  +  1

        D    = TMP     + N

        U    = D       + N

        UMAX = (NSP+1) - U

        ESP  = UMAX    - (ISP(IU+N)-1)

C

        IF ((PATH-1) * (PATH-2) * (PATH-5) * (PATH-6) .NE. 0)  GO TO 2

          IF (UMAX.LE.0)  GO TO 110

          CALL SNF

     *       (N,  P, IP,  IA, JA, A,

     *        RSP(D),  ISP(IJU), ISP(JU), ISP(IU), RSP(U), UMAX,

     *        ISP(IL),  ISP(JL),  FLAG)

          IF (FLAG.NE.0)  GO TO 100

C

C----SOLVE SYSTEM OF LINEAR EQUATIONS  MX = B

   2    IF ((PATH-1) * (PATH-2) * (PATH-3) .NE. 0)  GO TO 3

          IF (UMAX.LE.0)  GO TO 110

          CALL SNS

     *       (N,  P,  RSP(D), ISP(IJU), ISP(JU), ISP(IU), RSP(U),  Z, B,

     *        RSP(TMP))

C

   3    RETURN

C

C ** ERROR -- ERROR DETECTED IN SSF, SNF, OR SNS

 100    RETURN

C ** ERROR -- INSUFFICIENT STORAGE

 110    FLAG = 10*N + 1

        RETURN

C ** ERROR -- ILLEGAL PATH SPECIFICATION

 111    FLAG = 11*N + 1

        RETURN

        END

C

C

C***********************************************************************

C  INTERNAL DOCUMENTATION FOR SSF, SNF, AND SNS

C***********************************************************************

C

C  COMPRESSED STORAGE OF SPARSE MATRICES

C

C    THE STRICT UPPER TRIANGULAR PORTION OF THE MATRIX U IS STORED IN

C    (IA,JA,A) FORMAT USING THE ARRAYS IU, JU, AND U, EXCEPT THAT AN

C    ADDITIONAL ARRAY IJU IS USED TO REDUCE THE STORAGE REQUIRED FOR JU

C    BY ALLOWING SOME SEQUENCES OF COLUMN INDICES TO CORRESPOND TO MORE

C    THAN ONE ROW.  FOR I < N, IJU(I) IS THE INDEX IN JU OF THE FIRST

C    ENTRY FOR THE I-TH ROW;  IJU(N) IS THE NUMBER OF ENTRIES IN JU.

C    THUS, THE NUMBER OF ENTRIES IN THE I-TH ROW IS  IU(I+1) - IU(I),

C    THE NONZERO ENTRIES OF THE I-TH ROW ARE STORED CONSECUTIVELY IN

C

C        U(IU(I)),   U(IU(I)+1),   ..., U(IU(I+1)-1),

C

C    AND THE CORRESPONDING COLUMN INDICES ARE STORED CONSECUTIVELY IN

C

C        JU(IJU(I)), JU(IJU(I)+1), ..., JU(IJU(I)+IU(I+1)-IU(I)+1).

C

C    COMPRESSION IN JU OCCURS IN TWO WAYS.  FIRST, IF A ROW I WAS MERGED

C    INTO ROW K, AND THE NUMBER OF ELEMENTS MERGED IN FROM (THE TAIL

C    PORTION OF) ROW I IS THE SAME AS THE FINAL LENGTH OF ROW K, THEN

C    THE KTH ROW AND THE TAIL OF ROW I ARE IDENTICAL AND IJU(K) POINTS

C    TO THE START OF THE TAIL.  SECOND, IF SOME TAIL PORTION OF THE

C    (K-1)ST ROW IS IDENTICAL TO THE HEAD OF THE KTH ROW, THEN IJU(K)

C    POINTS TO THE START OF THAT TAIL PORTION.  FOR EXAMPLE, THE NONZERO

C    STRUCTURE OF THE STRICT UPPER TRIANGULAR PART OF THE MATRIX

C

C             ( D 0 0 0 X X X )

C             ( 0 D 0 X X 0 0 )

C             ( 0 0 D 0 X X 0 )

C         U = ( 0 0 0 D X X 0 )

C             ( 0 0 0 0 D X X )

C             ( 0 0 0 0 0 D X )

C             ( 0 0 0 0 0 0 D )

C

C    WOULD BE STORED AS

C

C               1  2  3  4  5  6  7  8

C         ----+------------------------

C          IU   1  4  6  8 10 12 12 12

C          JU   5  6  7  4  5  6

C         IJU   1  4  5  5  2  3  6           .

C

C    THE DIAGONAL ENTRIES OF U ARE EQUAL TO ONE AND ARE NOT STORED.

C

C

C  ADDITIONAL PARAMETERS

C

C    D     - REAL ONE-DIMENSIONAL ARRAY CONTAINING THE RECIPROCALS OF

C            THE DIAGONAL ENTRIES OF THE MATRIX D;  DIMENSION = N

C

C    IJU   - INTEGER ONE-DIMENSIONAL ARRAY CONTAINING POINTERS TO THE

C            START OF EACH ROW IN JU;  DIMENSION = N

C

C    IU    - INTEGER ONE-DIMENSIONAL ARRAY CONTAINING POINTERS TO

C            DELIMIT ROWS IN U;  DIMENSION = N+1

C

C    JU    - INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE COLUMN INDICES

C            CORRESPONDING TO THE ELEMENTS OF U;  DIMENSION = JUMAX

C

C    JUMAX - DECLARED DIMENSION OF THE ONE-DIMENSIONAL ARRAY JU;  JUMAX

C            MUST BE AT LEAST THE SIZE OF U MINUS COMPRESSION (IJU(N)

C            AFTER THE CALL TO SSF)

C

C    U     - REAL ONE-DIMENSIONAL ARRAY CONTAINING THE NONZERO ENTRIES

C            IN THE STRICT UPPER TRIANGLE OF U, STORED BY ROWS;

C            DIMENSION = UMAX

C

C    UMAX  - DECLARED DIMENSION OF THE ONE-DIMENSIONAL ARRAY U;  UMAX

C            MUST BE AT LEAST THE NUMBER OF NONZERO ENTRIES IN THE

C            STRICT UPPER TRIANGLE OF M PLUS FILLIN (IU(N+1)-1 AFTER THE

C            CALL TO SSF)

C

C

C***********************************************************************

C  SSF --  SYMBOLIC UT-D-U FACTORIZATION OF SPARSE SYMMETRIC MATRIX

C***********************************************************************

        SUBROUTINE  SSF

     *     (N, P,IP, IA,JA, IJU,JU,IU,JUMAX, Q, MARK, JL, FLAG)

C

C  ADDITIONAL PARAMETERS

C

C    Q     - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N

C

C    MARK  - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N

C

C    JL    - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N

C

C

C  DEFINITIONS OF INTERNAL PARAMETERS (DURING K-TH STAGE OF ELIMINATION)

C

C    Q CONTAINS AN ORDERED LINKED LIST REPRESENTATION OF THE NONZERO

C      STRUCTURE OF THE K-TH ROW OF U --

C        Q(K) IS THE FIRST COLUMN WITH A NONZERO ENTRY

C        Q(I) IS THE NEXT COLUMN WITH A NONZERO ENTRY AFTER COLUMN I

C      IN EITHER CASE, Q(I) = N+1 INDICATES THE END OF THE LIST

C

C    JL CONTAINS LISTS OF ROWS TO BE MERGED INTO UNELIMINATED ROWS --

C        I GE K => JL(I) IS THE FIRST ROW TO BE MERGED INTO ROW I

C        I LT K => JL(I) IS THE ROW FOLLOWING ROW I IN SOME LIST OF ROWS

C      IN EITHER CASE, JL(I) = 0 INDICATES THE END OF A LIST

C

C    MARK(I) IS THE LAST ROW STORED IN JU FOR WHICH U(MARK(I),I) NE 0

C

C    JUMIN AND JUPTR ARE THE INDICES IN JU OF THE FIRST AND LAST

C      ELEMENTS IN THE LAST ROW SAVED IN JU

C

C    LUK IS THE NUMBER OF NONZERO ENTRIES IN THE K-TH ROW

C

C-----------------------------------------------------------------------

C

        INTEGER  P(*), IP(*),  IA(*), JA(*),  IJU(*), JU(*), IU(*),

     *     Q(*),  MARK(*),  JL(*),  FLAG,  TAG, VJ, QM

        LOGICAL  CLIQUE

C

C----INITIALIZATION

        JUMIN = 1

        JUPTR = 0

        IU(1) = 1

        DO 1 K=1,N

          MARK(K) = 0

   1      JL(K) = 0

C

C----FOR EACH ROW K

        DO 18 K=1,N

          LUK = 0

          Q(K) = N+1

C

          TAG = MARK(K)

          CLIQUE = .FALSE.

          IF (JL(K).NE.0)  CLIQUE = JL(JL(K)).EQ.0

C

C------INITIALIZE NONZERO STRUCTURE OF K-TH ROW TO ROW P(K) OF M

          JMIN = IA(P(K))

          JMAX = IA(P(K)+1) - 1

          IF (JMIN.GT.JMAX)  GO TO 4

          DO 3 J=JMIN,JMAX

            VJ = IP(JA(J))

            IF (VJ.LE.K)  GO TO 3

C

              QM = K

   2          M = QM

              QM = Q(M)

              IF (QM.LT.VJ)  GO TO 2

              IF (QM.EQ.VJ)  GO TO 102

                LUK = LUK+1

                Q(M) = VJ

                Q(VJ) = QM

                IF (MARK(VJ).NE.TAG)  CLIQUE = .FALSE.

C

   3        CONTINUE

C

C------IF EXACTLY ONE ROW IS TO BE MERGED INTO THE K-TH ROW AND THERE IS

C------A NONZERO ENTRY IN EVERY COLUMN IN THAT ROW IN WHICH THERE IS A

C------NONZERO ENTRY IN ROW P(K) OF M, THEN DO NOT COMPUTE FILL-IN, JUST

C------USE THE COLUMN INDICES FOR THE ROW WHICH WAS TO HAVE BEEN MERGED

   4      IF (.NOT.CLIQUE)  GO TO 5

            IJU(K) = IJU(JL(K)) + 1

            LUK = IU(JL(K)+1) - (IU(JL(K))+1)

            GO TO 17

C

C------MODIFY NONZERO STRUCTURE OF K-TH ROW BY COMPUTING FILL-IN

C------FOR EACH ROW I TO BE MERGED IN

   5      LMAX = 0

          IJU(K) = JUPTR

C

          I = K

   6      I = JL(I)

          IF (I.EQ.0)  GO TO 10

C

C--------MERGE ROW I INTO K-TH ROW

            LUI = IU(I+1) - (IU(I)+1)

            JMIN = IJU(I) +  1

            JMAX = IJU(I) + LUI

            QM = K

C

            DO 8 J=JMIN,JMAX

              VJ = JU(J)

   7          M = QM

              QM = Q(M)

              IF (QM.LT.VJ)  GO TO 7

              IF (QM.EQ.VJ)  GO TO 8

                LUK = LUK+1

                Q(M) = VJ

                Q(VJ) = QM

                QM = VJ

   8          CONTINUE

C

C--------REMEMBER LENGTH AND POSITION IN JU OF LONGEST ROW MERGED

            IF (LUI.LE.LMAX)  GO TO 9

              LMAX = LUI

              IJU(K) = JMIN

C

   9        GO TO 6

C

C------IF THE K-TH ROW IS THE SAME LENGTH AS THE LONGEST ROW MERGED,

C------THEN USE THE COLUMN INDICES FOR THAT ROW

  10      IF (LUK.EQ.LMAX)  GO TO 17

C

C------IF THE TAIL OF THE LAST ROW SAVED IN JU IS THE SAME AS THE HEAD

C------OF THE K-TH ROW, THEN OVERLAP THE TWO SETS OF COLUMN INDICES --

C--------SEARCH LAST ROW SAVED FOR FIRST NONZERO ENTRY IN K-TH ROW ...

            I = Q(K)

            IF (JUMIN.GT.JUPTR)  GO TO 12

            DO 11 JMIN=JUMIN,JUPTR

              IF (JU(JMIN)-I)  11, 13, 12

  11          CONTINUE

  12        GO TO 15

C

C--------... AND THEN TEST WHETHER TAIL MATCHES HEAD OF K-TH ROW

  13        IJU(K) = JMIN

            DO 14 J=JMIN,JUPTR

              IF (JU(J).NE.I)  GO TO 15

              I = Q(I)

              IF (I.GT.N)  GO TO 17

  14          CONTINUE

            JUPTR = JMIN - 1

C

C------SAVE NONZERO STRUCTURE OF K-TH ROW IN JU

  15      I = K

          JUMIN = JUPTR +  1

          JUPTR = JUPTR + LUK

          IF (JUPTR.GT.JUMAX)  GO TO 106

          DO 16 J=JUMIN,JUPTR

            I = Q(I)

            JU(J) = I

  16        MARK(I) = K

          IJU(K) = JUMIN

C

C------ADD K TO ROW LIST FOR FIRST NONZERO ELEMENT IN K-TH ROW

  17      IF (LUK.LE.1)  GO TO 18

            I = JU(IJU(K))

            JL(K) = JL(I)

            JL(I) = K

C

  18      IU(K+1) = IU(K) + LUK

C

        FLAG = 0

        RETURN

C

C ** ERROR -- DUPLICATE ENTRY IN A

 102    FLAG = 2*N + P(K)

        RETURN

C ** ERROR -- INSUFFICIENT STORAGE FOR JU

 106    FLAG = 6*N + K

        RETURN

        END

C

C***********************************************************************

C  SNF -- NUMERICAL UT-D-U FACTORIZATION OF SPARSE SYMMETRIC POSITIVE

C         DEFINITE MATRIX

C***********************************************************************

        SUBROUTINE  SNF

     *     (N, P,IP, IA,JA,A, D, IJU,JU,IU,U,UMAX, IL, JL, FLAG)

C

C  ADDITIONAL PARAMETERS

C

C    IL    - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N

C

C    JL    - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N

C

C

C  DEFINITIONS OF INTERNAL PARAMETERS (DURING K-TH STAGE OF ELIMINATION)

C

C    (D(I),I=K,N) CONTAINS THE K-TH ROW OF U (EXPANDED)

C

C    IL(I) POINTS TO THE FIRST NONZERO ELEMENT IN COLUMNS K,...,N OF

C      ROW I OF U

C

C    JL CONTAINS LISTS OF ROWS TO BE ADDED TO UNELIMINATED ROWS --

C      I GE K => JL(I) IS THE FIRST ROW TO BE ADDED TO ROW I

C      I LT K => JL(I) IS THE ROW FOLLOWING ROW I IN SOME LIST OF ROWS

C      IN EITHER CASE, JL(I) = 0 INDICATES THE END OF A LIST

C

C-----------------------------------------------------------------------

C

        INTEGER  P(*), IP(*),  IA(*), JA(*),  IJU(*), JU(*), IU(*),

     *     UMAX,  IL(*),  JL(*),  FLAG,  VJ

C...    REAL  A(1),  D(1), U(1),  DK, UKIDI

        DOUBLE PRECISION  A(*),  D(*), U(*),  DK, UKIDI

C

C----CHECK FOR SUFFICIENT STORAGE FOR U

        IF (IU(N+1)-1 .GT. UMAX)  GO TO 107

C

C----INITIALIZATION

        DO 1 K=1,N

          KK=P(K)

          IP(KK)=K

          D(K) = 0

   1      JL(K) = 0

C

C----FOR EACH ROW K

        DO 11 K=1,N

C

C------INITIALIZE K-TH ROW WITH ELEMENTS NONZERO IN ROW P(K) OF M

   3      JMIN = IA(P(K))

          JMAX = IA(P(K)+1) - 1

          IF (JMIN.GT.JMAX) GO TO 5

          DO 4 J=JMIN,JMAX

            VJ = IP(JA(J))

            IF (K.LE.VJ)  D(VJ) = A(J)

   4        CONTINUE

C

C------MODIFY K-TH ROW BY ADDING IN THOSE ROWS I WITH U(I,K) NE 0

C------FOR EACH ROW I TO BE ADDED IN

   5      DK = D(K)

          I = JL(K)

   6      IF (I.EQ.0)  GO TO 9

            NEXTI = JL(I)

C

C--------COMPUTE MULTIPLIER AND UPDATE DIAGONAL ELEMENT

            ILI = IL(I)

            UKIDI = - U(ILI) * D(I)

            DK = DK + UKIDI * U(ILI)

            U(ILI) = UKIDI

C

C--------ADD MULTIPLE OF ROW I TO K-TH ROW ...

            JMIN = ILI     + 1

            JMAX = IU(I+1) - 1

            IF (JMIN.GT.JMAX)  GO TO 8

              MU = IJU(I) - IU(I)

              DO 7 J=JMIN,JMAX

   7            D(JU(MU+J)) = D(JU(MU+J)) + UKIDI * U(J)

C

C--------... AND ADD I TO ROW LIST FOR NEXT NONZERO ENTRY

              IL(I) = JMIN

              J = JU(MU+JMIN)

              JL(I) = JL(J)

              JL(J) = I

C

   8        I = NEXTI

            GO TO 6

C

C------CHECK FOR ZERO PIVOT AND SAVE DIAGONAL ELEMENT

   9      IF (DK.EQ.0)  GO TO 108

          D(K) = 1 / DK

C

C------SAVE NONZERO ENTRIES IN K-TH ROW OF U ...

          JMIN = IU(K)

          JMAX = IU(K+1) - 1

          IF (JMIN.GT.JMAX)  GO TO 11

            MU = IJU(K) - JMIN

            DO 10 J=JMIN,JMAX

              JUMUJ = JU(MU+J)

              U(J) = D(JUMUJ)

  10          D(JUMUJ) = 0

C

C------... AND ADD K TO ROW LIST FOR FIRST NONZERO ENTRY IN K-TH ROW

            IL(K) = JMIN

            I = JU(MU+JMIN)

            JL(K) = JL(I)

            JL(I) = K

  11      CONTINUE

C

        FLAG = 0

        RETURN

C

C ** ERROR -- INSUFFICIENT STORAGE FOR U

 107    FLAG = 7*N + 1

        RETURN

C ** ERROR -- ZERO PIVOT

 108    FLAG = 8*N + K

        RETURN

        END

C

C***********************************************************************

C  SNS -- SOLUTION OF SPARSE SYMMETRIC POSITIVE DEFINITE SYSTEM OF

C         LINEAR EQUATIONS  MX = B  GIVEN UT-D-U FACTORIZATION OF M

C***********************************************************************

        SUBROUTINE  SNS

     *     (N, P, D, IJU,JU,IU,U, Z, B, TMP)

        INTEGER  P(*),  IJU(*), JU(*), IU(*)

C...    REAL  D(1), U(1),  Z(1), B(1),  TMP(1),  TMPK, SUM

        DOUBLE PRECISION  D(*), U(*),  Z(*), B(*),  TMP(*),  TMPK, SUM

C

C  ADDITIONAL PARAMETERS

C

C    TMP   - REAL ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N

C

C-----------------------------------------------------------------------

C

C----SET TMP TO PERMUTED B

        DO 1 K=1,N

   1      TMP(K) = B(P(K))

C

C----SOLVE  UT D Y = B  BY FORWARD SUBSTITUTION

        DO 3 K=1,N

          TMPK = TMP(K)

          JMIN = IU(K)

          JMAX = IU(K+1) - 1

          IF (JMIN.GT.JMAX)  GO TO 3

          MU = IJU(K) - JMIN

          DO 2 J=JMIN,JMAX

   2        TMP(JU(MU+J)) = TMP(JU(MU+J)) + U(J) * TMPK

   3      TMP(K) = TMPK * D(K)

C

C----SOLVE  U X = Y  BY BACK SUBSTITUTION

        K = N

        DO 6 I=1,N

          SUM = TMP(K)

          JMIN = IU(K)

          JMAX = IU(K+1) - 1

          IF (JMIN.GT.JMAX)  GO TO 5

          MU = IJU(K) - JMIN

          DO 4 J=JMIN,JMAX

   4        SUM = SUM + U(J) * TMP(JU(MU+J))

   5      TMP(K) = SUM

          Z(P(K)) = SUM

   6      K = K-1

C

        RETURN

        END

      SUBROUTINE CDRVD

     *     (N, R,C,IC, IA,JA,A, B, Z, NSP,ISP,RSP,ESP, PATH, FLAG)

C-----------------------------------------------------------------------

C*** DRIVER FOR SUBROUTINES FOR SOLVING SPARSE NONSYMMETRIC SYSTEMS OF

C       LINEAR EQUATIONS (COMPRESSED POINTER STORAGE)

C-----------------------------------------------------------------------

C

C

      INTEGER  R(*), C(*), IC(*),  IA(*), JA(*),  ISP(*), ESP,  PATH,

     *   FLAG,  D, U, Q, ROW, TMP, AR,  UMAX

      DOUBLE PRECISION  A(*), B(*), Z(*), RSP(*)

C

C

      DATA LRATIO/2/

C

      IF (PATH.LT.1 .OR. 5.LT.PATH)  GO TO 111

C

      IL   = 1

      IJL  = IL  + (N+1)

      IU   = IJL +   N

      IJU  = IU  + (N+1)

      IRL  = IJU +   N

      JRL  = IRL +   N

      JL   = JRL +   N

C

C

      IF ((PATH-1) * (PATH-5) .NE. 0)  GO TO 5

        MAX = (LRATIO*NSP + 1 - JL) - (N+1) - 5*N

        JLMAX = MAX/2

        Q     = JL   + JLMAX

        IRA   = Q    + (N+1)

        JRA   = IRA  +   N

        IRAC  = JRA  +   N

        IRU   = IRAC +   N

        JRU   = IRU  +   N

        JUTMP = JRU  +   N

        JUMAX = LRATIO*NSP  + 1 - JUTMP

        ESP = MAX/LRATIO

        IF (JLMAX.LE.0 .OR. JUMAX.LE.0)  GO TO 110

C

        DO 1 I=1,N

          IF (C(I).NE.I)  GO TO 2

 1        CONTINUE

        GO TO 3

 2      AR = NSP + 1 - N

        CALL NROC

     *     (N, IC, IA,JA,A, ISP(IL), RSP(AR), ISP(IU), FLAG)

        IF (FLAG.NE.0)  GO TO 100

C

 3      CALL NSFC

     *     (N, R, IC, IA,JA,

     *      JLMAX, ISP(IL), ISP(JL), ISP(IJL),

     *      JUMAX, ISP(IU), ISP(JUTMP), ISP(IJU),

     *      ISP(Q), ISP(IRA), ISP(JRA), ISP(IRAC),

     *      ISP(IRL), ISP(JRL), ISP(IRU), ISP(JRU),  FLAG)

        IF(FLAG .NE. 0)  GO TO 100

C

        JLMAX = ISP(IJL+N-1)

        JU    = JL + JLMAX

        JUMAX = ISP(IJU+N-1)

        IF (JUMAX.LE.0)  GO TO 5

        DO 4 J=1,JUMAX

 4        ISP(JU+J-1) = ISP(JUTMP+J-1)

C

C

 5    JLMAX = ISP(IJL+N-1)

      JU    = JL  + JLMAX

      JUMAX = ISP(IJU+N-1)

      L     = (JU + JUMAX - 2 + LRATIO)  /  LRATIO    +    1

      LMAX  = ISP(IL+N) - 1

      D     = L   + LMAX

      U     = D   + N

      ROW   = NSP + 1 - N

      TMP   = ROW - N

      UMAX  = TMP - U

      ESP   = UMAX - (ISP(IU+N) - 1)

C

      IF ((PATH-1) * (PATH-2) .NE. 0)  GO TO 6

        IF (UMAX.LE.0)  GO TO 110

        CALL NNFC

     *     (N,  R, C, IC,  IA, JA, A, Z, B,

     *      LMAX, ISP(IL), ISP(JL), ISP(IJL), RSP(L),  RSP(D),

     *      UMAX, ISP(IU), ISP(JU), ISP(IJU), RSP(U),

     *      RSP(ROW), RSP(TMP),  ISP(IRL), ISP(JRL),  FLAG)

        IF(FLAG .NE. 0)  GO TO 100

C

 6    IF ((PATH-3) .NE. 0)  GO TO 7

        CALL NNSC

     *     (N,  R, C,  ISP(IL), ISP(JL), ISP(IJL), RSP(L),

     *      RSP(D),    ISP(IU), ISP(JU), ISP(IJU), RSP(U),

     *      Z, B,  RSP(TMP))

C

 7    IF ((PATH-4) .NE. 0)  GO TO 8

        CALL NNTC

     *     (N,  R, C,  ISP(IL), ISP(JL), ISP(IJL), RSP(L),

     *      RSP(D),    ISP(IU), ISP(JU), ISP(IJU), RSP(U),

     *      Z, B,  RSP(TMP))

 8    RETURN

C

C

 100  RETURN

C

 110  FLAG = 10*N + 1

      RETURN

C

 111  FLAG = 11*N + 1

      RETURN

      END

C-----------------------------------------------------------------------

      SUBROUTINE NNFC

     *     (N, R,C,IC, IA,JA,A, Z, B,

     *      LMAX,IL,JL,IJL,L, D, UMAX,IU,JU,IJU,U,

     *      ROW, TMP, IRL,JRL, FLAG)

C-----------------------------------------------------------------------

C*** SUBROUTINE NNFC

C*** NUMERICAL LDU-FACTORIZATION OF SPARSE NONSYMMETRIC MATRIX AND

C      SOLUTION OF SYSTEM OF LINEAR EQUATIONS (COMPRESSED POINTER

C      STORAGE)

C-----------------------------------------------------------------------

C

C

      INTEGER RK,UMAX

      INTEGER  R(*), C(*), IC(*), IA(*), JA(*), IL(*), JL(*), IJL(*)

      INTEGER  IU(*), JU(*), IJU(*), IRL(*), JRL(*), FLAG

      DOUBLE PRECISION  A(*), L(*), D(*), U(*), Z(*), B(*), ROW(*)

      DOUBLE PRECISION  TMP(*), LKI, SUM, DK

C

C

      IF(IL(N+1)-1 .GT. LMAX) GO TO 104

      IF(IU(N+1)-1 .GT. UMAX) GO TO 107

      DO 1 K=1,N

        IRL(K) = IL(K)

        JRL(K) = 0

 1      CONTINUE

C

C

      DO 19 K=1,N

C

        ROW(K) = 0

        I1 = 0

        IF (JRL(K) .EQ. 0) GO TO 3

        I = JRL(K)

 2      I2 = JRL(I)

        JRL(I) = I1

        I1 = I

        ROW(I) = 0

        I = I2

        IF (I .NE. 0) GO TO 2

C

 3      JMIN = IJU(K)

        JMAX = JMIN + IU(K+1) - IU(K) - 1

        IF (JMIN .GT. JMAX) GO TO 5

        DO 4 J=JMIN,JMAX

 4        ROW(JU(J)) = 0

C

 5      RK = R(K)

        JMIN = IA(RK)

        JMAX = IA(RK+1) - 1

        DO 6 J=JMIN,JMAX

          ROW(IC(JA(J))) = A(J)

 6        CONTINUE

C

        SUM = B(RK)

        I = I1

        IF (I .EQ. 0) GO TO 10

C

 7        LKI = -ROW(I)

C

          L(IRL(I)) = -LKI

          SUM = SUM + LKI * TMP(I)

          JMIN = IU(I)

          JMAX = IU(I+1) - 1

          IF (JMIN .GT. JMAX) GO TO 9

          MU = IJU(I) - JMIN

          DO 8 J=JMIN,JMAX

 8          ROW(JU(MU+J)) = ROW(JU(MU+J)) + LKI * U(J)

 9        I = JRL(I)

          IF (I .NE. 0) GO TO 7

C

C

 10     IF (ROW(K) .EQ. 0.0D0) GO TO 108

        DK = 1.0D0 / ROW(K)

        D(K) = DK

        TMP(K) = SUM * DK

        IF (K .EQ. N) GO TO 19

        JMIN = IU(K)

        JMAX = IU(K+1) - 1

        IF (JMIN .GT. JMAX) GO TO 12

        MU = IJU(K) - JMIN

        DO 11 J=JMIN,JMAX

 11       U(J) = ROW(JU(MU+J)) * DK

 12     CONTINUE

C

C

        I = I1

        IF (I .EQ. 0) GO TO 18

 14     IRL(I) = IRL(I) + 1

        I1 = JRL(I)

        IF (IRL(I) .GE. IL(I+1)) GO TO 17

        IJLB = IRL(I) - IL(I) + IJL(I)

        J = JL(IJLB)

 15     IF (I .GT. JRL(J)) GO TO 16

          J = JRL(J)

          GO TO 15

 16     JRL(I) = JRL(J)

        JRL(J) = I

 17     I = I1

        IF (I .NE. 0) GO TO 14

 18     IF (IRL(K) .GE. IL(K+1)) GO TO 19

        J = JL(IJL(K))

        JRL(K) =JRL(J)

        JRL(J) = K

 19     CONTINUE

C

C

      K = N

      DO 22 I=1,N

        SUM = TMP(K)

        JMIN = IU(K)

        JMAX = IU(K+1) - 1

        IF (JMIN .GT. JMAX)  GO TO 21

        MU = IJU(K) - JMIN

        DO 20 J=JMIN,JMAX

 20       SUM = SUM - U(J) * TMP(JU(MU+J))

 21     TMP(K) = SUM

        Z(C(K)) = SUM

 22     K = K-1

      FLAG = 0

      RETURN

C

C

 104  FLAG = 4*N + 1

      RETURN

C

 107  FLAG = 7*N + 1

      RETURN

C

 108  FLAG = 8*N + K

      RETURN

      END

C-----------------------------------------------------------------------

      SUBROUTINE NNSC

     *     (N, R, C, IL, JL, IJL, L, D, IU, JU, IJU, U, Z, B, TMP)

C-----------------------------------------------------------------------

C*** SUBROUTINE NNSC

C*** NUMERICAL SOLUTION OF SPARSE NONSYMMETRIC SYSTEM OF LINEAR

C      EQUATIONS GIVEN LDU-FACTORIZATION (COMPRESSED POINTER STORAGE)

C-----------------------------------------------------------------------

C

C

      INTEGER R(*), C(*), IL(*), JL(*), IJL(*), IU(*), JU(*), IJU(*)

      DOUBLE PRECISION  L(*), D(*), U(*), B(*), Z(*), TMP(*), TMPK,SUM

C

C

      DO 1 K=1,N

 1      TMP(K) = B(R(K))

C

      DO 3 K = 1,N

        JMIN = IL(K)

        JMAX = IL(K+1) - 1

        TMPK = -D(K) * TMP(K)

        TMP(K) = -TMPK

        IF (JMIN .GT. JMAX) GO TO 3

        ML = IJL(K) - JMIN

        DO 2 J=JMIN,JMAX

 2        TMP(JL(ML+J)) = TMP(JL(ML+J)) + TMPK * L(J)

 3      CONTINUE

C

      K = N

      DO 6 I=1,N

        SUM = -TMP(K)

        JMIN = IU(K)

        JMAX = IU(K+1) - 1

        IF (JMIN .GT. JMAX) GO TO 5

        MU = IJU(K) - JMIN

        DO 4 J=JMIN,JMAX

 4        SUM = SUM + U(J) * TMP(JU(MU+J))

 5      TMP(K) = -SUM

        Z(C(K)) = -SUM

        K = K - 1

 6      CONTINUE

      RETURN

      END

C-----------------------------------------------------------------------

      SUBROUTINE NNTC

     *     (N, R, C, IL, JL, IJL, L, D, IU, JU, IJU, U, Z, B, TMP)

C-----------------------------------------------------------------------

C*** SUBROUTINE NNTC

C*** NUMERIC SOLUTION OF THE TRANSPOSE OF A SPARSE NONSYMMETRIC SYSTEM

C      OF LINEAR EQUATIONS GIVEN LU-FACTORIZATION (COMPRESSED POINTER

C      STORAGE)

C-----------------------------------------------------------------------

C

C

      INTEGER R(*), C(*), IL(*), JL(*), IJL(*), IU(*), JU(*), IJU(*)

      DOUBLE PRECISION L(*), D(*), U(*), B(*), Z(*), TMP(*), TMPK,SUM

C

C

      DO 1 K=1,N

 1      TMP(K) = B(C(K))

C

      DO 3 K=1,N

        JMIN = IU(K)

        JMAX = IU(K+1) - 1

        TMPK = -TMP(K)

        IF (JMIN .GT. JMAX) GO TO 3

        MU = IJU(K) - JMIN

        DO 2 J=JMIN,JMAX

 2        TMP(JU(MU+J)) = TMP(JU(MU+J)) + TMPK * U(J)

 3      CONTINUE

C

      K = N

      DO 6 I=1,N

        SUM = -TMP(K)

        JMIN = IL(K)

        JMAX = IL(K+1) - 1

        IF (JMIN .GT. JMAX) GO TO 5

        ML = IJL(K) - JMIN

        DO 4 J=JMIN,JMAX

 4        SUM = SUM + L(J) * TMP(JL(ML+J))

 5      TMP(K) = -SUM * D(K)

        Z(R(K)) = TMP(K)

        K = K - 1

 6      CONTINUE

      RETURN

      END

C-----------------------------------------------------------------------

      SUBROUTINE NROC (N, IC, IA, JA, A, JAR, AR, P, FLAG)

C-----------------------------------------------------------------------

C    YALE SPARSE MATRIX PACKAGE - NONSYMMETRIC CODES

C-----------------------------------------------------------------------

C*** SUBROUTINE NROC

C*** REORDERS ROWS OF A, LEAVING ROW ORDER UNCHANGED

C-----------------------------------------------------------------------

C

      INTEGER  IC(*), IA(*), JA(*), JAR(*), P(*), FLAG

      DOUBLE PRECISION  A(*), AR(*)

C

C

      DO 5 K=1,N

        JMIN = IA(K)

        JMAX = IA(K+1) - 1

        IF(JMIN .GT. JMAX) GO TO 5

        P(N+1) = N + 1

C

        DO 3 J=JMIN,JMAX

          NEWJ = IC(JA(J))

          I = N + 1

 1        IF(P(I) .GE. NEWJ) GO TO 2

            I = P(I)

            GO TO 1

 2        IF(P(I) .EQ. NEWJ) GO TO 102

          P(NEWJ) = P(I)

          P(I) = NEWJ

          JAR(NEWJ) = JA(J)

          AR(NEWJ) = A(J)

 3        CONTINUE

C

        I = N + 1

        DO 4 J=JMIN,JMAX

          I = P(I)

          JA(J) = JAR(I)

 4        A(J) = AR(I)

 5      CONTINUE

      FLAG = 0

      RETURN

C

C

 102  FLAG = N + K

      RETURN

      END

C-----------------------------------------------------------------------

      SUBROUTINE NSFC

     *      (N, R, IC, IA,JA, JLMAX,IL,JL,IJL, JUMAX,IU,JU,IJU,

     *       Q, IRA,JRA, IRAC, IRL,JRL, IRU,JRU, FLAG)

C-----------------------------------------------------------------------

C*** SUBROUTINE NSFC

C*** SYMBOLIC LDU-FACTORIZATION OF NONSYMMETRIC SPARSE MATRIX

C      (COMPRESSED POINTER STORAGE)

C-----------------------------------------------------------------------

C

C

      INTEGER CEND, QM, REND, RK, VJ

      INTEGER IA(*), JA(*), IRA(*), JRA(*), IL(*), JL(*), IJL(*)

      INTEGER IU(*), JU(*), IJU(*), IRL(*), JRL(*), IRU(*), JRU(*)

      INTEGER R(*), IC(*), Q(*), IRAC(*), FLAG

C

C

      NP1 = N + 1

      JLMIN = 1

      JLPTR = 0

      IL(1) = 1

      JUMIN = 1

      JUPTR = 0

      IU(1) = 1

      DO 1 K=1,N

        IRAC(K) = 0

        JRA(K) = 0

        JRL(K) = 0

 1      JRU(K) = 0

C

      DO 2 K=1,N

        RK = R(K)

        IAK = IA(RK)

        IF (IAK .GE. IA(RK+1))  GO TO 101

        JAIAK = IC(JA(IAK))

        IF (JAIAK .GT. K)  GO TO 105

        JRA(K) = IRAC(JAIAK)

        IRAC(JAIAK) = K

 2      IRA(K) = IAK

C

C

      DO 41 K=1,N

C

C

        Q(NP1) = NP1

        LUK = -1

C

        VJ = IRAC(K)

        IF (VJ .EQ. 0)  GO TO 5

 3        QM = NP1

 4        M = QM

          QM = Q(M)

          IF (QM .LT. VJ)  GO TO 4

          IF (QM .EQ. VJ)  GO TO 102

            LUK = LUK + 1

            Q(M) = VJ

            Q(VJ) = QM

            VJ = JRA(VJ)

            IF (VJ .NE. 0)  GO TO 3

C

 5      LASTID = 0

        LASTI = 0

        IJL(K) = JLPTR

        I = K

 6        I = JRU(I)

          IF (I .EQ. 0)  GO TO 10

          QM = NP1

          JMIN = IRL(I)

          JMAX = IJL(I) + IL(I+1) - IL(I) - 1

          LONG = JMAX - JMIN

          IF (LONG .LT. 0)  GO TO 6

          JTMP = JL(JMIN)

          IF (JTMP .NE. K)  LONG = LONG + 1

          IF (JTMP .EQ. K)  R(I) = -R(I)

          IF (LASTID .GE. LONG)  GO TO 7

            LASTI = I

            LASTID = LONG

C

 7        DO 9 J=JMIN,JMAX

            VJ = JL(J)

 8          M = QM

            QM = Q(M)

            IF (QM .LT. VJ)  GO TO 8

            IF (QM .EQ. VJ)  GO TO 9

              LUK = LUK + 1

              Q(M) = VJ

              Q(VJ) = QM

              QM = VJ

 9          CONTINUE

            GO TO 6

C

C

 10     QM = Q(NP1)

        IF (QM .NE. K)  GO TO 105

        IF (LUK .EQ. 0)  GO TO 17

        IF (LASTID .NE. LUK)  GO TO 11

C

        IRLL = IRL(LASTI)

        IJL(K) = IRLL + 1

        IF (JL(IRLL) .NE. K)  IJL(K) = IJL(K) - 1

        GO TO 17

C

 11     IF (JLMIN .GT. JLPTR)  GO TO 15

        QM = Q(QM)

        DO 12 J=JLMIN,JLPTR

          IF (JL(J) - QM)  12, 13, 15

 12       CONTINUE

        GO TO 15

 13     IJL(K) = J

        DO 14 I=J,JLPTR

          IF (JL(I) .NE. QM)  GO TO 15

          QM = Q(QM)

          IF (QM .GT. N)  GO TO 17

 14       CONTINUE

        JLPTR = J - 1

C

 15     JLMIN = JLPTR + 1

        IJL(K) = JLMIN

        IF (LUK .EQ. 0)  GO TO 17

        JLPTR = JLPTR + LUK

        IF (JLPTR .GT. JLMAX)  GO TO 103

          QM = Q(NP1)

          DO 16 J=JLMIN,JLPTR

            QM = Q(QM)

 16         JL(J) = QM

 17     IRL(K) = IJL(K)

        IL(K+1) = IL(K) + LUK

C

C

        Q(NP1) = NP1

        LUK = -1

C

        RK = R(K)

        JMIN = IRA(K)

        JMAX = IA(RK+1) - 1

        IF (JMIN .GT. JMAX)  GO TO 20

        DO 19 J=JMIN,JMAX

          VJ = IC(JA(J))

          QM = NP1

 18       M = QM

          QM = Q(M)

          IF (QM .LT. VJ)  GO TO 18

          IF (QM .EQ. VJ)  GO TO 102

            LUK = LUK + 1

            Q(M) = VJ

            Q(VJ) = QM

 19       CONTINUE

C

 20     LASTID = 0

        LASTI = 0

        IJU(K) = JUPTR

        I = K

        I1 = JRL(K)

 21       I = I1

          IF (I .EQ. 0)  GO TO 26

          I1 = JRL(I)

          QM = NP1

          JMIN = IRU(I)

          JMAX = IJU(I) + IU(I+1) - IU(I) - 1

          LONG = JMAX - JMIN

          IF (LONG .LT. 0)  GO TO 21

          JTMP = JU(JMIN)

          IF (JTMP .EQ. K)  GO TO 22

C

            LONG = LONG + 1

            CEND = IJL(I) + IL(I+1) - IL(I)

            IRL(I) = IRL(I) + 1

            IF (IRL(I) .GE. CEND)  GO TO 22

              J = JL(IRL(I))

              JRL(I) = JRL(J)

              JRL(J) = I

 22       IF (LASTID .GE. LONG)  GO TO 23

            LASTI = I

            LASTID = LONG

C

 23       DO 25 J=JMIN,JMAX

            VJ = JU(J)

 24         M = QM

            QM = Q(M)

            IF (QM .LT. VJ)  GO TO 24

            IF (QM .EQ. VJ)  GO TO 25

              LUK = LUK + 1

              Q(M) = VJ

              Q(VJ) = QM

              QM = VJ

 25         CONTINUE

          GO TO 21

C

 26     IF (IL(K+1) .LE. IL(K))  GO TO 27

          J = JL(IRL(K))

          JRL(K) = JRL(J)

          JRL(J) = K

C

C

 27     QM = Q(NP1)

        IF (QM .NE. K)  GO TO 105

        IF (LUK .EQ. 0)  GO TO 34

        IF (LASTID .NE. LUK)  GO TO 28

C

        IRUL = IRU(LASTI)

        IJU(K) = IRUL + 1

        IF (JU(IRUL) .NE. K)  IJU(K) = IJU(K) - 1

        GO TO 34

C

 28     IF (JUMIN .GT. JUPTR)  GO TO 32

        QM = Q(QM)

        DO 29 J=JUMIN,JUPTR

          IF (JU(J) - QM)  29, 30, 32

 29       CONTINUE

        GO TO 32

 30     IJU(K) = J

        DO 31 I=J,JUPTR

          IF (JU(I) .NE. QM)  GO TO 32

          QM = Q(QM)

          IF (QM .GT. N)  GO TO 34

 31       CONTINUE

        JUPTR = J - 1

C

 32     JUMIN = JUPTR + 1

        IJU(K) = JUMIN

        IF (LUK .EQ. 0)  GO TO 34

        JUPTR = JUPTR + LUK

        IF (JUPTR .GT. JUMAX)  GO TO 106

          QM = Q(NP1)

          DO 33 J=JUMIN,JUPTR

            QM = Q(QM)

 33         JU(J) = QM

 34     IRU(K) = IJU(K)

        IU(K+1) = IU(K) + LUK

C

C

        I = K

 35       I1 = JRU(I)

          IF (R(I) .LT. 0)  GO TO 36

          REND = IJU(I) + IU(I+1) - IU(I)

          IF (IRU(I) .GE. REND)  GO TO 37

            J = JU(IRU(I))

            JRU(I) = JRU(J)

            JRU(J) = I

            GO TO 37

 36       R(I) = -R(I)

 37       I = I1

          IF (I .EQ. 0)  GO TO 38

          IRU(I) = IRU(I) + 1

          GO TO 35

C

C

 38     I = IRAC(K)

        IF (I .EQ. 0)  GO TO 41

 39       I1 = JRA(I)

          IRA(I) = IRA(I) + 1

          IF (IRA(I) .GE. IA(R(I)+1))  GO TO 40

          IRAI = IRA(I)

          JAIRAI = IC(JA(IRAI))

          IF (JAIRAI .GT. I)  GO TO 40

          JRA(I) = IRAC(JAIRAI)

          IRAC(JAIRAI) = I

 40       I = I1

          IF (I .NE. 0)  GO TO 39

 41     CONTINUE

C

      IJL(N) = JLPTR

      IJU(N) = JUPTR

      FLAG = 0

      RETURN

C

C

 101  FLAG = N + RK

      RETURN

C

 102  FLAG = 2*N + RK

      RETURN

C

 103  FLAG = 3*N + K

      RETURN

C

 105  FLAG = 5*N + K

      RETURN

C

 106  FLAG = 6*N + K

      RETURN

      END

C-----------------------------------------------------------------------

      SUBROUTINE NNFCAD

     *     (N, R,C,IC, IA,JA,A, Z, B,

     *      LMAX,IL,JL,IJL, UMAX,IU,JU,IJU,U,

     *      ROW, TMP, IRL,JRL, FLAG)

C-----------------------------------------------------------------------

C*** SUBROUTINE NNFC

C*** NUMERICAL LDU-FACTORIZATION OF SPARSE NONSYMMETRIC MATRIX AND

C      SOLUTION OF SYSTEM OF LINEAR EQUATIONS (COMPRESSED POINTER

C      STORAGE)

C-----------------------------------------------------------------------

C

C

      INTEGER RK,UMAX

      INTEGER  R(*), C(*), IC(*), IA(*), JA(*), IL(*), JL(*), IJL(*)

      INTEGER  IU(*), JU(*), IJU(*), IRL(*), JRL(*), FLAG

      REAL*8  A(*),  U(*), Z(*), B(*), ROW(*)

      REAL*8 TMP(*), LKI, SUM, DK

C

C

      IF(IL(N+1)-1 .GT. LMAX) GO TO 104

      IF(IU(N+1)-1 .GT. UMAX) GO TO 107

      DO 1 K=1,N

        IRL(K) = IL(K)

        JRL(K) = 0

 1      CONTINUE

C

C

      DO 19 K=1,N

C

        ROW(K) = 0

        I1 = 0

        IF (JRL(K) .EQ. 0) GO TO 3

        I = JRL(K)

 2      I2 = JRL(I)

        JRL(I) = I1

        I1 = I

        ROW(I) = 0

        I = I2

        IF (I .NE. 0) GO TO 2

C

 3      JMIN = IJU(K)

        JMAX = JMIN + IU(K+1) - IU(K) - 1

        IF (JMIN .GT. JMAX) GO TO 5

        DO 4 J=JMIN,JMAX

 4        ROW(JU(J)) = 0

C

 5      RK = R(K)

        JMIN = IA(RK)

        JMAX = IA(RK+1) - 1

        DO 6 J=JMIN,JMAX

          ROW(IC(JA(J))) = A(J)

 6        CONTINUE

C

        SUM = B(RK)

        I = I1

        IF (I .EQ. 0) GO TO 10

C

 7        LKI = -ROW(I)

C

          SUM = SUM + LKI * TMP(I)

          JMIN = IU(I)

          JMAX = IU(I+1) - 1

          IF (JMIN .GT. JMAX) GO TO 9

          MU = IJU(I) - JMIN

          DO 8 J=JMIN,JMAX

 8          ROW(JU(MU+J)) = ROW(JU(MU+J)) + LKI * U(J)

 9        I = JRL(I)

          IF (I .NE. 0) GO TO 7

C

C

 10     IF (ROW(K) .EQ. 0.0D0) GO TO 108

        DK = 1.0D0 / ROW(K)

        TMP(K) = SUM * DK

        IF (K .EQ. N) GO TO 19

        JMIN = IU(K)

        JMAX = IU(K+1) - 1

        IF (JMIN .GT. JMAX) GO TO 12

        MU = IJU(K) - JMIN

        DO 11 J=JMIN,JMAX

 11       U(J) = ROW(JU(MU+J)) * DK

 12     CONTINUE

C

C

        I = I1

        IF (I .EQ. 0) GO TO 18

 14     IRL(I) = IRL(I) + 1

        I1 = JRL(I)

        IF (IRL(I) .GE. IL(I+1)) GO TO 17

        IJLB = IRL(I) - IL(I) + IJL(I)

        J = JL(IJLB)

 15     IF (I .GT. JRL(J)) GO TO 16

          J = JRL(J)

          GO TO 15

 16     JRL(I) = JRL(J)

        JRL(J) = I

 17     I = I1

        IF (I .NE. 0) GO TO 14

 18     IF (IRL(K) .GE. IL(K+1)) GO TO 19

        J = JL(IJL(K))

        JRL(K) =JRL(J)

        JRL(J) = K

 19     CONTINUE

C

C

      K = N

      DO 22 I=1,N

        SUM = TMP(K)

        JMIN = IU(K)

        JMAX = IU(K+1) - 1

        IF (JMIN .GT. JMAX)  GO TO 21

        MU = IJU(K) - JMIN

        DO 20 J=JMIN,JMAX

 20       SUM = SUM - U(J) * TMP(JU(MU+J))

 21     TMP(K) = SUM

        Z(C(K)) = SUM

 22     K = K-1

      FLAG = 0

      RETURN

C

C

 104  FLAG = 4*N + 1

      RETURN

C

 107  FLAG = 7*N + 1

      RETURN

C

 108  FLAG = 8*N + K

      RETURN

      END

      SUBROUTINE ADRVD

     *     (N, R,C,IC, IA,JA,A, B, Z, NSP,ISP,RSP,ESP, PATH, FLAG)

C-----------------------------------------------------------------------

C*** DRIVER FOR SUBROUTINES FOR SOLVING SPARSE NONSYMMETRIC SYSTEMS OF

C       LINEAR EQUATIONS (COMPRESSED POINTER STORAGE)

C-----------------------------------------------------------------------

C

C

      INTEGER  R(*), C(*), IC(*),  IA(*), JA(*),  ISP(*), ESP,  PATH,

     *   FLAG, U,  Q, ROW, TMP, AR,  UMAX

      REAL*8 A(*), B(*), Z(*), RSP(*)

C

C

      DATA LRATIO/1/

C

C

      IL   = 1

      IJL  = IL  + (N+1)

      IU   = IJL +   N

      IJU  = IU  + (N+1)

      IRL  = IJU +   N

      JRL  = IRL +   N

      JL   = JRL +   N

C

C

        MAX = (LRATIO*NSP + 1 - JL) - (N+1) - 5*N

        JLMAX = MAX/2

        Q     = JL   + JLMAX

        IRA   = Q    + (N+1)

        JRA   = IRA  +   N

        IRAC  = JRA  +   N

        IRU   = IRAC +   N

        JRU   = IRU  +   N

        JUTMP = JRU  +   N

        JUMAX = LRATIO*NSP  + 1 - JUTMP

        ESP = MAX/LRATIO

        IF (JLMAX.LE.0 .OR. JUMAX.LE.0)  GO TO 110

C

        DO 1 I=1,N

          IF (C(I).NE.I)  GO TO 2

 1        CONTINUE

        GO TO 3

 2      AR = NSP + 1 - N

        CALL NROC

     *     (N, IC, IA,JA,A, ISP(IL), RSP(AR), ISP(IU), FLAG)

        IF (FLAG.NE.0)  GO TO 100

C

 3      CALL NSFC

     *     (N, R, IC, IA,JA,

     *      JLMAX, ISP(IL), ISP(JL), ISP(IJL),

     *      JUMAX, ISP(IU), ISP(JUTMP), ISP(IJU),

     *      ISP(Q), ISP(IRA), ISP(JRA), ISP(IRAC),

     *      ISP(IRL), ISP(JRL), ISP(IRU), ISP(JRU),  FLAG)

        IF(FLAG .NE. 0)  GO TO 100

C

        JLMAX = ISP(IJL+N-1)

        JU    = JL + JLMAX

        JUMAX = ISP(IJU+N-1)

        IF (JUMAX.LE.0)  GO TO 5

        DO 4 J=1,JUMAX

 4        ISP(JU+J-1) = ISP(JUTMP+J-1)

C

C

 5    JLMAX = ISP(IJL+N-1)

      JU    = JL  + JLMAX

      JUMAX = ISP(IJU+N-1)

      LMAX = ISP(IL+N) - 1

      U     = (JU + JUMAX - 2 + LRATIO ) / LRATIO + 1

      UMAX = ISP(IU+N) - 1

      ROW   = U + UMAX

      TMP   = ROW + N

      ESP = NSP - (TMP + N)

      IF(ESP.LT.0) GOTO 110

C

        CALL NNFCAD

     *     (N,  R, C, IC,  IA, JA, A, Z, B,

     *      LMAX, ISP(IL), ISP(JL), ISP(IJL),

     *      UMAX, ISP(IU), ISP(JU), ISP(IJU), RSP(U),

     *      RSP(ROW), RSP(TMP),  ISP(IRL), ISP(JRL),  FLAG)

        IF(FLAG .NE. 0)  GO TO 100

 8    RETURN

C

C

 100  RETURN

C

 110  FLAG = 10*N + 1

      RETURN

C

      END

      SUBROUTINE PDRVD

     *     (N, IA,JA,A, P,IP, NSP,ISP, PATH, FLAG)

C-----------------------------------------------------------------------

C  ODRV -- DRIVER FOR SPARSE MATRIX REORDERING ROUTINES

C-----------------------------------------------------------------------

      INTEGER  IA(*), JA(*), P(*), IP(*), ISP(*), PATH, FLAG,

     *   V, L, HEAD,  TMP, Q

      DOUBLE PRECISION  A(*)

      LOGICAL  DFLAG

C

C

      FLAG = 0

      IF (PATH.LT.1 .OR. 5.LT.PATH) GO TO 111

C

C

      IF ((PATH-1) * (PATH-2) * (PATH-4) .NE. 0) GO TO 1

        MAX = (NSP-N)/2

        V    = 1

        L    = V     +  MAX

        HEAD = L     +  MAX

        NEXT = HEAD  +  N

        IF (MAX.LT.N)  GO TO 110

C

        CALL  MDN

     *     (N, IA,JA, MAX,ISP(V),ISP(L), ISP(HEAD),P,IP, ISP(V), FLAG)

        IF (FLAG.NE.0)  GO TO 100

C

C

 1    IF ((PATH-2) * (PATH-3) * (PATH-4) * (PATH-5) .NE. 0)  GO TO 2

        TMP = (NSP+1) -      N

        Q   = TMP     - (IA(N+1)-1)

        IF (Q.LT.1)  GO TO 110

C

        DFLAG = PATH.EQ.4 .OR. PATH.EQ.5

        CALL SRO

     *     (N,  IP,  IA, JA, A,  ISP(TMP),  ISP(Q),  DFLAG)

C

 2    RETURN

C

C ** ERROR -- ERROR DETECTED IN MD

 100  RETURN

C ** ERROR -- INSUFFICIENT STORAGE

 110  FLAG = 10*N + 1

      RETURN

C ** ERROR -- ILLEGAL PATH SPECIFIED

 111  FLAG = 11*N + 1

      RETURN

      END

      SUBROUTINE MDN

     *     (N, IA,JA, MAX, V,L, HEAD,LAST,NEXT, MARK, FLAG)

C-----------------------------------------------------------------------

C  MD -- MINIMUM DEGREE ALGORITHM (BASED ON ELEMENT MODEL)

C-----------------------------------------------------------------------

      INTEGER  IA(*), JA(*),  V(*), L(*),  HEAD(*), LAST(*), NEXT(*),

     *   MARK(*),  FLAG,  TAG, DMIN, VK,EK, TAIL

      EQUIVALENCE  (VK,EK)

C

C

      TAG = 0

      CALL  MDIN

     *   (N, IA,JA, MAX,V,L, HEAD,LAST,NEXT, MARK,TAG, FLAG)

      IF (FLAG.NE.0)  RETURN

C

      K = 0

      DMIN = 1

C

C

 1    IF (K.GE.N)  GO TO 4

C

C

 2      IF (HEAD(DMIN).GT.0)  GO TO 3

          DMIN = DMIN + 1

          GO TO 2

C

C

 3      VK = HEAD(DMIN)

        HEAD(DMIN) = NEXT(VK)

        IF (HEAD(DMIN).GT.0)  LAST(HEAD(DMIN)) = -DMIN

C

C

        K = K+1

        NEXT(VK) = -K

        LAST(EK) = DMIN - 1

        TAG = TAG + LAST(EK)

        MARK(VK) = TAG

C

C

        CALL  MDM

     *     (VK,TAIL, V,L, LAST,NEXT, MARK)

C

C

        CALL  MDP

     *     (K,EK,TAIL, V,L, HEAD,LAST,NEXT, MARK)

C

C

        CALL  MDU

     *     (EK,DMIN, V,L, HEAD,LAST,NEXT, MARK)

C

        GO TO 1

C

C

 4    DO 5 K=1,N

        NEXT(K) = -NEXT(K)

 5      LAST(NEXT(K)) = K

C

      RETURN

      END

C-----------------------------------------------------------------------

      SUBROUTINE MDIN

     *     (N, IA,JA, MAX,V,L, HEAD,LAST,NEXT, MARK,TAG, FLAG)

C-----------------------------------------------------------------------

C  MDI -- INITIALIZATION

C-----------------------------------------------------------------------

      INTEGER  IA(*), JA(*),  V(*), L(*),  HEAD(*), LAST(*), NEXT(*),

     *   MARK(*), TAG,  FLAG,  SFS, VI,DVI, VJ

C

C

      DO 1 VI=1,N

        MARK(VI) = 1

        L(VI) = 0

 1      HEAD(VI) = 0

      SFS = N+1

C

C

      DO 6 VI=1,N

        JMIN = IA(VI)

        JMAX = IA(VI+1) - 1

        IF (JMIN.GT.JMAX)  GO TO 6

        DO 5 J=JMIN,JMAX

          VJ = JA(J)

          IF (VJ-VI) 2, 5, 4

C

C

 2        LVK = VI

          KMAX = MARK(VI) - 1

          IF (KMAX .EQ. 0) GO TO 4

          DO 3 K=1,KMAX

            LVK = L(LVK)

            IF (V(LVK).EQ.VJ) GO TO 5

 3          CONTINUE

C

 4          IF (SFS.GE.MAX)  GO TO 101

C

C

            MARK(VI) = MARK(VI) + 1

            V(SFS) = VJ

            L(SFS) =L(VI)

            L(VI) = SFS

            SFS = SFS+1

C

C

            MARK(VJ) = MARK(VJ) + 1

            V(SFS) = VI

            L(SFS) = L(VJ)

            L(VJ) = SFS

            SFS = SFS+1

 5        CONTINUE

 6      CONTINUE

C

C

      DO 7 VI=1,N

        DVI = MARK(VI)

        NEXT(VI) = HEAD(DVI)

        HEAD(DVI) =VI

        LAST(VI) = -DVI

        NEXTVI = NEXT(VI)

        IF (NEXTVI.GT.0)  LAST(NEXTVI) = VI

 7      MARK(VI) = TAG

C

      RETURN

C

C

 101  FLAG = 9*N + VI

      RETURN

      END



















