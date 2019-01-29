C   NPFC        - NUMBER OF PF-COILS
C
C   PFCUR1,2(L) - VALUE OF PFC-CURRENT OF TOTAL CROSS-SECTION (in MA)
C                 L = 1,2,...,NPFC
C   PFCW1,2(L)  - VALUE OF PFC-CURRENT OF ONE REAL  TURN (in MA)
C                 L = 1,2,...,NPFC
C   PFCD1,2(L)  - VALUE OF PFC-CURRENT OF ONE MATH. TURN (in MA)
C                 L = 1,2,...,NPFC
C
C   NEPFC(L)   - EQUIVALENT "SIGNS"  OF PF-COILS
C                L = 1,2,...,NPFC
C   WEPFC(L)   - EQUIVALENT "WEIGHTS" OF PF-COILS
C                L = 1,2,...,NPFC
C   NTURN(L)   - NUMBER OF REAL TURNS  OF PF-COILS
C                L = 1,2,...,NPFC
C   NDIV(L)    - NUMBER OF DIVIDING TURNS OF PF-COILS
C                L = 1,2,...,NPFC
C   NLOC(L)    - LOCATION ARRAY OF DIVIDING TURNS OF PF-COILS
C
C                !!! L = 0,1,2,...,NPFC !!! :
C                NLOC(0) = 0, NLOC(1) = NDIV(1),
C                NLOC(2) = NDIV(1) + NDIV(2),...,
C   NCPFC ===>   NLOC(NPFC) = NDIV(1) + NDIV(2) +...+ NDIV(NPFC) = NCPFC
C  ---------------------------------------------------------
C   NEQUI       - NUMBER OF EQUIVALENT PF-COIL GROUPS ( NEQUI .LE. NPFC )
C
C   PFRES(L)    - RESISTANCE OF EQUIVALENT PF-COIL GROUPS
C                 L = 1,2,...,NEQUI
C   PFVOL1,2(L) - VOLTAGE    OF EQUIVALENT PF-COIL GROUPS
C                 L = 1,2,...,NEQUI
C   PFCEQW(L)   - CURRENT    OF EQUIVALENT PF-COIL GROUPS
C                 L = 1,2,...,NEQUI
C  ---------------------------------------------------------
C   NCPFC    - NUMBER OF PFC EQUILIBRIUM CURRENTS
C
C   NECON(L) - EQUIVALENT "SIGNS"  OF PFC EQUILIBR. CURRENTS
C                 L = 1,2,...,NCPFC
C   WECON(L) - EQUIVALENT "WEIGHTS" OF PFC EQUILIBR. CURRENTS
C                 L = 1,2,...,NCPFC
C  ---------------------------------------------------------
C
C   NC       - NUMBER OF ALL EQUILIBR. CURRENTS
C
C   PC(L)    - VALUES (IN MA) OF ALL EQUILIBR. CURRENTS
C                 L = 1,2,...,NC.
C   RC(L),ZC(L) - CILINDER COORD.(IN M) OF ALL EQUILIBR. CURRENTS
C                 L = 1,2,...,NC.
C   NTYPE(L) - NUMBER OF TYPE OF ALL EQUIL. CURRENT CROSS-SECTIONS
C                 L = 1,2,...,NC
C   VC(L)    - VERTICAL SIZE OF ALL EQUILIBR. CURRENT CROSS-SECTIONS
C                 L = 1,2,...,NC
C   HC(L)    - HORIZONTAL SIZE OF ALL EQUILIBR. CURRENT CROSS-SECTIONS
C                 L = 1,2,...,NC
C***********************************************************************




C.......................................................................
C     NGAV1  - PARAMETER OF SETTING OF THE FREE BOUNDARY EQUILIBRIUM
C              PROBLEM
C
C     POSSIBILITY OF CHANGE OF "NGAV1" PARAMETER
C
C    NGAV1 = 0 - given alf0    and  plasma current
C          = 1 - given betpol  and  plasma current
C          = 2 - given alf0    and  psi at mag.axes
C          = 3 - given betpol  and  psi at mag.axes
C          = 4 - given alf0    and  "helicity"
C          = 5 - given betpol  and  "helicity"
