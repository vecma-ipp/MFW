subroutine chease(equil_in,equil_out,param_code)
  !
  !                                        AUTHOR
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !                      **************************
  !                      *                        *
  !                      *      C H E A S E       *
  !                      *                        *
  !                      **************************
  !
  !
  !
  !     (C)UBIC (H)ERMITE (E)LEMENT (A)XISYMETIRC (S)TATIC (E)QUILIBRIUM
  !
  !
  !                    CONVERGENCE TESTS ARE SHOWN IN
  !
  !   [1] H.LUETJENS, A.BONDESON, A.ROY, COMP.PHYS.COMM 69 (1992) P.287-298
  !
  !      A full description of the code and the numerical methods of
  !                    solution are described in
  !
  !   [2] H.LUETJENS, A.BONDESON, O.SAUTER, COMP.PHYS.COMM 97 (1996) P.219-260
  !
  !   COMPILATION DIRECTIONS: 
  !   -----------------------
  !   THIS PROGRAM MUST IMPERATIVELY BE COMPILED AT LEAST IN 64BITS
  !   PRECISION, I.E. SINGLE PRECISION ON CRAY, DOUBLE-PRECISION
  !   ON 32BIT WORKSTATIONS. THIS PROGRAM CONTAINS MANY SUBROUTINES
  !   AND THERE ARE COMMANDS UP TO 25 FORTRAN LINES AND DO LOOPS
  !   WITH SEVERAL HUNDREDS FORTRAN LINES. THEREFORE, THE USER MUST
  !   BE AWARE THAT THE DEFAULT SET-UP OF HIS LOCAL COMPILER CAN BE
  !   UNADAPTED FOR THE COMPILATION OF CHEASE. 
  !
  !   ON CRAY SYSTEMS, CHEASE SHOULD BE COMPILED WITH
  !
  !   cf77 -Wf"-o agress" -Zp -l sci chease.f
  !
  !   ON 32 BITS SUN SPARC-10 WORKSTATIONS WITH
  !
  !   f77 -O -Nx300 -Nl30 -r8 -i4 chease.f
  !
  !   ON 32 BITS HP-K200 WORKSTATIONS WITH
  !
  !   f77 -O +Onolimit +autodblpad chease.f
  !   
  !     TEST CASES : 
  !     ------------
  !
  !     1) SOLOVEV EQUILIBRIUM
  !     ----------------------
  !        INPUT CHANNEL 5:
  !
  !          ***
  !          ***
  !          ***
  !          ***
  !           $EQDATA
  !             NTCASE=1,
  !             NCHI=100,NPSI=15,NS= 30,NT= 30,
  !           $END
  !           $NEWRUN
  !           $END 
  !
  !     2) JET EQUILIBRIUM. J_PHI SPECIFIED WITH TT-PRIME AND P-PRIME
  !     -------------------------------------------------------------
  !        INPUT CHANNEL 5:
  !
  !          ***
  !          ***
  !          ***
  !          ***
  !           $EQDATA
  !             NTCASE=2,
  !             NCHI=100,NPSI=15,NS= 30,NT= 30,
  !           $END
  !           $NEWRUN
  !           $END 
  !
  !     3) NET EQUILIBRIUM. J_PHI SPECIFIED WITH I* AND P-PRIME
  !     -------------------------------------------------------
  !
  !        INPUT CHANNEL 5:
  !
  !          ***
  !          ***
  !          ***
  !          ***
  !           $EQDATA
  !             NTCASE=3,
  !             NCHI=100,NPSI=15,NS= 30,NT= 30,
  !           $END
  !           $NEWRUN
  !           $END 
  !
  !     4) ASYMMETRIC EQUILIBRIUM. J_PHI SPECIFIED WITH I* AND P-PRIME
  !     --------------------------------------------------------------
  !        INPUT CHANNEL 5:
  !
  !          ***
  !          ***
  !          ***
  !          ***
  !           $EQDATA
  !             NTCASE=4,
  !             NCHI=100,NPSI=15,NS= 30,NT= 30,
  !           $END
  !           $NEWRUN
  !           $END 
  !
  !
  !                            **********
  !
  !     LIST OF SUBROUTINES
  !     ---------------------
  !
  !     MASTER       CONTROLS THE RUN                        0.01
  !
  !
  !     LABRUN       LABEL THE RUN                           1.01
  !     CLEAR        CLEAR ALL COMMONS                       1.02
  !     PRESET       SET UP THE DEFAULT CASE                 1.03
  !     DATA         READ NAMELIST                           1.04
  !     AUXVAL       SET UP AUXILLIARY VALUES                1.05
  !     COTROL       CONTROL READ IN PARAMETERS              1.06
  !     TCASE        SET UP TEST CASES                       1.07
  !
  !     STEPON       LEAD THE CALCULATIONS                   2.01
  !     BALLIT       LEAD BALLOONING OPTIMZATION AND 
  !                  SPECIFICATION OF BOOTSTRAP CURRENT      2.02
  !     INITIA       INITIALIZE VERTICAL MATRIX INDEXATION
  !                  AND QUADRATURE POINTS FOR EQUILIBRIUM
  !                  INTEGRATION                             2.03
  !     EQDIM        SET UP SMALL AND FINAL EQUILIBRIUM      2.04
  !     MATRIX       LEAD CONSTRUCTION AND LDLT
  !                  DECOMPOSITION OF EQ MATRIX              2.05
  !     ITIPR        LEAD ITERATION OVER CURRENT PROFILE     2.06
  !     NONLIN       LEAD ITERATION OVER NONLINEARITY        2.07
  !     CHECK        COMPUTE GLOBAL RESIDU OF A * PSI - B    2.08
  !     OLDNEW       STORE  CONVERGED EQUILIBRIUM            2.09
  !     OLDEQ        READ CONVERGED EQUILIBRIUM              2.10
  !
  !     MESH         SET UP DISCRETIZATION MESHES            2.A01
  !     PACKME       MESH PACKING WITH LORENTZIANS           2.A02
  !     PSVOL        S-MESH PACKING SO THAT D(RHO) / DS = 0  2.A03
  !     TETARE       AUTOMATIC THETA-MESH PACKING            2.A04
  !     QPLACS       S-MESH PACKING AT PREDEFINED Q-VALUES   2.A05
  !     PACKMEP      AS PACKME, BUT FOR 2*PI PERIODIC MESHES 2.A06
  !
  !     GUESS        INITIALIZE PICARD ITERATION             2.B01
  !
  !     MAGAXE       FIND MAGNETIC AXIS                      2.C01
  !     EVLATE       EVALUATE PSI, D(PSI)/D(R) AND
  !                  D(PSI)/D(Z) AT (R,Z)                    2.C02
  !
  !     SETUPA       CONSTRUCT A                             2.D01
  !     SETUPB       CONSTRUCT B                             2.D02
  !     LIMITA       IMPOSE BOUNDARY CONDITIONS ON A         2.D03
  !     LIMITB       IMPOSE BOUNDARY CONDITIONS ON B         2.D04
  !     IDENTA       PERFORM ROW AND COLUMN OPERATIONS IN A  2.D05
  !     IDENTB       PERFORM ROW OPERATIONS IN B             2.D06
  !     AWAY         REMOVE 1 ROW AND COLUMN IN A            2.D07
  !     CENTER       EVALUATE COEFFICENTS REQUIRED TO
  !                  IMPOSE BOUNDARY CONDITIONS              2.D08
  !
  !     SOLVIT       SOLVE GRAD-SHAFRANOV EQUATION           2.E01
  !     DIRECT       GAUSS ELIMINATION                       2.E02
  !     ERROR        COMPUTE ERROR ON PSI                    2.E03
  !     ENERGY       COMPUTE AVERAGED POLOIDAL MAGNETIC 
  !                  FIELD ENERGY                            2.E04
  !     SMOOTH       BICUBIC SPLINE SMOOTHING OF BICUBIC
  !                  HERMITE EQUILIBRIUM SOLUTION            2.E05
  !     CONVER       CONVERGENCE TESTS                       2.E06
  !
  !     NOREPT       EQUILIBRIUM TRANSFORMATIONS             2.F01
  !     RSCALE       SCALE EQUILIBRIUM AGAINST R OF
  !                  MAGNETIC AXIS                           2.F02
  !     TSHIFT       SHIFT TOROIDAL FLUX PROFILE             2.F03
  !     PRNORM       SCALE EQUILIBRIUM
  !
  !     TEST         COMPUTE RELATIVE ERROR FOR SOLOVEV      2.G01
  !     SOLOVEV      COMPUTE ANALYTIC SOLOVEV EQUILIBRIUM    2.G02
  !
  !     MAPPIN       LEAD COMPUTATION OF MAPPINGS            2.M01
  !     SURFACE      INTEGRATION OF LOCAL AND GLOBAL FLUX
  !                  SURFACE QUANTITIES                      2.M02
  !     CHIPSI       INTERPOLATION OF LOCAL FLUX SURFACE
  !                  QUANTITES ON ERATO MESH                 2.M03
  !     ERDATA       COMPUTE EQ'S FOR ERATO AND LOCAL SHEAR
  !                  AND ZERO LINE OF AVERAGED MAGNETIC
  !                  FIELD LINE CURVATURE                    2.M04
  !     CINT         COMPUTE INTEGRALS NEEDED TO OBTAIN THE
  !                  T-TPRIME PROFILE FROM THE I-PRIME AND
  !                  THE P-PRIME PROFILE                     2.M05
  !     PREMAP       LEAD COMPUTATION OF PROFILES ON S-MESH  2.M06
  !     GCHI         INTERPOLATE LOCAL FLUX SURFACE
  !                  QUANTITIES ON GAUSS INTEGRATION POINTS  2.M07
  !     GIJLIN       COMPUTE LOCAL QUANTITES NEEDED BY MARS  2.M08
  !     FOURIER      PERFORM FOURIER TRANSFORM OF QUANTITIES
  !                  COMPUTED BY GIJLIN                      2.M09
  !     PROFILE      COMPUTE PROFILES ON S-MESH              2.M10
  !     BALOON       BALOONING STABILITY AND LOCAL 
  !                  INTERCHANGE TESTS                       2.M11
  !     GLOQUA       COMPUTE AUXILIARY GLOBAL FLUX SURFACE
  !                  QUANTITES                               2.M12
  !     VACUMM       COMPUTE VACUUM EQ'S FOR MARS            2.M13
  !     VLION        COMPUTE EQ'S FOR LION AT PLASMA SURFACE 2.M14
  !     OUTNVW       COMPUTE EQ'S FOR NOVA-W AND PEST        2.M15
  !     STCHPS       COMPUTE SIGMA(PSI,CHI) AND
  !                          THETA(PSI,CHI) FOR NOVA-W       2.M16
  !     JNOVAW       COMPUTE R,Z AND STABILITY MESH 
  !                  JACOBIAN AT (SIGMA, THETA)              2.M17
  !     TPSI         COMPUTE SIGMA(PSI,THETA-PENN) AND
  !                          THETA(PSI,THETA-PENN)           2.M20
  !     OUTPEN       COMPUTE EQ'S FOR PENN A-PENN)           2.M21
  !     OUTXT        COMPUTE AND WRITES DATA FOR XTOR
  !
  !     FOURFFT      COMPUTE FAST FOURIER TRANSFORMS OF ALL 
  !                  EQ'S FOR MARS INSIDE THE PLASMA         2.M22
  !     VACUFFT      COMPUTE FAST FOURIER TRANSFORMS OF ALL 
  !                  EQV'S FOR MARS IN THE VACUMM            2.M23
  !     SPLIFFT      COMPUTES CUBIC SPLINE INTERPOLATION 
  !                  AND FAST FOURIER TRANSFORM              2.M24
  !
  !     CURENT       COMPUTE CURRENT DENSITY FOR GIVEN PSI   2.J01
  !                  VALUES
  !     ISOFUN       COMPUTE T, T-PRIME, P AND P-PRIME ON
  !                  S-MESH                                  2.J02
  !
  !     PRFUNC       FUNCTIONAL FORM FOR PRESCRIPTION OF
  !                  TT'(S),I*(S) OR I_PARA(S)               2.I01
  !     ATCOEF       TT'(S),I*(S) OR I_PARA(S) WITH AT'S     2.I02
  !     COPYAT       COMPUTE COEFFICIENTS OF POLYNOMIAL
  !                  SECTIONS FROM AT'S                      2.I03
  !
  !     PPRIME       COMPUTE P-PRIME PROFILE                 2.P01
  !     BSFUNC       COMPUTE FUNCTION FOR FRACTION OF
  !                  BOOTSTRAP CURRENT                       2.P02
  !     PPSPLN       SPLINE INTERPOLATION OF P-PRIME         2.P03
  !     APCOEF       COMPUTE P-PRIME WITH AP'S               2.P04
  !     COPYAP       COMPUTE COEFFICENT OF POLYNOMIAL 
  !                  SECTIONS FROM AP'S                      2.P05
  !     APCOEF2      ALTERNATIVE VERSION OF APCOEF           2.P06
  !     COPYAPP      ALTERNATIVE VERSION OF COPYAPP          2.P07
  !     BLTEST       LEAD BALLOONING AND MERCIER STABILITY
  !                  CALCULATION FOR BALLOONING OPTIMIZATION 2.P08
  !     RESPPR       INITIALIZE PROFILES FOR BALLOONING
  !                  OPTIMIZATION                            2.P09
  !     PPRM         MODIFICATION P-PRIME PROFILE DURING
  !                  BALLOONING OPTIMIZATION                 2.P10
  !     PPBSTR       LEAD COMPUTATION OF P-PRIME WHEN
  !                  BOOTSTRAP CURRENT DENSITY IS SPECIFIED  2.P11
  !
  !     POLYNM       COMPUTE POLYNOMIAL COEFFICIENTS OF
  !                  DENSITY AND TEMPERATURE WHEN CURRENT
  !                  DENSITY IS GIVEN IN TERMS OF DENSITY
  !                  AND TEMPERATURE                         2.T01
  !     DRHODP       COMPUTE D(RHO)/D(PSI) FOR A GIVEN SET
  !                  OF PSI VALUES                           2.T02
  !
  !     ISOFIND      LEAD TRACING OF CONSTANT FLUX SURFACES  2.U01
  !     CUBRT        TRACE CONSTANT FLUX SURFACES            2.U02
  !     GAUSS        GAUSS QUADRATURE QUANTITES IN
  !                  [0; 1] INTERVAL                         2.U03
  !     RMRAD        COMPUTE INTERSECTIONS OF Z OF MAGNETIC
  !                  AXIS WITH CONSTANT FLUX SURFACES        2.U04
  !
  !     BOUND        COMPUTE PLASMA SURFACE                  2.X01
  !     BNDSPL       CUBIC SPLINE INTERPOLATION OF 
  !                  EXPERIMENTAL BOUNDARY POINTS            2.X03
  !     SUBZ         SHIFT EXPERIMENTAL BOUNDARY VERTICALLY  2.X04
  !     RZBOUND      COMPUTE (R,Z) COORDINATES OF BOUNDARY   2.X05
  !
  !     BASIS1       COMPUTE BASIS FUNCTIONS AT GAUSS 
  !                  INTEGRATION POINTS                      2.Y01
  !     BASIS2       COMPUTE FIRST DERIVATIVES OF BASIS 
  !                  FUNCTIONS AT GAUSS INTEGRATION POINTS   2.Y02
  !     BASIS3       COMPUTE FIRST AND 2ND DERIVATIVES OF 
  !                  BASIS FUNCTIONS AT GAUSS INTEGRATION 
  !                  POINTS                                  2.Y03
  !     BASIS4       COMPUTE FIRST DERIVATIVES OF BASIS 
  !                  FUNCTIONS IN SIGMA DIRECTION AT GAUSS 
  !                  INTEGRATION POINTS                      2.Y04
  !     PSICEL       COMPUTE VARIABLES DEFINING THE BICUBIC
  !                  EXPANSIONS OF PSI IN KN CELLS           2.Y05
  !     PSIBOX       EVALUATE PSI ON A (R,Z) GRID            2.Y06
  !
  !     OUTPUT       CONTROL INPUT/OUTPUT                    3.A01
  !     PRIQQU       PRINT EQUILIBRIUM QUANTITIES Q-VALUES   
  !                  SPECIFIED IN QPLACS                     3.A02
  !     OUTMKSA      SAVE EQUILIBRIUM QUANTITIES IN MKSA     3.A03
  !
  !     IODISK       PERFORM DISK FILE OPERATIONS            3.B01
  !     WRPLOT       WRITE PLOT QUANTITIES                   3.B02
  !     SHAVE        SHAVE AWAY OUTER POLOIDAL FLUX SURFACES 3.B03
  !     SURFRZ       SAVE (R,Z)'S OF LAST FLUX SURFACE       3.B04
  !     NERAT        COMPUTE ERATO NAMELIST                  3.B05
  !     GENOUT       GENERAL OUTPUT ROUTINE USED TO
  !                  CONSRUCT INPUT FILE OF LINEAR RESISTIF
  !                  CODE                                    3.B06
  !
  !
  !
  !                            **********
  !
  !     AUXILLIARY SUBROUTINES :
  !     ------------------------
  !
  !     ALDLT        DECOMPOSE A = L*D*LT                    MR01
  !     LYV          SOLVE  L*Y  = V                         MR02
  !     DWY          SOLVE  D*W  = Y                         MR03
  !     LTXW         SOLVE  LT*X = W                         MR04
  !
  !     SPLINE       CUBIC SPLINE INTERPOLATION. BOUNDARY
  !                  CONDITIONS BY CUBIC LAGRANGE 
  !                  INTERPOLATION                           MSP01
  !     MSPLINE      AS SPLINE, BUT DOES M INTERPOLATIONS 
  !                  IN PARALLEL                             MSP02
  !     SPLCY        CUBIC SPLINE INTERPOLATION. PERIODIC
  !                  BOUNDARY CONDITIONS.                    MSP03
  !     MSPLCY       AS SPLCY, BUT DOES M INTERPOLATIONS 
  !                  IN PARALLEL                             MSP04
  !     SPLCYP       CUBIC SPLINE INTERPOLATION OF A PERIODIC
  !                  FUNCTION WITH PERIODIC DEFINITION 
  !                  INTERVAL                                MSP05
  !
  !     NTRIDG       LU DECOMPOSE ND2-ND1+1 TRIDIAGONAL
  !                  SYSTEMS                                 MRD01
  !     TRIDAGM      INVERT M TRIDIAGONAL SYSTEMS IN 
  !                  PARALLEL                                MRD02
  !     TRICYC       INVERT 1 TRIDIAGONAL SYSTEM WITH
  !                  PERIODIC BOUNDARY CONDITIONS            MRD03
  !     TRICYCM      INVERT M TRIDIAGONAL SYSTEM WITH
  !                  PERIODIC BOUNDARY CONDITIONS IN
  !                  PARALLEL                                MRD04
  !
  !     PAGE         JUMP A PAGE                             U1
  !     BLINES       JUMP N LINES                            U2
  !     MESAGE       WRITE A 48-CHARACTERS STRING            U10
  !     RVAR         WRITE NAME AND REAL VALUE               U20
  !     RVAR2        WRITE 2*(NAME AND REAL VALUE)           U21
  !     RVAR3        WRITE 3*(NAME AND REAL VALUE)           U22
  !     IVAR         WRITE NAME AND INTEGER VALUE            U23
  !     IVAR2        WRITE 2*(NAME AND INTEGER VALUE)        U24
  !     IVAR3        WRITE 3*(NAME AND INTEGER VALUE)        U25
  !     HVAR         WRITE NAME AND CHARACTER VALUE          U26
  !     LVAR         WRITE NAME AND LOGICAL VALUE            U27
  !     RARRAY       WRITE NAME AND REAL ARRAY               U30
  !     IARRAY       WRITE NAME AND INTEGER ARRAY            U31
  !     LARRAY       WRITE NAME AND LOGICAL ARRAY            U32
  !     HARRAY       WRITE NAME AND CHARACTER ARRAY          U33
  !     SARRAY       WRITE NAME AND SCALED VALUES OF REAL    U34
  !     0ARRAY       WRITE NAME AND REAL ARRAY INTO NCHAN    U35
  !     WRTEXT       WRITE REAL VARIABLE NAME AND VALUE INTO
  !                  TEXT ARRAY                              U36
  !     WITEXT       WRITE INTEGER VARIABLE NAME AND VALUE
  !                  INTO TEXT ARRAY                         U37
  !     WHTEXT       WRITE TEXT VARIABLE INTI TEXT ARRAY     U38
  !     RESETR       RESET REAL ARRAY                        U40
  !     RESETI       RESET INTEGER ARRAY                     U41
  !     RESETH       RESET CHARACTER ARRAY                   U42
  !     RESETL       RESET LOGICAL ARRAY                     U43
  !     RESETC       RESET COMPLEX ARRAY                     U44
  !     SCOPYR       Y = Y + RF * (X - Y)                    U49
  !
  !     RUNTIM       UPDATE CPU TIME AND PRINT IT            U50
  !     DAYTIM       PRINT OUT DATE AND TIME                 U51
  !
  !     VZERO        X = 0 (X REAL)                          MAT7
  !     CVZERO       X = 0 (X COMPLEX)                       MAT8
  !     ACOPY        Y = X (X,Y REAL) WITH INTERMEDIATE
  !                        STORAGE                           MAT9
  !     ICOPY        Y = X (X,Y INTEGER) WITH INTERMEDIATE
  !                        STORAGE                           MAT10
  !     CCOPY        Y = X (X,Y COMPLEX) WITH INTERMEDIATE
  !                        STORAGE                           MAT11
  !
  !                            **********
  !
  !     CRAY SUBROUTINES AND FUNCTIONS :
  !     --------------------------------
  !     FOR MORE DETAILS, SEE CRAY LIBRARY MANUAL.
  !
  !     ISMAX
  !     ISMIN
  !     ISAMIN
  !     ISRCHFGT
  !     ISRCHFGE
  !     DAXPY  
  !     DCOPY  
  !     DDOT
  !     DSCAL  
  !     SSUM
  !     ISSUM     
  !
  !                            **********
  !
  !     NUMERICAL RECIPES SUBROUTINES
  !     -----------------------------
  !     FOR MORE DETAILS, SEE NUMERICAL RECIPES, W.H.PRESS,B.P.FLANNERY,
  !     S.A.TEUKOLSKY, W.T.VETTERLING, CAMBRIDGE UNIVERSITY PRESS 1986,1989,
  !     FORTRAN VERSION.
  !
  !     SORT3
  !     INDEXX
  !     REALFT
  !     FOUR1
  !
  !                            **********
  !
  !     DISK CHANNELS :                     SEE PUBLICATION, TABLE 5
  !     ---------------
  !     LIST OF COMMONS :                   SEE PUBLICATION, TABLE 8
  !     -----------------
  !     LIST OF STATEMENT FUNCTIONS DECKS : SEE PUBLICATION, TABLE 7
  !     -----------------------------------
  !     EQUILIBRIUM NAMELIST VARIABLES :    SEE PUBLICATION, TABLE 9-11
  !     --------------------------------
  !
  !                            **********
  !
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  use globals
  use euITM_schemas                       ! module containing the equilibrium type definitions
  use euitm_xml_parser
!!$  use euitm_routines
  use assign_chease_codeparameters_choices_xml2eg

  IMPLICIT NONE

  interface
     subroutine assign_code_parameters(codeparameters, return_status)

       use prec_const

       use euitm_schemas
       use euitm_xml_parser  
       use globals

       implicit none

       type (type_param), intent(in) :: codeparameters
       integer(ikind), intent(out) :: return_status 
     end subroutine assign_code_parameters
  end interface

  type(type_equilibrium),pointer      :: equil_in(:), equil_out(:)
  type(type_param) :: param_code
  integer istatus, iout_size, i, isize_comment
  !
  !**********************************************************************
  !
  call cpu_time(stime)
  !
  if (associated(equil_in)) then
    iout_size = size(equil_in)
  else
    iout_size = 1
  end if
  iout_size = 1
  !
  ! If within KEPLER, do here what was performed in chease_prog.f90, except loading shot
  !
  !  NITMOPT=22
  IF (NVERBOSE .GE. 0) write(0,*) 'NITMOPT = ',NITMOPT
  if (associated(equil_in)) then
    IF (NVERBOSE .GE. 1) write(*,*) 'equil_in(1)%eqgeometry%geom_axis%r = ',equil_in(1)%eqgeometry%geom_axis%r
    IF (NVERBOSE .GE. 1) write(*,*) 'equil_in(1)%eqgeometry%geom_axis%r = ',equil_in(1)%eqgeometry%geom_axis%r
  end if
  if (allocated(CPSICL)) then
    print *,'CPSICL allocated'
  else
    print *,'CPSICL NOT allocated'
  endif
  IF (NITMOPT .EQ. 22) THEN
    ! ASSUME WITHIN KEPLER, IN/OUT EQUILIBRIUM VIA ARGUMENTS ONLY
    ! NAMELIST VALUES WITHIN EQUIL_IN(1)%CODEPARAM%PARAMETERS(:)
    !
    CALL PRESET
    ! call assign_chease_codeparameters_reflist(param_code,istatus)
    call assign_chease_codepar_choices(param_code,istatus)
    if (istatus /= 0) then
      IF (NVERBOSE .GE. 0) write(0, *) 'ERROR: Could not assign some code parameters.'
      return
    end if
    !
    ! set shot/run to equil read/write from kepler: 22
    IF (NVERBOSE .GE. 1) THEN
      print *,' nitmshot= ',nitmshot
      print *,' nitmrun= ',nitmrun
      print *,' nitmopt= ',nitmopt
      print *,' ndiagop= ',ndiagop
    END IF
    ! avoid: call euitm_copy(equil_in,eqchease_in) so no need for module euitm_routines
!!$     call euitm_copy(equil_in,eqchease_in) 
    eqchease_in => equil_in
    IF ( (associated(eqchease_in)) .and. (NVERBOSE .GE. 1)) write(*,*) 'eqchease_in(1)%eqgeometry%geom_axis%r = ',eqchease_in(1)%eqgeometry%geom_axis%r
    !
    ! allocate only for one time point outputs of chease at this stage:
    iout_size = 1
!!$     if (associated(equil_out)) then
!!$        iout_size = size(equil_out)
!!$     else
!!$        allocate(equil_out(1))
!!$     end if
    if (size(equil_in) .NE. 1) then
      IF (NVERBOSE .GE. 1) print *,'(size(equil_in)= ',size(equil_in),' .NE. 1), not sure is ok now, add copy slice'
    end if
    !
    ! Assign some values related to codeparam when passing within kepler
    isize_comment = size(comments)
    do i=1,isize_comment
      if (adjustl(trim(comments(i))) == '') then
        ! comments not given in codeparam (and no labels from chease_namelist file possible
        comments(i) = 'Chease run from Kepler'
        exit
      end if
    end do
  END IF
  !
  if (iout_size > 1) print *,'iout_size = ',iout_size,' > 1: is it correct?'
  !  
  ! Starts clock for cpu time calculations
  IF (NVERBOSE .GE. 1) CALL RUNTIM
  ! put here any modifications of the main mesh values and not in AUXVAL
  ! since they are used in globals_init to set the array dimensions
  IF(NIDEAL .EQ. 7) THEN
    ! seems outgyro uses too much memory and gets segmentation fault if nrbox>240
    ! Some bugs fixed so now seems OK. Might need to add ",256" below if needed
    NRBOX = MIN(NRBOX,3*NPISOEFF)
    NZBOX = MIN(NZBOX,3*NPISOEFF)
    NPSI  = NRBOX - 1
    NPSI1 = NRBOX
  END IF
  !
  CALL globals_init
  CALL TCASE
  CALL COTROL
  CALL AUXVAL
  !
  ! Note: should not cpoy eqchease_in to eqchease_out since some data might not be overwritten and then inconsistency happens
  ! to avoid using module with euitm_copy, using pointer assignement for equil_out, thus cannot deallocate eqchease_out
  ! Otherwise makes kepler crash when run 2nd time, so just (re-)allocate here
  allocate(eqchease_out(iout_size))
  do i=1,iout_size
    if (nprof2d .eq. 0) then
      allocate(eqchease_out(i)%profiles_2d(1))
    else
      allocate(eqchease_out(i)%profiles_2d(int(nprof2d/10)+2))
    end if
    allocate(eqchease_out(i)%eqgeometry%boundary(1))
    ! copy input parameters to codeparam
    if (associated(param_code%parameters)) then
      allocate(eqchease_out(i)%codeparam%parameters(size(param_code%parameters)))
      eqchease_out(i)%codeparam%parameters = param_code%parameters
    end if
    if (associated(eqchease_in)) then
      eqchease_out(i)%time = eqchease_in(i)%time
    else
      eqchease_out(i)%time = -1._rkind
    end if
  end do
  !
  !**********************************************************************
  !                                                                     *
  ! 3. STEP ON THE CALCULATION                                          *
  !                                                                     *
  !**********************************************************************
  !
  CALL STEPON
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! NOTE THAT NOW eqchease_out IS IN SI UNITS WITH COCOS_OUT CONVENTION
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  IF (NVERBOSE .GE. 1) CALL RUNTIM
  !
  ! avoid: call euitm_copy(eqchease_out,equil_out) so no need for module euitm_routines
  !!$  call euitm_copy(eqchease_out,equil_out)
  equil_out => eqchease_out
  IF (NVERBOSE .GE. 2) THEN
    print *,' equil_out(1)%time= ',equil_out(1)%time
    ! equil_out(1)%time=1._rkind
    !  print *,' equil_out(1)%time= ',equil_out(1)%time
    print *,' nitmopt= ',nitmopt
    print *,' ndiagop= ',ndiagop
    write(*,*) 'equil_out(1)%eqgeometry%geom_axis%r = ',equil_out(1)%eqgeometry%geom_axis%r
  END IF
  IF (associated(equil_out(1)%eqgeometry%boundary(1)%r)) THEN
    IF (NVERBOSE .GE. 2) write(*,*) 'equil_out(1)%eqgeometry%boundary(1)%r(1:2) = ',equil_out(1)%eqgeometry%boundary(1)%r(1:2)
  ELSE
    IF (NVERBOSE .GE. 0) write(0,*) 'equil_out(1)%eqgeometry%boundary(1)%r not associated, problems with CHEASE? did not converge?'
  END IF
  !
  ! Deallocate arrays
  !
!!$  CALL g_1   ! Deallocate dynamic arrays
!!$  if (associated(eqchease_in)) deallocate(eqchease_in)
!!$  if (associated(eqchease_out)) deallocate(eqchease_out)
  !
END subroutine chease
