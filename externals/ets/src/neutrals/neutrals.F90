! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
      MODULE NEUTRALS

             CONTAINS



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

      SUBROUTINE NEUTRALS_ETS  (COREIMPUR_ITER,   EQUILIBRIUM_ITER,  COREPROF_ITER,   &
                                CORENEUTRALS_OLD, CORENEUTRALS_ITER,                  &
                                CORESOURCE_NEW,   CORENEUTRALS_NEW,                   &
		                CONTROL_INTEGER,  CONTROL_DOUBLE)									 

                                  
	  
!-------------------------------------------------------!
!     This routine provides interface between CPO       !
!     derived types and internal ETS derived types.     !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   R. Stankiewicz, I. Ivanova-Stanik   !
!     Contacts:     romsta@ifpilm.waw.pl                !
!                                                       !
!     Comments:     input data for NEUTRALS MODULE      !
!                                                       !
!                                                       !
!     D.Kalupin:    Some corrections are introduced     !
!-------------------------------------------------------!


      USE EUITM_SCHEMAS
      USE ETS_PLASMA
      USE TYPE_SOLVER    
      USE COPY_STRUCTURES
      USE DEALLOCATE_STRUCTURES
      USE ITM_TYPES
      USE ITM_CONSTANTS
      USE ALLOCATE_DEALLOCATE

#ifdef GOT_AMNSPROTO
      USE amns_types
      USE amns_module
      USE euitm_routines
#endif 


      IMPLICIT NONE

! +++ Input/Output to numerical solver:
      TYPE (NUMERICS)                   :: SOLVER		     !contains all I/O quantities to numerics part

! +++ CPO derived types:
      TYPE (TYPE_CORENEUTRALS),POINTER  :: CORENEUTRALS_OLD(:)       !input CPO with neutrals
      TYPE (TYPE_CORENEUTRALS),POINTER  :: CORENEUTRALS_ITER(:)      !input CPO with neutrals
      TYPE (TYPE_CORENEUTRALS),POINTER  :: CORENEUTRALS_NEW(:)       !input CPO with neutrals
      TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_ITER(:)       !input CPO with geometry quantities from previous ITERration
      TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_ITER(:)          !input CPO with plasma profiles
      TYPE (TYPE_COREIMPUR),   POINTER  :: COREIMPUR_ITER(:)         !input CPO with impurities
      TYPE (TYPE_CORESOURCE),  POINTER  :: CORESOURCE_NEW(:)         !output CPO for neutral radiation
      

! +++ Dimensions:
     INTEGER                            :: NEQ                       !number of radial points     (input, determined from EQUILIBRIUM CPO)
     INTEGER                            :: NRHO                      !number of radial points     (input, determined from COREPROF CPO)
     INTEGER                            :: NION                      !number of ion species       (input, determined from COREPROF CPO)
     INTEGER                            :: NIMP                      !number of impurity species  (input, determined from COREIMPUR CPO)
     INTEGER,              ALLOCATABLE  :: NZIMP(:)
     INTEGER,              ALLOCATABLE  :: NN_BND_TYPE(:,:)          !boundary condition, type, one neutral
     INTEGER                            :: NNEUT                     !number of neutrals species                (input)
     INTEGER                            :: NNUCL                     !number of neutrals species                (input)
     INTEGER,              ALLOCATABLE  :: NTYPE(:)                  !number of impurity ionization states (input)     
     INTEGER,              ALLOCATABLE  :: NCOMP(:)                  !max_number of distinct atoms enter the composition-"1" wich is neutral
     INTEGER,                PARAMETER  :: NOCUR = 1                 !number of CPO ocurancies in the work flow
     INTEGER                            :: ZMAX, TMAX 

! +++ Indexes:
     INTEGER                            :: IRHO                      !
     INTEGER                            :: IION                      !
     INTEGER                            :: IIMP                      !
     INTEGER                            :: ICOMP                     !
     INTEGER                            :: ITYPE                     !
     INTEGER                            :: INEUT                     !
     INTEGER                            :: INUCL                     !
     INTEGER                            :: IZ                        !
     INTEGER                            :: HOT_NEUTRALS, COLD_NEUTRALS


   
     REAL (R8),            ALLOCATABLE  :: RHO(:)                    !toroidal flux coordinate,not normalise,[m]  
     REAL (R8),            ALLOCATABLE  :: ANEUT(:)                  !masa of the neutral 
     REAL (R8),            ALLOCATABLE  :: VPR(:)                    !V',                                    [m^2]
     REAL (R8),            ALLOCATABLE  :: VPRM(:)                   !V' (at previous time step),            [m^2]
     REAL (R8),            ALLOCATABLE  :: G3(:)                     !<(nabla_rho)^2>,                       [-]
     REAL (R8),            ALLOCATABLE  :: NE(:)                     !electron density                      [m^-3]
     REAL (R8),            ALLOCATABLE  :: TE(:)		     !electron temperature
     REAL (R8),            ALLOCATABLE  :: NI(:,:)                   !ion density of "1"-ionization state          [m^-3]
     REAL (R8),            ALLOCATABLE  :: TI(:)		     !ion temperature of "1"-ionization state
     REAL (R8),            ALLOCATABLE  :: DN0(:,:,:)                !density gradient,                           [m^-4]
     REAL (R8),            ALLOCATABLE  :: FLUX(:,:,:)               !ion flux,                                   [1/s]
     REAL (R8),            ALLOCATABLE  :: FLUX_INTER(:,:,:)         !ion flux,                                   [1/s]
     REAL (R8),            ALLOCATABLE  :: N0(:,:,:)		     !one neutral density 
     REAL (R8),            ALLOCATABLE  :: N0M(:,:,:)	       	     !old one neutral density 
     REAL (R8),            ALLOCATABLE  :: T0(:,:,:)                 !temperature one neutrals,                 [eV] 
     REAL (R8),            ALLOCATABLE  :: VT(:,:,:)                 !toroidal velosicty for one neutrals,
     REAL (R8),            ALLOCATABLE  :: VR(:,:,:)                 !radial velosicty for one neutrals
     REAL (R8),            ALLOCATABLE  :: VP(:,:,:)                 !poloidal velosicty for one neutrals 
     REAL (R8),            ALLOCATABLE  :: VCONV(:,:,:) 	     !pinch velocity for different ionisation               [m/s]
     REAL (R8),            ALLOCATABLE  :: DIFF(:,:,:) 
     REAL (R8),            ALLOCATABLE  :: NN_BND(:,:,:)             !boundary condition, value, one neutral[depends on NZ_BND_TYPE]
     REAL (R8),            ALLOCATABLE  :: NNSOURCE(:,:,:)           !value of the source term,[m^-3.s^-1]
     REAL (R8)                          :: AMIX, TAU                 !mixing factor, time step,              [s]

! dimension for solver 
     INTEGER,                INTENT(IN) :: CONTROL_INTEGER(1)          !integer control parameters
     REAL (R8),              INTENT(IN) :: CONTROL_DOUBLE(2)           !real control parameters
     INTEGER                            :: solut_method
     INTEGER                            :: FLAG                       !flag for equation: 0 - interpretative (not solved), 1 - predictive (solved)
     INTEGER                            :: NDIM                       !number of equations to be solved
     INTEGER                            :: SOLVER_TYPE                !specifies the option for numerical solution
     REAL (R8),            ALLOCATABLE  :: Y(:)                       !function at the current amd previous time steps
     REAL (R8),            ALLOCATABLE  :: YM(:)                      !function at the current amd previous time steps
     REAL (R8),            ALLOCATABLE  :: DY(:)                      !derivative of function
     REAL (R8),            ALLOCATABLE  :: A(:)                       !coefficients for numerical solver
     REAL (R8),            ALLOCATABLE  :: B(:)                       !coefficients for numerical solver
     REAL (R8),            ALLOCATABLE  :: C(:)                       !coefficients for numerical solver
     REAL (R8),            ALLOCATABLE  :: D(:)                       !coefficients for numerical solver
     REAL (R8),            ALLOCATABLE  :: E(:)                       !coefficients for numerical solver
     REAL (R8),            ALLOCATABLE  :: F(:)                       !coefficients for numerical solver
     REAL (R8),            ALLOCATABLE  :: G(:)                       !coefficients for numerical solver
     REAL (R8)                          :: H                          !coefficients for numerical solver
     REAL (R8)                          :: V(2), U(2), W(2)           !boundary conditions for numerical solver
     REAL (R8),            ALLOCATABLE  :: FUN(:), INTFUN(:)


! +++ parameters for atomic data
     REAL (R8),            ALLOCATABLE  :: IONIZAT(:,:)		      !atom data: ionization coefficient       (after interpolation)  [m^3/s]
     REAL (R8),            ALLOCATABLE  :: RECOMB(:,:)		      !atom data: ionization coefficient       (after interpolation)  [m^3/s]
     REAL (R8),            ALLOCATABLE  :: POTENTIAL(:,:)             !atom data: potential on jonisation       [eV]
     REAL (R8),            ALLOCATABLE  :: CHARGEEXCH(:,:)            !atom data: charge-exchange rate coefficient [m^3/s]
     REAL (R8),            ALLOCATABLE  :: SI_EXP(:,:)
     REAL (R8),            ALLOCATABLE  :: SSI_EXP(:,:)
     REAL (R8),            ALLOCATABLE  :: QI_EXP(:,:)
     REAL (R8),            ALLOCATABLE  :: SZ_EXP(:,:,:)
     REAL (R8),            ALLOCATABLE  :: QZ_EXP(:,:,:)
     REAL (R8),            ALLOCATABLE  :: QE_EXP(:)
     REAL (R8),            ALLOCATABLE  :: QE_EXP_TOT(:)

     REAL (R8)                          :: TIME                       !Time      

     LOGICAL, SAVE                      :: first = .TRUE.
     INTEGER                            :: IFAIL                  

     
#ifdef GOT_AMNSPROTO
     TYPE (amns_handle_type),                 SAVE :: amns
     TYPE (amns_handle_rx_type), ALLOCATABLE, SAVE :: amns_ei(:,:), amns_eip(:,:), amns_rc(:,:)
     TYPE (amns_handle_rx_type), ALLOCATABLE, SAVE :: amns_cx(:,:), amns_lr(:,:),  amns_br(:,:)
     TYPE (amns_version_type)                      :: amns_database
     TYPE (amns_reaction_type)                     :: ei_rx, eip_rx, rc_rx, cx_rx, lr_rx, br_rx
     TYPE (amns_reactants_type)                    :: species
     TYPE (amns_query_type)                        :: query
     TYPE (amns_answer_type)                       :: answer
     TYPE (amns_set_type)                          :: set
     REAL (R8)                                     :: ZN_neut, MI_neut
#endif    
     CHARACTER (len=80)                            :: FORMAT
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +


! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
! +++ Set dimensions:
    NEQ                        = SIZE  (EQUILIBRIUM_ITER(1)%profiles_1d%rho_tor )
    NRHO                       = SIZE  (CORENEUTRALS_ITER(1)%rho_tor            )
    CALL GET_COMP_DIMENSIONS           (CORENEUTRALS_ITER(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
    NDIM                       = 1


!   Maximum charge state of Ions and Impurities:
!    ZMAX                       = 0
!    DO IION = 1,NION
!       ZMAX                    = MAX(INT(CORENEUTRALS_ITER(1)%COMPOSITIONS%IONS(IION)%ZION),ZMAX)
!    END DO
!    ZMAX                       = MAX(MAXVAL(NZIMP),ZMAX)
    TMAX                       = MAXVAL(NTYPE)

    ZMAX = 1

 


! +++ Allocate output CPOs:
      CALL ALLOCATE_CORENEUTRALS_CPO   (NOCUR, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORENEUTRALS_NEW)
      CALL COPY_CPO                    (CORENEUTRALS_ITER(1),              CORENEUTRALS_NEW(1))
      CALL ALLOCATE_CORESOURCE_CPO     (NOCUR, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORESOURCE_NEW)
      CALL DEALLOCATE_CPO              (CORESOURCE_NEW(1)%COMPOSITIONS)
      CALL COPY_CPO                    (CORENEUTRALS_ITER(1)%COMPOSITIONS, CORESOURCE_NEW(1)%COMPOSITIONS)




! +++ Copy time and grids to output CPOs:
     CORENEUTRALS_NEW(1)%datainfo%cocos         = 13
     CORENEUTRALS_NEW(1)%time                   = COREPROF_ITER(1)%time
     CORENEUTRALS_NEW(1)%rho_tor                = COREPROF_ITER(1)%rho_tor
     CORENEUTRALS_NEW(1)%rho_tor_norm           = COREPROF_ITER(1)%rho_tor_norm

     CORESOURCE_NEW(1)%datainfo%cocos           = 13
     CORESOURCE_NEW(1)%time                     = COREPROF_ITER(1)%time
     CORESOURCE_NEW(1)%VALUES(1)%rho_tor        = COREPROF_ITER(1)%rho_tor
     CORESOURCE_NEW(1)%VALUES(1)%rho_tor_norm   = COREPROF_ITER(1)%rho_tor_norm
	 




! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
! +++ Allocate types for interface with numerical solver:
      CALL  ALLOCATE_NUMERICS          (NDIM, NRHO, SOLVER, ifail)


! +++ Allocate internal variables:
      ALLOCATE (        RHO(NRHO)             )
      ALLOCATE (        VPR(NRHO)             )
      ALLOCATE (       VPRM(NRHO)             )
      ALLOCATE (         G3(NRHO)             )
      ALLOCATE (         NE(NRHO)             )
      ALLOCATE (         TE(NRHO)             )
      ALLOCATE (         TI(NRHO)             )
      ALLOCATE (         NI(NRHO,NNEUT)       )

      ALLOCATE (     QE_EXP(NRHO)             )
      ALLOCATE (     QE_EXP_TOT(NRHO)         )
      ALLOCATE (     SI_EXP(NRHO,NION)        )
      ALLOCATE (     QI_EXP(NRHO,NION)        )
      ALLOCATE (     SZ_EXP(NRHO,NIMP,ZMAX)   )
      ALLOCATE (     QZ_EXP(NRHO,NIMP,ZMAX)   )
      ALLOCATE (    SSI_EXP(NRHO,NION)        )	

      ALLOCATE (          Y(NRHO)             )
      ALLOCATE (         YM(NRHO)             )
      ALLOCATE (         DY(NRHO)             )
      ALLOCATE (          A(NRHO)             )
      ALLOCATE (          B(NRHO)             )
      ALLOCATE (          C(NRHO)             )
      ALLOCATE (          D(NRHO)             )
      ALLOCATE (          E(NRHO)             )
      ALLOCATE (          F(NRHO)             )
      ALLOCATE (          G(NRHO)             )

      ALLOCATE (        FUN(NRHO)             )
      ALLOCATE (     INTFUN(NRHO)             )
	 
      ALLOCATE (      ANEUT(NNEUT)            )

      ALLOCATE (         N0(NRHO,NNEUT,TMAX)  )
      ALLOCATE (        N0M(NRHO,NNEUT,TMAX)  )
      ALLOCATE (      VCONV(NRHO,NNEUT,TMAX)  )
      ALLOCATE (       DIFF(NRHO,NNEUT,TMAX)  )
      ALLOCATE (        DN0(NRHO,NNEUT,TMAX)  )
      ALLOCATE (       FLUX(NRHO,NNEUT,TMAX)  )
      ALLOCATE (     NN_BND(3,   NNEUT,TMAX)  )
      ALLOCATE (NN_BND_TYPE(     NNEUT,TMAX)  )
      ALLOCATE (         T0(NRHO,NNEUT,TMAX)  )
      ALLOCATE (         VT(NRHO,NNEUT,TMAX)  )
      ALLOCATE (         VP(NRHO,NNEUT,TMAX)  )
      ALLOCATE (         VR(NRHO,NNEUT,TMAX)  )
      ALLOCATE (   NNSOURCE(NRHO,NNEUT,TMAX)  )

      ALLOCATE (    IONIZAT(NRHO,0:ZMAX)      )
      ALLOCATE (     RECOMB(NRHO,0:ZMAX)      )
      ALLOCATE (  POTENTIAL(NRHO,0:ZMAX)      )
      ALLOCATE ( CHARGEEXCH(NRHO,0:ZMAX)      )
			



! +++ Fill internal variables:
      SI_EXP               = 0._R8
      SSI_EXP              = 0._R8
      SZ_EXP               = 0._R8
      QI_EXP               = 0._R8
      QE_EXP               = 0._R8
      QE_EXP_tot           = 0._R8

      N0                   = 0._R8
      N0M                  = 0._R8 
      DN0                  = 0._R8 
      VCONV                = 0._R8 
      NN_BND               = 0._R8
      NN_BND_TYPE          = 0
      T0                   = 0._R8
      VT                   = 0._R8
      VR                   = 0._R8
      VP                   = 0._R8
      NNSOURCE             = 0._R8
      DIFF                 = 0._R8

      RHO                  = COREPROF_ITER(1)%RHO_TOR
      NE                   = COREPROF_ITER(1)%NE%VALUE
      TE                   = COREPROF_ITER(1)%TE%VALUE
      TI(:)                = COREPROF_ITER(1)%TI%VALUE(:,1)

      AMIX                 = CONTROL_DOUBLE(2)
      TAU                  = CONTROL_DOUBLE(1)           
      SOLVER_TYPE          = CONTROL_INTEGER(1) 


      CALL L3deriv          (EQUILIBRIUM_ITER(1)%profiles_1d%volume, EQUILIBRIUM_ITER(1)%profiles_1d%rho_tor, NEQ,  &
                             VPR,                                    RHO,                                     NRHO)

      CALL L3deriv          (EQUILIBRIUM_ITER(1)%profiles_1d%volume, EQUILIBRIUM_ITER(1)%profiles_1d%rho_tor, NEQ,  &
                             VPRM,                                   RHO,                                     NRHO)
		  
      CALL L3interp         (EQUILIBRIUM_ITER(1)%profiles_1d%gm3,    EQUILIBRIUM_ITER(1)%profiles_1d%rho_tor, NEQ,  &
                             G3,                                     RHO,                                     NRHO)

      DO INEUT=1, NNEUT
         INUCL             = CORENEUTRALS_OLD(1)%compositions%NEUTRALSCOMP(INEUT)%NEUTCOMP(1)%nucindex
         ANEUT(INEUT)      = CORENEUTRALS_OLD(1)%compositions%nuclei(INUCL)%amn

         CORENEUTRALS_ITER(1)%PROFILES(INEUT)%NEUTRALTYPE(2)%t0%value(:) = COREPROF_ITER(1)%TI%VALUE(:,1)
         CORENEUTRALS_ITER(1)%PROFILES(INEUT)%NEUTRALTYPE(1)%t0%value(:) = CORENEUTRALS_ITER(1)%PROFILES(INEUT)%NEUTRALTYPE(1)%t0%boundary%value(1)

         DO ITYPE= 1, NTYPE(INEUT)    
           CALL L3interp (CORENEUTRALS_ITER(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%n0%value,          CORENEUTRALS_ITER(1)%rho_tor, SIZE(CORENEUTRALS_ITER(1)%rho_tor),  &
                          N0(:,INEUT,ITYPE),                                                         RHO,                          NRHO)
           CALL L3interp (CORENEUTRALS_OLD(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%n0%value,           CORENEUTRALS_ITER(1)%rho_tor, SIZE(CORENEUTRALS_ITER(1)%rho_tor),  &
                          N0M(:,INEUT,ITYPE),                                                        RHO,                          NRHO)
           CALL L3interp (CORENEUTRALS_ITER(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%t0%value,          CORENEUTRALS_ITER(1)%rho_tor, SIZE(CORENEUTRALS_ITER(1)%rho_tor),  &
                          T0(:,INEUT,ITYPE),                                                         RHO,                          NRHO)
           CALL L3interp (CORENEUTRALS_ITER(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%v0%toroidal%value, CORENEUTRALS_ITER(1)%rho_tor, SIZE(CORENEUTRALS_ITER(1)%rho_tor),  &
                          VT(:,INEUT,ITYPE),                                                         RHO,                          NRHO)
           CALL L3interp (CORENEUTRALS_ITER(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%v0%poloidal%value, CORENEUTRALS_ITER(1)%rho_tor, SIZE(CORENEUTRALS_ITER(1)%rho_tor),  &
                          VP(:,INEUT,ITYPE),                                                         RHO,                          NRHO)
           CALL L3interp (CORENEUTRALS_ITER(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%v0%radial%value,   CORENEUTRALS_ITER(1)%rho_tor, SIZE(CORENEUTRALS_ITER(1)%rho_tor),  &
                          VR(:,INEUT,ITYPE),                                                         RHO,                          NRHO)

           NN_BND(:,INEUT,ITYPE)      = CORENEUTRALS_ITER(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%n0%boundary%value(:)
           NN_BND_TYPE(INEUT,ITYPE)   = CORENEUTRALS_ITER(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%n0%boundary%type
         END DO


         DO IION=1,NION
           IF(ABS(ANEUT(INEUT) - COREPROF_ITER(1)%compositions%nuclei(COREPROF_ITER(1)%compositions%IONS(IION)%nucindex)%amn).LE. 0.25) THEN
              NI(:,INEUT)                       = COREPROF_ITER(1)%NI%VALUE(:,IION)
           ENDIF
         ENDDO	
	
         DO IIMP=1,NIMP
           IF(ABS(ANEUT(INEUT) - COREIMPUR_ITER(1)%compositions%nuclei(COREIMPUR_ITER(1)%compositions%IMPURITIES(IIMP)%nucindex)%amn).LE. 0.25) THEN
              CALL L3interp (COREIMPUR_ITER(1)%IMPURITY(IIMP)%NZ(:,1),   COREIMPUR_ITER(1)%rho_tor,   SIZE(COREIMPUR_ITER(1)%rho_tor),  &
                             NI(:,INEUT),                                RHO,                         NRHO)
           ENDIF		   
         END DO
      END DO





! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
! +++ Build atomic data:
      IF(first) THEN
#ifdef GOT_AMNSPROTO
       WRITE(*,*)      'ITM AMNSPROTO data used (via UAL)'
       ALLOCATE(amns_ei(0:ZMAX, NNEUT),  amns_rc(0:ZMAX, NNEUT),   &
                amns_eip(0:ZMAX,NNEUT),  amns_lr(0:ZMAX, NNEUT),   & 
		amns_br(0:ZMAX, NNEUT),  amns_cx(0:ZMAX, NNEUT))
       CALL ITM_AMNS_SETUP(amns)
       query%string  = 'version'
       CALL ITM_AMNS_QUERY(amns,query,answer)
       WRITE(*,*)      'AMNS data base version = ',TRIM(answer%string)
       ei_rx%string  = 'EI'
       eip_rx%string = 'EIP'
       rc_rx%string  = 'RC'
       lr_rx%string  = 'LR'
       br_rx%string  = 'BR'
       cx_rx%string  = 'CX'
       FORMAT        = '(''ZN = '',f5.2,'', IS = '',i2,'', RX = '',a,'', SRC = '',a)'
       query%string  = 'source'

       DO INEUT=1, NNEUT

          INUCL                       = CORENEUTRALS_OLD(1)%compositions%NEUTRALSCOMP(INEUT)%NEUTCOMP(1)%nucindex
          ZN_neut                     = CORENEUTRALS_OLD(1)%compositions%nuclei(INUCL)%zn
          MI_neut                     = CORENEUTRALS_OLD(1)%compositions%nuclei(INUCL)%amn
          MI_neut                     = 0

          DO IZ = 0, ZMAX-1
            IF (ZN_neut .GE. IZ) THEN
! EI
               allocate(species%components(4))
               species%components =  &
                    (/ amns_reactant_type(ZN_neut, iz, MI_neut, 0), &
                       amns_reactant_type(0, -1, 0, 0), &
                       amns_reactant_type(ZN_neut, iz+1, MI_neut, 1), &
                       amns_reactant_type(0, -1, 0, 1) &
                    /)
              CALL ITM_AMNS_SETUP_TABLE  (amns, ei_rx, species,   amns_ei(IZ, INEUT))
              deallocate(species%components)
            END IF
          END DO
          

          DO IZ = 0, ZMAX
            IF (ZN_neut .GE. IZ) THEN
! EIP
               allocate(species%components(2))
               species%components =  &
                    (/ amns_reactant_type(ZN_neut, iz, MI_neut, 0), &
                       amns_reactant_type(ZN_neut, iz, MI_neut, 1) &
                    /)
	      CALL ITM_AMNS_SETUP_TABLE  (amns, eip_rx, species,  amns_eip(IZ, INEUT))
              deallocate(species%components)

            END IF
          ENDDO

          DO IZ = 1, ZMAX
            IF (ZN_neut .GE. IZ) THEN
! CX
               allocate(species%components(4))
               species%components = &
                    (/ amns_reactant_type(ZN_neut, iz, MI_neut, 0), &
                       amns_reactant_type(1, 0, 0, 0), &
                       amns_reactant_type(ZN_neut, iz-1, MI_neut, 1), &
                       amns_reactant_type(1, 1, 0, 1) &
                    /)
	      CALL ITM_AMNS_SETUP_TABLE  (amns, cx_rx, species,   amns_cx(IZ, INEUT))
              deallocate(species%components)
! RC
               allocate(species%components(4))
               species%components =  &
                    (/ amns_reactant_type(ZN_neut, iz, MI_neut, 0), &
                       amns_reactant_type(0, -1, 0, 0), &
                       amns_reactant_type(ZN_neut, iz-1, MI_neut, 1), &
                       amns_reactant_type(0, -1, 0, 1) &
                    /)
              CALL ITM_AMNS_SETUP_TABLE  (amns, rc_rx, species,   amns_rc(IZ, INEUT))
              deallocate(species%components)
            END IF
          ENDDO

       END DO
#endif    
          first=.FALSE.
       ENDIF



        

 
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
! +++ Calculate neutrals:
      NEUTRAL_TYPE_LOOP1: DO INEUT=1,NNEUT

 
			  
    



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
! + + + + start calculation for neutrals  + + + + + + + + 

      POTENTIAL              = 0.0_R8
      IONIZAT                = 0.0_R8
      RECOMB                 = 0.0_R8
      CHARGEEXCH             = 0.0_R8
	
	
#ifdef GOT_AMNSPROTO
      DO IZ=0,ZMAX-1
         CALL ITM_AMNS_RX    (amns_ei(IZ,INEUT),  IONIZAT(:,IZ),      TE, NE)
      END DO
      DO IZ=1,ZMAX
         CALL ITM_AMNS_RX    (amns_eip(IZ,INEUT), POTENTIAL(:,IZ),    TE, NE)
         CALL ITM_AMNS_RX    (amns_rc(IZ,INEUT),  RECOMB(:,IZ),       TE, NE)
         CALL ITM_AMNS_RX    (amns_cx(IZ,INEUT),  CHARGEEXCH(:,IZ),   TE, NE)
      END DO
#endif 
 

 



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!    solution of particle transport equation for        +
!    individual state of neutrals for low energy        +
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + 
      COLD_NEUTRALS       = 0
      IF(ASSOCIATED(CORENEUTRALS_OLD(1)%compositions%neutralscomp(INEUT)%type))THEN
         DO ITYPE = 1, NTYPE(INEUT)
            IF (CORENEUTRALS_OLD(1)%compositions%neutralscomp(INEUT)%type(ITYPE)%flag .EQ. 0) &
                COLD_NEUTRALS = 1
         END DO
      END IF

      IF (COLD_NEUTRALS .EQ. 0) GOTO 123



! ***************** ITYPE = 1, cold neutrals
               
      ITYPE =  1    
	       
	       
      DO IRHO =1 , NRHO
	 NNSOURCE(IRHO,INEUT,ITYPE) = 0.0
         DIFF(IRHO,INEUT,ITYPE) =   (9.56D7*T0(IRHO,INEUT,ITYPE)/ANEUT(INEUT))                           &
                                  / (3.0*(NE(IRHO)*IONIZAT(IRHO,0)+CHARGEEXCH(IRHO,1)*NI(IRHO,INEUT))) 
      END DO
   
! +++ Set equation t0 'predictive' and all coefficients to zero:
      FLAG         = 1
      Y(:)         = 0.0D0
      DY(:)        = 0.0D0
      YM(:)        = 0.0D0
      A(:)         = 0.0D0
      B(:)         = 0.0D0
      C(:)         = 0.0D0
      D(:)         = 0.0D0
      E(:)         = 0.0D0
      F(:)         = 0.0D0
      G(:)         = 0.0D0
      H            = 0.0D0
      V(:)         = 0.0D0
      U(:)         = 0.0D0
      W(:)         = 0.0D0


! +++ Coefficients for ion diffusion equation  in form:
!
!     (A*Y-B*Y(t-1))/H + 1/C * (-D*Y' + E*Y) = F - G*Y

      DO IRHO=1,NRHO
         Y(IRHO)   = N0(IRHO,INEUT,ITYPE)
	 YM(IRHO)  = N0M(IRHO,INEUT,ITYPE)
         A(IRHO)   = VPR(IRHO)
         B(IRHO)   = VPRM(IRHO) 
         C(IRHO)   = 1.D0
         D(IRHO)   = VPR(IRHO)*G3(IRHO)*DIFF(IRHO,INEUT,ITYPE)
         E(IRHO)   = VPR(IRHO)*G3(IRHO)*VCONV(IRHO,INEUT,ITYPE)       
         F(IRHO)   = VPR(IRHO)*NNSOURCE(IRHO,INEUT,ITYPE)				
         G(IRHO)   = VPR(IRHO)*(NE(IRHO)*IONIZAT(IRHO,0)+NI(IRHO,INEUT)*CHARGEEXCH(IRHO,1))    
      END DO 
      H            = TAU



! +++ Boundary conditions for ion diffusion equation in form:
!
!     V*Y' + U*Y =W 
!
! +++ On axis:
!     dNN/drho(rho=0)=0:
      V(1)         = 1.D0
      U(1)         = 0.D0
      W(1)         = 0.D0

! +++ At the edge:
!     FIXED NN
      IF(NN_BND_TYPE(INEUT,ITYPE).EQ.1) THEN
        V(2)       = 0.D0
        U(2)       = 1.D0
        W(2)       = NN_BND(1,INEUT,ITYPE)
      ENDIF
 

!     FIXED grad_NN
      IF(NN_BND_TYPE(INEUT,ITYPE).EQ.2) THEN
        V(2)       = 1.D0
        U(2)       = 0.D0
        W(2)       = NN_BND(1,INEUT,ITYPE)
      ENDIF

!     FIXED L_NN
      IF(NN_BND_TYPE(INEUT,ITYPE).EQ.3) THEN
        V(2)       = NN_BND(1,INEUT,ITYPE)
        U(2)       = 1.D0
        W(2)       = 0.D0
      ENDIF

!     FIXED Flux_NN
      IF(NN_BND_TYPE(INEUT,ITYPE).EQ.4) THEN
        V(2)       = -G3(NRHO)*DIFF(NRHO,INEUT,ITYPE)*VPR(NRHO)
        U(2)       = G3(NRHO)*VCONV(NRHO,INEUT,ITYPE)*VPR(NRHO)
        W(2)       = NN_BND(1,INEUT,ITYPE)
      ENDIF
 

!     Generic boundary condition
      IF(NN_BND_TYPE(INEUT,ITYPE).EQ.5) THEN
        V(2)       = NN_BND(1,INEUT,ITYPE)
        U(2)       = NN_BND(2,INEUT,ITYPE)
        W(2)       = NN_BND(3,INEUT,ITYPE)
      ENDIF



! +++ Density equation is not solved:
      IF (NN_BND_TYPE(INEUT,ITYPE).EQ.0) THEN
         CALL L3deriv (Y, RHO, NRHO,    DY, RHO, NRHO)

         FLAG       = 0

         RHO_LOOP4: DO IRHO=1,NRHO
            A(IRHO) = 1.0D0
            B(IRHO) = 1.0D0
            C(IRHO) = 1.0D0
            D(IRHO) = 0.0D0
            E(IRHO) = 0.0D0
            F(IRHO) = 0.0D0
            G(IRHO) = 0.0D0  
				  
         END DO RHO_LOOP4

         V(2)       = 0.0D0
         U(2)       = 1.0D0
         W(2)       = Y(NRHO)
      END IF


! +++ Defining coefficients for numerical solver:    
      SOLVER%TYPE                   = SOLVER_TYPE
      SOLVER%EQ_FLAG(NDIM)          = FLAG
      SOLVER%NDIM                   = NDIM
      SOLVER%NRHO                   = NRHO
      SOLVER%AMIX                   = AMIX

 
      RHO_LOOP5: DO IRHO=1,NRHO

	SOLVER%RHO(IRHO)            = RHO(IRHO)
	SOLVER%Y(NDIM,IRHO)         = Y(IRHO)
	SOLVER%DY(NDIM,IRHO)        = DY(IRHO)
	SOLVER%YM(NDIM,IRHO)        = YM(IRHO)
	SOLVER%A(NDIM,IRHO)         = A(IRHO)
	SOLVER%B(NDIM,IRHO)         = B(IRHO) 
	SOLVER%C(NDIM,IRHO)         = C(IRHO)
	SOLVER%D(NDIM,IRHO)         = D(IRHO)
	SOLVER%E(NDIM,IRHO)         = E(IRHO)
	SOLVER%F(NDIM,IRHO)         = F(IRHO)
	SOLVER%G(NDIM,IRHO)         = G(IRHO)

      END DO RHO_LOOP5

      SOLVER%H                      = H

      SOLVER%V(NDIM,1)              = V(1)
      SOLVER%U(NDIM,1)              = U(1)
      SOLVER%W(NDIM,1)              = W(1)
      SOLVER%V(NDIM,2)              = V(2)
      SOLVER%U(NDIM,2)              = U(2)
      SOLVER%W(NDIM,2)              = W(2)


! +++ Solution of density diffusion equation:            
        CALL SOLUTION_INTERFACE (SOLVER, ifail)
     

! +++ New neutrals density:  
      RHO_LOOP6: DO IRHO=1,NRHO
	 Y(IRHO)                     = SOLVER%Y(NDIM,IRHO)
	 DY(IRHO)                    = SOLVER%DY(NDIM,IRHO)

      END DO RHO_LOOP6


! +++ New profiles of neutrals density flux and integral source:                
      RHO_LOOP7: DO IRHO=1,NRHO
		
	 N0M(IRHO,INEUT,ITYPE)    = N0(IRHO,INEUT,ITYPE)

	 N0(IRHO,INEUT,ITYPE)     = Y(IRHO)
         DN0(IRHO,INEUT,ITYPE)    = DY(IRHO)    
         FUN(IRHO)                = 1.D0/RHO(IRHO)*(VPR(IRHO)*NNSOURCE(IRHO,INEUT,ITYPE)            &
			               + VPRM(IRHO)*N0M(IRHO,INEUT,ITYPE)/TAU                       &
			               - N0(IRHO,INEUT,ITYPE)*VPR(IRHO)*(1.D0/TAU))    
	 FLUX(IRHO,INEUT,ITYPE)   = VPR(IRHO)*G3(IRHO)*                                             &
                                       ( Y(IRHO)*VCONV(IRHO,INEUT,ITYPE) - DY(IRHO)*DIFF(IRHO,INEUT,ITYPE))
	   
      END DO RHO_LOOP7
	  


 123  CONTINUE


      HOT_NEUTRALS           = 0
      IF(ASSOCIATED(CORENEUTRALS_OLD(1)%compositions%neutralscomp(INEUT)%type))THEN
         DO ITYPE = 1, NTYPE(INEUT)
            IF (CORENEUTRALS_OLD(1)%compositions%neutralscomp(INEUT)%type(ITYPE)%flag .EQ. 1) &
                HOT_NEUTRALS = 1
         END DO
      END IF

      IF (HOT_NEUTRALS .EQ. 0) GOTO 124

!********************* ITYPE = 2, hot neutrals
      ITYPE=2      
       
      DO IRHO=1,NRHO
         NNSOURCE(IRHO,INEUT,ITYPE)=0.0
      END DO

      DO IRHO=1,NRHO
         DIFF(IRHO,INEUT,ITYPE)=(9.56D7*T0(IRHO,INEUT,ITYPE)/ANEUT(INEUT))/(3.0*(ne(IRHO)*IONIZAT(IRHO,0)+CHARGEEXCH(IRHO,1)*ni(IRHO,INEUT)))
      END DO
    
! +++ Set equation t0 'predictive' and all coefficients to zero:
      FLAG         = 1
      Y(:)         = 0.0D0
      DY(:)        = 0.0D0
      YM(:)        = 0.0D0
      A(:)         = 0.0D0
      B(:)         = 0.0D0
      C(:)         = 0.0D0
      D(:)         = 0.0D0
      E(:)         = 0.0D0
      F(:)         = 0.0D0
      G(:)         = 0.0D0
      H            = 0.0D0
      V(:)         = 0.0D0
      U(:)         = 0.0D0
      W(:)         = 0.0D0


! +++ Coefficients for ion diffusion equation  in form:
!
!     (A*Y-B*Y(t-1))/H + 1/C * (-D*Y' + E*Y) = F - G*Y

      RHO_LOOP8: DO IRHO=1,NRHO
	Y(IRHO)    = N0(IRHO,INEUT,ITYPE)
	YM(IRHO)   = N0M(IRHO,INEUT,ITYPE)
        A(IRHO)    = VPR(IRHO)
        B(IRHO)    = VPRM(IRHO) 
        C(IRHO)    = 1.D0
        D(IRHO)    = VPR(IRHO)*G3(IRHO)*DIFF(IRHO,INEUT,ITYPE)
        E(IRHO)    = VPR(IRHO)*G3(IRHO)*VCONV(IRHO,INEUT,ITYPE)       
        F(IRHO)    = VPR(IRHO)*NNSOURCE(IRHO,INEUT,ITYPE)+                             &			  
                     VPR(IRHO)*NI(IRHO,INEUT)*CHARGEEXCH(IRHO,1)*N0(IRHO,INEUT,1)+ &
                     VPR(IRHO)*NI(IRHO,INEUT)*NE(IRHO)*RECOMB(IRHO,1)
	G(IRHO)    = VPR(IRHO)*(Ni(IRHO,INEUT)*CHARGEEXCH(IRHO,1)+NE(IRHO)*IONIZAT(IRHO,0)) 
 
      END DO RHO_LOOP8
 
      H            = TAU


! +++ Boundary conditions for ion diffusion equation in form:
!
!     V*Y' + U*Y =W 
!
! +++ On axis:
!     dNN/drho(rho=0)=0:
      V(1)         = 1.D0
      U(1)         = 0.D0
      W(1)         = 0.D0

! +++ At the edge:
!       FIXED NN
      IF(NN_BND_TYPE(INEUT,ITYPE).EQ.1) THEN
        V(2)       = 0.D0
        U(2)       = 1.D0
        W(2)       = NN_BND(1,INEUT,ITYPE)
      ENDIF

!     FIXED grad_NN
      IF(NN_BND_TYPE(INEUT,ITYPE).EQ.2) THEN
        V(2)       = 1.D0
        U(2)       = 0.D0
        W(2)       = NN_BND(1,INEUT,ITYPE)
      ENDIF

!     FIXED L_NN
      IF(NN_BND_TYPE(INEUT,ITYPE).EQ.3) THEN
        V(2)       = NN_BND(1,INEUT,ITYPE)
        U(2)       = 1.D0
        W(2)       = 0.D0
      ENDIF

!     FIXED Flux_NN
      IF(NN_BND_TYPE(INEUT,ITYPE).EQ.4) THEN
        V(2)       = -G3(NRHO)*DIFF(NRHO,INEUT,ITYPE)*VPR(NRHO)
        U(2)       = G3(NRHO)*VCONV(NRHO,INEUT,ITYPE)*VPR(NRHO)
        W(2)       = NN_BND(1,INEUT,ITYPE)
      ENDIF
 

!     Generic boundary condition
      IF(NN_BND_TYPE(INEUT,ITYPE).EQ.5) THEN
        V(2)       = NN_BND(1,INEUT,ITYPE)
        U(2)       = NN_BND(2,INEUT,ITYPE)
        W(2)       = NN_BND(3,INEUT,ITYPE)
      ENDIF



! +++ Density equation is not solved:
      IF(NN_BND_TYPE(INEUT,ITYPE).EQ.0) THEN

        CALL L3deriv (Y, RHO, NRHO,    DY, RHO, NRHO)

        FLAG       = 0

        RHO_LOOP9: DO IRHO=1,NRHO

	   A(IRHO) = 1.0D0
	   B(IRHO) = 1.0D0
	   C(IRHO) = 1.0D0
	   D(IRHO) = 0.0D0
	   E(IRHO) = 0.0D0
           F(IRHO) = 0.0D0
	   G(IRHO) = 0.0D0  
				  
        END DO RHO_LOOP9

        V(2)       = 0.0D0
        U(2)       = 1.0D0
        W(2)       = Y(NRHO)
      END IF


! +++ Defining coefficients for numerical solver:    
      SOLVER%TYPE                   = SOLVER_TYPE
      SOLVER%EQ_FLAG(NDIM)          = FLAG
      SOLVER%NDIM                   = NDIM
      SOLVER%NRHO                   = NRHO
      SOLVER%AMIX                   = AMIX

 
      RHO_LOOP10: DO IRHO=1,NRHO

         SOLVER%RHO(IRHO)           = RHO(IRHO)
         SOLVER%Y(NDIM,IRHO)        = Y(IRHO)
         SOLVER%DY(NDIM,IRHO)       = DY(IRHO)
         SOLVER%YM(NDIM,IRHO)       = YM(IRHO)
         SOLVER%A(NDIM,IRHO)        = A(IRHO)
         SOLVER%B(NDIM,IRHO)        = B(IRHO) 
         SOLVER%C(NDIM,IRHO)        = C(IRHO)
         SOLVER%D(NDIM,IRHO)        = D(IRHO)
         SOLVER%E(NDIM,IRHO)        = E(IRHO)
         SOLVER%F(NDIM,IRHO)        = F(IRHO)
         SOLVER%G(NDIM,IRHO)        = G(IRHO)

      END DO RHO_LOOP10

      SOLVER%H                      = H

      SOLVER%V(NDIM,1)              = V(1)
      SOLVER%U(NDIM,1)              = U(1)
      SOLVER%W(NDIM,1)              = W(1)
      SOLVER%V(NDIM,2)              = V(2)
      SOLVER%U(NDIM,2)              = U(2)
      SOLVER%W(NDIM,2)              = W(2)


! +++ Solution of density diffusion equation:            
      CALL SOLUTION_INTERFACE (SOLVER, ifail)
     

! +++ New impurity density:  
      RHO_LOOP11: DO IRHO=1,NRHO

	 Y(IRHO)                    = SOLVER%Y(NDIM,IRHO)
	 DY(IRHO)                   = SOLVER%DY(NDIM,IRHO)

      END DO RHO_LOOP11


! +++ New profiles of impurity density flux and integral source:                
      RHO_LOOP12: DO IRHO=1,NRHO
		
	 N0M(IRHO,INEUT,ITYPE)   = N0(IRHO,INEUT,ITYPE)
	 N0(IRHO,INEUT,ITYPE)    = Y(IRHO)                                        
         DN0(IRHO,INEUT,ITYPE)   = DY(IRHO)    
         FUN(IRHO)                = 1.D0/RHO(IRHO)*(VPR(IRHO)*NNSOURCE(IRHO,INEUT,ITYPE)            &
				      + VPRM(IRHO)*N0M(IRHO,INEUT,ITYPE)/TAU                       &
				      - N0(IRHO,INEUT,ITYPE)*VPR(IRHO)*(1.D0/TAU))    
         FLUX(IRHO,INEUT,ITYPE)   = VPR(IRHO)*G3(IRHO)*                                             &
                                      ( Y(IRHO)*VCONV(IRHO,INEUT,ITYPE) - DY(IRHO)*DIFF(IRHO,INEUT,ITYPE))

      END DO RHO_LOOP12

      DO IRHO=1,NRHO


	 DO IION=1,NION
	    IF(ABS(ANEUT(INEUT)-COREPROF_ITER(1)%compositions%nuclei(COREPROF_ITER(1)%compositions%IONS(IION)%nucindex)%amn).LE.0.25) THEN
	       DO ITYPE=1,NTYPE(INEUT)
	          SI_EXP(IRHO,IION)  = SI_EXP(IRHO,IION)+NE(IRHO)*(N0(IRHO,INEUT,ITYPE)*IONIZAT(IRHO,0)-RECOMB(IRHO,1)*NI(IRHO,INEUT))
               END DO
	    ENDIF
          END DO

	  IF (ASSOCIATED(CORESOURCE_NEW(1)%VALUES(1)%sz)) THEN
	
          DO  IIMP=1,NIMP
	     IF(ABS(ANEUT(INEUT)-COREIMPUR_ITER(1)%compositions%nuclei(COREIMPUR_ITER(1)%compositions%IMPURITIES(IIMP)%nucindex)%amn).LE.0.25) THEN
	        DO ITYPE=1,NTYPE(INEUT)
		   SZ_EXP(IRHO,IIMP,1) = SZ_EXP(IRHO,IIMP,1)+NE(IRHO)*(N0(IRHO,INEUT,ITYPE)*IONIZAT(IRHO,0)-RECOMB(IRHO,1)*NI(IRHO,INEUT))
                END DO
	     ENDIF
          END DO

	      ENDIF

       END DO


! +++ Energy source/sink caused by neutrals:
       DO IRHO=1,NRHO

 
          DO IION=1,NION
	     IF(ABS(ANEUT(INEUT)-COREPROF_ITER(1)%compositions%nuclei(COREPROF_ITER(1)%compositions%IONS(IION)%nucindex)%amn).LE.0.25) THEN
		DO ITYPE=1,NTYPE(INEUT)
		   QE_EXP(IRHO)       = QE_EXP(IRHO)      + 1.5_R8 * ITM_EV *                                                                &
                                        (- IONIZAT(IRHO,0)         *  POTENTIAL(IRHO,1)              * N0(IRHO,INEUT,ITYPE) * NE(IRHO))
             
		   QI_EXP(IRHO,IION)  = QI_EXP(IRHO,IION) + 1.5_R8 * ITM_EV *                                                                &
                                        (  CHARGEEXCH(IRHO,1)  * (T0(IRHO,INEUT,ITYPE)-TI(IRHO)) * N0(IRHO,INEUT,ITYPE) * NI(IRHO,INEUT))
                END DO
	     ENDIF

             QE_EXP_TOT(IRHO)         = QE_EXP_TOT(IRHO) + QE_EXP(IRHO)

          END DO

	  IF (ASSOCIATED(CORESOURCE_NEW(1)%VALUES(1)%sz)) THEN
 
          DO IIMP=1,NIMP
	     IF(ABS(ANEUT(INEUT)-COREIMPUR_ITER(1)%compositions%nuclei(COREIMPUR_ITER(1)%compositions%IMPURITIES(IIMP)%nucindex)%amn).LE.0.25) THEN
		DO ITYPE=1,NTYPE(INEUT)
		   QE_EXP(IRHO)       = QE_EXP(IRHO)      + 1.5_R8 * ITM_EV *                                                                &
                                        (- IONIZAT(IRHO,0)         *  POTENTIAL(IRHO,1)              * N0(IRHO,INEUT,ITYPE) * NE(IRHO))
             
		   QZ_EXP(IRHO,IIMP,1)= QZ_EXP(IRHO,IIMP,1) + 1.5_R8 * ITM_EV *                                                                &
                                        (  CHARGEEXCH(IRHO,1)  * (T0(IRHO,INEUT,ITYPE)-TI(IRHO)) * N0(IRHO,INEUT,ITYPE) * NI(IRHO,INEUT))

                 END DO
	      ENDIF

              QE_EXP_TOT(IRHO)         = QE_EXP_TOT(IRHO) + QE_EXP(IRHO)

           END DO
           END IF



 124       CONTINUE


	END DO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + 
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + 


! +++ Save output to CPOs:

        LOOP_IRHO20: DO IRHO=1,NRHO
	   DO ITYPE=1,NTYPE(INEUT)                   
	      CORENEUTRALS_NEW(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%n0%value(IRHO) = N0(IRHO,INEUT,ITYPE)
	      CORENEUTRALS_NEW(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%n0%flux(IRHO)  = FLUX(IRHO,INEUT,ITYPE)	
	   END DO
 

           DO IION=1,NION
              IF(ABS(ANEUT(INEUT)-COREPROF_ITER(1)%compositions%nuclei(COREPROF_ITER(1)%compositions%IONS(IION)%nucindex)%amn).LE.0.25) THEN
                 CORESOURCE_NEW(1)%VALUES(1)%si%exp(IRHO,IION)    = SI_EXP(IRHO,IION) 
                 CORESOURCE_NEW(1)%VALUES(1)%qi%exp(IRHO,IION)    = QI_EXP(IRHO,IION) 
                 CORESOURCE_NEW(1)%VALUES(1)%se%exp(IRHO)         = CORESOURCE_NEW(1)%VALUES(1)%se%exp(IRHO) +       &
                                                                CORESOURCE_NEW(1)%VALUES(1)%si%exp(IRHO,IION)
              END IF
           END DO


           IF (ASSOCIATED(CORESOURCE_NEW(1)%VALUES(1)%sz)) THEN
              DO IIMP=1,NIMP
                 IF(ABS(ANEUT(INEUT)-COREIMPUR_ITER(1)%compositions%nuclei(COREIMPUR_ITER(1)%compositions%IMPURITIES(IIMP)%nucindex)%amn).LE.0.25) THEN
                    CORESOURCE_NEW(1)%VALUES(1)%sz(IIMP)%exp(IRHO,1) = SZ_EXP(IRHO,IIMP,1)
                    CORESOURCE_NEW(1)%VALUES(1)%se%exp(IRHO)         = CORESOURCE_NEW(1)%VALUES(1)%se%exp(IRHO) +       &
                                                                       CORESOURCE_NEW(1)%VALUES(1)%sz(IIMP)%exp(IRHO,1) 
                 END IF
              END DO
           END IF


	   CORESOURCE_NEW(1)%VALUES(1)%qe%exp(IRHO)           = QE_EXP_TOT(IRHO) 


	END DO LOOP_IRHO20

!			

        
          
      END DO NEUTRAL_TYPE_LOOP1
!     Finished loop over neutral types
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + 
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + 



    
      ALLOCATE            (CORESOURCE_NEW(1)%VALUES(1)%sourceid%id(1))
      ALLOCATE            (CORESOURCE_NEW(1)%VALUES(1)%sourceid%description(1))
      CORESOURCE_NEW(1)%VALUES(1)%sourceid%id          = 'gaspuff'
      CORESOURCE_NEW(1)%VALUES(1)%sourceid%flag        = 21
      CORESOURCE_NEW(1)%VALUES(1)%sourceid%description = 'Gas puff'

      DEALLOCATE (    RHO     )
      DEALLOCATE (    VPR     )
      DEALLOCATE (   VPRM     )
      DEALLOCATE (     G3     )
      DEALLOCATE (     NE     )
      DEALLOCATE (     TE     )
      DEALLOCATE (     TI     )
      DEALLOCATE (     NI     )

      DEALLOCATE ( QE_EXP     )
      DEALLOCATE ( QE_EXP_TOT )
      DEALLOCATE ( SI_EXP     )
      DEALLOCATE ( QI_EXP     )
      DEALLOCATE ( SZ_EXP     )
      DEALLOCATE ( QZ_EXP     )
      DEALLOCATE (SSI_EXP     )	

      DEALLOCATE (      Y     )
      DEALLOCATE (     YM     )
      DEALLOCATE (     DY     )
      DEALLOCATE (      A     )
      DEALLOCATE (      B     )
      DEALLOCATE (      C     )
      DEALLOCATE (      D     )
      DEALLOCATE (      E     )
      DEALLOCATE (      F     )
      DEALLOCATE (      G     )

      DEALLOCATE (   FUN      )
      DEALLOCATE (INTFUN      )
      DEALLOCATE (  ANEUT     )
	 
      DEALLOCATE (        N0  )
      DEALLOCATE (       N0M  )
      DEALLOCATE (      VCONV )
      DEALLOCATE (       DIFF )
      DEALLOCATE (       DN0  )
      DEALLOCATE (       FLUX )
      DEALLOCATE (     NN_BND )
      DEALLOCATE (NN_BND_TYPE )
      DEALLOCATE (         T0 )
      DEALLOCATE (         VT )
      DEALLOCATE (         VP )
      DEALLOCATE (         VR )
      DEALLOCATE (   NNSOURCE )

      DEALLOCATE (    IONIZAT )
      DEALLOCATE (     RECOMB )
      DEALLOCATE (  POTENTIAL )
      DEALLOCATE ( CHARGEEXCH )

      WRITE (*,*) 'NEUTRALS finished <==========='  
      WRITE (*,*) ' '  

			    
      RETURN


      END SUBROUTINE NEUTRALS_ETS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + 


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
      SUBROUTINE WRITEOUTNEUTRALS   (ITIME_OUT,CORENEUTRALS)     


!     This subroutine stores the results of computations
!     into files

      USE EUITM_SCHEMAS
      USE ITM_TYPES
      IMPLICIT NONE

      TYPE (TYPE_CORENEUTRALS),   POINTER  :: CORENEUTRALS(:)   !input CPO with neutrals
   
! +++ Internal parameters:
      INTEGER                              :: NRHO                      !number of radial points     (input, determined from COREPROF CPO)
      INTEGER                              :: NNEUT                     !number of neutrals species  (input, determined from CORENEUTRALSCPO)
      INTEGER			   	   :: ITIME_OUT 
      INTEGER				   :: INEUT                     !index of impurity component, number of considered impurity components (max ionization state)
      INTEGER				   :: IRHO
      INTEGER                              :: NTYPE,ITYPE               !type part for one neutrals
      REAL (R8),              ALLOCATABLE  :: RHO(:)                    !toroidal flux coordinate,not normalise,[m]

      
      CHARACTER (33)                   FILENAME
      
 
      NRHO                   = SIZE (CORENEUTRALS(1)%RHO_TOR, DIM=1) 
      NNEUT                  = SIZE (CORENEUTRALS(1)%profiles,DIM=1)

      ALLOCATE (RHO(NRHO))
	  
      RHO		     = CORENEUTRALS(1)%RHO_TOR

      IF (NNEUT.GE.2) THEN
         WRITE(*,*)'in programe Write neutrals'
         WRITE(*,*)'CORENEUTRALS(1)%profiles(1)%neutraltype(1)%n0%value(NRHO)=',CORENEUTRALS(1)%profiles(1)%neutraltype(1)%n0%value(NRHO) 
      ENDIF

      DO INEUT=1,NNEUT
                  
         NTYPE	= SIZE(CORENEUTRALS(1)%profiles(INEUT)%neutraltype)  
             
         WRITE(*,*)'ntype=',ntype,'irena'
   
         WRITE(FILENAME,'(a,i1.1,a,i7.7,a)') 'eq_ets_data/OUTNE',INEUT,'/NEU',ITIME_OUT,'.DAT'
         OPEN (UNIT=20, FILE=FILENAME)	
	         
         DO IRHO = 1, NRHO 
                       WRITE (20,'(100(1x,e14.7))')RHO(IRHO),(CORENEUTRALS(1)%profiles(INEUT)%neutraltype(ITYPE)%n0%value(IRHO),ITYPE=1,NTYPE),&
                       CORENEUTRALS(1)%profiles(INEUT)%prad0(IRHO)
	 END DO 
		
         CLOSE (20)
           
      END DO 
   
      DEALLOCATE (RHO)


      RETURN
      END SUBROUTINE WRITEOUTNEUTRALS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


      END MODULE NEUTRALS
