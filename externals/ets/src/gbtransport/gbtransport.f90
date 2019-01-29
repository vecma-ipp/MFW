MODULE GBTRANSPORT

CONTAINS





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
SUBROUTINE GB_TRANSPORT(EQUILIBRIUM, COREPROF, CORETRANSP)

!-------------------------------------------------------!
!     This routine checks for the consistency of        !
!     profiles (psi, q, jparallel) and equilibrium      !
!                                                       !
!     information received in: EQUILIBRIUM              !
!                              COREPROF                 !
!                                                       !
!     information saved in:    CORETRANSP               !
!                              (allocated internaly)    !
!                                                       !
!     controling parameter:    CODE_PARAMETERS          !
!                                                       !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Comments:     created for V&V between ETS, JETTO  !
!                   and ASTRA                           !
!                                                       !
!-------------------------------------------------------!


      USE EUITM_SCHEMAS
      USE EUITM_ROUTINES
      USE ITM_CONSTANTS

      USE COPY_STRUCTURES
      USE DEALLOCATE_STRUCTURES

      USE ALLOCATE_DEALLOCATE

      IMPLICIT NONE


! +++ CPO derived types:
      TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM(:)   !input CPO with geometry quantities from previous time
      TYPE (TYPE_COREPROF),    POINTER  :: COREPROF(:)      !input CPO with plasma profiles   
      TYPE (TYPE_CORETRANSP),  POINTER  :: CORETRANSP(:)    !output CPO with transport coefficients   


! +++ Local parameters:
      REAL(R8)                          :: TIME
      REAL(R8),            ALLOCATABLE  :: AMJ(:)
      REAL(R8),            ALLOCATABLE  :: ZMJ(:)
      REAL(R8),            ALLOCATABLE  :: RHO(:)
      REAL(R8),            ALLOCATABLE  :: TE(:)
      REAL(R8),            ALLOCATABLE  :: NE(:)
      REAL(R8),            ALLOCATABLE  :: MU(:)
      REAL(R8)                          :: ROC
      REAL(R8)                          :: HRO
      REAL(R8)                          :: BTOR
      REAL(R8)                          :: YLP, YHAGB, HAGB, YHATL, HATL

      REAL(R8),            ALLOCATABLE  :: DIFF_TE(:)       !Output: electron heat diffusion [m^2/s]
      REAL(R8),            ALLOCATABLE  :: DIFF_TI(:,:)     !Output: ion heat diffusion [m^2/s]


! +++ Dimensions:
      INTEGER,               PARAMETER  :: NOCUR = 1             !number of CPO ocurancies in the work flow
      INTEGER                           :: NRHO, IRHO            !number of radial points     (input, determined from COREPROF CPO)
      INTEGER                           :: NEQ                   !number of radial points     (input, determined from EQUILIBRIUM CPO)
      INTEGER                           :: NNUCL, INUCL          !number of nuclei species
      INTEGER                           :: NION, IION            !number of ion species
      INTEGER                           :: NIMP,     IIMP        !number of impurity species
      INTEGER,              ALLOCATABLE :: NZIMP(:)              !number of ionization states for each impurity
      INTEGER                           :: NNEUT,    INEUT       !number of neutrals species
      INTEGER,              ALLOCATABLE :: NCOMP(:)              !number of components for each neutral
      INTEGER,              ALLOCATABLE :: NTYPE(:)              !number of types for each neutral




! +++ Set dimensions:
      NRHO                   = SIZE (COREPROF(1)%rho_tor, DIM=1)
      CALL GET_COMP_DIMENSIONS      (COREPROF(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)


 
! +++ Allocate output CPO:
      CALL ALLOCATE_CORETRANSP_CPO  (NOCUR, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORETRANSP)
      call deallocate_cpo(CORETRANSP(1)%compositions)
      CALL COPY_CPO                 (COREPROF(1)%compositions, CORETRANSP(1)%compositions)


! +++ Allocate local variables:
      ALLOCATE      (       AMJ(NION)      )
      ALLOCATE      (       ZMJ(NION)      )

      ALLOCATE      (       RHO(NRHO)      )
      ALLOCATE      (        TE(NRHO)      )
      ALLOCATE      (        NE(NRHO)      )
      ALLOCATE      (        MU(NRHO)      )

      ALLOCATE      (   DIFF_TE(NRHO)      )
      ALLOCATE      (   DIFF_TI(NRHO,NION) )
 

 
! +++ Local parameters:
      TIME     = COREPROF(1)%time

      BTOR    = COREPROF(1)%toroid_field%b0 

      RHO     = COREPROF(1)%rho_tor
    
      TE      = COREPROF(1)%te%value / 1.E3_R8                    ! Temperature is specified in [keV]
      NE      = COREPROF(1)%ne%value / 1.E19_R8                   ! Density is specified in [10^19 m^-3]

      MU      = 1.0_R8 / COREPROF(1)%profiles1d%q%value

      ROC     = RHO(NRHO)

      

      DO IION = 1, NION

         INUCL      = COREPROF(1)%COMPOSITIONS%IONS(IION)%nucindex

         AMJ(IION)  = COREPROF(1)%COMPOSITIONS%NUCLEI(INUCL)%amn
         ZMJ(IION)  = COREPROF(1)%COMPOSITIONS%IONS(IION)%zion


         DO IRHO = 1, NRHO
  
         !-------------------------------------------------------!
         ! HAGB [m#2/s]:                                         !
         !       Heat conductivity Anomalous gyroBohm            !
         !       BOHM=c*Te/(e*B)=TE/BTOR*10^3[m^2/s]             !
         !       HAGB=BOHM*(dTe/dr)*a/Te*ro/a; ro=rl_i/omega_ci  !		
         !					                 !
         !       Source:  M.Erba et al. JET-P(96)10              !
         !                I.Vojtsekhovich 01-08-96               !
         !					                 !
         !       Usage:	HE=...+0.035*HAGB*XSTEP(0.8);            !
         !                Modified by G.Pereverzev 14-JAN-97     !
         !                to include recommended numerical       !
         !                pre-factor 0.035                       !
         !					                 !
         !-------------------------------------------------------!

         IF ( IRHO.EQ.1 )  THEN
            YLP   = 0.0_R8
            YHAGB = 0.32_R8 * SQRT(AMJ(IION)) / BTOR**2 / ZMJ(IION)
         ELSE
            YLP   = YHAGB / (RHO(IRHO)-RHO(IRHO-1)) *             &
                    ABS(TE(IRHO)-TE(IRHO-1)) / (TE(IRHO)+TE(IRHO-1))
         END IF
         HAGB     = YLP*TE(IRHO) * SQRT(TE(IRHO))



        !-------------------------------------------------------!
        ! HATL [m#2/s]:	                                        !
        !       Heat conductivity Anomalous by Taroni for L-mode!
        !       BOHM=c*Te/(e*B)=TE/BTOR*10^3[m^2/s]             !
        !       HATL=A_e*BOHM*q^2*(dp/dr)*a/p;                  !
        !       A_e recommended = 2.5E-4 is included in YHATL   !
        !       Source:	M.Erba et al. JET-R(95)02               !
        !                (Pereverzev 04-AUG-95)                 !      
        !       According to M.Erba et al. JET-P(96)10          !
        !       a*(dp/dr)/p is replaced with a*(dp_e/dr)/p_e    !
        !                (Pereverzev 14-JAN-97)                 !
        !                                                       !
        !-------------------------------------------------------!
	IF( IRHO.EQ.1 )	THEN
	   YLP   = 0.0_R8
	   YHATL = 0.5_R8 * ROC / BTOR
	ELSE
	   YLP   = YHATL / (RHO(IRHO)-RHO(IRHO-1)) *              &
                   ABS(NE(IRHO)*TE(IRHO)-NE(IRHO-1)*TE(IRHO-1)) / (NE(IRHO)+NE(IRHO-1))
	END IF
	HATL     = YLP / MU(IRHO)**2


        !-------------------------------------------------------!
        ! Transport coefficients:                               !
        !       XE = HATL+HAGB+...                              !
        !       XI = 2*HATL+HAGB+...                            !
        !                                                       !
        !-------------------------------------------------------!
         DIFF_TE(IRHO)      = HATL + HAGB
         DIFF_TI(IRHO,IION) = 2.0_R8 * HATL + HAGB

         END DO   
      END DO


! +++ Save output in CPO:
!
! Only electron and ion heat diffusion is updated

      CORETRANSP(1)%time                            =  TIME                 !time    [s]

      CORETRANSP(1)%VALUES(1)%rho_tor               =  RHO                  !rho     [m]

      CORETRANSP(1)%VALUES(1)%sigma                 =  0.E0_R8

      CORETRANSP(1)%VALUES(1)%te_transp%diff_eff    =  DIFF_TE
      CORETRANSP(1)%VALUES(1)%te_transp%vconv_eff   =  0.E0_R8

      CORETRANSP(1)%VALUES(1)%ni_transp%diff_eff    =  0.E0_R8
      CORETRANSP(1)%VALUES(1)%ni_transp%vconv_eff   =  0.E0_R8

      CORETRANSP(1)%VALUES(1)%ti_transp%diff_eff    =  DIFF_TI
      CORETRANSP(1)%VALUES(1)%ti_transp%vconv_eff   =  0.E0_R8

      CORETRANSP(1)%VALUES(1)%vtor_transp%diff_eff  =  0.E0_R8
      CORETRANSP(1)%VALUES(1)%vtor_transp%vconv_eff =  0.E0_R8



! +++ Deallocate local variables:
      DEALLOCATE    ( AMJ     )
      DEALLOCATE    ( ZMJ     )

      DEALLOCATE    ( RHO     )
      DEALLOCATE    ( TE      )
      DEALLOCATE    ( NE      )
      DEALLOCATE    ( MU      )

      DEALLOCATE    ( DIFF_TE )
      DEALLOCATE    ( DIFF_TI )

      DEALLOCATE    ( NZIMP   )
      DEALLOCATE    ( NCOMP   )
      DEALLOCATE    ( NTYPE   ) 


END SUBROUTINE GB_TRANSPORT
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   



END MODULE GBTRANSPORT
