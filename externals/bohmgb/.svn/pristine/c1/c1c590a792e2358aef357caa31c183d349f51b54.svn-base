SUBROUTINE BohmGB(EQUILIBRIUM, COREPROF, CORETRANSP, code_parameters)

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
!     controlling parameter:   CODE_PARAMETERS          !
!                                                       !
!     IMPORTANT: the q profile is taken from the        !
!         equilbrium CPO, not coreprof                  !
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
!      USE EUITM_ROUTINES
      USE ITM_CONSTANTS

!      USE COPY_STRUCTURES
!      USE DEALLOCATE_STRUCTURES

!      USE ALLOCATE_DEALLOCATE

  USE Mod_Turb
  USE BohmGB_Coeff
  USE copy_structures
  USE write_structures

      IMPLICIT NONE


! +++ CPO derived types:
      TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM(:)   !input CPO with geometry quantities from previous time
      TYPE (TYPE_COREPROF),    POINTER  :: COREPROF(:)      !input CPO with plasma profiles   
      TYPE (TYPE_CORETRANSP),  POINTER  :: CORETRANSP(:)    !output CPO with transport coefficients   
      TYPE (type_param) :: code_parameters     !input xml file


!...  XML declarations

  integer(ITM_I4) :: return_status

  character(len = 132), target :: codename(1) = 'BohmGB'
  character(len = 132), target :: codeversion(1) = '0'


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
      INTEGER,               PARAMETER  :: NOCUR = 1        !number of CPO ocurancies in the work flow
      INTEGER                           :: NR_EQ, NRHO, IRHO       !number of radial points (input, determined from COREPROF CPO), radial index
      INTEGER                           :: NION, IION       !number of ion species   (input, determined from COREPROF CPO), ion index



! +++ Set dimensions:
      NRHO                   = SIZE (COREPROF(1)%rho_tor, DIM=1)
      NION                   = SIZE (COREPROF(1)%composition%amn, DIM=1)

! +++ Allocate output CPO:
!      CALL ALLOCATE_CORETRANSP_CPO  (NOCUR, NRHO, NION,        CORETRANSP )

!...  in the future loop this over nocur slices different from 1 slice
!...  some solution would have to be found for code_parameters

  IF (.NOT. ASSOCIATED(coretransp)) THEN

  allocate(coretransp(1))
  allocate(coretransp(1)%codeparam%codename(1))
  allocate(coretransp(1)%codeparam%codeversion(1))
  if (.not. associated(code_parameters%parameters)) then
    write(*,*) 'ERROR: BohmGB parameters not associated!'
    stop
  else
    allocate(coretransp(1)%codeparam%parameters(size( &
     code_parameters%parameters)))
  end if

  coretransp(1)%codeparam%codename = codename
  coretransp(1)%codeparam%codeversion = codeversion
  coretransp(1)%codeparam%parameters = code_parameters%parameters

  call assign_turb_parameters(code_parameters, return_status)

  if (return_status /= 0) then
    write(*,*) 'ERROR: Could not assign BohmGB parameters.'
    return
  end if

  CALL Turb_Constructor(coretransp(1), 1, nrho, nion)

  END IF

!...  write out input cpos

  IF (write_cpos) THEN

  call open_write_file(12, 'BohmGBIn' )
  call write_cpo(coreprof(1), 'Coreprof' )
  call write_cpo(equilibrium(1), 'Equil' )
  call close_write_file

  END IF

!...  copy composition over

  call copy_cpo(coreprof(1)%composition,coretransp(1)%composition)



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

      BTOR    = ABS(COREPROF(1)%toroid_field%b0)

      RHO     = COREPROF(1)%rho_tor
    
      TE      = COREPROF(1)%te%value / 1.E3_R8                    ! Temperature is specified in [keV]
      NE      = COREPROF(1)%ne%value / 1.E19_R8                   ! Density is specified in [10^19 m^-3]

!...  interpolate to get q, which does not come from coreprof

!      MU      = 1.0_R8 / COREPROF(1)%profiles1d%q%value

      NR_EQ=SIZE(EQUILIBRIUM(1)%profiles_1d%rho_tor)
      CALL L3interp( EQUILIBRIUM(1)%profiles_1d%q, &
           EQUILIBRIUM(1)%profiles_1d%rho_tor, NR_EQ, MU, RHO, NRHO)
      MU      = 1.0_R8 / MU

      ROC     = RHO(NRHO)

      AMJ     = COREPROF(1)%composition%amn
      ZMJ     = COREPROF(1)%composition%zion


      DO IION = 1, NION
         DO IRHO = 1, NRHO
  
         !-------------------------------------------------------!
         ! HAGB [m^2/s]:                                         !
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
        ! HATL [m^2/s]:	                                        !
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
         DIFF_TE(IRHO)      = chi_coeff_e * HATL + chigb_coeff_e * HAGB
         DIFF_TI(IRHO,IION) = chi_coeff_i * HATL + chigb_coeff_i * HAGB

         END DO   
      END DO


! +++ Save output in CPO:
!
! Only electron and ion heat diffusion is updated

      CORETRANSP(1)%time                  =  TIME                 !time    [s]

      CORETRANSP(1)%VALUES(1)%rho_tor               =  RHO                  !rho     [m]

!      CORETRANSP(1)%VALUES(1)%sigma                 =  0.E0_R8

      CORETRANSP(1)%VALUES(1)%te_transp%diff_eff    =  DIFF_TE

      CORETRANSP(1)%VALUES(1)%ne_transp%diff_eff(:,2) =  DIFF_TE / chi_d

      DO IION=1,NION
         CORETRANSP(1)%VALUES(1)%ni_transp%diff_eff(:,IION,2) =  &
              CORETRANSP(1)%VALUES(1)%ne_transp%diff_eff(:,2)
      END DO

      CORETRANSP(1)%VALUES(1)%ti_transp%diff_eff(:,1)    =  DIFF_TI(:,1)

      IF (nion > 1) CORETRANSP(1)%VALUES(1)%ti_transp%diff_eff(:,2:nion)   =  &
           chiratio_z*DIFF_TI(:,2:nion)

      CORETRANSP(1)%VALUES(1)%vtor_transp%diff_eff  =  chiratio_phi * DIFF_TI

      CORETRANSP(1)%VALUES(1)%rho_tor_norm = CORETRANSP(1)%VALUES(1)%rho_tor/ &
           MAXVAL(EQUILIBRIUM(1)%profiles_1d%rho_tor)

! +++ Deallocate local variables:
      DEALLOCATE    ( AMJ     )
      DEALLOCATE    ( ZMJ     )

      DEALLOCATE    ( RHO     )
      DEALLOCATE    ( TE      )
      DEALLOCATE    ( NE      )
      DEALLOCATE    ( MU      )

      DEALLOCATE    ( DIFF_TE )
      DEALLOCATE    ( DIFF_TI )


  IF (write_diags) THEN

open (10,file='transp.dat')
write (10,'(a3,12x,5(a2,13x))')  'RHO','XI','XE','TE','NE','MU'
do irho=1,nrho
   write (10,'(6(e14.8,1x))') CORETRANSP(1)%VALUES(1)%rho_tor(irho), &
        CORETRANSP(1)%VALUES(1)%ti_transp%diff_eff(irho,1), &
        CORETRANSP(1)%VALUES(1)%te_transp%diff_eff(irho), &
        COREPROF(1)%te%value(irho) / 1.E3_R8, &
        COREPROF(1)%ne%value(irho) / 1.E19_R8, &
        1.0_R8 / COREPROF(1)%profiles1d%q%value(irho)
enddo
close (10)

  END IF

!...  write out output cpos

  IF (write_cpos) THEN

  call open_write_file(12, 'BohmGBOut' )
  call write_cpo(coretransp(1), 'coretransp' )
  call close_write_file

  END IF

END SUBROUTINE BohmGB
