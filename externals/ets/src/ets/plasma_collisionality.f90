! + + + + + + + + + + + + + + + + + + + + + + + + + + + + 
!> This routine calculates the collision frquencies and various exchange terms determined by collisions
!>
!> 2010-08-25: changed LOG() to LOG10() in the Coulomb logarithm expressions [DPC]
!>
!> \author S.Moradi, D.Kalupin
!>
!> \version "$Id: plasma_collisions.f90 1494 2013-04-13 14:20:13Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + 
MODULE PLASMA_COLLISIONALITY

CONTAINS

! + + + + + + + + + + + + + + + + + + + + + + + + + + + + 
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + 
   
SUBROUTINE PLASMA_COLLISIONS                        &
     (GEOMETRY,PROFILES,COLLISIONS,ifail) 

!-------------------------------------------------------!
!    This routine calculates the collision frquencies   !
!    and various exchange terms determined by collisions!
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   S.Moradi, D.Kalupin                 !
!     Contacts:     smoradi@ulb.ac.be                   !
!                   D.Kalupin@fz-juelich.de             !
!-------------------------------------------------------!

! +++ Declaration of variables: 

  USE ITM_CONSTANTS
  USE ETS_PLASMA

  IMPLICIT NONE


! +++ External parameters, input from / output to the work flow:
  TYPE (MAGNETIC_GEOMETRY) :: GEOMETRY                       !contains all geometry quantities
  TYPE (PLASMA_PROFILES)   :: PROFILES                       !contains profiles of plasma parameters
  TYPE (COLLISIONALITY)    :: COLLISIONS                     !contains all terms determined by plasma collisions

  INTEGER                  :: ifail



! +++ Internal parameters:
!     Indexes and dimensions
  INTEGER   :: NRHO                                          !number of radial points (input)
  INTEGER   :: NION                                          !number of ion species (input)

  INTEGER   :: IRHO                                          !current radial knot
  INTEGER   :: IION                                          !ion index 1
  INTEGER   :: IZ                                            !ion imdex 2

!     Plasma parameter profiles
  REAL (R8) :: BT                                            !toroidal magnetic field,                                               [T]
  REAL (R8) :: G2(GEOMETRY%NRHO)                             !<R>,                                                                   [m]

  REAL (R8) :: AION(PROFILES%NION)                           !ion mass number                                                        [--]      
  REAL (R8) :: ZION(PROFILES%NION)                           !ion charge numbers                                                     [--]      

  REAL (R8) :: NI(PROFILES%NRHO,PROFILES%NION)               !density of background ions,                                            [m^-3]
  REAL (R8) :: TI(PROFILES%NRHO,PROFILES%NION)               !ion temperature,                                                       [eV]
  REAL (R8) :: NE(PROFILES%NRHO)                             !electron density,                                                      [m^-3]
  REAL (R8) :: TE(PROFILES%NRHO)                             !electron temperature,                                                  [eV]
  REAL (R8) :: TZI, MZI, NZI                                 !effective:     ion temperature   [eV],    mass   [g],    and density   [m^-3]       
  REAL (R8) :: VTOR(PROFILES%NRHO,PROFILES%NION)             !ion toroidal velocity,                                                 [m/s]       


  INTEGER   :: TE_BND_TYPE                                   !boundary condition type for electron temperature, if = 0, no energy exchange to electrons
  INTEGER   :: TI_BND_TYPE(PROFILES%NION)                    !boundary condition type for ion temperature, if = 0, no energy exchange to this ion component
  INTEGER   :: VTOR_BND_TYPE(PROFILES%NION)                  !boundary condition type for ion rotation, if = 0, no momentum exchange to this ion component


!     Collision time and other intermediate quantities
  REAL (R8) :: CLOG                                          !Coulomb logarithm      
  REAL (R8) :: TAU_E(PROFILES%NRHO)                          !electron collision time,                                               [s]  
  REAL (R8) :: TAU_EI(PROFILES%NRHO,PROFILES%NION)           !electron-ion collision time,                                           [s]
  REAL (R8) :: TAU_ZI(PROFILES%NRHO,PROFILES%NION)           !ion-ion collision time,                                                [s]              



! +++ Output collision quantities:
!     current transport
  REAL (R8) :: SIGMA(PROFILES%NRHO)                          !plasma parallel conductivity,                                          [(Ohm*m)^-1]      

!     electron energy transport
  REAL (R8) :: VIE(PROFILES%NRHO)                            !energy sink from electrons to all ion components (frequency),          [1/(m^3*s)]     
  REAL (R8) :: QIE(PROFILES%NRHO)                            !energy input to electrons from all ion components,                     [eV/(m^3*s)]     

!     ion energy transport
  REAL (R8) :: VEI(PROFILES%NRHO,PROFILES%NION)              !energy sink from ion component (IION) to electons (frequency),         [1/(m^3*s)]
  REAL (R8) :: VZI(PROFILES%NRHO,PROFILES%NION)              !energy sink from ion component (IION) to all other ions (frequency),   [1/(m^3*s)]
  REAL (R8) :: VIZ(PROFILES%NRHO,PROFILES%NION,PROFILES%NION)!ion-ion collision frequency, MATRIX (NION,NION),                       [1/(m^3*s)]
  REAL (R8) :: QZI(PROFILES%NRHO,PROFILES%NION)              !energy input to ion component (IION) from all other ions,              [eV/(m^3*s)]
  REAL (R8) :: QEI(PROFILES%NRHO,PROFILES%NION)              !energy input to ion component (IION) from electons,                    [eV/(m^3*s)]

!     ion momentum transport
  REAL (R8) :: WIZ(PROFILES%NRHO,PROFILES%NION,PROFILES%NION)!ion-ion momentum exchange, MATRIX (NION,NION),                         [kg/(m^3*s)]
  REAL (R8) :: WZI(PROFILES%NRHO,PROFILES%NION)              !momentum sink from ion component (IION) to all other ions (frequency), [kg/(m^2*s)]
  REAL (R8) :: UZI(PROFILES%NRHO,PROFILES%NION)              !momentum input to ion component (IION) from all other ions,            [kg/(m*s^2)]


! +++ Constants:
  REAL (R8), PARAMETER   :: ME  = itm_me*1e3_R8              !electron mass                                                          [g]
  REAL (R8), PARAMETER   :: MP  = itm_mp*1e3_R8              !proton mass                                                            [g]      
  REAL (R8), PARAMETER   :: E   = itm_qe*3e9_R8              !elementary charge,                                                     [esu]

  REAL (R8), PARAMETER   :: CT  = itm_ev*1e7_R8              !energy associated with 1 eV,                                           [erg]
  REAL (R8), PARAMETER   :: CN  = 1.E-6_R8                   !density convergence                                                    from [m^-3] to [cm^-3]
  REAL (R8), PARAMETER   :: CS  = 9.E9_R8                    !conductivity convergence                                               from [(Ohm*m)^-1] to [s^-1]
  REAL (R8), PARAMETER   :: CM  = 1.E-3_R8                   !mass convergence                                                       from [g] to [kg]




! +++ Set up dimensions:
  NRHO                 = PROFILES%NRHO
  NION                 = PROFILES%NION



! +++ Boundary conditions for electron temperature:      
  TE_BND_TYPE          = PROFILES%TE_BND_TYPE


! +++ Boundary conditions for ion temperature and rotation:      
  ION_LOOP1: DO IION = 1,NION
     TI_BND_TYPE(IION)  = PROFILES%TI_BND_TYPE(IION)
     VTOR_BND_TYPE(IION)= PROFILES%VTOR_BND_TYPE(IION)
  END DO ION_LOOP1



! +++ Set up profiles:
  RHO_LOOP1: DO IRHO =1,NRHO
     G2(IRHO)           = GEOMETRY%G2(IRHO)

     TE(IRHO)           = PROFILES%TE(IRHO)
     NE(IRHO)           = PROFILES%NE(IRHO)

     ION_LOOP2: DO IION = 1,NION
        TI(IRHO,IION)    = PROFILES%TI(IRHO,IION)
        NI(IRHO,IION)    = PROFILES%NI(IRHO,IION)
        VTOR(IRHO,IION)  = PROFILES%VTOR(IRHO,IION)          
        AION(IION)       = PROFILES%MION(IION)
        ZION(IION)       = PROFILES%ZION(IION)
     END DO ION_LOOP2


! +++ Electron collisions:
!       determination of Coulomb logarithm:
     IF(TE(IRHO).GE.1.E1_R8) CLOG = 24.E0_R8 - 1.15E0_R8*LOG10(NE(IRHO)*CN) + 2.30E0_R8*LOG10(TE(IRHO))
     IF(TE(IRHO).LT.1.E1_R8) CLOG = 23.E0_R8 - 1.15E0_R8*LOG10(NE(IRHO)*CN) + 3.45E0_R8*LOG10(TE(IRHO))


!       electron collision time:
     TAU_E(IRHO)          = (SQRT(2.D0*ME)*(Te(IRHO))**1.5) / 1.8D-19 / (NE(IRHO)*CN) / CLOG


!       Plasma electrical conductivity:
     SIGMA(IRHO)          = 1.96E0_R8 * E**2 *NE(IRHO)*CN * TAU_E(IRHO) /ME /CS

  END DO RHO_LOOP1



! +++ Various ion exchange quantities:      
  RHO_LOOP2: DO IRHO = 1,NRHO
     VIE(IRHO)            = 0.e0_R8
     QIE(IRHO)            = 0.e0_R8



     ION_LOOP3: DO IION = 1,NION
        VEI(IRHO,IION)     = 0.e0_R8
        QEI(IRHO,IION)     = 0.e0_R8
        VZI(IRHO,IION)     = 0.e0_R8
        QZI(IRHO,IION)     = 0.e0_R8
        UZI(IRHO,IION)     = 0.e0_R8
        WZI(IRHO,IION)     = 0.e0_R8               !DPC 2009-01-19



! +++ Electron-Ion collisions: 
!         determination of Coulomb logarithm:
        IF(TE(IRHO).GE.10.*ZION(IION)**2) THEN
           CLOG = 24.E0_R8 - 1.15E0_R8*LOG10(NE(IRHO)*CN) + 2.30E0_R8*LOG10(TE(IRHO))
        ELSE IF(TE(IRHO).LT.10.*ZION(IION)**2) THEN
           CLOG = 23.E0_R8 - 1.15E0_R8*LOG10(NE(IRHO)*CN) + 3.45E0_R8*LOG10(TE(IRHO))
        ELSE
           WRITE(*,*) 'Should not get here!'
           WRITE(*,*) IRHO, TE(IRHO)
           PAUSE
           STOP 'ERROR'
        ENDIF

!         determination of electron-ion collision time and energy exchange term:
        TAU_EI(IRHO,IION)   = (Te(IRHO)*MP*AION(IION) + TI(IRHO,IION)*ME)**1.5 / 1.8D-19                  &
		               / (SQRT(AION(IION)*ME*MP)) / (NI(IRHO,IION)*CN) / ZION(IION)**2 / CLOG



        VEI(IRHO,IION)      = 0.e0_R8 

        IF (TAU_EI(IRHO,IION).GT.0.e0_R8.AND.TE_BND_TYPE.NE.0.AND.TI_BND_TYPE(IION).NE.0)                 &
!       from electrons to ion component (IION):
        VEI(IRHO,IION)      = NE(IRHO) / TAU_EI(IRHO,IION)
        QEI(IRHO,IION)      = VEI(IRHO,IION) * TE(IRHO)          


!       from all ion components to electrons:
        VIE(IRHO)           = VIE(IRHO) + VEI(IRHO,IION)
        QIE(IRHO)           = QIE(IRHO) + VEI(IRHO,IION) * TI(IRHO,IION)



! +++ Ion-Ion collisions: 
        ION_LOOP4: DO IZ = 1,NION
           IF (IZ.NE.IION) THEN
           VIZ(IRHO,IION,IZ) = 0.e0_R8  


!           determination of Coulomb logarithm:
           MZI               = MP * AION(IION)*AION(IZ) / (AION(IZ)+AION(IION)) 

           CLOG = 23.0D0 - LOG( ZION(IION)*ZION(IZ) * (AION(IION)+AION(IZ)) / (AION(IION)*Ti(IRHO,IZ)+AION(IZ)*Ti(IRHO,IION))) &
	                 - LOG( SQRT(NI(IRHO,IION)*CN*ZION(IION)**2./Ti(IRHO,IION) + NI(IRHO,IZ)*CN*ZION(IZ)**2./Ti(IRHO,IZ)) )


!           determination of ion-ion collision time and energy exchange term:
           TAU_ZI(IRHO,IZ) = (Ti(IRHO,IION)*MP*AION(IZ) + TI(IRHO,IZ)*MP*AION(IION))**1.5 / 1.8D-19       &
		               / (SQRT(AION(IION)*MP*AION(IZ)*MP)) / (NI(IRHO,IION)*CN)/ZION(IION)**2/ZION(IZ)**2 / CLOG




!           ion-ion collision frequency:
           VIZ(IRHO,IION,IZ) = 0.e0_R8 
           WIZ(IRHO,IION,IZ) = 0.e0_R8 

           IF (TAU_ZI(IRHO,IZ).GT.0.e0_R8.AND.TI_BND_TYPE(IION).NE.0.AND.TI_BND_TYPE(IZ).NE.0)     &          
                VIZ(IRHO,IION,IZ) = NI(IRHO,IZ) / TAU_ZI(IRHO,IZ)

           IF (TAU_ZI(IRHO,IZ).GT.0.e0_R8.AND.VTOR_BND_TYPE(IION).NE.0.AND.VTOR_BND_TYPE(IZ).NE.0) &          
                WIZ(IRHO,IION,IZ) = NI(IRHO,IZ) / TAU_ZI(IRHO,IZ) * MP*AION(IZ)*CM * G2(IRHO)


!           total exchange terms, from all ion components to ion component (IION):
           VZI(IRHO,IION)    = VZI(IRHO,IION) + VIZ(IRHO,IION,IZ)
           QZI(IRHO,IION)    = QZI(IRHO,IION) + VIZ(IRHO,IION,IZ) * TI(IRHO,IZ) 
           WZI(IRHO,IION)    = WZI(IRHO,IION) + WIZ(IRHO,IION,IZ)           
           UZI(IRHO,IION)    = UZI(IRHO,IION) + WIZ(IRHO,IION,IZ) * VTOR(IRHO,IZ)      
           
           END IF



! +++ Collision quantities, output to the work flow:
           COLLISIONS%VII(IRHO,IION,IZ) = VIZ(IRHO,IION,IZ)
           COLLISIONS%WII(IRHO,IION,IZ) = WIZ(IRHO,IION,IZ)

        END DO ION_LOOP4

        COLLISIONS%VEI(IRHO,IION)       = VEI(IRHO,IION)
        COLLISIONS%QEI(IRHO,IION)       = QEI(IRHO,IION)
        COLLISIONS%VZI(IRHO,IION)       = VZI(IRHO,IION)
        COLLISIONS%QZI(IRHO,IION)       = QZI(IRHO,IION)
        COLLISIONS%WZI(IRHO,IION)       = WZI(IRHO,IION)
        COLLISIONS%UZI(IRHO,IION)       = UZI(IRHO,IION)

     END DO ION_LOOP3

     COLLISIONS%SIGMA(IRHO)            = SIGMA(IRHO)
     COLLISIONS%VIE(IRHO)              = VIE(IRHO)
     COLLISIONS%QIE(IRHO)              = QIE(IRHO)

  END DO RHO_LOOP2




  RETURN




END SUBROUTINE PLASMA_COLLISIONS

! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +


END MODULE PLASMA_COLLISIONALITY
