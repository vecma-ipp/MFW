MODULE EQUILIBRIUM_TOOLS
!--------------------------------------------------------
!     The module contains subroutines                   !
!     used during initialization of ETS                 !
!     for defining magnetic configuration               !
!                                                       !
!     author: Denis Kalupin                             !
!     e-mail: Denis.Kalupin@euro-fusion.org             !
!--------------------------------------------------------



CONTAINS
!--------------------------------------------------------
!--------------------------------------------------------
SUBROUTINE GEOMETRY_FROM_WF_PARAMETERS(EQUILIBRIUM,   EQUILIBRIUM_OUT,  &
!AXIS:
                                       GEO_AX, MAG_AX, PLASMA_AX,       &
!GEOMETRICAL QUANTITIES:
                                       IP, AMIN,                        &
                                       ELONG_UP, ELONG_LOW,             &
                                       TRIA_UP, TRIA_LOW,               &
!SPACE_RESOLUTION:
                                       NPSI, NDIM1, NDIM2, NPOINTS) 

!--------------------------------------------------------
!     This subroutine prefiles equilibrium              !
!     variables from parameters provided in the         !
!     workflow.                                         !
!     It culates the boundary                           !
!     and RHO_TOR (at the boundary) for the ETS grid.   !
!--------------------------------------------------------

! +++ Declaration of variables: 
    USE EUITM_SCHEMAS
    USE ITM_TYPES
    USE ITM_CONSTANTS
    USE ETS_PLASMA

    IMPLICIT NONE

! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM(:)      !time independent CPO slice 
    TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM_OUT(:)  !time independent CPO slice 

! +++ Local derived types:
    TYPE (DIAGNOSTIC)                :: DIAG                !diagnostic output 

! +++ Internal derived types:
    REAL (R8)                        :: GEO_AX(3)           !{R, Z, B} at device geometrical centre
    REAL (R8)                        :: MAG_AX(3)           !{R, Z, B} at magnetic axis
    REAL (R8)                        :: PLASMA_AX(3)        !{R, Z, B} at LCMS centre

    REAL (R8)                        :: IP                  !Total plasma current [A]
    REAL (R8)                        :: AMIN                !minor radius [m]
    REAL (R8)                        :: ELONG               !averaged elongation oof LCMS
    REAL (R8)                        :: ELONG_UP, ELONG_LOW !upper and lower elongation oof LCMS
    REAL (R8)                        :: TRIA                !averaged triangularity oof LCMS
    REAL (R8)                        :: TRIA_UP,  TRIA_LOW  !upper and lower triangularity oof LCMS

    REAL (R8)                        :: THETA               !poloidal angle
    REAL (R8)                        :: RHO_BND             !toroidal minor radius, boundary value[m]
    REAL (R8),           ALLOCATABLE :: RHO(:)              !toroidal minor radius [m]
    REAL (R8),           ALLOCATABLE :: RHO_2D(:)           !toroidal minor radius for 2D signals [m]

    INTEGER                          :: NPSI,    IPSI       !total number and index of rho grid on grid and output CPOs    
    INTEGER                          :: NDIM1,   IDIM1      !total number and index of first dimension of EQ 2D signals   
    INTEGER                          :: NDIM2,   IDIM2      !total number and index of second dimension of EQ 2D signals
    INTEGER                          :: NPOINTS, ITHETA     !total number and index of boundary points    
    INTEGER                          :: NLINE, i
    CHARACTER*1000                   :: OUTPUT_DIAG



! +++ Nullify diagnostic output:
    DIAG%ERROR_MESSAGE    = " "
    DIAG%IERR             = 0


! +++ Allocate output CPO:
    IF(ASSOCIATED(EQUILIBRIUM_OUT)) DEALLOCATE (EQUILIBRIUM_OUT)
    ALLOCATE (EQUILIBRIUM_OUT(1))

! +++ GLOBAL_PARAM:
    EQUILIBRIUM_OUT(1)%global_param%i_plasma                    = IP
    EQUILIBRIUM_OUT(1)%global_param%toroid_field%r0             = GEO_AX(1)
    EQUILIBRIUM_OUT(1)%global_param%toroid_field%b0             = GEO_AX(3)
    
    EQUILIBRIUM_OUT(1)%global_param%mag_axis%position%r         = MAG_AX(1)
    EQUILIBRIUM_OUT(1)%global_param%mag_axis%position%z         = MAG_AX(2)
    EQUILIBRIUM_OUT(1)%global_param%mag_axis%bphi               = MAG_AX(3)


! +++ RHO BOUNDARY:
    CALL CALCULATE_RHO_TOR(PLASMA_AX, MAG_AX, AMIN, ELONG_UP, ELONG_LOW, TRIA_UP, TRIA_LOW, &
                           RHO_BND, DIAG)
    IF (DIAG%IERR.LE.-1) GOTO 90

    ALLOCATE (RHO(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor(NPSI))

    DO IPSI=1,NPSI
       RHO(IPSI)                                                = RHO_BND/(NPSI-1)*(IPSI-1)  
    END DO 
    EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor                      = RHO



! +++ PROFILES_1D:
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%elongation(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%tria_upper(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%tria_lower(NPSI))
    
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%r_inboard(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%r_outboard(NPSI))
    
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%r_inboard(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%r_outboard(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm1(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm2(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm3(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm4(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm5(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm6(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm7(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm8(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm9(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%volume(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%vprime(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%area(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%aprime(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%F_dia(NPSI))
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%rho_vol(NPSI))

    EQUILIBRIUM_OUT(1)%profiles_1d%elongation(NPSI)            = (ELONG_UP + ELONG_LOW) / 2._R8
    EQUILIBRIUM_OUT(1)%profiles_1d%tria_upper(NPSI)            = TRIA_UP
    EQUILIBRIUM_OUT(1)%profiles_1d%tria_lower(NPSI)            = TRIA_LOW
    
    EQUILIBRIUM_OUT(1)%profiles_1d%r_inboard(NPSI)             = PLASMA_AX(1) - AMIN
    EQUILIBRIUM_OUT(1)%profiles_1d%r_outboard(NPSI)            = PLASMA_AX(1) + AMIN
    EQUILIBRIUM_OUT(1)%profiles_1d%gm1                         = 1.E0_R8 / PLASMA_AX(1)**2 
    EQUILIBRIUM_OUT(1)%profiles_1d%gm2                         = 1.E0_R8 / PLASMA_AX(1)**2
    EQUILIBRIUM_OUT(1)%profiles_1d%gm3                         = 1.E0_R8 
    EQUILIBRIUM_OUT(1)%profiles_1d%gm4                         = 1.E0_R8 / PLASMA_AX(3)**2
    EQUILIBRIUM_OUT(1)%profiles_1d%gm5                         = PLASMA_AX(3)**2 
    EQUILIBRIUM_OUT(1)%profiles_1d%gm6                         = 1.E0_R8 / PLASMA_AX(3)**2 
    EQUILIBRIUM_OUT(1)%profiles_1d%gm7                         = 1.E0_R8 
    EQUILIBRIUM_OUT(1)%profiles_1d%gm8                         = PLASMA_AX(1) 
    EQUILIBRIUM_OUT(1)%profiles_1d%gm9                         = 1.E0_R8 / PLASMA_AX(1)
    EQUILIBRIUM_OUT(1)%profiles_1d%volume                      = 2.E0_R8 * ITM_PI**2 * RHO**2 * PLASMA_AX(1)
    EQUILIBRIUM_OUT(1)%profiles_1d%vprime                      = 4.E0_R8 * ITM_PI**2 * RHO    * PLASMA_AX(1)
    EQUILIBRIUM_OUT(1)%profiles_1d%area                        = ITM_PI * RHO**2
    EQUILIBRIUM_OUT(1)%profiles_1d%aprime                      = 4.E0_R8 * ITM_PI**2 * PLASMA_AX(1) 
    EQUILIBRIUM_OUT(1)%profiles_1d%F_dia                       = PLASMA_AX(3) * PLASMA_AX(1)
    EQUILIBRIUM_OUT(1)%profiles_1d%rho_vol                     = SQRT(EQUILIBRIUM_OUT(1)%profiles_1d%volume/EQUILIBRIUM_OUT(1)%profiles_1d%volume(NPSI))


! +++ EQ_GEOMETRY:
    EQUILIBRIUM_OUT(1)%eqgeometry%geom_axis%r                   = PLASMA_AX(1)
    EQUILIBRIUM_OUT(1)%eqgeometry%geom_axis%z                   = PLASMA_AX(2)

    EQUILIBRIUM_OUT(1)%eqgeometry%a_minor                       = AMIN
    EQUILIBRIUM_OUT(1)%eqgeometry%elongation                    = (ELONG_UP + ELONG_LOW) / 2._R8
    EQUILIBRIUM_OUT(1)%eqgeometry%elong_upper                   = ELONG_UP
    EQUILIBRIUM_OUT(1)%eqgeometry%elong_lower                   = ELONG_LOW
    EQUILIBRIUM_OUT(1)%eqgeometry%tria_upper                    = TRIA_UP
    EQUILIBRIUM_OUT(1)%eqgeometry%tria_lower                    = TRIA_LOW


! +++ BOUNDARY:
    ALLOCATE (EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1))
    ALLOCATE (EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%r(NPOINTS))
    ALLOCATE (EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z(NPOINTS))

    DO ITHETA=1, NPOINTS
       THETA                                                    = REAL(ITHETA-1,R8)/REAL(NPOINTS-1,R8)*2.0_R8*itm_pi
       IF (theta.LE.itm_pi)                         TRIA        = TRIA_UP
       IF (theta.LE.itm_pi)                         ELONG       = ELONG_UP
       IF (theta.GT.itm_pi)                         TRIA        = TRIA_LOW
       IF (theta.GT.itm_pi)                         ELONG       = ELONG_LOW
       EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%r(ITHETA)      = PLASMA_AX(1) + AMIN * (COS(theta) - TRIA*(SIN(theta))**2)
       EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z(ITHETA)      = PLASMA_AX(2) + AMIN * ELONG * SIN(theta)

       IF (ABS(EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z(ITHETA)).LE.1.E-15_R8)   &
            EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z(ITHETA) = 0.0E0_R8
    END DO


! +++ COORD_SYS:
    ALLOCATE   (RHO_2D(NDIM1))
    RHO_LOOP2: DO IDIM1=1,NDIM1
       RHO_2D(IDIM1)                                             = RHO_BND/(NDIM1-1)*(IDIM1-1)  
    END DO RHO_LOOP2

    ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%grid_type(1))
    ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%grid%dim1(NDIM1))
    ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%grid%dim2(NDIM2))
    ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%position%R(NDIM1,NDIM2))
    ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%position%Z(NDIM1,NDIM2))



    EQUILIBRIUM_OUT(1)%coord_sys%grid_type                      = '(rho,theta)'
    EQUILIBRIUM_OUT(1)%coord_sys%grid%dim1                      = RHO_2D
    
    DO IDIM2=1, NDIM2
       THETA                                                    = REAL(IDIM2-1,R8)/REAL(NDIM2-1,R8)*2.0_R8*itm_pi
       EQUILIBRIUM_OUT(1)%coord_sys%grid%dim2(IDIM2)            = THETA
       DO IDIM1=1, NDIM1
          
          IF (theta.LE.itm_pi)                      TRIA        = TRIA_UP
          IF (theta.GT.itm_pi)                      TRIA        = TRIA_LOW
          IF (theta.LE.itm_pi)                      ELONG       = ELONG_UP
          IF (theta.GT.itm_pi)                      ELONG       = ELONG_LOW
          
          EQUILIBRIUM_OUT(1)%coord_sys%position%R(IDIM1,IDIM2)  = PLASMA_AX(1) + AMIN * (COS(theta) - TRIA*(SIN(theta))**2) * RHO_2D(IDIM1)/RHO_2D(NDIM1)
          EQUILIBRIUM_OUT(1)%coord_sys%position%Z(IDIM1,IDIM2)  = PLASMA_AX(2) + AMIN * RHO_2D(IDIM1)/RHO_2D(NDIM1) * ELONG * SIN(theta)
          IF (ABS(EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z(IDIM2)).LE.1.E-15_R8)                                              &
               EQUILIBRIUM_OUT(1)%coord_sys%position%Z(IDIM1,IDIM2)  = 0.0E0_R8
       END DO
    END DO
    
    DEALLOCATE   (RHO)
    DEALLOCATE   (RHO_2D)

! +++ CODEPARAM:
 90 OUTPUT_DIAG = "GEOMETRY_FROM_WF_PARAMETERS: "
    OUTPUT_DIAG = OUTPUT_DIAG//TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))
    NLINE = FLOOR(LEN_TRIM(ADJUSTL(OUTPUT_DIAG))/132.001)+1
    ALLOCATE     (EQUILIBRIUM_OUT(1)%codeparam%codename(1))
    ALLOCATE     (EQUILIBRIUM_OUT(1)%codeparam%codeversion(1))
    ALLOCATE     (EQUILIBRIUM_OUT(1)%codeparam%output_diag(NLINE))

    EQUILIBRIUM_OUT(1)%codeparam%codename            = 'GEOM_FROM_WF_PARAMETERS'
    EQUILIBRIUM_OUT(1)%codeparam%codeversion         = 'GEOM_FROM_WF_PARAMETERS_4.10b.10'
    EQUILIBRIUM_OUT(1)%codeparam%output_flag         =  DIAG%IERR
    DO i = 1,NLINE
    EQUILIBRIUM_OUT(1)%codeparam%output_diag(i)      =  OUTPUT_DIAG(((i-1)*132+1):(i*132))
    END DO


RETURN
END SUBROUTINE GEOMETRY_FROM_WF_PARAMETERS
!--------------------------------------------------------
!--------------------------------------------------------



!--------------------------------------------------------
!--------------------------------------------------------
SUBROUTINE GEOMETRY_FROM_CPO(EQUILIBRIUM,   EQUILIBRIUM_OUT) 

!--------------------------------------------------------
!     This subroutine checks key                        !
!     equilibrium variables.                            !
!     If necessary it recalculates the boundary         !
!     and RHO_TOR (at the boundary) for the ETS grid.   !
!--------------------------------------------------------


! +++ Declaration of variables: 
    USE EUITM_SCHEMAS
    USE ITM_TYPES
    USE ITM_CONSTANTS
    USE ETS_PLASMA
    USE COPY_STRUCTURES
    USE DEALLOCATE_STRUCTURES

    IMPLICIT NONE

! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM(:)      !time independent CPO slice 
    TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM_OUT(:)  !time independent CPO slice 


! +++ Local derived types:
    TYPE (DIAGNOSTIC)                :: DIAG                !diagnostic output 


! +++ Internal derived types:
    REAL (R8)                        :: AMIN                !minor radius [m]
    REAL (R8)                        :: ELONG               !averaged elongation oof LCMS
    REAL (R8)                        :: ELONG_UP, ELONG_LOW !upper and lower elongation oof LCMS
    REAL (R8)                        :: TRIA                !averaged triangularity oof LCMS
    REAL (R8)                        :: TRIA_UP,  TRIA_LOW  !upper and lower triangularity oof LCMS

    REAL (R8)                        :: GEO_AX(3)           !{R, Z, B} at device geometrical centre
    REAL (R8)                        :: MAG_AX(3)           !{R, Z, B} at magnetic axis
    REAL (R8)                        :: PLASMA_AX(3)        !{R, Z, B} at LCMS centre

    REAL (R8)                        :: THETA               !poloidal angle
    REAL (R8)                        :: RHO_BND             !toroidal minor radius, boundary value[m]

    INTEGER                          :: NPSI=200,    IPSI   !total number and index of rho grid on grid and output CPOs    
    INTEGER                          :: NPOINTS=300, ITHETA !total number and index of boundary points    
    INTEGER                          :: NLINE, i
    CHARACTER*1000                   :: OUTPUT_DIAG



! +++ Nullify diagnostic output:
    DIAG%ERROR_MESSAGE    = " "
    DIAG%IERR             = 0


! +++ Copy input Equlibrium to output:
    ALLOCATE           (EQUILIBRIUM_OUT(1))
    CALL COPY_CPO      (EQUILIBRIUM(1),   EQUILIBRIUM_OUT(1)) 



! +++ Check Plasma Current:
    IF (EQUILIBRIUM(1)%global_param%i_plasma.NE.EQUILIBRIUM(1)%global_param%i_plasma) THEN
       DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"PLASMA CURRENT is not present in input EQUILIBRIUM. "
       DIAG%IERR             = -1
       GOTO 100
    END IF
    
    CALL EQ_PARAM_FROM_CPO(EQUILIBRIUM, GEO_AX, MAG_AX, PLASMA_AX,AMIN,   &
                           ELONG_UP, ELONG_LOW, TRIA_UP, TRIA_LOW, DIAG)
    IF (DIAG%IERR.LE.-1) GOTO 100

! +++ Check RHO_TOR:
    IF (.NOT.ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%rho_tor)) THEN
       DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"RHO_TOR is not present in input EQUILIBRIUM. "
       DIAG%IERR            = DIAG%IERR + 1

       CALL CALCULATE_RHO_TOR(PLASMA_AX, MAG_AX, AMIN, ELONG_UP, ELONG_LOW, TRIA_UP, TRIA_LOW, &
                              RHO_BND, DIAG)
       IF (DIAG%IERR.LE.-1)   GOTO 100

       IF (ASSOCIATED (EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor)) THEN
            NPSI            = SIZE(EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor)
         ELSE
            ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor(NPSI))
         END IF

       DO IPSI=1,NPSI
         EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor(IPSI)  = RHO_BND/(NPSI-1)*(IPSI-1)  
       END DO

       DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"RHO_TOR was recalculated. "
       
    END IF

! +++ Check Boundary:
    IF (.NOT.ASSOCIATED (EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%r) .OR.    &
        .NOT.ASSOCIATED (EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z))     THEN

       DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"Boundary is not defined in input EQUILIBRIUM. "
       DIAG%IERR            = DIAG%IERR + 1

       IF(.NOT.ASSOCIATED (EQUILIBRIUM_OUT(1)%eqgeometry%boundary))           &
            ALLOCATE      (EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1))

       IF(ASSOCIATED      (EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%r))   THEN
            NPOINTS = SIZE(EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%r)
            IF(.NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z))  &
            ALLOCATE      (EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z(NPOINTS))

         ELSE IF (ASSOCIATED(EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z)) THEN
            NPOINTS = SIZE(EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z)
            ALLOCATE      (EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%r(NPOINTS))

         ELSE
            ALLOCATE      (EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%r(NPOINTS))
            ALLOCATE      (EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z(NPOINTS))

         END IF


       DO ITHETA=1, NPOINTS
          THETA                                                    = REAL(ITHETA-1,R8)/REAL(NPOINTS-1,R8)*2.0_R8*itm_pi
          IF (theta.LE.itm_pi)                         TRIA        = TRIA_UP
          IF (theta.LE.itm_pi)                         ELONG       = ELONG_UP
          IF (theta.GT.itm_pi)                         TRIA        = TRIA_LOW
          IF (theta.GT.itm_pi)                         ELONG       = ELONG_LOW
          EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%r(ITHETA)      = PLASMA_AX(1) + AMIN * (COS(theta) - TRIA*(SIN(theta))**2)
          EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z(ITHETA)      = PLASMA_AX(2) + AMIN * ELONG * SIN(theta)
          
          IF (ABS(EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z(ITHETA)).LE.1.E-15_R8)   &
               EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z(ITHETA) = 0.0E0_R8
       END DO

    END IF


! +++ Outpu Error messages

!   CODEPARAM:
100 OUTPUT_DIAG = "GEOMETRY_FROM_CPO: "
    OUTPUT_DIAG = OUTPUT_DIAG//TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))
    NLINE = FLOOR(LEN_TRIM(ADJUSTL(OUTPUT_DIAG))/132.001)+1
    IF(ASSOCIATED(EQUILIBRIUM_OUT(1)%codeparam%codename))    &
       DEALLOCATE(EQUILIBRIUM_OUT(1)%codeparam%codename)
    IF(ASSOCIATED(EQUILIBRIUM_OUT(1)%codeparam%codeversion)) &
       DEALLOCATE(EQUILIBRIUM_OUT(1)%codeparam%codeversion)
    IF(ASSOCIATED(EQUILIBRIUM_OUT(1)%codeparam%output_diag)) &
       DEALLOCATE(EQUILIBRIUM_OUT(1)%codeparam%output_diag)
    ALLOCATE     (EQUILIBRIUM_OUT(1)%codeparam%codename(1))
    ALLOCATE     (EQUILIBRIUM_OUT(1)%codeparam%codeversion(1))
    ALLOCATE     (EQUILIBRIUM_OUT(1)%codeparam%output_diag(NLINE))

    EQUILIBRIUM_OUT(1)%codeparam%codename            = 'GEOM_FROM_CPO'
    EQUILIBRIUM_OUT(1)%codeparam%codeversion         = 'GEOM_FROM_CPO_4.10b.10'
    EQUILIBRIUM_OUT(1)%codeparam%output_flag         =  DIAG%IERR
    DO i = 1,NLINE
    EQUILIBRIUM_OUT(1)%codeparam%output_diag(i)      =  OUTPUT_DIAG(((i-1)*132+1):(i*132))
    END DO


RETURN
END SUBROUTINE GEOMETRY_FROM_CPO
!--------------------------------------------------------
!--------------------------------------------------------



!--------------------------------------------------------
!--------------------------------------------------------
SUBROUTINE CALCULATE_RHO_TOR(PLASMA_AX, MAG_AX,         &
                             AMIN,                      &
                             ELONG_UP,  ELONG_LOW,      &
                             TRIA_UP,   TRIA_LOW,       &
!                            OUTPUT:
                             RHO,       DIAG)
!-------------------------------------------------------!
!     This routine calculates RHO_TOR based             !
!     on three moment description of the boundary       !
!                                                       !
!-------------------------------------------------------!
!     Source:       ASTRA                               !
!     Developers:   D.Kalupin                           !
!     Contacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Comments:     created for the ETS workflow        !
!                                                       !
!-------------------------------------------------------!

    USE ITM_CONSTANTS
    USE ETS_PLASMA
    USE ITM_TYPES

    IMPLICIT NONE

! +++ Local derived types:
    TYPE (DIAGNOSTIC) :: DIAG        !diagnostic output 

! +++ Input/Output variables:
    REAL (R8)         :: PLASMA_AX(3)! PLASMA AXIS:   {R[m],Z[m],B[T]}
    REAL (R8)         :: MAG_AX(3)   ! MAGNETIC AXIS: {R[m],Z[m],B[T]}
    REAL (R8)         :: AMIN        ! minor radius    a[m]
    REAL (R8)         :: ELONG_UP    ! elongation upper
    REAL (R8)         :: ELONG_LOW   ! elongation lower
    REAL (R8)         :: TRIA_UP     ! triangularity upper
    REAL (R8)         :: TRIA_LOW    ! triangularity lower
 
    REAL (R8)         :: RHO         ! toroidal minor radius [m]

! +++ Internal (ASTRA) variables:
    REAL (R8)         :: YR          ! Major radius               [m]
    REAL (R8)         :: YD          ! Shafranov shift            [m]
    REAL (R8)         :: YA          ! Minor radius               [m]
    REAL (R8)         :: YE          ! Elongation                 [d/l]
    REAL (R8)         :: YT          ! Triangularity              [d/l]


    REAL (R8)         :: YD1,YT2,YGE
    REAL (R8)         :: Y1,Y2,Y3,Y4,Y5,Y6
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   



! +++ Copy values to ASTRA variables:
    YR   =  PLASMA_AX(1)     
    YD   =  MAG_AX(1) - PLASMA_AX(1)    
    YA   =  AMIN     
    YE   =  (ELONG_UP + ELONG_LOW) /  2._R8     
    YT   =  (TRIA_UP  + TRIA_LOW)  /  2._R8    




! +++ ASTRA routine:
     YT2 = 2._R8*YT
     YGE = YA/(YR+YD)
     IF (ABS(YGE) .GT. 1._R8)    GOTO 8
     IF (YGE) 9,5,1


  1  CONTINUE
     YD1 = 1._R8+YT2*(YT2-2._R8/YGE)
     IF (YD1) 3,2,2


  2  CONTINUE
     YD1 = SQRT(YD1)
     Y1 = (2._R8-YT2*YGE)*YD1/(1._R8+YD1)
     Y2 = YGE-YT2
     Y3 = SQRT(1._R8-YT2*YGE+YGE*YD1)
     Y4 = SQRT(1._R8-YT2*YGE-YGE*YD1)
     Y5 = SQRT(1._R8+YGE)
     Y6 = SQRT(1._R8-YGE)
     Y1 = (Y1+Y2)/(Y3+(1._R8-YT2)*Y5)-(Y1-Y2)/(Y4+(1._R8+YT2)*Y6)
     Y1 = Y1/SQRT(2._R8*(1._R8-YT2*YGE+Y5*Y6))
     GOTO 4


  3  CONTINUE
     Y5 = SQRT(1._R8+YGE)
     Y6 = SQRT(1._R8-YGE)
     Y1 = (1._R8-YT2)*Y5+(1._R8+YT2)*Y6
     Y1 = (1._R8-Y1/SQRT(2._R8*(1._R8-YT2*YGE+Y5*Y6)))/YT2


! +++ OUTPUT:
  4  CONTINUE
     RHO = 2._R8*SQRT(YR*YA*YE*Y1)
     GOTO 100


  5  CONTINUE
     RHO = 0._R8
     GOTO 100
 

  8  DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"CALCULATE_RHO_TOR >>> Illegal input: R+Delta < a. "
     DIAG%IERR            = -1
     GOTO 100


  9  DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"CALCULATE_RHO_TOR >>> Illegal input. "
     DIAG%IERR            = -1


 100 CONTINUE


RETURN
END SUBROUTINE CALCULATE_RHO_TOR
!--------------------------------------------------------
!--------------------------------------------------------



!--------------------------------------------------------
!--------------------------------------------------------
SUBROUTINE EQ_PARAM_FROM_CPO(EQUILIBRIUM,                     &
!AXIS:
                             GEO_AX, MAG_AX, PLASMA_AX,       &
!GEOMETRICAL QUANTITIES:
                             AMIN,                            &
                             ELONG_UP, ELONG_LOW,             &
                             TRIA_UP, TRIA_LOW,               &
                             DIAG) 

!--------------------------------------------------------
!     This subroutine checks key                        !
!     equilibrium variables.                            !
!--------------------------------------------------------


! +++ Declaration of variables: 
    USE EUITM_SCHEMAS
    USE ITM_TYPES
    USE ETS_PLASMA
    USE COPY_STRUCTURES

    IMPLICIT NONE

! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM(:)     !time independent CPO slice 


! +++ Local derived types:
    TYPE (DIAGNOSTIC)                :: DIAG               !diagnostic output 


! +++ Internal derived types:
    REAL (R8)                        :: R_0,   Z_0,   B_0
    REAL (R8)                        :: R_MAG, Z_MAG, B_MAG 
    REAL (R8)                        :: R_PLA, Z_PLA, B_PLA 

    REAL (R8)                        :: AMIN 
    REAL (R8)                        :: ELONG 
    REAL (R8)                        :: ELONG_UP,  ELONG_LOW 
    REAL (R8)                        :: TRIA_UP,   TRIA_LOW 
    REAL (R8)                        :: RHO_BND 

    REAL (R8)                        :: GEO_AX(3)  
    REAL (R8)                        :: MAG_AX(3)  
    REAL (R8)                        :: PLASMA_AX(3)  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

    

! +++ Global parameters:


! Minor radius
    IF (EQUILIBRIUM(1)%eqgeometry%a_minor.EQ.EQUILIBRIUM(1)%eqgeometry%a_minor) THEN
       AMIN                     = EQUILIBRIUM(1)%eqgeometry%a_minor
    ELSE
       IF (ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%r_inboard).AND.     &
           ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%r_outboard)) THEN
          AMIN                  = (EQUILIBRIUM(1)%profiles_1d%r_outboard(SIZE(EQUILIBRIUM(1)%profiles_1d%r_outboard)) - &
                                   EQUILIBRIUM(1)%profiles_1d%r_inboard(SIZE(EQUILIBRIUM(1)%profiles_1d%r_inboard))) / 2._R8
          DIAG%ERROR_MESSAGE    = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"MINOR RADIUS is approximated from other signals. "
          DIAG%IERR             = DIAG%IERR + 1
       ELSE
          IF (ASSOCIATED (EQUILIBRIUM(1)%eqgeometry%boundary(1)%r)) THEN
             AMIN               = (MAXVAL(EQUILIBRIUM(1)%eqgeometry%boundary(1)%r) - &
                                   MINVAL(EQUILIBRIUM(1)%eqgeometry%boundary(1)%r)) / 2._R8
             DIAG%ERROR_MESSAGE = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"MINOR RADIUS is approximated from other signals. "
             DIAG%IERR          = DIAG%IERR + 1
          ELSE
             DIAG%ERROR_MESSAGE = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"MINOR RADIUS is not present in input EQUILIBRIUM. "
             DIAG%IERR          = DIAG%IERR + 1
          END IF
        END IF
    END IF


! Elongation
    IF (EQUILIBRIUM(1)%eqgeometry%elongation.EQ.EQUILIBRIUM(1)%eqgeometry%elongation) THEN
       ELONG                   = EQUILIBRIUM(1)%eqgeometry%elongation
    ELSE
       IF (ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%elongation)) THEN
          ELONG                = EQUILIBRIUM(1)%profiles_1d%elongation(SIZE(EQUILIBRIUM(1)%profiles_1d%elongation))
       ELSE
          DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"ELONGATION is not present in input EQUILIBRIUM. "
          DIAG%IERR            = DIAG%IERR + 1
       END IF
    END IF


! Elongation Upper
    IF (EQUILIBRIUM(1)%eqgeometry%elong_upper.EQ.EQUILIBRIUM(1)%eqgeometry%elong_upper) THEN
       ELONG_UP                = EQUILIBRIUM(1)%eqgeometry%elong_upper
    ELSE
       IF (ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%elongation)) THEN
          ELONG_UP             = EQUILIBRIUM(1)%profiles_1d%elongation(SIZE(EQUILIBRIUM(1)%profiles_1d%elongation))
       ELSE
          DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"ELONG_UPPER is not present in input EQUILIBRIUM. "
          DIAG%IERR            = DIAG%IERR + 1
       END IF
    END IF


! Elongation Lower
    IF (EQUILIBRIUM(1)%eqgeometry%elong_lower.EQ.EQUILIBRIUM(1)%eqgeometry%elong_lower) THEN
       ELONG_LOW              = EQUILIBRIUM(1)%eqgeometry%elong_lower
    ELSE
       IF (ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%elongation)) THEN
          ELONG_LOW           = EQUILIBRIUM(1)%profiles_1d%elongation(SIZE(EQUILIBRIUM(1)%profiles_1d%elongation))
       ELSE
          DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"ELONG_LOWER is not present in input EQUILIBRIUM. "
          DIAG%IERR            = DIAG%IERR + 1
       END IF
    END IF


! Triangularity Upper
    IF (EQUILIBRIUM(1)%eqgeometry%tria_upper.EQ.EQUILIBRIUM(1)%eqgeometry%tria_upper) THEN
       TRIA_UP    = EQUILIBRIUM(1)%eqgeometry%tria_upper
    ELSE
       IF (ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%tria_upper)) THEN
          TRIA_UP= EQUILIBRIUM(1)%profiles_1d%tria_upper(SIZE(EQUILIBRIUM(1)%profiles_1d%tria_upper))
       ELSE
          DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"TRIANG_UPPER is not present in input EQUILIBRIUM. "
          DIAG%IERR            = DIAG%IERR + 1
       END IF
    END IF


! Triangularity Lower
    IF (EQUILIBRIUM(1)%eqgeometry%tria_lower.EQ.EQUILIBRIUM(1)%eqgeometry%tria_lower) THEN
       TRIA_LOW                = EQUILIBRIUM(1)%eqgeometry%tria_lower
    ELSE
       IF (ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%tria_lower)) THEN
          TRIA_LOW= EQUILIBRIUM(1)%profiles_1d%tria_lower(SIZE(EQUILIBRIUM(1)%profiles_1d%tria_lower))
       ELSE
          DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"TRIANG_LOWER is not present in input EQUILIBRIUM. "
          DIAG%IERR            = DIAG%IERR + 1
       END IF
    END IF


! +++ Axis:

!   GEOMETRICAL CENTRE OF THE DEVICE
    IF (EQUILIBRIUM(1)%global_param%toroid_field%r0.EQ.EQUILIBRIUM(1)%global_param%toroid_field%r0) THEN
       R_0        = EQUILIBRIUM(1)%global_param%toroid_field%r0
    ELSE
       DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"R_0 is not present in input EQUILIBRIUM. "
       DIAG%IERR            = DIAG%IERR + 1
       IF (ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%r_inboard).AND.     &
           ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%r_outboard)) THEN
          R_0     = (EQUILIBRIUM(1)%profiles_1d%r_inboard(SIZE(EQUILIBRIUM(1)%profiles_1d%r_inboard)) + &
                     EQUILIBRIUM(1)%profiles_1d%r_outboard(SIZE(EQUILIBRIUM(1)%profiles_1d%r_outboard))) / 2._R8
       ELSE
          IF (ASSOCIATED (EQUILIBRIUM(1)%eqgeometry%boundary(1)%r)) THEN
             R_0  = (MAXVAL(EQUILIBRIUM(1)%eqgeometry%boundary(1)%r) + &
                     MINVAL(EQUILIBRIUM(1)%eqgeometry%boundary(1)%r)) / 2._R8
          ELSE
             DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"R_0 can not be defined from other signals. "
          END IF
       END IF
    END IF


    IF (EQUILIBRIUM(1)%global_param%toroid_field%b0.EQ.EQUILIBRIUM(1)%global_param%toroid_field%b0) THEN
       B_0        = EQUILIBRIUM(1)%global_param%toroid_field%b0
    ELSE
       DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"B_0 is not present in input EQUILIBRIUM. "
       DIAG%IERR            = DIAG%IERR + 1
    END IF


    Z_0           = 0._R8




!   PLASMA AXIS
    IF (EQUILIBRIUM(1)%eqgeometry%geom_axis%r.EQ.EQUILIBRIUM(1)%eqgeometry%geom_axis%r) THEN
       R_PLA      = EQUILIBRIUM(1)%eqgeometry%geom_axis%r
    ELSE
       R_PLA      = R_0
       DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"R_plasma is not present in input EQUILIBRIUM. "
       DIAG%IERR            = DIAG%IERR + 1
    END IF

    IF (EQUILIBRIUM(1)%eqgeometry%geom_axis%z.EQ.EQUILIBRIUM(1)%eqgeometry%geom_axis%z) THEN
       Z_PLA      = EQUILIBRIUM(1)%eqgeometry%geom_axis%z
    ELSE
       Z_PLA      = Z_0
       DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"B_plasma is not present in input EQUILIBRIUM. "
       DIAG%IERR            = DIAG%IERR + 1
    END IF


    B_PLA         = B_0 * R_0 / R_PLA



!   MAGNETIC AXIS
    IF (EQUILIBRIUM(1)%global_param%mag_axis%position%r.EQ.EQUILIBRIUM(1)%global_param%mag_axis%position%r) THEN
       R_MAG      = EQUILIBRIUM(1)%global_param%mag_axis%position%r
    ELSE
       R_MAG      = R_PLA
       DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"R_mag is not present in input EQUILIBRIUM. "
       DIAG%IERR            = DIAG%IERR + 1
    END IF

    IF (EQUILIBRIUM(1)%global_param%mag_axis%position%z.EQ.EQUILIBRIUM(1)%global_param%mag_axis%position%z) THEN
       Z_MAG      = EQUILIBRIUM(1)%global_param%mag_axis%position%z
    ELSE
       Z_MAG      = Z_PLA
       DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"Z_mag is not present in input EQUILIBRIUM. "
       DIAG%IERR            = DIAG%IERR + 1
    END IF

    IF (EQUILIBRIUM(1)%global_param%mag_axis%bphi.EQ.EQUILIBRIUM(1)%global_param%mag_axis%bphi) THEN
       B_MAG      = EQUILIBRIUM(1)%global_param%mag_axis%bphi
    ELSE
       B_MAG      = B_PLA * R_PLA / R_MAG
       DIAG%ERROR_MESSAGE   = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//"B_mag is not present in input EQUILIBRIUM. "
       DIAG%IERR            = DIAG%IERR + 1
    END IF




    GEO_AX(1)    = R_0   
    GEO_AX(2)    = Z_0
    GEO_AX(3)    = B_0 

    MAG_AX(1)    = R_MAG
    MAG_AX(2)    = Z_MAG
    MAG_AX(3)    = B_MAG

    PLASMA_AX(1) = R_PLA
    PLASMA_AX(2) = Z_PLA 
    PLASMA_AX(3) = B_PLA 


RETURN
END SUBROUTINE EQ_PARAM_FROM_CPO
!--------------------------------------------------------
!--------------------------------------------------------




!--------------------------------------------------------
!--------------------------------------------------------
END MODULE EQUILIBRIUM_TOOLS
