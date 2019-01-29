! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE GEOM_FROM_CPO(EQUILIBRIUM, GEO_AX, MAG_AX, PLASMA_AX, IP, AMIN, ELONG_UP, ELONG_LOW, TRIA_UP, TRIA_LOW, ifail) 


! +++ Declaration of variables: 
    USE EUITM_SCHEMAS
    USE ITM_TYPES


    IMPLICIT NONE

! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM(:)     !time independent CPO slice 


! +++ Internal derived types:
    REAL (R8)                        :: R_0,   Z_0,   B_0
    REAL (R8)                        :: R_MAG, Z_MAG, B_MAG 
    REAL (R8)                        :: R_PLA, Z_PLA, B_PLA 

    REAL (R8)                        :: IP 
    REAL (R8)                        :: AMIN 
    REAL (R8)                        :: ELONG 
    REAL (R8)                        :: ELONG_UP,  ELONG_LOW 
    REAL (R8)                        :: TRIA_UP,   TRIA_LOW 

    REAL (R8)                        :: GEO_AX(3)  
    REAL (R8)                        :: MAG_AX(3)  
    REAL (R8)                        :: PLASMA_AX(3)  

    INTEGER                          :: ifail               !0-no errors; 1-information errors were fixed by the routine; 2-missing informaion
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

    
    ifail         = 0

! +++ Global parameters:
    IF (EQUILIBRIUM(1)%global_param%i_plasma.EQ.EQUILIBRIUM(1)%global_param%i_plasma) THEN
       IP         = EQUILIBRIUM(1)%global_param%i_plasma
    ELSE
       WRITE(*,*)'PLASMA CURRENT is not present in input EQUILIBRIUM'
       ifail      = 2
    END IF

    IF (EQUILIBRIUM(1)%eqgeometry%a_minor.EQ.EQUILIBRIUM(1)%eqgeometry%a_minor) THEN
       AMIN       = EQUILIBRIUM(1)%eqgeometry%a_minor
    ELSE
       IF (ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%r_inboard).AND.     &
           ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%r_outboard)) THEN
          AMIN    = (EQUILIBRIUM(1)%profiles_1d%r_outboard(SIZE(EQUILIBRIUM(1)%profiles_1d%r_outboard)) - &
                     EQUILIBRIUM(1)%profiles_1d%r_inboard(SIZE(EQUILIBRIUM(1)%profiles_1d%r_inboard))) / 2._R8
       ELSE
          IF (ASSOCIATED (EQUILIBRIUM(1)%eqgeometry%boundary(1)%r)) THEN
             AMIN = (MAXVAL(EQUILIBRIUM(1)%eqgeometry%boundary(1)%r) - &
                     MINVAL(EQUILIBRIUM(1)%eqgeometry%boundary(1)%r)) / 2._R8
          ELSE
             WRITE(*,*)'MINOR RADIUS is not present in input EQUILIBRIUM'
             ifail      = 2
          END IF
        END IF
    END IF

    IF (EQUILIBRIUM(1)%eqgeometry%elongation.EQ.EQUILIBRIUM(1)%eqgeometry%elongation) THEN
       ELONG      = EQUILIBRIUM(1)%eqgeometry%elongation
    ELSE
       ifail      = MAX(ifail,1)
       IF (ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%elongation)) THEN
          ELONG   = EQUILIBRIUM(1)%profiles_1d%elongation(SIZE(EQUILIBRIUM(1)%profiles_1d%elongation))
       ELSE
          WRITE(*,*)'ELONGATION is not present in input EQUILIBRIUM'
          ifail   = 2
       END IF
    END IF

    IF (EQUILIBRIUM(1)%eqgeometry%elong_upper.EQ.EQUILIBRIUM(1)%eqgeometry%elong_upper) THEN
       ELONG_UP   = EQUILIBRIUM(1)%eqgeometry%elong_upper
    ELSE
       ifail      = MAX(ifail,1)
       IF (ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%elongation)) THEN
          ELONG_UP= EQUILIBRIUM(1)%profiles_1d%elongation(SIZE(EQUILIBRIUM(1)%profiles_1d%elongation))
       ELSE
          WRITE(*,*)'ELONG_UPPER is not present in input EQUILIBRIUM'
          ifail   = 2
       END IF
    END IF


    IF (EQUILIBRIUM(1)%eqgeometry%elong_lower.EQ.EQUILIBRIUM(1)%eqgeometry%elong_lower) THEN
       ELONG_LOW  = EQUILIBRIUM(1)%eqgeometry%elong_lower
    ELSE
       ifail      = MAX(ifail,1)
       IF (ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%elongation)) THEN
          ELONG_LOW= EQUILIBRIUM(1)%profiles_1d%elongation(SIZE(EQUILIBRIUM(1)%profiles_1d%elongation))
       ELSE
          WRITE(*,*)'ELONG_LOWER is not present in input EQUILIBRIUM'
          ifail   = 2
       END IF
    END IF

    IF (EQUILIBRIUM(1)%eqgeometry%tria_upper.EQ.EQUILIBRIUM(1)%eqgeometry%tria_upper) THEN
       TRIA_UP    = EQUILIBRIUM(1)%eqgeometry%tria_upper
    ELSE
       ifail      = MAX(ifail,1)
       IF (ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%tria_upper)) THEN
          TRIA_UP= EQUILIBRIUM(1)%profiles_1d%tria_upper(SIZE(EQUILIBRIUM(1)%profiles_1d%tria_upper))
       ELSE
          WRITE(*,*)'TRIANGULARITY UPPER is not present in input EQUILIBRIUM'
          ifail   = 2
       END IF
    END IF

    IF (EQUILIBRIUM(1)%eqgeometry%tria_lower.EQ.EQUILIBRIUM(1)%eqgeometry%tria_lower) THEN
       TRIA_LOW   = EQUILIBRIUM(1)%eqgeometry%tria_lower
    ELSE
       ifail      = MAX(ifail,1)
       IF (ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%tria_lower)) THEN
          TRIA_LOW= EQUILIBRIUM(1)%profiles_1d%tria_lower(SIZE(EQUILIBRIUM(1)%profiles_1d%tria_lower))
       ELSE
          WRITE(*,*)'TRIANGULARITY LOWER is not present in input EQUILIBRIUM'
          ifail   = 2
       END IF
    END IF


! +++ Axis:

!   GEOMETRICAL CENTRE OF THE DEVICE
    IF (EQUILIBRIUM(1)%global_param%toroid_field%r0.EQ.EQUILIBRIUM(1)%global_param%toroid_field%r0) THEN
       R_0        = EQUILIBRIUM(1)%global_param%toroid_field%r0
    ELSE
       ifail      = MAX(ifail,1)
       IF (ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%r_inboard).AND.     &
           ASSOCIATED (EQUILIBRIUM(1)%profiles_1d%r_outboard)) THEN
          R_0     = (EQUILIBRIUM(1)%profiles_1d%r_inboard(SIZE(EQUILIBRIUM(1)%profiles_1d%r_inboard)) + &
                     EQUILIBRIUM(1)%profiles_1d%r_outboard(SIZE(EQUILIBRIUM(1)%profiles_1d%r_outboard))) / 2._R8
       ELSE
          IF (ASSOCIATED (EQUILIBRIUM(1)%eqgeometry%boundary(1)%r)) THEN
             R_0  = (MAXVAL(EQUILIBRIUM(1)%eqgeometry%boundary(1)%r) + &
                     MINVAL(EQUILIBRIUM(1)%eqgeometry%boundary(1)%r)) / 2._R8
          ELSE
             WRITE(*,*)'R_P0 is not present in input EQUILIBRIUM'
             ifail      = 2
          END IF
       END IF
    END IF


    IF (EQUILIBRIUM(1)%global_param%toroid_field%b0.EQ.EQUILIBRIUM(1)%global_param%toroid_field%b0) THEN
       B_0        = EQUILIBRIUM(1)%global_param%toroid_field%b0
    ELSE
       WRITE(*,*)'B_0 is not present in input EQUILIBRIUM'
       ifail      = 2
    END IF


    Z_0           = 0._R8




!   PLASMA AXIS
    IF (EQUILIBRIUM(1)%eqgeometry%geom_axis%r.EQ.EQUILIBRIUM(1)%eqgeometry%geom_axis%r) THEN
       R_PLA      = EQUILIBRIUM(1)%eqgeometry%geom_axis%r
    ELSE
       R_PLA      = R_0
       WRITE(*,*)'R_PLASMA is not present in input EQUILIBRIUM'
       ifail      = MAX(ifail,1)
    END IF

    IF (EQUILIBRIUM(1)%eqgeometry%geom_axis%z.EQ.EQUILIBRIUM(1)%eqgeometry%geom_axis%z) THEN
       Z_PLA      = EQUILIBRIUM(1)%eqgeometry%geom_axis%z
    ELSE
       Z_PLA      = Z_PLA
       WRITE(*,*)'Z_PLASMA is not present in input EQUILIBRIUM'
       ifail      = MAX(ifail,1)
    END IF


    B_PLA         = B_0 * R_0 / R_PLA



!   MAGNETIC AXIS
    IF (EQUILIBRIUM(1)%global_param%mag_axis%position%r.EQ.EQUILIBRIUM(1)%global_param%mag_axis%position%r) THEN
       R_MAG      = EQUILIBRIUM(1)%global_param%mag_axis%position%r
    ELSE
       R_MAG      = R_PLA
       WRITE(*,*)'R_MAG is not present in input EQUILIBRIUM'
       ifail      = MAX(ifail,1)
    END IF

    IF (EQUILIBRIUM(1)%global_param%mag_axis%position%z.EQ.EQUILIBRIUM(1)%global_param%mag_axis%position%z) THEN
       Z_MAG      = EQUILIBRIUM(1)%global_param%mag_axis%position%z
    ELSE
       Z_MAG      = Z_PLA
       WRITE(*,*)'Z_MAG is not present in input EQUILIBRIUM'
       ifail      = MAX(ifail,1)
    END IF

    IF (EQUILIBRIUM(1)%global_param%mag_axis%bphi.EQ.EQUILIBRIUM(1)%global_param%mag_axis%bphi) THEN
       B_MAG      = EQUILIBRIUM(1)%global_param%mag_axis%bphi
    ELSE
       B_MAG      = B_PLA * R_PLA / R_MAG
       WRITE(*,*)'B_MAG is not present in input EQUILIBRIUM'
       ifail      = MAX(ifail,1)
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


  END SUBROUTINE GEOM_FROM_CPO
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
