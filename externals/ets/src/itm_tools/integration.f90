MODULE INTEGRATION

CONTAINS

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!  This subroutine calculates integral of a function y(x)
!  from X=0
SUBROUTINE INTEGRAL_VALUE(N,X,Y,INTY)

    use itm_types

    IMPLICIT NONE

    INTEGER :: N                                          ! number of radial points (input)
    INTEGER :: I

    REAL (R8) :: X(N), &                                  ! argument array (input)
         Y(N), &                                  ! function array (input)
         INTY(N)                                  ! function integral array (output)


    INTY(1)=Y(1)*X(1)/2.0_R8
    DO i=2,N
       INTY(I)=INTY(I-1)+(Y(I)+Y(I-1))*(X(I)-X(I-1))/2.0_R8
    ENDDO

    RETURN
END SUBROUTINE INTEGRAL_VALUE
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

END MODULE INTEGRATION
