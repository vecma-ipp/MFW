module besselx

contains 

  SUBROUTINE Besselj0(x,j0)
!!$  Abramowitz & Stegun, Eqs 9.4.1 und 9.4.3 on p 369
!!$  maintained by B Scott after 2005 as part of delta-FEFI
!!$  definitive reference for delta-FEFI: Phys Plasmas 17 (2010) 102306

    IMPLICIT NONE

    REAL :: x,j0,y

    REAL, PARAMETER, DIMENSION (7) :: a = &
         (/ 1.0000000, -2.2499997, 1.2656208, -0.3163866, &
         0.0444479, -0.0039444, 0.0002100 /)
    REAL, PARAMETER, DIMENSION (7) :: b = &
         (/  0.79788456, -0.00000770, -0.00552740, -0.00009512, &
         0.00137237, -0.00072805,  0.00014476 /)
    REAL, PARAMETER, DIMENSION (7) :: c = &
         (/ -0.78539816, -0.04166397, -0.00003954,  0.00262573, &
         -0.00054125, -0.00029333,  0.00013558 /)

    IF (x < 3.0) THEN
       y = (x/3.0)**2
       j0 = a(1)+y*(a(2)+y*(a(3)+y*(a(4)+y*(a(5)+y*(a(6)+y*a(7))))))
    ELSE
       y = 3.0/x
       j0 = (b(1)+y*(b(2)+y*(b(3)+y*(b(4)+y*(b(5)+y*(b(6)+y*b(7))))))) &
            *COS(x+c(1)+y*(c(2)+y*(c(3)+y*(c(4)+y*(c(5)+y*(c(6)+y*c(7))))))) &
            /SQRT(x)
    END IF

  END SUBROUTINE Besselj0

end module besselx
