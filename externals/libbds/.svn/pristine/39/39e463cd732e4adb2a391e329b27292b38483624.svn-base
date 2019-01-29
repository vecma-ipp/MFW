module gammax

contains 

  SUBROUTINE Gamma012(x,gam0,gam1,gam2)
!!$  Abramowitz & Stegun, Eqs 9.8.1 und 9.8.2 on p 378
!!$  maintained by B Scott after 2005 as part of delta-FEFI
!!$  definitive reference for delta-FEFI: Phys Plasmas 17 (2010) 102306

    IMPLICIT NONE

    REAL :: x,gam0,gam1,gam2
    REAL :: y,dgamdx,ii,sx

    REAL, PARAMETER, DIMENSION (7) :: a = &
         (/ 1.0, 3.5156229, 3.0899424, 1.2067492, &
         0.2659732, 0.0360768, 0.0045813 /)
    REAL, PARAMETER, DIMENSION (9) :: b = &
         (/  0.39894228, 0.01328592, 0.00225319, -0.00157565, &
         0.00916281, -0.02057706, 0.02635537, -0.01647633, 0.00392377 /)

    IF (x == 0.) THEN
       gam0=1.
       gam1=1.
       gam2=0.
       RETURN
    END IF

    IF (x < 3.75) THEN
       y = (x/3.75)*(x/3.75)
       gam0 = (a(1)+y*(a(2)+y*(a(3)+y*(a(4)+y*(a(5)+y*(a(6)+y*a(7))))))) &
            * EXP(-x)
       dgamdx = (a(2)+y*(2.*a(3)+y*(3.*a(4)+ &
            y*(4.*a(5)+y*(5.*a(6)+y*6.*a(7)))))) * (x/7.03125) * EXP(-x) &
            - gam0 
    ELSE
       y = 3.75/x
       sx = 1./SQRT(x)
       ii = (b(1)+y*(b(2)+y*(b(3)+y*(b(4)+y*(b(5)+y*(b(6)+y*(b(7) &
            +y*(b(8)+y*b(9))))))))) 
       gam0 = ii * sx
       dgamdx = (y*(b(2)+y*(2.*b(3)+y*(3.*b(4)+y*(4.*b(5)+y*(5.*b(6)+y*(6.*b(7) &
            +y*(7.*b(8)+y*8.*b(9))))))))*(-3.75/(x*x)) - ii/(2.*x)) * sx
    END IF

    gam1=SQRT(gam0)
    gam2=(x/(2.*gam1))*dgamdx

  END SUBROUTINE Gamma012

end module gammax
