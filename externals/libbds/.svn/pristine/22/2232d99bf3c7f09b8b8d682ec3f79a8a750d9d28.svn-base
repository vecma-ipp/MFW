module rans

contains

  FUNCTION Ran1(idum) RESULT(xx)
!!$ follows discussion given in Abramowitz & Stegun, Sec 26.8 on pp 949-50
!!$ formula Xn = (a X + b) mod T is given there, where a,b,T are integers
!!$     and X,Xn are also integers -- the real is formed by dividing by
!!$     the period T to produce a real pseudo-random on (0,1)
!!$ the three integers are given in Numerical Recipes as a good choice
!!$ coded by hand by B Scott in 1988 
!!$ used to start drift wave turbulence: J Comput Phys 78 (1988) 90
!!$     following method of J C McWilliams, J Fluid Mech 146 (1984) 21
!!$     studying formation of coherent vortices in 2D fluid turbulence

    USE ITM_Types

    IMPLICIT NONE

    REAL(R8) :: xx
    INTEGER(ITM_I4), INTENT(IN) :: idum

    INTEGER(ITM_I4), PARAMETER :: ia=3877, ib=29573, ic=139968
    INTEGER(ITM_I4), SAVE :: jran

    IF (idum < 0) jran=0

    jran=MOD(jran*ia+ib,ic)

    xx=REAL(jran)/REAL(ic)

  END FUNCTION Ran1

end module rans
