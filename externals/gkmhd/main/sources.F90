SUBROUTINE Sources(tau)

!...  profile sustainment only

  USE Coeff
  USE Vars

  IMPLICIT NONE

  REAL :: tau

  INTEGER :: i,j,j0,mu,ileft,iright,ioff
  REAL :: tdfl,tsrc,p0,pleft,pright

  dvp0=tau*sflux*psfr

  DO i=1+ngdx,nx0+ngdx
     j0=fsfc(i)%ny0
     DO j=1+ngdy,j0+ngdy
        DO mu=1,neqs
           fsfc(i)%suu(mu,j)=fsfc(i)%suu(mu,j) &
                - dvp0(i)*(fsfc(i)%uu(0,mu,j)-nn0)
        END DO
        fsfc(i)%suu(1,j)=fsfc(i)%suu(1,j) + tau*sn0*psfl(i)
        fsfc(i)%suu(2,j)=fsfc(i)%suu(2,j) + tau*(sn0+sv0)*psfl(i)
     END DO
  END DO

END SUBROUTINE Sources
