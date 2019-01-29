SUBROUTINE Strip

!...  strips mean from vars for diags

  USE Vars

  IMPLICIT NONE

  INTEGER :: i,j,j0,j1,j2,mu

  DO i=1+ngdx,nx0+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy

     DO mu=1,nvars

        fsfc(i)%fuu(mu,:)=fsfc(i)%fuu(mu,:) &
             - SUM(fsfc(i)%fuu(mu,j1:j2))/j0

     END DO
  END DO

END SUBROUTINE Strip
