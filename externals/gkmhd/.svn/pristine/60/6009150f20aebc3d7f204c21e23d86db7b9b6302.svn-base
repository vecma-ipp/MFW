SUBROUTINE Delsq2d(amult,diag,muc,mux)

!...  perp Helmholtz operator to get uux from uuc
!...  these are controlled via indices mux and muc in uu in Vars

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: muc,mux
  REAL :: amult,diag

  INTEGER :: i,j,j0,j1,j2

  DO i=1+ngdx,nx0+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy

     DO j=j1,j2
        fsfc(i)%uu(0,mux,j)=diag*fsfc(i)%uu(0,muc,j) &
             + amult*SUM(fsfc(i)%aratio(1:n_neighbors,j)* &
             (fsfc(i)%uu(1:n_neighbors,muc,j)-fsfc(i)%uu(0,muc,j)))/ &
             fsfc(i)%area(j)
     END DO
  END DO

!...  call bndys in calling routine

END SUBROUTINE Delsq2d
