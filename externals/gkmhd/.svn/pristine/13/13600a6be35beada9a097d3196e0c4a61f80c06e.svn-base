SUBROUTINE Arakawa(thxm,uu,mua,suu,mu1,mu9)

!...  2D Poisson bracket construct, Arakawa scheme
!...  4th order

  USE Dim

  IMPLICIT NONE

  INTEGER :: mua,mu1,mu9
  REAL :: thxm
  REAL, DIMENSION(0:n_neighbors,nvars,nx,ny) :: uu
  REAL, DIMENSION(nx,ny,neqs) :: suu

  INTEGER :: mu,muc
  INTEGER :: i,j
  REAL :: jplusplus,jpluscross,jcrossplus, &
       jcrosscross,jpluscross2,jcrossplus2

  DO j=2,ny-1
     DO i=2,nx-1

        DO mu=mu1,mu9
           muc=mua+mu
        jplusplus = (uu(1,muc,i,j)-uu(5,muc,i,j))*(uu(3,mu,i,j)-uu(7,mu,i,j)) &
             - (uu(3,muc,i,j)-uu(7,muc,i,j))*(uu(1,mu,i,j)-uu(5,mu,i,j))
        jpluscross = uu(1,muc,i,j)*(uu(2,mu,i,j)-uu(8,mu,i,j)) &
             - uu(5,muc,i,j)*(uu(4,mu,i,j)-uu(6,mu,i,j)) &
             - uu(3,muc,i,j)*(uu(2,mu,i,j)-uu(4,mu,i,j)) &
             + uu(7,muc,i,j)*(uu(8,mu,i,j)-uu(6,mu,i,j))
        jcrossplus = uu(2,muc,i,j)*(uu(3,mu,i,j)-uu(1,mu,i,j)) &
             - uu(6,muc,i,j)*(uu(5,mu,i,j)-uu(7,mu,i,j)) &
             - uu(4,muc,i,j)*(uu(3,mu,i,j)-uu(5,mu,i,j)) &
             + uu(8,muc,i,j)*(uu(1,mu,i,j)-uu(7,mu,i,j))

        suu(i,j,mu)=suu(i,j,mu) - thxm*(jplusplus+jpluscross+jcrossplus)
        END DO

     END DO
  END DO

!... done

END SUBROUTINE Arakawa
