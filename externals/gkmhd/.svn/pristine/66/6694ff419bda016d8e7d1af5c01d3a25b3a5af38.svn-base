SUBROUTINE Bndys

!...  on fuu within fsfc structure, parallelised over x

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: mu1,mu9

  INTEGER :: i,j,i1,i2,i3,i4,j0,j1

!...  periodic for y

  IF (ngdy > 0) THEN
     DO i=1+ngdx,nx0+ngdx
        j0=fsfc(i)%ny0
        j1=j0+ngdy
        DO j=1,ngdy
           fsfc(i)%fuu(:,j)=fsfc(i)%fuu(:,j0+j)
           fsfc(i)%fuu(:,j1+j)=fsfc(i)%fuu(:,j1+j-j0)
        END DO
     END DO
  END IF

!...  Neumann/Dirichlet for x

  DO i=1,ngdx
     i1=ngdx+1
     fsfc(i)%fuu(:,:)=fsfc(i1)%fuu(:,:)
  END DO

  i1=nx0+ngdx
  DO i=i1+1,i1+ngdx
     fsfc(i)%fuu(:,:)=0.
  END DO

END SUBROUTINE Bndys
