SUBROUTINE Polarise

!...  Laplacian polarisation

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: i,j,j0

!...  recover vars

  DO i=1,nx
     j0=fsfc(i)%ny
     DO j=1,j0
        fsfc(i)%uu(0,nvars-2,j)=fsfc(i)%rtor(j)
        fsfc(i)%uu(0,nvars-1,j)=fsfc(i)%ztor(j)
        fsfc(i)%uu(0,nvars,j)=fsfc(i)%btor(j)
     END DO
  END DO

  CALL Neighbors(1,neqs)
  CALL Neighbors(nvars-2,nvars)
  CALL Loops

END SUBROUTINE Polarise


SUBROUTINE Getpuu(mu1,mu9)

  USE Vars

  IMPLICIT NONE

  INTEGER :: mu1,mu9

  INTEGER :: i,j,mu

  DO i=1,nx
     DO j=1+ngdy,fsfc(i)%ny0+ngdy
        DO mu=mu1,mu9
           fsfc(i)%puu(mu,j)=fsfc(i)%uu(0,mu,j)
        END DO
     END DO
  END DO

END SUBROUTINE Getpuu
