SUBROUTINE Yshift(dyp,mu1,mu2)

  USE Coeff
  USE Vars

  IMPLICIT NONE

  REAL :: dyp
  INTEGER :: mu1,mu2

  INTEGER :: i,j,j0,j1,j2
  INTEGER :: jy
  REAL :: dy

!  jy=FLOOR(dyp)

  DO i=1+ngdx,nx0+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy

     dy=dyp/qtor(i)
     dy=dy/fsfc(i)%hy
     jy=FLOOR(dy)
     dy=dy-jy

     fsfc(i)%uu(:,mu1:mu2,j1:j2)= &
          (1.-dy)*CSHIFT(fsfc(i)%uu(:,mu1:mu2,j1:j2),jy,3) &
          + dy*CSHIFT(fsfc(i)%uu(:,mu1:mu2,j1:j2),jy+1,3)

     IF (ngdy > 0) THEN
        DO j=1,ngdy
           fsfc(i)%uu(:,mu1:mu2,j)=fsfc(i)%uu(:,mu1:mu2,j0+j)
           fsfc(i)%uu(:,mu1:mu2,j2+j)=fsfc(i)%uu(:,mu1:mu2,j2+j-j0)
        END DO
     END IF

  END DO

END SUBROUTINE Yshift


SUBROUTINE Yshift2d(dyp)

  USE Coeff
  USE Vars

  IMPLICIT NONE

  REAL :: dyp

  INTEGER :: i,j,j0,j1,j2
  INTEGER :: jy
  REAL :: dy

!  jy=FLOOR(dyp)

  DO i=1+ngdx,nx0+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy

     dy=dyp/qtor(i)
     dy=dy/fsfc(i)%hy
     jy=FLOOR(dy)
     dy=dy-jy

     fsfc(i)%uuc(j1:j2)= &
          (1.-dy)*CSHIFT(fsfc(i)%uuc(j1:j2),jy) &
          + dy*CSHIFT(fsfc(i)%uuc(j1:j2),jy+1)

     IF (ngdy > 0) THEN
        DO j=1,ngdy
           fsfc(i)%uuc(j)=fsfc(i)%uuc(j0+j)
           fsfc(i)%uuc(j2+j)=fsfc(i)%uuc(j2+j-j0)
        END DO
     END IF

  END DO

END SUBROUTINE Yshift2d


SUBROUTINE Yshiftf(dyp,mu1,mu2)

  USE Coeff
  USE Vars

  IMPLICIT NONE

  REAL :: dyp
  INTEGER :: mu1,mu2

  INTEGER :: i,j,j0,j1,j2
  INTEGER :: jy
  REAL :: dy

!  jy=FLOOR(dyp)

  DO i=1+ngdx,nx0+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy

     dy=dyp/qtor(i)
     dy=dy/fsfc(i)%hy
     jy=FLOOR(dy)
     dy=dy-jy

     fsfc(i)%fuu(mu1:mu2,j1:j2)= &
          (1.-dy)*CSHIFT(fsfc(i)%fuu(mu1:mu2,j1:j2),jy,2) &
          + dy*CSHIFT(fsfc(i)%fuu(mu1:mu2,j1:j2),jy+1,2)

     IF (ngdy > 0) THEN
        DO j=1,ngdy
           fsfc(i)%fuu(mu1:mu2,j)=fsfc(i)%fuu(mu1:mu2,j0+j)
           fsfc(i)%fuu(mu1:mu2,j2+j)=fsfc(i)%fuu(mu1:mu2,j2+j-j0)
        END DO
     END IF

  END DO

END SUBROUTINE Yshiftf


