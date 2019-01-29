SUBROUTINE Probes(iu)

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: iu
  INTEGER :: nxpr,i,j,j1,j2,mu,mua
  LOGICAL, SAVE :: first_flag = .TRUE.
  REAL, DIMENSION(:), ALLOCATABLE :: uupr

  OPEN (iu,file=dfile,form='unformatted',position='append')

  nxpr=nx2

  DO mua=1,2
     IF (mua == 1) mu=muphi
     IF (mua == 2) mu=1
     DO i=1+ngdx,nx0+ngdx
        IF (i == nxpr) THEN

           IF (first_flag) THEN
              j=fsfc(i)%ny0
              ALLOCATE(uupr(j))

              j1=1+ngdy
              j2=j+ngdy

              first_flag = .FALSE.
           END IF

           uupr(1:j)=fsfc(i)%uu(0,mu,j1:j2)
           WRITE (iu) uupr
        END IF
     END DO
  END DO

  CLOSE (iu)

END SUBROUTINE Probes
