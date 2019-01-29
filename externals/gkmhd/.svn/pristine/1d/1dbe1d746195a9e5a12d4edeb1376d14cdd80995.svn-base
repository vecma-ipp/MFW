SUBROUTINE Psnaps(iu,time)

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: iu
  REAL :: time

  INTEGER :: i,j,j0,j1,j2
  INTEGER :: ip,i0,i1,i9

  OPEN (iu,file=pfile,form='unformatted',position='append')
  WRITE (iu) time

  CALL Getpuu(1,nvsnap)

  i0=nx00/npesx0

  DO ip=0,npesx0-1
     i1=ip*i0+ngdx+1
     i9=ip*i0+ngdx+i0

     DO i=i1,i9
        j0=fsfc(i)%ny0
        j1=ngdy+1
        j2=ngdy+j0

        WRITE (iu) fsfc(i)%puu(1:nvsnap,j1:j2)
     END DO
  END DO

  CLOSE(iu)

END SUBROUTINE Psnaps
