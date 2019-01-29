SUBROUTINE Psnapsin(iu,time,ieof)

  USE Vars
  USE Coeff

  IMPLICIT NONE

  INTEGER :: iu,ieof
  REAL :: time

  INTEGER :: i,j,j0,j1,j2
  INTEGER :: ip,i0,i1,i9

  ieof=0

  READ (iu,END=190) time

  i0=nx00/npesx0

  DO ip=0,npesx0-1
     i1=ip*i0+ngdx+1
     i9=ip*i0+ngdx+i0

     DO i=i1,i9
        j0=fsfc(i)%ny0
        j1=ngdy+1
        j2=ngdy+j0

        READ (iu) fsfc(i)%fuu(1:nvsnap,j1:j2)
     END DO
  END DO

  CALL Bndys

  i0=nx0+1
  i=i0+ngdx
  fsfc(i)%rtor=fsbndyr
  fsfc(i)%ztor=fsbndyz
  fsfc(i)%fuu(nvars-4,:)=fsbndyr
  fsfc(i)%fuu(nvars-3,:)=fsbndyz

  GOTO 191

190 ieof=1
  WRITE (0,*) "EOF reached in SNAPSIN"

191 CONTINUE

END SUBROUTINE Psnapsin
