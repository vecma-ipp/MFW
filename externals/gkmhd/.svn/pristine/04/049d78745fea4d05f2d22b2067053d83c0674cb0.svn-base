PROGRAM Snap

!...  reloads a t file from a given step in a p file
!...  local two dimensional version

  USE Vars
  USE Coeff
  USE Plotins

  IMPLICIT NONE

  INTEGER :: ipl,isnap,ieof
  REAL :: time

!...  allocations

  INCLUDE 'sinit.h90'

  OPEN (14,file='p1.dat',form='unformatted',status='old')

  ALLOCATE(uu(ny,nx,nvars))

!...  search for snap frame

  DO ipl=1,10000
     isnap=0
     CALL Psnapsin(14,time,ieof)
     IF (ieof /= 0) EXIT
     IF (NINT(time) == ntsnap .OR. ipl == nfsnap) isnap=1
     IF (isnap == 1) THEN
        CALL Tsnaps(16,time)
        WRITE (6,100) time
        EXIT
     END IF
  END DO

!...  didnt find snap frame

  IF (isnap == 0) WRITE (6,110) ntsnap,time

!...  aus

100 FORMAT('snapped at t = ',g10.4)
110 FORMAT('frame ',i4,' not found: last was t = ',g10.4)

END PROGRAM Snap
