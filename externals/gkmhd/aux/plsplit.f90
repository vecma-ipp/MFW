PROGRAM Plsplit

!...  splits a p file into pieces for storage in tight environments
!...  two dimensional version

  USE Vars
  USE Coeff
  USE Plotins

  IMPLICIT NONE

  INTEGER :: ifl,ifile,ipl,iu,ieof,iframe,iflag
  INTEGER :: j,i,mu
  REAL :: time,timel
  CHARACTER*8 :: lbl

!...  allocations

  INCLUDE 'sinit.h90'

  ALLOCATE(uu(ny,nx,nvars))

!...  open p files

  CALL Pfopen(iu,nfiles)

!...  search for snap frame

  iflag=0
  timel=-1.

  ifile=0

  iframe=0

  DO ifl=1,10000

     ifile=ifile+1

     IF (ifile < 10) write (lbl,111) ifile
     IF (ifile >= 10) write (lbl,112) ifile
111  FORMAT('p10',i1,'.dat')
112  FORMAT('p1',i2,'.dat')
     OPEN (16,file=lbl,form='unformatted')
     WRITE (6,110) lbl

     DO ipl=1,nfsnap

191     iframe=iframe+1

192     CONTINUE

        CALL Psnapsin(iu,time,ieof)

        IF (ieof /= 0) THEN
           iu=iu+1
           IF (iu-20 > nfiles) THEN
              CLOSE (16)
              GOTO 199
           END IF
           GOTO 192
        END IF

!...  time check

        IF (NINT(time) < ntstart .OR. iframe < nfstart) GOTO 191
        IF (NINT(time) > ntstop .AND. iframe > nfstop) GOTO 199

        IF (time < timel) THEN
           CLOSE (16)
           ifile=ifile+1
           IF (ifile < 10) write (lbl,111) ifile
           IF (ifile >= 10) write (lbl,112) ifile
           OPEN (16,file=lbl,form='unformatted')
           WRITE (6,110) lbl
        END IF

        IF (time == timel) CYCLE

        timel=time

!...  do the snap

        CALL Snapp(16,time)
        WRITE (6,100) time

     END DO
     CLOSE (16)
  END DO
199 CONTINUE

!...  aus

100 FORMAT('  snapped at t = ',g10.4)
110 FORMAT('file ',a8,' opened:')

END PROGRAM Plsplit


SUBROUTINE Snapp(iu,time)

  USE Vars

  IMPLICIT NONE

  INTEGER :: iu
  REAL :: time

  INTEGER :: i,j,mu

  WRITE (iu) time

  DO mu=1,nvsnap
     WRITE (iu) ((uu(j,i,mu),j=1,ny),i=1,nx)
  END DO

END SUBROUTINE Snapp
