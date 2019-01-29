PROGRAM Dgsplit

!...  splits part of a corrupted d file into a coherent whole
!...  three dimensional version

  USE Pvars
  USE Coeff
  USE Plotins

  IMPLICIT NONE

  INTEGER :: nt,nxny
  PARAMETER (nt=100001)

  INTEGER :: its,iu,ieof,iframe,ifile,j,iflag
  REAL :: time,timel
  CHARACTER*8 :: lbl

!...  allocations

  INCLUDE 'sinit.h90'

  ALLOCATE(en(nen))
  ALLOCATE(uup(ny0,5))

!...  open d files

  CALL Dfopen(iu,ndfiles)

  ifile=1

  write (lbl,111) ifile

  OPEN (16,file=lbl,form='unformatted')
  WRITE (6,110) lbl

!...  accumulate

  iflag=0
  timel=-1.

  iframe=0
  DO its=1,nt
191  iframe=iframe+1
     READ (iu,END=199) time
     GOTO 198
199  iu=iu+1
     IF (iu-20 > ndfiles) GOTO 190
     GOTO 191
198  CONTINUE
     READ (iu) en
     IF (NINT(time) > NINT(tprobe)) THEN
        READ (iu) (uup(j,1),j=1,ny0)
        READ (iu) (uup(j,2),j=1,ny0)
     END IF

!...  time check

     IF (NINT(1000*time) < NINT(1000*time0)) GOTO 191

     IF (NINT(1000*time) > NINT(1000*time9)) GOTO 190

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

     WRITE (16) time
     WRITE (16) en
     IF (NINT(time) > NINT(tprobe)) THEN
        WRITE (16) (uup(j,1),j=1,ny0)
        WRITE (16) (uup(j,2),j=1,ny0)
     END IF

     WRITE (6,100) time

  END DO
190 CONTINUE

!...  aus

100 FORMAT('  snapped at t = ',g12.6)
110 FORMAT('file ',a8,' opened:')
111 FORMAT('d10',i1,'.dat')
112 FORMAT('d1',i2,'.dat')

END PROGRAM Dgsplit
