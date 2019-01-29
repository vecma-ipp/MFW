PROGRAM Pltime

!...  finds time steps of a p file

  USE Vars
  USE Coeff
  USE Plotins

  IMPLICIT NONE

  CHARACTER*32 :: lbl

  INTEGER :: i,j,l,iu,ieof,iframe,ipl,istr,io
  REAL :: time,t0,t9

!...  open h files

  OPEN (12,file='hpl.dat',form='formatted',status='old')
  OPEN (15,file='hdw.dat',form='formatted',status='old')

!...  get parms, options

  READ (15,parm)
  READ (12,plotin)
  CALL Plein

!...  open p files

  CALL Pfopen(iu,nfiles)

!...  start sampling

  iframe=0
  t0=0.
  DO ipl=1,10000
191  iframe=iframe+1

!...  read snapshot

     DO istr=1,istride
192     CONTINUE
        CALL Psnapsin(iu,time,ieof)
        IF (ieof .ne. 0) GOTO 199
        GOTO 198
199     iu=iu+1
        IF (iu-20 .gt. nfiles) GOTO 190
        GOTO 192
198     CONTINUE
     END DO

!...  put time

     WRITE (6,110) time

  END DO
190 ipl=ipl-1

!...  aus

  CALL Plaus

110 FORMAT(" time = ",g12.6)

END PROGRAM Pltime
