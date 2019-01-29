PROGRAM Plreyn

!...  Reynolds stress spectra for HW
!...  plus/minus plotting

  USE Coeff
  USE Pvars
  USE Vars
  USE Varsfl
  USE Plotins

  IMPLICIT NONE

  CHARACTER*32 :: lbl
  COMPLEX ::  vexfl,veyfl
  REAL, DIMENSION (:), ALLOCATABLE :: rre,rme,rediag

  INTEGER :: i,j,l,iu,ieof,iframe,ipl,istr,io,ip,im
  REAL :: xx0,xx9,time,et,t0,t9,hx0,aspec0,aspec
  REAL :: et1,et2,et3,et4,et5,et6,et7,et8,et9,et10
  REAL :: et11,et12,et13,et14,et15,et16,et17,et18,et19,et20
  REAL :: et21,et22,et23,et24,et25,et26,et27,et28,et29,et30
  REAL :: tavg,rky1,rky2,rky5
  REAL :: exmax,exmin,eemax,eemin,p10,p01,rky0
  INTEGER :: k1,k9,ntkex,ntkee,lp10

!...  allocations

  INCLUDE 'pinitfl.h90'

  ALLOCATE(xp(nx))
  ALLOCATE(rkyl(nl))

  rky0=rkps

  xp=xx
  rkyl=LOG10((/ (l*rky0,l=1,nl) /))

  ALLOCATE(rre(nl))
  ALLOCATE(rme(nl))

  ALLOCATE(rediag(nl))

  ALLOCATE(pfx(nx))

!...  zero everything

     rre=0.

!...  initialise plotting

  CALL Plein("plreyn",2,2)

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

!...  time check

     IF (NINT(time) < ntstart .OR. iframe < nfstart) GOTO 191
     IF (NINT(time) > ntstop .AND. iframe > nfstop) GOTO 190
     IF (ipl == 1) t0=time

!...  fluctuation energies

#include <getrfl.h90>

!...  end plot tasks

     t9=time

  END DO

190 ipl=ipl-1

  WRITE (6,100) t0,t9,ipl

!...  get time averages

  tavg=1./(xzero+ipl)

  rre=rre*tavg
  rme=rme*tavg

!...  write 50 percent drive peak

  rediag=MAX(0.,rre)

  tavg=SUM(rediag)
  et1=0.
  et2=0.
  et5=0.

  IF (tavg > 0.) THEN

  DO l=ll1,ll2
     et1=et1+rediag(l)
     IF (et1 > 0.1*tavg) THEN
        WRITE (6,*) '10 percent RE peak is mode ',l
        rky1=rky0*l
        EXIT
     END IF
  END DO

  DO l=ll1,ll2
     et2=et2+rediag(l)
     IF (et2 > 0.2*tavg) THEN
        WRITE (6,*) '20 percent RE peak is mode ',l
        rky2=rky0*l
        EXIT
     END IF
  END DO

  DO l=ll1,ll2
     et5=et5+rediag(l)
     IF (et5 > 0.5*tavg) THEN
        WRITE (6,*) '50 percent RE peak is mode ',l
        rky5=rky0*l
        EXIT
     END IF
  END DO

  ELSE

     rky1=rky0*nl
     rky2=rky0*nl
     rky5=rky0*nl

  END IF

!...  plot energetics

#include <getrspec.h90>

!...  put time

#ifdef PGPLOT
  IF (iptype == 1) THEN
     CALL Pgsci(black)
     WRITE (lbl,115) t0,t9
     CALL Pgmtxt("b",4.0,1.0,1.0,lbl)
  END IF
#endif

  IF (iptype == 2) WRITE (13,115) t0,t9

  WRITE (6,100) t0,t9,ipl

!...  aus

  CALL Plaus

100 FORMAT(" time = ",g9.3," to ",g9.3," with ",i4," plot steps")
115 FORMAT("t = ",g9.3," -- ",g9.3)
135 FORMAT(2i4)
136 FORMAT("quit")

END PROGRAM Plreyn
