PROGRAM Plsnap1

!...  fluctuation spectra 
!...  plots each frame, not average

  USE Coeff
  USE Pvars
  USE Vars
  USE Varsfl
  USE Plotins

  IMPLICIT NONE

  CHARACTER*32 :: lbl
  REAL, DIMENSION (:), ALLOCATABLE :: ene,enn,enp,enw,enh, &
       den,dei,dec,dev
  REAL, DIMENSION (:), ALLOCATABLE :: exne,exnn,exnp,exnw,exnh, &
       dxn,dxi,dxc,dxv

  INTEGER :: i,j,l,iu,ieof,iframe,ipl,istr,io,ip,im,lp
  REAL :: xx0,xx9,time,et,t0,t9,hx0,aspec0,aspec
  REAL :: et1,et2,et3,et4,et5,et6,et7,et8,et9,et10
  REAL :: et11,et12,et13,et14,et15,et16,et17,et18,et19,et20
  REAL :: et21,et22,et23,et24,et25,et26,et27,et28,et29,et30
  REAL :: tavg,rky12
  REAL :: exmax,exmin,eemax,eemin,p10,p01,rky0
  INTEGER :: k1,k9,ntkex,ntkee,lp10

!...  allocations

  INCLUDE 'pinitfl.h90'

  ALLOCATE(xp(nx0))
  ALLOCATE(rkyl(nl))

  rky0=rkps

  xp=ra(1+ngdx:nx0+ngdx)/delta
  rkyl=LOG10((/ (l*rky0,l=1,nl) /))

  ALLOCATE(ene(nl))
  ALLOCATE(enn(nl))
  ALLOCATE(enp(nl))
  ALLOCATE(enw(nl))
  ALLOCATE(enh(nl))
  ALLOCATE(den(nl))
  ALLOCATE(dei(nl))
  ALLOCATE(dec(nl))
  ALLOCATE(dev(nl))

  ALLOCATE(exne(nx0))
  ALLOCATE(exnn(nx0))
  ALLOCATE(exnp(nx0))
  ALLOCATE(exnw(nx0))
  ALLOCATE(exnh(nx0))
  ALLOCATE(dxn(nx0))
  ALLOCATE(dxi(nx0))
  ALLOCATE(dxc(nx0))
  ALLOCATE(dxv(nx0))

!...  initialise plotting

  CALL Plein("pldw",2,1)

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

!...  zero everything

     ene=0.
     enn=0.
     enp=0.
     enw=0.
     enh=0.
     den=0.
     dei=0.
     dec=0.
     dev=0.
     exne=0.
     exnn=0.
     exnp=0.
     exnw=0.
     exnh=0.
     dxn=0.
     dxi=0.
     dxc=0.
     dxv=0.

!...  fluctuation energies

#include <getefl.h90>

!...  plot energetics

!...  tell IDL not to quit

  IF (iptype == 2) WRITE (13,*) "cont"

!...  energy spectra only

  eemax=MAX(MAXVAL(enp),MAXVAL(enn),100.,xzero)
  lp10=CEILING(LOG10(eemax))
  eemax=lp10
  eemin=eemax-8.
  ntkee=8
  xx0=FLOOR(MINVAL(rkyl))
  xx9=CEILING(MAXVAL(rkyl))

#ifdef PGPLOT
  IF (iptype == 1) THEN
     CALL Pgsci(black)
     CALL Pgsls(1)
     CALL Pgenv(xx0,xx9,eemin,eemax,0,30)
     CALL Pglab("ky","A","E spectra")
  END IF
#endif

  IF (iptype == 2) THEN
     WRITE (13,135) NINT(xx0),NINT(xx9)
     WRITE (13,135) lp10
  END IF

  CALL Plspec(1,1,blue,dash,"e",nl,rkyl,ene,xx0,xx9,eemin,eemax)
  CALL Plspec(1,2,red,soli,"n",nl,rkyl,enn,xx0,xx9,eemin,eemax)
  CALL Plspec(1,4,blue,soli,"p",nl,rkyl,enp,xx0,xx9,eemin,eemax)
  CALL Plspec(1,5,blue,ddot,"w",nl,rkyl,enw,xx0,xx9,eemin,eemax)
  CALL Plspec(1,6,green,ddot,"H",nl,rkyl,enh,xx0,xx9,eemin,eemax)

  IF (iptype == 2) WRITE (13,136)

!...  put time

#ifdef PGPLOT
  IF (iptype == 1) THEN
     CALL Pgsci(black)
     WRITE (lbl,115) time
     CALL Pgmtxt("b",4.0,1.0,1.0,lbl)
  END IF
#endif

  IF (iptype == 2) WRITE (13,115) time

  WRITE (6,110) time

!...  end plot tasks

     t9=time

  END DO

190 ipl=ipl-1

  WRITE (6,100) t0,t9,ipl

!...  aus

  CALL Plaus

100 FORMAT(" time = ",g9.3," to ",g9.3," with ",i4," plot steps")
110 FORMAT(" time = ",g10.4)
115 FORMAT("t = ",g10.4)
135 FORMAT(2i4)
136 FORMAT("quit")

END PROGRAM Plsnap1
