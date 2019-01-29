PROGRAM Dgdw

!...  HW version -- temporal energy diagnostics

  USE Coeff
  USE Plotins

  IMPLICIT NONE

  INTEGER :: iu,i,its,it0,nt,nstat,igm
  INTEGER :: iesat,idedt,ifluxes
  REAL :: t,t0,t9,tav,gammah,tstat,eh,el

  PARAMETER (nt=100001)
  CHARACTER*48 :: lbl
  REAL, DIMENSION (nt) :: ene=0.,enn=0.,eni=0.,enp=0.,enw=0., &
       ett=0.,time=0.,gamt=0., &
       den=0.,dei=0.,des=0.,dev=0.,dec=0., &
       flxe=0.,flxi=0.
  REAL :: et

  REAL, DIMENSION(:), ALLOCATABLE :: sprobe

!...  allocations

  INCLUDE 'dinit.h90'

  ALLOCATE (sprobe(ny0))

  t0=time0
  t9=time9
  tav=timeav
  iesat=1
  idedt=1
  ifluxes=1
  it0=0

!...  initialise plotting

  CALL Plein("dgdw",1,1)

!...  start sampling

  DO its=1,nt
192  READ (iu,END=190) t
     GOTO 191
190  iu=iu+1
     IF (iu-20 > ndfiles) EXIT
     GOTO 192
191  CONTINUE
     IF (NINT(100*t) > NINT(100*t9)) EXIT
     IF (NINT(100*t) > NINT(100*t0) .AND. it0 == 0) it0=its
     READ (iu) en
!.     IF (t > tprobe) THEN
     IF (NINT(t) > NINT(tprobe)) THEN
        READ (iu) sprobe
        READ (iu) sprobe
     END IF

     time(its)=t

     ene(its)=en(1)
     enn(its)=en(2)
     eni(its)=en(3)

     ett(its)=SUM(en(1:3))
     et=1./(xzero+2.*ett(its))

     enp(its)=en(9)
     enw(its)=en(10)

     des(its)=en(13)*et
     dev(its)=en(14)*et
     dec(its)=en(15)*et

     flxe(its)=en(11)
     flxi(its)=en(12)

!.     WRITE (0,215) its,t

  END DO

  its=its-1

  t0=MAX(t0,2.*time(1)-time(2))
  t9=time(its)
  nstat=MAX(1,MIN(its-2,NINT(tav/(tau0*nstep))))
  tstat=t9-time(its+1-nstat)

!...  do saturated levels

  IF (iesat == 1) THEN

     CALL Estart(t0,t9,tstat)

     igm=0
     CALL Evol(1,'E','e',ene,gamt,time,igm,t0,t9,it0,its,nstat)
     CALL Evol(2,'E','n',enn,gamt,time,igm,t0,t9,it0,its,nstat)
     CALL Evol(3,'E','N',eni,gamt,time,igm,t0,t9,it0,its,nstat)

     CALL Evol(9,'E','T',ett,gamt,time,igm,t0,t9,it0,its,nstat)

     CALL Evol(11,'A','p',enp,gamt,time,igm,t0,t9,it0,its,nstat)
     CALL Evol(12,'A','w',enw,gamt,time,igm,t0,t9,it0,its,nstat)

  END IF

!...  do de/dt terms

  IF (idedt == 1) THEN

     CALL Estart(t0,t9,tstat)

     igm=0
     CALL Evol(1,'G','n',den,gamt,time,igm,t0,t9,it0,its,nstat)
     CALL Evol(2,'G','N',dei,gamt,time,igm,t0,t9,it0,its,nstat)

     CALL Evol(5,'G','s',des,gamt,time,igm,t0,t9,it0,its,nstat)
     CALL Evol(6,'G','c',dec,gamt,time,igm,t0,t9,it0,its,nstat)
     CALL Evol(8,'G','v',dev,gamt,time,igm,t0,t9,it0,its,nstat)

     igm=1
     CALL Evol(9,'G','T',ett,gamt,time,igm,t0,t9,it0,its,nstat)

!...  check energy error growth

     ene=gamt -des +dev+dec

     igm=0
     CALL Evol(10,'G','E',ene,gamt,time,igm,t0,t9,it0,its,nstat)

  END IF

!...  time traces of fluxes

  IF (ifluxes == 1) THEN

     CALL Estart(t0,t9,tstat)

     igm=0
     CALL Evol(1,'F','e',flxe,gamt,time,igm,t0,t9,it0,its,nstat)
     CALL Evol(2,'F','i',flxi,gamt,time,igm,t0,t9,it0,its,nstat)

  END IF

!...  done

  CALL Putparm
  WRITE (6,110) tau0,(time(its)-time(its-1))/nstep
  WRITE (6,115) t0,t9,tstat

!...  aus

  CALL Plaus

110 FORMAT("tau was ",g9.3,", ends at ",g9.3)
115 FORMAT(" time = ",g12.6," to ",g12.6,/, &
         "     stats taken over last dt = ",g12.6)
215 FORMAT("step ",i5," time = ",g14.8)

END PROGRAM Dgdw


SUBROUTINE Putparm

  USE Coeff

  IMPLICIT NONE

  REAL :: x1,x2,y0,dy
  CHARACTER*48 :: lbl

  WRITE (6,200) dpl,rkps,wn

200 FORMAT(" dpl = ",g9.3," K = ",g9.3,/, &
    " wn = ",g9.3)

END SUBROUTINE Putparm


SUBROUTINE Estart(t0,t9,tstat)

  IMPLICIT NONE

  REAL :: t0,t9,tstat
  CHARACTER*48 :: lbl
  INTEGER iptype
  COMMON/pctrl/iptype

#ifdef PGPLOT
  IF (iptype == 1) THEN
     CALL Pgpage
     CALL Pgsls(1)
     CALL Pgsvp(0.,1.,0.,1.)
     CALL Pgswin(0.,1.,0.,1.)
     CALL Pgsci(2)
     CALL Pgbox("bc",0.0,1,"bc",0.0,1)

     CALL Pgsci(1)
     CALL Pgsch(0.7)

     WRITE (lbl,100) t0,t9
     CALL Pgmtxt("b",-1.5,0.15,0.5,lbl)
     WRITE (lbl,110) tstat
     CALL Pgmtxt("b",-1.5,0.35,0.5,lbl)
  END IF
#endif

  IF (iptype == 2) WRITE (13,120) t0,t9,tstat

100 FORMAT("t = ",g9.3," -- ",g9.3)
110 FORMAT("\gDt = ",g9.3)
120 FORMAT("new page",/,"   for time = ",g9.3," -- ",g9.3, &
         " stats over dt = ",g9.3)

END SUBROUTINE Estart


SUBROUTINE Evol(ipg,lchr,pchr,enrg,gamt,time,igm,t0,t9,it0,its,nstat)

  IMPLICIT NONE

  INTEGER :: ipg,igm,it0,its,nstat
  REAL :: t0,t9
  REAL enrg(its),time(its),gamt(its)
  CHARACTER*1 :: lchr,pchr

  CHARACTER*48 :: lbl
  INTEGER :: i,j
  REAL :: dtm,dtp,dtpm
  REAL :: x1,x2,y1,y2
  REAL :: xlo,xhi,ylo,yhi

  REAL :: pi=3.1415927
  INTEGER iptype
  COMMON/pctrl/iptype

!...  get growth rates or energies

  gamt=0.
  IF (igm /= 0) THEN
     DO i=it0+1,its-1
        dtm=time(i)-time(i-1)
        dtp=time(i+1)-time(i)
        dtpm=dtm+dtp
        gamt(i)=(dtm*dtm*(enrg(i+1)-enrg(i)) + &
             dtp*dtp*(enrg(i)-enrg(i-1)))/(dtp*dtm*dtpm)
     END DO
     i=it0
     gamt(i)=2.*gamt(i+1)-gamt(i+2)
     i=its
     gamt(i)=2.*gamt(i-1)-gamt(i-2)

     gamt=gamt/(1.e-30+2.*enrg)
  ELSE
     gamt=enrg
  END IF

!...  draw and label the map

  j=(ipg-1)/4
  i=ipg-1-4*j

  xhi=0.33*j+0.30
  xlo=xhi-0.25
  yhi=1.-0.27*i
  yhi=(1.+7.*yhi)/9.
  ylo=yhi-0.15

  x1=time(it0)
  x2=time(its)
  y1=MIN(0.,MINVAL(gamt))
  y2=MAX(0.,MAXVAL(gamt))

#ifdef PGPLOT
  IF (iptype == 1) THEN
     CALL Pgsch(0.5)

     CALL Pgsci(1)
     CALL Pgsls(1)
     CALL Pgsvp(xlo,xhi,ylo,yhi)
     CALL Pgswin(x1,x2,y1,y2)
     CALL Pgbox("bcnt",0.0,1,"bcntv",0.0,1)

     CALL Pgsci(2)
     CALL Pgline(its-it0+1,time(it0),gamt(it0))

     IF (y1*y2 < 0.) THEN
        CALL Pgsls(3)
        CALL Pgsci(3)
        CALL Line("dash",x1,0.,x2,0.)
     END IF
  END IF
#endif

  IF (iptype == 2) WRITE (13,100) lchr,pchr,i,j

  IF (iptype /= 1) CALL Tracec(pchr,time(it0),gamt(it0),its-it0+1)

  CALL Pstats(lchr,pchr,gamt,its,nstat)

100 FORMAT("plot qty ",2a1,/,2i4)

END SUBROUTINE Evol


SUBROUTINE Pstats(lchr,pchr,enrg,its,nstat)

  CHARACTER*32 lbl
  CHARACTER*1 pchr,lchr
  REAL enrg(its)
  INTEGER iptype
  COMMON/pctrl/iptype

!...  if only one point write it out

  IF (its < 2) THEN
     WRITE (6,115) lchr,pchr,enrg(1)
     RETURN
  END IF

  CALL Stats(enrg,its,nstat,eavg,edev)

  WRITE (6,115) lchr,pchr,eavg,edev
  IF (iptype == 2) WRITE (13,115) lchr,pchr,eavg,edev

#ifdef PGPLOT
  IF (iptype == 1) THEN
     WRITE (lbl,115) lchr,pchr,eavg,edev
     CALL Pgsci(1)
     CALL Pgsch(0.5)
     CALL Pgmtxt("t",1.5,0.5,0.5,lbl)
  END IF
#endif

115 FORMAT(2a1," = ",g10.3," +/- ",g9.3)

END SUBROUTINE Pstats


SUBROUTINE Stats(gamt,its,nstat,gam,dgam)

  REAL gamt(its)

  gam=0.
  dgam=0.
  ni=nstat

  DO i=its-ni,its-1
     gam=gam+gamt(i)
  END DO
  gam=gam/ni
  DO i=its-ni,its-1
     dgam=dgam+(gamt(i)-gam)*(gamt(i)-gam)
  END DO
  dgam=SQRT(dgam/ni)

END SUBROUTINE Stats
