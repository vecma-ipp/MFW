PROGRAM Plcory_probes

!...  drift-direction correlation from probe data

  USE Pvars
  USE Coeff
  USE FFTs
  USE Plotins

  IMPLICIT NONE

  INTEGER :: iu,i,j,l,mu,its,iframe,istr,ieof
  REAL :: time,t0,t9

  CHARACTER*32 :: lbl
  INTEGER :: idy
  REAL :: fn1,fn2,pe1,lcorp,lcorm
  REAL :: ymax,p10,p01
  INTEGER :: ntkex,lp10

  CHARACTER*8 :: lvars(2),lvars1(2)
  DATA lvars/"phi","ne"/
  DATA lvars1/"#gf","n#de"/

!...  allocations

  INCLUDE 'dinitfl.h90'

  IF (ivcor1 == 0) ivcor1=2
  IF (ivcor2 == 0) ivcor2=2
  ll1=MAX(ll1,1)
  ll2=MIN(ll2,nl)

  ALLOCATE(uup(ny0,2))
  ALLOCATE(uuky(nl,2))

  ALLOCATE(fcor(-ny0:ny0))
  ALLOCATE(ycor(-ny0:ny0))

!...  initialise FFTs

  CALL SCFFT(0,ny0,1.0,rfty,cftky,tg,wk,0)

!...  zero correlation function and set correlation grid

  ycor=(/ (hy*idy, idy=-ny0,ny0) /)
  fcor=0.
  fn1=0.
  fn2=0.

!...  initialise plotting

  CALL Plein("plcory",1,1)

!...  start sampling

  t0=0.
  DO its=1,100000

!...  read probes

192  READ (iu,END=190) time
     GOTO 191
190  iu=iu+1
     IF (iu-20 > ndfiles) EXIT
     GOTO 192
191  CONTINUE
     IF (time > time9) EXIT
     IF (NINT(time) > ntstop) EXIT
     READ (iu) en
     IF (NINT(time) > NINT(tprobe)) THEN
!.     IF (time > tprobe) THEN
        READ (iu) (uup(j,1),j=1,ny0)
        READ (iu) (uup(j,2),j=1,ny0)
     ELSE
        GOTO 192
     END IF
     IF (time < time0) GOTO 192
     IF (NINT(time) < ntstart) GOTO 192
     IF (its == 1) t0=time

!...  go to ky space

     DO mu=1,2
        rfty=uup(:,mu)
        CALL SCFFT(-1,ny0,1.0/ny0,rfty,cftky,tg,wk,0)
        uuky(1:nl,mu)=cftky(1:nl)
     END DO

!...  accumulate correlation function

     DO l=ll1,ll2
        DO idy=-ny0,ny0
           fcor(idy)=fcor(idy)+ &
                REAL( CONJG(uuky(l,ivcor2))*uuky(l,ivcor1)* &
                CEXP(CMPLX(0.,(rkps*l*ycor(idy)))) )
        END DO
        fn1=fn1+REAL( CONJG(uuky(l,ivcor1))*uuky(l,ivcor1) )
        fn2=fn2+REAL( CONJG(uuky(l,ivcor2))*uuky(l,ivcor2) )
     END DO

!...  end plot tasks

     t9=time

  END DO

  its=its-1
  IF (its == 0) THEN
     WRITE (6,*) 'no data were taken'
     STOP
  END IF

  WRITE (6,100) lvars(ivcor2),lvars(ivcor1)
  WRITE (6,110) t0,t9,its

!...  normalise

!.  fcor=fcor/MAXVAL(fcor)
  fcor=fcor/SQRT(fn1*fn2)

!...  find correlation lengths

  pe1=EXP(-1.0)

  j=0
  DO WHILE ( (fcor(j+1)-pe1)*(fcor(j)-pe1) > 0.)
     j=j+1
  END DO
  fn1=fcor(j)-pe1
  fn2=fcor(j+1)-pe1
  lcorp=(ABS(fn1)*ycor(j+1)+ABS(fn2)*ycor(j))/ABS(fn2-fn1)
  WRITE (6,*) 'Ly = ',lcorp

  j=0
  DO WHILE ( (fcor(j+1)-pe1)*(fcor(j)-pe1) > 0.)
     j=j-1
  END DO
  fn1=fcor(j)-pe1
  fn2=fcor(j+1)-pe1
  lcorm=(ABS(fn1)*ycor(j+1)+ABS(fn2)*ycor(j))/ABS(fn2-fn1)
  WRITE (6,*) 'Ly = ',lcorm

!...  plot correlation function

  ymax=tpi/rkps
  p10=1.e10
  lp10=IFIX(alog10(p10*ymax))-10
  p10=10.**lp10
  p01=1./p10
  ntkex=IFIX(p01*ymax)+1
  lp10=NINT(p10*ntkex)
  ymax=lp10

#ifdef PLPLOT
     IF (iptype == 1) THEN
  CALL Plschr(0.,0.7)

  ymax=40.

  CALL Plcol(15)
  CALL Pllsty(soli)
  CALL Plenv(-ymax,ymax,-1.,1.,0,0)
  CALL Pllab("#gDy"," "," ")

  WRITE (lbl,115) t0,t9
  CALL Plmtex("b",3.5,0.80,0.5,lbl)

  CALL Plschr(0.,1.0)

  WRITE (lbl,121) lvars1(ivcor2)
  CALL Plmtex("t",1.5,0.48,1.0,lbl)
  WRITE (lbl,122) lvars1(ivcor1)
  CALL Plmtex("t",1.5,0.5,0.0,lbl)

  CALL Plcol(red)
  CALL Plline(2*ny0+1,ycor,fcor)

  CALL Plcol(blue)
  CALL Pllsty(dash)
  CALL Pljoin(-ymax,0.,ymax,0.)
  CALL Pljoin(0.,1.0,0.,-1.0)

  CALL Plcol(yellow)
  CALL Pllsty(ddot)
  CALL Pljoin(-ymax,pe1,ymax,pe1)

  CALL Pljoin(lcorp,-1.,lcorp,1.)
  CALL Pljoin(lcorm,-1.,lcorm,1.)
  CALL Plschr(0.,0.7)
  CALL Plcol(black)
  CALL Pllsty(soli)
  WRITE (lbl,125) lcorm,lcorp
  CALL Plmtex("b",3.5,0.25,0.5,lbl)

     END IF
#endif

  IF (iptype == 2) THEN
     WRITE (13,112) ivcor1,ivcor2
     WRITE (13,113) t0,t9
     WRITE (13,135) -lp10,lp10,2*ntkex
  END IF

  IF (iptype /= 1) THEN
     CALL Line("dash",-ymax,0.,ymax,0.)
     CALL Line("dash",0.,1.0,0.,-1.0)
     CALL Poutpg("soli",ycor,fcor,2*ny0+1)
  END IF

!...  aus

  CALL Plaus

100 FORMAT("Fcory of ",a6,"against ",a6)
110 FORMAT(" time = ",g9.3," to ",g9.3," with ",i4," plot steps")
112 FORMAT(" y correlation, var 1    var 9",/,2i4)
113 FORMAT(" t = ",g9.3," -- ",g9.3)
115 FORMAT("t = ",g9.3," -- ",g9.3)
121 FORMAT("Fcory of ",a8)
122 FORMAT("against ",a8)
125 FORMAT("Lcor =",2g12.4)
135 FORMAT(2i4)

END PROGRAM Plcory_probes
