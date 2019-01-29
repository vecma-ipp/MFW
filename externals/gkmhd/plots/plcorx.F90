PROGRAM Plcorx

!...  radial correlation from p file data

  USE Coeff
  USE Pvars
  USE Vars
  USE FFT, ONLY : four1d_real
  USE Plotins

  IMPLICIT NONE

  INTEGER :: iu,i,i0,j,l,mu,ipl,iframe,istr,ieof
  REAL :: time,t0,t9
  REAL :: vex,vey

  CHARACTER*32 :: lbl
  INTEGER :: idx
  REAL :: fn1,fn2,pe1,lcorp,lcorm,rkx0
  REAL :: ymax,p10,p01
  INTEGER :: ntkex,lp10

  INTEGER, PARAMETER :: npics=5
  CHARACTER*8 :: lvars(npics),lvars1(npics)
  DATA lvars/"phi","ne","vor","vex","vey"/
  DATA lvars1/"\gf","n\de","\gW","V\ux","V\uy"/

!...  allocations

  INCLUDE 'pinit.h90'

  rkx0=tpi/(hx*nx0)

  IF (ivcor1 == 0) ivcor1=2
  IF (ivcor2 == 0) ivcor2=2
  ll1=MAX(ll1,1)
  ll2=MIN(ll2,nk)

  ALLOCATE(uup(nx0,npics))
  ALLOCATE(uuky(0:nk,npics))

  ALLOCATE(fcor(-nx0:nx0))
  ALLOCATE(xcor(-nx0:nx0))

!...  zero correlation function and set correlation grid

  dvp0=1.-pdf
  IF (ibcx == 0) dvp0=1.

  xcor=(/ (hx*idx, idx=-nx0,nx0) /)
  fcor=0.
  fn1=0.
  fn2=0.

  pe1=EXP(-1.0)

!...  initialise plotting

  CALL Plein("plcorx",1,1)

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

!...  get pvars

     CALL Strip

!...  loop over lines

     DO j=1+ngdy,ny0+ngdy

!...  extract probe data

     DO i0=1,nx0
        i=i0+ngdx
        INCLUDE 'vardefp.h90'
        vex=drifty*(puu(muphi,i,j+1)-puu(muphi,i,j-1))
        vey=driftx*(puu(muphi,i+1,j)-puu(muphi,i-1,j))

        uup(i0,1)=phi*dvp0(i)
        uup(i0,2)=ne*dvp0(i)
        uup(i0,3)=vor*dvp0(i)
        uup(i0,4)=vex*dvp0(i)
        uup(i0,5)=vey*dvp0(i)
     END DO

!...  go to kx space

     DO mu=1,npics
        CALL four1d_real(uup(1:nx0,mu), uuky(:,mu), -1)
     END DO
     uuky=uuky/REAL(nx0)

!...  accumulate correlation function

     DO l=ll1,ll2
        DO idx=-nx0,nx0
           fcor(idx)=fcor(idx)+ &
                REAL( CONJG(uuky(l,ivcor2))*uuky(l,ivcor1)* &
                CEXP(CMPLX(0.,(rkx0*l*xcor(idx)))) )
        END DO
        fn1=fn1+REAL( CONJG(uuky(l,ivcor1))*uuky(l,ivcor1) )
        fn2=fn2+REAL( CONJG(uuky(l,ivcor2))*uuky(l,ivcor2) )
     END DO

!...  end loop over lines

     END DO

!...  end plot tasks

     t9=time

  END DO

190 ipl=ipl-1

  WRITE (6,100) lvars(ivcor2),lvars(ivcor1)
  WRITE (6,110) t0,t9,ipl

!...  normalise

!.  fcor=fcor/MAXVAL(fcor)
  fcor=fcor/SQRT(fn1*fn2)

!...  find correlation lengths

  j=0
  DO WHILE ( (fcor(j+1)-pe1)*(fcor(j)-pe1) > 0.)
     j=j+1
  END DO
  fn1=fcor(j)-pe1
  fn2=fcor(j+1)-pe1
  lcorp=(ABS(fn1)*xcor(j+1)+ABS(fn2)*xcor(j))/ABS(fn2-fn1)
  WRITE (6,*) 'Lx = ',lcorp

  j=0
  DO WHILE ( (fcor(j+1)-pe1)*(fcor(j)-pe1) > 0.)
     j=j-1
  END DO
  fn1=fcor(j)-pe1
  fn2=fcor(j+1)-pe1
  lcorm=(ABS(fn1)*xcor(j+1)+ABS(fn2)*xcor(j))/ABS(fn2-fn1)
  WRITE (6,*) 'Lx = ',lcorm

!...  plot correlation function

  ymax=tpi/rkx0
  lp10=FLOOR(LOG10(ymax))
  p10=10.**lp10
  p01=1./p10
  ntkex=CEILING(p01*ymax)
  ymax=NINT(p10*ntkex)

#ifdef PGPLOT
  IF (iptype == 1) THEN

     ymax=40.

     CALL Pgsch(0.7)

     CALL Pgsci(1)
     CALL Pgsls(1)
     CALL Pgenv(-ymax,ymax,-1.,1.,0,0)
     CALL Pglab("\gDx"," "," ")

     WRITE (lbl,115) t0,t9
     CALL Pgmtxt("b",3.5,0.80,0.5,lbl)

     CALL Pgsch(1.0)

     WRITE (lbl,121) lvars1(ivcor2)
     CALL Pgmtxt("t",1.5,0.48,1.0,lbl)
     WRITE (lbl,122) lvars1(ivcor1)
     CALL Pgmtxt("t",1.5,0.5,0.0,lbl)

     CALL Pgsci(red)
     CALL Pgline(2*nx0+1,xcor,fcor)

     CALL Pgsci(blue)
     CALL Pgsls(dash)
     CALL Line("dash",-ymax,0.,ymax,0.)
     CALL Line("dash",0.,1.0,0.,-1.0)

     CALL Pgsci(green)
     CALL Pgsls(ddot)
     CALL Line("ddot",-ymax,pe1,ymax,pe1)

     CALL Line("ddot",lcorp,-1.,lcorp,1.)
     CALL Line("ddot",lcorm,-1.,lcorm,1.)
     CALL Pgsch(0.7)
     CALL Pgsci(black)
     CALL Pgsls(soli)
     WRITE (lbl,125) lcorm,lcorp
     CALL Pgmtxt("b",3.5,0.25,0.5,lbl)

  END IF
#endif

  IF (iptype == 2) THEN
     WRITE (13,112) ivcor1,ivcor2
     WRITE (13,113) t0,t9
     WRITE (13,*) -ymax,ymax,2*ntkex
  END IF

  IF (iptype /= 1) THEN
     CALL Line("dash",-ymax,0.,ymax,0.)
     CALL Line("dash",0.,1.0,0.,-1.0)
     CALL Poutpg("soli",xcor,fcor,2*nx0+1)
  END IF

!...  aus

  CALL Plaus

100 FORMAT("Fcorx of ",a8,"against ",a8)
110 FORMAT(" time = ",g9.3," to ",g9.3," with ",i4," plot steps")
112 FORMAT(" x correlation, var 1    var 9",/,2i4)
113 FORMAT(" t = ",g9.3," -- ",g9.3)
115 FORMAT("t = ",g9.3," -- ",g9.3)
121 FORMAT("Fcorx of ",a8)
122 FORMAT("against ",a8)
125 FORMAT("Lcor =",2g12.4)

END PROGRAM Plcorx
