PROGRAM Pladist

!...  amplitude distributions from p file, binned

  USE Coeff
  USE Pvars
  USE Vars
  USE Plotins

  IMPLICIT NONE

  CHARACTER*64 :: lbl
  INTEGER :: ntmax,ipts

  INTEGER :: ib,jb,nb,ndev
  REAL :: sfloor,kpass,cpass

  INTEGER :: iframe,istr,iu,ieof,ipl
  REAL :: t0,t9,time
  REAL :: tavg,vex,vey,bflx,bfly

  INTEGER :: i,j,j0,mu,npts
  REAL :: xlo,xhi,ylo,yhi,fdevp,fdevm

  INTEGER, PARAMETER :: npics=5
  REAL, DIMENSION(npics) :: favg,fdev
  CHARACTER*8 :: lvars(npics),lvars1(npics)
  DATA lvars/"phi","ne","vor","vex","vey"/
  DATA lvars1/"\gf","n\de","\gW","V\ux","V\uy"/

!...  allocations

  INCLUDE 'pinit.h90'

  nb=51
  ndev=4
  sfloor=-4.
  kpass=0.08

  ntmax=200

  ALLOCATE (fcors(nx0*ny0*ntmax,npics))

  ALLOCATE(cx(nb,npics+2))
  ALLOCATE(yp(nb))

  yp=(/ ((2.0*ndev/nb)*( (ib-1)-(nb-1)/2 ),ib=1,nb) /)

!...  zero everything

  npts=0

  fcors=xzero
  favg=xzero
  fdev=xzero
  cx=xzero

!...  initialise plotting

  CALL Plein("pladist",1,1)

!...  start sampling

  iframe=0
  t0=0.
  DO ipl=1,ntmax
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

     DO i=ix1,ix2

!...  accumulate points

     DO j0=1,ny0
        j=j0+ngdy
        INCLUDE 'vardefp.h90'
        vex=drifty*(puu(muphi,i,j+1)-puu(muphi,i,j-1))
        vey=driftx*(puu(muphi,i+1,j)-puu(muphi,i-1,j))

        npts=npts+1

        fcors(npts,1)=phi
        fcors(npts,2)=ne
        fcors(npts,3)=vor
        fcors(npts,4)=vex
        fcors(npts,5)=vey

     END DO

!...  end loop over lines

     END DO

!...  end plot tasks

     t9=time

  END DO

190 ipl=ipl-1
  IF (ipl == 0) THEN
     WRITE (6,*) 'no data were taken'
     STOP
  END IF

  WRITE (6,100) t0,t9,ipl,npts

!...  find stats

  DO i=1,npts
     favg(:)=favg(:)+fcors(i,:)
  END DO
  favg=favg/npts
  DO i=1,npts
     fdev(:)=fdev(:)+(fcors(i,:)-favg(:))*(fcors(i,:)-favg(:))
  END DO
  fdev=SQRT(fdev/npts)
  DO i=1,npts
     fcors(i,:)=fcors(i,:)/fdev(:)
  END DO

!...  bin it

  DO mu=1,npics
     fdevp=NINT(favg(mu)/fdev(mu))+ndev
     fdevm=NINT(favg(mu)/fdev(mu))-ndev
     DO i=1,npts
        ib=NINT( nb*(fcors(i,mu)-fdevm)/(2*ndev) + 0.5 )
        ib=MAX(ib,1)
        ib=MIN(ib,nb)
        cx(ib,mu)=cx(ib,mu)+1
     END DO
  END DO

  tavg=1.0/(xzero+npts)
  cx=LOG10( cx*tavg )

!...  model distributions -- reset cpass if kpass is not 0.08

  cpass=1./(1.38624*SQRT(pi))
  DO ib=1,nb
     cx(ib,npics+1)=LOG10( (2.0*ndev/nb)*EXP(-yp(ib)*yp(ib)/2.)/SQRT(tpi) )
     cx(ib,npics+2)=LOG10( (2.0*ndev/nb)*cpass/ &
          ( (1.+kpass*yp(ib)*yp(ib))**(1.+0.5/kpass) ) )
  END DO

!...  draw main box

#ifdef PGPLOT
  IF (iptype == 1) THEN
     CALL Pgpage
     CALL Pgsvp(0.,1.,0.,1.)
     CALL Pgswin(0.,1.,0.,1.)
     CALL Pgsci(red)
     CALL Pgbox("bc",0.0,1,"bc",0.0,1)

     CALL Pgsci(black)
     CALL Pgsch(0.7)

     WRITE (lbl,115) t0,t9
     CALL Pgmtxt("t",-1.5,0.95,1.0,lbl)
  END IF
#endif

!...  npics plots

  DO mu=1,npics
     WRITE (6,160) lvars(mu)
     WRITE (6,165) favg(mu),fdev(mu)
!.     WRITE (6,170) (cx(ib,mu),ib=1,nb)
#ifdef PGPLOT
     IF (iptype == 1) THEN
        j=mu/4
        i=mu-1-3*j
        xhi=0.35+i*0.3+j*0.15
        xlo=xhi-0.2
        yhi=0.5+(1-j)*4./9. - 0.08
        ylo=yhi-0.25
        CALL Pgsci(1)
        CALL Pgsls(soli)
        CALL Pgsvp(xlo,xhi,ylo,yhi)
        CALL Pgswin(FLOAT(-ndev),FLOAT(ndev),sfloor,0.)
        CALL Pgbox("bcnt",0.0,1,"bcntv",0.0,1)
        CALL Pglab("A/\gs"," ",lvars1(mu))
        CALL Pgmtxt("lv",2.5,0.875,0.5,"P")
!.        CALL Pgmtxt("t",3.5,0.5,0.5,lvars1(mu))
        CALL Pgsci(red)
        CALL Pgline(nb,yp,cx(1,mu))
        CALL Pgsci(blue)
        CALL Pgsls(dash)
        CALL Pgline(nb,yp,cx(1,6))
        CALL Pgsci(yellow)
        CALL Pgsls(ddot)
        CALL Pgline(nb,yp,cx(1,7))
     END IF
#endif
  END DO

!...  put time

  WRITE (6,100) t0,t9,ipl,npts

!...  write whats in cx

  IF (iptype == 2) THEN
     WRITE (13,113) t0,t9,ipl,npts
     WRITE (13,150) nb,ndev,NINT(sfloor),npics
     WRITE (13,*) 'std dev grid follows'
     WRITE (13,175) (yp(ib),ib=1,nb)
     WRITE (13,*) 'gaussian distribution follows'
     WRITE (13,175) (cx(ib,6),ib=1,nb)
     WRITE (13,*) 'passive scalar distribution follows'
     WRITE (13,175) (cx(ib,7),ib=1,nb)
     DO mu=1,npics
        WRITE (13,160) lvars(mu)
        WRITE (13,165) favg(mu),fdev(mu)
        WRITE (13,175) (cx(ib,mu),ib=1,nb)
     END DO
  END IF

!...  aus

  CALL Plaus

100 FORMAT(" time = ",g9.3," to ",g9.3," with ",i4," plot steps", &
         " and ",i6," points")
113 FORMAT("amplitude distributions over time",/, &
         "t = ",g9.3," -- ",g9.3,/,"   with ",i4," plot steps", &
         " and ",i6," points")
115 FORMAT("t = ",g9.3," -- ",g9.3)

150 FORMAT(4i6,"  bins, devs, floor, and vars")
160 FORMAT("temporal distribution of ",a8)
165 FORMAT("  avg and sdev are",/,2g12.4)
170 FORMAT(21f6.2)
175 FORMAT(6g12.4)

END PROGRAM Pladist
