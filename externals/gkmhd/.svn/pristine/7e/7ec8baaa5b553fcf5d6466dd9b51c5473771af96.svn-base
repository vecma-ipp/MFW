PROGRAM Flxpdf

!...  GEM version -- pdf of fluxes

  USE Vars
  USE Pvars
  USE Coeff
  USE FFTs
  USE Plotins

  IMPLICIT NONE

  CHARACTER*64 :: lbl
  INTEGER :: ntmax,ipts

  REAL, DIMENSION(:), ALLOCATABLE :: favg,fdev

  INTEGER :: ib,jb,nb,ndev
  REAL :: sfloor,kpass,cpass

  INTEGER :: iframe,istr,iu,ieof,ipl
  REAL :: t0,t9,time
  REAL :: tavg
  INTEGER :: i0,j0,k0
  REAL :: et,p0,p1,p2,p3,p4
  REAL :: vex

  INTEGER :: i,j,k,l,mu,npts,ix1,ix2
  REAL :: xlo,xhi,ylo,yhi,fdevp,fdevm

  INTEGER :: ncases
  PARAMETER (ncases=1)
  CHARACTER*8 :: lvars(ncases),lvars1(ncases)
  DATA lvars/"F n"/
  DATA lvars1/"F#dn#u"/

!...  allocations

  INCLUDE 'pinit.h90'
  INCLUDE 'fset1.h90'

  ll1=MAX(ll1,1)
  ll2=MIN(ll2,nl)

  ix1=nx0/8
  ix2=nx-ngdx+1-ix1
  ix1=ngdx+ix1

  nb=51
  ndev=6
  sfloor=-6.
  kpass=0.08

  ntmax=2000

  ALLOCATE(pfxa(nx,ncases))

  ALLOCATE (fcors(nx0*ntmax,ncases))
  ALLOCATE (favg(ncases))
  ALLOCATE (fdev(ncases))

  ALLOCATE(cx(nb,ncases+2))
  ALLOCATE(yp(nb))

  yp=(/ ((2.0*ndev/nb)*( (ib-1)-(nb-1)/2 ),ib=1,nb) /)

!...  set FFT

  CALL SCFFT(0,ny0,1.0,rfty,cftky,tg,wk,0)

!...  zero everything

  npts=0

  fcors=xzero
  favg=xzero
  fdev=xzero
  cx=xzero

!...  initialise plotting

  CALL Plein("flxpdf",1,1)

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

!...  get fluxes

     pfxa=0.

     DO i0=1,nx0
        i=i0+ngdx

        DO j0=1,ny0
           j=j0+ngdy

           INCLUDE 'vardef.h90'

           vex=drifty*(uu(j+1,i,1)-uu(j-1,i,1))

           pfxa(i,1)=pfxa(i,1)+ne*vex
        END DO
     END DO

     pfxa=pfxa/(ny0)

!...  accumulate points

     DO i=ix1,ix2
        npts=npts+1

        fcors(npts,:)=pfxa(i,:)
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

  DO mu=1,ncases
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
  cx=ALOG10( cx*tavg )

!...  model distributions -- reset cpass if kpass is not 0.08

  cpass=1./(1.38624*SQRT(pi))
  DO ib=1,nb
     cx(ib,ncases+1)=ALOG10( (2.0*ndev/nb)*EXP(-yp(ib)*yp(ib)/2.)/SQRT(tpi) )
     cx(ib,ncases+2)=ALOG10( (2.0*ndev/nb)*cpass/ &
          ( (1.+kpass*yp(ib)*yp(ib))**(1.+0.5/kpass) ) )
  END DO

!...  draw main box

#ifdef PLPLOT
     IF (iptype == 1) THEN
  CALL Pladv(0)
  CALL Plvpas(0.,1.,0.,1.,512./768.)
  CALL Plwind(0.,1.,0.,1.)
  CALL Plcol(1)
  CALL Plbox("bc",0.0,1,"bc",0.0,1)

  CALL Plcol(15)
  CALL Plschr(0.,0.7)

  WRITE (lbl,115) t0,t9
  CALL Plmtex("b",-0.7,0.95,1.0,lbl)
     END IF
#endif

!...  ncases plots

  DO mu=1,ncases
     WRITE (6,160) lvars(mu)
     WRITE (6,165) favg(mu),fdev(mu)
!.     WRITE (6,170) (cx(ib,mu),ib=1,nb)
     IF (iptype == 1) THEN
#ifdef PLPLOT
     j=mu/4
     i=mu-1-3*j
     xhi=0.35+i*0.3+j*0.15
     xlo=xhi-0.2
     yhi=0.5+(1-j)*4./9. - 0.08
     ylo=yhi-0.25
     CALL Plcol(15)
     CALL Pllsty(soli)
     CALL Plvpor(xlo,xhi,ylo,yhi)
     CALL Plwind(FLOAT(-ndev),FLOAT(ndev),sfloor,0.)
     CALL Plbox("bcnt",0.0,1,"bcntv",0.0,1)
     CALL Pllab("A/#gs","P",lvars1(mu))
     WRITE (lbl,120) fdev(mu)
     CALL Plmtex("t",3.5,0.5,0.5,lbl)
!.     CALL Plmtex("t",3.5,0.5,0.5,lvars1(mu))
     CALL Plcol(red)
     CALL Plline(nb,yp,cx(1,mu))
     CALL Plcol(blue)
     CALL Pllsty(dash)
     CALL Plline(nb,yp,cx(1,ncases+1))
     CALL Plcol(yellow)
     CALL Pllsty(ddot)
     CALL Plline(nb,yp,cx(1,ncases+2))
#endif
     END IF
  END DO

!...  put time

  WRITE (6,100) t0,t9,ipl,npts

!...  write whats in cx

  IF (iptype == 2) THEN
     WRITE (13,113) t0,t9,ipl,npts
     WRITE (13,150) nb,ndev,NINT(sfloor),ncases
     WRITE (13,*) 'std dev grid follows'
     WRITE (13,175) (yp(ib),ib=1,nb)
     DO mu=1,ncases
        WRITE (13,160) lvars(mu)
        WRITE (13,165) favg(mu),fdev(mu)
        WRITE (13,175) (cx(ib,mu),ib=1,nb)
     END DO
  END IF

!...  aus

  CALL Plaus

100 FORMAT(" time = ",g9.3," to ",g9.3," with ",i4," plot steps", &
         " and ",i6," points")
113 FORMAT("distributions over time",/, &
         "t = ",g9.3," -- ",g9.3,/,"   with ",i4," plot steps", &
         " and ",i6," points")
115 FORMAT("t = ",g9.3," -- ",g9.3)
120 FORMAT("#gs = ",g9.3)

150 FORMAT(4i6,"  bins, devs, floor, and vars")
160 FORMAT("temporal distribution of ",a8)
165 FORMAT("  avg and sdev are",/,2g12.4)
170 FORMAT(21f6.2)
175 FORMAT(6g12.4)

END PROGRAM Flxpdf
