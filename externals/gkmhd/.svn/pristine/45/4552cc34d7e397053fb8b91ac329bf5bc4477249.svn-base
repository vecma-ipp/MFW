PROGRAM Plcoher_probes

!...  inter-variable coherence from probe data, scatter plot

  USE Pvars
  USE Coeff
  USE Plotins

  IMPLICIT NONE

  CHARACTER*64 :: lbl
  INTEGER :: ntmax,ipts

  REAL :: favg(2),fdev(2)

  INTEGER :: ib,jb,nb,ndev

  INTEGER :: iframe,istr,iu,ieof,its
  REAL :: t0,t9,time
  REAL :: tavg

  INTEGER :: i,j,mu,npts
  REAL :: x1,x2,y1,y2
  REAL :: p0

  CHARACTER*8 :: lvars(2),lvars1(2)
  DATA lvars/"phi","ne"/
  DATA lvars1/"#gf","n#de"/

  REAL :: zc(4)
  INTEGER :: lcolors(4)
  DATA zc/ 0.1, 0.37, 0.5, 0.8 /
  DATA lcolors/ 2, 9, 3, 1 /

!...  allocations

  INCLUDE 'dinit.h90'

  IF (ivcor1 == 0) ivcor1=1
  IF (ivcor2 == 0) ivcor2=2

  nb=51
  ndev=4

  ntmax=10000

  ALLOCATE(uup(ny0,2))

  ALLOCATE (fcors(ny0*ntmax,2))

  ALLOCATE(cx(nb,nb))
  ALLOCATE(yp(nb))

  yp=(/ ((2.0*ndev/nb)*( (ib-1)-(nb-1)/2 ),ib=1,nb) /)

!...  zero everything

  npts=0

  fcors=xzero
  favg=xzero
  fdev=xzero
  cx=xzero

!...  initialise plotting

  CALL Plein("plcoher",1,1)

!...  start sampling

  t0=0.
  DO its=1,ntmax

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

!...  filter profiles and accumulate points

     DO mu=1,2
        p0=SUM(uup(:,mu))/ny0
	uup(:,mu)=uup(:,mu)-p0
     END DO
     DO j=1,ny0
        npts=npts+1
        DO mu=1,2
           fcors(npts,mu)=uup(j,mu)
        END DO
     END DO

!...  end plot tasks

     t9=time

  END DO

  its=its-1

  WRITE (6,100) lvars(ivcor2),lvars(ivcor1),npts
  WRITE (6,110) t0,t9,its

!...  find stats

  DO i=1,npts
     favg(:)=favg(:)+fcors(i,:)
  END DO
  favg=favg/npts
  DO i=1,npts
     fcors(i,:)=fcors(i,:)-favg(:)
     fdev(:)=fdev(:)+fcors(i,:)*fcors(i,:)
  END DO
  fdev=SQRT(fdev/npts)
  DO i=1,npts
     fcors(i,:)=fcors(i,:)/fdev(:)
  END DO

!...  bin it

  DO i=1,npts
     ib=NINT( nb*(fcors(i,ivcor1)+ndev)/(2*ndev) + 0.5 )
     ib=MAX(ib,1)
     ib=MIN(ib,nb)
     jb=NINT( nb*(fcors(i,ivcor2)+ndev)/(2*ndev) + 0.5 )
     jb=MAX(jb,1)
     jb=MIN(jb,nb)
     cx(ib,jb)=cx(ib,jb)+1
  END DO

  tavg=1.0*nb/(ndev*npts+xzero)
!.  cx=ALOG10( cx*tavg )
  cx=cx*tavg

!...  plot coherence

#ifdef PLPLOT
     IF (iptype == 1) THEN
  x1=-FLOAT(ndev)
  x2=FLOAT(ndev)
  y1=-FLOAT(ndev)
  y2=FLOAT(ndev)

  CALL Plschr(0.,0.7)

  CALL Plcol(15)
  CALL Pllsty(soli)
  CALL Plenv(x1,x2,y1,y2,0,0)
  CALL Pllab("A/#gs","A/#gs"," ")

  WRITE (lbl,115) t0,t9
  CALL Plmtex("b",3.5,0.80,0.5,lbl)

  CALL Plschr(0.,1.0)

  WRITE (lbl,121) lvars1(ivcor2)
  CALL Plmtex("t",1.5,0.60,1.0,lbl)
  WRITE (lbl,122) lvars1(ivcor1)
  CALL Plmtex("t",1.5,0.62,0.0,lbl)

  zc=zc*MAXVAL(cx)

  DO mu=1,4
     CALL Plcol(lcolors(mu))
     CALL Plcon1(cx,nb,nb,1,nb,1,nb,zc(mu),1,yp,yp)
  END DO
     END IF
#endif

  IF (iptype == 2) THEN
     WRITE (13,100) lvars(ivcor2),lvars(ivcor1),npts
     WRITE (13,110) t0,t9,its
     WRITE (13,150) nb,ivcor2,ivcor1
     WRITE (13,160) yp
     WRITE (13,160) cx
  END IF

  WRITE (6,*) 'max is ',MAXVAL(cx)

!...  aus

  CALL Plaus

100 FORMAT("cross coherence of ",a6,"against ",a6," with ",i6," samples")
110 FORMAT("time = ",g9.3," -- ",g9.3," with ",i4," plot steps")
115 FORMAT("t = ",g9.3," -- ",g9.3)
121 FORMAT("cross coherence of ",a8)
122 FORMAT("against ",a8)

150 FORMAT(4i6)
160 FORMAT(10f8.4)

END PROGRAM Plcoher_probes
