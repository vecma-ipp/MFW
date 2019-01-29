PROGRAM Plcoher

!...  inter-variable coherence, scatter plot

  USE Coeff
  USE Pvars
  USE Vars
  USE Plotins

  IMPLICIT NONE

  CHARACTER*64 :: lbl
  INTEGER :: ntmax,ipts

  REAL :: favg(2),fdev(2)

  INTEGER :: ib,jb,nb,ndev

  INTEGER :: iframe,istr,iu,ieof,ipl
  REAL :: t0,t9,time
  REAL :: tavg
  REAL :: vex,vey

  INTEGER :: i,j,j0,mu,npts
  REAL :: x1,x2,y1,y2

  INTEGER, PARAMETER :: npics=5
  CHARACTER*8 :: lvars(npics),lvars1(npics)
  DATA lvars/"phi","ne","vor","vex","vey"/
  DATA lvars1/"\gf","n\de","\gW","V\ux","V\uy"/

  REAL :: zc(4),ztr(6)
  INTEGER :: lcolors(4)
  DATA zc/ 0.1, 0.37, 0.5, 0.8 /
  DATA lcolors/ 1, 9, 7, 2 /

!...  allocations

  INCLUDE 'pinit.h90'

  IF (ivcor1 == 0) ivcor1=1
  IF (ivcor2 == 0) ivcor2=2

  nb=51
  ndev=4

  ntmax=200

  ALLOCATE (fcors(nx0*ny0*ntmax,2))

  ALLOCATE(uup(ny0,npics))

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

        uup(j0,1)=phi
        uup(j0,2)=ne
        uup(j0,3)=vor
        uup(j0,4)=vex
        uup(j0,5)=vey

        npts=npts+1

        fcors(npts,1)=uup(j0,ivcor1)
        fcors(npts,2)=uup(j0,ivcor2)

     END DO

!...  end loop over lines

     END DO

!...  end plot tasks

     t9=time

  END DO

190 ipl=ipl-1

  WRITE (6,100) lvars(ivcor2),lvars(ivcor1),npts
  WRITE (6,110) t0,t9,ipl

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
     ib=NINT( nb*(fcors(i,1)+ndev)/(2*ndev) + 0.5 )
     ib=MAX(ib,1)
     ib=MIN(ib,nb)
     jb=NINT( nb*(fcors(i,2)+ndev)/(2*ndev) + 0.5 )
     jb=MAX(jb,1)
     jb=MIN(jb,nb)
     cx(ib,jb)=cx(ib,jb)+1
  END DO

  tavg=1.0*nb/(ndev*npts+xzero)
!.  cx=LOG10( cx*tavg )
  cx=cx*tavg

!...  plot coherence

#ifdef PGPLOT
  IF (iptype == 1) THEN
     x1=-FLOAT(ndev)
     x2=FLOAT(ndev)
     y1=-FLOAT(ndev)
     y2=FLOAT(ndev)

     CALL Pgsch(0.7)

     CALL Pgsci(1)
     CALL Pgsls(soli)
     CALL Pgenv(x1,x2,y1,y2,0,0)
     CALL Pglab("A/\gs","A/\gs"," ")

     WRITE (lbl,115) t0,t9
     CALL Pgmtxt("b",3.5,0.80,0.5,lbl)

     CALL Pgsch(1.0)

     WRITE (lbl,121) lvars1(ivcor2)
     CALL Pgmtxt("t",1.5,0.60,1.0,lbl)
     WRITE (lbl,122) lvars1(ivcor1)
     CALL Pgmtxt("t",1.5,0.62,0.0,lbl)

     zc=zc*MAXVAL(cx)

     ztr(1)=2.*yp(1)-yp(2)
     ztr(2)=yp(2)-yp(1)
     ztr(3)=0.
     ztr(4)=2.*yp(1)-yp(2)
     ztr(5)=0.
     ztr(6)=yp(2)-yp(1)

     DO mu=1,4
        CALL Pgsci(lcolors(mu))
        CALL Pgcont(cx,nb,nb,1,nb,1,nb,zc(mu),1,ztr)
     END DO

     CALL Pgsci(blue)
     CALL Pgsls(dash)
     CALL Line("dash",0.,y1,0.,y2)

     CALL Pgsci(blue)
     CALL Pgsls(dash)
     CALL Line("dash",x1,0.,x2,0.)

  END IF
#endif

  IF (iptype == 2) THEN
     WRITE (13,100) lvars(ivcor2),lvars(ivcor1),npts
     WRITE (13,110) t0,t9,ipl
     WRITE (13,150) nb,ivcor2,ivcor1
     WRITE (13,160) yp
     WRITE (13,160) cx
  END IF

  WRITE (6,*) 'max is ',MAXVAL(cx)*nb/(2*ndev)

!...  aus

  CALL Plaus

100 FORMAT("cross coherence of ",a8,"against ",a8," with ",i6," samples")
110 FORMAT("time = ",g9.3," -- ",g9.3," with ",i4," plot steps")
115 FORMAT("t = ",g9.3," -- ",g9.3)
121 FORMAT("cross coherence of ",a8)
122 FORMAT("against ",a8)

150 FORMAT(4i6)
160 FORMAT(10f8.4)

END PROGRAM Plcoher
