PROGRAM Phase_dist

!...  phase distributions from p file data
!...  uses several flux surfaces

  USE Coeff
  USE Pvars
  USE Vars
  USE FFT, ONLY : four1d_real
  USE Plotins

  IMPLICIT NONE

  CHARACTER*48 :: lbl
  REAL :: arg
  INTEGER :: nb,nbnl,nbpnl

  INTEGER :: iframe,istr,iu,ieof,ipl
  REAL :: t0,t9,time
  REAL :: tavg
  REAL :: vex,vey

  INTEGER :: i,j,j0,l,mu
  REAL :: x1,x2,y1,y2

  INTEGER, PARAMETER :: npics=5
  CHARACTER*8 :: lvars(npics),lvars1(npics)
  DATA lvars/"phi","ne","vor","vex","vey"/
  DATA lvars1/"\gf","n\de","\gW","V\ux","V\uy"/

#ifdef PGPLOT
  EXTERNAL pglogy
#endif
  REAL :: zc(4),ztr(6)
  INTEGER :: lcolors(4)
!.  DATA zc/ 0.1, 0.37, 0.5, 0.8 /
!.  DATA zc/ 0.1, 0.2, 0.3, 0.4 /
  DATA zc/ 0.05, 0.1, 0.15, 0.2 /
  DATA lcolors/ 1, 4, 3, 2 /

!...  allocations

  INCLUDE 'pinit.h90'

  IF (ivcor1 == 0) ivcor1=1
  IF (ivcor2 == 0) ivcor2=2

  ALLOCATE(uup(ny0,npics))
  ALLOCATE(uuky(0:nl,npics))

  nb=51
  nbnl=nb*nl
  nbpnl=4*(nb+nl)

  ALLOCATE(rkyl(nl))
  ALLOCATE(cx(nb,nl))
  ALLOCATE(yp(nb))

!...  set plot arrays

  DO j=1,nb
     yp(j)=tpi*(j-0.5)/nb - pi
  END DO
  rkyl=LOG10((/ (l*rkps,l=1,nl) /))

!...  zero everything

  cx=xzero

!...  initialise plotting

  CALL Plein("phasdist",1,1)

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

!...  loop over lines

     DO i=ix1,ix2

!...  extract probe data

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
     END DO

!...  go to ky space

     DO mu=1,npics
        CALL four1d_real(uup(1:ny0,mu), uuky(:,mu), -1)
     END DO
     uuky=uuky/REAL(ny0)

!...  get the correlations and increment bins

     DO l=1,nl
        arg=IMAG(LOG(xzero+CONJG(uuky(l,ivcor2))*uuky(l,ivcor1)))
        j=NINT( nb*(arg+pi)/(2.*pi)+0.5 )
        IF (j < 1 .or. j > nb) THEN
           WRITE (6,400) l,j,arg
           STOP
        END IF
        cx(j,l)=cx(j,l)+1
     END DO

!...  end loop over lines

     END DO

!...  end plot tasks

     t9=time

  END DO

190 ipl=(ipl-1)*(ix2-ix1+1)
  IF (ipl == 0) THEN
     WRITE (6,*) 'no data were taken'
     STOP
  END IF

  WRITE (6,100) lvars(ivcor2),lvars(ivcor1)
  WRITE (6,110) t0,t9,ipl

!...  normalise cx

  tavg=1./ipl
  cx=cx*tavg

!.  cx=cx/MAXVAL(cx)

!...  plot distribution

#ifdef PGPLOT
  IF (iptype == 1) THEN
     x1=-pi
     x2=pi
     y1=FLOOR(MINVAL(rkyl))
     y2=CEILING(MAXVAL(rkyl))

     CALL Pgsch(0.7)

     CALL Pgsci(black)
     CALL Pgsls(1)
     CALL Pgenv(x1,x2,y1,y2,0,20)
     CALL Pglab("\ga","k\dy\u\gr\ds"," ")

     WRITE (lbl,115) t0,t9
     CALL Pgmtxt("b",3.5,0.80,0.5,lbl)

     CALL Pgsch(1.0)

     WRITE (lbl,121) lvars1(ivcor2)
     CALL Pgmtxt("t",1.5,0.48,1.0,lbl)
     WRITE (lbl,122) lvars1(ivcor1)
     CALL Pgmtxt("t",1.5,0.5,0.0,lbl)

     ztr(1)=2.*yp(1)-yp(2)
     ztr(2)=yp(2)-yp(1)
     ztr(3)=0.
     ztr(4)=2.*rkyl(1)-rkyl(2)
     ztr(5)=0.
     ztr(6)=rkyl(2)-rkyl(1)

     DO mu=1,4
        CALL Pgsci(lcolors(mu))
        CALL Pgconx(cx,nb,nl,1,nb,1,nl,zc(mu),1,pglogy)
     END DO

     CALL Pgsci(blue)
     CALL Pgsls(dash)
     CALL Line("dash",0.,y1,0.,y2)

  END IF
#endif

  IF (iptype == 2) THEN
     WRITE (13,110) t0,t9,ipl
     WRITE (13,150) nb,nl-1,ivcor2,ivcor1
     WRITE (13,155) rkps
     WRITE (13,160) lvars(ivcor2),lvars(ivcor1)
     WRITE (13,*) "mode  -pi  ---  0 --- pi"
     WRITE (13,175) ((cx(j,l),j=1,nb),l=1,nl-1)
  END IF

  WRITE (6,*) 'max is ',MAXVAL(cx)

!...  aus

  CALL Plaus

100 FORMAT("phase of ",a8,"relative to ",a8)
110 FORMAT(" time = ",g9.3," to ",g9.3," with ",i4," plot steps")
115 FORMAT("t = ",g9.3," -- ",g9.3)
121 FORMAT("phase of ",a8)
122 FORMAT("relative to ",a8)

150 FORMAT(4i6)
155 FORMAT(g9.3)
160 FORMAT("phase of ",a8," relative to ",a8)
165 FORMAT(21f6.2)
175 FORMAT(10f8.4)

400 FORMAT(" index j out of bounds:",/, &
         "   mode l = ",i3,"  index j = ",i3,"  phase = ",g18.9)

END PROGRAM Phase_dist
