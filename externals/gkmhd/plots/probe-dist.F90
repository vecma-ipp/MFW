PROGRAM Phase_dist_probes

!...  phase distributions from probe data

  USE Pvars
  USE Coeff
  USE FFTs
  USE Plotins

  IMPLICIT NONE

  CHARACTER*48 :: lbl
  REAL :: arg
  INTEGER :: nb,nbnl,nbpnl

  INTEGER :: iframe,istr,iu,ieof,its
  REAL :: t0,t9,time
  REAL :: tavg

  INTEGER :: j,l,mu
  REAL :: x1,x2,y1,y2

  CHARACTER*8 :: lvars(2),lvars1(2)
  DATA lvars/"phi","ne"/
  DATA lvars1/"#gf","n#de"/

  REAL :: zc(4)
  INTEGER :: lcolors(4)
  DATA zc/ 0.1, 0.37, 0.5, 0.8 /
  DATA lcolors/ 2, 9, 3, 1 /

!...  allocations

  INCLUDE 'dinitfl.h90'

  IF (ivcor1 == 0) ivcor1=1
  IF (ivcor2 == 0) ivcor2=2

  ALLOCATE(uup(ny0,2))
  ALLOCATE(uuky(nl,2))

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
  DO l=1,nl
     rkyl(l)=ALOG10(l*rkps)
  END DO

!...  initialise FFTs

  CALL SCFFT(0,ny0,1.0,rfty,cftky,tg,wk,0)

!...  zero everything

  cx=xzero

!...  initialise plotting

  CALL Plein("phasdist",1,1)

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

!...  get the correlations and increment bins

     DO l=1,nl
        arg=AIMAG(CLOG(xzero+CONJG(uuky(l,ivcor2))*uuky(l,ivcor1)))
        j=NINT( nb*(arg+pi)/(2.*pi)+0.49999 )
        IF (j < 1 .or. j > nb) THEN
           WRITE (6,400) l,j,arg
           STOP
        END IF
        cx(j,l)=cx(j,l)+1
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

!...  normalise cx

  tavg=1./its
  cx=cx*tavg

!...  plot distribution

#ifdef PLPLOT
     IF (iptype == 1) THEN
  x1=-pi
  x2=pi
  y1=FLOOR(MINVAL(rkyl))
  y2=CEILING(MAXVAL(rkyl))

  CALL Plschr(0.,0.7)

  CALL Plcol(15)
  CALL Pllsty(soli)
  CALL Plenv(x1,x2,y1,y2,0,20)
  CALL Pllab("#ga","k#dy"," ")

  WRITE (lbl,115) t0,t9
  CALL Plmtex("b",3.5,0.80,0.5,lbl)

  CALL Plschr(0.,1.0)

  WRITE (lbl,121) lvars1(ivcor2)
  CALL Plmtex("t",1.5,0.48,1.0,lbl)
  WRITE (lbl,122) lvars1(ivcor1)
  CALL Plmtex("t",1.5,0.5,0.0,lbl)

  DO mu=1,4
     CALL Plcol(lcolors(mu))
     CALL Plcon1(cx,nb,nl,1,nb,1,nl,zc(mu),1,yp,rkyl)
  END DO
     END IF
#endif

  IF (iptype == 2) THEN
     WRITE (13,110) t0,t9,its
     WRITE (13,150) nb,nl-1,ivcor2,ivcor1
     WRITE (13,155) rkps
     WRITE (13,160) lvars(ivcor2),lvars(ivcor1)
     WRITE (13,*) "mode  -pi  ---  0 --- pi"
     WRITE (13,175) ((cx(j,l),j=1,nb),l=1,nl-1)
  END IF

  WRITE (6,*) 'max is ',MAXVAL(cx)

!...  aus

  CALL Plaus

100 FORMAT("phase of ",a6,"relative to ",a6)
110 FORMAT(" time = ",g9.3," to ",g9.3," with ",i4," plot steps")
115 FORMAT("t = ",g9.3," -- ",g9.3)
121 FORMAT("phase of ",a8)
122 FORMAT("relative to ",a8)

150 FORMAT(4i6)
155 FORMAT(g9.3)
160 FORMAT("phase of ",a6," relative to ",a6)
165 FORMAT(21f6.2)
175 FORMAT(10f8.4)

400 FORMAT(" index j out of bounds:",/, &
         "   mode l = ",i2,"  index j = ",i2,"  phase = ",g10.4)

END PROGRAM Phase_dist_probes
