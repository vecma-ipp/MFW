PROGRAM Pgdw

!...  HW version -- temporal energy diagnostics from pfiles

  USE Vars
  USE Vconv
  USE Pvars
  USE Coeff
  USE SolnScr
  USE FFTs
  USE Plotins

  IMPLICIT NONE

  INTEGER :: i,j,l,mu,ieof,iframe,istr
  INTEGER :: iu,its,it0,nt,nstat,igm
  REAL :: t,t0,t9,tav,gammah,tstat,eh,el

  PARAMETER (nt=1001)
  CHARACTER*48 :: lbl
  REAL, DIMENSION (nt) :: time,gamt
  INTEGER :: i0,j0
  REAL :: p0
  REAL :: vex,vey
  REAL :: fex
  REAL :: dnedx,dvordx

!...  allocations

  INCLUDE 'pinit.h90'
  INCLUDE 'fset1.h90'

  ALLOCATE(fcors(nt,30))
  ALLOCATE(cx(nx0,30))

  fcors=0.

  t0=time0
  t9=time9
  tav=timeav
  gammah=xzero
  it0=0

!...  allocate auxiliary variables

!...  initialise plotting

  CALL Plein("pgdw",1,1)

!...  start sampling

  iframe=0
  DO its=1,nt
191  iframe=iframe+1

!...  read snapshot

     DO istr=1,istride
192     CONTINUE
        CALL Psnapsin(iu,t,ieof)
        IF (ieof .ne. 0) GOTO 199
        GOTO 198
199     iu=iu+1
        IF (iu-20 .gt. nfiles) GOTO 190
        GOTO 192
198     CONTINUE
     END DO

!...  time check

     IF (t < t0) GOTO 191
     IF (t > t9) GOTO 190
     IF (it0 == 0) it0=its
     IF (its == 1) t0=t

     time(its)=t

!...  get pvars

!...  diags go here

!.     WRITE (0,215) its,t

     cx=0.

     DO i0=1,nx0
        i=i0+ngdx

        DO j0=1,ny0
           j=j0+ngdy

           INCLUDE 'vardef.h90'

           vex=drifty*(uu(j+1,i,1)-uu(j-1,i,1))
           vey=driftx*(uu(j,i+1,1)-uu(j,i-1,1))

           fex=ne*vex

           dnedx=hx2m*(uu(j,i+1,2)-uu(j,i-1,2))
           dvordx=hx2m*(uu(j,i+1,4)-uu(j,i-1,4))

           cx(i0,1)=cx(i0,1)+ne
           cx(i0,2)=cx(i0,2)+phi
           cx(i0,3)=cx(i0,3)+vor

           cx(i0,5)=cx(i0,5)+vex*vey

           cx(i0,7)=cx(i0,7)+fex
           cx(i0,8)=cx(i0,8)+vey

        END DO
     END DO

     cx=cx/(ny00)

     DO i=1,nx0

        fcors(its,1)=fcors(its,1)-cx(i,4)*cx(i,9)
        fcors(its,2)=fcors(its,2)+cx(i,3)*cx(i,5)
        fcors(its,3)=fcors(its,3)+cx(i,8)*cx(i,8)

     END DO

!...  end diags

  END DO

190 its=its-1

  t0=MAX(t0,2.*time(1)-time(2))
  t9=time(its)
  nstat=MAX(1,MIN(its-2,NINT(tav/(tau0*nstep*ndg))))
  tstat=t9-time(its-nstat)

  fcors=fcors/(nx00)

!...  show diags

  CALL Estart(t0,t9,tstat)

  igm=0
  CALL Evol(1,'E','Z',fcors(1,1),gamt,time,igm,t0,t9,it0,its,nstat)
  CALL Evol(2,'R','E',fcors(1,2),gamt,time,igm,t0,t9,it0,its,nstat)
  CALL Evol(3,'E','v',fcors(1,3),gamt,time,igm,t0,t9,it0,its,nstat)

!...  done

  WRITE (6,100) t0,t9,its,tstat

!...  aus

  CALL Plaus

100 FORMAT(" time = ",g9.3," to ",g9.3," with ",i4," plot steps",/, &
         "     stats taken over last dt = ",g9.3)

215 FORMAT("step ",i5," time = ",g14.8)

END PROGRAM Pgdw


SUBROUTINE Estart(t0,t9,tstat)

  IMPLICIT NONE

  REAL :: t0,t9,tstat
  CHARACTER*48 :: lbl
  INTEGER iptype
  COMMON/pctrl/iptype

#ifdef PLPLOT
  IF (iptype == 1) THEN
  CALL Pladv(0)
  CALL Plvpas(0.,1.,0.,1.,512./768.)
  CALL Plwind(0.,1.,0.,1.)
  CALL Plcol(1)
  CALL Pllsty(1)
  CALL Plbox("bc",0.0,1,"bc",0.0,1)

  CALL Plcol(15)
  CALL Plschr(0.,0.7)

  WRITE (lbl,100) t0,t9
!.  CALL Plmtex("b",-2.2,0.15,0.5,lbl)
  WRITE (lbl,110) tstat
  CALL Plmtex("b",-0.7,0.15,0.5,lbl)
  END IF
#endif

  IF (iptype == 2) WRITE (13,120) t0,t9,tstat

100 FORMAT("t = ",g9.3," -- ",g9.3)
110 FORMAT("#gDt = ",g9.3)
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

  IF (igm /= 0) THEN
     DO i=it0+1,its-1
        dtm=time(i)-time(i-1)
        dtp=time(i+1)-time(i)
        dtpm=dtm+dtp
        gamt(i)=(dtm*dtm*(enrg(i+1)-enrg(i)) + &
             dtp*dtp*(enrg(i)-enrg(i-1)))/(dtp*dtm*dtpm)
     END DO
     i=it0
     gamt(i)=(enrg(i+1)-enrg(i))/(time(i+1)-time(i))
     i=its
     gamt(i)=(enrg(i)-enrg(i-1))/(time(i)-time(i-1))

     y2=MAXVAL(ABS(gamt))
     y1=MAXVAL(ABS(enrg))

!.     gamt=gamt/(1.e-4*y1+2.*enrg)
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

#ifdef PLPLOT
  IF (iptype == 1) THEN
  CALL Plschr(0.,0.5)

  CALL Plcol(15)
  CALL Pllsty(1)
  CALL Plvpor(xlo,xhi,ylo,yhi)
  CALL Plwind(x1,x2,y1,y2)
  CALL Plbox("bcnt",0.0,1,"bcntv",0.0,1)

  CALL Plcol(1)
  CALL Plline(its-it0+1,time(it0),gamt(it0))

  IF (y1*y2 < 0.) THEN
     CALL Pllsty(2)
     CALL Plcol(2)
     CALL Pljoin(x1,0.,x2,0.)
  END IF
  END IF
#endif

  IF (iptype == 2) WRITE (13,100) lchr,pchr,i,j

  IF (iptype /= 1) CALL Tracec(pchr,time(it0),gamt(it0),its-it0)

  CALL Pstats(lchr,pchr,gamt,its,nstat)

100 FORMAT("plot qty ",2a1,/,2i4)

END SUBROUTINE Evol


SUBROUTINE Pstats(lchr,pchr,enrg,its,nstat)

  CHARACTER*32 lbl
  CHARACTER*1 pchr,lchr
  REAL enrg(its)
  INTEGER iptype
  COMMON/pctrl/iptype

  CALL Stats(enrg,its,nstat,eavg,edev)

  WRITE (6,115) lchr,pchr,eavg,edev
  IF (iptype == 2) WRITE (13,115) lchr,pchr,eavg,edev

#ifdef PLPLOT
  IF (iptype == 1) THEN
     WRITE (lbl,115) lchr,pchr,eavg,edev
     CALL Plcol(15)
     CALL Plschr(0.,0.5)
     CALL Plmtex("t",1.5,0.5,0.5,lbl)
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
