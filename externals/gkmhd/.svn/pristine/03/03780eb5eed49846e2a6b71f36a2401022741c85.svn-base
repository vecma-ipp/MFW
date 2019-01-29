PROGRAM Profavg

!...  simple 1D profiles, time averaged

  USE Coeff
  USE Pvars
  USE Vars
  USE Plotins

  IMPLICIT NONE

  INTEGER :: iu,i,j,i0,j0,j1,j2,ip,im,mu,ipl,iframe,istr,ieof
  REAL :: xx0,xx9,time,t0,t9,tavg
  REAL :: hx0

  CHARACTER*32 :: lbl

  INTEGER, PARAMETER :: npics=4
  CHARACTER*8 :: lvars(npics),lvars1(npics)
  DATA lvars/"ne","phi","vey","vor"/
  DATA lvars1/"n\de\u","\gf","V\uy","\gW"/

!...  allocations

  INCLUDE 'pinit.h90'

  ALLOCATE(xp(nx0))
  ALLOCATE(cx(nx0,npics))

  xp=ra(1+ngdx:nx0+ngdx)/delta
  xx0=0.
  xx9=1./delta

  cx=0.

!...  initialise plotting

  CALL Plein("plprof",1,1)

  IF (iptype == 2) THEN
     WRITE (13,112) xx0,xx9,npics
  END IF

!...  start sampling

  t0=0.
  iframe=0
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

     INCLUDE 'getpvars.h90'

!...  accumulate profiles

     DO i0=1,nx0
        i=i0+ngdx
        j0=fsfc(i)%ny0
        DO j=1+ngdy,j0+ngdy
           INCLUDE 'vardefp.h90'

           cx(i0,1)=cx(i0,1)+ne
           cx(i0,2)=cx(i0,2)+phi
!           cx(i0,3)=cx(i0,3)+vey
           cx(i0,4)=cx(i0,4)+vor
        END DO
        cx(i0,:)=cx(i0,:)/REAL(j0)
     END DO

     DO i0=1,nx0
        ip=MIN(i0+1,nx0)
        im=MAX(i0-1,1)
        hx0=driftx/(ra(ip+ngdx)-ra(im+ngdx))
        cx(i0,3)=hx0*(cx(ip,2)-cx(im,2))
     END DO
!     cx(1,3)=2.*cx(2,3)-cx(3,3)

!...  end plot tasks

     t9=time

  END DO

190 ipl=ipl-1

  WRITE (6,100) t0,t9,ipl

!...  take averages

  tavg=1.0/(xzero+ipl)

  cx=cx*tavg

!...  draw profiles

#ifdef PGPLOT
  IF (iptype == 1) THEN
     CALL Pgpage
     CALL Pgsls(1)
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

  IF (iptype == 2) WRITE (13,113) t0,t9

  DO i=1,npics
     CALL Prof(i,cx(ngdx+1,i),nx0,xp,lvars(i),lvars1(i),xx0,xx9,npics)
  END DO

!...  put time

  WRITE (6,100) t0,t9,ipl

!...  aus

  CALL Plaus

100 FORMAT("time = ",g9.3," to ",g9.3," with ",i4," plot steps")
112 FORMAT(" x1    x9    npics",/,6x,2g12.4,i8)
113 FORMAT(" t = ",g9.3," -- ",g9.3)
115 FORMAT("t = ",g9.3," -- ",g9.3)

END PROGRAM Profavg


SUBROUTINE Prof(ipg,cx,nx,x,lvar,lvar1,x1,x2,npics)

!...  plots x-profile, variable uu

  INTEGER :: nx,npics
  REAL :: x1,x2,y1,y2
  REAL :: cx(nx),x(nx)
  CHARACTER*8 :: lvar,lvar1
  CHARACTER*48 :: lbl
  REAL :: xlo,xhi,ylo,yhi
  INCLUDE 'pages.h'
  COMMON/pctrl/iptype

!...  find grid limits: do ix1--ix2

  ix1=1
  ix2=nx

  y1=MIN(0.,MINVAL(cx))
  y2=MAX(0.,MAXVAL(cx))

!...  draw and label the map

#ifdef PGPLOT
  IF (iptype == 1) THEN
     ncases=npics

     INCLUDE 'cbndydefs.h90'

     CALL Pgsch(0.7)

     CALL Pgsci(1)
     CALL Pgsls(1)
     CALL Pgsvp(xlo,xhi,ylo,yhi)
     CALL Pgswin(x1,x2,y1,y2)
     CALL Pgbox("bcnt",0.0,1,"bcntv",0.0,1)

     CALL Pgsch(1.0)

     CALL Pgmtxt("t",1.5,0.45,1.0,lvar1)
     CALL Pgmtxt("t",1.5,0.47,0.0,"(x)")

     CALL Pgsci(2)
     CALL Pgline(ix2-ix1+1,x(ix1),cx(ix1))

     IF (y1*y2 < 0.) THEN
        CALL Pgsls(3)
        CALL Pgsci(3)
        CALL Line("dash",x1,0.,x2,0.)
     END IF
  END IF
#endif

  IF (iptype /= 1) CALL Poutpg("soli",x(ix1),cx(ix1),ix2-ix1+1)

!...  label the plot

  WRITE (6,100) lvar,y1,y2
  WRITE (6,110) x1,x2

!...  done

100 FORMAT(a8,"(x)... ",g10.3," <--> ",g10.3)
110 FORMAT(g10.3," < x < ",g10.3)

END SUBROUTINE Prof
