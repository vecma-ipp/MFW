PROGRAM Plprof

!...  simple 1D profiles

  USE Coeff
  USE Pvars
  USE Vars
  USE Plotins

  IMPLICIT NONE

  INTEGER :: iu,i,j,i0,j0,j1,j2,ip,im,mu,ipl,iframe,istr,ieof,iiter
  REAL :: xx0,xx9,time
  REAL :: hx0,psi0,drr,dzz

  CHARACTER*32 :: lbl

  INTEGER, PARAMETER :: npics=4
  CHARACTER*8 :: lvars(npics),lvars1(npics)
  DATA lvars/"psi","pressure","current","dtheta"/
  DATA lvars1/"\gq","P","J","d\gh"/

!...  allocations

  INCLUDE 'pinit.h90'

  ALLOCATE(xp(nx0))
  ALLOCATE(cx(nx0,npics))

  xp=rho(1+ngdx:nx0+ngdx)
  xx0=0.
  xx9=1.

!...  initialise plotting

  CALL Plein("plprof",1,1)

  IF (iptype == 2) THEN
     WRITE (13,112) xx0,xx9,npics
  END IF

!...  start sampling

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

!...  get pvars

     INCLUDE 'readprofs.h90'

!     INCLUDE 'getpvars.h90'

!...  accumulate profiles

     cx=0.

     raxis=fsfc(2+ngdx)%fuu(nvars-4,1+ngdy)
     WRITE (0,*) 'raxis',raxis

     DO i0=1,nx0
        i=i0+ngdx
        j1=fsfc(i)%ny0

        DO j0=1,j1
           j=j0+ngdy

           INCLUDE 'vardefp.h90'

           cx(i0,1)=cx(i0,1)+psi
           cx(i0,2)=cx(i0,2)+pressure
!           cx(i0,3)=cx(i0,3)+(current/rtor) / fsavgr1(i)
           cx(i0,3)=cx(i0,3)+(current*rtor) / raxis
!           cx(i0,3)=cx(i0,3)+(current*rtor) * fsavgr1(i)
           cx(i0,4)=cx(i0,4)+phige
        END DO
        cx(i0,:)=cx(i0,:)/REAL(j1)
     END DO

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

     WRITE (lbl,115) time
     CALL Pgmtxt("t",-1.5,0.95,1.0,lbl)
  END IF
#endif

  IF (iptype == 2) WRITE (13,113) time

  xp=rho_tor(1+ngdx:nx0+ngdx)

  DO mu=1,npics
     CALL Prof(mu,cx(1,mu),nx0,xp,lvars(mu),lvars1(mu),xx0,xx9,npics)
#ifdef PGPLOT
  IF (iptype == 1) THEN
     CALL Pgsci(green)
     CALL Pgsls(3)
     i0=1+ngdx
     SELECT CASE(mu)
        CASE(1)
!           CALL Pgline(nx0,xp,psitor(i0))
           CALL Pgpnts(nx0,xp,psitor(i0),5,1)
        CASE(2)
           CALL Pgline(nx0,xp,ptor(i0))
        CASE(3)
           CALL Pgline(nx0,xp,jtor(i0))
        CASE DEFAULT
     END SELECT
  END IF
#endif
  END DO

!...  put time

  WRITE (6,100) time

!...  end plot tasks

  END DO

190 ipl=ipl-1

!...  aus

  CALL Plaus

100 FORMAT("time = ",g10.4)
112 FORMAT(" x1    x9    npics",/,6x,2g12.4,i8)
113 FORMAT(" t = ",g10.4)
115 FORMAT("t = ",g10.4)

END PROGRAM Plprof


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
