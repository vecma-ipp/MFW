PROGRAM Plequil

!...  result 1D profiles

  USE Coeff
  USE Pvars
  USE Vars
  USE Plotins

  IMPLICIT NONE

  INTEGER :: iu,i,j,i0,j0,j1,j2,ip,im,mu,ipl,iframe,istr,ieof,iiter
  REAL :: xx0,xx9,time
  REAL :: hx0,psi0,drr,dzz

  CHARACTER*32 :: lbl

  INTEGER, PARAMETER :: npics=8
  CHARACTER*8 :: lvars(npics),lvars1(npics)
  DATA lvars/"rho","rhovol","rhotor","q","psi","phi","current","pressure"/
  DATA lvars1/"\gr","\gr\dV","\gr\dT","q","\gq","\gF","J","P"/

!...  allocations

  INCLUDE 'pinit.h90'

  ALLOCATE(xp(nx))
  ALLOCATE(cx(nx,npics))

  xp=ra
  xx0=0.
  xx9=1.

!...  initialise plotting

  CALL Plein("plequil",1,1)

  IF (iptype == 2) THEN
     WRITE (13,112) xx0,xx9,npics
  END IF

!...  start sampling

  iframe=0
  DO ipl=1,niter_eq
191  iframe=iframe+1

!...  get pvars

     INCLUDE 'readprofs.h90'

!...  load profiles

     cx(:,1)=rho
     cx(:,2)=rho_vol
     cx(:,3)=rho_tor
     cx(:,4)=qtor
     cx(:,5)=psitor
     cx(:,6)=phitor
     cx(:,7)=jtor
     cx(:,8)=ptor

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

  DO mu=1,npics
     CALL Prof(mu,cx(1,mu),nx,xp,lvars(mu),lvars1(mu),xx0,xx9,npics)
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

END PROGRAM Plequil


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
