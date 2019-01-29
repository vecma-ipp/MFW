PROGRAM Plpol

!...  poloidal plane 2D ctrs of four vars
!...  draws ctrs in each of 3 main quadrangles

  USE Coeff
  USE Pvars
  USE Vars
  USE Plotins

  IMPLICIT NONE

  INTEGER :: iu,i,j,i0,i1,j0,j1,j2,mu,nu,ipl,iframe,istr,ieof=0,iiter,iflag=0
  REAL :: xx0,yy0,xx9,yy9,time
  REAL :: x1,y1

  CHARACTER*32 :: lbl

  INCLUDE 'pages.h'

  INTEGER, PARAMETER :: npics=4
  CHARACTER*8 :: lvars(npics),lvars1(npics)
  DATA lvars/"psi","pressure","current","dtheta"/
  DATA lvars1/"\gq","P","J\dtor","d\gh"/
!  DATA lvars/"psi","theta","current","dtheta"/
!  DATA lvars1/"\gq","\gh\dc","J\dtor","d\gh"/

!...  allocations

  INCLUDE 'pinit.h90'

  ny0=3*nx0
  ALLOCATE(cxa(nx0,ny0,npics+2))

  i1=0
  DO i0=1,nx0
     i=i0+ngdx
     i1=i1+fsfc(i)%ny0
  END DO
  ALLOCATE(cx(i1+1,npics+2))

  cxa=0.
  cx=0.

  xx0=MINVAL(fsbndyr)
  xx9=MAXVAL(fsbndyr)
  yy0=MINVAL(fsbndyz)
  yy9=MAXVAL(fsbndyz)

!...  initialise plotting

  CALL Plein("plpol",1,1)

  aspect = (yy9-yy0)/(xx9-xx0)

  IF (iptype == 2) THEN
     WRITE (13,112) xx0,xx9,yy0,yy9,aspect,npics
  END IF

!...  start sampling

  iframe=0
  DO ipl=1,10000
191  iframe=iframe+1

!...  read snapshot

     IF (time < 20.) THEN
        CALL Psnapsin(iu,time,ieof)
        INCLUDE 'readprofs.h90'
     ELSE
        IF (iflag == 0) THEN
           DO i0=1,nx0
              i=i0+ngdx
              DO j=1,fsfc(i)%ny
                 INCLUDE 'vardefp.h90'
                 x1=(rtor-rr0-0.6)*5.
                 y1=(ztor)*10.

                 y1=EXP(-y1*y1-x1*x1)
                 fsfc(i)%fuu(muphige,j)=y1
              END DO
           END DO
           iflag=1
        END IF
        time=time+1.0
        CALL Yshiftf(tpi/8.0,muphige,muphige)
     END IF

!...  time check

     IF (NINT(time) < ntstart .OR. iframe < nfstart) GOTO 191
     IF (NINT(time) > ntstop .AND. iframe > nfstop) GOTO 190

!...  get pvars

!.     CALL Strip

!     INCLUDE 'getpvars.h90'

!...  load ctrs

     i1=0
     DO i0=1,nx0
        i=i0+ngdx
        j1=fsfc(i)%ny0
        DO j0=1,j1
           j=j0+ngdy

           INCLUDE 'vardefp.h90'

           cx(i1+j0,1)=rtor
           cx(i1+j0,2)=ztor
           cx(i1+j0,3)=psi
           cx(i1+j0,4)=pressure
!           cx(i1+j0,4)=theta
           cx(i1+j0,5)=current*rtor
           cx(i1+j0,6)=phige
!           cx(i1+j0,6)=theta-(tpi/j1)*(j-1-ngdy)
        END DO

        i1=i1+j1
     END DO

     DO mu=1,npics+2
        i1=0
        DO i0=1,nx0
           j2=2*(i0-1)
           DO j0=1,i0
              cxa(i0,j0,mu)=cx(i1+j0,mu)
              cxa(j0,i0,mu)=cx(i1+2*i0-j0,mu)
              cxa(i0,nx0+j0,mu)=cx(i1+j2+j0,mu)
              cxa(j0,nx0+i0,mu)=cx(i1+j2+2*i0-j0,mu)
              cxa(i0,2*nx0+j0,mu)=cx(i1+2*j2+j0,mu)
              cxa(j0,2*nx0+i0,mu)=cx(i1+2*j2+2*i0-j0,mu)
           END DO
           i1=i1+MAX(1,6*(i0-1))
        END DO
     END DO

!...  draw contours in x-y plane

#ifdef PGPLOT
  IF (iptype == 1) THEN
     CALL Pgpage
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
     CALL Ctrs(mu,cxa(1,1,mu+2),nx0,ny0,xx0,xx9,yy0,yy9, &
          cxa,lvars(mu),lvars1(mu),npics)
     IF (iptype == 2) THEN
        j0=SIZE(fsbndyr)
        CALL Poutpg("dash",fsbndyr,fsbndyz,j0)
     END IF
#ifdef PGPLOT
     IF (iptype == 1) THEN
        IF (mu == 1 .OR. mu == 3) THEN
           CALL Pgsls(3)
           CALL Pgsci(green)
           CALL Line("dash",rr0,yy0,rr0,yy9)
        END IF
        IF (mu == 3) THEN
           CALL Pgsls(1)
           CALL Pgsci(green)
           DO nu=0,5
              DO i0=1,nx0
                 i=i0+ngdx
                 j=1+ngdy+nu*fsfc(i)%ny0/6
                 dvp1(i0)=fsfc(i)%fuu(nvars-4,j)
                 dvp2(i0)=fsfc(i)%fuu(nvars-3,j)
              END DO
              CALL Pgline(nx0,dvp1,dvp2)
           END DO
           DO nu=1,4
              i=nx0*nu/4
              j0=fsfc(i)%ny0
              j1=j0+1
              DO j=1,j1
                 dvpy1(j)=fsfc(i)%fuu(nvars-4,j+ngdy)
                 dvpy2(j)=fsfc(i)%fuu(nvars-3,j+ngdy)
              END DO
              CALL Pgline(j1,dvpy1,dvpy2)
           END DO
        END IF
        CALL Pgsci(green)
        j0=SIZE(fsbndyr)
        CALL Pgline(j0,fsbndyr,fsbndyz)
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
112 FORMAT(" x1    x9    y1    y9    ny/nx    npics",/,6x,5g12.4,i8)
113 FORMAT(" t = ",g10.4)
115 FORMAT("t = ",g10.4)

END PROGRAM Plpol


SUBROUTINE Ctrs(ipg,cx,nx,ny,x1,x2,y1,y2,rth,lvar,lvar1,npics)

!...  plots polar in x-y plane

  IMPLICIT NONE

  INTEGER :: ipg,nx,ny,npics
  REAL :: x1,x2,y1,y2
  REAL :: cx(nx,ny),rth(nx,ny,2)

  INTEGER :: iptype,ix1,ix2,jy1,jy2,i,j,ncases
  REAL :: zc(13),ztr(6),dh
  REAL :: xlo,xhi,ylo,yhi
  REAL :: xctr,yctr,dx2,dy2
  CHARACTER*8 :: lvar,lvar1
  CHARACTER*48 :: lbl
  INCLUDE 'pages.h'
  COMMON/pctrl/iptype
#ifdef PGPLOT
  EXTERNAL pgpolar
#endif

!...  have the contour array

  dh=0.16*MAXVAL(ABS(cx(2:nx-1,2:ny-1)))

!...  draw and label the map

#ifdef PGPLOT
  IF (iptype == 1) THEN
     ncases=npics

     INCLUDE 'cbndydefs.h90'

     CALL Pgsch(0.7)

     CALL Pgsls(1)
     CALL Pgsci(1)
     CALL Pgsvp(xlo,xhi,ylo,yhi)
     CALL Pgwnad(x1,x2,y1,y2)
     CALL Pgbox("bcnt",0.0,1,"bcntv",0.0,1)

     WRITE (lbl,117) dh
     CALL Pgmtxt("b",3.5,0.5,0.5,lbl)

     CALL Pgsch(1.0)

     CALL Pgmtxt("t",1.5,0.45,1.0,lvar1)
     CALL Pgmtxt("t",1.5,0.47,0.0,"(R,Z)")

     CALL Pgsci(2)
     zc(1:6)=(/ (dh*i,i=1,6) /)
     CALL Pgconx(cx,nx,ny,1,nx,1,nx,zc,6,pgpolar)
     CALL Pgconx(cx,nx,ny,1,nx,nx+1,2*nx,zc,6,pgpolar)
     CALL Pgconx(cx,nx,ny,1,nx,2*nx+1,3*nx,zc,6,pgpolar)
     CALL Pgsci(4)
     zc(1:6)=(/ (dh*(i-7),i=1,6) /)
     CALL Pgconx(cx,nx,ny,1,nx,1,nx,zc,-6,pgpolar)
     CALL Pgconx(cx,nx,ny,1,nx,nx+1,2*nx,zc,-6,pgpolar)
     CALL Pgconx(cx,nx,ny,1,nx,2*nx+1,3*nx,zc,-6,pgpolar)
     WRITE (6,110) lvar,dh
  END IF
#endif

  IF (iptype /= 1) THEN
     zc(1)=0.
     zc(2)=.16
     WRITE (6,100) lvar
     CALL Rcontr2(0,zc,1,cx,nx,rth,1,nx,1,rth(1,1,2),1,ny,1,"rect")
  END IF

!...  label the plot

  IF (iptype == 2) WRITE(13,110) lvar,dh

  WRITE (6,120) x1,x2
  WRITE (6,125) y1,y2

!...  done

100 FORMAT(a4," contours:")
110 FORMAT(a4,"(x,y)... dz = ",g9.3)
117 FORMAT("\gD = ",g9.3)
120 FORMAT(g10.3," < R < ",g10.3)
125 FORMAT(g10.3," < Z < ",g10.3)

END SUBROUTINE Ctrs

