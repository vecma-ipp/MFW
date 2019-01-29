PROGRAM Plctr1

!...  simple 2D ctrs of one var 

  USE Coeff
  USE Pvars
  USE Vars
  USE Plotins

  IMPLICIT NONE

  INTEGER :: iu,i,j,i0,j0,j1,j2,ipl,iframe,istr,ieof
  REAL :: xx0,yy0,xx9,yy9,time

  CHARACTER*32 :: lbl

  INCLUDE 'pages.h'

  INTEGER, PARAMETER :: npics=4
  CHARACTER*8 :: lvars(npics),lvars1(npics)
  DATA lvars/"phi","ne","he","vor"/
  DATA lvars1/"\gf","n\de\u","h\de\u","\gW"/

!...  allocations

  INCLUDE 'pinit.h90'
  IF (ivplot == 0) ivplot=4

  ny0=6*nx0

  ALLOCATE(xp(nx0))
  ALLOCATE(yp(ny0))
  ALLOCATE(cxa(nx0,ny0,npics))

  cxa=0.

  hy=tpi/REAL(ny0)
  yp=(/ (hy*(j-0.5),j=1,ny0) /)
  xp=ra(1+ngdx:nx0+ngdx)

  xx0=MINVAL(xp)
  xx9=MAXVAL(xp)
  yy0=MINVAL(yp)
  yy9=MAXVAL(yp)

!...  initialise plotting

  CALL Plein("plctr",1,1)

  aspect = (yy9-yy0)/(xx9-xx0)

  IF (iptype == 2) THEN
     WRITE (13,112) xx0,xx9,yy0,yy9,aspect,1
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

     INCLUDE 'getpvars.h90'

     DO i0=1,nx0
        i=i0+ngdx
        j1=fsfc(i)%ny0
        DO j0=1,j1
           j=j0+ngdy

           INCLUDE 'vardefp.h90'

           cxa(i0,j0,1)=phi
           cxa(i0,j0,2)=ne
           cxa(i0,j0,3)=he
           cxa(i0,j0,4)=vor
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

  i=ivplot
     CALL Ctrs(i,cxa(1,1,i),nx0,ny0,xp,yp, &
          lvars(i),lvars1(i),xx0,xx9,yy0,yy9,1)

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

END PROGRAM Plctr1


SUBROUTINE Ctrs(ipg,cx,nx,ny,x,y,lvar,lvar1,x1,x2,y1,y2,npics)

!...  plots in x-y plane, variable uu

  IMPLICIT NONE

  INTEGER :: ipg,nx,ny,npics
  REAL :: x1,x2,y1,y2
  REAL :: cx(nx,ny),x(nx),y(ny)

  INTEGER :: iptype,ix1,ix2,jy1,jy2,i,j,ncases
  REAL :: zc(13),ztr(6),dh
  REAL :: xlo,xhi,ylo,yhi
  REAL :: xctr,yctr,dx2,dy2
  CHARACTER*8 :: lvar,lvar1
  CHARACTER*48 :: lbl
  INCLUDE 'pages.h'
  COMMON/pctrl/iptype
#ifdef PGPLOT
  EXTERNAL pgxy
#endif

!...  find grid limits: do ix1--ix2, jy1--jy2

  CALL Gridlims(x,nx,x1,x2,ix1,ix2)
  CALL Gridlims(y,ny,y1,y2,jy1,jy2)

!...  have the contour array

  dh=0.16*MAXVAL(ABS(cx))

!...  draw and label the map

#ifdef PGPLOT
  IF (iptype == 1) THEN
     ncases=npics

     xlo=0.1
     xhi=0.9
     ylo=0.1
     yhi=0.9

     CALL Pgsch(0.7)

     CALL Pgsci(1)
     CALL Pgsvp(xlo,xhi,ylo,yhi)
     CALL Pgwnad(x1,x2,y1,y2)
     CALL Pgbox("bcnt",0.0,1,"bcntv",0.0,1)

     WRITE (lbl,117) dh
     CALL Pgmtxt("b",3.5,0.5,0.5,lbl)

     CALL Pgsch(1.0)

     CALL Pgmtxt("t",1.5,0.45,1.0,lvar1)
     CALL Pgmtxt("t",1.5,0.47,0.0,"(x,y)")

     CALL Pgsci(2)
     zc(1:6)=(/ (dh*i,i=1,6) /)
     CALL Pgconx(cx,nx,ny,ix1,ix2,jy1,jy2,zc,6,pgxy)
     CALL Pgsci(4)
     zc(1:6)=(/ (dh*(i-7),i=1,6) /)
     CALL Pgconx(cx,nx,ny,ix1,ix2,jy1,jy2,zc,-6,pgxy)
     WRITE (6,110) lvar,dh
  END IF
#endif

  IF (iptype /= 1) THEN
     zc(1)=0.
     zc(2)=.16
     WRITE (6,100) lvar
     CALL Rcontr(0,zc,1,cx,nx,x,ix1,ix2,1,y,jy1,jy2,1,"rect")
  END IF

!...  label the plot

  IF (iptype == 2) WRITE(13,110) lvar,dh

  WRITE (6,120) x(ix1),x(ix2)
  WRITE (6,125) y(jy1),y(jy2)

!...  done

100 FORMAT(a4," contours:")
110 FORMAT(a4,"(x,y)... dz = ",g9.3)
117 FORMAT("\gD = ",g9.3)
120 FORMAT(g10.3," < x < ",g10.3)
125 FORMAT(g10.3," < y < ",g10.3)

END SUBROUTINE Ctrs

