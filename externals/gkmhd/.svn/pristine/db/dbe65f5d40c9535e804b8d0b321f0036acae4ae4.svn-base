PROGRAM Plfxt

!...  fs avg flux versus radius and time

  USE Vars
  USE Pvars
  USE Coeff
  USE Plotins

  IMPLICIT NONE

  CHARACTER*64 :: lbl
  INTEGER :: ntmax,ipts

  INTEGER :: iframe,istr,iu,ieof,ipl
  INTEGER :: nfr
  REAL :: xx0,xx9
  REAL :: t0,t9,time
  REAL :: tavg,vex,vey,bflx,bfly,dnedx,dtedx,dtidx,fx,reyn

  INTEGER :: mu,i,i0,j,j1,j2,npts
  REAL :: xlo,xhi,ylo,yhi

  INCLUDE 'pages.h'

  CHARACTER*8 :: lvar

!...  which quantity

  lvar="Qe"
!.  lvar="RE"

!...  allocations

  INCLUDE 'pinit.h90'

  ntmax=20000

  ALLOCATE (pfx(20))
  ALLOCATE (pfxa(nx0,ntmax))

  ALLOCATE(xp(nx0))
  ALLOCATE(yp(ntmax))

  xp=xx(1+ngdx:nx-ngdx)

  xx0=xp(1)
  xx9=xp(nx0)

!...  zero everything

  npts=0

!...  initialise plotting

  CALL Plein("plfxt",1,1)

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

!...  accumulate profiles

     DO i0=1,nx0
	i=i0+ngdx
        pfx=0.
        DO j=1+ngdy,ny-ngdy

           INCLUDE 'vardef.h90'

           vex=drifty*(uu(j+1,i,1)-uu(j-1,i,1))
           vey=driftx*(uu(j,i+1,1)-uu(j,i-1,1))

           dnedx=hx2m*(uu(j,i+1,2)-uu(j,i-1,2))

           pfx(1)=pfx(1)+ne*vex
           pfx(2)=pfx(2)+vex*vey
           pfx(3)=pfx(3)+vor

        END DO

        pfx=pfx/(ny0)

        pfxa(i0,ipl)=pfx(1)

     END DO

     yp(ipl)=time

!...  end plot tasks

     t9=time

  END DO

190 ipl=ipl-1

  WRITE (6,100) t0,t9,ipl

!...  draw contours in x-t plane

  aspect = 4.

  IF (iptype == 2) THEN
     WRITE (13,112) xx0,xx9,t0,t9,aspect
  END IF

#ifdef PLPLOT
     IF (iptype == 1) THEN
     CALL Pladv(0)
     CALL Plvpas(0.,1.,0.,1.,512./768.)
     CALL Plwind(0.,1.,0.,1.)
     CALL Plcol(red)
     CALL Plbox("bc",0.0,1,"bc",0.0,1)

     CALL Plcol(black)
     CALL Plschr(0.,0.7)

     WRITE (lbl,115) t0,t9
     CALL Plmtex("t",-0.7,0.95,1.0,lbl)
     END IF
#endif

!.  nfr=100
  nfr=ipl

  DO i0=1,4
     j1=(nfr/4)*(i0-1)+1
     j2=j1+nfr/4-1
     CALL Ctrs(i0,pfxa(1,j1),nx0,nfr/4,xp,yp(j1),lvar,xx0,xx9,yp(j1),yp(j2))
  END DO

!...  aus

  CALL Plaus

100 FORMAT(" time = ",g9.3," to ",g9.3," with ",i4," plot steps")
112 FORMAT(" x1    x9    y1    y9    ny/nx",/,6x,5g12.4)
115 FORMAT("t = ",g9.3," -- ",g9.3)

END PROGRAM Plfxt


SUBROUTINE Ctrs(ipg,cx,nx,ny,x,y,lvar,x1,x2,y1,y2)

!...  plots in x-t plane, variable cx

  INTEGER :: ipg,nx,ny
  REAL :: x1,x2,y1,y2
  REAL :: cx(nx,ny),x(nx),y(ny),zc(13)
  CHARACTER*4 :: lvar
  CHARACTER*8 :: lvar1
  CHARACTER*48 :: lbl,lblx
  REAL :: xlo,xhi,ylo,yhi
  REAL :: xctr,yctr,dx2,dy2
  REAL, DIMENSION(4) :: xctrs,yctrs
  INCLUDE 'pages.h'
  COMMON/pctrl/iptype

!...  find grid limits: do ix1--ix2, jy1--jy2

  ix1=1
  ix2=nx
  jy1=1
  jy2=ny

!...  contour array already to hand

  dh=0.16*MAXVAL(ABS(cx))

!...  draw and label the map

#ifdef PLPLOT
  IF (iptype == 1) THEN
  ncases=4

  INCLUDE 'cbndydefs.h90'

  CALL Plschr(0.,0.7)

  CALL Plcol(15)
  CALL Plvpas(xlo,xhi,ylo,yhi,aspect)
  CALL Plwind(x1,x2,y1,y2)
  CALL Plbox("bcnt",0.0,1,"bcntv",0.0,1)

  SELECT CASE (lvar)
     CASE ("  Qe")
        lvar1="Q#de"
     CASE ("  RE")
        lvar1="R#dE"
     CASE (" vor")
        lvar1="#gW"
     CASE DEFAULT
        lvar1=lvar
  END SELECT

!.  WRITE (lbl,115) lvar//"(x,t)"
  WRITE (lbl,117) dh
!.  CALL Pllab(lblx," "," ")
  CALL Plmtex("b",3.5,0.5,0.5,lbl)

  CALL Plschr(0.,1.0)

  CALL Plmtex("t",1.5,0.45,1.0,lvar1)
  CALL Plmtex("t",1.5,0.47,0.0,"(x,t)")

!...  draw the contour lines

  CALL Plcol(1)
  zc(1:6)=(/ (dh*i,i=1,6) /)
  CALL Plcon1(cx,nx,ny,ix1,ix2,jy1,jy2,zc,6,x,y)
  CALL Plcol(9)
  zc(1:6)=(/ (dh*(i-7),i=1,6) /)
  CALL Plcon1(cx,nx,ny,ix1,ix2,jy1,jy2,zc,6,x,y)
  WRITE (6,110) lvar,dh
  END IF
#endif

  IF (iptype == 2) THEN
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
110 FORMAT(a4,"(x,t)... dz = ",g9.3)
!. 115 FORMAT(a16)
115 FORMAT(a4,"(x,t)")
!. 117 FORMAT("dz = ",g9.3)
117 FORMAT("#gD = ",g9.3)
120 FORMAT(g10.3," < x < ",g10.3)
125 FORMAT(g10.3," < t < ",g10.3)

END SUBROUTINE Ctrs

