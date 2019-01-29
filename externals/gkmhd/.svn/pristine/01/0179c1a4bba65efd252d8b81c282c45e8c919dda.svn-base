PROGRAM Plderiv

!...  poloidal plane 2D ctrs of psi derivatives to check signs
!...  draws ctrs in each of 3 main quadrangles

  USE Coeff
  USE Pvars
  USE Vars
  USE Plotins

  IMPLICIT NONE

  INTEGER :: iu,i,j,i0,i1,j0,j1,j2,mu,nu,ipl,iframe,istr,ieof,iiter
  REAL :: xx0,yy0,xx9,yy9,time
  INTEGER :: m0,mm,mp
  REAL :: uur,uuz,thxm

  CHARACTER*32 :: lbl

  INCLUDE 'pages.h'

  INTEGER, PARAMETER :: npics=2
  CHARACTER*8 :: lvars(npics),lvars1(npics)
  DATA lvars/"uR","uZ"/
  DATA lvars1/"u\dR","u\dZ"/

!...  allocations

  INCLUDE 'pinit.h90'

  n_neighbors = 6

  DO i=1,nx
     j=fsfc(i)%ny
     ALLOCATE(fsfc(i)%uu(0:n_neighbors,nvars,j))
     ALLOCATE(fsfc(i)%area(j))
     ALLOCATE(fsfc(i)%area2(j))
     ALLOCATE(fsfc(i)%aratio(n_neighbors,j))
     ALLOCATE(fsfc(i)%diag(j))
     ALLOCATE(fsfc(i)%uuc(j))
     ALLOCATE(fsfc(i)%uux(j))
     ALLOCATE(fsfc(i)%amult(0:n_neighbors,j))
  END DO
  ALLOCATE(duu(0:n_neighbors,nvars))
  ALLOCATE(puu(nvars))

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
     WRITE (13,112) xx0,xx9,yy0,yy9,aspect
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

!.     CALL Strip

!     INCLUDE 'getpvars.h90'

     INCLUDE 'readprofs.h90'

     DO i=1,nx
        DO j=1,fsfc(i)%ny
           DO mu=1,nvars
              fsfc(i)%uu(0,mu,j)=fsfc(i)%fuu(mu,j)
           END DO
        END DO
     END DO

     CALL Neighbors(1,nvars)
     CALL Loops

!...  evaluate shear -- components of world line field put in 5,6

     i1=0
     DO i0=1,nx0
        i=i0+ngdx
        DO j0=1,fsfc(i)%ny0
           j=j0+ngdy

           INCLUDE 'vardefp.h90'

           thxm=rtor/(rr0*rr0)
           thxm=thxm/fsfc(i)%area2(j)

           duu(:,:)=fsfc(i)%uu(:,:,j)
           puu=0.

           DO m0=1,6
              mp=m0+1
              mm=m0-1
              IF (m0 == 6) mp=1
              IF (m0 == 1) mm=6
              puu(5)=puu(5) - thxm*( &
                   - (duu(mp,mupsi)-duu(mm,mupsi))*duu(m0,nvars-4) )
              puu(6)=puu(6) - thxm*( &
                   - (duu(mp,mupsi)-duu(mm,mupsi))*duu(m0,nvars-3) )
           END DO
           fsfc(i)%fuu(5:6,j)=puu(5:6)
        END DO
     END DO

     CALL Bndysp(5,6)

!...  load ctrs

     i1=0
     DO i0=1,nx0
        i=i0+ngdx
        j1=fsfc(i)%ny0
        DO j0=1,j1
           j=j0+ngdy

!           INCLUDE 'vardefp.h90'

           rtor=fsfc(i)%fuu(nvars-4,j)
           ztor=fsfc(i)%fuu(nvars-3,j)
           uur=fsfc(i)%fuu(5,j)
           uuz=fsfc(i)%fuu(6,j)

           cx(i1+j0,1)=rtor
           cx(i1+j0,2)=ztor
           cx(i1+j0,3)=uur
           cx(i1+j0,4)=uuz
        END DO

        i1=i1+j1
     END DO

!     write (0,*) 'load cxa'

     DO mu=1,npics+2
        i1=0
        DO i0=1,nx0
           j2=2*(i0-1)
           DO j0=1,i0
              cxa(i0,j0,mu)=cx(i1+j0,mu)
!              if (i0 == nx0) write (0,*) 'j = ',j0,'  jp = ',i1+j0
              cxa(j0,i0,mu)=cx(i1+2*i0-j0,mu)
!              if (i0 == nx0) write (0,*) 'j = ',j0,'  jp = ',i1+2*i0-j0
              cxa(i0,nx0+j0,mu)=cx(i1+j2+j0,mu)
!              if (i0 == nx0) write (0,*) 'j = ',j0,'  jp = ',i1+j2+j0
              cxa(j0,nx0+i0,mu)=cx(i1+j2+2*i0-j0,mu)
!              if (i0 == nx0) write (0,*) 'j = ',j0,'  jp = ',i1+j2+2*i0-j0
              cxa(i0,2*nx0+j0,mu)=cx(i1+2*j2+j0,mu)
!              if (i0 == nx0) write (0,*) 'j = ',j0,'  jp = ',i1+2*j2+j0
              cxa(j0,2*nx0+i0,mu)=cx(i1+2*j2+2*i0-j0,mu)
!              if (i0 == nx0) write (0,*) 'j = ',j0,'  jp = ',i1+2*j2+2*i0-j0
           END DO
           i1=i1+MAX(1,6*(i0-1))
!           write (0,*) 'done i = ',i0,' i1 = ',i1
        END DO
     END DO

!     write (0,*) 'loaded cxa'

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
        CALL Pgsls(3)
        CALL Pgsci(green)
        CALL Line("dash",rr0,yy0,rr0,yy9)
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
!        END IF
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

END PROGRAM Plderiv


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

SUBROUTINE Bndysp(mu1,mu9)

!...  on fuu within fsfc structure

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: mu1,mu9

  INTEGER :: i,j,i1,i2,i3,i4,j0,j1

!...  periodic for y

  IF (ngdy > 0) THEN
     DO i=1+ngdx,nx0+ngdx
        j0=fsfc(i)%ny0
        j1=j0+ngdy
        DO j=1,ngdy
           fsfc(i)%fuu(mu1:mu9,j)=fsfc(i)%fuu(mu1:mu9,j0+j)
           fsfc(i)%fuu(mu1:mu9,j1+j)=fsfc(i)%fuu(mu1:mu9,j1+j-j0)
        END DO
     END DO
  END IF

!...  Neumann/Dirichlet for x

  DO i=1,ngdx
     i1=ngdx+1
     fsfc(i)%fuu(mu1:mu9,:)=fsfc(i1)%fuu(mu1:mu9,:)
  END DO

  i1=nx0+ngdx
  DO i=i1+1,i1+ngdx
     fsfc(i)%fuu(mu1:mu9,:)=0.
  END DO

  DO i=1,nx
     fsfc(i)%uu(0,mu1:mu9,:)=fsfc(i)%fuu(mu1:mu9,:)
  END DO
  CALL Neighbors(mu1,mu9)

END SUBROUTINE Bndysp

SUBROUTINE Neighbors(mu1,mu9)

!...  load neighbors for memory local derivatives

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: mu1,mu9

  INTEGER :: i,j,i0,i1,i2,j0,j1,j2,mu

!...  3D models do parallel direction communication

!...  load drift plane neighbors

  i1=1
  IF (mypex == 0) THEN
     i1=1+ngdx
     i2=i1+1
     j1=1+ngdy
     DO mu=1,6
        j=mu+ngdy
        fsfc(i1)%uu(mu,mu1:mu9,j1)=fsfc(i2)%uu(0,mu1:mu9,j)
     END DO
     i1=2
  END IF

  DO i0=i1,nx0
     i=i0+ngdx
     j0=fsfc(i)%ny0/6

     DO mu=0,5

        j1=mu*j0+1+ngdy
        j2=mu*j0+j0+ngdy

        DO j=j1,j2

           fsfc(i)%uu(1,mu1:mu9,j)=fsfc(i)%uu(0,mu1:mu9,j+1)
           fsfc(i)%uu(4,mu1:mu9,j)=fsfc(i)%uu(0,mu1:mu9,j-1)

           fsfc(i)%uu(5,mu1:mu9,j)=fsfc(i+1)%uu(0,mu1:mu9,j+mu)
           fsfc(i)%uu(6,mu1:mu9,j)=fsfc(i+1)%uu(0,mu1:mu9,j+1+mu)

           fsfc(i+1)%uu(2,mu1:mu9,j+mu)=fsfc(i)%uu(0,mu1:mu9,j)
           fsfc(i+1)%uu(3,mu1:mu9,j+1+mu)=fsfc(i)%uu(0,mu1:mu9,j)
        END DO
        fsfc(i)%uu(2,mu1:mu9,j2)=fsfc(i-1)%uu(0,mu1:mu9,j2-mu)
        fsfc(i)%uu(3,mu1:mu9,j1)=fsfc(i)%uu(0,mu1:mu9,j1-1)
        fsfc(i)%uu(4,mu1:mu9,j1)=fsfc(i+1)%uu(0,mu1:mu9,j1+mu-1)
!        fsfc(i+1)%uu(2,mu1:mu9,j2+mu+1)=fsfc(i)%uu(0,mu1:mu9,j2+1)
!        fsfc(i+1)%uu(3,mu1:mu9,j1+mu)=fsfc(i+1)%uu(0,mu1:mu9,j1+mu-1)
     END DO
  END DO

END SUBROUTINE Neighbors


SUBROUTINE Loops

!...  loop geometry

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: i,j,j0,j1,j2
  INTEGER :: muc,mux,m0,mp,mm
  REAL :: area,area2,aratio,dr1,dz1,dr2,dz2
  REAL :: det,drdn,drdx,dzdn,dzdx,dndr,dndz,dxdr,dxdz,wwa,wwb

  DO i=1+ngdx,nx0+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy

     fsfc(i)%aratio=0.

     DO j=j1,j2
        muc=nvars-4
        mux=nvars-3
        duu(:,muc:mux)=fsfc(i)%uu(:,muc:mux,j)
        area=0.
        area2=0.
        DO m0=1,6
           mp=m0+1
           mm=m0-1
           IF (m0 == 6) mp=1
           IF (m0 == 1) mm=6

           drdn=duu(m0,muc)-duu(0,muc)
           drdx=duu(mp,muc)-duu(0,muc)
           dzdn=duu(m0,mux)-duu(0,mux)
           dzdx=duu(mp,mux)-duu(0,mux)

           det=1./(drdn*dzdx-drdx*dzdn)
           dndr=dzdx*det
           dndz=-drdx*det
           dxdr=-dzdn*det
           dxdz=drdn*det
           wwa=(dzdx-dzdn)*dndr-(drdx-drdn)*dndz
           wwb=(dzdx-dzdn)*dxdr-(drdx-drdn)*dxdz
           fsfc(i)%aratio(m0,j)=fsfc(i)%aratio(m0,j)+wwa
           fsfc(i)%aratio(mp,j)=fsfc(i)%aratio(mp,j)+wwb

           area2=area2 + duu(m0,muc)*duu(mp,mux) - duu(mp,muc)*duu(m0,mux)
!           area2=area2 - (duu(mp,muc)-duu(mm,muc))*duu(m0,mux)
        END DO

        IF (area2 < 0.) THEN
           WRITE (0,*) 'negative loop area  2x area =',area2
           WRITE (0,*) 'i j = ',i,j
           STOP
        END IF

        fsfc(i)%area2(j)=area2/2.
        fsfc(i)%area(j)=area2/6.
     END DO
     fsfc(i)%aratio=fsfc(i)%aratio/2.
  END DO

END SUBROUTINE Loops

