PROGRAM Plcuts

!...  simple 1D profile cuts at z = 0

  USE Coeff
  USE Pvars
  USE Vars
  USE Plotins

  IMPLICIT NONE

  INTEGER :: iu,i,j,i0,i1,i2,j0,j1,j2,ip,im,np,mu,ipl,iframe,istr,ieof
  REAL :: xx0,xx9,time
  REAL :: hx0

  CHARACTER*32 :: lbl

  INTEGER, PARAMETER :: npics=4
  CHARACTER*8 :: lvars(npics),lvars1(npics)
  DATA lvars/"ne","phi","vey","vor"/
  DATA lvars1/"n\de\u","\gf","V\uy","\gW"/

!...  allocations

  INCLUDE 'pinit.h90'

!  lvars(3)="ni"
!  lvars1(3)="n\di\u"

  np=2*nx0-1
  ALLOCATE(xp(np))
  ALLOCATE(cx(np,npics))

  xx0=-1./delta
  xx9=1./delta

!...  initialise plotting

  CALL Plein("plcut",1,1)

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

     INCLUDE 'getpvars.h90'

!...  accumulate profiles

     DO i0=1,nx0
        i=i0+ngdx

        j0=fsfc(i)%ny0
        j1=ngdy+1
        j2=ngdy+1+j0/2

        j=j1
        i1=nx0-1+i0
        xp(i1)=(fsfc(i)%rtor(j)-rr0)/delta
        INCLUDE 'vardefp.h90'
        cx(i1,1)=ne
        cx(i1,2)=phi
!        cx(i1,3)=ni
!        cx(i1,3)=hx2m*(fsfc(i+1)%fuu(muphi,j)-fsfc(i-1)%fuu(muphi,j))
        cx(i1,4)=vor

        j=j2
        i2=nx0+1-i0
        xp(i2)=(fsfc(i)%rtor(j)-rr0)/delta
        INCLUDE 'vardefp.h90'
        cx(i2,1)=ne
        cx(i2,2)=phi
!        cx(i2,3)=ni
!        cx(i2,3)=hx2m*(fsfc(i+1)%fuu(muphi,j)-fsfc(i-1)%fuu(muphi,j))
        cx(i2,4)=vor

     END DO

     DO i=1,np
        ip=MIN(i+1,np)
        im=MAX(i-1,1)
        hx0=driftx/(xp(ip)-xp(im))
        cx(i,3)=hx0*(cx(ip,2)-cx(im,2))
     END DO
!     cx(1,3)=2.*cx(2,3)-cx(3,3)
!     cx(np,3)=2.*cx(np-1,3)-cx(np-2,3)

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

  DO i=1,npics
     CALL Prof(i,cx(1,i),np,xp,lvars(i),lvars1(i),xx0,xx9,npics)
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

END PROGRAM Plcuts


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
