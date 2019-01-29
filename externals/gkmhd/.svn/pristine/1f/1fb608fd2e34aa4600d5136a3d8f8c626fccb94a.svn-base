PROGRAM Plctrc

!...  simple 2D ctr movies 4 to a page
!...  read twice to get the max and min

!...  color temperature version, works with IDL only
!...  uses IDLs triangles infrastructure

  USE Coeff
  USE Pvars
  USE Vars
  USE Plotins

  IMPLICIT NONE

  INTEGER :: iu,i,j,i0,i1,j0,j1,j2,mu,ipl,iframe,istr,ieof
  REAL :: xx0,yy0,xx9,yy9,time

  CHARACTER*32 :: lbl

  INCLUDE 'pages.h'

  INTEGER :: ireads
  INTEGER, PARAMETER :: npics=4
  REAL, DIMENSION(npics) :: uumax,uumin
  CHARACTER*8 :: lvars(npics),lvars1(npics)
  DATA lvars/"phi","ne","he","vor"/

!...  allocations

  INCLUDE 'pinit.h90'

  i1=0
  DO i0=1,nx0
     i=i0+ngdx
     i1=i1+fsfc(i)%ny0
  END DO

  ALLOCATE(xp(i1))
  ALLOCATE(yp(i1))
  ALLOCATE(cx(i1,npics))

  uumax=xzero
  uumin=-xzero

  i1=0
  DO i0=1,nx0
     i=i0+ngdx
     j0=fsfc(i)%ny0
     xp(i1+1:i1+j0)=fsfc(i)%rtor(ngdy+1:ngdy+j0)
     yp(i1+1:i1+j0)=fsfc(i)%ztor(ngdy+1:ngdy+j0)
     i1=i1+j0
  END DO

  xx0=MINVAL(xp)
  xx9=MAXVAL(xp)
  yy0=MINVAL(yp)
  yy9=MAXVAL(yp)

!...  initialise plotting

  IF (iptype /= 2) THEN
     WRITE (6,*) ' these color smoke pics work only in IDL...'
     WRITE (6,*) ' triangles work only in IDL...'
     WRITE (6,*) ' setting iptype to 2 and hoping for the best!'
  END IF

  iptype = 2

  CALL Plein("plctrc",1,1)

  aspect = (yy9-yy0)/(xx9-xx0)

  IF (iptype == 2) THEN
     WRITE (13,112) xx0,xx9,yy0,yy9,aspect,npics
  END IF

!...  start sampling

  DO ireads=1,2
  DO iu=21,20+nfiles
     REWIND iu
  END DO
  iu=21

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

     i1=0
     DO i0=1,nx0
        i=i0+ngdx
        j1=fsfc(i)%ny0
        DO j0=1,j1
           j=j0+ngdy

           INCLUDE 'vardefp.h90'

           cx(i1+j0,1)=phi
           cx(i1+j0,2)=ne
           cx(i1+j0,3)=he
           cx(i1+j0,4)=vor
        END DO

        i1=i1+j1
     END DO

!...  find the extrema

     IF (ireads == 1) THEN

        DO mu=1,npics
           uumax(mu)=MAX(uumax(mu),MAXVAL(ABS(cx(:,mu))))
        END DO

     END IF

!...  put out normalised vars

     IF (ireads == 2) THEN

        WRITE (13,113) time

        i1=SIZE(xp)

        WRITE (13,100) i1
        WRITE (13,220) (xp(i),i=1,i1)
        WRITE (13,220) (yp(i),i=1,i1)
        DO mu=1,npics
           cx(:,mu)=uumax(mu)*(cx(:,mu)-uumin(mu))

           WRITE (13,200) mu
           WRITE (13,210) (FLOOR(cx(i,mu)),i=1,i1)
!           WRITE (13,220) (cx(i,mu),i=1,i1)

        END DO

!...  write time

        WRITE (6,110) time

     END IF

!...  end plot tasks

  END DO
190 CONTINUE

  IF (ireads == 1) THEN

     uumin=-uumax
     DO mu=1,npics
        WRITE (13,150) mu,uumax(mu),uumin(mu)
        uumax(mu)=254.99/(uumax(mu)-uumin(mu))
     END DO

  END IF

!...  end twice read loop

  END DO

!...  aus

  CALL Plaus

100 FORMAT("array strip dim",/,i5)
110 FORMAT("time = ",g10.4)
112 FORMAT(" x1    x9    y1    y9    ny/nx    npics",/,6x,5g12.4,i8)
113 FORMAT(" t = ",g9.3)
150 FORMAT("var     max     min",/,i4,2g12.4)
200 FORMAT("var",/,i5)
210 FORMAT(20i4)
220 FORMAT(5f15.7)
!.210 FORMAT(80z1)

END PROGRAM Plctrc
