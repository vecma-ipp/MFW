SUBROUTINE Double

  USE Vars

  IMPLICIT NONE

  TYPE (FluxSurface), DIMENSION(:), ALLOCATABLE :: fsfc2

!...  allocate auxiliary fsfc2 and copy fsfc to it

  ALLOCATE(fsfc2(nx))
  DO i=1,nx
     j=fsfc(i)%ny

     ALLOCATE(fsfc2(i)%uu(0:n_neighbors,nvars,j))
     ALLOCATE(fsfc2(i)%puu(nvars,j))

     ALLOCATE(fsfc2(i)%area(j))
     ALLOCATE(fsfc2(i)%area2(j))
     ALLOCATE(fsfc2(i)%aratio(n_neighbors,j))

     ALLOCATE(fsfc2(i)%diag(j))
     ALLOCATE(fsfc2(i)%uuc(j))
     ALLOCATE(fsfc2(i)%uux(j))
     ALLOCATE(fsfc2(i)%amult(0:n_neighbors,j))

     ALLOCATE(fsfc2(i)%fuu(neqs,j))
     ALLOCATE(fsfc2(i)%fuu1(neqs,j))
     ALLOCATE(fsfc2(i)%fuu2(neqs,j))
     ALLOCATE(fsfc2(i)%suu(neqs,j))
     ALLOCATE(fsfc2(i)%suu1(neqs,j))
     ALLOCATE(fsfc2(i)%suu2(neqs,j))

     fsfc2(i)%uu=fsfc(i)%uu
     fsfc2(i)%puu=fsfc(i)%puu

     fsfc2(i)%area=fsfc(i)%area
     fsfc2(i)%area2=fsfc(i)%area2
     fsfc2(i)%aratio=fsfc(i)%aratio

     fsfc2(i)%diag=fsfc(i)%diag
     fsfc2(i)%uuc=fsfc(i)%uuc
     fsfc2(i)%uux=fsfc(i)%uux
     fsfc2(i)%amult=fsfc(i)%amult

     fsfc2(i)%fuu=fsfc(i)%fuu
     fsfc2(i)%fuu1=fsfc(i)%fuu1
     fsfc2(i)%fuu2=fsfc(i)%fuu2
     fsfc2(i)%suu=fsfc(i)%suu
     fsfc2(i)%suu1=fsfc(i)%suu1
     fsfc2(i)%suu2=fsfc(i)%suu2

  END DO

!...  deallocate original fsfc

  DO i=1,nx

     DEALLOCATE(fsfc(i)%uu)
     DEALLOCATE(fsfc(i)%puu)

     DEALLOCATE(fsfc(i)%area)
     DEALLOCATE(fsfc(i)%area2)
     DEALLOCATE(fsfc(i)%aratio)

     DEALLOCATE(fsfc(i)%diag)
     DEALLOCATE(fsfc(i)%uuc)
     DEALLOCATE(fsfc(i)%uux)
     DEALLOCATE(fsfc(i)%amult)

     DEALLOCATE(fsfc(i)%fuu)
     DEALLOCATE(fsfc(i)%fuu1)
     DEALLOCATE(fsfc(i)%fuu2)
     DEALLOCATE(fsfc(i)%suu)
     DEALLOCATE(fsfc(i)%suu1)
     DEALLOCATE(fsfc(i)%suu2)

  END DO

  DEALLOCATE(fsfc)

!...  allocate new doubled fsfc

  nx00=nx00*2

  ny00=4*nx00

  nx0=nx00
  ny0=ny00

  nx=nx0+2*ngdx
  ny=ny0+2*ngdy

  nxm=nx-1
  nxm2=nx-2
  nxm3=nx-3
  nxm4=nx-4

  nym=ny-1
  nym2=ny-2
  nym3=ny-3
  nym4=ny-4

  nk=nx00/2
  nl=ny00/2

  ALLOCATE(fsfc(nx))

  DO i=1,nx
     i0=i-ngdx
     j0=MAX(1, (i0-1)*6)
     fsfc(i)%ny0=j0
     j2=MAX(1, j0/2)
     fsfc(i)%ny2=j2+ngdy
     j=j0+2*ngdy
     fsfc(i)%ny=j

     hym=tpi/REAL(j0)
     fsfc(i)%hy=hym

     ALLOCATE(fsfc(i)%clocketa(j))
     ALLOCATE(fsfc(i)%eta(j))
     ALLOCATE(fsfc(i)%rtor(j))
     ALLOCATE(fsfc(i)%ztor(j))
     ALLOCATE(fsfc(i)%btor(j))
     ALLOCATE(fsfc(i)%theta(j))

     ALLOCATE(fsfc(i)%uu(0:n_neighbors,nvars,j))
     ALLOCATE(fsfc(i)%puu(nvars,j))

     ALLOCATE(fsfc(i)%area(j))
     ALLOCATE(fsfc(i)%area2(j))
     ALLOCATE(fsfc(i)%aratio(n_neighbors,j))

     ALLOCATE(fsfc(i)%diag(j))
     ALLOCATE(fsfc(i)%uuc(j))
     ALLOCATE(fsfc(i)%uux(j))
     ALLOCATE(fsfc(i)%amult(0:n_neighbors,j))

     ALLOCATE(fsfc(i)%fuu(neqs,j))
     ALLOCATE(fsfc(i)%fuu1(neqs,j))
     ALLOCATE(fsfc(i)%fuu2(neqs,j))
     ALLOCATE(fsfc(i)%suu(neqs,j))
     ALLOCATE(fsfc(i)%suu1(neqs,j))
     ALLOCATE(fsfc(i)%suu2(neqs,j))
  END DO

!...  do 1d arrays

  dvp1=ra
  i1=SIZE(ra)
  DEALLOCATE(ra)

  ALLOCATE(ra(nx))
  nx2=ngdx+1-mypex*nx0
  hx=1.0/REAL(nx00)
  ra=(/ (hx*(i-nx2),i=1,nx) /)

  CALL Doublex(dvp2,dvp1,i1,xx,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,rho,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,rho_tor,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,rho_vol,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,rho_cut,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,itor,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,jtor,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,ptor,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,qtor,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,vtor,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,psitor,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,phitor,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,fsavgr0,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,fsavgr1,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,fsavgr2,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,sshift,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,dpdpsi,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,diidpsi,ra,nx)
  CALL Doublex(dvp2,dvp1,i1,dphidv,ra,nx)

  i=ngdx+nx0+1
  j0=fsfc(i)%ny
  hym=fsfc(i)%hy
  j1=SIZE(eq%eta)
  DEALLOCATE(fsbndyeta)
  ALLOCATE(fsbndyeta(j0))
  fsbndyeta=(/ (hym*(j-ngdy-1),j=1,j0) /)

  CALL L3interp(eq%rtor,eq%eta,j1,fsbndyr,fsbndyeta,j0)
  CALL L3interp(eq%ztor,eq%eta,j1,fsbndyz,fsbndyeta,j0)

  DEALLOCATE(dvp0)
  DEALLOCATE(dvp1)
  DEALLOCATE(dvp2)

  DEALLOCATE(yy)
  DEALLOCATE(dvpy)
  DEALLOCATE(dvpy1)
  DEALLOCATE(dvpy2)
  DEALLOCATE(dvpy3)

  ALLOCATE(dvp0(nx))
  ALLOCATE(dvp1(nx))
  ALLOCATE(dvp2(nx))

  ALLOCATE(yy(ny))
  ALLOCATE(dvpy(6*nx00+2*ngdy))
  ALLOCATE(dvpy1(6*nx00+2*ngdy))
  ALLOCATE(dvpy2(6*nx00+2*ngdy))
  ALLOCATE(dvpy3(6*nx00+2*ngdy))

!...  fill new fsfc structure

  DO i=1,nx
     j=fsfc(i)%ny
     hym=fsfc(i)%hy
     fsfc(i)%eta=(/ (hym*(j0-ngdy-1),j0=1,j) /)
  END DO

  DO i0=3,nx0+1,2
     i1=(i0-1)/2+1
     i=i0+ngdx
     i2=i1+ngdx

     j2=fsfc2(i2)%ny
     j=fsfc(i)%ny

     CALL L3interp(fsfc2(i2)%rtor,fsfc2(i2)%eta,j2, &
          fsfc(i)%rtor,fsfc(i)%eta,j)
     CALL L3interp(fsfc2(i2)%ztor,fsfc2(i2)%eta,j2, &
          fsfc(i)%ztor,fsfc(i)%eta,j)
  END DO

  IF (mypex == 0) THEN
     DO i=1,1+ngdx
        fsfc(i)%rtor=raxis
        fsfc(i)%ztor=zaxis
     END DO
  END IF

  DO i0=1,nx0-1,2
     i=i0+ngdx
     i1=i+1
     i2=i+2

     j0=fsfc(i)%ny0/6

     DO mu=0,5

        j1=mu*j0+1+ngdy
        j2=mu*j0+j0+ngdy + 1

        DO j=j1,j2
           fsfc(i1)%rtor(j)=(fsfc(i)%rtor(j)+fsfc(i2)%rtor(j))/2.
           fsfc(i1)%ztor(j)=(fsfc(i)%ztor(j)+fsfc(i2)%ztor(j))/2.
        END DO
     END DO
  END DO

  DO i0=1,nx0
     i=i0+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy

     fsfc(i)%theta(j1:j2)= &
          ATAN2(fsfc(i)%ztor(j1:j2)-zaxis,fsfc(i)%rtor(j1:j2)-raxis)

     DO j=1,ngdy
        fsfc(i)%rtor(j)=fsfc(i)%rtor(j+j0)
        fsfc(i)%ztor(j)=fsfc(i)%ztor(j+j0)
        fsfc(i)%theta(j)=fsfc(i)%theta(j+j0)-tpi
     END DO
     DO j=j2+1,j2+ngdy
        fsfc(i)%rtor(j)=fsfc(i)%rtor(j-j0)
        fsfc(i)%ztor(j)=fsfc(i)%ztor(j-j0)
        fsfc(i)%theta(j)=fsfc(i)%theta(j-j0)+tpi
     END DO
  END DO

  DO i=1,nx
     j0=fsfc(i)%ny
     DO j=1,j0
        fsfc(i)%uu(0,mutheta,j)=fsfc(i)%theta(j)
        fsfc(i)%uu(0,nvars-2,j)=fsfc(i)%rtor(j)
        fsfc(i)%uu(0,nvars-1,j)=fsfc(i)%ztor(j)
        fsfc(i)%uu(0,nvars,j)=itor(i)/fsfc(i)%rtor(j)
     END DO
  END DO
  CALL Neighbors(nvars-2,nvars)
  CALL Loops

END SUBROUTINE Double


SUBROUTINE Doublex(dvp2,ra2,nx2,xx,ra,nx)
  IMPLICIT NONE
  INTEGER :: nx2,nx
  REAL, DIMENSION(:) :: dvp2,ra2,xx,ra
  dvp2=xx
  DEALLOCATE(xx)
  ALLOCATE(xx(nx))
  CALL L3interp(dvp2,ra2,nx2,xx,ra,nx)
END SUBROUTINE Doublex
