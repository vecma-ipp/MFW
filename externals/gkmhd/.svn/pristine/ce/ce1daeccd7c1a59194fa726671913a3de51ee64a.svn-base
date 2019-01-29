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
        muc=nvars-2
        mux=nvars-1
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

