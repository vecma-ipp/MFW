!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        SUBROUTINE lsq_sur6(r,z,u,n,c,rm,zm,um,dp)
C
C...    list square surf. fitting
C       c(1)+c(2)*(r-r1)+c(3)*(z-z1)+c(4)*(r-r1)**2+c(5)*(z-z1)**2+c(6)*(r-r1)*(z-z1)
C
        IMPLICIT REAL*8(A-H,O-Z)
C
        DIMENSION r(*),z(*),u(*),dp(*)
        DIMENSION A(6,6),B(6),c(6)
        DIMENSION IP(6)
!!!test        DIMENSION u_dum(*)

         SQRT(xx)=DSQRT(xx)

!!!test  
!
!      do i=1,n
!      u(i)=
!     &  1.d0
!     & +2.d0*(r(i)-r(1))
!     & +3.d0*(z(i)-z(1))
!     & +4.d0*(r(i)-r(1))**2
!     & +5.d0*(z(i)-z(1))**2
!     & +6.d0*(r(i)-r(1))*(z(i)-z(1))
!      enddo
!
!!!test  

         s_r= 0.d0
         s_z= 0.d0
         s_r2=0.d0
         s_z2=0.d0
         s_rz=0.d0


         s_r3= 0.d0
         s_rz2=0.d0
         s_r2z=0.d0

         s_z3=0.d0

         s_r4=  0.d0
         s_r2z2=0.d0
         s_r3z= 0.d0

         s_z4= 0.d0
         s_rz3=0.d0

         s_u= 0.d0
         s_ur=0.d0
         s_uz=0.d0
         s_ur2= 0.d0
         s_uz2=0.d0
         s_urz=0.d0


        do k=1,n

         s_r=  s_r + (r(k)-r(1))
         s_z=  s_z + (z(k)-z(1))
         s_r2= s_r2 + (r(k)-r(1))**2
         s_z2= s_z2 + (z(k)-z(1))**2
         s_rz= s_rz + (r(k)-r(1))*(z(k)-z(1))
         s_u=  s_u +u(k)


         s_r3=  s_r3 + (r(k)-r(1))**3
         s_rz2= s_rz2 + (r(k)-r(1))*(z(k)-z(1))**2
         s_r2z= s_r2z + (r(k)-r(1))**2*(z(k)-z(1))
         s_ur=  s_ur + u(k)*(r(k)-r(1))

         s_z3= s_z3 + (z(k)-z(1))**3
         s_uz= s_uz + u(k)*(z(k)-z(1))

         s_r4=   s_r4 + (r(k)-r(1))**4
         s_r2z2= s_r2z2 + (r(k)-r(1))**2*(z(k)-z(1))**2
         s_r3z=  s_r3z +  (r(k)-r(1))**3*(z(k)-z(1))
         s_ur2=  s_ur2 +  u(k)*(r(k)-r(1))**2

         s_z4=  s_z4 + (z(k)-z(1))**4
         s_rz3= s_rz3 + (r(k)-r(1))*(z(k)-z(1))**3
         s_uz2= s_uz2 + u(k)*(z(k)-z(1))**2 

         s_urz= s_urz + u(k)*(r(k)-r(1))*(z(k)-z(1))

        enddo

C   THE CREATION THE MATRIX A and RIGHT HAND B:

         A(1,1) = dfloat(n)
         A(1,2) = s_r
         A(1,3) = s_z
         A(1,4) = s_r2
         A(1,5) = s_z2
         A(1,6) = s_rz

         B(1) = s_u

         A(2,1) = s_r
         A(2,2) = s_r2
         A(2,3) = s_rz
         A(2,4) = s_r3
         A(2,5) = s_rz2
         A(2,6) = s_r2z

         B(2) = s_ur

         A(3,1) = s_z
         A(3,2) = s_rz
         A(3,3) = s_z2
         A(3,4) = s_r2z
         A(3,5) = s_z3
         A(3,6) = s_rz2

         B(3) = s_uz

         A(4,1) = s_r2
         A(4,2) = s_r3
         A(4,3) = s_r2z
         A(4,4) = s_r4
         A(4,5) = s_r2z2
         A(4,6) = s_r3z

         B(4) = s_ur2

         A(5,1) = s_z2
         A(5,2) = s_rz2
         A(5,3) = s_z3
         A(5,4) = s_r2z2
         A(5,5) = s_z4
         A(5,6) = s_rz3

         B(5) = s_uz2

         A(6,1) = s_rz
         A(6,2) = s_r2z
         A(6,3) = s_rz2
         A(6,4) = s_r3z
         A(6,5) = s_rz3
         A(6,6) = s_r2z2

         B(6) = s_urz

!       do i=1,6
!          amx=0.d0
!
!         do j=1,6
!          if( dabs(a(i,j)) .gt. amx ) then
!           amx=dabs(a(i,j))
!          endif 
!         enddo
!
!         do j=1,6
!           a(i,j)=a(i,j)/amx
!         enddo
!
!          b(i)=b(i)/amx
!
!       enddo

C  THE SOLUTION THE MATRIX EQUATION Ac=B.

         CALL GE(6,6,A,B,c,IP)

c   magnetic axis

         det=4.d0*c(4)*c(5)-c(6)**2

         det_r=-2.d0*c(2)*c(5)+c(6)*c(3)
         det_z=-2.d0*c(3)*c(4)+c(6)*c(2)

         rm=det_r/det
         zm=det_z/det

         um=c(1)+c(2)*rm+c(3)*zm+c(4)*rm**2+c(5)*zm**2+c(6)*rm*zm

         rm=rm+r(1)
         zm=zm+z(1)

         dudr=c(2)
         dudz=c(3)
         dudr2=2.d0*c(4)
         dudz2=2.d0*c(5)
         dudrdz=c(6)

         dp(1)=dudr
         dp(2)=dudz
         dp(3)=dudr2
         dp(4)=dudrdz
         dp(5)=dudz2


         RETURN
         END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        SUBROUTINE DERIV5(X,Y,F,M,N,U)
C
C...    DEFINITON OF THE FIRST AND SECOND DERIVATIONS.
C
C
        IMPLICIT REAL*8(A-H,O-Z)
C
        DIMENSION X(*),Y(*),F(*),U(*)
        DIMENSION A(5,5),B(5),S(5)
c        DIMENSION AA(5,5),W1(5),W2(5)
        DIMENSION IP(5)
C
         SQRT(R)=DSQRT(R)
C
C
            SDX2  = 0.d0
            SDY2  = 0.d0
            SDXDY = 0.d0
            SDX3  = 0.d0
            SDX2DY= 0.d0
            SDXDY2= 0.d0
            SDY3  = 0.d0
            SDX4  = 0.d0
            SDX3DY= 0.d0
            SDX2Y2= 0.d0
            SDXDY3= 0.d0
            SDY4  = 0.d0
C
            DAVER2= 0.d0
C
            FDX   = 0.d0
            FDY   = 0.d0
            FDX2  = 0.d0
            FDXDY = 0.d0
            FDY2  = 0.d0
C
C  THE MAIN LOOP
C
         DO 1 I=2,M
            DX=X(I)-X(1)
            DY=Y(I)-Y(1)
            DF=F(I)-F(1)
            DX2=DX*DX
            DY2=DY*DY
C
            SDX2  =SDX2   + DX2
            SDY2  =SDY2   + DY2
            SDXDY =SDXDY  + DX   *DY
            SDX3  =SDX3   + DX2  *DX
            SDX2DY=SDX2DY + DX2  *DY
            SDXDY2=SDXDY2 + DX   *DY2
            SDY3  =SDY3   + DY2  *DY
            SDX4  =SDX4   + DX2  *DX2
            SDX3DY=SDX3DY + DX2  *DX  *DY
            SDX2Y2=SDX2Y2 + DX2  *DY2
            SDXDY3=SDXDY3 + DX   *DY2 *DY
            SDY4  =SDY4   + DY2  *DY2
C
            DAVER2=DAVER2 + DX2  +   DY2
C
            FDX   =FDX    + DF   *DX
            FDY   =FDY    + DF   *DY
            FDX2  =FDX2   + DF   *DX2
            FDXDY =FDXDY  + DF   *DX  *DY
            FDY2  =FDY2   + DF   *DY2
C
 1       CONTINUE
C
C   THE CREATION THE MATRIX A:
C
         A(1,1) = SDX2
         A(1,2) = SDXDY
         A(1,3) = SDX3   * 0.5d0
         A(1,4) = SDX2DY
         A(1,5) = SDXDY2 * 0.5d0
         A(2,1) = SDXDY
         A(2,2) = SDY2
         A(2,3) = SDX2DY * 0.5d0
         A(2,4) = SDXDY2
         A(2,5) = SDY3   * 0.5d0
         A(3,1) = SDX3
         A(3,2) = SDX2DY
         A(3,3) = SDX4   * 0.5d0
         A(3,4) = SDX3DY
         A(3,5) = SDX2Y2 * 0.5d0
         A(4,1) = SDX2DY
         A(4,2) = SDXDY2
         A(4,3) = SDX3DY * 0.5d0
         A(4,4) = SDX2Y2
         A(4,5) = SDXDY3 * 0.5d0
         A(5,1) = SDXDY2
         A(5,2) = SDY3
         A(5,3) = SDX2Y2 * 0.5d0
         A(5,4) = SDXDY3
         A(5,5) = SDY4   * 0.5d0
C
C   THE CREATION THE RIGHT HAND B:
C
         B(1) = FDX
         B(2) = FDY
         B(3) = FDX2
         B(4) = FDXDY
         B(5) = FDY2
C
C  THE WEIGHTS S(I):
C
         DAVER=SQRT(DAVER2)
C
         S(1) = 1.d0/SQRT(SDX2)/DAVER
         S(2) = 1.d0/SQRT(SDY2)/DAVER
         S(3) = 1.d0/SDX2/DAVER
         S(4) = 1.d0/SQRT(SDX2*SDY2)/DAVER
         S(5) = 1.d0/SDY2/DAVER
C
         DO 2 I=1,N
         DO 3 J=1,N
            A(I,J)=S(I)*A(I,J)
 3       CONTINUE
            B(I)=S(I)*B(I)
 2       CONTINUE
C
C  THE SOLUTION THE MATRIX EQUATION AU=B.
C
         IFAIL=0
C
ccccc    CALL F04ATF(A,5,B,N,U,AA,5,W1,W2,IFAIL)
         CALL GE(5,5,A,B,U,IP)
C
ccccc    IF( IFAIL .NE. 0 ) WRITE (6,*)' IFAIL= ',IFAIL
C
         RETURN
         END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C...    CUBIC FIT
C
        SUBROUTINE DERIV9(X,Y,F,M,N,U)
C
        IMPLICIT REAL*8(A-H,O-Z)
C
        DIMENSION X(1:M),Y(1:M),F(1:M),U(1:N)
        DIMENSION A(1:9,1:9),B(1:9)
        DIMENSION IP(1:9)
!        DIMENSION AA(1:100,1:9),BB(1:100)
        REAL*8, ALLOCATABLE :: AA(:,:),BB(:)
C
C...    DIMENSION CHECK
!        IF(M.GT.100) THEN
!          WRITE(6,*) '     DERIV9: M EXCEEDS 100'
!          STOP
!        ENDIF
        IF (.NOT. ALLOCATED(AA)) ALLOCATE(AA(M,9))
        IF (.NOT. ALLOCATED(BB)) ALLOCATE(BB(M))
C...    THE MAIN LOOP
C
        DO I=1,M-1
          DX=X(I+1)-X(1)
          DY=Y(I+1)-Y(1)
          DF=F(I+1)-F(1)
          DXX=.5*DX*DX
          DXY=   DX*DY
          DYY=.5*DY*DY
C
          AA(I,1)=DX
          AA(I,2)=DY
          AA(I,3)=DXX
          AA(I,4)=DXY
          AA(I,5)=DYY
          !cubic
          DXXX=   DX*DX*DX
          DXXY=   DX*DX*DY
          DXYY=   DX*DY*DY
          DYYY=   DY*DY*DY
          AA(I,6)=DXXX
          AA(I,7)=DXXY
          AA(I,8)=DXYY
          AA(I,9)=DYYY
C
          BB(I)=DF
C
        ENDDO
C
C...     THE CREATION THE MATRIX A:
C
         DO K=1,9
           DO L=1,9
             AKL=0.
             DO I=1,M-1
               AKL=AKL+AA(I,K)*AA(I,L)
             ENDDO
             A(K,L)=AKL
           ENDDO
         ENDDO
C
C...     THE CREATION THE RIGHT HAND B:
C
         DO K=1,9
           BK=0.
           DO I=1,M-1
             BK=BK+AA(I,K)*BB(I)
           ENDDO
           B(K)=BK
         ENDDO
C         
         DEALLOCATE(AA,BB)   
C
C...     THE SOLUTION THE MATRIX EQUATION AU=B.
C
         IFAIL=0
C
         CALL GE(N,9,A,B,U,IP)
C
         IF( IFAIL .NE. 0 ) WRITE (6,*)' DERIV9:IFAIL= ',IFAIL
C
         RETURN
         END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        SUBROUTINE GE(N,NZ,A,X,Y,IP)
C
C...    GAUSS ELIMINATION
C
        IMPLICIT REAL*8(A-H,O-Z)
C
        DIMENSION A(1:NZ,1:NZ),X(1:N),Y(1:N),IP(1:N)
C
        ABS(XARG)=DABS(XARG)
C
        DO 90 I=1,N
          IP(I)=I
 90     CONTINUE
C
        DO 100 I=1,N-1
C...      MAX ROW ELEMENT SEARCH
          RM=ABS(A(I,I))
          JM=I
          DO 101 J=I,N
            ABA=ABS(A(I,J))
            IF(ABA.GT.RM) THEN
              RM=ABA
              JM=J
            ENDIF
 101      CONTINUE
C...      PERMUTATIONS
          IPE=IP(I)
          IP(I)=IP(JM)
          IP(JM)=IPE
          DO 102 K=1,N
            APE=A(K,I)
            A(K,I)=A(K,JM)
            A(K,JM)=APE
 102      CONTINUE
          AD=1.d0/A(I,I)
          DO 100 K=I+1,N
            AM=A(K,I)*AD
            X(K)=X(K)-AM*X(I)
            DO 100 J=I,N
              A(K,J)=A(K,J)-AM*A(I,J)
 100    CONTINUE
C
        Y(N)=X(N)/A(N,N)
        DO 200 I=N-1,1,-1
          AD=1./A(I,I)
          Y(I)=X(I)
          DO 201 J=N,I+1,-1
            Y(I)=Y(I)-A(I,J)*Y(J)
 201      CONTINUE
          Y(I)=Y(I)*AD
 200    CONTINUE
C
C...    BACK PERMUTATION
        DO 300 I=1,N
          X(IP(I))=Y(I)
 300    CONTINUE
        DO 400 I=1,N
          Y(I)=X(I)
 400    CONTINUE
C
        RETURN
        END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
      REAL*8 FUNCTION GREENI(R,Z,RP,ZP)
C  GREEN'S FUNCTION FOR TOROIDAL CURRENT LOOP IN INFINITY AREA.
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      REAL*8 MMDELK,MMDELE
C
      SQRT(X)=DSQRT(X)
C
      T=SQRT(4.d0*R*RP/((R+RP)**2+(Z-ZP)**2))
C
C CALCULATION OF COMPLETE ELLIPTIC INTEGRALS OF 1TH AND 2TH KIND.
C
         TT=(1.d0-T**2)
C
C THE FUNCTIONS MMDELK, MMDELE FROM IMSL LIBRARY.
C
C        ELCK=MMDELK(2,T,IFAILK)
C        ELCE=MMDELE(2,T,IFAILE)
C
C THE FUNCTIONS S21BBF, S21BCF FROM NAG LIBRARY.
C
         ELCK=              S21BBF(0.D0,TT,1.D0,IFAILK)
         ELCE=ELCK-T*T/3.D0*S21BCF(0.D0,TT,1.D0,IFAILE)
C
      IF(IFAILK.NE.0.OR.IFAILE.NE.0) WRITE(*,*)' ATTENTION! GREEN F.:',
     ,      '  IFAILK, IFAILE= ',IFAILK,IFAILE
C
      GREENI = ( (1.D0-T*T*0.5D0)*ELCK-ELCE )*( SQRT(R*RP)/T )
C
      RETURN
      END
C
C********************************************************************

       subroutine grGREN(R0,Z0,R,Z, dGdr,dGdz)

c green's function derivatives at the point (R,Z)
c the source position is (R0,Z0)
c to obtain the real values of derivatives dGdr,dGdz must be devided by pi

       IMPLICIT REAL*8(A-H,O-Z)
       SQRT(X)=DSQRT(X)
 
       t=SQRT( 4.D0*R*R0/( (R+R0)**2+(Z-Z0)**2 ) )
 
         tt=(1.D0-t**2)

C THE FUNCTIONS S21BBF, S21BCF FROM NAG LIBRARY.

         fK=              S21BBF(0.D0,tt,1.D0,IFAILK)
         fE=fK-t*t/3.D0*S21BCF(0.D0,tt,1.D0,IFAILE)

      IF(IFAILK.NE.0.OR.IFAILE.NE.0) WRITE(*,*)' ATTENTION! GREEN F.:',
     *      '  IFAILK, IFAILE= ',IFAILK,IFAILE
 
        Q = ( (1.D0-t*t*0.5D0)*fK-fE )/t
	   
        dtdr = 0.5d0*( t/R-t*t*t*(R+R0)/(2.d0*R*R0) )

        dtdz =-t*t*t*(z-z0)/(4.d0*R*R0) 

        dQdt = -Q/t + 0.5d0*( fE/tt-fK)

        dGdr = 0.5d0*sqrt(R0/R)*Q + sqrt(R0*R)*dQdt*dtdr 

        dGdz = sqrt(R0*R)*dQdt*dtdz 

      RETURN
      END

C********************************************************************

      SUBROUTINE bint(X,Y,R0,Z0,r1,z1,F,I)
      IMPLICIT REAL*8(A-H,O-Z)
C
C===============FUNCTIONS===============FUNCTIONS================C
C
      SQRT(X)=DSQRT(X)
      ALOG(X)=DLOG(X)
C
C======================SPECIAL FUNCTIONS=========================C
C
      D(X1,Y1,X2,Y2)=  SQRT( (X1-X2)**2 + (Y1-Y2)**2 )
C
      TINT(T)=(T-PSCAL)*(0.5d0*DLOG(PVEC**2+(T-PSCAL)**2+EPS**2)-1.d0)+
     +         PVEC*DATAN((T-PSCAL)/(PVEC+EPS))
C
C===============FUNCTIONS===============FUNCTIONS================C
C
      EPSH=1.d-9
      F=0.d0

        W=0.d0
        H=D(R0,Z0,R1,Z1)
        EPS=EPSH*H
C
C  INTEGRAL ALONG THE EDGE OF REGION
C
        RM=0.5d0*(R0+R1)
        ZM=0.5d0*(Z0+Z1)
        DTM=D(X,Y,RM,ZM)
        DFM=D(-X,Y,RM,ZM)
C
        IF( I .NE. 0 ) THEN
C
          PSCAL=( (-X-R0)*(R1-R0) + (Y-Z0)*(Z1-Z0) )/H
          PVEC= ( (-X-R0)*(Z1-Z0) - (Y-Z0)*(R1-R0) )/H
C
          W = TINT(H)-TINT(0.D0)
C
          PSCAL=( (X-R0)*(R1-R0) + (Y-Z0)*(Z1-Z0) )/H
          PVEC= ( (X-R0)*(Z1-Z0) - (Y-Z0)*(R1-R0) )/H
C
          W = W - (TINT(H)-TINT(0.D0))
C
          IF( DTM .LT. 0.25d0*H ) THEN
              DFJ =D(-X,Y,R0,Z0)
              DFJ1=D(-X,Y,R1,Z1)
              DTJ =D( X,Y,R0,Z0)
              DTJ1=D( X,Y,R1,Z1)
C
C+++          W = W - H * ALOG( DFJ*DFJ1/(D(X,Y,R0,Z0)*D(X,Y,R1,Z1)) )
C
            W = W*DFM-0.5d0*H*(ALOG(DFJ/DTJ)*DFJ+ALOG(DFJ1/DTJ1)*DFJ1)
C
           ELSE
C
C+++          W = W - H * ALOG( DFM / DTM )
C
              W = ( W - H * ALOG( DFM / DTM ) ) * DFM
C
           ENDIF
         ENDIF
C
        IF( DTM .LT. 0.25d0*H ) THEN
C
            E  = 0.5d0 * ( GREENI(X,Y,R0,Z0) + GREENI(X,Y,R1,Z1) )
C
        ELSE
C
            E  =  GREENI(X,Y,RM,ZM)
C
        ENDIF
C
        F =  E*H + 0.25D0*W
C
      F = F /3.14159265359d0
C
       RETURN
      END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        SUBROUTINE bint_d(X,Y,R0,Z0,r1,z1,Fr,Fz,i)

        IMPLICIT REAL*8(A-H,O-Z)

         SQRT(Xxx)=DSQRT(Xxx)

         D(X1,Y1,X2,Y2)= dSQRT( (X1-X2)**2 + (Y1-Y2)**2 )

        RM=0.5d0*(R0+R1)
        ZM=0.5d0*(Z0+Z1)

       call grGREN(Rm,Zm,x,y, dGdr,dGdz)

        dell=d(R0,Z0,r1,z1)

        Fr=dGdr*dell/3.14159265359d0
        Fz=dGdz*dell/3.14159265359d0

       RETURN
       END
C********************************************************************
       subroutine GRENg(R0,Z0,R,Z,fgreen,dGdr,dGdz)

c green's function derivatives at the point (R,Z)
c the source position is (R0,Z0)
c to obtain the real values of derivatives dGdr,dGdz must be devided by pi

       IMPLICIT REAL*8(A-H,O-Z)
       SQRT(X)=DSQRT(X)
 
       t=SQRT( 4.D0*R*R0/( (R+R0)**2+(Z-Z0)**2 ) )
 
         tt=(1.D0-t**2)

C THE FUNCTIONS S21BBF, S21BCF FROM NAG LIBRARY.

         fK=              S21BBF(0.D0,tt,1.D0,IFAILK)
         fE=fK-t*t/3.D0*S21BCF(0.D0,tt,1.D0,IFAILE)

      IF(IFAILK.NE.0.OR.IFAILE.NE.0) WRITE(*,*)' ATTENTION! GREEN F.:',
     *      '  IFAILK, IFAILE= ',IFAILK,IFAILE

        Q = ( (1.D0-t*t*0.5D0)*fK-fE )/t

        fgreen= Q*sqrt(R*R0)
 
        dtdr = 0.5d0*( t/R-t*t*t*(R+R0)/(2.d0*R*R0) )

        dtdz =-t*t*t*(z-z0)/(4.d0*R*R0) 

        dQdt = -Q/t + 0.5d0*( fE/tt-fK)

        dGdr = 0.5d0*sqrt(R0/R)*Q + sqrt(R0*R)*dQdt*dtdr 

        dGdz = sqrt(R0*R)*dQdt*dtdz 

      RETURN
      END
C********************************************************************
       subroutine d2GREN(R0,Z0,R,Z,d2Gdrz,d2Gdzz,d2Gdrr)

c green's function second derivatives at the point (R,Z)
c the source position is (R0,Z0)
c to obtain the real values of derivatives dGdr,dGdz must be devided by pi

       IMPLICIT REAL*8(A-H,O-Z)
       SQRT(X)=DSQRT(X)
 
       t=SQRT( 4.D0*R*R0/( (R+R0)**2+(Z-Z0)**2 ) )
 
         tt=(1.D0-t**2)

C THE FUNCTIONS S21BBF, S21BCF FROM NAG LIBRARY.

         fK=              S21BBF(0.D0,tt,1.D0,IFAILK)
         fE=fK-t*t/3.D0*S21BCF(0.D0,tt,1.D0,IFAILE)

      IF(IFAILK.NE.0.OR.IFAILE.NE.0) WRITE(*,*)' ATTENTION! GREEN F.:',
     *      '  IFAILK, IFAILE= ',IFAILK,IFAILE

        Q = ( (1.D0-t*t*0.5D0)*fK-fE )/t

        fgreen= Q*sqrt(R*R0)
 
        dtdr = 0.5d0*( t/R-t*t*t*(R+R0)/(2.d0*R*R0) )

        dtdz =-t*t*t*(z-z0)/(4.d0*R*R0) 

        dQdt = -Q/t + 0.5d0*( fE/tt-fK)

       d2tdrz =
     &  -(1/r/2-3.D0/4.D0*t**2*(r+r0)/r/r0)*t**3*(z-z0)/r/r0/4.D0


       d2tdzz =
     &  3.D0/16.D0*t**5*(z-z0)**2/r**2/r0**2-t**3/r/r0/4.D0


      d2tdrr=
     &(1/r/2-3.D0/4.D0*t**2*(r+r0)/r/r0)*(t/r/2-t**3*(r+r0)/r/r0/4) 
     #-t/r**2/2-t**3/r/r0/4+t**3*(r+r0)/r**2/r0/4 


       d2qdt2 =
     & q/t**2+fe/(1-t**2)**2*t+1/(1-t**2)*(fe-fk)/t/2-(fe/(1-t**2)-fk) 
     #/t/2.D0 


        dGdr = 0.5d0*sqrt(R0/R)*Q + sqrt(R0*R)*dQdt*dtdr 
       d2Gdzz=sqrt(R*R0)*(d2qdt2*dtdz**2+dQdt*d2tdzz)
       d2Gdrz=sqrt(R*R0)*(dQdt*dtdz/r/2.d0+d2qdt2*dtdr*dtdz+dQdt*d2tdrz)
       d2Gdrr=sqrt(R*R0)*(dQdt*dtdr/r/2.d0+d2qdt2*dtdr*dtdr+dQdt*d2tdrr-
     &  q/r**2/2.d0) + dGdr/r/2.d0

        !dGdr = 0.5d0*sqrt(R0/R)*Q + sqrt(R0*R)*dQdt*dtdr 
        !dGdz = sqrt(R0*R)*dQdt*dtdz 


      RETURN
      END

C********************************************************************

         REAL*8 FUNCTION CPR(H,NP,S)

         IMPLICIT REAL*8(A-H,O-Z)

         F(X)=H*(1.d0-X**NP)/(1.d0-X)-S

         i=0
         X=1.d0
         EPS=1.D-8

         A=0.0001D0
         B=20.D0

 3       X=(A+B)/2.d0
         Y0=F(X)
         Y1=F(A)
         Y2=F(B)
         i=i+1

         if(i.gt.1000) then

          write(6,*) ' ***print from routine CPR:'
          write(6,*) ' can`t generate grid out of plasma domain'
          write(6,*) ' program is terminated'
          stop

         endif

         IF(DABS(Y0).LT.EPS) GO TO 1
         IF((Y0*Y2).GT.0.D0) GO TO 2

         A=X
         GO TO  3
 2       B=X
         GO TO 3
 1       CONTINUE
         CPR=X

         RETURN
         END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         REAL*8 FUNCTION FUNSQ(R1,R2,R3,R4,Z1,Z2,Z3,Z4)

         INCLUDE 'double.inc'

         R13=R3-R1
         R24=R4-R2
         Z13=Z3-Z1
         Z24=Z4-Z2
         FUNSQ=(R13*Z24-R24*Z13)*0.5D0

         !IF(ZS.LT.0.) WRITE(6,*) '***ERROR:SQUARE<0',zs

         !FUNSQ=ZS*0.5D0

         RETURN
         END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      SUBROUTINE pROG1D(M,U, A,B,C,F, ALF,BET)
!**********************************************************************
!     PROGONKA - USUAL
!
!     A(J)*U(J+1)-C(J)*U(J)+B(J)*U(J-1)=-F(J) , J=1,M
!
!      B(1)=0 , A(M)=0
!
!**********************************************************************

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION U(*), A(*),B(*),C(*),F(*), ALF(*),BET(*)

      ALF(1)=A(1)/C(1)
      BET(1)=F(1)/C(1)

      DO 20 K=2,M

      DK=C(K)-ALF(K-1)*B(K)
      ALF(K)=A(K)/DK
20    BET(K)=(F(K)+BET(K-1)*B(K))/DK

      U(M) = BET(M)

      DO 30 J=2,M

      K=M+1-J
30    U(K)=ALF(K)*U(K+1)+BET(K)

      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        real*8 function blin_(r0,z0,r1,r2,z1,z2,u1,u2,u3,u4 )

         include 'double.inc'

        s1=(r2-r0)*(z2-z0)
        s3=(r0-r1)*(z0-z1)
        s2=(r0-r1)*(z2-z0)
        s4=(r2-r0)*(z0-z1)

        u0=(s1*u1+s2*u2+s3*u3+s4*u4)/(s1+s2+s3+s4)

        blin_=u0

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






