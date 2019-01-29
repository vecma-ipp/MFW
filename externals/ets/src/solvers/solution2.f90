! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> NUMERICAL SOLUTION
!>
!> This subroutine is prepared to solve single
!> transport equation in standardised form adopted
!> by the ETS.
!>
!> \author M.Tokar, D.Kalupin
!>
!> \version "$Id: solution2.f90 320 2009-07-13 13:27:15Z coster $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +    

SUBROUTINE SOLUTION2 (SOLVER, ifail)

!-------------------------------------------------------!
!                                                       !
!     This subroutine is prepared to solve single       !    
!     transport equation in standardised form adopted   !
!     by the ETS.                                       !
!                                                       !
!-------------------------------------------------------!  
!                                                       !
!     Source:       provided by M.Tokar on 17.02.2009   !
!     Developers:   M.Tokar, D.Kalupin                  !
!     Kontacts:     M.Tokar@fz-juelich.de               !
!                   D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     equation is solved in               !
!                   INTEGRAL form                       !
!                                                       !
!-------------------------------------------------------!  




      USE TYPE_SOLVER

      USE ITM_TYPES

      IMPLICIT NONE

      INTEGER, INTENT (INOUT)         :: ifail

! +++ Input/Output with ETS:
      TYPE (NUMERICS), INTENT (INOUT) :: SOLVER               !contains all I/O quantities to numerics part


! +++ Internal input/output parameters:
      INTEGER   :: IDIM,    NDIM                              !equation index and total number of equations to be solved

      INTEGER   :: IRHO,    NRHO                              !radius index, number of radial points

      INTEGER   :: FLAG                                       !flag for equation: 0 - interpretative (not solved), 1 - predictive (solved)

      REAL (R8) :: RHO(SOLVER%NRHO)                           !radii

      REAL (R8) :: AMIX                                       !fraction of new sollution mixed

      REAL (R8) :: Y(SOLVER%NRHO), YM(SOLVER%NRHO)            !function at the current amd previous time steps
      REAL (R8) :: DY(SOLVER%NRHO)                            !derivative of function

      REAL (R8) :: A(SOLVER%NRHO), B(SOLVER%NRHO)             !coefficients
      REAL (R8) :: C(SOLVER%NRHO), D(SOLVER%NRHO)             !coefficients 
      REAL (R8) :: E(SOLVER%NRHO), F(SOLVER%NRHO)             !coefficients 
      REAL (R8) :: G(SOLVER%NRHO), H                          !coefficients 
      REAL (R8) :: DD(SOLVER%NRHO), DE(SOLVER%NRHO)           !derivatives of coefficients 

      REAL (R8) :: V(2), U(2), W(2)                           !boundary conditions 






! + + + + + + + + + INTERFACE PART  + + + + + + + + + + +    

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++ Set up local variables (input, obtained from ETS):
!     Control parameters:
  NDIM           = SOLVER%NDIM
  NRHO           = SOLVER%NRHO
  AMIX           = SOLVER%AMIX



! +++ Solution of equations starting from 1 to NDIM:
  EQUATION_LOOP1: DO IDIM = 1, NDIM

     FLAG         = SOLVER%EQ_FLAG(IDIM)

     IF (FLAG.EQ.0) GOTO 20                                !equation is not solved



! +++ Numerical coefficients obtained from physics part  in form:
!
!     (A*Y-B*Y(t-1))/H + 1/C * (-D*Y' + E*Y) = F - G*Y

     H            = SOLVER%H

     RHO_LOOP1: DO IRHO=1,NRHO
        RHO(IRHO)  = SOLVER%RHO(IRHO)

        Y(IRHO)    = SOLVER%Y(IDIM,IRHO)
        DY(IRHO)   = SOLVER%DY(IDIM,IRHO)
        YM(IRHO)   = SOLVER%YM(IDIM,IRHO)

        A(IRHO)    = SOLVER%A(IDIM,IRHO) 
        B(IRHO)    = SOLVER%B(IDIM,IRHO)
        C(IRHO)    = SOLVER%C(IDIM,IRHO)
        D(IRHO)    = SOLVER%D(IDIM,IRHO) 
        E(IRHO)    = SOLVER%E(IDIM,IRHO)
        F(IRHO)    = SOLVER%F(IDIM,IRHO)
        G(IRHO)    = SOLVER%G(IDIM,IRHO) 
     END DO RHO_LOOP1


! +++ Boundary conditions for numerical solver in form:
!
!     V*Y' + U*Y = W 

! +++ On axis:

     V(1)         = SOLVER%V(IDIM,1)
     U(1)         = SOLVER%U(IDIM,1)
     W(1)         = SOLVER%W(IDIM,1) 

! +++ At the edge:

     V(2)         = SOLVER%V(IDIM,2) 
     U(2)         = SOLVER%U(IDIM,2) 
     W(2)         = SOLVER%W(IDIM,2) 



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++  Solution of transport equation:      

     CALL SOLVER_RITM_INTEGRAL        &
          (NRHO,RHO,AMIX,  Y,YM,DY,   A,B,C,D,E,F,G,H,  U,V,W)
!              radii   b.c.       coeff.      solution   spline


        RHO_LOOP3: DO IRHO = 1,NRHO
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++ Return solution to ETS:      
          SOLVER%Y(IDIM,IRHO)  = Y(IRHO)
          SOLVER%DY(IDIM,IRHO) = DY(IRHO)

        END DO RHO_LOOP3



20   CONTINUE



  END DO EQUATION_LOOP1



  RETURN



END SUBROUTINE SOLUTION2


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

! + + + + + + + + + NUMERICAL PART  + + + + + + + + + + +    
      SUBROUTINE SOLVER_RITM_INTEGRAL (n,r,amix,z,zm,dz,       &
          aets,bets,cets,dets,eets,fets,gets,tau,uets,vets,wets)

      USE ITM_TYPES

      IMPLICIT NONE

      INTEGER         i,n

      REAL (R8)         tau,xi,p,amix,error,errornew
!DPC 2009-02-28 Changed h(n) to h(n+1) to match definition in solut3
      REAL (R8)         h(n+1),r(n),z(n),zm(n)
      REAL (R8)         dz(n),dzm(n)
      REAL (R8)         aets(n),bets(n),cets(n)
      REAL (R8)         dets(n),eets(n),gets(n),fets(n),gamets(n)
      REAL (R8)         uets(2),vets(2),wets(2)
      REAL (R8)         cs(n),csn(n)
      REAL (R8)         al(n),be(n),so(n),nu(n),dif(n),vel(n),dfl(n)
      REAL (R8)         om(n),dom(n),s(n),ints(n)
      REAL (R8)         a(n),b(n),f(n),u(2),v(2),w(2)
      REAL (R8)         y(n),d(n)
      REAL (R8)         dd(n)

! +++ ETS - standard equation in cylindrical geometry
      DO i=2,n
         al(i)     = aets(i)*cets(i)/r(i)
         be(i)     = bets(i)*cets(i)/r(i)
         so(i)     = fets(i)*cets(i)/r(i)
         nu(i)     = gets(i)*cets(i)/r(i)
         dif(i)    = dets(i)/r(i)
         vel(i)    = eets(i)/r(i)
         gamets(i) = 0.d0
         dfl(i)    = gamets(i)/r(i)
         h(i-1)    = r(i)-r(i-1) 
      END DO
      h(n)=0.0_R8                        !DPC 2009-02-28

      FORALL(i=1:n) cs(i)=0.d0

      CALL DERIVN2(n,r,zm,dzm)

! +++ parameters for integral representation
 10   xi=1.d-1

      DO i=2,n
         p     = (1.d0-xi)*al(i)+tau*nu(i)
         om(i) = xi*al(i)+(p+dabs(p))/2.d0
         s(i)  = so(i)+((p-dabs(p))/2.d0*z(i)-(om(i)-be(i))*zm(i))/tau
      END DO

      CALL AXIS(n,h,r,om)

      CALL AXIS(n,h,r,s)


! +++ coefficients for differential equation
      CALL DERIVN2 (n,r,om,dom)
      CALL INTEGRAL (n,h,r,s,ints)

      DO i=2,n
         a(i) = 3.d0/r(i)-dom(i)/om(i)-vel(i)/dif(i)
         b(i) = om(i)/tau/dif(i)+2.d0/r(i)*(3.d0/r(i)-a(i))
         f(i) = om(i)/r(i)/dif(i)*                             &
                (ints(i)/r(i)+dif(i)*dzm(i)-vel(i)*zm(i)-dfl(i))
      END DO


! +++ boundary conditions
      IF(wets(1).NE.0.d0) THEN
         write(6,*) 'wrong boundary condition: w_1 is not 0' 
         STOP
      END IF
    
      IF(uets(1).EQ.0.d0) THEN                            !dz/dr(0)=0
         u(1)=b(2)
         v(1)=-1.d0/r(2)-a(2)
         w(1)=f(2)
      END IF
    
      IF(vets(1).EQ.0.d0) THEN                            !z(0)=0
         u(1) = b(2)+2.d0/r(2)**2
         v(1) = -a(2)-2.d0/r(2)
         w(1) = f(2)
      END IF

      w(2) = vets(2)*f(n)+                          &
             om(n)/r(n)*(wets(2)-uets(2)*zm(n)-vets(2)*dzm(n))
      v(2) = uets(2)+vets(2)*vel(n)/dif(n)
      u(2) = vets(2)*om(n)/tau/dif(n)+2.d0/r(n)*v(2)


! +++ solution: cs=z-zm 
      CALL SOLUT3 (n,h,a,b,f,u,v,w,y,d)

      CALL AXIS (n,h,r,y)

      IF(vets(1).EQ.0.d0) y(1)=0.d0

!      CALL DERIVN2 (n,r,y,d)

      DO i=1,n
         csn(i)   = (r(i)*d(i)+2.d0*y(i))/om(i)
         cs(i)    = cs(i)*(1.d0-amix)+csn(i)*amix
         z(i)     = zm(i)+cs(i)
      END DO

      CALL DERIVN2(n,r,z,dz)

! +++ error in solution
      error=0.d0

      DO i=1,n
         errornew  =dsqrt(dabs(1.d0-csn(i)/cs(i)))
         IF(cs(i).NE.0.d0.AND.error.LT.errornew) error=errornew
      END DO

      IF (error.GT.1.d-5*amix) GOTO 10


      RETURN


      END SUBROUTINE SOLVER_RITM_INTEGRAL
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
      SUBROUTINE SOLUT3 (n,h,a,b,f,u,v,w,y,dy)
!-------------------------------------------------------!
!                                                       !
!     This subroutine provides solution for the equation!
!                                                       !
!     d(dy/dx)/dx + a(x)*dy/dx = b(x)*y - f(x)          !
!                                                       !
!-------------------------------------------------------!
      USE ITM_TYPES

      IMPLICIT NONE

      INTEGER   i,j,k,l,m,n

      REAL (R8)   h(n+1),a(n),b(n),f(n),y(n),dy(n),lam(10001,2)
      REAL (R8)   u(2),v(2),w(2)
      REAL (R8)   y0(n),y1(10001,2,2)
      REAL (R8)   et(2,2),mu(2),det
      REAL (R8)   c(n),d(n),g(2),x
      REAL (R8)   cdy(n,2)

      DO i=2,n
         y0(i)        = f(i)/b(i)
         det          = a(i)**2/4.d0+b(i)

         DO k=1,2
            lam(i,k)  = -a(i)/2.d0-(-1)**k*dsqrt(det)


            FORALL(l=1:2)                                   &
            y1(i,k,l) = dexp(dmin1(200.d0,(-1)**l*lam(i,k)  &
                        *h(i-2+l)/2.d0))
         END DO

      END DO

     
      c(3)            = v(1)/(v(1)-h(3)*u(1))
      d(3)            = h(3)*w(1)/(h(3)*u(1)-v(1))


      DO i=3,n-1

         DO m=1,2
            l         = 3-m
            j         = i+(-1)**m

            DO k=1,2 
              et(k,m) = -(-1)**m*(lam(j,m)-lam(i,k)        &
                        -(lam(j,l)-lam(i,k))*y1(j,l,l)     &
                        /y1(j,m,l))/(lam(j,1)-lam(j,2))
            END DO

            mu(m)     = -(-1)**m*(y0(i)-y0(j))/y1(i,l,m)   &
                        *(lam(j,m)-lam(j,l)*y1(j,l,l)      &
                        /y1(j,m,l))/(lam(j,1)-lam(j,2))
         END DO

         det          = et(1,2)*et(2,1)-et(1,1)*et(2,2)    &
                        *y1(i,l,1)/y1(i,2,1)               &
                        *y1(i,2,2)/y1(i,l,2)

         dy(i)        = 0.0D0

         DO m=1,2
            l         = 3-m
            g(m)      = y1(i+(-1)**m,l,l)/y1(i,l,m)*       &
                        (et(m,l)-y1(i,l,l)/y1(i,m,l)       &
                        *et(l,l))/det
!        derivative:
            dy(i)     = dy(i)+lam(i,m)*                    &
                        (mu(m)*y1(i,l,l)/y1(i,m,l)*        &
                         et(l,l)-mu(l)*et(l,m))/det
            cdy(i,m)  = (et(m,l)*lam(i,l)-                 &
                         y1(i,l,l)/y1(i,m,l)*et(l,l)*lam(i,m))/det
         END DO

         x            = (mu(1)*(y1(i,2,2)/y1(i,1,2)        &
                        *et(2,2)-et(1,2))+mu(2)*(y1(i,1,1) &
                        /y1(i,2,1)*et(1,1)-et(2,1)))/det-  &
                        y0(i-1)*g(1)-y0(i+1)*g(2)+y0(i)
         c(i+1)       = g(2)/(1.d0-g(1)*c(i))
         d(i+1)       = (g(1)*d(i)+x)/(1.d0-g(1)*c(i))

      END DO

      y(n)            = (h(n-1)*w(2)+v(2)*d(n))            &
                        /(h(n-1)*u(2)+v(2)-v(2)*c(n))

      DO i=n,3,-1
         y(i-1)       = c(i)*y(i)+d(i)
      END DO


!     derivative:
      DO i=3,n-1
         DO m=1,2
            l         = 3-m
            j         = i+(-1)**m
            dy(i)     = dy(i)+(y(j)-y0(j))*y1(j,l,l)/y1(i,l,m)*cdy(i,m)
         END DO
      END DO

      dy(2)=(y(3)-y(3))/h(2)
      dy(n)=(y(n)-y(n-1))/h(n-1)


      RETURN


      END SUBROUTINE SOLUT3
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
     SUBROUTINE DERIVN2(N,X,Y,DY1)
!-------------------------------------------------------!
!                                                       !
!    This subroutine calculates first and               !
!    second derivatives, DY1 and DY2,                   !
!    of function Y respect to argument X                !
!                                                       !
!-------------------------------------------------------!
      USE ITM_TYPES

      IMPLICIT NONE

      INTEGER      N, I
      REAL (R8)    X(N), Y(N), DY1(N)
      REAL (R8)    H(n), DY2(n)

      SAVE




      DO i=2,n-1
        dy1(i)  = (y(i+1)-y(i-1))/(x(i+1)-x(i-1)) 
      END DO


      dy1(1)    = (y(2)-y(1))/(x(2)-x(1))
      dy1(n)    = (y(n)-y(n-1))/(x(n)-x(n-1))


      RETURN


      END SUBROUTINE DERIVN2
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
     SUBROUTINE DERIVN21(N,X,Y,DY1)
!-------------------------------------------------------!
!                                                       !
!    This subroutine calculates first and               !
!    second derivatives, DY1 and DY2,                   !
!    of function Y respect to argument X                !
!                                                       !
!-------------------------------------------------------!
      USE ITM_TYPES

      IMPLICIT NONE

      INTEGER      N, I
      REAL (R8)    X(N), Y(N), DY1(N)
      REAL (R8)    H(n), DY2(n)

      SAVE


      DO i=1,n-1
        h(i)    = x(i+1)-x(i)
      END DO


      DO i=2,n-1
        dy1(i)  = ((y(i+1)-y(i))*h(i-1)/h(i)                 &
                  +(y(i)-y(i-1))*h(i)/h(i-1))/(h(i)+h(i-1))
        dy2(i)  = 2.d0*((y(i-1)-y(i))/h(i-1)                 &
                  +(y(i+1)-y(i))/h(i))/(h(i)+h(i-1))
      END DO


      dy1(1)    = dy1(2)-dy2(2)*h(1)
      dy1(n)    = dy1(n-1)+dy2(n-1)*h(n-1)


      RETURN


      END SUBROUTINE DERIVN21
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
      SUBROUTINE INTEGRAL (n, h, r, f, int)
!-------------------------------------------------------!
!                                                       !
!     This subroutine calculates integral               !
!     from 0 to r_n of f*r byinterpolating f            !
!     with a local quadratic spline                     !
!                                                       !
!-------------------------------------------------------!

      USE ITM_TYPES

      IMPLICIT NONE

      INTEGER       i, n

      REAL (R8)       h(n), r(n), f(n), int(n)
      REAL (R8)       den
      REAL (R8)       a(n), b(n)


      DO i=2,n-1
         den     = h(i-1)*(h(i-1)+h(i))*h(i)
         a(i)    = ((f(i+1)-f(i))*h(i-1)**2-(f(i-1)-f(i))*h(i)**2)/den
         b(i)    = ((f(i+1)-f(i))*h(i-1)+(f(i-1)-f(i))*h(i))/den
      END DO


      INT(1)     = 0.d0

      INT(2)     = f(2)*r(2)**2/2.d0-a(2)*r(2)**3/6.d0+b(2)*r(2)**4/12.d0


      DO i=2,n-2 
        INT(i+1) = INT(i)+(r(i)*f(i)+r(i+1)*f(i+1))*h(i)/2.d0+           &
                   (f(i)+r(i)*a(i)-f(i+1)-r(i+1)*a(i+1))*h(i)**2/8.d0+   &
                   (a(i)+r(i)*b(i)+a(i+1)+r(i+1)*b(i+1))*h(i)**3/24.d0+  &
                   (b(i)-b(i+1))*h(i)**4/64.d0
      END DO


      INT(n)     = INT(n-1)+(f(n-1)-a(n-1)*r(n-1)+b(n-1)*r(n-1)**2)*     &
                   (r(n)**2-r(n-1)**2)/2.d0+(a(n-1)-2.d0*b(n-1)*r(n-1))* &
                   (r(n)**3-r(n-1)**3)/3.d0+b(n-1)*(r(n)**4-r(n-1)**4)/4.d0


      RETURN


      END SUBROUTINE INTEGRAL
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
      SUBROUTINE AXIS(n, h, r, f)
!-------------------------------------------------------!
!                                                       !
!     This subroutine finds                             !
!     f(r_1=0) from f(r_2), f(r_3) and f(r_4)           !
!                                                       !
!-------------------------------------------------------!  

      USE ITM_TYPES

      IMPLICIT NONE

      INTEGER    n
      REAL (R8)    h(n), r(n), f(n)

      f(1)     = ((f(2)*r(4)/h(2)+f(4)*r(2)/h(3))*r(3)        &
                 -f(3)*(r(2)/h(2)+r(4)/h(3))*r(2)*r(4)/r(3))  &
                 /(r(4)-r(2))


      RETURN


      END SUBROUTINE AXIS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


