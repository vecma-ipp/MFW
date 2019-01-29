! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This subroutine is prepared to solve single transport equation in standardised form adopted by the ETS.
!>
!> \author M.Tokar, D.Kalupin
!>
!> \version "$Id: solution7.f90 321 2009-07-13 13:29:01Z coster $"
! + + + + + + + + + NUMERICAL SOLUTION  + + + + + + + + +    

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + NUMERICAL SOLUTION  + + + + + + + + +    

      SUBROUTINE SOLUTION7 (SOLVER, ifail)

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!                                                       +
!     This subroutine is prepared to solve single       +   
!     transport equation in standardised form           +
!     adopted by the ETS.                               +
!                                                       +
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!     Source:       1-D transport code RITM             +
!     Developers:   M.Tokar, D.Kalupin                  +
!     Kontacts:     M.Tokar@fz-juelich.de               +
!                   D.Kalupin@fz-juelich.de             +
!                                                       +
!     Comments:    equation is solved in                +
!                  DIFFERENTIAL form                    +
!                                                       +
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


      USE TYPE_SOLVER

      IMPLICIT NONE

      INTEGER, PARAMETER              :: DP = KIND(1.0D0)     ! Double precision  

      INTEGER, INTENT (INOUT)         :: ifail

! +++ Input/Output with ETS:
      TYPE (NUMERICS), INTENT (INOUT) :: SOLVER               !contains all I/O quantities to numerics part


! +++ Internal input/output parameters:
      INTEGER   :: IDIM,    NDIM                              !equation index and total number of equations to be solved

      INTEGER   :: IRHO,    NRHO                              !radius index, number of radial points

      INTEGER   :: FLAG                                       !flag for equation: 0 - interpretative (not solved), 1 - predictive (solved)

      REAL (DP) :: RHO(SOLVER%NRHO)                           !radii

      REAL (DP) :: AMIX                                       !fraction of new sollution mixed

      REAL (DP) :: Y(SOLVER%NRHO), YM(SOLVER%NRHO)            !function at the current amd previous time steps
      REAL (DP) :: DY(SOLVER%NRHO)                            !derivative of function

      REAL (DP) :: A(SOLVER%NRHO), B(SOLVER%NRHO)             !coefficients
      REAL (DP) :: C(SOLVER%NRHO), D(SOLVER%NRHO)             !coefficients 
      REAL (DP) :: E(SOLVER%NRHO), F(SOLVER%NRHO)             !coefficients 
      REAL (DP) :: G(SOLVER%NRHO), H                          !coefficients 
      REAL (DP) :: DD(SOLVER%NRHO), DE(SOLVER%NRHO)           !derivatives of coefficients 

      REAL (DP) :: V(2), U(2), W(2)                           !boundary conditions 


! +++ Coefficients used by internal numerical solver:
      INTEGER   :: I, N                                       !radius index, number of radial points
      INTEGER   :: ISP                                        !spline flag

      REAL (DP) :: X(SOLVER%NRHO)                             !radii

      REAL (DP) :: SOL(SOLVER%NRHO)                           !solution 
      REAL (DP) :: DSOL(SOLVER%NRHO)                          !derivative of solution

      REAL (DP) :: AS(SOLVER%NRHO), BS(SOLVER%NRHO)           !coefficients
      REAL (DP) :: CS(SOLVER%NRHO)                            !coefficients 

      REAL (DP) :: VS(2), US(2), WS(2)                        !boundary conditions 



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

        H            = SOLVER%H

        CALL DERIVN7(NRHO,RHO,D,DD)                             !Derivation of coefficient D
        CALL DERIVN7(NRHO,RHO,E,DE)                             !Derivation of coefficient E

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
! +++ Set up numerical coefficients used in the solver
!     (translation is done for differential form 
!      of transport equation):

!       indexies:
        ISP          = 1
        N            = NRHO
!       coefficients:
        RHO_LOOP2: DO I = 2,N
          X(I)       = RHO(I)
          AS(I)      = -E(I)/D(I) + DD(I)/D(I) 
          BS(I)      = A(I)*C(I)/H/D(I) + C(I)*G(I)/D(I) + DE(I)/D(I)
          CS(I)      = C(I)/D(I) * (F(I) + B(I)*YM(I)/H)
        END DO RHO_LOOP2
          X(1)       = RHO(1)
       CALL AXIS7(N, X, AS)
       CALL AXIS7(N, X, BS)
       CALL AXIS7(N, X, CS)

!       boundary conditions:
        VS(1)        = V(1)
        US(1)        = U(1)
        WS(1)        = W(1) 
        VS(2)        = V(2) 
        US(2)        = U(2) 
        WS(2)        = W(2) 
  

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++  Solution of transport equation:      

        CALL SOLUTN7  (X,N,   VS,US,WS,   AS,BS,CS,   SOL,DSOL)
!                    radii   b.c.        coeff.      solution


        RHO_LOOP3: DO IRHO = 1,NRHO
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++ New function and derivative:      
          Y(IRHO)    = Y(IRHO)*(1.D0-AMIX)  + SOL(IRHO)*AMIX
          DY(IRHO)   = DY(IRHO)*(1.D0-AMIX) + DSOL(IRHO)*AMIX


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++ Return solution to ETS:      
          SOLVER%Y(IDIM,IRHO)  = Y(IRHO)
          SOLVER%DY(IDIM,IRHO) = DY(IRHO)

        END DO RHO_LOOP3





 20     CONTINUE


      END DO EQUATION_LOOP1




      RETURN



      END SUBROUTINE SOLUTION7


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  








! + + + + + + + + + NUMERICAL PART  + + + + + + + + + + +    


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
      SUBROUTINE SOLUTN7 (RHO,N,   V,U,W,   A,B,F,   Y,DY)
!                       radii   b.c.     coeff.   solution
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

      REAL (R8)   rho(n),h(n+1),a(n),b(n),f(n),y(n),dy(n),lam(10001,2)
      REAL (R8)   u(2),v(2),w(2)
      REAL (R8)   y0(n),y1(10001,2,2)
      REAL (R8)   et(2,2),mu(2),det
      REAL (R8)   c(n),d(n),g(2),x
      REAL (R8)   cdy(n,2)
 
      h               = 0.D0
 
      DO i=1,n-1
         h(i)         = rho(i+1)-rho(i)
      END DO
         h(n)         = h(n-1)

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


!      CALL DERIVN7(N,RHO,y,dy)                             

      RETURN


      END SUBROUTINE SOLUTN7
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!  These subroutines calculate first and second derivatives,
!  DY1 and DY2, of function Y respect to argument X
	SUBROUTINE DERIVN7(N,X,Y,DY1)
	
        IMPLICIT NONE
        
        INTEGER, PARAMETER :: DP = KIND(1.0D0)                ! Double precision  
        INTEGER :: N                                          ! number of radial points (input)
        INTEGER :: I

	REAL (DP) :: X(N), &                                  ! argument array (input)
                     Y(N), &                                  ! function array (input)
                     DY1(N)                                   ! function derivative array (output)
	REAL (DP) :: H(N),DY2(N)

	DO I=1,N-1
	   H(I)=X(I+1)-X(I)
	END DO

	DO I=2,N-1
	   DY1(I)=((Y(I+1)-Y(I))*H(I-1)/H(I)+(Y(I)-Y(I-1))*H(I)/H(I-1)) &
                 /(H(I)+H(I-1))
	   DY2(I)=2.d0*((Y(I-1)-Y(I))/H(I-1)+(Y(I+1)-Y(I))/H(I)) &
                 /(H(I)+H(I-1))
	END DO

	DY1(1)=DY1(2)-DY2(2)*H(1)
	DY1(N)=DY1(N-1)+DY2(N-1)*H(N-1)

	RETURN
	END SUBROUTINE DERIVN7
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
      SUBROUTINE AXIS7(n, r, f)
!-------------------------------------------------------!
!                                                       !
!     This subroutine finds                             !
!     f(r_1=0) from f(r_2), f(r_3) and f(r_4)           !
!                                                       !
!-------------------------------------------------------!  

      IMPLICIT NONE

      INTEGER    n, i
      REAL *8    h(n), r(n), f(n)

  
      DO i=1,3
        h(i)=r(i+1)-r(i)
      END DO

      f(1)     = ((f(2)*r(4)/h(2)+f(4)*r(2)/h(3))*r(3)        &
                 -f(3)*(r(2)/h(2)+r(4)/h(3))*r(2)*r(4)/r(3))  &
                 /(r(4)-r(2))


      RETURN


      END SUBROUTINE AXIS7
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
