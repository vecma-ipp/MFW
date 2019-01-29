! + + + + + + + + + + + + + + + + + + + + + + + + + + + + 
!> This subroutine is prepared to solve single transport
!> equation in standardised form adopted by the ETS.      
!>                                                         
!> \author fesb team                                                            
!>                                                                              
!> \version "$Id$"                                                              
! + + + + + + + + + NUMERICAL SOLUTION  + + + + + + + + +                       

SUBROUTINE SOLUTIONFEM (SOLVER, ifail)

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!                                                       +  
!     This subroutine is prepared to solve single       +   
!     transport equation in standardised form           +   
!     adopted by the ETS.                               +   
!                                                       +   
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
!     Source:       FESB Team                           +   
!     Developers:   Anna Šušnjara, Vicko Doriæ          +   
!                                                       +   
!     Comments:     finite element method based solver  +   
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   


  USE TYPE_SOLVER
  USE ITM_TYPES  

  IMPLICIT NONE

! +++ Input/Output with ETS:
  INTEGER,         INTENT (INOUT) :: ifail
  TYPE (NUMERICS), INTENT (INOUT) :: SOLVER               !contains all I/O quantities to numerics part                                                         

!#ifdef WANTCOS

! +++ Internal input/output parameters:
  INTEGER   :: IDIM,    NDIM                              !equation index and total number of equations to be solved                                            
  INTEGER   :: IRHO,    NRHO                              !radius index, number of radial points                                                                

  REAL (R8) :: RHO(SOLVER%NRHO)                           !radii
  REAL (R8) :: RHOMAX                                           

  REAL (R8) :: AMIX                                       !fraction of new sollution mixed                                                                      

  REAL (R8) :: Y(SOLVER%NRHO), YM(SOLVER%NRHO)            !function at the current amd previous time steps                                                      
  REAL (R8) :: DY(SOLVER%NRHO)                            !derivative of function                                                                               
                                                                                
  REAL (R8) :: YY(SOLVER%NRHO)                            !new function         

  REAL (R8) :: A(SOLVER%NRHO), B(SOLVER%NRHO)             !coefficients
  REAL (R8) :: C(SOLVER%NRHO), D(SOLVER%NRHO)             !coefficients 
  REAL (R8) :: E(SOLVER%NRHO), F(SOLVER%NRHO)             !coefficients 
  REAL (R8) :: G(SOLVER%NRHO), H                          !coefficients 
  REAL (R8) :: DD(SOLVER%NRHO), DE(SOLVER%NRHO)           !derivatives of coefficients                                                                          

  REAL (R8) :: V(2), U(2), W(2)                           !boundary conditions 

! +++ Coefficients used by internal numerical solver:
  REAL (R8) ::  X(SOLVER%NRHO)                            !normalized radii
  !REAL (R8) ::  RUB2, RUB1                                !radii of inner and outer boundaries                                                                  


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++ Set up local variables (input, obtained from ETS):   
!     Control parameters:                                  
  NDIM           = SOLVER%NDIM                             
  NRHO           = SOLVER%NRHO                             
  AMIX           = SOLVER%AMIX                             
  H              = SOLVER%H                                


! +++ Solution of equations starting from 1 to NDIM:
  EQUATION_LOOP1: DO IDIM = 1, NDIM                 
     IF (SOLVER%EQ_FLAG(IDIM).EQ.0) GOTO 20              !equation is not solved



! +++ Numerical coefficients obtained from physics part in form:
!     (A*Y-B*Y(t-1))/H + 1/C * (-D*Y' + E*Y)' = F - G*Y         

     RHOMAX        = SOLVER%RHO(NRHO)
                                     
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

     X             = RHO/RHOMAX
                               
     CALL DERIV_HOLOB(NRHO,X,D,DD)
     CALL DERIV_HOLOB(NRHO,X,E,DE)
                                  

! +++ Boundary conditions in form:
!     V*Y' + U*Y = W              

     U = SOLVER%U(IDIM,:)
     V = SOLVER%V(IDIM,:)
     W = SOLVER%W(IDIM,:)

     !RUB1 = W(1)
     !RUB2 = W(2)      ! RUB2 = W(2)*RHOMAX;

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++  Solution of transport equation:                     
                                                           
     CALL fem_pde_solver (A,B,C,D,DD,E,DE,F,G,H,  &        
                          YM,YY,X,NRHO,U, V, W)       
	 !CALL fem_pde_solver (A,B,C,D,DD,E,DE,F,G,H,  &        
      !                    YM,YY,RUB1,RUB2,X,NRHO&
		!				  U, V, W)        						  

     Y = Y*(1.e0_R8-AMIX)  + YY*AMIX  
                                      
     CALL DERIV_HOLOB(NRHO,RHO,Y,DY)  
                                      
     SOLVER%Y(IDIM,:)  = Y            
     SOLVER%DY(IDIM,:) = DY           
                                      
20   CONTINUE                         

  END DO EQUATION_LOOP1

!#endif

  RETURN

END SUBROUTINE SOLUTIONFEM







!#ifdef WANTCOS

! + + + + + + + + + NUMERICAL PART  + + + + + + + + + + +    

!********************************************************************************                                                                               
!*********************************************************************************                                                                              
SUBROUTINE fem_pde_solver (A,B,C,D,DD,E,DE,F,G,dt_in,&                          
                                                   YM,YY,X,N,&
												   U, V, W)         
!SUBROUTINE fem_pde_solver (A,B,C,D,DD,E,DE,F,G,dt_in,&                          
                                                   !YM,YY,RUB1,RUB2,X,N&
												   !U, V, W)    
  USE itm_types
  IMPLICIT NONE
               
  INTEGER,   INTENT(in)    ::  N
  REAL (R8), INTENT(in)    ::  X(N), dt_in
  REAL (R8), INTENT(in)    ::  A(N),B(N),C(N),D(N),E(N),F(N)
  REAL (R8), INTENT(in)    ::  G(N),DD(N),DE(N),YM(N)  
  REAL (R8), INTENT(in)    ::  U(2), V(2), W(2)  
                                                            
  REAL (R8), INTENT(inOUT) ::  YY(N)                        
                                                            
  !REAL (R8), INTENT(in)    ::  RUB1, RUB2                   
                                                            
  !Global matrices                                          
  REAL (R8), DIMENSION(:,:), ALLOCATABLE ::  MAT,DET,KOF,DET1,DET2,DET3
  REAL (R8), DIMENSION(:,:), ALLOCATABLE :: MAT_dirichle, KOF_dirichle
                                                                       
  REAL (R8), DIMENSION(:,:), ALLOCATABLE ::  RHS,LHS                   
                                                                       
  ! pivot and ok are arguments for SGESV()                             
  INTEGER, DIMENSION(:), ALLOCATABLE     :: pivot, pivot_Dirichle                  
  INTEGER       :: ok                                                  
                                                                       
  !shape functions and their derivatives, gauss points and weights     
  REAL (R8)     :: KSI(4),    WEIGHT(4)                                
  REAL (R8)     :: N1(4),     N2(4)                                    
  REAL (R8)     :: NN1(2,2),  NN2(2,2),  NN3(2,2),  NN4(2,2)           
  REAL (R8)     :: dNN1(2,2), dNN2(2,2), dNN3(2,2), dNN4(2,2)          
  REAL (R8)     :: dNdN(2,2)                                           
                                                                       
  !counters in loops, auxiliary index                                  
  INTEGER       :: k, i,j, i1, i2                                      
                                                                       
  !variables inside loop                                               
  REAL (R8)     :: delX                                                
  REAL (R8)     :: a_in(4),  b_in(4), c_in(4), d_in(4)                 
  REAL (R8)     :: e_in(4),  f_in(4), g_in(4)                          
  REAL (R8)     :: de_in(4), dd_in(4)                                  
  REAL (R8)     :: AF(4),    BF(4),   CF(4),   DF(4)                   
                                                                       
  !local matrices                                                      
  REAL (R8)     :: MATlok(2,2), KOFlok(1,2)                            
  REAL (R8)     :: DET1lok(2,2),DET2lok(2,2),  DET3lok(2,2)            
                                                                       
  !auxiliary                                                           
  REAL (R8)     :: pom1(1,2),   pom2(2,1)                              
  REAL (R8)     :: tmp1(4,2),   tmp22(4),      tmp2(1,4)               
                                                                       
  !allocation                                                          
  ALLOCATE(MAT(N,N))                                                   
  ALLOCATE(DET(N,N))                                                   
  ALLOCATE(DET1(N,N))                                                  
  ALLOCATE(DET2(N,N))                                                  
  ALLOCATE(DET3(N,N))                                                  
  ALLOCATE(KOF(1,N))                                                   
  ALLOCATE(RHS(1,N))                                                   
  ALLOCATE(LHS(N,N))  

  ALLOCATE(MAT_dirichle(N-1, N-1))
  ALLOCATE(KOF_dirichle(1,N-1))
  ALLOCATE(pivot_Dirichle(N-1))
                                                                       
  !initialization                                                      
  MAT(:,:) = 0.0E0_R8                                                  
  DET(:,:) = 0.0E0_R8                                                  
  DET1(:,:)= 0.0E0_R8                                                  
  DET2(:,:)= 0.0E0_R8                                                  
  DET3(:,:)= 0.0E0_R8                                                  
  KOF(1,:) = 0.0E0_R8                                                  
  RHS(1,:) =0.0E0_R8                                                   
  LHS(:,:) =0.0E0_R8   

  MAT_dirichle(:,:) = 0.0E0_R8
  KOF_dirichle(:,:) = 0.0E0_R8
                                                                       
  KSI = (/-0.86113631E0_R8,-0.33998104E0_R8,&                          
       0.33998104E0_R8,0.86113631E0_R8/)                               
  WEIGHT = (/ 0.34785485E0_R8,0.65214515E0_R8,&                        
       0.65214515E0_R8,0.34785485E0_R8/)                               
                                                                       
  N1 = (1-KSI)/2                                                       
  N2 = (1+KSI)/2                                                       
                                                                       
  ! NN1, NN2, NN3, NN4                                                 
  pom1(1,1) = N1(1)                                                    
  pom1(1,2) = N2(1)                                                    
  pom2(1,1) = N1(1)                                                    
  pom2(2,1) = N2(1)                                                    
  NN1 = MATMUL(pom2,pom1)                                              
                                                                       
  pom1(1,1) = N1(2)                                                    
  pom1(1,2) = N2(2)                                                    
  pom2(1,1) = N1(2)                                                    
  pom2(2,1) = N2(2)                                                    
  NN2 = MATMUL(pom2,pom1)                                              
                                                                       
  pom1(1,1) = N1(3)                                                    
  pom1(1,2) = N2(3)                                                    
  pom2(1,1) = N1(3)                                                    
  pom2(2,1) = N2(3)                                                    
  NN3 = MATMUL(pom2,pom1)                                              
                                                                       
  pom1(1,1) = N1(4)                                                    
  pom1(1,2) = N2(4)                                                    
  pom2(1,1) = N1(4)                                                    
  pom2(2,1) = N2(4)                                                    
  NN4 = MATMUL(pom2,pom1)                                              
                                                                       
  dNdN = RESHAPE((/1, -1, -1, 1/), SHAPE(dNdN))                        
                                                                       
  !                                                                    
  solveEQloop: DO k =1, N-1                                            
                                                                       
     delX = X(k+1) - X(k)                                              
                                                                       
     ! dNN1, dNN2, dNN3, dNN4                                          
     pom2(1,1) = -1/delX                                               
     pom2(2,1) =  1/delX                                               
                                                                       
                                                                       
     pom1(1,1) = N1(1)                                                 
     pom1(1,2) = N2(1)                                                 
     dNN1 = MATMUL(pom2,pom1)                                          
                                                                       
     pom1(1,1) = N1(2)                                                 
     pom1(1,2) = N2(2)                                                 
     dNN2 = MATMUL (pom2,pom1)                                         
                                                                       
     pom1(1,1) = N1(3)                                                 
     pom1(1,2) = N2(3)                                                 
     dNN3 = MATMUL (pom2,pom1)                                         
                                                                       
     pom1(1,1) = N1(4)                                                 
     pom1(1,2) = N2(4)                                                 
     dNN4 = MATMUL (pom2,pom1)                                         
                                                                       
                                                                       
     a_in  = A(k) * N1 + A(k+1) *N2                                    
     b_in  = B(k) * N1 + B(k+1) *N2                                    
     c_in  = C(k) * N1 + C(k+1) *N2                                    
     d_in  = D(k) * N1 + D(k+1) *N2                                    
     e_in  = E(k) * N1 + E(k+1) *N2                                    
     f_in  = F(k) * N1 + F(k+1) *N2                                    
     g_in  = G(k) * N1 + G(k+1) *N2                                    
     de_in = DE(k) * N1 + DE(k+1) *N2                                  
     dd_in = DD(k) * N1 + DD(k+1) *N2                                  
                                                                       
     AF     = a_in*c_in/d_in                                           
     BF     = (dd_in-e_in)/d_in                                        
     !CF     = (de_in+g_in*c_in)/d_in                                  
     CF     = ((de_in+g_in*c_in)/d_in)+(c_in*(a_in-b_in)/(d_in*dt_in)) 
     DF     = c_in*f_in/d_in                                           
                                                                       
     MATlok  = (delX/2)  * ( AF(1)*NN1*WEIGHT(1)  + AF(2)*NN2*WEIGHT(2)  +&
                AF(3)*NN3*WEIGHT(3)  + AF(4)*NN4*WEIGHT(4))                
                                                                           
     DET3lok = (1/delX) *   dNdN                                           
                                                                           
     DET1lok = (delX/2) * ( BF(1)*dNN1*WEIGHT(1) + BF(2)*dNN2*WEIGHT(2) +& 
               BF(3)*dNN3*WEIGHT(3) + BF(4)*dNN4*WEIGHT(4))                
                                                                           
     DET2lok = (delX/2) * ( CF(1)*NN1*WEIGHT(1)  + CF(2)*NN2*WEIGHT(2)  +& 
               CF(3)*NN3*WEIGHT(3)  + CF(4)*NN4*WEIGHT(4))                 
                                                                           
     tmp1    = RESHAPE((/N1,N2/),SHAPE(tmp1))                              
     tmp22   = DF*WEIGHT                                                   
     tmp2    = RESHAPE((/tmp22/),SHAPE(tmp2))                              
     KOFlok  = MATMUL(tmp2,tmp1)                                           
     KOFlok  = (delX/2)*KOFlok                                             
                                                                           
     i1 = k                                                                
     i2 = k+1                                                              
                                                                           
     !matrix assembling                                                    
     MAT(i1:i2,i1:i2)= MAT(i1:i2,i1:i2)+MATlok                             
     DET1(i1:i2,i1:i2)=DET1(i1:i2,i1:i2)+DET1lok                           
     DET2(i1:i2,i1:i2)=DET2(i1:i2,i1:i2)+DET2lok                           
     DET3(i1:i2,i1:i2)=DET3(i1:i2,i1:i2)+DET3lok                           
     KOF(:,i1:i2)=KOF(:,i1:i2)+KOFlok                                      
                                                                           
  END DO solveEQloop                                                       
                                                                           
  DET = DET1 - DET2 -DET3      


! General form of boundary condition is split in two parts: 

! Robin's BC, (special case would be Neuman)
IF (v(2).NE.0) THEN

	KOF(1,N)=KOF(1,N)+W(2)/V(2)  ! On RHS w(2)/u(2) should be added  (for u(2) = 0 this is basically Neumman's boundary condition

    !right-hand side 
	RHS = RESHAPE( (/YM/), SHAPE(RHS))                                       
	RHS = MATMUL(RHS,MAT) + dt_in*KOF

    !left-hand side 
	LHS = MAT - dt_in*DET
    LHS(N,N) = LHS(N,N) + U(2)/V(2)  	! On the LHS u(2)/v(2) should be added for the last element in matrix --- this multiplies the psi(N)

	LHS = TRANSPOSE(LHS)                                                     
                                                                           
	!call sgesv to solve linear eq.                                          
	CALL SGESV(N, 1, LHS, N, pivot, RHS, N, ok)                              
                                                                           
	! the solution is in RHS                
	
	
	
! Dirichle's boundary condition
ELSEIF (v(2).EQ.0) THEN

    !right-hand side 
	RHS = RESHAPE( (/YM/), SHAPE(RHS))                                       
	RHS = MATMUL(RHS,MAT) + dt_in*KOF

    !left-hand side 
	LHS = MAT - dt_in*DET

	! Dirichle's boundary condition requires the following procedure:
	
	! last row and last column thrown out
	MAT_Dirichle(:,:) = LHS(1:N-1,1:N-1) 

	! adding the last column multipied with w(2)/u(2) on the RHS
	KOF_dirichle(1,:) = RHS(1,1:N-1) + W(2)/U(2) * LHS(N,1:N-1)				!LHS(N-1,1:N)

	! transponiram MdtD ... ali treba ti manja Mdt_transp
    MAT_Dirichle = transpose(MAT_Dirichle)

    !call sgesv to solve linear eq.
    call SGESV(N-1, 1, MAT_Dirichle, N-1, pivot_Dirichle, KOF_dirichle, N-1, ok) !PSI=(M-dt*D_D)\(M*psi.'+dt*K);

	RHS(1,1:N-1) = KOF_dirichle(1,:)
	RHS(1,N) = W(2)/U(2)  

ENDIF
                                                                        
   !return solution                                                         
	RHO_LOOP2: DO i=1, N                                                     
		YY(i)  = RHS(1,i)                                                     
	END DO RHO_LOOP2     
	
  !deallocation                                                            
  DEALLOCATE(pivot, pivot_Dirichle)                                                        
  DEALLOCATE(MAT,DET,KOF)                                                  
  DEALLOCATE(DET1,DET2,DET3)                                               
  DEALLOCATE(RHS,LHS)    

  DEALLOCATE(KOF_dirichle, MAT_Dirichle)
                                                                           
END SUBROUTINE fem_pde_solver                                              



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!>  These subroutines calculate derivative,                
!>  DY of function Y respect to argument X                 
!>                                                         
!> \author                                                 
!>                                                         
!> \version "$Id$"                                         
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
SUBROUTINE DERIV_HOLOB(N,X1,F,FC)                          
                                                           
  USE itm_types                                            
                                                           
  IMPLICIT NONE                                            
                                                           
  INTEGER, INTENT(in) :: N                                 
  REAL (r8), INTENT(in) ::   X1(N), F(N)                   
  REAL (r8), INTENT (out) :: Fc(N)                         

  fc(1)= (-1.0)*(-1.0/2.0)*((f(1)-f(2))/(x1(1)-x1(2)))&
       &+(-2.0)*(-1.0)    *((f(1)-f(3))/(x1(1)-x1(3)))&
       &+(-3.0)*(1.0/2.0) *((f(1)-f(4))/(x1(1)-x1(4)))
                                                       
  fc(2)= (-1.0)*(-1.0/2.0)*((f(2)-f(3))/(x1(2)-x1(3)))&
       &+(-2.0)*(-1.0)    *((f(2)-f(4))/(x1(2)-x1(4)))&
       &+(-3.0)*(1.0/2.0) *((f(2)-f(5))/(x1(2)-x1(5)))
                                                       
  fc(3)= ((1.0/4.0)*((f(4)-f(2))/(x1(4)-x1(2)))*(2.0*1.0)) &
       &+((1.0/8.0)*((f(5)-f(1))/(x1(5)-x1(1)))*(2.0*2.0)) 
                                                            
  fc(4:N-1-2)= ((5.0/32.0)*((f(5:N-1-1)-f(3:N-1-3))/(x1(5:N-1-1)-x1(3:N-1-3)))*2.0*1.0)&                                                                        
       &+(1.0/8.00)*((f(6:N-1)  -f(2:N-1-4))/(x1(6:N-1)  -x1(2:N-1-4)))*2.0*2.0&
       &+(1.0/32.0)*((f(7:N-1+1)-f(1:N-1-5))/(x1(7:N-1+1)-x1(1:N-1-5)))*2.0*3.0

        fc(N-1-1)=   (((1.0/4.0)*((f(N-1+1-1)-f(N-1+1-2))/(x1(N-1+1-1)-x1(N-1+1-2)))*(2*1))&
      &+((1.0/8.0)*((f(N-1+1)  -f(N-1+1-4))/(x1(N-1+1)  -x1(N-1+1-4)))*(2*2)))

 fc(N-1)=     (-1.0)*(-1.0/2.0)*((f(N-1+1-1)-f(N-1+1-2))/(x1(N-1+1-1)-x1(N-1+1-2)))&
                           &+(-2.0)*(-1.0)    *((f(N-1+1-1)-f(N-1+1-3))/(x1(N-1+1-1)-x1(N-1+1-3)))&
                           &+(-3.0)*(1.0/2.0) *((f(N-1+1-1)-f(N-1+1-4))/(x1(N-1+1-1)-x1(N-1+1-4)))

 fc(N-1+1)=   (-1.0)*(-1.0/2.0)*((f(N-1+1)-f(N-1+1-1))/(x1(N-1+1)-x1(N-1+1-1)))&
      &+(-2.0)*(-1.0)    *((f(N-1+1)-f(N-1+1-2))/(x1(N-1+1)-x1(N-1+1-2)))&
                           &+(-3.0)*(1.0/2.0) *((f(N-1+1)-f(N-1+1-3))/(x1(N-1+1)-x1(N-1+1-3)))

 RETURN
END SUBROUTINE DERIV_HOLOB

!#endif
