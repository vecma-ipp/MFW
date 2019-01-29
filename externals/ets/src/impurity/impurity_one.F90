! +++          IMPURITY TRANSPORT EQUATIONS  FOR ONE IMPURITY         +++


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
#ifdef GOT_AMNSPROTO
SUBROUTINE IMPURITY_ONE(TE,NE,NZ1,NZM1,VPR,VPRM,R0,BT,BTPRIME,DIFF,FLUX,FLUX_INTER,rho,&
     VCONV,NRHO,SIMP2,NSOURCE,NZ_BND,NZ_BND_TYPE,control_double,CONTROL_INTEGER,G1,IMP_RADIATION,SE_EXP,&
     max_nzimp,amns_ei,amns_rc,amns_lr,amns_br,amns_eip,LIN_RAD1,BREM_RAD1,JON_EN1,REC_LOS1)
#else
SUBROUTINE IMPURITY_ONE(TE,NE,NZ1,NZM1,VPR,VPRM,R0,BT,BTPRIME,DIFF,FLUX,FLUX_INTER,rho,&
     VCONV,NRHO,SIMP2,NSOURCE,NZ_BND,NZ_BND_TYPE,control_double,CONTROL_INTEGER,G1,IMP_RADIATION,SE_EXP,&
     max_nzimp)
#endif

!--------------------------------------------------------------!
!     This subroutine solves impuriy particle transport        !
!     equations for impurity components from 1 to NSTATE,      !
!     and provides: density and flux of impurity components    !
!     from 1 to NSTATE                                         !
!--------------------------------------------------------------!
!     Source:       ---                                        !
!     Developers:   R.Stankiewicz,I.M.Ivanova-Stanik           !
!     Kontacts:     irena@ifpilm.waw.pl                       !
!                                                              !
!     Comments:     might change after the ITM                 !
!                   data stucture is finalized                 !
!                                                              !
!--------------------------------------------------------------!

  USE ITM_TYPES
  USE TYPE_SOLVER
  USE ITM_CONSTANTS
#ifdef GOT_AMNSPROTO
  use amns_types
  use amns_module
#endif    


  IMPLICIT NONE

  
! +++ Input/Output to numerical solver:
  TYPE (NUMERICS)           :: SOLVER		            !contains all I/O quantities to numerics part


! +++ Internal parameters:
  INTEGER                   :: IRHO,    NRHO		    !radius index, number of radial points
  INTEGER                   :: IIMP, SIMP2,SIMP,ISIMP	    !index of impurity component, number of considered impurity components (max ionization state)
  INTEGER                   :: izimp, nzimp
  INTEGER                   :: NZ_BND_TYPE(SIMP2)           !boundary condition, type
  INTEGER                   :: INDEX_T                      !index for linear interpolation for atomic data
  INTEGER                   :: MAX_NZIMP
 
  REAL (R8)                 :: BT, BTM, BTPRIME		    !magnetic field from current time step, [T], previous time steps, [T], time derivative, [T/s]
  REAL (R8)                 :: R0
  REAL (R8)                 :: RHO(NRHO)                    !normalised minor radius,               [m]
  REAL (R8)		    :: VPR(NRHO)                    !V',                                    [m^2]
  REAL (R8)		    :: VPRM(NRHO)                   !V' (at previous time step),            [m^2]
  REAL (R8)                 :: G1(NRHO)                     !<(nabla_rho)^2>,                       [-]
  REAL (R8)                 :: NZ1(NRHO,SIMP2)              !density from current ans previous time step,      [m^-3]
  REAL (R8)                 :: NZM1(NRHO,SIMP2)             !density from previous time step, 
  REAL (R8)		    :: FLUX(NRHO,SIMP2)             !ion flux,                                          [1/s]
  REAL (R8)	            :: FLUX_INTER(NRHO,SIMP2)       !ion flux, contributing to convective heat transport  [1/s]
  REAL (R8)                 :: NE(NRHO),TE(NRHO)            !electron density                                    [m^-3]
  REAL (R8)                 :: DIFF(NRHO,SIMP2)             !diffusion coefficient                  [m^2/s] and [m/s]
  REAL (R8)                 :: VCONV(NRHO,simp2)            !pinch velocity,                 [m^2/s] and [m/s]
  REAL (R8)                 :: NSOURCE(NRHO,SIMP2)
  REAL (R8)                 :: NZ_BND(3,SIMP2)              !boundary condition, value, [depends on NZ_BND_TYPE]
  
  REAL (R8)                 :: REAL_INDEX_T                 !index for linear interpolation for atomic data
  REAL (R8)                 :: ALFA(NRHO,SIMP2)	            !atom data: ionization coefficient(after interpolation)[m^3/s]
  REAL (R8)                 :: BETA(NRHO,SIMP2)	            !atom data: recombination coefficient    (after interpolation)  [m^3/s]
  REAL (R8)                 :: GAMA(NRHO,SIMP2)	            !atom data: cooling coefficient          (after interpolation)  
  REAL (R8)                 :: SLIN(NRHO,SIMP2)	            !atom data: linear radiation             (after interpolation)  
  REAL (R8)                 :: IMP_RADIATION(NRHO,SIMP2)    !impurity radiation        
  REAL (R8)                 :: ALFA_NZ(max_nzimp+2,500)     !atom data: ionization coefficient        [m^3/s]
  REAL (R8)                 :: BETA_NZ(max_nzimp+2,500)     !atom data: recombination coefficient     [m^3/s]   
  REAL (R8)                 :: SLIN_NZ(max_nzimp+2,500)     !atom data: linear radiation 
  REAL (R8)                 :: GAMA_NZ(max_nzimp+2,500)     !atom data: cooling coefficient
  REAL (R8)                 :: POTENTIAL(nrho,1:max_nzimp+2)
  REAL (R8)                 :: AMIX, TAU                    !mixing factor, time step,              [s]

! radiation
  REAL (R8)                 :: LIN_RAD1(NRHO,SIMP2)	    !profile of lineradiation for one impurity 
  REAL (R8)                 :: BREM_RAD1(NRHO,SIMP2)	    !profile of bremst. for one impurity  
  REAL (R8)                 :: JON_EN1(NRHO,SIMP2)	    !profile of jonisation energy for one impurity  
  REAL (R8)                 :: REC_LOS1(NRHO,SIMP2)	    !profile of bremst. for one impurity  

  REAL (R8)                 :: SE_EXP(NRHO)	   
     
  INTEGER                   :: solut_method
  INTEGER                   :: FLAG                         !flag for equation: 0 - interpretative (not solved), 1 - predictive (solved)
  INTEGER                   :: NDIM                         !number of equations to be solved
  INTEGER                   :: SOLVER_TYPE                  !specifies the option for numerical solution
  REAL (R8)                 :: Y(NRHO)                      !function at the current amd previous time steps
  REAL (R8)                 :: YM(NRHO)                     !function at the current amd previous time steps
  REAL (R8)                 :: DY(NRHO)                     !derivative of function
  REAL (R8)                 :: A(NRHO)                      !coefficients for numerical solver
  REAL (R8)                 :: B(NRHO)                      !coefficients for numerical solver
  REAL (R8)                 :: C(NRHO)                      !coefficients for numerical solver
  REAL (R8)                 :: D(NRHO)                      !coefficients for numerical solver
  REAL (R8)                 :: E(NRHO)                      !coefficients for numerical solver
  REAL (R8)                 :: F(NRHO)                      !coefficients for numerical solver
  REAL (R8)                 :: G(NRHO)                      !coefficients for numerical solver
  REAL (R8)                 :: H                            !coefficients for numerical solver
  REAL (R8)                 :: V(2), U(2), W(2)             !boundary conditions for numerical solver
  
  REAL (R8)                 :: FUN1(NRHO), INTFUN1(NRHO)
  
 
  INTEGER,     INTENT(IN)   :: CONTROL_INTEGER(2)           !integer control parameters
  REAL (R8),   INTENT(IN)   :: CONTROL_DOUBLE(5)            !real control parameters
  
  REAL (R8)                 :: DNZ(NRHO)                    !density gradient,                                         [m^-4]
  REAL (R8)                 :: INT_SOURCE(NRHO)             !integral of source                                        [1/s]
  REAL (R8)                 :: NZP(NRHO,SIMP2)              !impurity density (Alternated Direction method)
  REAL (R8)                 :: A1p(NRHO), B1p(NRHO)         !coefficients for numerical solver (AD method)
  REAL (R8)                 :: C1p(NRHO), D1pp(NRHO)        !coefficients for numerical solver (AD method)
  REAL (R8)                 :: Vp(2), Up(2), Wp(2)          !coefficients for numerical solver (AD method)
  REAL (R8)                 :: DRHO, DRHOP, DRHOM    
  REAL (R8)                 :: A2p(SIMP2), B2p(SIMP2)       !coefficients for numerical solver (AD method)
  REAL (R8)                 :: C2p(SIMP2), D2pp(SIMP2)      !coefficients for numerical solver (AD method)
  
  REAL (R8)                 :: TIME 
  
  REAL                      :: DensityFluxIn(NRHO)
  REAL                      :: FluxIn, FluxOut
  REAL                      :: CalkaTrapez
! declaration for cubint
  INTEGER                   ::ntab
  REAL                      ::xtab(NRHO)
  REAL                      ::ftab(NRHO)
  INTEGER                   ::ia_in
  INTEGER                   ::ib_in
  REAL                      ::result
  REAL                      ::error
! ***************  
  INTEGER                   :: ifail

#ifdef GOT_AMNSPROTO
  type (amns_handle_rx_type) :: amns_ei(1:max_nzimp+1), amns_rc(1:max_nzimp+1), &
       amns_lr(1:max_nzimp+1), amns_br(1:max_nzimp+1),amns_eip(1:max_nzimp+1)
#endif
     
! +++ Set up dimensions:
  NDIM                  = 1                               !no coupling between density equations
  SIMP                  = SIMP2-2      
  nzimp                 = simp
  
! +++ Allocate types for interface with numerical solver:
  CALL  ALLOCATE_NUMERICS (NDIM, NRHO, SOLVER, ifail)
     

! +++ Set up local variables:

!irena
  solut_method          = 1                           !temporary fix

  AMIX                  = CONTROL_DOUBLE(2)
  TAU                   = CONTROL_DOUBLE(1)           ! 0.5 - Lackner's method
  SOLVER_TYPE           = CONTROL_INTEGER(1) 

  BTM			=BT-BTPRIME*TAU
     
! +++ AtomData Coefficients: Linear Interpolation

  ALFA        = 0.0D0
  BETA        = 0.0D0
  GAMA        = 0.0D0
  SLIN        = 0.0D0
  POTENTIAL   = 0.0D0
!++++ radiation+energy losses
  LIN_RAD1    = 0.0D0
  BREM_RAD1   = 0.0D0
  JON_EN1     = 0.0D0
  REC_LOS1     = 0.0D0
  
  
#ifdef GOT_AMNSPROTO
  do izimp=1, nzimp+1
        
 
     
      
     if(izimp .ne. nzimp+1) then
        call ITM_AMNS_RX(amns_ei(izimp),alfa(:,izimp),te,ne)
!DPC            write(*,*) 'alfa(nrho,izimp)=',alfa(nrho,izimp)
        call ITM_AMNS_RX(amns_lr(izimp),slin(:,izimp),te,ne)             ! guess: slin == line radiation loss
	call ITM_AMNS_RX(amns_eip(izimp+1),potential(:,izimp),te,ne)     ! guess: potential= potential of ionization
             
            
     endif
     
     if(izimp .ne. 1) then
        call ITM_AMNS_RX(amns_rc(izimp),beta(:,izimp),te,ne)
        call ITM_AMNS_RX(amns_br(izimp),gama(:,izimp),te,ne)             ! guess: gama == bremsstrahlung + recombination energy loss
	
     endif
    
      
  enddo
   
 
   
!!!      stop
#else
    
  call READ_DATA (ALFA_NZ,BETA_NZ,Gama_nz,SLIN_NZ,max_nzimp,simp)
  slin_nz = slin_nz * 1.E-30        ! DPC addition
  gama_nz = gama_nz * 1.E-20        ! DPC addition --- this one is just a guess!

		

  RHO_LOOPA: DO IRHO=1, NRHO  
      
     INDEX_T = INT( 499.0/5.0*log10(TE(IRHO)) ) + 1
     REAL_INDEX_T =  499.0/5.0*log10(TE(IRHO)) + 1.0 
  
     IMP_LOOPA: DO ISIMP=1,SIMP+1  

        ALFA(IRHO,ISIMP) = ( ALFA_NZ(ISIMP,INDEX_T+1) - ALFA_NZ(ISIMP,INDEX_T) )*(REAL_INDEX_T - INDEX_T) + ALFA_NZ(ISIMP,INDEX_T)
        BETA(IRHO,ISIMP) = ( BETA_NZ(ISIMP,INDEX_T+1) - BETA_NZ(ISIMP,INDEX_T) )*(REAL_INDEX_T - INDEX_T) + BETA_NZ(ISIMP,INDEX_T)
        SLIN(IRHO,ISIMP) = ( SLIN_NZ(ISIMP,INDEX_T+1) - SLIN_NZ(ISIMP,INDEX_T) )*(REAL_INDEX_T - INDEX_T) + SLIN_NZ(ISIMP,INDEX_T)
        GAMA(IRHO,ISIMP) = ( GAMA_NZ(ISIMP,INDEX_T+1) - GAMA_NZ(ISIMP,INDEX_T) )*(REAL_INDEX_T - INDEX_T) + GAMA_NZ(ISIMP,INDEX_T)

     END DO IMP_LOOPA
     
  END DO RHO_LOOPA
#endif


!DPC  write(*,'(a,1p,100(e15.6))') 'alfa: ',      (alfa(nrho/2,izimp),izimp=1,nzimp+1)
!DPC  write(*,'(a,1p,100(e15.6))') 'beta: ',      (beta(nrho/2,izimp),izimp=1,nzimp+1)
!DPC  write(*,'(a,1p,100(e15.6))') 'gama: ',      (gama(nrho/2,izimp),izimp=1,nzimp+1)
!DPC  write(*,'(a,1p,100(e15.6))') 'slin: ',      (slin(nrho/2,izimp),izimp=1,nzimp+1)
!DPC  write(*,'(a,1p,100(e15.6))') 'potential: ', (potential(nrho/2,izimp),izimp=1,nzimp+1)

!DPC  write(*,'(a,1p,100(e25.16))') 'BOUNDARY atomic physics', &
!DPC       te(nrho), ne(nrho), alfa(nrho,1:nzimp+1), slin(nrho,1:nzimp+1), beta(nrho,1:nzimp+1), gama(nrho,1:nzimp+1)

  SELECT CASE (SOLUT_METHOD)


  CASE(1)

   

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!    solution of particle transport equation for
!    individual state of impurity
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + 

     IMP_LOOP1: DO ISIMP=2,SIMP+1      ! "1" - Neutral impurity, "2" - "+1", ...

! +++ Set equation to 'predictive' and all coefficients to zero:
        FLAG         = 1
        Y(:)         = 0.0D0
        DY(:)        = 0.0D0
        YM(:)        = 0.0D0
        A(:)         = 0.0D0
        B(:)         = 0.0D0
        C(:)         = 0.0D0
        D(:)         = 0.0D0
        E(:)         = 0.0D0
        F(:)         = 0.0D0
        G(:)         = 0.0D0
        H            = 0.0D0
        V(:)         = 0.0D0
        U(:)         = 0.0D0
        W(:)         = 0.0D0


! +++ Coefficients for ion diffusion equation  in form:
!
!     (A*Y-B*Y(t-1))/H + 1/C * (-D*Y' + E*Y) = F - G*Y

        RHO_LOOP3: DO IRHO=1,NRHO
           
           Y(IRHO)           = NZ1(IRHO,ISIMP)
           YM(IRHO)          = NZM1(IRHO,ISIMP)
           A(IRHO)           = VPR(IRHO)
           B(IRHO)           = VPRM(IRHO) 
           C(IRHO)           = 1.D0
           D(IRHO)           = VPR(IRHO)*G1(IRHO)*DIFF(IRHO,ISIMP)
           E(IRHO)           = VPR(IRHO)*G1(IRHO)*VCONV(IRHO,ISIMP)                    &
                - BTPRIME/2.D0/BT*RHO(IRHO)*VPR(IRHO)
           F(IRHO)           = VPR(IRHO)*NSOURCE(IRHO,ISIMP)+VPR(IRHO)*( NZ1(IRHO,ISIMP-1)*NE(IRHO)*ALFA(IRHO,ISIMP-1)  &
                + NZ1(IRHO,ISIMP+1)*NE(IRHO)*BETA(IRHO,ISIMP+1) )
           G(IRHO)           = VPR(IRHO)*(NE(IRHO)*ALFA(IRHO,ISIMP) + NE(IRHO)*BETA(IRHO,ISIMP) )    
           IF (IRHO.eq.nrho) then
			 


           ENDIF

        END DO RHO_LOOP3
 

        H                  = 0.5*TAU
! +++ Boundary conditions for ion diffusion equation in form:
!
!     V*Y' + U*Y =W 
!
! +++ On axis:
!       dNZ/drho(rho=0)=0:
        IF(DIFF(1,ISIMP).GT.0.D0) THEN
           V(1) = -DIFF(1,ISIMP)
           U(1) = VCONV(1,ISIMP)
           W(1) = 0.D0
        ELSE
           V(1) = 1.D0
           U(1) = 0.D0
           W(1) = 0.D0
        END IF


! +++ At the edge:
!       FIXED NZ

        IF(NZ_BND_TYPE(ISIMP).EQ.1) THEN
           V(2) = 0.D0
           U(2) = 1.D0
           W(2) = NZ_BND(1,ISIMP)
        ENDIF

!       FIXED grad_NZ
        IF(NZ_BND_TYPE(ISIMP).EQ.2) THEN
           V(2) = 1.D0
           U(2) = 0.D0
           W(2) = NZ_BND(1,ISIMP)
        ENDIF
        
!       FIXED L_NZ
        IF(NZ_BND_TYPE(ISIMP).EQ.3) THEN
           V(2) = NZ_BND(1,ISIMP)
           U(2) = 1.D0
           W(2) = 0.D0
        ENDIF

!       FIXED Flux_NZ
        IF(NZ_BND_TYPE(ISIMP).EQ.4) THEN
           V(2) = -G1(NRHO)*DIFF(NRHO,ISIMP)*VPR(NRHO)
           U(2) = G1(NRHO)*VCONV(NRHO,ISIMP)*VPR(NRHO)
           W(2) = NZ_BND(1,ISIMP)
        ENDIF
        
!       Generic boundary condition
        IF(NZ_BND_TYPE(ISIMP).EQ.5) THEN
           V(2) = NZ_BND(1,ISIMP)
           U(2) = NZ_BND(2,ISIMP)
           W(2) = NZ_BND(3,ISIMP)
        ENDIF

! +++ Density equation is not solved:
        IF(NZ_BND_TYPE(ISIMP).EQ.0) THEN

           CALL DERIVN (NRHO,RHO,Y,DY)                       !temperature gradient

           FLAG       = 0

           RHO_LOOP4: DO IRHO=1,NRHO

              A(IRHO)   = 1.0D0
              B(IRHO)   = 1.0D0
              C(IRHO)   = 1.0D0
              D(IRHO)   = 0.0D0
              E(IRHO)   = 0.0D0
              F(IRHO)   = 0.0D0
              G(IRHO)   = 0.0D0  
              
           END DO RHO_LOOP4

           V(2)         = 0.0D0
           U(2)         = 1.0D0
           W(2)         = Y(NRHO)
        END IF



! +++ Defining coefficients for numerical solver:    
        SOLVER%TYPE                   = SOLVER_TYPE
        SOLVER%EQ_FLAG(NDIM)          = FLAG
        SOLVER%NDIM                   = NDIM
        SOLVER%NRHO                   = NRHO
        SOLVER%AMIX                   = AMIX

 
        RHO_LOOP5: DO IRHO=1,NRHO

           SOLVER%RHO(IRHO)            = RHO(IRHO)
           SOLVER%Y(NDIM,IRHO)         = Y(IRHO)
           SOLVER%DY(NDIM,IRHO)        = DY(IRHO)
           SOLVER%YM(NDIM,IRHO)        = YM(IRHO)
           SOLVER%A(NDIM,IRHO)         = A(IRHO)
           SOLVER%B(NDIM,IRHO)         = B(IRHO) 
           SOLVER%C(NDIM,IRHO)         = C(IRHO)
           SOLVER%D(NDIM,IRHO)         = D(IRHO)
           SOLVER%E(NDIM,IRHO)         = E(IRHO)
           SOLVER%F(NDIM,IRHO)         = F(IRHO)
           SOLVER%G(NDIM,IRHO)         = G(IRHO)

        END DO RHO_LOOP5
        
        SOLVER%H                      = H
        
        SOLVER%V(NDIM,1)              = V(1)
        SOLVER%U(NDIM,1)              = U(1)
        SOLVER%W(NDIM,1)              = W(1)
        SOLVER%V(NDIM,2)              = V(2)
        SOLVER%U(NDIM,2)              = U(2)
        SOLVER%W(NDIM,2)              = W(2)


! +++ Solution of density diffusion equation:            
        CALL SOLUTION_INTERFACE (SOLVER, ifail)
     

! +++ New impurity density:  
        RHO_LOOP6: DO IRHO=1,NRHO

           Y(IRHO)                  = SOLVER%Y(NDIM,IRHO)
           DY(IRHO)                 = SOLVER%DY(NDIM,IRHO)

        END DO RHO_LOOP6



! +++ New profiles of impurity density flux and integral source:                
        RHO_LOOP7: DO IRHO=1,NRHO
           
           NZM1(IRHO,ISIMP)   = NZ1(IRHO,ISIMP)
           NZ1(IRHO,ISIMP)    = Y(IRHO)                                        
           DNZ(IRHO)   = DY(IRHO) 
           IF (RHO(IRHO).NE.0.0D0) THEN
              FUN1(IRHO)  =  1.D0/RHO(IRHO)*(VPR(IRHO)*NSOURCE(IRHO,ISIMP)      &    
                   + VPRM(IRHO)*NZM1(IRHO,ISIMP)/TAU                    &
                   - NZ1(IRHO,ISIMP)*VPR(IRHO)*(1.D0/TAU)) 
           ELSE IF (RHO(IRHO).EQ.0.0D0) THEN
              FUN1(IRHO)  =  4.D0*ITM_PI**2*R0* (NSOURCE(IRHO,ISIMP)            &    
                   + NZM1(IRHO,ISIMP)/TAU - NZ1(IRHO,ISIMP)*(1.D0/TAU))
           END IF

    

        END DO RHO_LOOP7

			

        CALL INTEGR(NRHO,RHO,FUN1,INTFUN1)                     !Integral source 

        RHO_LOOP8: DO IRHO=1,NRHO
           Flux_inter(IRHO,ISIMP)     = INTFUN1(IRHO)                          &
                + BTPRIME/2.D0/BT*RHO(IRHO)*VPR(IRHO)*Y(IRHO)
           

           FLUX(IRHO,ISIMP)    = VPR(IRHO)*G1(IRHO)*                    &
                ( Y(IRHO)*VCONV(IRHO,ISIMP) - DY(IRHO)*DIFF(IRHO,ISIMP) )
           
        END DO RHO_LOOP8

 
     END DO IMP_LOOP1


     LOOP_CHANGE: DO ISIMP=2,SIMP+1
        DO IRHO=1,NRHO
           
           NZM1(IRHO,ISIMP)=NZ1(IRHO,ISIMP)
           
        end do
     end do LOOP_CHANGE
     




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!    solution of particle transport equation for
!    individual state of impurity
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

     IMP_LOOP21: DO ISIMP=SIMP+1, 2, -1      ! "1" - Neutral impurity, "2" - "+1", ...




! +++ Set equation to 'predictive' and all coefficients to zero:
        FLAG         = 1
        Y(:)         = 0.0D0
        DY(:)        = 0.0D0
        YM(:)        = 0.0D0
        A(:)         = 0.0D0
        B(:)         = 0.0D0
        C(:)         = 0.0D0
        D(:)         = 0.0D0
        E(:)         = 0.0D0
        F(:)         = 0.0D0
        G(:)         = 0.0D0
        H            = 0.0D0
        V(:)         = 0.0D0
        U(:)         = 0.0D0
        W(:)         = 0.0D0 

! +++ Coefficients for ion diffusion equation  in form:
!
!     (A*Y-B*Y(t-1))/H + 1/C * (-D*Y' + E*Y) = F - G*Y

        RHO_LOOP23: DO IRHO=1,NRHO

           Y(IRHO)           = NZ1(IRHO,ISIMP)
           YM(IRHO)          = NZM1(IRHO,ISIMP)
           A(IRHO)           = VPR(IRHO)
           B(IRHO)           = VPRM(IRHO) 
           C(IRHO)           = 1.D0
           D(IRHO)           = VPR(IRHO)*G1(IRHO)*DIFF(IRHO,ISIMP)
           E(IRHO)           = VPR(IRHO)*G1(IRHO)*VCONV(IRHO,ISIMP)                    &
                - BTPRIME/2.D0/BT*RHO(IRHO)*VPR(IRHO)
           F(IRHO)           = VPR(IRHO)*NSOURCE(IRHO,ISIMP)+VPR(IRHO)*(NZ1(IRHO,ISIMP-1)*NE(IRHO)*ALFA(IRHO,ISIMP-1) &
                + NZ1(IRHO,ISIMP+1)*NE(IRHO)*BETA(IRHO,ISIMP+1) )
           G(IRHO)           = VPR(IRHO)*(NE(IRHO)*ALFA(IRHO,ISIMP) + NE(IRHO)*BETA(IRHO,ISIMP) ) 
	
	        
	
			    
        END DO RHO_LOOP23

        H            = 0.5*TAU
             
			 


! +++ Boundary conditions for ion diffusion equation in form:
!
!     V*Y' + U*Y =W 
!
! +++ On axis:
        IF(DIFF(1,ISIMP).GT.0.D0) THEN
           V(1) = -DIFF(1,ISIMP)
           U(1) = VCONV(1,ISIMP)
           W(1) = 0.D0
        ELSE
           V(1) = 1.D0
           U(1) = 0.D0
           W(1) = 0.D0
        END IF

! +++ At the edge:
!       FIXED NZ
!!write(*,*)'NZ_BND_TYPE(ISIMP)=',NZ_BND_TYPE(ISIMP),'odwrotnej'
!!pause
        IF(NZ_BND_TYPE(ISIMP).EQ.1) THEN
           V(2) = 0.D0
           U(2) = 1.D0
           W(2) = NZ_BND(1,ISIMP)
        ENDIF
        
!       FIXED grad_NZ
        IF(NZ_BND_TYPE(ISIMP).EQ.2) THEN
           V(2) = 1.D0
           U(2) = 0.D0
           W(2) = NZ_BND(1,ISIMP)
        ENDIF

!       FIXED L_NZ
        IF(NZ_BND_TYPE(ISIMP).EQ.3) THEN
           V(2) = NZ_BND(1,ISIMP)
           U(2) = 1.D0
           W(2) = 0.D0
        ENDIF

!       FIXED Flux_NZ
        IF(NZ_BND_TYPE(ISIMP).EQ.4) THEN
           V(2) = -G1(NRHO)*DIFF(NRHO,ISIMP)*VPR(NRHO)
           U(2) = G1(NRHO)*VCONV(NRHO,ISIMP)*VPR(NRHO) 
           W(2) = NZ_BND(1,ISIMP)
        ENDIF

!       Generic boundary condition
        IF(NZ_BND_TYPE(ISIMP).EQ.5) THEN
           V(2) = NZ_BND(1,ISIMP)
           U(2) = NZ_BND(2,ISIMP)
           W(2) = NZ_BND(3,ISIMP)
        ENDIF



! +++ Density equation is not solved:
        IF(NZ_BND_TYPE(ISIMP).EQ.0) THEN

           CALL DERIVN (NRHO,RHO,Y,DY)                       !temperature gradient

           FLAG       = 0

           RHO_LOOP24: DO IRHO=1,NRHO

              A(IRHO)   = 1.0D0
              B(IRHO)   = 1.0D0
              C(IRHO)   = 1.0D0
              D(IRHO)   = 0.0D0
              E(IRHO)   = 0.0D0
              F(IRHO)   = 0.0D0
              G(IRHO)   = 0.0D0  
              
           END DO RHO_LOOP24
           
           V(2)         = 0.0D0
           U(2)         = 1.0D0
           W(2)         = Y(NRHO)
        END IF


! +++ Defining coefficients for numerical solver:    
        SOLVER%TYPE                   = SOLVER_TYPE
        SOLVER%EQ_FLAG(NDIM)          = FLAG
        SOLVER%NDIM                   = NDIM
        SOLVER%NRHO                   = NRHO
        SOLVER%AMIX                   = AMIX
 
        RHO_LOOP25: DO IRHO=1,NRHO

           SOLVER%RHO(IRHO)            = RHO(IRHO)
           SOLVER%Y(NDIM,IRHO)         = Y(IRHO)
           SOLVER%DY(NDIM,IRHO)        = DY(IRHO)
           SOLVER%YM(NDIM,IRHO)        = YM(IRHO)
           SOLVER%A(NDIM,IRHO)         = A(IRHO)
           SOLVER%B(NDIM,IRHO)         = B(IRHO) 
           SOLVER%C(NDIM,IRHO)         = C(IRHO)
           SOLVER%D(NDIM,IRHO)         = D(IRHO)
           SOLVER%E(NDIM,IRHO)         = E(IRHO)
           SOLVER%F(NDIM,IRHO)         = F(IRHO)
           SOLVER%G(NDIM,IRHO)         = G(IRHO)
           
        END DO RHO_LOOP25

        SOLVER%H                      = H
        SOLVER%V(NDIM,1)              = V(1)
        SOLVER%U(NDIM,1)              = U(1)
        SOLVER%W(NDIM,1)              = W(1)
        SOLVER%V(NDIM,2)              = V(2)
        SOLVER%U(NDIM,2)              = U(2)
        SOLVER%W(NDIM,2)              = W(2)



! +++ Solution of density diffusion equation:            
        CALL SOLUTION_INTERFACE (SOLVER, ifail)
     

! +++ New impurity density:  
        RHO_LOOP26: DO IRHO=1,NRHO

           Y(IRHO)                  = SOLVER%Y(NDIM,IRHO)
           DY(IRHO)                 = SOLVER%DY(NDIM,IRHO)

        END DO RHO_LOOP26

! +++ New profiles of impurity density flux and integral source:                
        RHO_LOOP27: DO IRHO=1,NRHO
           
           NZM1(IRHO,ISIMP)   = NZ1(IRHO,ISIMP)
           NZ1(IRHO,ISIMP)    = Y(IRHO)                                        
           DNZ(IRHO)   = DY(IRHO)    
           IF (RHO(IRHO).NE.0.0D0) THEN
              FUN1(IRHO)  = 1.D0/RHO(IRHO)*(VPR(IRHO)*NSOURCE(IRHO,ISIMP)          &
                   + VPRM(IRHO)*NZM1(IRHO,ISIMP)/TAU                       &
                   - NZ1(IRHO,ISIMP)*VPR(IRHO)*(1.D0/TAU))   
           ELSE IF (RHO(IRHO).EQ.0.0D0) THEN
              FUN1(IRHO)  = 4.0d0*ITM_PI**2*R0*(NSOURCE(IRHO,ISIMP)                &
                   + NZM1(IRHO,ISIMP)/TAU                                  &
                   - NZ1(IRHO,ISIMP)*(1.D0/TAU))   
           END IF
           
        END DO RHO_LOOP27
        
        
        
			 
        CALL INTEGR(NRHO,RHO,FUN1,INTFUN1)                     !Integral source 

        RHO_LOOP28: DO IRHO=1,NRHO


           Flux_inter(IRHO,ISIMP)     = INTFUN1(IRHO)                          &
                + BTPRIME/2.D0/BT*RHO(IRHO)*VPR(IRHO)*Y(IRHO)


           FLUX(IRHO,ISIMP)           = VPR(IRHO)*G1(IRHO)*                    &
                ( Y(IRHO)*VCONV(IRHO,ISIMP) - DY(IRHO)*DIFF(IRHO,ISIMP) )

        END DO RHO_LOOP28

     END DO IMP_LOOP21



  CASE(2)

! + + Metoda naprzemiennych kierunkow   + +

! + + + + + + + + + + + + + + + + + + + + +


! Definition of Progonka Coefficients - SpaceLoop

     
     DO ISIMP=2,SIMP+1    ! Impurity State Loop

        NZP = 0.0

! +++ Boundary conditions for ion diffusion equation in form:
!
!     V*Y' + U*Y =W 
!
! +++ On axis:
        IF(DIFF(1,ISIMP).GT.0.D0) THEN
           V(1) = -DIFF(1,ISIMP)
           U(1) = VCONV(1,ISIMP)
           W(1) = 0.D0
        ELSE
           V(1) = 1.D0
           U(1) = 0.D0
           W(1) = 0.D0
        END IF

! +++ At the edge:
!       FIXED NZ
        IF(NZ_BND_TYPE(ISIMP).EQ.1) THEN
           V(2) = 0.D0
           U(2) = 1.D0
           W(2) = NZ_BND(1,ISIMP)
        ENDIF

!       FIXED grad_NZ
        IF(NZ_BND_TYPE(ISIMP).EQ.2) THEN
           V(2) = 1.D0
           U(2) = 0.D0
           W(2) = NZ_BND(1,ISIMP)
        ENDIF

!       FIXED L_NZ
        IF(NZ_BND_TYPE(ISIMP).EQ.3) THEN
           V(2) = NZ_BND(1,ISIMP)
           U(2) = 1.D0
           W(2) = 0.D0
        ENDIF

!       FIXED Flux_NZ
        IF(NZ_BND_TYPE(ISIMP).EQ.4) THEN
           V(2) = -G(NRHO)*DIFF(NRHO,ISIMP)
           U(2) = G(NRHO)*VCONV(NRHO,ISIMP)
           W(2) = NZ_BND(1,ISIMP)
        ENDIF

!       Generic boundary condition
        IF(NZ_BND_TYPE(ISIMP).EQ.5) THEN
           V(2) = NZ_BND(1,ISIMP)
           U(2) = NZ_BND(2,ISIMP)
           W(2) = NZ_BND(3,ISIMP)
        ENDIF


! For progonka:

        Vp(1) = U(1) - V(1)/(RHO(2)-RHO(1))
        Up(1) = V(1)/(RHO(2)-RHO(1))
        Wp(1) = W(1)
        
        Vp(2) = U(2) + V(2)/(RHO(NRHO)-RHO(NRHO-1))
        Up(2) = - V(2)/(RHO(NRHO)-RHO(NRHO-1))
        Wp(2) = W(2)
        
 

  ! +++ Coefficients for ion diffusion equation  in form:
!
!     (A*Y-B*Y(t-1))/H + 1/C * (-D*Y' + E*Y) = 0

        RHO_LOOP33: DO IRHO=1,NRHO
          
         
           A(IRHO)           = VPR(IRHO)
           B(IRHO)           = VPRM(IRHO) 
           C(IRHO)           = 1.D0
           D(IRHO)           = VPR(IRHO)*G1(IRHO)*DIFF(IRHO,ISIMP)
           E(IRHO)           = VPR(IRHO)*G1(IRHO)*VCONV(IRHO,ISIMP)                    &
                - BTPRIME/2.D0/BT*RHO(IRHO)*VPR(IRHO)
        END DO RHO_LOOP33
        H                  = TAU
        
        A1p = 0.0
        B1p = 0.0
        C1p = 0.0
        D1pp = 0.0	  


        DO IRHO=2,NRHO-1 ! SPACE LOOP

           dRHO =  0.5*(RHO(IRHO+1)-RHO(IRHO-1))
           
           dRHOp = RHO(IRHO+1)-RHO(IRHO)
           
           dRHOm = RHO(IRHO)-RHO(IRHO-1)
           
           
           A1p(IRHO) = H/dRHO/C(IRHO)*( 0.5*(D(IRHO+1)+D(IRHO))/dRHOp ) - &
                H/dRHO/C(IRHO)*( 0.5*(E(IRHO)+E(IRHO)) )
           
           B1p(IRHO) = A(IRHO) + &
                H/dRHO/C(IRHO)*( 0.5*(D(IRHO+1)+D(IRHO))/dRHOp + 0.5*(D(IRHO)+D(IRHO-1))/dRHOm ) - &
                H/dRHO/C(IRHO)*( 0.25*(E(IRHO+1)-E(IRHO-1)) )
           
           C1p(IRHO) =  H/dRHO/C(IRHO)*( 0.5*(D(IRHO)+D(IRHO-1))/dRHOm ) + &
                H/dRHO/C(IRHO)*( 0.5*(E(IRHO)+E(IRHO-1)) )
           
           D1pp(IRHO) = B(IRHO)*NZ1(IRHO,ISIMP)
           
        END DO ! SPACE LOOP

! Finding Ni(NP) - Solotion of Set of Algrbraic Equation by Progonka
!
! -Ap*Ni(+1,t+dt) + Bp*Ni(0,t+dt) - Cp*Ni(-1,t+dt) = Dpp

 
        CALL UNHIDE_IIMP(NRHO, SIMP,ISIMP, A1p, B1p, C1p, D1pp, Vp, Up, Wp, NZP)


! Przepisanie macierzy 
			  
        NZ1(:,ISIMP) = NZP(:,ISIMP)
	 
     END DO  !IONS LOOP =========



     DO IRHO=2, NRHO-1  ! SPACE LOOP


! Definition of Progonka Coefficients - IonizationState


        NZP = 0.0
        
        A2p = 0.0
        B2p = 0.0
        C2p = 0.0
        D2pp = 0.0



        DO ISIMP=2,SIMP+1  ! ION LOOP
 

           A2p(ISIMP) = A(IRHO)*NE(IRHO)*BETA(IRHO,ISIMP+1)*TAU
           
           B2p(ISIMP) = A(IRHO)*NE(IRHO)*( ALFA(IRHO,ISIMP)+BETA(IRHO,ISIMP) )*TAU + A(IRHO)
           
           C2p(ISIMP) = A(IRHO)*NE(IRHO)*ALFA(IRHO,ISIMP-1)*TAU
           
           D2pp(ISIMP) = A(IRHO)*NZ1(IRHO,ISIMP)
           
        END DO ! ION LOOP


! Finding Ni(NP) - Solotion of Set of Algrbraic Equation by Progonka
!
! -Ap*Ni(+1,t+dt) + Bp*Ni(0,t+dt) - Cp*Ni(-1,t+dt) = dp

		 
        CALL UNHIDE_NRHO(NRHO, SIMP, IRHO, A2p, B2p, C2p, D2pp, NZ1(IRHO,1), NZP)

! Rewriting matrix for Next Time Step
			  
        NZ1(IRHO,:) = NZP(IRHO,:)


     ENDDO   ! SPACE LOOP 



  END SELECT



  FluxIn = 0.0
  FluxOut = 0.0
  
  
  DO IRHO=1,NRHO
     
     DensityFluxIn(IRHO) = ALFA(IRHO,1)*VPR(IRHO)*NZ1(IRHO,1)*NE(IRHO)-BETA(IRHO,2)*VPR(IRHO)*NZ1(IRHO,2)*NE(IRHO)
     
  ENDDO
  
  
  FluxIn = CalkaTrapez( DensityFluxIn, NRHO-1, RHO(NRHO)-RHO(NRHO-1) )
  
  DO ISIMP=2, SIMP+1
     
     FluxOut = FluxOut - VPR(NRHO)*G1(NRHO)*DIFF(NRHO,ISIMP)*(NZ1(NRHO,ISIMP)-NZ1(NRHO-1,ISIMP))/(RHO(NRHO)-RHO(NRHO-1))
     
  END DO
  
				

  !WRITE(*,*) ALFA(2,2),FluxIn, FluxOut, NRHO
!				pause

! Flux after "progonka"

                
  do ISIMP=1,simp+1
     DO IRHO=1,NRHO
        
        
        Y(IRHO)=NZ1(IRHO,ISIMP)                                         
        
        IF (RHO(IRHO).NE.0.0D0) THEN
           FUN1(IRHO)  = 1.D0/RHO(IRHO)*(VPR(IRHO)*NSOURCE(IRHO,ISIMP)          &
                +VPRM(IRHO)*NZM1(IRHO,ISIMP)/TAU                       &
                -NZ1(IRHO,ISIMP)*VPR(IRHO)*(1.D0/TAU))   
        ELSE IF (RHO(IRHO).EQ.0.0D0) THEN
           FUN1(IRHO)  = 4.D0*ITM_PI**2*R0*(NSOURCE(IRHO,ISIMP)          &
                +NZM1(IRHO,ISIMP)/TAU                       &
                -NZ1(IRHO,ISIMP)*(1.D0/TAU))   
        END IF
        
     END DO
     
     CALL INTEGR(NRHO,RHO,FUN1,INTFUN1)                     !Integral source 
     
     CALL DERIVN (NRHO,RHO,Y,DY)
     
     DO IRHO=1,NRHO
        
        
        Flux_inter(IRHO,ISIMP)     = INTFUN1(IRHO)                          &
             + BTPRIME/2.D0/BT*RHO(IRHO)*VPR(IRHO)*y(IRHO)
        
        
        FLUX(IRHO,ISIMP)           = VPR(IRHO)*G1(IRHO)*                    &
             ( Y(IRHO)*VCONV(IRHO,ISIMP) - DY(IRHO)*DIFF(IRHO,ISIMP) )
        
     END DO
     
  END DO


! ImpurityRadiation & Electron source

  SE_EXP  = 0.0_R8

  DO IRHO = 1,NRHO
     
     DO ISIMP = 2,SIMP+1
             
        
	LIN_RAD1  (IRHO,ISIMP)    = NE(IRHO)*NZ1(IRHO,ISIMP)*SLIN(IRHO,ISIMP)
        BREM_RAD1 (IRHO,ISIMP)    = NE(IRHO)*NZ1(IRHO,ISIMP)*GAMA(IRHO,ISIMP) 
        JON_EN1   (IRHO,ISIMP)    = NE(IRHO)*NZ1(IRHO,ISIMP)*POTENTIAL(IRHO,ISIMP)*ALFA(IRHO,ISIMP)
	REC_LOS1  (IRHO,ISIMP)    = NE(IRHO)*NZ1(IRHO,ISIMP)*POTENTIAL(IRHO,ISIMP-1)*BETA(IRHO,ISIMP)
        IMP_RADIATION(IRHO,ISIMP) = LIN_RAD1(IRHO,ISIMP) + BREM_RAD1(IRHO,ISIMP) + JON_EN1(IRHO,ISIMP)*itm_ev &
	                            - REC_LOS1(IRHO,ISIMP)*itm_ev
	                           
     ENDDO



!     SE_EXP(IRHO)                 =  NZ1(IRHO,1)*NE(IRHO)*ALFA(IRHO,1) !Included in NEUTRAL routine
     DO ISIMP = 2,SIMP
         SE_EXP(IRHO)             =  SE_EXP(IRHO)  &
                                     +  NZ1(IRHO,ISIMP)*NE(IRHO)*ALFA(IRHO,ISIMP) - NZ1(IRHO,ISIMP)*NE(IRHO)*BETA(IRHO,ISIMP)  
     ENDDO
     SE_EXP(IRHO)                 =   SE_EXP(IRHO) - NZ1(IRHO,SIMP+1)*NE(IRHO)*BETA(IRHO,SIMP+1)   

     
  ENDDO

     

! +++ Deallocate types for interface with numerical solver:
  CALL  DEALLOCATE_NUMERICS (SOLVER, ifail)


  RETURN 



END SUBROUTINE IMPURITY_ONE
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  







! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!-------------------------------------------------------!
!                                                       !
!______________  MATHEMATICAL SUBROUTINES: _____________!
!                                                       !
!-------------------------------------------------------!
! These subroutines have been extracted from RITM code, !
! and consist of derivation and integration routines    !
!-------------------------------------------------------!





 





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


SUBROUTINE UNHIDE_IIMP(NRHO, SIMP, ISIMP, As, Bs, Cs, ds, Vbnd, Ubnd, Wbnd, NZPs)

! Wyznaczenie Ni(NP) - Solotion of Set of Algrbraic Equation by Progonka:
!                            -As*Ni2(-1) + Bs*Ni2(0) - Cs*Ni2(-1) = dp
! Boundary Condition
!                                Ni2(RHO=0,t)  = -b1/a2*n(RHO=HRHO(),t)+c1/a1
!                                Ni2(L-HRHO(),t) = -aN/bN*n(L,t)+cN/bN

  INTEGER, PARAMETER :: DP = KIND(1.0D0)! Double precision  

  INTEGER NRHO, SIMP, ISIMP
  
  REAL (DP), DIMENSION(NRHO, SIMP+2) :: NZPs             ! Auxiliary Impurity density ( Position, IonizationState)
  
  REAL (DP), DIMENSION(NRHO) :: As, Bs, Cs, ds
  
  REAL (DP), DIMENSION(2) :: Vbnd, Ubnd, Wbnd
  
  REAL (DP), DIMENSION(NRHO) :: alfas, betas
  
  INTEGER IRHO
  
  
  
  REAL (DP) a1_bound, b1_bound, c1_bound
  
  REAL (DP) aN_bound, bN_bound, cN_bound
  
  
  a1_bound = Vbnd(1)
  b1_bound = Ubnd(1)
  c1_bound = Wbnd(1)
  
  aN_bound = Vbnd(2)
  bN_bound = Ubnd(2)
  cN_bound = Wbnd(2)
  
  
  alfas(1) = -b1_bound/a1_bound
  betas(1) =  c1_bound/a1_bound
  
  
  
  
  
  DO IRHO=2,NRHO-1	
     
     alfas(IRHO) = As(IRHO)/( Bs(IRHO) - Cs(IRHO)*alfas(IRHO-1) )
     
     betas(IRHO) = ( Cs(IRHO)*betas(IRHO-1) + ds(IRHO) )/( Bs(IRHO) - Cs(IRHO)*alfas(IRHO-1) )
     
  END DO
  
  
  
  NZPs(NRHO,ISIMP) = ( cN_bound - betas(NRHO-1)*bN_bound )/( aN_bound - alfas(NRHO-1)*bN_bound  )
  
  
  DO IRHO=NRHO-1, 2, -1
     
     NZPs(IRHO,ISIMP) = alfas(IRHO)*NZPs(IRHO+1,ISIMP) + betas(IRHO)
     
  END DO
  
  
  
  NZPs(1,ISIMP)=alfas(1)*NZPs(2,ISIMP) + betas(1)
  
  
END SUBROUTINE UNHIDE_IIMP

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





SUBROUTINE UNHIDE_NRHO(NRHO, SIMP, IRHO, As, Bs, Cs, ds, Nbnd, NZPs)

! Wyznaczenie Ni(NP) - Solotion of Set of Algrbraic Equation by Progonka:
!                            -As*Ni2(-1) + Bs*Ni2(0) - Cs*Ni2(-1) = dp
! Boundary Condition
!                                Ni2(RHO=0,t)  = -b1/b2*n(RHO=HRHO(),t)+c1/a1
!                                Ni2(L-HRHO(),t) = -aN/bN*n(L,t)+cN/bN


  INTEGER, PARAMETER :: DP = KIND(1.0D0)! Double precision  

  INTEGER NRHO, SIMP, IRHO
  
  REAL (DP) :: Nbnd
  
  REAL (DP), DIMENSION(NRHO,SIMP+2) :: NZPs             ! Auxiliary Impurity density ( Position, IonizationState)
  
  REAL (DP), DIMENSION(SIMP+2) :: As, Bs, Cs, ds
  
  REAL (DP), DIMENSION(SIMP+2) :: alfas, betas
  
  INTEGER ISIMP
  
  
  REAL a1_bound, b1_bound, c1_bound
  
  REAL aN_bound, bN_bound, cN_bound
  
  a1_bound = 1.0
  b1_bound = 0.0
  c1_bound = Nbnd
  
  
  
  aN_bound = 1.0
  bN_bound = 0.0
  cN_bound = 0.0
  
  
  alfas(1) = -b1_bound/a1_bound
  betas(1) =  c1_bound/a1_bound
  
  
  
  
  
  DO ISIMP=2,SIMP+1	
     
     alfas(ISIMP) = As(ISIMP)/( Bs(ISIMP) - Cs(ISIMP)*alfas(ISIMP-1) )
     
     betas(ISIMP) = ( Cs(ISIMP)*betas(ISIMP-1) + ds(ISIMP) )/( Bs(ISIMP) - Cs(ISIMP)*alfas(ISIMP-1) )
     
  END DO
  
  
  NZPs(IRHO,SIMP+2) = ( cN_bound - betas(SIMP+1)*bN_bound )/( aN_bound - alfas(SIMP+1)*bN_bound  )
  
  
  
  DO ISIMP=SIMP+1, 2, -1
     
     NZPs(IRHO,ISIMP) = alfas(ISIMP)*NZPs(IRHO,ISIMP+1) + betas(ISIMP)
     
  END DO
  
  
  NZPs(IRHO,1) = alfas(1)*NZPs(IRHO,2) + betas(1)
  
  
END SUBROUTINE UNHIDE_NRHO


REAL FUNCTION CalkaTrapez(wartosc, n, dx)
  
  INTEGER n ! liczba przedzialow
  REAL, DIMENSION(n+1) :: wartosc 
  REAL (8) :: dx
  INTEGER i
  
  CalkaTrapez = 0.0
  CalkaTrapez = 0.5*(wartosc(1)+wartosc(n+1))*dx
  DO i = 2 , n
     CalkaTrapez = CalkaTrapez + wartosc(i)*dx
  END DO
  
END FUNCTION CalkaTrapez
      
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
SUBROUTINE READ_DATA (ALFA_NZ,BETA_NZ,Gama_nz,SLIN_NZ,max_nzimp,simp,potential)
      
!     This subroutine reads atom data coeficients for 
!     impurity module

   

  INTEGER, PARAMETER   :: DP = KIND(1.0D0)                ! Double precision 
  INTEGER ISIMP, JT,SIMP,max_nzimp
  
  CHARACTER*70, DIMENSION(3,74) :: ATOMNAME
  
  REAL (DP) :: ALFA_nz(max_nzimp+2,500)
  REAL (DP) :: BETA_nz(max_nzimp+2,500)
  REAL (DP) :: GAMA_nz(max_nzimp+2,500)
  REAL (DP) :: SLIN_nz(max_nzimp+2,500)
  REAL (DP) :: POTENTIAL(MAX_NZIMP+2)
  
  ATOMNAME(1,1)='./src/atomdata/HTABLE'
  ATOMNAME(2,1)='./src/atomdata/HPROM_OLD'
  ATOMNAME(3,1)='./src/atomdata/HJON'			 

  ATOMNAME(1,2)='./src/atomdata/HETABLE1'
  ATOMNAME(2,2)='./src/atomdata/HEPROM1'
  ATOMNAME(3,2)='./src/atomdata/HEJON'
  
  ATOMNAME(1,3)='./src/atomdata/LITABLE1'
  ATOMNAME(2,3)='./src/atomdata/LIPROM1'
  ATOMNAME(3,3)='./src/atomdata/LIJON'             
  
  ATOMNAME(1,4)='./src/atomdata/BETABLE1'
  ATOMNAME(2,4)='./src/atomdata/BEPROM1'
  ATOMNAME(3,4)='./src/atomdata/BEJON'
  
  ATOMNAME(1,5)='./src/atomdata/BTABLE1'
  ATOMNAME(2,5)='./src/atomdata/BPROM1'
  ATOMNAME(3,5)='./src/atomdata/BJON'
  
  ATOMNAME(1,6)='./src/atomdata/CTABLE1'
  ATOMNAME(2,6)='./src/atomdata/CPROM1'
  ATOMNAME(3,6)='./src/atomdata/CJON'
  
  
  ATOMNAME(1,7)='./src/atomdata/NTABLE1'
  ATOMNAME(2,7)='./src/atomdata/NPROM1'
  ATOMNAME(3,7)='./src/atomdata/NJON'
  
  ATOMNAME(1,8)='./src/atomdata/OTABLE1'
  ATOMNAME(2,8)='./src/atomdata/OPROM1'
  ATOMNAME(3,8)='./src/atomdata/OJON'
  
  ATOMNAME(1,10)='./src/atomdata/NETABLE1'
  ATOMNAME(2,10)='./src/atomdata/NEPROM1'
  ATOMNAME(3,10)='./src/atomdata/NEJON'
  
  ATOMNAME(1,14)='./src/atomdata/SITABLE1'
  ATOMNAME(2,14)='./src/atomdata/SIPROM1'
  ATOMNAME(3,14)='./src/atomdata/SIJON'
  
  ATOMNAME(1,18)='./src/atomdata/ARTABLE1'
  ATOMNAME(2,18)='./src/atomdata/ARPROM1'
  ATOMNAME(3,18)='./src/atomdata/ARJON'
  
  ATOMNAME(1,26)='./src/atomdata/FETABLE1'
  ATOMNAME(2,26)='./src/atomdata/FEPROM1'
  ATOMNAME(3,26)='./src/atomdata/FEJON'
  
  ATOMNAME(1,28)='./src/atomdata/NITABLE1'
  ATOMNAME(2,28)='./src/atomdata/NIPROM1'
  
  ATOMNAME(1,42)='./src/atomdata/MOTABLE1'
  ATOMNAME(2,42)='./src/atomdata/MOPROM1'
  ATOMNAME(3,42)='./src/atomdata/MOJON'
  
  ATOMNAME(1,74)='./src/atomdata/WTABLE1'
  ATOMNAME(2,74)='./src/atomdata/WPROM1'
  ATOMNAME(3,74)='./src/atomdata/WJON'
  
  
  ALFA_NZ = 0.0; BETA_NZ = 0.0; GAMA_NZ = 0.0
  POTENTIAL = 0.0
  
  
  OPEN(23,FILE=ATOMNAME(1,SIMP),STATUS='OLD')
  
  
  
  DO ISIMP=1,SIMP;  READ(23,*) (ALFA_NZ(ISIMP,JT),BETA_NZ(ISIMP+1,JT),GAMA_NZ(ISIMP+1,JT), JT=1, 500);	  ENDDO
     
  CLOSE(23)

  IF (SIMP.EQ.1.)then 
  go to 40
  endif  
     
  OPEN(33,FILE=ATOMNAME(2,SIMP),STATUS='OLD')
  
  READ(33,*) ((SLIN_NZ(ISIMP,JT),JT=1, 500 ), ISIMP = 1,SIMP+1 )
  
  CLOSE(33)
     
40  OPEN(43,FILE=ATOMNAME(3,SIMP),STATUS='OLD')
  
  READ(43,*) (POTENTIAL(ISIMP), ISIMP = 1,SIMP )
  
  CLOSE(43)
     
     
END SUBROUTINE READ_DATA
   
   
