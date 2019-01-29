! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This subroutine solves transport equations using matrix PROGONKA method 
!>
!> \author R.Stankiewicz
!>
!> \version "$Id: solution3.f90 87 2009-01-28 20:37:31Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
SUBROUTINE SOLUTION4 (SOLVER,ifail)

!-------------------------------------------------------!
!     This subroutine solves transport equations        !
!     using matrix PROGONKA method                      !
!-------------------------------------------------------!
!     Source:       --                                  !
!     Developers:   R.Stankiewicz                       !
!     Contacts:     romsta@ifpilm.waw.pl                !
!                                                       !
!     Comments:     --                                  !
!                                                       !
!-------------------------------------------------------!


! +++ Declaration of variables: 
  use itm_types
  USE TYPE_SOLVER

  IMPLICIT NONE  

  TYPE(NUMERICS)    :: SOLVER

  INTEGER           :: IFAIL 
  INTEGER           :: NDIM,IDIM1,IDIM2,IUPWIND
  INTEGER           :: NRHO,IRHO1,IRHO2,IRHOP,IRHOM

  REAL (R8)         :: TAU,H12
  REAL (R8)         :: RHO(SOLVER%NRHO)

  REAL (R8)         :: RHO_DERIVN(SOLVER%NRHO) !AF 4.Oct.2011

  REAL (R8)         :: Y(SOLVER%NDIM,SOLVER%NRHO)
  REAL (R8)         :: YM(SOLVER%NDIM,SOLVER%NRHO)
  REAL (R8)         :: DY(SOLVER%NDIM,SOLVER%NRHO)
  REAL (R8)         :: A(SOLVER%NDIM,SOLVER%NRHO)
  REAL (R8)         :: B(SOLVER%NDIM,SOLVER%NRHO)
  REAL (R8)         :: C(SOLVER%NDIM,SOLVER%NRHO)
  REAL (R8)         :: D(SOLVER%NDIM,SOLVER%NRHO)
  REAL (R8)         :: E(SOLVER%NDIM,SOLVER%NRHO)
  REAL (R8)         :: F(SOLVER%NDIM,SOLVER%NRHO)
  REAL (R8)         :: G(SOLVER%NDIM,SOLVER%NRHO)
  REAL (R8)         :: CM1(SOLVER%NDIM,SOLVER%NDIM,SOLVER%NRHO)
  REAL (R8)         :: U(SOLVER%NDIM,2)
  REAL (R8)         :: V(SOLVER%NDIM,2)
  REAL (R8)         :: W(SOLVER%NDIM,2)

  REAL (R8)         :: AA(SOLVER%NRHO,SOLVER%NDIM,SOLVER%NDIM)
  REAL (R8)         :: BB(SOLVER%NRHO,SOLVER%NDIM,SOLVER%NDIM)
  REAL (R8)         :: CC(SOLVER%NRHO,SOLVER%NDIM,SOLVER%NDIM)
  REAL (R8)         :: FF(SOLVER%NRHO,SOLVER%NDIM)
  REAL (R8)         :: H(SOLVER%NRHO)
  REAL (R8)         :: UU(2,SOLVER%NDIM,SOLVER%NDIM)
  REAL (R8)         :: VV(2,SOLVER%NDIM,SOLVER%NDIM)
  REAL (R8)         :: WW(2,SOLVER%NDIM)

  REAL (R8)         :: YY(SOLVER%NRHO,SOLVER%NDIM)
  REAL (R8)         :: AMIX



! +++ Initial coefficients are set to zero:
  IUPWIND           = 0

  AA(:,:,:)         = 0.0_R8
  BB(:,:,:)         = 0.0_R8
  CC(:,:,:)         = 0.0_R8
  CM1(:,:,:)        = 0.0_R8
  FF(:,:)           = 0.0_R8

  UU(:,:,:)         = 0.0_R8
  VV(:,:,:)         = 0.0_R8
  WW(:,:)           = 0.0_R8

  YY(:,:)           = 0.0_R8

  NDIM              = SOLVER%NDIM
  NRHO              = SOLVER%NRHO
  AMIX              = SOLVER%AMIX


! +++ Input coefficients from the work flow:
  RHO_LOOP1: DO IRHO1 = 1,NRHO
     RHO(IRHO1)            = SOLVER%RHO(IRHO1)

     DIM_LOOP1: DO IDIM1 = 1,NDIM
        Y(IDIM1,IRHO1)            = SOLVER%Y(IDIM1,IRHO1)
        YM(IDIM1,IRHO1)           = SOLVER%YM(IDIM1,IRHO1)
        DY(IDIM1,IRHO1)           = SOLVER%DY(IDIM1,IRHO1)

        A(IDIM1,IRHO1)            = SOLVER%A(IDIM1,IRHO1)
        B(IDIM1,IRHO1)            = SOLVER%B(IDIM1,IRHO1)
        C(IDIM1,IRHO1)            = SOLVER%C(IDIM1,IRHO1)
        D(IDIM1,IRHO1)            = SOLVER%D(IDIM1,IRHO1)
        E(IDIM1,IRHO1)            = SOLVER%E(IDIM1,IRHO1)
        F(IDIM1,IRHO1)            = SOLVER%F(IDIM1,IRHO1)
        G(IDIM1,IRHO1)            = SOLVER%G(IDIM1,IRHO1)

        DIM_LOOP2: DO IDIM2 = 1,NDIM
           CM1(IDIM1,IDIM2,IRHO1)  = SOLVER%CM1(IDIM1,IDIM2,IRHO1) 
        END DO DIM_LOOP2

        U(IDIM1,1)                = SOLVER%U(IDIM1,1)
        V(IDIM1,1)                = SOLVER%V(IDIM1,1)
        W(IDIM1,1)                = SOLVER%W(IDIM1,1)
        U(IDIM1,2)                = SOLVER%U(IDIM1,2)
        V(IDIM1,2)                = SOLVER%V(IDIM1,2)
        W(IDIM1,2)                = SOLVER%W(IDIM1,2)

     END DO DIM_LOOP1

  END DO RHO_LOOP1
  TAU                           = SOLVER%H


! +++ Set up coefficients for matrix solver:
  DO  IDIM2 = 1,NDIM
     DO  IDIM1 = 1,2
        WW(IDIM1,IDIM2)           = W(IDIM2,IDIM1)
     ENDDO
  ENDDO


  DO IRHO1 = 2,NRHO
     IRHOM                        = IRHO1-1
     H(IRHOM)                     = RHO(IRHO1)-RHO(IRHOM)
  ENDDO
  DO IRHO1=1,NRHO
   DO IDIM1=1,NDIM
   DO IDIM2=1,NDIM
   F(IDIM2,IRHO1)=F(IDIM2,IRHO1)!-CM1(IDIM1,IDIM2,IRHO1)*YM(IDIM1,IRHO1)
   enddo
   enddo
  ENDDO
  DO IDIM1 = 1,NDIM
     UU(1,IDIM1,IDIM1)            = - 0.5*V(IDIM1,1)/H(1)+0.5*U(IDIM1,1)
     VV(1,IDIM1,IDIM1)            =  0.5*V(IDIM1,1)/H(1)+0.5*U(IDIM1,1)
     UU(2,IDIM1,IDIM1)            =  0.5* V(IDIM1,2)/H(NRHO-1)+0.5*U(IDIM1,2)
     VV(2,IDIM1,IDIM1)            = - 0.5*V(IDIM1,2)/H(NRHO-1)+0.5*U(IDIM1,2)      
  ENDDO
  IRHO1=2
  IRHOP                        = IRHO1+1
  IRHOM                        = IRHO1-1
  H12                          = 0.5*(H(IRHO1)+2.*H(IRHOM))

     DOLOOP2IDIM1 : DO IDIM1 = 1,NDIM
        FF(IRHO1,IDIM1)            = B(IDIM1,IRHO1)*YM(IDIM1,IRHO1)+F(IDIM1,IRHO1)*TAU
        AA(IRHO1,IDIM1,IDIM1)      = -0.5/C(IDIM1,IRHO1)/H(IRHO1)/ H12               &
             *  (D(IDIM1,IRHOP)+D(IDIM1,IRHO1))
        CC(IRHO1,IDIM1,IDIM1)      = -0.5/C(IDIM1,IRHO1)/H(IRHOM)/ H12               &
             * D(IDIM1,IRHOM) 
        BB(IRHO1,IDIM1,IDIM1)      = -AA(IRHO1,IDIM1,IDIM1) -CC(IRHO1,IDIM1,IDIM1)

        IF (IUPWIND.NE.1) THEN
           AA(IRHO1,IDIM1,IDIM1)    = AA(IRHO1,IDIM1,IDIM1)                           &
                + 0.25 /C(IDIM1,IRHO1)/H12                      &
                * (E(IDIM1,IRHOP)+E(IDIM1,IRHO1))
           CC(IRHO1,IDIM1,IDIM1)    = CC(IRHO1,IDIM1,IDIM1)                           &
                - 0.5 /C(IDIM1,IRHO1)/H12                      &
                * E(IDIM1,IRHOM)
           BB(IRHO1,IDIM1,IDIM1)    = BB(IRHO1,IDIM1,IDIM1)                           &    
                + 0.25 /C(IDIM1,IRHO1)/H12                      &
!                * (E(IDIM1,IRHO1)+E(IDIM1,IRHOM)-2.*E(IDIM1,IRHOM)) !AF 2.Dec.2011 - Roman found this bug that affects the axis
                * (E(IDIM1,IRHO1)+E(IDIM1,IRHOP)-2.*E(IDIM1,IRHOM)) !AF 2.Dec.2011 - Roman found this bug that affects the axis
        ELSE
           IF (E(IDIM1,IRHO1).GE.0.) THEN
              CC(IRHO1,IDIM1,IDIM1)  = CC(IRHO1,IDIM1,IDIM1)                           &
                   - E(IDIM1,IRHOM)/C(IDIM1,IRHO1)/H12
              BB(IRHO1,IDIM1,IDIM1)  = BB(IRHO1,IDIM1,IDIM1)                           &
                   + E(IDIM1,IRHO1) / C(IDIM1,IRHO1)/H12
           ELSE
              AA(IRHO1,IDIM1,IDIM1)  = AA(IRHO1,IDIM1,IDIM1)                           &
                   + E(IDIM1,IRHOP)/C(IDIM1,IRHOP)/H12
              BB(IRHO1,IDIM1,IDIM1)  = BB(IRHO1,IDIM1,IDIM1)                           &
                   + E(IDIM1,IRHO1) / C(IDIM1,IRHO1)/H12
           ENDIF

        ENDIF

        BB(IRHO1,IDIM1,IDIM1)      = BB(IRHO1,IDIM1,IDIM1)+G(IDIM1,IRHO1)
        
        DO IDIM2 = 1,NDIM
           BB(IRHO1,IDIM1,IDIM2)    = BB(IRHO1,IDIM1,IDIM2)!- CM1(IDIM1,IDIM2,IRHO1)
        ENDDO


     ENDDO DOLOOP2IDIM1
  IRHO1=NRHO-1

  
     IRHOP                        = IRHO1+1
     IRHOM                        = IRHO1-1
     H12                          = 0.5*(2.*H(IRHO1)+H(IRHOM))

     DOLOOP2IDIM2 : DO IDIM1 = 1,NDIM
        FF(IRHO1,IDIM1)            = B(IDIM1,IRHO1)*YM(IDIM1,IRHO1)+F(IDIM1,IRHO1)*TAU
        AA(IRHO1,IDIM1,IDIM1)      = -0.5/C(IDIM1,IRHO1)/H(IRHO1)/ H12               &
             * D(IDIM1,IRHOP)
        CC(IRHO1,IDIM1,IDIM1)      = -0.5/C(IDIM1,IRHO1)/H(IRHOM)/ H12               &
             * (D(IDIM1,IRHO1)+ D(IDIM1,IRHOM)) 
        BB(IRHO1,IDIM1,IDIM1)      = -AA(IRHO1,IDIM1,IDIM1) -CC(IRHO1,IDIM1,IDIM1)

        IF (IUPWIND.NE.1) THEN
           AA(IRHO1,IDIM1,IDIM1)    = AA(IRHO1,IDIM1,IDIM1)                           &
                + 0.5 /C(IDIM1,IRHO1)/H12                      &
                * E(IDIM1,IRHOP)
           CC(IRHO1,IDIM1,IDIM1)    = CC(IRHO1,IDIM1,IDIM1)                           &
                - 0.25 /C(IDIM1,IRHO1)/H12                      &
                * (E(IDIM1,IRHO1)+E(IDIM1,IRHOM))
           BB(IRHO1,IDIM1,IDIM1)    = BB(IRHO1,IDIM1,IDIM1)                           &    
                + 0.25 /C(IDIM1,IRHO1)/H12                      &
                * (2.*E(IDIM1,IRHOP)-E(IDIM1,IRHOM)-E(IDIM1,IRHO1))
        ELSE
           IF (E(IDIM1,IRHO1).GE.0.) THEN
              CC(IRHO1,IDIM1,IDIM1)  = CC(IRHO1,IDIM1,IDIM1)                           &
                   - E(IDIM1,IRHOM)/C(IDIM1,IRHO1)/H12
              BB(IRHO1,IDIM1,IDIM1)  = BB(IRHO1,IDIM1,IDIM1)                           &
                   + E(IDIM1,IRHO1) / C(IDIM1,IRHO1)/H12
           ELSE
              AA(IRHO1,IDIM1,IDIM1)  = AA(IRHO1,IDIM1,IDIM1)                           &
                   + E(IDIM1,IRHOP)/C(IDIM1,IRHOP)/H12
              BB(IRHO1,IDIM1,IDIM1)  = BB(IRHO1,IDIM1,IDIM1)                           &
                   + E(IDIM1,IRHO1) / C(IDIM1,IRHO1)/H12
           ENDIF

        ENDIF

        BB(IRHO1,IDIM1,IDIM1)      = BB(IRHO1,IDIM1,IDIM1)+G(IDIM1,IRHO1)
        
        DO IDIM2 = 1,NDIM
           BB(IRHO1,IDIM1,IDIM2)    = BB(IRHO1,IDIM1,IDIM2)!- CM1(IDIM1,IDIM2,IRHO1)
        ENDDO


     ENDDO DOLOOP2IDIM2


  DOLOOP1IRHO1: DO IRHO1 = 3,NRHO-2
     IRHOP                        = IRHO1+1
     IRHOM                        = IRHO1-1
     H12                          = 0.5*(H(IRHO1)+H(IRHOM))

     DOLOOP2IDIM3 : DO IDIM1 = 1,NDIM
        FF(IRHO1,IDIM1)            = B(IDIM1,IRHO1)*YM(IDIM1,IRHO1)+F(IDIM1,IRHO1)*TAU
        AA(IRHO1,IDIM1,IDIM1)      = -0.5/C(IDIM1,IRHO1)/H(IRHO1)/ H12               &
             * (D(IDIM1,IRHO1)+ D(IDIM1,IRHOP))
        CC(IRHO1,IDIM1,IDIM1)      = -0.5/C(IDIM1,IRHO1)/H(IRHOM)/ H12               &
             * (D(IDIM1,IRHO1)+ D(IDIM1,IRHOM)) 
        BB(IRHO1,IDIM1,IDIM1)      = -AA(IRHO1,IDIM1,IDIM1) -CC(IRHO1,IDIM1,IDIM1)

        IF (IUPWIND.NE.1) THEN
           AA(IRHO1,IDIM1,IDIM1)    = AA(IRHO1,IDIM1,IDIM1)                           &
                + 0.25 /C(IDIM1,IRHO1)/H12                      &
                * (E(IDIM1,IRHO1)+E(IDIM1,IRHOP))
           CC(IRHO1,IDIM1,IDIM1)    = CC(IRHO1,IDIM1,IDIM1)                           &
                - 0.25 /C(IDIM1,IRHO1)/H12                      &
                * (E(IDIM1,IRHO1)+E(IDIM1,IRHOM))
           BB(IRHO1,IDIM1,IDIM1)    = BB(IRHO1,IDIM1,IDIM1)                           &    
                + 0.25 /C(IDIM1,IRHO1)/H12                      &
                * (E(IDIM1,IRHOP)-E(IDIM1,IRHOM))
        ELSE
           IF (E(IDIM1,IRHO1).GE.0.) THEN
              CC(IRHO1,IDIM1,IDIM1)  = CC(IRHO1,IDIM1,IDIM1)                           &
                   - E(IDIM1,IRHOM)/C(IDIM1,IRHO1)/H12
              BB(IRHO1,IDIM1,IDIM1)  = BB(IRHO1,IDIM1,IDIM1)                           &
                   + E(IDIM1,IRHO1) / C(IDIM1,IRHO1)/H12
           ELSE
              AA(IRHO1,IDIM1,IDIM1)  = AA(IRHO1,IDIM1,IDIM1)                           &
                   + E(IDIM1,IRHOP)/C(IDIM1,IRHOP)/H12
              BB(IRHO1,IDIM1,IDIM1)  = BB(IRHO1,IDIM1,IDIM1)                           &
                   + E(IDIM1,IRHO1) / C(IDIM1,IRHO1)/H12
           ENDIF

        ENDIF

        BB(IRHO1,IDIM1,IDIM1)      = BB(IRHO1,IDIM1,IDIM1)+G(IDIM1,IRHO1)

        DO IDIM2 = 1,NDIM
           BB(IRHO1,IDIM1,IDIM2)    = BB(IRHO1,IDIM1,IDIM2)!- CM1(IDIM1,IDIM2,IRHO1)
        ENDDO


     ENDDO DOLOOP2IDIM3


  ENDDO DOLOOP1IRHO1

  


!------------------------------COUPLING TERMS ----------------------------
!   DO IRHO1 = 2,NRHO-1
!   IRHOP = IRHO1+1
!   IRHOM = IRHO1-1
!   DO IDIM1 = 1,NDIM
!   H12 = 0.5*(H(IRHO1)+H(IRHOM))   
!   if(IUPWIND.NE.1) THEN
!   AA(IRHO1,NDIM,IDIM1) = AA(IRHO1,NDIM,IDIM1)+  &
!   0.25 *S2(IRHO1)/H12*(S3(IDIM1,IRHO1)+S3(IDIM1,IRHOP))
!   CC(IRHO1,NDIM,IDIM1) = CC(IRHO1,NDIM,IDIM1)-   &
!   -0.25 *S2(IRHO1)/H12*(S3(IDIM1,IRHO1)+S3(IDIM1,IRHOM))
!  BB(IRHO1,NDIM,IDIM1) = BB(IRHO1,NDIM,IDIM1) +&    
!  0.25 /S2(IRHO1)/H12*(S3(IDIM1,IRHOP)-S3(IDIM1,IRHOM))        
!  ELSE
!     IF(E(IDIM1,IRHO1).GE.0.) THEN
!     CC(IRHO1,NDIM,IDIM1) = CC(IRHO1,NDIM,IDIM1) -   &
!     S2(IRHOM)*S3(IDIM1,IRHOM)/H12
!     BB(IRHO1,NDIM,IDIM1) = BB(IRHO1,NDIM,IDIM1)+  &
!     S2(IRHO1) * S3(IDIM1,IRHO1)/H12
!     ELSE
!     AA(IRHO1,NDIM,IDIM1) = AA(IRHO1,NDIM,IDIM1)+   &
!     S2(IRHOP)*S3(IDIM1,IRHOP)/H12
!     BB(IRHO1,NDIM,IDIM1) = BB(IRHO1,NDIM,IDIM1)+  &
!     S2(IRHO1) * S3(IDIM1,IRHO1)/H12
!     ENDIF
!   ENDIF   



  BB                         = BB*TAU
  AA                         = AA*TAU
  CC                         = CC*TAU


  DO IRHO1 = 1,NRHO
     DO IDIM1 = 1,NDIM
        BB(IRHO1,IDIM1,IDIM1)  = BB(IRHO1,IDIM1,IDIM1)+ A(IDIM1,IRHO1) 
     ENDDO
  ENDDO



! +++ Call the matrix progonka:
  CALL MPROGONKA_4(NRHO,NDIM,AA,BB,CC,FF,UU,VV,WW,YY)


! +++ New solution: 
  DO IDIM1 = 1,NDIM
     DO IRHO1 = 1,NRHO
        Y(IDIM1,IRHO1)         =  YY(IRHO1,IDIM1)*AMIX+Y(IDIM1,IRHO1)*(1.e0_R8-AMIX)
     ENDDO
  ENDDO

  !AF 4.Oct.2011 - at this stage IRHO.eq.1 and IRHO.eq.NRHO are still the ghost points, so DERIVN needs the corresponding (regular) RHO grid,
  !otherwise DY at points 2 and N-1 will be wrong
  RHO_DERIVN = RHO
  RHO_DERIVN(1) = RHO(2) - ( RHO(3) - RHO(2) )
  RHO_DERIVN(NRHO) = RHO(NRHO-1) + ( RHO(NRHO-1) - RHO(NRHO-2) )
  !AF - End

!  CALL DERIVN3_4(NRHO,NDIM,RHO,Y,DY) !AF 4.Oct.2011
  CALL DERIVN3_4(NRHO,NDIM,RHO_DERIVN,Y,DY) !AF 4.Oct.2011
  DO IDIM1=1,NDIM
!  DY(IDIM1,1)=0.5*(Y(IDIM1,2)-Y(IDIM1,1))/H(1) !AF 6.Oct.2011
  DY(IDIM1,1)=0.5*(DY(IDIM1,2)+DY(IDIM1,1)) !AF 6.Oct.2011 - this uses DY from DERIVN3_4 - seems to make no difference
  Y(IDIM1,1)=0.5*(Y(IDIM1,2)+Y(IDIM1,1))
  !DY(IDIM1,NRHO)=0.5*(Y(IDIM1,NRHO-1)-Y(IDIM1,NRHO))/H(NRHO-1) !AF 23.Sep.2011
!  DY(IDIM1,NRHO)=0.5*(Y(IDIM1,NRHO)-Y(IDIM1,NRHO-1))/H(NRHO-1) !AF 23.Sep.2011, 6.Oct.2011
  DY(IDIM1,NRHO)=0.5*(DY(IDIM1,NRHO)+DY(IDIM1,NRHO-1)) !AF 23.Sep.2011, 6.Oct.2011 - this uses DY from DERIVN3_4 - seems to make no difference
  Y(IDIM1,NRHO)=0.5*(Y(IDIM1,NRHO)+Y(IDIM1,NRHO-1))
  ENDDO
! +++ Return solution to the work flow:
  DO IDIM1 = 1,NDIM
     DO IRHO1 = 1,NRHO
        SOLVER%Y(IDIM1,IRHO1)  = Y(IDIM1,IRHO1)
        SOLVER%DY(IDIM1,IRHO1) = DY(IDIM1,IRHO1)
     ENDDO
  ENDDO


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++ Return solution to ETS:      


  RETURN



END SUBROUTINE SOLUTION4 !AF, 14.Sep.2011
!END SUBROUTINE SOLUTION3 !AF, 14.Sep.2011
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!>     Finds  Solution of discrete equation of the form
!>     A_i * Y_i+1 + B_i * Y_i +C_i * Y_i-1 = F_i
!>     where:
!>            Y_i - unknown wektor at point i  (Y_i(NDIM))
!>            F_i - Rhs at  point i  (F_i(NDIM)) 
!>            A_i,B_i,C_i - equation coefficients (matrixes (NDIM,NDIM)) 
!>            U,V,W - matrixes with boundary conditions          
!>
!> \author R.Stankiewicz
!>
!> \version "$Id: solution3.f90 87 2009-01-28 20:37:31Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +                 
SUBROUTINE MPROGONKA_4 (NP,NDIM,A,B,C,F,U,V,W,Y)  !AF, 14.Sep.2011
!SUBROUTINE MPROGONKA (NP,NDIM,A,B,C,F,U,V,W,Y) !AF, 14.Sep.2011
!     Finds  Solution of discrete equation of the form
!     A_i * Y_i+1 + B_i * Y_i +C_i * Y_i-1 = F_i
!     where:
!            Y_i - unknown wektor at point i  (Y_i(NDIM))
!            F_i - Rhs at  point i  (F_i(NDIM)) 
!            A_i,B_i,C_i - equation coefficients (matrixes (NDIM,NDIM)) 
!            U,V,W - matrixes with boundary conditions          


  use itm_types
  IMPLICIT NONE      
!! Number of points 
  INTEGER, INTENT(IN) :: NP
!! Rank of the block matrix
  INTEGER, INTENT(IN) :: NDIM    
!! Equation coefficients: A, B, C 
  REAL(R8), DIMENSION(NP,NDIM,NDIM), INTENT(IN) :: A, B, C    
!! Right hand side: F  
  REAL(R8), DIMENSION(NP,NDIM), INTENT(IN) :: F    
!! Boundary conditions arrays U,V
  REAL(R8), DIMENSION(2,NDIM,NDIM), INTENT(IN) :: U,V   
!! Boundary condition array W
  REAL(R8), DIMENSION(2,NDIM), INTENT(IN) :: W         
!! Solution: Y  
  REAL(R8), DIMENSION(NP,NDIM), INTENT(OUT) :: Y 

!   working variables    
!! Progonka coefficients: alpha, beta 
  REAL(R8), DIMENSION(NP,NDIM,NDIM) ::  alf 
  REAL(R8), DIMENSION(NP,NDIM) ::       bet      
  REAL(R8), DIMENSION(NDIM,NDIM) ::  temp1,temp2,temp3            
  REAL(R8), DIMENSION(NP,NDIM) ::       wekt1   
  REAL(R8), DIMENSION(NDIM) ::       wekt2            
  INTEGER :: i, j, j2, k, n, nn, n2
  REAL(R8) :: stemp1,stemp2,det




  IF(NDIM .EQ. 1) THEN
!     BOUNDARY CONDITION      
     ALF(2,1,1) =-V(1,1,1)/U(1,1,1)
     BET(2,1)= W(1,1)/U(1,1,1)

     DO  J=2,NP-1
        J2=J+1
        DET=B(J,1,1)+ALF(J,1,1)*C(J,1,1)
        IF(ABS(DET).GT.1.e-30_R8) THEN
           ALF(J2,1,1)=-A(J,1,1)/DET
           BET(J2,1)=(F(J,1)-BET(J,1)*C(J,1,1))/DET
        ELSE
           ALF(J2,1,1)=0.
           BET(J2,1)=0.
        ENDIF
     ENDDO

!     BOUNDARY CONDITION
     Y(NP,1)=0.
     DET=V(2,1,1)*ALF(NP,1,1)+U(2,1,1)
     IF(ABS(DET).GT.1.e-30_R8) Y(NP,1)=(W(2,1)-BET(NP,1)*V(2,1,1))/DET

     DO  J=NP-1,1,-1
        J2=J+1
        Y(J,1)=ALF(J2,1,1)*Y(J2,1)+BET(J2,1)
     ENDDO


  ELSE
!     BOUNDARY CONDITION 
     DO  I=1,NDIM
        DO  J=1,NDIM
           TEMP1(I,J)=U(1,I,J)
        ENDDO
     ENDDO
     CALL INVMATRIX_4(NDIM, TEMP1, TEMP2)      

     DO  I=1,NDIM  
        BET(2,I)=0.0             
        DO  J=1,NDIM            
           ALF(2,I,J)=0.0       
           DO  K=1,NDIM
              ALF(2,I,J)=ALF(2,I,J)-TEMP2(I,K)*V(1,K,J)
           ENDDO
           BET(2,I)=BET(2,I)+TEMP2(I,J)*W(1,J)       
        ENDDO
     ENDDO

     DO  N=2,NP-1
        DO  I=1,NDIM
           WEKT1(N,I)=F(N,I)
           DO  J=1,NDIM 
              TEMP1(I,J)=B(N,I,J)    
              DO  K=1,NDIM               
                 TEMP1(I,J)=TEMP1(I,J)+C(N,I,K)*ALF(N,K,J) 
              ENDDO
              WEKT1(N,I)=WEKT1(N,I)-C(N,I,J)*BET(N,J)        
           ENDDO
        ENDDO
        CALL INVMATRIX_4(NDIM, TEMP1, TEMP2)         

        N2=N+1
        DO  I=1,NDIM
           BET(N2,I)=0.
           DO  J=1,NDIM    
              ALF(N2,I,J)=0.
              DO  K=1,NDIM
                 ALF(N2,I,J)=ALF(N2,I,J)-TEMP2(I,K)*A(N,K,J)
              ENDDO
              BET(N2,I)=BET(N2,I)+TEMP2(I,J)*WEKT1(N,J) 
           ENDDO
        ENDDO
     ENDDO


1001 FORMAT(1X,3I4,1P,8E12.4)        


!     BOUNDARY CONDITION     
     DO  I=1,NDIM
        WEKT2(I)=W(2,I)
        DO  J=1,NDIM 
           TEMP1(I,J)=U(2,I,J)    
           DO  K=1,NDIM               
              TEMP1(I,J)=TEMP1(I,J)+V(2,I,K)*ALF(NP,K,J)
           ENDDO
           WEKT2(I)=WEKT2(I)-V(2,I,J)*BET(NP,J)        
        ENDDO
     ENDDO
     CALL INVMATRIX_4(NDIM, TEMP1, TEMP2)  

     DO  I=1,NDIM
        Y(NP,I)=0. 
        DO  J=1,NDIM  
           Y(NP,I)=Y(NP,I)+TEMP2(I,J)*WEKT2(J)                    
        ENDDO
     ENDDO

     DO  N=NP-1,1,-1 
        N2=N+1
        DO  I=1,NDIM
           Y(N,I)=BET(N2,I)
           DO J=1,NDIM
              Y(N,I)=Y(N,I)+ALF(N2,I,J)*Y(N2,J)
           ENDDO
        ENDDO
     ENDDO


  ENDIF


  RETURN

END SUBROUTINE MPROGONKA_4 !AF, 14.Sep.2011
!END SUBROUTINE MPROGONKA !AF, 14.Sep.2011

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Computes inverse of matrix 
!> Input
!>    AA   -    Matrix A (n by n)
!>    N    -    Dimension of matrix AA 
! Outputs
!>   Ainv  -    Inverse of matrix AA (n by n)    
!>
!> \author R.Stankiewicz
!>
!> \version "$Id: solution3.f90 87 2009-01-28 20:37:31Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

SUBROUTINE INVMATRIX_4(n, AA, Ainv) !AF, 14.Sep.2011
!SUBROUTINE INVMATRIX(n, AA, Ainv) !AF, 14.Sep.2011

! Computes inverse of matrix 
! Input
!-------------------------------------------------
!    AA   -    Matrix A (n by n)
!    N    -    Dimension of matrix AA 
!-------------------------------------------------
! Outputs
!   Ainv  -    Inverse of matrix AA (n by n)    
!-------------------------------------------------     

  use itm_types

  IMPLICIT NONE      

!! order of matrix
  INTEGER, INTENT(IN) :: n
!! Input Matrix
  REAL(R8), DIMENSION(n,n), INTENT(IN) :: AA

!! Output Inverted Matrix
  REAL(R8), DIMENSION(n,n), INTENT(OUT) :: Ainv    


  REAL(R8), DIMENSION(n) ::  scale      ! Scale factor  
  REAL(R8), DIMENSION(n,n) ::  b        ! Work array    
  REAL(R8), DIMENSION(n,n) ::  A        ! Working copy of input matrix
  INTEGER, DIMENSION(n) ::  index              ! Index Vector      
  INTEGER :: i, j, k, jPivot, indexJ
  REAL(R8) :: scalemax, ratio, ratiomax, coeff, determ, sum


! Copy matrix A so as not to modify original
  do i=1,N
     do j=1,N
        A(i,j) = AA(i,j)
     enddo
  enddo

!* Matrix b is initialized to the identity matrix
  do i=1,N
     do j=1,N
        if( i .eq. j ) then
           b(i,j) = 1.0
        else
           b(i,j) = 0.0
        endif
     enddo
  enddo

!* Set scale factor, scale(i) = max( |a(i,j)| ), for each row
  do i=1,N
     index(i) = i              ! Initialize row index list
     scalemax = 0.0
     do j=1,N
        if( abs(A(i,j)) .gt. scalemax ) then
           scalemax = abs(A(i,j))
        endif
     enddo
     scale(i) = scalemax
  enddo

!* Loop over rows k = 1, ..., (N-1) 
  do k=1,N-1
!* Select pivot row from max( |a(j,k)/s(j)| )
     ratiomax = 0.0
     jPivot = k
     do i=k,N
        ratio = abs(A(index(i),k))/scale(index(i))
        if( ratio .gt. ratiomax ) then
           jPivot=i
           ratiomax = ratio
        endif
     enddo
!* Perform pivoting using row index list
     indexJ = index(k)
     if( jPivot .ne. k ) then           ! Pivot
        indexJ = index(jPivot)
        index(jPivot) = index(k)    ! Swap index jPivot and k
        index(k) = indexJ  
     endif
!* Perform forward elimination
     do i=k+1,N
        coeff = A(index(i),k)/A(indexJ,k)
        do j=k+1,N
           A(index(i),j) = A(index(i),j) - coeff*A(indexJ,j)
        enddo
        A(index(i),k) = coeff
        do j=1,N
           b(index(i),j) = b(index(i),j) - A(index(i),k)*b(indexJ,j)
        enddo
     enddo
  enddo


!* Perform backsubstitution
  do k=1,N
     Ainv(N,k) = b(index(N),k)/A(index(N),N)
     do i=N-1,1,-1
        sum = b(index(i),k)
        do j=i+1,N
           sum = sum - A(index(i),j)*Ainv(j,k)
        enddo
        Ainv(i,k) = sum/A(index(i),i)
     enddo
  enddo

  return

END SUBROUTINE INVMATRIX_4 !AF, 14.Sep.2011
!END SUBROUTINE INVMATRIX !AF, 14.Sep.2011
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> These subroutines calculate first and second derivatives,
!> DY1 and DY2, of function Y respect to argument X
!>
!> \author R.Stankiewicz
!>
!> \version "$Id: solution3.f90 87 2009-01-28 20:37:31Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
SUBROUTINE DERIVN3_4(N,NFUN,X,FUN,DFUN) !AF, 14.Sep.2011
!SUBROUTINE DERIVN3(N,NFUN,X,FUN,DFUN) !AF, 14.Sep.2011

  use itm_types

  IMPLICIT NONE

  INTEGER :: N                                          ! number of radial points (input)
  INTEGER :: NFUN                                       ! number of functions
  INTEGER :: I, IFUN

  REAL (R8) :: X(N),           &                        ! argument array (input)
       FUN(NFUN,N),    &                        ! function array (input)
       DFUN(NFUN,N),   &                        ! function derivative array (output)
       Y(N),           &                        ! function array 1-D (local)
       DY1(N)                                   ! function derivative array 1-D (local)
  REAL (R8) :: H(N),DY2(N)

  REAL (R8) :: DDY !AF 6.Oct.2011



  DO I=1,N-1
     H(I)          = X(I+1)-X(I)
  END DO

  DO IFUN = 1,NFUN

     DO I = 1,N
        Y(I)         = FUN(IFUN,I)
     END DO

     DO I=2,N-1
        DY1(I)       = ((Y(I+1)-Y(I))*H(I-1)/H(I)+(Y(I)-Y(I-1))*H(I)/H(I-1)) &
             /(H(I)+H(I-1))
!        DY2(I)       = 2.e0_R8*((Y(I-1)-Y(I))/H(I-1)+(Y(I+1)-Y(I))/H(I))        & !AF 6.Oct.2011
!             /(H(I)+H(I-1))
     END DO

!     DY1(1)         = DY1(2)-DY2(2)*H(1) !AF 6.Oct.2011
!     DY1(N)         = DY1(N-1)+DY2(N-1)*H(N-1) !AF 6.Oct.2011

     DDY = 2.e0_R8*((Y(1)-Y(2))/H(1)+(Y(3)-Y(2))/H(2))/(H(2)+H(1))
     DY1(1) = DY1(2)-DDY*H(1)
     DDY = 2.e0_R8*((Y(N-2)-Y(N-1))/H(N-2)+(Y(N)-Y(N-1))/H(N-1))/(H(N-1)+H(N-2))
     DY1(N) = DY1(N-1)+DDY*H(N-1)

     DO I = 1,N
        DFUN(IFUN,I) = DY1(I)
     END DO


  END DO

  RETURN


END SUBROUTINE DERIVN3_4 !AF, 14.Sep.2011
!END SUBROUTINE DERIVN3 !AF, 14.Sep.2011
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
