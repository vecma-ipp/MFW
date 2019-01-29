MODULE CORONAL

CONTAINS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE  SET_CORONAL(COREIMPUR_IN, COREPROF_IN, COREIMPUR_OUT, INTERPOL, ICORONAL) 


! +++ Declaration of variables: 
    USE EUITM_SCHEMAS
    USE EUITM_ROUTINES
    USE COPY_STRUCTURES
    USE ALLOCATE_DEALLOCATE
    USE ITM_TYPES


    IMPLICIT NONE

    INTEGER,              PARAMETER  :: NSLICE = 1             !number of CPO ocurancies in the work flow
    INTEGER                          :: NRHO, NRHO2, IRHO
    INTEGER                          :: NNUCL,INUCL
    INTEGER                          :: NION, NZIMP1
    INTEGER                          :: NIMP, IIMP  
    INTEGER,             ALLOCATABLE :: NZIMP(:)
    INTEGER                          ::           IZIMP
    INTEGER                          :: NNEUT
    INTEGER,             ALLOCATABLE :: NCOMP(:)
    INTEGER,             ALLOCATABLE :: NTYPE(:)


    INTEGER                          :: INTERPOL           !interpolation index "0"-based on RHO_TOR; all other - based on RHO_TOR_NORM 
    INTEGER                          :: ICORONAL           !interpolation index: "0"-OFF; "1" - replace boundary conditions by coronal;   "2" - replace boundary conditions and profiles by coronal 

! +++ CPO derived types:
    TYPE (TYPE_COREIMPUR), POINTER   :: COREIMPUR_IN(:)    !input CPO with impurity 
    TYPE (TYPE_COREPROF),  POINTER   :: COREPROF_IN(:)     !input CPO with plasma profiles
    TYPE (TYPE_COREIMPUR), POINTER   :: COREIMPUR_OUT(:)   !output CPO with sources uploaded from the data base 


! +++ Internal derived types:
    REAL (R8),          ALLOCATABLE  :: RHO(:),  RHO2(:), NE(:), TE(:), N_IMPURITY(:,:)   
    REAL (R8)                        :: NIMP_TOT, AMN, ZN   



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
! +++ allocate and define grid of output CPO:
    NRHO               = SIZE (COREIMPUR_IN(1)%rho_tor, DIM=1)
    NRHO2              = SIZE (COREPROF_IN(1)%rho_tor,  DIM=1)

    CALL GET_COMP_DIMENSIONS      (COREIMPUR_IN(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
    IF(.NOT.(ASSOCIATED(COREIMPUR_OUT)))&
!!    CALL ALLOCATE_COREIMPUR_CPO   (NSLICE,      NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COREIMPUR_OUT)     !! overwritten by next line
    CALL COPY_CPO                 (COREIMPUR_IN, COREIMPUR_OUT)


    IF(ICORONAL .EQ. 0) GOTO 10


    ALLOCATE ( RHO(NRHO))
    ALLOCATE ( RHO2(NRHO2))

    IF (INTERPOL.EQ.0) THEN
       RHO             = COREIMPUR_IN(1)%rho_tor
       RHO2            = COREPROF_IN(1)%rho_tor
    ELSE
       RHO             = COREIMPUR_IN(1)%rho_tor / COREIMPUR_IN(1)%rho_tor(NRHO)
       RHO2            = COREPROF_IN(1)%rho_tor  / COREPROF_IN(1)%rho_tor(NRHO2)
    END IF




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
    ALLOCATE ( TE(NRHO))
    ALLOCATE ( NE(NRHO))
    
    IF (ASSOCIATED(COREPROF_IN(1)%ne%value))                                                      &
    CALL L3interp (COREPROF_IN(1)%ne%value,  RHO2, NRHO2,       NE,  RHO, NRHO)
    IF (ASSOCIATED(COREPROF_IN(1)%te%value))                                                      &
    CALL L3interp (COREPROF_IN(1)%te%value,  RHO2, NRHO2,       TE,  RHO, NRHO)
 
    IF (COREPROF_IN(1)%te%boundary%type .EQ. 1)  TE(NRHO)    =  COREPROF_IN(1)%te%boundary%value(1)



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
    DO IIMP = 1, NIMP
       INUCL          =      COREIMPUR_IN(1)%COMPOSITIONS%IMPURITIES(IIMP)%nucindex

       ZN             =      COREIMPUR_IN(1)%COMPOSITIONS%NUCLEI(INUCL)%zn
       AMN            =      COREIMPUR_IN(1)%COMPOSITIONS%NUCLEI(INUCL)%amn
       NIMP_TOT       =      sum(COREIMPUR_IN(1)%IMPURITY(IIMP)%boundary%value(1,:))
       write(*,*) 'set_coronal: iimp, NIMP_TOT = ', iimp, NIMP_TOT

       IF (ALLOCATED(N_IMPURITY)) DEALLOCATE(N_IMPURITY)
       ALLOCATE     (N_IMPURITY(NRHO,NZIMP(IIMP)))

       NZIMP1        =       NZIMP(IIMP)
       CALL CORONAL_DISTRIBUTION (NRHO, NE, TE, AMN, ZN, NZIMP1, N_IMPURITY)

       DO IRHO = 1,NRHO
          DO IZIMP = 1,NZIMP(IIMP)
             IF (N_IMPURITY(NRHO,IZIMP).LE.1.0D-200) N_IMPURITY(NRHO,IZIMP) = 0._R8 
          ENDDO
       ENDDO

!      Replace boundary conditions by coronal
       IF(ICORONAL .EQ. 1) THEN
          DO IZIMP = 1,NZIMP(IIMP)
             COREIMPUR_OUT(1)%IMPURITY(IIMP)%boundary%value(1,IZIMP)    = N_IMPURITY(NRHO,IZIMP)*NIMP_TOT 
             COREIMPUR_OUT(1)%IMPURITY(IIMP)%boundary%value(2,IZIMP)    = 0.0_R8
             COREIMPUR_OUT(1)%IMPURITY(IIMP)%boundary%value(3,IZIMP)    = 0.0_R8
          END DO
          
          
!      Replace boundary conditions and concentration profiles by coronal
       ELSE IF(ICORONAL .EQ. 2) THEN
          DO IZIMP = 1,NZIMP(IIMP)
             COREIMPUR_OUT(1)%IMPURITY(IIMP)%boundary%value(1,IZIMP)    = N_IMPURITY(NRHO,IZIMP)*NIMP_TOT 
             COREIMPUR_OUT(1)%IMPURITY(IIMP)%boundary%value(2,IZIMP)    = 0.0_R8
             COREIMPUR_OUT(1)%IMPURITY(IIMP)%boundary%value(3,IZIMP)    = 0.0_R8

             COREIMPUR_OUT(1)%IMPURITY(IIMP)%nz(:,IZIMP)                = N_IMPURITY(:,IZIMP)*NIMP_TOT 

             COREIMPUR_OUT(1)%IMPURITY(IIMP)%z(:,IZIMP)                 = IZIMP 
             COREIMPUR_OUT(1)%IMPURITY(IIMP)%zsq(:,IZIMP)               = IZIMP**2

          END DO

          
       END IF


    END DO




    IF (ALLOCATED(N_IMPURITY)) DEALLOCATE (N_IMPURITY)
    DEALLOCATE (TE)
    DEALLOCATE (NE)
    DEALLOCATE (RHO)
    DEALLOCATE (RHO2)


 10 RETURN


    END SUBROUTINE  SET_CORONAL 
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
SUBROUTINE coronal_distribution (NRHO, NE, TE, AMN,  ZN, NZIMP1, N_IMPURITY)
  USE itm_types
  USE amns_types
  USE amns_module


  IMPLICIT NONE

  INTEGER                                 :: NZIMP1
  INTEGER                                 :: NRHO, IRHO
  TYPE (amns_handle_type)                 :: amns                                            !< AMNS global handle
  TYPE (amns_handle_rx_type), ALLOCATABLE :: amns_ei(:), amns_rc(:), amns_lr(:), amns_br(:)  !< AMNS table handles
  TYPE (amns_reaction_type)               :: xx_rx
  TYPE (amns_reactants_type)              :: species
  TYPE (amns_query_type)                  :: query
  TYPE (amns_answer_type)                 :: answer
  TYPE (amns_set_type)                    :: set
  REAL (kind=R8)                          :: te(NRHO), ne(NRHO), N_IMPURITY(NRHO,NZIMP1)
  REAL (kind=R8),             ALLOCATABLE :: na(:), rhs(:)
  REAL (kind=R8),             ALLOCATABLE :: rate_ei(:,:), rate_rc(:,:), rate_lr(:,:), rate_br(:,:)
  REAL (kind=R8),             ALLOCATABLE :: l(:), d(:), u(:)
  REAL (kind=R8)                          :: zn, mi, AMN
  REAL (kind=R8)                          :: test, norm, line_radiation, recombination_radiation
  CHARACTER (len=12),         ALLOCATABLE :: state_labels(:)
  INTEGER                                 :: ns, is, isref
!================================================================================



  mi = AMN
  mi = 0
  ns = NZIMP1

! some initializations
  ALLOCATE(amns_ei(0:ns), amns_rc(0:ns), amns_lr(0:ns), amns_br(0:ns))
  ALLOCATE(state_labels(0:ns))
  ALLOCATE(l(0:ns), d(0:ns), u(0:ns), na(0:ns), rhs(0:ns))
  ALLOCATE(rate_ei(1:NRHO,0:ns), rate_rc(1:NRHO,0:ns), rate_lr(1:NRHO,0:ns), rate_br(1:NRHO,0:ns))

! set up the AMNS system

  CALL ITM_AMNS_SETUP(amns)
  WRITE(*,*) 'Done ITM_AMNS_SETUP'

! set up tables for ionization
  xx_rx%string='EI'
  DO is=0,ns
     IF(is .NE. zn) THEN
        allocate(species%components(4))
        species%components =  &
             (/ amns_reactant_type(zn, is, mi, 0), &
                amns_reactant_type(0, -1, 0, 0), &
                amns_reactant_type(zn, is+1, mi, 1), &
                amns_reactant_type(0, -1, 0, 1) &
              /)
        CALL ITM_AMNS_SETUP_TABLE(amns, xx_rx, species, amns_ei(is))
        deallocate(species%components)
        query%string='state_label'
        CALL ITM_AMNS_QUERY_TABLE(amns_ei(is),query,answer)
        state_labels(is)=answer%string
     ENDIF
  ENDDO

! set up tables for line radiation
  xx_rx%string='LR'
  DO is=0,ns
     IF(is .NE. zn) THEN
        allocate(species%components(2))
        species%components = &
             (/ amns_reactant_type(zn, is, mi, 0), &
                amns_reactant_type(zn, is, mi, 1) &
             /)
        CALL ITM_AMNS_SETUP_TABLE(amns, xx_rx, species, amns_lr(is))
        deallocate(species%components)
        query%string='state_label'
        CALL ITM_AMNS_QUERY_TABLE(amns_lr(is),query,answer)
        state_labels(is)=answer%string
     ENDIF
  ENDDO

! set up tables for recombination
  xx_rx%string='RC'
  DO is=0,ns
     IF(is .NE. 0) THEN
        allocate(species%components(4))
        species%components =  &
             (/ amns_reactant_type(zn, is, mi, 0), &
                amns_reactant_type(0, -1, 0, 0), &
                amns_reactant_type(zn, is-1, mi, 1), &
                amns_reactant_type(0, -1, 0, 1) &
              /)
        CALL ITM_AMNS_SETUP_TABLE(amns, xx_rx, species, amns_rc(is))
        deallocate(species%components)
        query%string='state_label'
        CALL ITM_AMNS_QUERY_TABLE(amns_rc(is),query,answer)
        state_labels(is)=answer%string
     ENDIF
  ENDDO

! set up tables for recombination radiation
  xx_rx%string='BR'
  DO is=0,ns
     IF(is .NE. 0) THEN
        allocate(species%components(2))
        species%components = &
             (/ amns_reactant_type(zn, is, mi, 0), &
                amns_reactant_type(zn, is, mi, 1) &
             /)
        CALL ITM_AMNS_SETUP_TABLE(amns, xx_rx, species, amns_br(is))
        deallocate(species%components)
        query%string='state_label'
        CALL ITM_AMNS_QUERY_TABLE(amns_br(is),query,answer)
        state_labels(is)=answer%string
     ENDIF
  ENDDO

  WRITE(*,*) 'Done ITM_AMNS_SETUP_TABLE'

! interpolate ionization and line radiation data
  set%string='nowarn'
  DO is=0,ns
     IF(is .NE. zn) THEN
         CALL ITM_AMNS_SET_TABLE(amns_ei(is),set)
         CALL ITM_AMNS_RX(amns_ei(is),rate_ei(:,is),te,ne)
         CALL ITM_AMNS_SET_TABLE(amns_lr(is),set)
         CALL ITM_AMNS_RX(amns_lr(is),rate_lr(:,is),te,ne)
     ENDIF
  ENDDO

! interpolate recombination and recombination radiation data
  set%string='nowarn'
  DO is=0,ns
     IF(is .NE. 0) THEN
         CALL ITM_AMNS_SET_TABLE(amns_rc(is),set)
         CALL ITM_AMNS_RX(amns_rc(is),rate_rc(:,is),te,ne)
         CALL ITM_AMNS_SET_TABLE(amns_br(is),set)
         CALL ITM_AMNS_RX(amns_br(is),rate_br(:,is),te,ne)
     ENDIF
  ENDDO

  WRITE(*,*) 'Done ITM_AMNS_RX'

! finalize the tables
  DO is=0,ns
     IF(is .NE. zn) THEN
        CALL ITM_AMNS_FINISH_TABLE(amns_ei(is))
        CALL ITM_AMNS_FINISH_TABLE(amns_lr(is))
     ENDIF
  ENDDO
  DO is=0,ns
     IF(is .NE. 0) THEN
        CALL ITM_AMNS_FINISH_TABLE(amns_rc(is))
        CALL ITM_AMNS_FINISH_TABLE(amns_br(is))
     ENDIF
  ENDDO

  WRITE(*,*) 'Done ITM_AMNS_FINISH_TABLE'

! finalize the system
  CALL ITM_AMNS_FINISH(amns)

  WRITE(*,*) 'Done ITM_AMNS_FINISH'

  OPEN(10,file='coronal.out')
  WRITE(10,'(100a15)') '#te            ',state_labels, &
                       '   LR          ',  &
                       '   BR          '

  DO IRHO=1, NRHO

! set up the matrix
     l(0)=0.0_R8
     d(0)=-rate_ei(IRHO,0)
     u(0)=rate_rc(IRHO,1)
     rhs(0)=0.0_R8
     DO is=1,ns-1
        l(is)=rate_ei(IRHO,is-1)
        d(is)=-rate_ei(IRHO,is)-rate_rc(IRHO,is)
        u(is)=rate_rc(IRHO,is+1)
        rhs(is)=0.0_R8
     ENDDO
     l(ns)=rate_ei(IRHO,ns-1)
     d(ns)=-rate_rc(IRHO,ns)
     u(ns)=0.0_R8
     rhs(ns)=0.0_R8

! we need to set the value of 1 charge state
     isref=0
     DO is=1, ns-1
        IF(rate_ei(IRHO,is) .LT. rate_rc(IRHO,is)) THEN
           EXIT
        ELSE
           isref=isref+1
        ENDIF
     ENDDO
!     write(*,*) te(IRHO), isref
     u(isref)=0.0_R8
     d(isref)=1.0_R8
     l(isref)=0.0_R8
     rhs(isref)=1.0_R8

! solve (l,d,u) na = rhs
     CALL solve_tridiag(l,d,u,rhs,na,ns+1)
     na(:) = na(:) / SUM(na)

     line_radiation = SUM(na(0:ns-1)*rate_lr(IRHO,0:ns-1))
     recombination_radiation = SUM(na(1:ns)*rate_br(IRHO,1:ns))

! OUTPUT DENSITIES:
     DO is=1,ns
        N_IMPURITY(IRHO,IS) = na(IS)
     ENDDO 


! check the answer

     norm=MAX(MAXVAL(rate_ei(IRHO,0:ns-1) * na(0:ns-1)), MAXVAL(rate_rc(IRHO,1:ns) * na(1:ns)))
     is=0
     test = (-rate_ei(IRHO,is) * na(is) + rate_rc(IRHO,is+1) * na(is+1))/norm
     IF(ABS(test).GT.1e-15_R8) WRITE(*,*) 'LARGE ERROR: ',IRHO, is, test
     DO is=1, ns-1
        test = (rate_ei(IRHO,is-1) * na(is-1) - (rate_rc(IRHO,is) + rate_ei(IRHO,is)) * na(is) + rate_rc(IRHO,is+1) * na(is+1))/norm
	IF(ABS(test).GT.1e-15_R8) WRITE(*,*) 'LARGE ERROR: ',IRHO, is, test
     ENDDO
     is=ns
     test = (rate_ei(IRHO,is-1) * na(is-1) - rate_rc(IRHO,is) * na(is))/norm
     IF(ABS(test).GT.1e-15_R8) WRITE(*,*) 'LARGE ERROR: ',IRHO, is, test


  ENDDO
  CLOSE(10)


  RETURN


END SUBROUTINE coronal_distribution


SUBROUTINE solve_tridiag(a,b,c,v,x,n)
  USE itm_types
  IMPLICIT NONE
!      a - sub-diagonal (means it is the diagonal below the main diagonal)
!      b - the main diagonal
!      c - sup-diagonal (means it is the diagonal above the main diagonal)
!      v - right part
!      x - the answer
!      n - number of equations
 
  INTEGER,INTENT(in) :: n
  REAL(kind=R8),DIMENSION(n),INTENT(in) :: a,b,c,v
  REAL(kind=R8),DIMENSION(n),INTENT(out) :: x
  REAL(kind=R8),DIMENSION(n) :: bp,vp
  REAL(kind=R8) :: m
  INTEGER i
 
! Make copies of the b and v variables so that they are unaltered by this sub
  bp(1) = b(1)
  vp(1) = v(1)
  
  !The first pass (setting coefficients):
  firstpass: DO i = 2,n
     m = a(i)/bp(i-1)
     bp(i) = b(i) - m*c(i-1)
     vp(i) = v(i) - m*vp(i-1)
  END DO firstpass
  
  x(n) = vp(n)/bp(n)
  !The second pass (back-substition)
  backsub:DO i = n-1, 1, -1
     x(i) = (vp(i) - c(i)*x(i+1))/bp(i)
  END DO backsub
  
END SUBROUTINE solve_tridiag
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

END MODULE CORONAL
