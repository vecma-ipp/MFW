! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This routine aimed in testing the solver solution
!> method for the set of  transport equations 
!> with given sources and transport coefficients.
!>
!> \author D.Kalupin, R.Stankiewicz
!>
!> \version "$Id: solver_test.F90 1763 2016-06-22 15:41:53Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
PROGRAM SOLVER_TEST
! + + + + + + + + + + + + + + + + + + + + + + + + + + + !  
!     This routine aimed in testing the solver solution !
!     method for the set of  transport equations        !
!     with given sources and transport coefficients.    !
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!     Source:       ---                                 +
!     Developers:   D.Kalupin, R.Stankiewicz            +
!     Kontacts:     D.Kalupin@fz-juelich.de             +
!                   Roman.Stankiewich@gmail.com         +
!                                                       +
!     Comments:     It will be removed as soon the      +
!                   KEPLER implementation is done       +
!                                                       +
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


! + + + Declaration of variables: + + + + + + + + + + + +

  use itm_types
  USE EUITM_SCHEMAS
  use copy_structures
#ifdef UAL
  USE EUITM_ROUTINES
#endif
  USE TYPE_ANALYTICS
  USE ETS_PLASMA
  USE ALLOCATE_DEALLOCATE
  USE ETS
  USE CONVERGENCE_CHECK
  USE COPY_CPO_ETS
  USE ANALYTICS1
  USE ITM_TEST_ROUTINES
  use deallocate_structures
  use xml_file_reader
  use ets_version
  USE PLASMA_COMPOSITION

  IMPLICIT NONE

  INTEGER                          :: ifail

  INTEGER                          :: SHOT_NO             !shot mumber
  INTEGER                          :: RUN_NO              !run mumber

  INTEGER                          :: NRHO                !number of radial points    (input)
  INTEGER                          :: NTIME               !number of time points      (input)
  INTEGER,               PARAMETER :: NSLICE = 1           !number of CPO ocurancies in the work flow

  INTEGER                          :: NNUCL               !number of neutrals species    (input)
  INTEGER                          :: NION                !number of ion species        (input)
  INTEGER                          :: NIMP                !number of impurity species   (input)
  INTEGER,       ALLOCATABLE, SAVE :: NZIMP(:)            !number of charge states for each impurity (input)

  INTEGER                          :: NNEUT               !number of neutrals species                (input)
  INTEGER,       ALLOCATABLE, SAVE :: NCOMP(:)            !number of components for each neutral
  INTEGER,       ALLOCATABLE, SAVE :: NTYPE(:)            !number of types for each neutral


  REAL (R8),     ALLOCATABLE, SAVE :: AMN(:)
  REAL (R8),     ALLOCATABLE, SAVE :: ZN(:)
  REAL (R8),     ALLOCATABLE, SAVE :: ZION(:)
  REAL (R8),     ALLOCATABLE, SAVE :: AMN_IMP(:)
  REAL (R8),     ALLOCATABLE, SAVE :: ZN_IMP(:)
  REAL (R8),     ALLOCATABLE, SAVE :: MAX_Z_IMP(:)

  INTEGER                          :: COLD_NEUTRALS
  INTEGER                          :: THERMAL_NEUTRALS
  INTEGER                          :: FAST_NEUTRALS
  INTEGER                          :: NBI_NEUTRALS
          
  INTEGER                          :: IRHO                !current radial knot
  INTEGER                          :: IION                !current ion type
  INTEGER                          :: ITIME               !current time step

  INTEGER                          :: NSOL                !Number of analytical example
  INTEGER                          :: SOLVER_TYPE         !representation of transport equations 
!1-"standard"; 2-"integral"(default)
  INTEGER                          :: SIGMA_SOURCE        !source for plasma conductivity

  REAL (R8)                        :: CONVREC             !required convergency 
  REAL (R8)                        :: TIME                !Time
  REAL (R8)                        :: TAU                 !time step, and mixing coefficient
  REAL (R8)                        :: AMIX                !mixing factor
  INTEGER                          :: ITER                !iteration index
  INTEGER, PARAMETER               :: MAXITER=1000        !maximum number of convergence iterations

  INTEGER                          :: PSI_BND_TYPE        !Type of boundary conditions current
  INTEGER                          :: NI_BND_TYPE         !Type of boundary conditions ion density 
  INTEGER                          :: TI_BND_TYPE         !Type of boundary conditions ion temperature
  INTEGER                          :: TE_BND_TYPE         !Type of boundary conditions electron temperature
  INTEGER                          :: VTOR_BND_TYPE       !Type of boundary conditions toroidal rotation

  TYPE (RUN_CONTROL)               :: CONTROL             !contains all parameters required by run

! +++ CPO derived types:
  TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM_OLD(:)  !input CPO with geometry quantities
  TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM_ITER(:) !input CPO with geometry quantities
  TYPE (TYPE_COREPROF),    POINTER :: COREPROF_OLD(:)     !input CPO with plasma profiles at the previous time step
  TYPE (TYPE_COREPROF),    POINTER :: COREPROF_NEW(:)     !output CPO with plasma profiles at the next time step
  TYPE (TYPE_COREPROF),    POINTER :: COREPROF_ITER(:)    !input CPO with plasma profiles at previous iteration
  TYPE (TYPE_COREPROF),    POINTER :: COREPROF_ANALYTIC(:)!input CPO with plasma profiles at previous iteration
  TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP(:)       !input CPO with transport coefficients
  TYPE (TYPE_CORESOURCE),  POINTER :: CORESOURCE(:)       !input CPO with sources
  TYPE (TYPE_COREIMPUR),   POINTER :: COREIMPUR(:)        !input CPO with impurities
  type (type_param)                :: code_parameters, ets_parameters
  character (len=32)               :: database_format

  INTEGER                          :: CONTROL_INTEGER(2)  !integer control parameters
  REAL (R8)                        :: CONTROL_DOUBLE(6)   !real control parameters

  INTEGER                          :: IDX

!
! the following interface blocks are for testing the Kepler interface 
! which does not allow for routines in modules to be called
!
  interface
     subroutine analyticalplasma(TIMEx,COREPROF_in,  &
          EQUILIBRIUM,COREPROF,CORETRANSP,CORESOURCE,COREIMPUR, code_parameters)

       use itm_types
       USE EUITM_SCHEMAS
       use ANALYTICS1

       implicit none

! input arguments
       REAL (R8)                              :: TIMEx
       TYPE (TYPE_COREPROF),   POINTER        :: COREPROF_in(:)
! output arguments
       TYPE (TYPE_EQUILIBRIUM),POINTER        :: EQUILIBRIUM(:)
       TYPE (TYPE_COREPROF),   POINTER        :: COREPROF(:)
       TYPE (TYPE_CORETRANSP), POINTER        :: CORETRANSP(:)
       TYPE (TYPE_CORESOURCE), POINTER        :: CORESOURCE(:)
       TYPE (TYPE_COREIMPUR),  POINTER        :: COREIMPUR(:)
       type (type_param)                      :: code_parameters

     end subroutine analyticalplasma

     subroutine itmets                                      &
          (COREPROF_OLD, COREPROF_ITER, COREPROF_NEW,       &
          EQUILIBRIUM_OLD, EQUILIBRIUM_ITER,                &
          CORETRANSP, CORESOURCE, COREIMPUR,                &
          CONTROL_INTEGERx, CONTROL_DOUBLEx, ets_parameters)
       
       USE EUITM_SCHEMAS
       USE ETS_PLASMA
       USE ETS
       
       IMPLICIT NONE

! input arguments
       TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_OLD(:), EQUILIBRIUM_ITER(:)
       TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_OLD(:), COREPROF_ITER(:)
       TYPE (TYPE_CORETRANSP),  POINTER  :: CORETRANSP(:)
       TYPE (TYPE_CORESOURCE),  POINTER  :: CORESOURCE(:)
       TYPE (TYPE_COREIMPUR),   POINTER  :: COREIMPUR(:)
       INTEGER                           :: CONTROL_INTEGERx(2) !integer control parameters
       REAL (R8)                         :: CONTROL_DOUBLEx(5)  !real control parameters
!output arguments       
       TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_NEW(:)
       type (type_param) :: ets_parameters
      
     end subroutine itmets

     subroutine convergence(COREPROF_ITER, COREPROF_OUT, CONTROL_DOUBLE)
       
       USE EUITM_SCHEMAS
       USE ETS_PLASMA
       USE CONVERGENCE_CHECK
       
       implicit none
       
! input arguments
       TYPE (TYPE_COREPROF), POINTER  :: COREPROF_ITER(:)
       TYPE (TYPE_COREPROF), POINTER  :: COREPROF_OUT(:)

!output arguments
       REAL (R8)                      :: CONTROL_DOUBLE(5)

     end subroutine convergence

  end interface

!
! start of the actual program
!

  call perfinit
  call perfon ('whole')

!
! get the input data
!

  call perfon ('read')
  shot_no=0
  run_no=-1

  call fill_param(ets_parameters, 'XML/ets.xml', '', 'XML/ets.xsd')
  call fill_param(code_parameters, 'XML/ets_analytics.xml', '', 'XML/ets_analytics.xsd')
  CALL PROCESS_XML(SOLVER_TYPE,SIGMA_SOURCE,TAU,AMIX,CONVREC,NRHO,NION,NIMP,NZIMP,NTIME,NSOL,      &
       PSI_BND_TYPE,NI_BND_TYPE,TI_BND_TYPE,TE_BND_TYPE,VTOR_BND_TYPE,                &
       shot_no, run_no, code_parameters, database_format)
!
! save the necessary data in the CONTROL structure
!
  ifail = 0

  CALL ALLOCATE_RUN_CONTROL          (                          CONTROL,    ifail)
  call set_control(SOLVER_TYPE,SIGMA_SOURCE,TAU,AMIX,CONVREC,control)
  CONTROL_INTEGER(1)     = CONTROL%SOLVER_TYPE
  CONTROL_INTEGER(2)     = CONTROL%SIGMA_SOURCE
  CONTROL_DOUBLE(1)      = CONTROL%TAU        
  CONTROL_DOUBLE(2)      = CONTROL%AMIX       
  CONTROL_DOUBLE(3)      = CONTROL%AMIXTR     
  CONTROL_DOUBLE(4)      = CONTROL%CONV       
  CONTROL_DOUBLE(5)      = CONTROL%CONVREC    
  call perfoff

!
! if the UAL preprocessor option is defined, and if the run and sequence 
! numbers are OK, we will write out the results using the UAL
!
! here we initialize the UAL
!

#ifdef UAL
  if(shot_no.gt.0.and.run_no.ge.0) then
     call perfon ('ualo')
     write(*,*) 'Using ', trim(database_format),' for the backend'
     select case (database_format)
     case ("mdsplus")
        call euitm_create('euitm',shot_no,run_no,0,0,idx)
     case ("hdf5")
        call euitm_create_hdf5('euitm',shot_no,run_no,0,0,idx)
     case default
        write(*,*) 'Unexpected database format choice : ',trim(database_format)
        stop 'Error: unrecognized database format'
     end select
     call perfoff
  endif
#endif


  call perfon ('alloc')

  NNUCL = NION
  NNEUT = NION
  ALLOCATE (NCOMP(NNEUT))
  ALLOCATE (NTYPE(NNEUT))
  NCOMP = 1
  NTYPE = 1

  CALL ALLOCATE_COREPROF_CPO    (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COREPROF_OLD)  
  CALL ALLOCATE_COREPROF_CPO    (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COREPROF_ITER)  

  ALLOCATE (AMN(NION))
  ALLOCATE (ZN(NION))
  ALLOCATE (ZION(NION))
  ALLOCATE (AMN_IMP(NIMP))
  ALLOCATE (ZN_IMP(NIMP))
  ALLOCATE (MAX_Z_IMP(NIMP))

  AMN       = 2.0_r8
  ZN        = 1.0_r8
  ZION      = 1.0_r8
  AMN_IMP   = 0.0_r8
  ZN_IMP    = 0.0_r8
  MAX_Z_IMP = 0.0_r8

  COLD_NEUTRALS    = 0
  THERMAL_NEUTRALS = 0
  FAST_NEUTRALS    = 0
  NBI_NEUTRALS     = 0
          
  CALL SET_PLASMA_COMPOSITION   (COREPROF_ITER,                         &
                                 NION,      NIMP,      NNEUT,           &
                                 AMN,       ZN,        ZION,            &
                                 AMN_IMP,   ZN_IMP,    MAX_Z_IMP,       &
                                 NCOMP,     NTYPE,                      &
                                 COLD_NEUTRALS,        THERMAL_NEUTRALS,&
                                 FAST_NEUTRALS,        NBI_NEUTRALS)

  call deallocate_cpo(COREPROF_OLD(1)%COMPOSITIONS)
  CALL COPY_CPO                 (COREPROF_ITER(1)%COMPOSITIONS,        COREPROF_OLD(1)%COMPOSITIONS)
  call perfoff

  allocate(coreprof_old(1)%codeparam%codename(1))
  coreprof_old(1)%codeparam%codename(1)='solver_test'
  allocate(coreprof_old(1)%codeparam%codeversion(1))
  coreprof_old(1)%codeparam%codeversion(1)=version
  allocate(coreprof_old(1)%codeparam%parameters(size(code_parameters%parameters)))
  coreprof_old(1)%codeparam%parameters=code_parameters%parameters

!
! initialize the start of the run
!
! we use ANALYTICAL_PLASMA to set up the initial plasma profile in 
! COREPROF_OLD, but then need to destroy the unneeded CPOs created by
! ANALYTICAL_PLASMA
!

  call perfon ('initprof')
  TIME = 0.0e0_R8

  CALL ANALYTICAL_PLASMA   (TIME, coreprof_old, EQUILIBRIUM_OLD, COREPROF_ANALYTIC,            &
       CORETRANSP, CORESOURCE, COREIMPUR, code_parameters)

!fc2k work around (either use the previous lines or the following ones)

!  CALL ANALYTICALPLASMA   (TIME, coreprof_old, EQUILIBRIUM_OLD, COREPROF_ANALYTIC,            &
!       CORETRANSP, CORESOURCE, COREIMPUR)

  call deallocate_cpo       (COREPROF_OLD)
  call copy_cpo             (COREPROF_ANALYTIC,COREPROF_OLD)
  CALL DEALLOCATE_CPO       (COREPROF_ANALYTIC)        
  CALL DEALLOCATE_CPO       (CORETRANSP       )          
  CALL DEALLOCATE_CPO       (CORESOURCE       )          
  CALL DEALLOCATE_CPO       (COREIMPUR        )          
  call perfoff


!----------------------------------------------------------------------!
!     Time evolution from the time step '1' until the NTIME            !
!----------------------------------------------------------------------!

  call perfon ('timestep')
  TIME_LOOP1: DO ITIME = 1,NTIME

     TIME = TIME + TAU

     WRITE (6,*) '========================================'
     WRITE (6,*) 'TIME=',TIME
     WRITE (6,*) '========================================'

!
! initialize iteration loop, using ANALYTICAL_PLASMA to provide the
! equilibrium, sources, transport coefficients and analytic answer (in 
! COREPROF_ANALYTIC
!
     ITER = 0

     call perfon ('t.analyt')
     CALL ANALYTICAL_PLASMA  (TIME, coreprof_old, EQUILIBRIUM_ITER, COREPROF_ANALYTIC,         &
          CORETRANSP, CORESOURCE, COREIMPUR, code_parameters)

!fc2k work around (either use the previous lines or the following ones)

!     CALL ANALYTICALPLASMA   (TIME, coreprof_old, EQUILIBRIUM_ITER, COREPROF_ANALYTIC,         &
!          CORETRANSP, CORESOURCE, COREIMPUR)
     call perfoff

!
! we need a new coreprof for the iteration loop
!

     call perfon ('t.copy.i')
     call copy_cpo(COREPROF_OLD,COREPROF_ITER)
     CALL COPY_BOUNDARY_COND     (COREPROF_ANALYTIC, COREPROF_ITER)

     call perfoff

!
! this is the iteration loop where we iterate until convergence
!

10   CONTINUE

!
! increment iteration number and stop if too many iterations
!
     ITER = ITER + 1
     if(ITER.gt.MAXITER) then
        write(*,'(a,i0,a)') 'Maximum number of iterations ( ',MAXITER,' ) exceeded'
        write(*,'(a,1pg10.3,a,i0)') 'Time = ', TIME, '  Number of time iterations = ', ITIME
        stop 'Error'
     endif

!
! this is the call to the ETS
! 
! it uses as input
!   COREPROF_OLD     : COREPROF at the old timestep
!   COREPROF_ITER    : COREPROF at the previous iteration
!   EQUILIBRIUM_OLD  : EQUILIBRIUM at the old timestep
!   EQUILIBRIUM_ITER : EQUILIBRIUM at the previous iteration
!   CORETRANSP       : CORETRANSP from ANALYTICAL_PLASMA
!   CORESOURCE       : CORESOURCE from ANALYTICAL_PLASMA
!   COREIMPUR        : COREIMPUR from ANALYTICAL_PLASMA
!   CONTROL          : control information (either a structure or two arrays)
!
! and provides as output
!   COREPROF_NEW     : COREPROF at the new iteration
!

     call perfon ('t.ets')

     CALL ITM_ETS                                              &
          (COREPROF_OLD, COREPROF_ITER, COREPROF_NEW,          &
          EQUILIBRIUM_OLD, EQUILIBRIUM_ITER,                   &
          CORETRANSP, CORESOURCE, COREIMPUR,                   &
          CONTROL_INTEGER, CONTROL_DOUBLE, ets_parameters)

!fc2k work around (either use the previous lines or the following ones)

!     CALL ITMETS                                               &
!          (COREPROF_OLD, COREPROF_ITER, COREPROF_NEW,          &
!          EQUILIBRIUM_OLD, EQUILIBRIUM_ITER,                   &
!          CORETRANSP, CORESOURCE, COREIMPUR,                   &
!          CONTROL_INTEGER, CONTROL_DOUBLE, ets_parameters) 

     call perfoff

!
! now we check for convergence
!
! note that CHECK_CONVERGENCE also copies COREPROF_NEW to COREPROF_ITER [no longer true]
!

     call perfon ('t.conv')

     CALL CHECK_CONVERGENCE   (COREPROF_ITER, COREPROF_NEW, CONTROL_DOUBLE)

!fc2k work around (either use the previous lines or the following ones)

!     CALL CONVERGENCE   (COREPROF_ITER, COREPROF_NEW, CONTROL_DOUBLE)

!
! since CHECK_CONVERGENCE copied COREPROF_NEW to COREPROF_ITER, we now
! need to destroy the extra copy of COREPROF
!

     CALL DEALLOCATE_CPO         (COREPROF_ITER     )        
     CALL COPY_CPO(COREPROF_NEW,COREPROF_ITER)         ! latest version of CHECK_CONVERGENCE no longer copies

     CALL DEALLOCATE_CPO         (COREPROF_NEW     )        
     
     call perfoff

!
! here we loop back if the convergence criterion has not been satisfied
! (currently disabled)
!

     write(*,*) 'Convergence test: ', ITER, CONTROL_DOUBLE(4), CONTROL_DOUBLE(5)
!DPC remove temporarily     IF (CONTROL%CONV.GT.CONTROL%CONVREC) GOTO 10 
     IF (CONTROL_DOUBLE(4).GT.CONTROL_DOUBLE(5)) GOTO 10 

!
! at this point the iteration loop has converged and we write out the results
!

     call perfon ('t.write')
     CALL WRITE_OUT           (ITIME,COREPROF_ITER, COREPROF_ANALYTIC)
     call perfoff
#ifdef UAL
     if(shot_no.gt.0.and.run_no.ge.0) then
        call perfon ('t.ualw')
        coreprof_iter(1)%time=time
	if(itime.eq.1) then
           write(*,*) 'euitm_put_non_timed: coreprof', coreprof_iter(1)%time,  &
                trim(coreprof_iter(1)%codeparam%codename(1)),  &
                '  ',  &
                trim(coreprof_iter(1)%codeparam%codeversion(1))
           call euitm_put_non_timed(idx,"coreprof",coreprof_iter(1))
        endif
        write(*,*) 'euitm_put_slice: coreprof', COREPROF_ITER(1)%time
        call euitm_put_slice(idx,"coreprof",COREPROF_ITER(1))
        EQUILIBRIUM_ITER(1)%time=time
        write(*,*) 'euitm_put_slice: equilibrium', EQUILIBRIUM_ITER(1)%time
        call euitm_put_slice(idx,"equilibrium",EQUILIBRIUM_ITER(1))
        CORETRANSP(1)%time=time
        write(*,*) 'euitm_put_slice: coretransp', CORETRANSP(1)%time
        call euitm_put_slice(idx,"coretransp",CORETRANSP(1))
        CORESOURCE(1)%time=time
        write(*,*) 'euitm_put_slice: coresource', CORESOURCE(1)%time
        call euitm_put_slice(idx,"coresource",CORESOURCE(1))
        COREPROF_ANALYTIC(1)%time=time
        write(*,*) 'euitm_put_slice: coreprof/1', COREPROF_ANALYTIC(1)%time
        call euitm_put_slice(idx,"coreprof/1",COREPROF_ANALYTIC(1))
        call perfoff
     endif
#endif

!
! we need to prepare for the next time-step
!
! this involves
!   copying current _ITER values to _OLD
!   destroying the now unneeded _ITER structures
!   destroying the unneeded ANALYTIC solution
!   destroying the unneeded CORETRANSP, CORESOURCE and COREIMPUR

     call perfon ('t.copy.o')

     CALL DEALLOCATE_CPO         (COREPROF_OLD    )        
     call copy_cpo(COREPROF_ITER,COREPROF_OLD)
     CALL DEALLOCATE_CPO         (COREPROF_ITER    )        
     CALL DEALLOCATE_CPO      (EQUILIBRIUM_OLD )         
     call copy_cpo(EQUILIBRIUM_ITER,EQUILIBRIUM_OLD)
     CALL DEALLOCATE_CPO      (EQUILIBRIUM_ITER )         
     CALL DEALLOCATE_CPO         (COREPROF_ANALYTIC)        
     CALL DEALLOCATE_CPO       (CORETRANSP       )          
     CALL DEALLOCATE_CPO       (CORESOURCE       )          
     CALL DEALLOCATE_CPO        (COREIMPUR        )          

     call perfoff

  END DO TIME_LOOP1
  call perfoff

!
! the computation has finished, we just need to clean up
!
! we need to
!   deallocate unneeded structures
!   finalize any I/O
!

  call perfon ('dealloc')

  CALL DEALLOCATE_RUN_CONTROL  (CONTROL,    ifail)
  CALL DEALLOCATE_CPO          (EQUILIBRIUM_OLD  )         
  CALL DEALLOCATE_CPO          (COREPROF_OLD     )
  call deallocate_CPO          (code_parameters  )
  call deallocate_CPO          (ets_parameters  )
  call perfoff


#ifdef UAL
  if(shot_no.gt.0.and.run_no.ge.0) then
     call perfon ('ualc')
     call euitm_close(idx)
     call perfoff
  endif
#endif

  call perfoff

  call perfout ('whole')


END PROGRAM SOLVER_TEST
