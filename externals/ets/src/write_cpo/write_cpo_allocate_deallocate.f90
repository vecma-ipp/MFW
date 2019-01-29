! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This module contains routines for
!> allocation/deallocation if CPOs used in ETS
!>
!> \author D.Kalupin ,chnage by Irena
!>
!> \version "$Id: write_cpo_allocate_deallocate.f90 1531 2013-08-09 11:02:11Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
MODULE WRITE_CPO_ALLOCATE_DEALLOCATE
!-------------------------------------------------------!  
!                                                       !
!     This module contains routines for                 !
!     allocation/deallocation if CPOs used in ETS       !
!                                                       !
!-------------------------------------------------------!  
!                                                       !
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!  

CONTAINS

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This routine allocates COREPROF CPO
!>
!> \author D.Kalupin
!>
!> \version "$Id: write_cpo_allocate_deallocate.f90 1531 2013-08-09 11:02:11Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE ALLOCATE_COREPROF_CPO (NSLICE, NRHO, NION, COREPROF)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates COREPROF CPO               !
!                                                       !
!-------------------------------------------------------!  
!                                                       !
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!  


    USE EUITM_SCHEMAS

    IMPLICIT NONE

    INTEGER                          :: NSLICE, ISLICE      !number of slices and slice index
    INTEGER                          :: NRHO                !number of radial points    (input)
    INTEGER                          :: NION                !number of ion species      (input)

    TYPE (TYPE_COREPROF),    POINTER :: COREPROF(:)         !CPO with internal ETS parameters profiles 


    ALLOCATE (COREPROF(NSLICE))

 
    DO ISLICE = 1,NSLICE

! +++ Radial coordinate:
       ALLOCATE (COREPROF(ISLICE)%rho_tor(NRHO))
 


! +++ Plasma composition:
       ALLOCATE (COREPROF(ISLICE)%composition%zion(NION))
       ALLOCATE (COREPROF(ISLICE)%composition%amn(NION))
       ALLOCATE (COREPROF(ISLICE)%composition%zn(NION))


! +++ Boundary conditions:
       ALLOCATE (COREPROF(ISLICE)%ni%flag(NION))
       ALLOCATE (COREPROF(ISLICE)%ti%flag(NION))
       ALLOCATE (COREPROF(ISLICE)%vtor%flag(NION))

       ALLOCATE (COREPROF(ISLICE)%ni%boundary%type(NION))
       ALLOCATE (COREPROF(ISLICE)%ti%boundary%type(NION))
       ALLOCATE (COREPROF(ISLICE)%vtor%boundary%type(NION))

       ALLOCATE (COREPROF(ISLICE)%ni%boundary%rho_tor(NION))
       ALLOCATE (COREPROF(ISLICE)%ti%boundary%rho_tor(NION))
       ALLOCATE (COREPROF(ISLICE)%vtor%boundary%rho_tor(NION))

       ALLOCATE (COREPROF(ISLICE)%psi%boundary%value(3))
       ALLOCATE (COREPROF(ISLICE)%ni%boundary%value(3,NION))
       ALLOCATE (COREPROF(ISLICE)%ne%boundary%value(3))
       ALLOCATE (COREPROF(ISLICE)%ti%boundary%value(3,NION))
       ALLOCATE (COREPROF(ISLICE)%te%boundary%value(3))
       ALLOCATE (COREPROF(ISLICE)%vtor%boundary%value(3,NION))


! +++ Plasma primary profiles:
       ALLOCATE (COREPROF(ISLICE)%psi%value(NRHO))
       ALLOCATE (COREPROF(ISLICE)%ni%value(NRHO,NION))
       ALLOCATE (COREPROF(ISLICE)%ne%value(NRHO))
       ALLOCATE (COREPROF(ISLICE)%ti%value(NRHO,NION))
       ALLOCATE (COREPROF(ISLICE)%te%value(NRHO))
       ALLOCATE (COREPROF(ISLICE)%vtor%value(NRHO,NION))


! +++ Plasma derived profiles:
       allocate (COREPROF(ISLICE)%psi%sigma_par%value(NRHO))
       ALLOCATE (COREPROF(ISLICE)%profiles1d%q%value(NRHO))
       ALLOCATE (COREPROF(ISLICE)%profiles1d%zeff%value(NRHO))
       ALLOCATE (COREPROF(ISLICE)%profiles1d%jtot%value(NRHO))
       ALLOCATE (COREPROF(ISLICE)%profiles1d%zeff%value(NRHO))
       ALLOCATE (COREPROF(ISLICE)%profiles1d%pr_th%value(NRHO))
       ALLOCATE (COREPROF(ISLICE)%profiles1d%pr_perp%value(NRHO))
       ALLOCATE (COREPROF(ISLICE)%profiles1d%pr_parallel%value(NRHO))
       ALLOCATE (COREPROF(ISLICE)%profiles1d%pe%value(NRHO))
       ALLOCATE (COREPROF(ISLICE)%profiles1d%pi%value(NRHO,NION))



    END DO



    RETURN

  END SUBROUTINE ALLOCATE_COREPROF_CPO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  SUBROUTINE ALLOCATE_CORETRANSP_CPO (NSLICE, NRHO, NION, NIMP, NZIMP, CORETRANSP)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates CORETRANSP CPO             !
!                                                       !
!-------------------------------------------------------!  
!                                                       !
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!  


    USE EUITM_SCHEMAS
    USE ITM_TYPES

    IMPLICIT NONE

    INTEGER                          :: NSLICE, ISLICE      !number of slices and slice index
    INTEGER                          :: NRHO                !number of radial points    (input)
    INTEGER                          :: NION                !number of ion species      (input)
    INTEGER                          :: NIMP, IIMP          !number of impurity species (input)
    INTEGER                          :: NZIMP(:)            !number of charge states for each impurity (input)

    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP(:)       !CPO with transport coefficients



    ALLOCATE (CORETRANSP(NSLICE))


    DO ISLICE = 1,NSLICE

       ALLOCATE (CORETRANSP(ISLICE)%VALUES(1))

! +++ Radial coordinate:
       ALLOCATE (CORETRANSP(ISLICE)%VALUES(1)%rho_tor(NRHO))


! +++ Plasma composition:
       ALLOCATE (CORETRANSP(ISLICE)%composition%zion(NION))
       ALLOCATE (CORETRANSP(ISLICE)%composition%amn(NION))
       ALLOCATE (CORETRANSP(ISLICE)%composition%zn(NION))


! +++ Profiles of transport coefficients:
       ALLOCATE (CORETRANSP(ISLICE)%VALUES(1)%sigma(NRHO))
       ALLOCATE (CORETRANSP(ISLICE)%VALUES(1)%ne_transp%diff_eff(NRHO,3))
       ALLOCATE (CORETRANSP(ISLICE)%VALUES(1)%ne_transp%vconv_eff(NRHO,3))
       ALLOCATE (CORETRANSP(ISLICE)%VALUES(1)%ni_transp%diff_eff(NRHO,NION,3))
       ALLOCATE (CORETRANSP(ISLICE)%VALUES(1)%ni_transp%vconv_eff(NRHO,NION,3))
       ALLOCATE (CORETRANSP(ISLICE)%VALUES(1)%ti_transp%diff_eff(NRHO,NION))
       ALLOCATE (CORETRANSP(ISLICE)%VALUES(1)%ti_transp%vconv_eff(NRHO,NION))
!        ALLOCATE (CORETRANSP(ISLICE)%VALUES(1)%ti_transp%qgi(NRHO,NION))
       ALLOCATE (CORETRANSP(ISLICE)%VALUES(1)%te_transp%diff_eff(NRHO))
       ALLOCATE (CORETRANSP(ISLICE)%VALUES(1)%te_transp%vconv_eff(NRHO))
       ALLOCATE (CORETRANSP(ISLICE)%VALUES(1)%vtor_transp%diff_eff(NRHO,NION))
       ALLOCATE (CORETRANSP(ISLICE)%VALUES(1)%vtor_transp%vconv_eff(NRHO,NION))

!       if(nimp.gt.0) then
          ALLOCATE (CORETRANSP(ISLICE)%values(1)%nz_transp(nimp))
          ALLOCATE (CORETRANSP(ISLICE)%values(1)%tz_transp(nimp))
          do iimp=1, nimp
             ALLOCATE (CORETRANSP(ISLICE)%values(1)%nz_transp(iimp)%diff_eff(NRHO,nzimp(iimp)))
             CORETRANSP(ISLICE)%values(1)%nz_transp(iimp)%diff_eff(:,:) = 0.0_R8
             ALLOCATE (CORETRANSP(ISLICE)%values(1)%nz_transp(iimp)%vconv_eff(NRHO,nzimp(iimp)))
             CORETRANSP(ISLICE)%values(1)%nz_transp(iimp)%vconv_eff(:,:) = 0.0_R8
             ALLOCATE (CORETRANSP(ISLICE)%values(1)%tz_transp(iimp)%diff_eff(NRHO,nzimp(iimp)))
             CORETRANSP(ISLICE)%values(1)%tz_transp(iimp)%diff_eff(:,:) = 0.0_R8
             ALLOCATE (CORETRANSP(ISLICE)%values(1)%tz_transp(iimp)%vconv_eff(NRHO,nzimp(iimp)))
             CORETRANSP(ISLICE)%values(1)%tz_transp(iimp)%vconv_eff(:,:) = 0.0_R8
          enddo
!       endif

    END DO

    RETURN

  END SUBROUTINE ALLOCATE_CORETRANSP_CPO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  SUBROUTINE ALLOCATE_CORESOURCE_CPO (NSLICE, NRHO, NION, NIMP, NZIMP, CORESOURCE)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates CORESOURCE CPO             !
!                                                       !
!-------------------------------------------------------!  
!                                                       !
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!  


    USE EUITM_SCHEMAS

    IMPLICIT NONE

    INTEGER                          :: NSLICE, ISLICE      !number of slices and slice index
    INTEGER                          :: NRHO                !number of radial points    (input)
    INTEGER                          :: NION                !number of ion species      (input)
    INTEGER                          :: NIMP, IIMP          !number of radial points    (input)
    INTEGER, allocatable             :: NZIMP(:)            !number of ion species      (input)

    TYPE (TYPE_CORESOURCE),  POINTER :: CORESOURCE(:)       !CPO with sources



    ALLOCATE (CORESOURCE(NSLICE))


    DO ISLICE = 1,NSLICE

       ALLOCATE(CORESOURCE(ISLICE)%VALUES(1))

! +++ Radial coordinate:
       ALLOCATE (CORESOURCE(ISLICE)%values(1)%rho_tor(NRHO))


! +++ Plasma composition:
       ALLOCATE (CORESOURCE(ISLICE)%composition%zion(NION))
       ALLOCATE (CORESOURCE(ISLICE)%composition%amn(NION))
       ALLOCATE (CORESOURCE(ISLICE)%composition%zn(NION))


! +++ Profiles of sources:
       ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%sigma(NRHO))
       ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%j(NRHO))
       ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%qe%exp(NRHO))
       ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%qe%imp(NRHO))
       ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%si%exp(NRHO,NION))
       ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%si%imp(NRHO,NION))
       ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%qi%exp(NRHO,NION))
       ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%qi%imp(NRHO,NION))
       ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%ui%exp(NRHO,NION))
       ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%ui%imp(NRHO,NION))

 !      if(nimp.gt.0) then
          ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%sz(NIMP))
          ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%qz(NIMP))
          do iimp=1, nimp
             ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%sz(iimp)%exp(NRHO,NZIMP(IIMP)))
             ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%sz(iimp)%imp(NRHO,NZIMP(IIMP)))  
             ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%qz(iimp)%exp(NRHO,NZIMP(IIMP)))
             ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%qz(iimp)%imp(NRHO,NZIMP(IIMP)))
          enddo
!       endif
    END DO


    RETURN

  END SUBROUTINE ALLOCATE_CORESOURCE_CPO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  SUBROUTINE ALLOCATE_compositionc_CPO (NUCL,  NNEUT, NTYPE, NCOMP, NION, NIMP, NZIMP, COMPOSITIONC)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates COMPOSITIONC CPO           !
!                                                       !
!-------------------------------------------------------!  
!                                                       !
!     Source:       ---                                 !
!     Developers:   I.Ivanova-Stanik                    !
!     Kontacts:     irena@ifpilm.waw.pl                 !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!  


    USE EUITM_SCHEMAS

    IMPLICIT NONE

!!    INTEGER                         :: NSLICE, ISLICE      !number of slices and slice index
    INTEGER                           :: NUCL, INUCL         !number of NUCLEI    (input)
    INTEGER                           :: NION,IION           !number of ion species      (input)
    INTEGER                           :: NIMP, IIMP          !number of radial points    (input)
    INTEGER,ALLOCATABLE               :: NZIMP(:)
    INTEGER                           :: NCOMP(:), INCOMP
    INTEGER                           :: NNEUT, INEUT        !number of neutrals species                (input)
    INTEGER, allocatable              :: NTYPE(:),intype     !number of impurity ionization states (input)     

   
    TYPE (TYPE_COMPOSITIONC),  POINTER :: COMPOSITIONC       !CPO with sources



    ALLOCATE (COMPOSITIONC)

       ALLOCATE(COMPOSITIONC%compositions%nuclei(NUCL))
       ALLOCATE(COMPOSITIONC%compositions%IONS(NION))
       ALLOCATE(COMPOSITIONC%compositions%IMPURITIES(NIMP))
       ALLOCATE(COMPOSITIONC%compositions%NEUTRALSCOMP(NNEUT))
     
 
! +++ Nuclei parameters:
      do inucl=1, nucl
          ALLOCATE(COMPOSITIONC%compositions%nuclei(INUCL)%label(INUCL))
 	  COMPOSITIONC%compositions%nuclei(INUCL)%zn               = 0
	  COMPOSITIONC%compositions%nuclei(INUCL)%amn              = 0
	  
      enddo



! +++ Ions parameters:
       do iion=1, nion  
	  COMPOSITIONC%compositions%IONS(IION)%nucindex             = 0
	  COMPOSITIONC%compositions%IONS(IION)%zion                 = 0
	  COMPOSITIONC%compositions%IONS(IION)%imp_flag             = 0
	  
       enddo

! +++ Impurity parameters:
       do iimp=1, nimp
           ALLOCATE(COMPOSITIONC%compositions%IMPURITIES(IIMP)%label(nzimp(iimp)))
	  
	  COMPOSITIONC%compositions%IMPURITIES(IIMP)%nucindex           = 0
	  COMPOSITIONC%compositions%IMPURITIES(IIMP)%i_ion 	        = 0
	  COMPOSITIONC%compositions%IMPURITIES(IIMP)%nzimp	        = 0
	  COMPOSITIONC%compositions%IMPURITIES(IIMP)%zmin               = 0

      enddo  

! +++ Neutrals parameters:
       do ineut=1, nneut
              
           ALLOCATE(COMPOSITIONC%compositions%NEUTRALSCOMP(INEUT)%TYPE(NTYPE(INEUT)))
           ALLOCATE(COMPOSITIONC%compositions%NEUTRALSCOMP(NNEUT)%NEUTCOMP(NCOMP(ineut)))

  !         COMPOSITIONC%compositions%NEUTRALSCOMP(INEUT)%TYPE(ntype(ineut))                   = 0
  !	   COMPOSITIONC%compositions%NEUTRALSCOMP(INEUT)%NEUTCOMP(NCOMP(ineut))                = 0
  !	   COMPOSITIONC%compositions%NEUTRALSCOMP(INEUT)%NEUTCOMP(NCOMP(ineut))%multiplicity     = 0 
		
       enddo
    
    
    RETURN

   END SUBROUTINE ALLOCATE_COMPOSITIONC_CPO


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This routine allocates COREIMPUR CPO
!>
!> \author D.Kalupin, I.Ivanova-Stanik 
!>
!> \version "$Id: write_cpo_allocate_deallocate.f90 1531 2013-08-09 11:02:11Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE ALLOCATE_COREIMPUR_CPO (NSLICE, NRHO, NIMP, NZIMP, COREIMPUR)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates COREIMPUR CPO              !
!                                                       !
!-------------------------------------------------------!  
!                                                       !
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                   irena@ifpilm.waw.pl                                    !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!  


    USE EUITM_SCHEMAS
    USE ITM_TYPES

    IMPLICIT NONE

    INTEGER                          :: NSLICE, ISLICE      !number of slices and slice index
    INTEGER                          :: NRHO                !number of radial points              (input)
    INTEGER                          :: NIMP, IIMP          !number of ion species                (input)
    INTEGER, allocatable             :: NZIMP(:)            !number of impurity ionization states (input)
    
    TYPE (TYPE_COREIMPUR),   POINTER :: COREIMPUR(:)        !CPO with impurities



    ALLOCATE (COREIMPUR(NSLICE))


    DO ISLICE = 1,NSLICE

       ALLOCATE (COREIMPUR(ISLICE)%rho_tor(NRHO))
       COREIMPUR(ISLICE)%rho_tor(:)              = 0.0_R8
       ALLOCATE (COREIMPUR(ISLICE)%desc_impur%amn(NIMP))
       COREIMPUR(ISLICE)%desc_impur%amn(:)       = 0.0_R8
       ALLOCATE (COREIMPUR(ISLICE)%desc_impur%nzimp(NIMP))
       COREIMPUR(ISLICE)%desc_impur%nzimp(:)     = 0
       ALLOCATE (COREIMPUR(ISLICE)%desc_impur%zn(NIMP))
       COREIMPUR(ISLICE)%desc_impur%zn(:)        = 0
       ALLOCATE (COREIMPUR(ISLICE)%desc_impur%zmin(NIMP,MAXVAL(NZIMP)))
       COREIMPUR(ISLICE)%desc_impur%zmin(:,:)    = 0
       ALLOCATE (COREIMPUR(ISLICE)%desc_impur%zmax(NIMP,MAXVAL(NZIMP)))
       COREIMPUR(ISLICE)%desc_impur%zmax(:,:)    = 0

       ALLOCATE (COREIMPUR(ISLICE)%IMPURITY(NIMP))

       do iimp=1, nimp
          ALLOCATE (COREIMPUR(ISLICE)%IMPURITY(IIMP)%nz(NRHO,NZIMP(IIMP)))
          COREIMPUR(ISLICE)%IMPURITY(IIMP)%nz                      = 0.0_R8
          ALLOCATE (COREIMPUR(ISLICE)%IMPURITY(IIMP)%flux%flux_dv(NRHO,NZIMP(IIMP)))
          COREIMPUR(ISLICE)%IMPURITY(IIMP)%flux%flux_dv            = 0.0_R8
          ALLOCATE (COREIMPUR(ISLICE)%IMPURITY(IIMP)%flux%flux_interp(NRHO,NZIMP(IIMP)))
          COREIMPUR(ISLICE)%IMPURITY(IIMP)%flux%flux_interp        = 0.0_R8
          ALLOCATE (COREIMPUR(ISLICE)%IMPURITY(IIMP)%z(NRHO,NZIMP(IIMP)))
          COREIMPUR(ISLICE)%IMPURITY(IIMP)%z                       = 0.0_R8
          ALLOCATE (COREIMPUR(ISLICE)%IMPURITY(IIMP)%zsq(NRHO,NZIMP(IIMP)))
          COREIMPUR(ISLICE)%IMPURITY(IIMP)%zsq                     = 0.0_R8
          ALLOCATE (COREIMPUR(ISLICE)%IMPURITY(IIMP)%boundary%value(3,NZIMP(IIMP)))
          COREIMPUR(ISLICE)%IMPURITY(IIMP)%boundary%value          = 0.0_R8
          ALLOCATE (COREIMPUR(ISLICE)%IMPURITY(IIMP)%boundary%type(NZIMP(IIMP)))
          COREIMPUR(ISLICE)%IMPURITY(IIMP)%boundary%type           = 0
          ALLOCATE (COREIMPUR(ISLICE)%IMPURITY(IIMP)%boundary%rho(NZIMP(IIMP)))
          COREIMPUR(ISLICE)%IMPURITY(IIMP)%boundary%rho            = 0.0_R8
          ALLOCATE (COREIMPUR(ISLICE)%IMPURITY(IIMP)%transp_coef%diff(NRHO,NZIMP(IIMP)))
          COREIMPUR(ISLICE)%IMPURITY(IIMP)%transp_coef%diff        = 0.0_R8
          ALLOCATE (COREIMPUR(ISLICE)%IMPURITY(IIMP)%transp_coef%vconv(NRHO,NZIMP(IIMP)))
          COREIMPUR(ISLICE)%IMPURITY(IIMP)%transp_coef%vconv       = 0.0_R8
          ALLOCATE (COREIMPUR(ISLICE)%IMPURITY(IIMP)%source_term%value(NRHO,NZIMP(IIMP)))
          COREIMPUR(ISLICE)%IMPURITY(IIMP)%source_term%value       = 0.0_R8
       enddo


    END DO

    RETURN

  END SUBROUTINE ALLOCATE_COREIMPUR_CPO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

! irena the change for neutrals

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  SUBROUTINE ALLOCATE_CORENEUTRALS_CPO (NSLICE, NRHO, NATM, NNEUT, NTYPE, NCOMP, CORENEUTRALS)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates CORENEUTRALS CPO              !
!                                                       !
!-------------------------------------------------------!  
!                                                       !
!     Source:       ---                                 !
!     Developers:   I.Ivanova-Stanik,R.Stankiewicz      !
!     Kontacts:     irena@ifpilm.waw.pl                 !
!                   romsta@ifpilm.waw.pl                !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!  


    USE EUITM_SCHEMAS
    USE ITM_TYPES

    IMPLICIT NONE

    INTEGER                          :: NSLICE, ISLICE      !number of slices and slice index
    INTEGER                          :: NRHO                !number of radial points              (input)
    INTEGER                          :: NATM, IATM          !number of atoms                      (input)
    INTEGER                          :: NNEUT, INEUT        !number of neutrals species                (input)
    INTEGER, allocatable             :: NTYPE(:)            !number of impurity ionization states (input)     
    INTEGER, allocatable             :: NCOMP(:)            !number of distinct atoms enter the composition-"1" wich is neutral
    INTEGER                          :: ICOMP, ITYPE
      
    TYPE (TYPE_CORENEUTRALS),   POINTER :: CORENEUTRALS(:)        !CPO with impurities



    ALLOCATE (CORENEUTRALS(NSLICE))


    DO ISLICE = 1,NSLICE

       ALLOCATE (CORENEUTRALS(ISLICE)%rho_tor(NRHO))
       CORENEUTRALS(ISLICE)%rho_tor(:)                                        = 0.0_R8	
       ALLOCATE (CORENEUTRALS(ISLICE)%rho_tor_norm(NRHO))
       CORENEUTRALS(ISLICE)%rho_tor_norm(:)                                   = 0.0_R8	

       if(natm.gt.0) then
          ALLOCATE (CORENEUTRALS(ISLICE)%neutcompo%atomlist(natm))
          do iatm = 1, natm
             CORENEUTRALS(ISLICE)%neutcompo%atomlist(iatm)%amn                = 0.0_R8
             CORENEUTRALS(ISLICE)%neutcompo%atomlist(iatm)%zn                 = 0.0_R8
          enddo
       endif
       if(nneut.gt.0) then
          ALLOCATE (CORENEUTRALS(ISLICE)%neutcompo%neutral(nneut))
          allocate (CORENEUTRALS(ISLICE)%profiles(nneut))
          do ineut = 1, nneut
             ALLOCATE (CORENEUTRALS(ISLICE)%neutcompo%neutral(ineut)%neutcomp(ncomp(ineut)))
             do icomp = 1, ncomp(ineut)
                CORENEUTRALS(ISLICE)%neutcompo%neutral(ineut)%neutcomp(ineut)%nucindex           = 0
                CORENEUTRALS(ISLICE)%neutcompo%neutral(ineut)%neutcomp(icomp)%multiplicity       = 0
             enddo
             ALLOCATE (CORENEUTRALS(ISLICE)%neutcompo%neutral(ineut)%type(ntype(ineut)))
             allocate (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(ntype(ineut)))
             do itype = 1, ntype(ineut)
                CORENEUTRALS(ISLICE)%neutcompo%neutral(ineut)%type(itype)%flag                  = 0
                ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%n0%value(NRHO))
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%n0%value(:)                   = 0.0_R8
                ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%n0%flux(NRHO))
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%n0%flux(:)                    = 0.0_R8
                ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%n0%boundary%value(3))
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%n0%boundary%value(:)          = 0.0_R8
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%n0%boundary%type              = 0
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%n0%boundary%rho_tor           = 0.0_R8
                ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%t0%value(NRHO))
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%t0%value(:)                   = 0.0_R8
                ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%t0%flux(NRHO))
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%t0%flux(:)                    = 0.0_R8
                ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%t0%boundary%value(3))
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%t0%boundary%value(:)          = 0.0_R8
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%t0%boundary%type              = 0
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%t0%boundary%rho_tor           = 0.0_R8
                ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%toroidal%value(NRHO))
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%toroidal%value(:)          = 0.0_R8
                ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%toroidal%boundary%value(3))
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%toroidal%boundary%value(:) = 0.0_R8
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%toroidal%boundary%type     = 0
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%toroidal%boundary%rho_tor  = 0.0_R8
                ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%poloidal%value(NRHO))
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%poloidal%value(:)          = 0.0_R8
                ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%poloidal%boundary%value(3))
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%poloidal%boundary%value(:) = 0.0_R8
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%poloidal%boundary%type     = 0
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%poloidal%boundary%rho_tor  = 0.0_R8
                ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%radial%value(NRHO))
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%radial%value(:)            = 0.0_R8
                ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%radial%boundary%value(3))
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%radial%boundary%value(:)   = 0.0_R8
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%radial%boundary%type       = 0
                CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%radial%boundary%rho_tor    = 0.0_R8
             enddo
          enddo
       endif
    END DO

    RETURN

  END SUBROUTINE ALLOCATE_CORENEUTRALS_CPO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  SUBROUTINE ALLOCATE_EQUILIBRIUM_CPO (NSLICE, NRHO, EQUILIBRIUM)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates EQUILIBRIUM CPO            !
!                                                       !
!-------------------------------------------------------!  
!                                                       !
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!  


    USE EUITM_SCHEMAS

    IMPLICIT NONE

    INTEGER                          :: NSLICE, ISLICE      !number of slices and slice index
    INTEGER                          :: NRHO                !number of radial points    (input)
    INTEGER                          :: ntheta=101          !number of poloidal points

    TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM(:)      !CPO with geometry quantities



    ALLOCATE (EQUILIBRIUM(NSLICE))


    DO ISLICE = 1,NSLICE

! +++ Equilibrium quantities:
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%rho_tor(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%q(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%pressure(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%jparallel(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%jphi(NRHO))

       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm1(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm2(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm3(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm4(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm5(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm6(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm7(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm8(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm9(NRHO))
         
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%volume(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%vprime(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%area(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%aprime(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%F_dia(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%pprime(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%ffprime(NRHO))
         
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%elongation(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%tria_upper(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%tria_lower(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%r_inboard(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%r_outboard(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%dpsidrho_tor(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%beta_pol(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%li(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%dvdrho(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%surface(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%ftrap(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%b_av(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%b_min(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%b_max(NRHO))
         
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%phi(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%psi(NRHO))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%rho_vol(NRHO))

       allocate(EQUILIBRIUM(1)%coord_sys%position%R(nrho, ntheta))
       allocate(EQUILIBRIUM(1)%coord_sys%position%Z(nrho, ntheta))

       allocate(EQUILIBRIUM(1)%eqgeometry%boundary(1))
       allocate(EQUILIBRIUM(1)%eqgeometry%boundary(1)%r(ntheta))  ! -1 bug in helena
       allocate(EQUILIBRIUM(1)%eqgeometry%boundary(1)%z(ntheta))  ! -1 bug in helena
         
    END DO
      
    RETURN

  END SUBROUTINE ALLOCATE_EQUILIBRIUM_CPO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  SUBROUTINE ALLOCATE_TOROIDFIELD_CPO (NSLICE, TOROIDFIELD)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates TOROIDFIELD CPO            !
!                                                       !
!-------------------------------------------------------!  
!                                                       !
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!  


    USE EUITM_SCHEMAS

    IMPLICIT NONE

    INTEGER                          :: NSLICE, ISLICE      !number of slices and slice index

    TYPE (TYPE_TOROIDFIELD), POINTER :: TOROIDFIELD(:)      !CPO with geometry quantities



    ALLOCATE (TOROIDFIELD(NSLICE))


    RETURN

  END SUBROUTINE ALLOCATE_TOROIDFIELD_CPO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
END MODULE WRITE_CPO_ALLOCATE_DEALLOCATE
