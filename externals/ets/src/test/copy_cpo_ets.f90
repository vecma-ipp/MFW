! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Module provides routines for copying parts of CPOs (COREPROF and EQUILIBRIUM)
!>
!> \author ???
!>
!> \version "$Id: copy_cpo_ets.f90 1616 2014-10-07 14:40:14Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
MODULE COPY_CPO_ETS

CONTAINS


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> 
!>
!> \author ???
!>
!> \version "$Id: copy_cpo_ets.f90 1616 2014-10-07 14:40:14Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE COPY_BOUNDARY_COND (COREPROF_IN, COREPROF_OUT) 

    USE EUITM_SCHEMAS

    IMPLICIT NONE


    TYPE (TYPE_COREPROF), POINTER  :: COREPROF_IN(:)    
    TYPE (TYPE_COREPROF), POINTER  :: COREPROF_OUT(:)  


    COREPROF_OUT(1)%psi%boundary%value      = COREPROF_IN(1)%psi%boundary%value
    COREPROF_OUT(1)%psi%boundary%type       = COREPROF_IN(1)%psi%boundary%type 

    COREPROF_OUT(1)%ni%boundary%value       = COREPROF_IN(1)%ni%boundary%value 
    COREPROF_OUT(1)%ni%boundary%type        = COREPROF_IN(1)%ni%boundary%type

    COREPROF_OUT(1)%ti%boundary%value       = COREPROF_IN(1)%ti%boundary%value 
    COREPROF_OUT(1)%ti%boundary%type        = COREPROF_IN(1)%ti%boundary%type

    COREPROF_OUT(1)%te%boundary%value       = COREPROF_IN(1)%te%boundary%value
    COREPROF_OUT(1)%te%boundary%type        = COREPROF_IN(1)%te%boundary%type 

    COREPROF_OUT(1)%vtor%boundary%value     = COREPROF_IN(1)%vtor%boundary%value 
    COREPROF_OUT(1)%vtor%boundary%type      = COREPROF_IN(1)%vtor%boundary%type 


    RETURN

  END SUBROUTINE COPY_BOUNDARY_COND


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> COPY CODEPARAM
!>
!> \author D. Coster
!>
!> \version "$Id: copy_cpo_ets.f90 1616 2014-10-07 14:40:14Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE COPY_CODEPARAM (CODEPARAM_IN, CODEPARAM_OUT) 

    USE EUITM_SCHEMAS

    IMPLICIT NONE


    TYPE (TYPE_CODEPARAM)  :: CODEPARAM_IN    
    TYPE (TYPE_CODEPARAM)  :: CODEPARAM_OUT 

    if(associated(codeparam_out%codename)) then
!       write(*,*) 'copy_codeparam dealloc codename'
       deallocate(codeparam_out%codename)
    endif
    allocate(codeparam_out%codename(size(codeparam_in%codename)))
    codeparam_out%codename = codeparam_in%codename

    if(associated(codeparam_out%codeversion)) then
!       write(*,*) 'copy_codeparam dealloc codeversion'
       deallocate(codeparam_out%codeversion)
    endif
    allocate(codeparam_out%codeversion(size(codeparam_in%codeversion)))
    codeparam_out%codeversion = codeparam_in%codeversion

    if(associated(codeparam_out%parameters)) then
!       write(*,*) 'copy_codeparam dealloc parameters'
       deallocate(codeparam_out%parameters)
    endif
    allocate(codeparam_out%parameters(size(codeparam_in%parameters)))
    codeparam_out%parameters = codeparam_in%parameters
!!!    codeparam_out%output_diag = codeparam_in%output_diag
    codeparam_out%output_flag = codeparam_in%output_flag

    RETURN

  END SUBROUTINE COPY_CODEPARAM

END MODULE COPY_CPO_ETS
