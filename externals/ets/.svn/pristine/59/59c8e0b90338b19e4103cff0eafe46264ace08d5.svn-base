program neo_test

  use itm_constants
  USE EUITM_SCHEMAS
  use euitm_routines
  use read_structures
  use write_structures
  use deallocate_structures

  implicit none

  type(type_coreprof), pointer       :: coreprof(:)
  type(type_coretransp), pointer     :: coretransp(:)
  type(type_coresource), pointer     :: coresource(:)
  type(type_coreimpur), pointer      :: coreimpur(:)
  type(type_equilibrium), pointer    :: equilibrium(:)
  type(type_toroidfield), pointer    :: toroidfield(:)
  type(type_neoclassic), pointer     :: neoclassic_neo(:), neoclassic_neowes(:), neoclassic_itmneoart(:), neoclassic_neos(:)

  integer                            :: idx, shot, run, interpol, nrho, irho, nion, iion
  REAL (R8)                          :: time
  character(len=17)                  :: filename
  logical                            :: exist

  INTERFACE
     SUBROUTINE NEO(EQUILIBRIUM,COREPROF,NEOCLASSIC)
       USE EUITM_SCHEMAS
       USE ITM_TYPES
       TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM(:)
       TYPE (TYPE_COREPROF),    POINTER :: COREPROF(:)  
       TYPE (TYPE_NEOCLASSIC),  POINTER :: NEOCLASSIC(:)
     END SUBROUTINE NEO
  END INTERFACE

  INTERFACE
     SUBROUTINE NEOWES(EQUILIBRIUM,COREPROF,NEOCLASSIC)
       USE EUITM_SCHEMAS
       USE ITM_TYPES
       TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM(:)
       TYPE (TYPE_COREPROF),    POINTER :: COREPROF(:)  
       TYPE (TYPE_NEOCLASSIC),  POINTER :: NEOCLASSIC(:)
     END SUBROUTINE NEOWES
  END INTERFACE

#ifdef GOT_NEOART
  INTERFACE
     SUBROUTINE ITMNEOART(EQUILIBRIUM,COREPROF,NEOCLASSIC)
       USE EUITM_SCHEMAS
       USE ITM_TYPES
       TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM(:)
       TYPE (TYPE_COREPROF),    POINTER :: COREPROF(:)  
       TYPE (TYPE_NEOCLASSIC),  POINTER :: NEOCLASSIC(:)
     END SUBROUTINE ITMNEOART
  END INTERFACE
#endif

#ifdef GOT_NEOS
  INTERFACE
     SUBROUTINE SIGNEOJBS(EQUILIBRIUM,COREPROF,NEOCLASSIC)
       USE EUITM_SCHEMAS
       USE ITM_TYPES
       TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM(:)
       TYPE (TYPE_COREPROF),    POINTER :: COREPROF(:)  
       TYPE (TYPE_NEOCLASSIC),  POINTER :: NEOCLASSIC(:)
     END SUBROUTINE SIGNEOJBS
  END INTERFACE
#endif

!  shot = 5 ; run = 45  ! BDSEQ, nion=1, nimp=0
!  shot = 5 ; run = 47  ! HELENA, nion=1, nimp=0
!  shot = 5 ; run = 63  ! BDSEQ, nion=4, nimp=0
!  shot = 5 ; run = 64  ! HELENA, nion=4, nimp=0
!  shot = 5 ; run = 123 ! EMEQ, nion=1, nimp=1, W
!  shot = 5 ; run = 142 ! cyl, nion=1, nimp=0 (H)
  shot = 5 ; run = 143 ! bdseq, nion=1, nimp=0 (H)
!  shot = 5 ; run = 144 ! emeq, nion=1, nimp=0 (H)
!  shot = 5 ; run = 145 ! helena, nion=1, nimp=0 (H)

  time = 10.0_R8

  interpol = 1

  allocate(COREPROF(1),CORETRANSP(1),CORESOURCE(1),  &
       COREIMPUR(1),EQUILIBRIUM(1),TOROIDFIELD(1))
#ifdef UAL
  CALL EUITM_OPEN       ('euitm', SHOT, RUN, IDX)
  CALL EUITM_GET_SLICE  (IDX, 'coreprof', COREPROF(1), TIME, INTERPOL)
  CALL EUITM_GET_SLICE  (IDX, 'equilibrium', EQUILIBRIUM(1), TIME, INTERPOL)
  CALL EUITM_GET_SLICE  (IDX, 'toroidfield', TOROIDFIELD(1), TIME, INTERPOL)
! the following are not needed for calling the neoclassical routines
! but are needed if we want to write out the ASCII version
  CALL EUITM_GET_SLICE  (IDX, 'coretransp', CORETRANSP(1), TIME, INTERPOL)
  CALL EUITM_GET_SLICE  (IDX, 'coresource', CORESOURCE(1), TIME, INTERPOL)
  CALL EUITM_GET_SLICE  (IDX, 'coreimpur', COREIMPUR(1), TIME, INTERPOL)
! write out an ASCII version if one doesn't already exist 
  write(filename,'(''CPO_'',I6.6,''_'',I6.6)') shot, run
  inquire(file=filename, exist=exist)
  if(exist) then
     write(*,*) filename,' already exists'
  else
     call open_write_file(1, filename)
     call write_cpo(coreprof(1), 'coreprof')
     call write_cpo(coretransp(1), 'coretransp')
     call write_cpo(coresource(1), 'coresource')
     call write_cpo(coreimpur(1), 'coreimpur')
     call write_cpo(equilibrium(1), 'equilibrium')
     call write_cpo(toroidfield(1), 'toroidfield')
     call close_write_file
  endif
#else
  write(filename,'(''CPO_'',I6.6,''_'',I6.6)') shot, run
  call open_read_file(1, filename)
  call read_cpo(coreprof(1), 'coreprof')
  call read_cpo(coretransp(1), 'coretransp')
  call read_cpo(coresource(1), 'coresource')
  call read_cpo(coreimpur(1), 'coreimpur')
  call read_cpo(equilibrium(1), 'equilibrium')
  call read_cpo(toroidfield(1), 'toroidfield')
  call close_read_file
#endif

  nrho=size(COREPROF(1)%rho_tor)
  nion=size(COREPROF(1)%ni%value,dim=2)
  allocate(COREPROF(1)%rho_tor_norm(nrho))
  COREPROF(1)%rho_tor_norm = COREPROF(1)%rho_tor / COREPROF(1)%rho_tor(nrho)
  equilibrium(1)%eqgeometry%elongation=1.0_R8
  if(.not.associated(coreprof(1)%profiles1d%pe%value)) then
     write(*,*) 'Calculating coreprof(1)%profiles1d%pe%value'
     allocate(coreprof(1)%profiles1d%pe%value(nrho))
     coreprof(1)%profiles1d%pe%value = coreprof(1)%te%value * coreprof(1)%ne%value * itm_ev
  endif
  if(.not.associated(coreprof(1)%profiles1d%pi%value)) then
     write(*,*) 'Calculating coreprof(1)%profiles1d%pi%value'
     allocate(coreprof(1)%profiles1d%pi%value(nrho,nion))
     coreprof(1)%profiles1d%pi%value = coreprof(1)%ti%value * coreprof(1)%ni%value * itm_ev
  endif
  if(.not.associated(coreprof(1)%profiles1d%pr_th%value)) then
     write(*,*) 'Calculating coreprof(1)%profiles1d%pr_th%value'
     allocate(coreprof(1)%profiles1d%pr_th%value(nrho))
     coreprof(1)%profiles1d%pr_th%value = coreprof(1)%profiles1d%pe%value + sum(coreprof(1)%profiles1d%pi%value,dim=2)
  endif

  write(*,*) (maxval(EQUILIBRIUM(1)%coord_sys%position%z)-minval(EQUILIBRIUM(1)%coord_sys%position%z))/ &
       (maxval(EQUILIBRIUM(1)%coord_sys%position%r)-minval(EQUILIBRIUM(1)%coord_sys%position%r))

  call NEO(EQUILIBRIUM,COREPROF,NEOCLASSIC_NEO)
  call NEOWES(EQUILIBRIUM,COREPROF,NEOCLASSIC_NEOWES)
#ifdef GOT_NEOS
  call SIGNEOJBS(EQUILIBRIUM,COREPROF,NEOCLASSIC_NEOS)
#endif
  if(coreprof(1)%rho_tor(1).eq.0.0_R8) then
     coreprof(1)%rho_tor(1)=coreprof(1)%rho_tor(2)/1e10_R8
  endif
#ifdef GOT_NEOART
  call ITMNEOART(EQUILIBRIUM,COREPROF,NEOCLASSIC_ITMNEOART)
#endif

  open(10,file='neo.dat')
  write(10,'(a,4(i4))') '# ',NRHO, NION, shot, run
  do irho=1,size(NEOCLASSIC_NEO(1)%sigma)
     write(10,1000) NEOCLASSIC_NEO(1)%rho_tor(irho), &
          NEOCLASSIC_NEO(1)%sigma(irho), &
          NEOCLASSIC_NEO(1)%jboot(irho), &
          NEOCLASSIC_NEO(1)%ne_neo%flux(irho), &
          NEOCLASSIC_NEO(1)%ne_neo%diff_eff(irho), &
          NEOCLASSIC_NEO(1)%ne_neo%vconv_eff(irho), &
          NEOCLASSIC_NEO(1)%te_neo%flux(irho), &
          NEOCLASSIC_NEO(1)%te_neo%diff_eff(irho), &
          NEOCLASSIC_NEO(1)%te_neo%vconv_eff(irho), &
          (NEOCLASSIC_NEO(1)%ni_neo%flux(irho,iion),iion=1,NION), &
          (NEOCLASSIC_NEO(1)%ni_neo%diff_eff(irho,iion),iion=1,NION), &
          (NEOCLASSIC_NEO(1)%ni_neo%vconv_eff(irho,iion),iion=1,NION), &
          (NEOCLASSIC_NEO(1)%ti_neo%flux(irho,iion),iion=1,NION), &
          (NEOCLASSIC_NEO(1)%ti_neo%diff_eff(irho,iion),iion=1,NION), &
          (NEOCLASSIC_NEO(1)%ti_neo%vconv_eff(irho,iion),iion=1,NION)
  enddo
  close(10)

  open(10,file='neowes.dat')
  write(10,'(a,4(i4))') '# ',NRHO, NION, shot, run
  do irho=1,size(NEOCLASSIC_NEOWES(1)%sigma)
     write(10,1000) NEOCLASSIC_NEOWES(1)%rho_tor(irho), &
          NEOCLASSIC_NEOWES(1)%sigma(irho), &
          NEOCLASSIC_NEOWES(1)%jboot(irho), &
          NEOCLASSIC_NEOWES(1)%ne_neo%flux(irho), &
          NEOCLASSIC_NEOWES(1)%ne_neo%diff_eff(irho), &
          NEOCLASSIC_NEOWES(1)%ne_neo%vconv_eff(irho), &
          NEOCLASSIC_NEOWES(1)%te_neo%flux(irho), &
          NEOCLASSIC_NEOWES(1)%te_neo%diff_eff(irho), &
          NEOCLASSIC_NEOWES(1)%te_neo%vconv_eff(irho), &
          (NEOCLASSIC_NEOWES(1)%ni_neo%flux(irho,iion),iion=1,NION), &
          (NEOCLASSIC_NEOWES(1)%ni_neo%diff_eff(irho,iion),iion=1,NION), &
          (NEOCLASSIC_NEOWES(1)%ni_neo%vconv_eff(irho,iion),iion=1,NION), &
          (NEOCLASSIC_NEOWES(1)%ti_neo%flux(irho,iion),iion=1,NION), &
          (NEOCLASSIC_NEOWES(1)%ti_neo%diff_eff(irho,iion),iion=1,NION), &
          (NEOCLASSIC_NEOWES(1)%ti_neo%vconv_eff(irho,iion),iion=1,NION)
  enddo
  close (10)

#ifdef GOT_NEOART
  open(10,file='itmneoart.dat')
  write(10,'(a,4(i4))') '# ',NRHO, NION, shot, run
  do irho=1,size(NEOCLASSIC_ITMNEOART(1)%sigma)
     write(10,1000) NEOCLASSIC_ITMNEOART(1)%rho_tor(irho), &
          NEOCLASSIC_ITMNEOART(1)%sigma(irho), &
          NEOCLASSIC_ITMNEOART(1)%jboot(irho), &
          NEOCLASSIC_ITMNEOART(1)%ne_neo%flux(irho), &
          NEOCLASSIC_ITMNEOART(1)%ne_neo%diff_eff(irho), &
          NEOCLASSIC_ITMNEOART(1)%ne_neo%vconv_eff(irho), &
          NEOCLASSIC_ITMNEOART(1)%te_neo%flux(irho), &
          NEOCLASSIC_ITMNEOART(1)%te_neo%diff_eff(irho), &
          NEOCLASSIC_ITMNEOART(1)%te_neo%vconv_eff(irho), &
          (NEOCLASSIC_ITMNEOART(1)%ni_neo%flux(irho,iion),iion=1,NION), &
          (NEOCLASSIC_ITMNEOART(1)%ni_neo%diff_eff(irho,iion),iion=1,NION), &
          (NEOCLASSIC_ITMNEOART(1)%ni_neo%vconv_eff(irho,iion),iion=1,NION), &
          (NEOCLASSIC_ITMNEOART(1)%ti_neo%flux(irho,iion),iion=1,NION), &
          (NEOCLASSIC_ITMNEOART(1)%ti_neo%diff_eff(irho,iion),iion=1,NION), &
          (NEOCLASSIC_ITMNEOART(1)%ti_neo%vconv_eff(irho,iion),iion=1,NION)
  enddo
  close (10)
#ifdef GOT_NEOART

#ifdef GOT_NEOS
  open(10,file='neos.dat')
  write(10,'(a,4(i4))') '# ',NRHO, NION, shot, run
  do irho=1,size(NEOCLASSIC_NEOS(1)%sigma)
     write(10,1000) NEOCLASSIC_NEOS(1)%rho_tor(irho), &
          NEOCLASSIC_NEOS(1)%sigma(irho), &
          NEOCLASSIC_NEOS(1)%jboot(irho)
!, &
!          NEOCLASSIC_NEOS(1)%ne_neo%flux(irho), &
!          NEOCLASSIC_NEOS(1)%ne_neo%diff_eff(irho), &
!          NEOCLASSIC_NEOS(1)%ne_neo%vconv_eff(irho), &
!          NEOCLASSIC_NEOS(1)%te_neo%flux(irho), &
!          NEOCLASSIC_NEOS(1)%te_neo%diff_eff(irho), &
!          NEOCLASSIC_NEOS(1)%te_neo%vconv_eff(irho), &
!          (NEOCLASSIC_NEOS(1)%ni_neo%flux(irho,iion),iion=1,NION), &
!          (NEOCLASSIC_NEOS(1)%ni_neo%diff_eff(irho,iion),iion=1,NION), &
!          (NEOCLASSIC_NEOS(1)%ni_neo%vconv_eff(irho,iion),iion=1,NION), &
!          (NEOCLASSIC_NEOS(1)%ti_neo%flux(irho,iion),iion=1,NION), &
!          (NEOCLASSIC_NEOS(1)%ti_neo%diff_eff(irho,iion),iion=1,NION), &
!          (NEOCLASSIC_NEOS(1)%ti_neo%vconv_eff(irho,iion),iion=1,NION)
  enddo
  close(10)
#endif

  if(associated(coreprof(1)%psi%sigma_par%value)) then
     open(10,file='sigma_par.dat')
     do irho=1,size(coreprof(1)%psi%sigma_par%value)
        write(10,1000) coreprof(1)%rho_tor(irho), coreprof(1)%psi%sigma_par%value(irho)
     enddo
     close (10)
  endif

  open(10,file='rho_tor_te_ne.dat')
  do irho=1,size(coreprof(1)%psi%sigma_par%value)
     write(10,1000) coreprof(1)%rho_tor(irho), coreprof(1)%te%value(irho), coreprof(1)%ne%value(irho)
  enddo
  close (10)

  call deallocate_cpo(COREPROF)
  call deallocate_cpo(CORETRANSP)
  call deallocate_cpo(CORESOURCE)
  call deallocate_cpo(COREIMPUR)
  call deallocate_cpo(EQUILIBRIUM)
  call deallocate_cpo(TOROIDFIELD)
  call deallocate_cpo(NEOCLASSIC_NEO)
#ifdef GOT_NEOS
  call deallocate_cpo(NEOCLASSIC_NEOS)
#endif
  call deallocate_cpo(NEOCLASSIC_NEOWES)
#ifdef GOT_NEOART
  call deallocate_cpo(NEOCLASSIC_ITMNEOART)
#endif

1000 format(1p,100(100g15.6))
end program neo_test
