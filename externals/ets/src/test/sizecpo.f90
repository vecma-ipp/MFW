program sizecpo
  use itm_types
  use euitm_schemas
  use euitm_routines
  use size_of_structures
  use deallocate_structures

  implicit none

  type(type_coreprof),pointer :: coreprof(:) => null()
  type(type_equilibrium),pointer :: equilibrium(:) => null()
  type(type_coretransp),pointer :: coretransp(:) => null()
  type(type_coresource),pointer :: coresource(:) => null()
  integer idx,nslice,islice
  integer (ITM_I8) total_size(3)
  integer shot, run
  integer iargc
  character*132 arg

  if(iargc().ne.2) then
     write(*,*) 'Need to specify shot and run no'
     stop 'ERROR!'
  endif
  call getarg(1,arg)
  read(arg,*) shot
  call getarg(2,arg)
  read(arg,*) run

  write(*,*) 'Processing shot/run = ',shot,' / ',run
  call euitm_open('euitm',shot,run,idx)

!  call euitm_open('euitm',5,2,idx)
!  call euitm_open('euitm',3,749,idx)
!  call euitm_open('euitm',5,55,idx)
!  call euitm_open('euitm',5,56,idx)
!  call euitm_open('euitm',5,57,idx)

  call set_size_of_maxlevel(0)
  total_size=0

  call euitm_get(idx,'coresource',coresource)
  if(associated(coresource)) then
     nslice=size(coresource)
     do islice=1,nslice
        total_size(3)=0
        call size_of_cpo(coresource(islice),total_size(3),.false.,'coresource')
        total_size(2)=total_size(2)+total_size(3)
     enddo
     call deallocate_cpo(coresource)
  endif
  write(*,*) ' coresource ',total_size(2),nslice
  total_size(1)=total_size(1)+total_size(2)
  total_size(2)=0

  call euitm_get(idx,'coretransp',coretransp)
  if(associated(coretransp)) then
     nslice=size(coretransp)
     do islice=1,nslice
        total_size(3)=0
        call size_of_cpo(coretransp(islice),total_size(3),.false.,'coretransp')
        total_size(2)=total_size(2)+total_size(3)
     enddo
     call deallocate_cpo(coretransp)
  endif
  write(*,*) ' coretransp ',total_size(2),nslice
  total_size(1)=total_size(1)+total_size(2)
  total_size(2)=0

  call euitm_get(idx,'coreprof',coreprof)
  if(associated(coreprof)) then
     nslice=size(coreprof)
     do islice=1,nslice
        total_size(3)=0
        call size_of_cpo(coreprof(islice),total_size(3),.false.,'coreprof')
        total_size(2)=total_size(2)+total_size(3)
     enddo
     call deallocate_cpo(coreprof)
  endif
  write(*,*) '   coreprof ',total_size(2),nslice
  total_size(1)=total_size(1)+total_size(2)
  total_size(2)=0

  call euitm_get(idx,'equilibrium',equilibrium)
  if(associated(equilibrium)) then
     nslice=size(equilibrium)
     do islice=1,nslice
        total_size(3)=0
        call size_of_cpo(equilibrium(islice),total_size(3),.false.,'equilibrium')
        total_size(2)=total_size(2)+total_size(3)
     enddo
     call deallocate_cpo(equilibrium)
  endif
  write(*,*) 'equilibrium ',total_size(2),nslice
  total_size(1)=total_size(1)+total_size(2)
  total_size(2)=0

  write(*,*) '      Total ',total_size(1)

  call euitm_close(idx)

end program sizecpo

  
