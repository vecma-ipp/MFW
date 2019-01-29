! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Print out the times stores in CPOs
!>
!> \author D. Coster
!>
!> \version "$Id: timescpo.f90 958 2010-12-15 14:20:58Z coster $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
program timescpo
  use itm_types
  use euitm_schemas
  use euitm_routines

  implicit none

  real(R8), pointer               :: times(:)
  integer                         :: idx,nslice,islice
  integer                         :: shot, run
  integer                         :: iargc, iarg, i, req_arg=2, narg, ncpo
  character(len=132)              :: arg, arg2
  character(len=132), allocatable :: cpos(:)
  character(len=132)              :: user, machine, ual_version

  call getenv('USER', user)
  call getenv('DATAVERSION', ual_version)
  if(ual_version.eq.'') then
     ual_version = '4.08b'
  endif
  call getenv('TOKAMAKNAME', machine)
  if(machine.eq.'') then
     machine = 'test'
  endif
  iarg=1
  narg=iargc()
10 if(narg.lt.req_arg) then
     write(*,*) 'Need to specify shot and run no'
     write(*,*) 'Optionally preceded by "-u USER -d MACHINE -v UAL_VERSION"'
     write(*,*) 'Optionally followed by a list of CPOs'
     stop 'ERROR!'
  endif
  call getarg(iarg,arg)
  if(arg(1:1).eq.'-') then
     call getarg(iarg+1,arg2)
     select case (arg(2:2))
     case ('u')
        user=arg2
     case('d')
        machine=arg2
     case('v')
        ual_version=arg2
     case default
        write(*,*) 'Unrecognized option ', trim(arg), ' ',trim(arg2)
        stop
     end select
     iarg=iarg+2
     req_arg=req_arg+2
     goto 10
  endif
  read(arg,*) shot
  call getarg(iarg+1,arg)
  read(arg,*) run
  iarg=iarg+2

  if(iarg.gt.narg) then
     ncpo=1
     allocate(cpos(ncpo))
     cpos(1)='coreprof'
  else
     ncpo=narg-iarg+1
     allocate(cpos(ncpo))
     do i=1, ncpo
        call getarg(iarg+i-1,cpos(i))
     enddo
  endif

  write(*,*) 'Processing shot/run = ',shot,' / ',run
  call euitm_open_env('euitm',shot,run,idx,trim(USER),trim(machine),trim(ual_version))
  do i=1, ncpo
     write(*,*) trim(cpos(i))
     call euitm_get_times(idx,trim(cpos(i)),times)
     write(*,*) 'Number of time points ',size(times)
     write(*,*) times
     deallocate(times)
  enddo
  call euitm_close(idx)
  deallocate(cpos)

end program timescpo

  
