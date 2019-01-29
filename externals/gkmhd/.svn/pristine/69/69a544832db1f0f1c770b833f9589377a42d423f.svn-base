#ifdef ITMCPOs
SUBROUTINE GKMHD(eq_in, eq_out, code_parameters)
#else
PROGRAM Equil
#endif

!...  2D tokamak equilibrium setup
!...  divergence balance form of Grad Shafranov equation
!...  triangular grid
!...  moving grid nodes, onto flux sfc at constant angle

#ifdef ITMCPOs
  USE Euitm_Schemas
  USE write_structures
#endif

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: iplt,i,j,ierr
  REAL, SAVE :: time,psi0,psi1

  LOGICAL, SAVE :: first_flag = .true.

#ifdef ITMCPOs

  TYPE (type_equilibrium), pointer :: eq_in(:), eq_out(:)
  TYPE (type_param) :: code_parameters

!...  XML declarations

  integer(ITM_I4) :: return_status

  character(len = 132), target :: codename(1) = 'GKMHD'
  character(len = 132), target :: codeversion(1) = 'Dec 2012'

!...  assign parms

  allocate(eq_out(1))
  allocate(eq_out(1)%codeparam%codename(1))
  allocate(eq_out(1)%codeparam%codeversion(1))
  if (.not. associated(code_parameters%parameters)) then
    write(*,*) 'ERROR: GKMHD parameters not associated!'
    stop
  else
    allocate(eq_out(1)%codeparam%parameters(size( &
     code_parameters%parameters)))
  end if

!.  write(*,*) 'GKMHD Parameters : ', code_parameters%parameters

!-- add to eq_out
  eq_out(1)%codeparam%codename = codename
  eq_out(1)%codeparam%codeversion = codeversion
  eq_out(1)%codeparam%parameters = code_parameters%parameters

!-- assign code parameters to internal variables
  call assign_equil_parameters(code_parameters, return_status)

  if (return_status /= 0) then
    write(*,*) 'ERROR: Could not assign GKMHD parameters.'
    return
  end if

!.  write(*,*) 'done assigning GKMHD parameters'

     IF (write_snaps) THEN
        OPEN (15,file='hdw.dat',form='formatted')
        WRITE (15,parm)
        CLOSE (15)
     END IF
#else

!...  open files and get parms

  IF (first_flag) THEN

#ifdef PERF
  CALL PERFINIT

  CALL PERFON('init')
#endif

        OPEN (15,file='hdw.dat',form='formatted',status='old')
        READ (15,parm)
        CLOSE (15)
  END IF
#endif

!...  system initialisation

  IF (first_flag) THEN

#include <mppein.h90>

!...  geometry
!...  CPO profiles and boundary surface in metric

  CALL Metric

!...  allocations

  n_neighbors = 6

  INCLUDE 'vset.h90'

!...  start metric info file

  IF (write_diags) THEN
     OPEN (10,file=ffile,form='formatted')
     WRITE (10,*) 'metric information'
     CLOSE (10)
  END IF

!...  initial state

#ifdef ITMCPOs
  IF (write_cpos) THEN
     call open_write_file(12, 'EqIn' )
     call write_cpo(eq_in, 'Equil' )
     call close_write_file
  END IF

  CALL GetBndy(eq_in(1))
#else
  CALL GetBndy
#endif

  CALL ReLay

!...  get the flux surfaces

  CALL GetPsi(psi0,psi1)

  CALL FSDiags

  time=0.0
  IF (write_snaps) CALL Psnaps(40,time)

!...  stepping

     first_flag = .false.

  ELSE

#ifdef ITMCPOs
  IF (write_cpos) THEN
     call open_write_file(12, 'EqIn' )
     call write_cpo(eq_in, 'Equil' )
     call close_write_file
  END IF

  CALL GetProfs(eq_in(1))
!  CALL GetBndy(eq_in(1))
#endif

  END IF

#ifdef PERF
  CALL PERFOFF

  CALL PERFON('main')
#endif

  DO iplt=1,niter_eq

#ifndef ITMCPOs
#ifdef PERF
     CALL PERFON('step')
#endif
#endif

     time=REAL(iplt)

     CALL MoveAxis(psi0,psi1)

     CALL GetPsi(psi0,psi1)

     CALL FSDiags

!     IF (iplt > 6 .AND. iplt < niter_eq) THEN
     IF (psi0 == psi1 .AND. iplt < niter_eq) THEN
        CALL ReGrid(psi0)
     END IF

     IF (write_diags) write (0,*) 'convergence = ',econvg, &
          MAXVAL(ABS(fsfc(nx2)%uu(0,mupsi,:) - psitor(nx2)))
     IF (MAXVAL(ABS(fsfc(nx2)%uu(0,mupsi,:) - psitor(nx2))) < econvg) EXIT

!...  lo-res diagnostics

#ifndef ITMCPOs
#ifdef PERF
     CALL PERFOFF

     CALL PERFON('snap_wr')
#endif
#endif

     IF (mype == 0 .AND. write_diags) WRITE (0,*) 'step',iplt
     IF (write_snaps) CALL Psnaps(40,time)

#ifndef ITMCPOs
#ifdef PERF
     CALL PERFOFF
#endif
#endif

  END DO

  iplt=iplt-1

!...  save current state

#ifndef ITMCPOs
#ifdef PERF
  CALL PERFOFF

  CALL PERFON('final_wr')
#endif
#endif

  CALL FSDiags

!...  exit CPOs

#ifdef ITMCPOs

  CALL PutEquil(eq_out(1))

!...  stamp time

  eq_out(1)%time=eq_in(1)%time

!...  write output CPO

  IF (write_cpos) THEN
     call open_write_file(12, 'EqOut' )
     call write_cpo(eq_out(1), 'Equil' )
     call close_write_file
  END IF

!  call PetscFinalize(ierr)

!...  aus

END SUBROUTINE GKMHD

#else

  IF (mype == 0) THEN
     WRITE (6,330) time
     OPEN (19,file='stopped',form='formatted')
     WRITE (19,330) time
     CLOSE (19)
  END IF

#ifdef PERF
  CALL PERFOFF

  CALL PERFOUT('main')
#endif

330 FORMAT('stopped at time = ',g9.3)

END PROGRAM Equil
#endif
