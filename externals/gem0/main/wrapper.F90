PROGRAM Wrapper

  !...  uses a CPO file to wrap things

  USE ITM_types
  USE Euitm_schemas
#ifdef DOUAL
  USE Euitm_routines
#else
  USE Read_structures
#endif
  USE Write_structures

  IMPLICIT NONE

  TYPE (type_equilibrium), pointer :: eq(:) => null()
  TYPE (type_coreprof), pointer :: coreprof(:) => null()
  TYPE (type_coretransp), pointer :: coretransp(:) => null()
  TYPE (type_param) :: code_parameters

  INTEGER(ITM_I4) :: idx,shot=-1,srun
  REAL(R8) :: stime
  CHARACTER(len=12) :: suser,smach,svers = '4.10b'

  interface
     subroutine gem(eq, coreprof, coretransp, code_parameters)
       use euitm_schemas
       type (type_equilibrium), pointer ::  eq(:)
       type (type_coreprof), pointer :: coreprof(:)
       type (type_coretransp), pointer :: coretransp(:)
       type (type_param) :: code_parameters
     end subroutine gem
  end interface

!...  get initial state

  ALLOCATE (eq(1))
  ALLOCATE (coreprof(1))

#ifdef DOUAL
  suser='bds'
  smach='test'
  shot=1
  srun=1
  stime=0.0

  CALL Euitm_Open_Env('euitm',shot,srun,idx,suser,smach,svers)
  CALL Euitm_Get_Slice(idx,'equilibrium',eq(1),stime,2)
  CALL Euitm_Get_Slice(idx,'coreprof',coreprof(1),stime,2)

  call open_write_file(10, 'cpofile' )
  call write_cpo(coreprof(1), 'Coreprof' )
  call write_cpo(eq(1), 'Equil' )
  call close_write_file
#else
  call open_read_file(10, 'cpofile' )
  call read_cpo(coreprof(1), 'Coreprof' )
  call read_cpo(eq(1), 'Equil' )
  call close_read_file
#endif

!...  run codes

  CALL Get_Code_Parms(code_parameters, 'gem.xml', '', 'gem.xsd')
  CALL gem(eq, coreprof, coretransp, code_parameters)

!...  write results

  call open_write_file(12, 'Done' )
  call write_cpo(coreprof(1), 'Coreprof' )
  call write_cpo(eq(1), 'Equil' )
  call write_cpo(coretransp(1), 'Coretransp' )
  call close_write_file

END PROGRAM Wrapper


