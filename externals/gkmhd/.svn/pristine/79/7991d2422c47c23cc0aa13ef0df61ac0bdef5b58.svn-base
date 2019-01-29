PROGRAM Equil

!...  wrapper for subroutine version of GKMHD

  USE Phys_Constants
  USE Euitm_Schemas
  USE Read_structures
  USE Write_structures
  USE Copy_structures
  USE Deallocate_structures
  USE xml_file_reader

  IMPLICIT NONE

  TYPE (type_equilibrium), pointer :: eq_in(:), eq_out(:)
  TYPE (type_param) :: code_parameters
  TYPE (type_coreprof), pointer :: coreprof(:)

  interface
     subroutine gkmhd(eq_in, eq_out, code_parameters)
       use euitm_schemas
       type (type_equilibrium), pointer :: eq_in(:)
       type (type_equilibrium), pointer :: eq_out(:)
       type (type_param) :: code_parameters
     end subroutine gkmhd
  end interface

  INTEGER(ITM_I4) :: npsi_eq = 101, neta_eq = 256
  INTEGER(ITM_I4) :: nr_prof
  INTEGER(ITM_I4) :: i,ios
  REAL(R8) :: theta

  REAL(R8), DIMENSION(:), POINTER :: nne,tte,nni,tti
  REAL(R8), DIMENSION(:), POINTER :: eta,rho,jtor,ptor

  INTEGER :: irun, nruns = 1

  LOGICAL :: use_coreprof = .false.

!...  get the CPOs

  allocate(coreprof(1))
  allocate(eq_in(1))

     WRITE (6,*) 'using the CPO file'

  IF (use_coreprof) THEN

     WRITE (6,*) 'set Equil from Coreprof'

     call open_read_file(10, 'cpofile' )
     call read_cpo(coreprof(1), 'Coreprof' )
!     call read_cpo(eq_in(1), 'Equil' )
     call close_read_file

     nr_prof=SIZE(coreprof(1)%rho_tor)
     ALLOCATE(eq_in(1)%profiles_1d%rho_tor(nr_prof))
     ALLOCATE(eq_in(1)%profiles_1d%jphi(nr_prof))
     ALLOCATE(eq_in(1)%profiles_1d%pressure(nr_prof))
     eq_in(1)%profiles_1d%rho_tor=coreprof(1)%rho_tor
     eq_in(1)%profiles_1d%jphi=coreprof(1)%profiles1d%jphi%value
     eq_in(1)%profiles_1d%pressure=coreprof(1)%profiles1d%pr_th%value

     eq_in(1)%global_param%toroid_field%r0 = &
          coreprof(1)%toroid_field%r0
     eq_in(1)%global_param%toroid_field%b0 = &
          coreprof(1)%toroid_field%b0

     WRITE (6,*) 'using the bndy file'

     OPEN (unit = 17, file = 'plasma_boundary.in', &
          status = 'old', form = 'formatted', &
          action = 'read')
     neta_eq = 0
     DO
        READ(17, *, iostat = ios) theta, theta
        IF (ios == -1) EXIT
        neta_eq = neta_eq + 1
     END DO

     WRITE (6,*) 'bndy has ',neta_eq,' points'

     ALLOCATE(eq_in(1)%eqgeometry%boundary(1))
     ALLOCATE(eq_in(1)%eqgeometry%boundary(1)%r(neta_eq))
     ALLOCATE(eq_in(1)%eqgeometry%boundary(1)%z(neta_eq))

     REWIND 17

     DO i = 1, neta_eq
        READ(17, *) &
             eq_in(1)%eqgeometry%boundary(1)%r(i), &
             eq_in(1)%eqgeometry%boundary(1)%z(i)
     END DO
     CLOSE (17)

  ELSE

     call open_read_file(10, 'cpofile' )
     call read_cpo(coreprof(1), 'Coreprof' )
     call read_cpo(eq_in(1), 'Equil' )
     call close_read_file

  END IF

!...  put the initial CPOs

  call open_write_file(10, 'GKMHDinit' )
  call write_cpo(coreprof(1), 'Coreprof' )
  call write_cpo(eq_in(1), 'Equil' )
  call close_write_file

!...  run the case

  DO irun=1,nruns
     CALL Get_Code_Parms(code_parameters, 'gkmhd.xml', '', 'gkmhd.xsd')
     CALL GKMHD(eq_in, eq_out, code_parameters)
     IF (irun < nruns) THEN
        call deallocate_cpo(eq_in(1))
        call copy_cpo(eq_out(1),eq_in(1))
        call deallocate_cpo(eq_out)
!        call deallocate(eq_out)
     END IF
  END DO

!...  auxiliaries

!  write (0,*) 'do putrz'
!  CALL Putrz
!  CALL Putb
!  write (0,*) 'done putrz'

  INCLUDE 'cprof.h90'

!...  put the final CPOs

  call open_write_file(10, 'GKMHD' )
  call write_cpo(coreprof(1), 'Coreprof' )
  call write_cpo(eq_out(1), 'Equil' )
  call close_write_file

END PROGRAM Equil


SUBROUTINE Putrz

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: iu = 20

  INTEGER :: i,j,i0,j0,j1,j2

  OPEN (iu,file='EQRZ',form='unformatted')
  WRITE (iu) nx

  WRITE (iu) ra
  WRITE (iu) qtor
  WRITE (iu) psitor
  WRITE (iu) phitor
  WRITE (iu) itor

  DO i=1,nx
     j0=fsfc(i)%ny

     WRITE (iu) fsfc(i)%rtor(1:j0)
     WRITE (iu) fsfc(i)%ztor(1:j0)
     WRITE (iu) fsfc(i)%btor(1:j0)
     WRITE (iu) fsfc(i)%clocketa(1:j0)
     WRITE (iu) fsfc(i)%theta(1:j0)
  END DO

  CLOSE(iu)

END SUBROUTINE Putrz


SUBROUTINE Putb

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: iu = 20

  INTEGER :: i,i1,i2,j0,j1,j2

  OPEN (iu,file='bndy.dat',form='formatted')

  i1=nx0*7/8+ngdx+1
  i2=nx0+ngdx+1

  DO i=i1,i2,i2-i1
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy

     WRITE (iu,100) j0
     WRITE (iu,110) fsfc(i)%rtor(j1:j2)
     WRITE (iu,110) fsfc(i)%ztor(j1:j2)
  END DO

  j0=SIZE(fsbndyr)-2*ngdy
     j1=1+ngdy
     j2=j0+ngdy
  WRITE (iu,100) j0
  WRITE (iu,110) fsbndyr(j1:j2)
  WRITE (iu,110) fsbndyz(j1:j2)

  CLOSE(iu)

100 format(i3)
110 format(8f10.6)

END SUBROUTINE Putb
