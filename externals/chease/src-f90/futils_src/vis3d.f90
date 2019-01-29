MODULE vis3d
  !
  !  Output 3D arrays for 3D visialization
  !  HDF5 based formats: PIXIE and XDMF
  !
  !   P. Angelino, T.M. Tran CRPP/EPFL.
  !
  !  Meshes:
  !V    s,chi mesh
  !V    field aligned mesh
  !V    volume selection (select one, high resolution, 3D slice to output)
  !V    Custom mesh size
  !V    Data type (4Bytes, 8Bytes)
  !
  !  Output format:
  !V    parallel HDF5
  !V         1 file x time step
  !V         1 file
  !V    PIXIE (parallel read in VisIT)
  !
  !V  Pointer to functions (Fortran 2003 support only).
  !
  USE futils
  !
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  !
  PRIVATE
  PUBLIC :: init3Ddiagnostics, write3D, checkout3dfile, get3dmeshgrid, get3dmeshinfo
  PUBLIC :: get_field_func, DIAG3D_PARAMETERS
  !
  ! Subroutine interface definitions
  !
  INTERFACE
     SUBROUTINE get_field_func(x, y, z, field)
       DOUBLE PRECISION, INTENT(IN) :: x, y, z
       DOUBLE PRECISION, INTENT(INOUT) :: field
     END SUBROUTINE get_field_func
  END INTERFACE
  INTERFACE
     SUBROUTINE coordf(st, chit, phit, xt, yt, zt)
       DOUBLE PRECISION, INTENT(IN)    :: st, chit
       DOUBLE PRECISION, INTENT(INOUT) :: phit
       DOUBLE PRECISION, INTENT(OUT)   :: xt, yt, zt
     END SUBROUTINE coordf
  END INTERFACE
  !
  !
  ! Gobal parameters
  !
  INTEGER, PARAMETER :: dp = selected_real_kind(15, 307) ! ensure 64bit real kind
  REAL(kind=dp), PARAMETER :: PI = 3.141592653589793238462643383279502884197_dp
  REAL(kind=dp), PARAMETER :: phi_up_default = 6.283185307179586476925286766559005768394_dp
  INTEGER, PARAMETER :: fid_xmf = 30
  ! Select if use link to the grid file or keep the grid with the data
  LOGICAL, PARAMETER :: use_h5links = .FALSE.
  !
  ! Gobal variables
  !
  TYPE DIAG3D_PARAMETERS
     CHARACTER(LEN=2) :: mesh_type                ! sc: s, chi, phi; fa: field aligned
     CHARACTER :: data_type                       ! s: single precision; d: double precision
     CHARACTER(LEN=16) :: output_format                 ! format of the output files
     INTEGER, DIMENSION(3) :: mesh_size   ! size of the output mesh
     DOUBLE PRECISION, DIMENSION(2) :: slim           ! slice in s direction
     DOUBLE PRECISION, DIMENSION(2) :: chilim         ! slice in chi direction
     DOUBLE PRECISION, DIMENSION(2) :: philim         ! slice in phi direction
     LOGICAL :: forcelocal                        ! grid is forced to be local to the processors
     PROCEDURE(coordf), POINTER, NOPASS :: coordf_ptr   ! Pointer to the coordinate transform function
     INTEGER :: comm_loc                   ! Communicator of distributed 3d data
     INTEGER :: nphip                      ! # of grid points in the distributed dimension per procs
     LOGICAL :: nlres                       ! Flag for restart runs
     LOGICAL :: verbose                    ! Write 3D module messages to std output?
  END TYPE DIAG3D_PARAMETERS
  !
  ! Mesh options
  CHARACTER(LEN=2), SAVE :: mesh_type
  CHARACTER, SAVE :: data_type
  CHARACTER(LEN=16) :: output_format
  CHARACTER(LEN=4) :: output_format_type
  INTEGER, DIMENSION(3), TARGET, SAVE :: mesh_size
  DOUBLE PRECISION, DIMENSION(2), TARGET, SAVE :: slim
  DOUBLE PRECISION, DIMENSION(2), TARGET, SAVE :: chilim
  DOUBLE PRECISION, DIMENSION(2), TARGET, SAVE :: philim
  ! 3D grid:
  DOUBLE PRECISION, DIMENSION(:), TARGET, ALLOCATABLE, SAVE :: sgrid_loc, chigrid_loc, phigrid_loc
  DOUBLE PRECISION, TARGET, SAVE :: dsgrid, dchigrid, dphigrid
  ! output field:
  ! XDMF format
  DOUBLE PRECISION, DIMENSION(:), POINTER, SAVE :: field_out_ptr
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, TARGET, SAVE :: field_out
  ! PIXIE format
  DOUBLE PRECISION, DIMENSION(:,:,:), POINTER, SAVE :: field_out_3D_ptr
  DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, TARGET, SAVE :: field_out_3D
  ! Mesh:
  ! XDMF format
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, SAVE :: xmesh_loc, zmesh_loc, ymesh_loc
  ! PIXIE format
  DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, SAVE ::  xmesh_loc_3D, zmesh_loc_3D, ymesh_loc_3D
  ! Parallelization options
  LOGICAL, SAVE :: local_grid ! are the grid points local to the processor?
  LOGICAL, SAVE :: forcelocal = .FALSE. ! grid is force to be local to the processors
  INTEGER, TARGET, SAVE :: nphip  ! number of grid points in the parallelized direction
  INTEGER, SAVE :: pshift  ! shift in the phigrid_loc array, if grid is local
  ! MPI communicator for 3D output
  INTEGER, SAVE :: me_loc, comm_loc, nvp_loc
  LOGICAL, SAVE :: lverbose ! If true, write 3D module messages to std output
  ! IF the mpi communicator has been redefined, tells if the rank is still active or not
  LOGICAL, SAVE :: me_active = .TRUE.
  ! Pointer to the coordinate transform function
  PROCEDURE(coordf), POINTER, SAVE :: get_coords
  !
CONTAINS
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE init3Ddiagnostics(parameters)
    TYPE(DIAG3D_PARAMETERS), INTENT(IN) :: parameters
    !
    INTEGER :: ierr
    !
    ! Set module communicator
    !
    comm_loc = parameters%comm_loc
    nphip    = parameters%nphip
    lverbose = parameters%verbose
    !
    ! Read module parametes
    !
    CALL SET_3D_PARAMETERS(parameters)
    !
    ! Get MPI infos
    !
    CALL MPI_COMM_RANK(comm_loc, me_loc, ierr)
    CALL MPI_COMM_SIZE(comm_loc, nvp_loc, ierr)
    !
    ! Generate 3D grid (s, chi, phi)
    !
    CALL SET_3D_GRID()
    !
    ! Check if the proc is not contributing to the 3d output
    !
    IF (.NOT.me_active) RETURN
    !
    ! Set the graphics format
    !
    output_format_type = output_format(1:4)
    !
    SELECT CASE(output_format_type)
    CASE('XDMF')
       CALL create_mesh_xdmf()
    CASE('PIXI')
       CALL create_mesh_pixie()
       ! Initialize pixie writer
       IF (parameters%nlres) THEN
          CALL write_pixie('INITIALIZE_RESTART')
       ELSE
          CALL write_pixie('INITIALIZE')
       END IF
    CASE DEFAULT
       CALL create_mesh_pixie()
    END SELECT
    !
    IF (lverbose) CALL write_3d_parameters
    !
  END SUBROUTINE init3Ddiagnostics
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE create_mesh_pixie()
    INTEGER :: ii, jj, zz
    DOUBLE PRECISION    :: st, chit, phit, xt, yt, zt
    !
    ! Allocate mesh arrays
    !
    ALLOCATE(field_out_3D(mesh_size(1), mesh_size(2), nphip))
    ALLOCATE(xmesh_loc_3D(mesh_size(1), mesh_size(2), nphip))
    ALLOCATE(ymesh_loc_3D(mesh_size(1), mesh_size(2), nphip))
    ALLOCATE(zmesh_loc_3D(mesh_size(1), mesh_size(2), nphip))
    !
    ! Produce curvilinear structured mesh
    !
    DO zz=1,nphip
       DO jj=1,mesh_size(2)
          DO ii=1,mesh_size(1)
             st   = sgrid_loc(ii)
             chit = chigrid_loc(jj)
             phit = phigrid_loc(zz+pshift)
             CALL get_coords(st, chit, phit, xt, yt, zt)
             xmesh_loc_3d(ii,jj,zz) = xt
             zmesh_loc_3d(ii,jj,zz) = zt
             ymesh_loc_3d(ii,jj,zz) = yt
          END DO
       END DO
    END DO
    !
  END SUBROUTINE create_mesh_pixie
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE create_mesh_xdmf()
    INTEGER :: ii, jj, zz, cnt
    DOUBLE PRECISION    :: st, chit, phit, xt, yt, zt
    INTEGER :: mesh_array_size
    !
    ! Allocate mesh arrays
    !
    mesh_array_size = mesh_size(1)*mesh_size(2)*(nphip)
    ALLOCATE(field_out(1:mesh_array_size))
    ALLOCATE(xmesh_loc(1:mesh_array_size), ymesh_loc(1:mesh_array_size), zmesh_loc(1:mesh_array_size))
    !
    ! Produce curvilinear structured mesh
    !
    cnt = 1
    DO zz=1,nphip
       DO jj=1,mesh_size(2)
          DO ii=1,mesh_size(1)
             st   = sgrid_loc(ii)
             chit = chigrid_loc(jj)
             phit = phigrid_loc(zz+pshift)
             CALL get_coords(st, chit, phit, xt, yt, zt)
             xmesh_loc(cnt) = xt
             zmesh_loc(cnt) = zt
             ymesh_loc(cnt) = yt
             cnt = cnt + 1
          END DO
       END DO
    END DO
    !
  END SUBROUTINE create_mesh_xdmf
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE get3dmeshgrid(sgrid_ptr, chigrid_ptr, phigrid_ptr)
    ! Return the mesh grid
    DOUBLE PRECISION, DIMENSION(:), POINTER, INTENT(OUT) :: sgrid_ptr, chigrid_ptr, phigrid_ptr
    sgrid_ptr => sgrid_loc
    chigrid_ptr => chigrid_loc
    phigrid_ptr => phigrid_loc
    !
  END SUBROUTINE get3dmeshgrid
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE get3dmeshinfo(zdsgrid, zdchigrid, zdphigrid, zsmin, &
       zchimin, zphimin, zsbin, zchibin, zphibin, zlocalgrid)
    ! Return the mesh grid parameters
    ! Mesh spacing
    DOUBLE PRECISION, INTENT(OUT), OPTIONAL :: zdsgrid, zdchigrid, zdphigrid
    ! Mesh lower bounds
    DOUBLE PRECISION, INTENT(OUT), OPTIONAL :: zsmin, zchimin, zphimin
    ! Number of mesh points
    ! phibin: number of local mesh points in the parallelized dimension
    INTEGER, INTENT(OUT), OPTIONAL :: zsbin, zchibin, zphibin
    LOGICAL, INTENT(OUT), OPTIONAL :: zlocalgrid
    IF (PRESENT(zdsgrid)) zdsgrid = dsgrid
    IF (PRESENT(zdchigrid)) zdchigrid = dchigrid
    IF (PRESENT(zdphigrid)) zdphigrid = dphigrid
    IF (PRESENT(zsmin)) zsmin = slim(1)
    IF (PRESENT(zchimin)) zchimin = chilim(1)
    IF (PRESENT(zphimin)) zphimin = philim(1)+pshift*dphigrid
    IF (PRESENT(zsbin)) zsbin = mesh_size(1)
    IF (PRESENT(zchibin)) zchibin = mesh_size(2)
    IF (PRESENT(zphibin)) zphibin = nphip
    IF (PRESENT(zlocalgrid)) zlocalgrid = local_grid
    !
  END SUBROUTINE get3dmeshinfo
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE write3D(out_field, ztime, field_array_3d, field_function_ptr, filename, lclose_file)
    CHARACTER(*), INTENT(IN) :: out_field
    INTEGER, INTENT(IN) :: ztime
    DOUBLE PRECISION, DIMENSION(:,:,:), INTENT(IN), TARGET, OPTIONAL :: field_array_3d
    PROCEDURE(get_field_func), POINTER, OPTIONAL :: field_function_ptr
    LOGICAL, INTENT(IN), OPTIONAL :: lclose_file
    CHARACTER(len=128), INTENT(OUT), OPTIONAL :: filename     !! Output file name
    CHARACTER(len=118)   :: sufix        !! Output file name sufix
    CHARACTER(len=128) :: file_3d     !! Output file name
    INTEGER :: ierr
    LOGICAL :: lfclose
    ! If the proc is not in the active communicator returns
    IF (.NOT.me_active) RETURN
    !
    ! If lclose_file not specified, always close files
    IF (PRESENT(lclose_file)) THEN
       lfclose = lclose_file
    ELSE
       lfclose = .TRUE.
    END IF
    !
    ! If field array is not present it is set to point to null
    !
    SELECT CASE(output_format_type)
    CASE('XDMF')
       IF (PRESENT(field_array_3d)) THEN
          !field_out_ptr => field_array_3d
       ELSE IF (PRESENT(field_function_ptr)) THEN
          CALL get_3d_field_xdmf(field_function_ptr)
          field_out_ptr => field_out
       ELSE
          IF (lverbose)  WRITE(*,*) 'WARNING: wrong number of input argument to write3D'
          field_out_3D = 0
          field_out_3D_ptr => field_out_3D
       END IF
    CASE DEFAULT
       IF (PRESENT(field_array_3d)) THEN
          field_out_3D_ptr => field_array_3d
       ELSE IF (PRESENT(field_function_ptr)) THEN
          CALL get_3d_field_pixie(field_function_ptr)
          field_out_3D_ptr => field_out_3D
       ELSE
          IF (lverbose)  WRITE(*,*) 'WARNING: wrong number of input argument to write3D'
          field_out_3D = 0
          field_out_3D_ptr => field_out_3D
       END IF
    END SELECT
    !
    ! Writing according to format
    !
    SELECT CASE(output_format)
    CASE('XDMF_many')
       ! Generate filename according to output field and time step
       WRITE(sufix,"(a,'_',i6.6)") TRIM(out_field),ztime
       file_3d='output_3d_'//TRIM(sufix)
       CALL write_manyfiles_h5(file_3d, out_field)
    CASE('XDMF_one')
       ! Generate filename according to output field
       ! Time steps as separate datasets
       file_3d='output_3d_'//TRIM(out_field)
       CALL write_onefile_h5(file_3d, ztime, out_field)
    CASE('PIXIE_many')
       ! Generate filename according to output field and time step
       WRITE(sufix,"(a,'_',i6.6)") TRIM(out_field),ztime
       file_3d='output_3d_'//TRIM(sufix)
       CALL write_pixie(file_3d, ztime, out_field, .FALSE., .FALSE., lfclose)
    CASE('PIXIE_one')
       ! Generate filename according to output field
       ! Time steps as separate datasets
       file_3d='output_3d_'//TRIM(out_field)
       CALL write_pixie(file_3d, ztime, out_field, .FALSE., .TRUE., lfclose)
    CASE('PIXIE_only')
       ! One file for all datasets
       ! Time steps as separate datasets
       file_3d='output_3d'
       CALL write_pixie(file_3d, ztime, out_field, .TRUE., .TRUE., lfclose)
    CASE('PIXIE_step')
       ! One file for all datasets
       ! Time steps as separate files
       WRITE(sufix,"(i6.6)") ztime
       file_3d='output_3d_'//TRIM(sufix)
       CALL write_pixie(file_3d, ztime, out_field, .TRUE., .FALSE., lfclose)
    END SELECT
    IF (PRESENT(filename)) filename = TRIM(file_3d)//'.h5'  ! filename of the h5 file for output
  END SUBROUTINE write3D
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE get_3d_field_pixie(get_field)
    PROCEDURE(get_field_func), POINTER, OPTIONAL :: get_field
    DOUBLE PRECISION    :: st, chit, phit, tmp
    INTEGER :: ii, jj, zz
    !
    ! Getting the values of the output field
    !
    DO zz=1,nphip
       DO jj=1,mesh_size(2)
          DO ii=1,mesh_size(1)
             st   = sgrid_loc(ii)
             chit = chigrid_loc(jj)
             phit = phigrid_loc(zz+pshift)
             tmp = field_out_3D(ii,jj,zz)
             CALL get_field(st, chit, phit, tmp)
             field_out_3D(ii,jj,zz) = tmp
          END DO
       END DO
    END DO
    !
  END SUBROUTINE get_3d_field_pixie
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE get_3d_field_xdmf(get_field)
    PROCEDURE(get_field_func), POINTER, OPTIONAL :: get_field
    DOUBLE PRECISION    :: st, chit, phit, tmp
    INTEGER :: ii, jj, zz, cnt
    !
    ! Getting the values of the output field
    !
    cnt = 1
    DO zz=1,nphip
       DO jj=1,mesh_size(2)
          DO ii=1,mesh_size(1)
             st   = sgrid_loc(ii)
             chit = chigrid_loc(jj)
             phit = phigrid_loc(zz+pshift)
             tmp = field_out_3D(ii,jj,zz)
             CALL get_field(st, chit, phit, tmp)
             field_out_3D(ii,jj,zz) = tmp
             cnt = cnt + 1
          END DO
       END DO
    END DO
    !
  END SUBROUTINE get_3d_field_xdmf
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE write_pixie(file_3d, ztime, fieldname, ljoinfields, ljoinsteps, lfclose)
    !
    ! Write 3D data in PIXIE format
    ! one file, all time frames
    !
    CHARACTER(*), INTENT(IN) :: file_3d     !! Output file name
                                            !! = INIT, initialize the pixie writer
    CHARACTER(*), INTENT(IN), OPTIONAL :: fieldname    !! Output field name
    INTEGER, INTENT(IN), OPTIONAL :: ztime
    LOGICAL, INTENT(IN), OPTIONAL :: ljoinfields, ljoinsteps, lfclose
    CHARACTER(len=128) :: filename     !! Output file name
    CHARACTER(len=21) :: datagroup    !! Output data group
    CHARACTER(len=36) :: dataname
    INTEGER :: ierr, buf_size
    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: field_buf
    LOGICAL :: lwritenewfile
    LOGICAL :: lwritedatagroup
    CHARACTER(LEN=15), DIMENSION(3) ::  attr_data  ! Attribute data
    INTEGER, SAVE :: write_cnt     ! timestep number in the PIXIE file
                                   ! must be an integer running from 0 to n, increasing by 1
    INTEGER, SAVE :: previous_time
    ! 3d file identifier
    INTEGER, SAVE :: fid = 0
    !
    ! Check if initialization
    SELECT CASE(file_3d)
    CASE('INITIALIZE')
       write_cnt = -1
       RETURN
    CASE('INITIALIZE_RESTART')
       write_cnt = -2
       RETURN
    END SELECT
    !
    filename= TRIM(file_3d)//'.h5'  ! filename of the PIXIE file
    !
    attr_data(1)='/node_coords/X '
    attr_data(2)='/node_coords/Y '
    attr_data(3)='/node_coords/Z '
    !
    ! IF for PIXIE_many, always create a new file
    IF ((.NOT.ljoinfields).AND.(.NOT.ljoinsteps)) THEN
       lwritenewfile = .TRUE.
    ELSE
       lwritenewfile = .FALSE.
    END IF
    !
    ! IF fields are on separate file always write TIMESTEP datagroup
    IF (ljoinfields) THEN
       lwritedatagroup = .FALSE.
    ELSE
       lwritedatagroup = .TRUE.
    END IF
    !
    ! Set the timestep parameters
    ! check if it is the first call
    SELECT CASE(write_cnt)
    CASE(0)
       ! For PIXIE_one, create new files for each field at first call
       IF (.NOT.ljoinfields) lwritenewfile = .TRUE.
    CASE(-1)
       ! Regular startup, create new files and datagroups
       CALL write_pixie_grid()
       previous_time = ztime
       write_cnt = 0
       lwritedatagroup = .TRUE.
       lwritenewfile = .TRUE.
    CASE(-2)
       ! CASE for RESTART
       ! IF the timesteps are in the same file, read the last written step
       ! and append new data
       ! ELSE create the new timestep file
       IF (ljoinsteps) THEN
          ! read last write_cnt before the restart
          CALL openf(filename, fid)
          CALL getatt(fid, '/', 'last_step', write_cnt)
          CALL closef(fid)
          write_cnt = write_cnt + 1
       ELSE
          lwritenewfile = .TRUE.
          write_cnt = 0
       END IF
       lwritedatagroup = .TRUE.
       previous_time = ztime
    END SELECT
    !
    ! Check if it is a new timestep
    ! IF so create a new TIMESTEP data group
    ! and a new timestep file, if needed.
    IF (previous_time/=ztime) THEN
       lwritedatagroup = .TRUE.
       IF (.NOT.ljoinsteps) lwritenewfile = .TRUE.
       write_cnt = write_cnt + 1
       previous_time = ztime
    END IF
    !
    ! IF the timesteps are written in the same file, enumerate them
    IF (ljoinsteps) THEN
       WRITE(datagroup,"('/Timestep_',i0)") write_cnt
    ELSE
       datagroup = '/Timestep'
    END IF
    !
    ! Write file
    IF (local_grid) THEN
       IF (lwritenewfile) THEN
          CALL creatf(filename, fid, &
               &      desc="Output3D", &
               &      real_prec=data_type, &
               &      mpiposix=.FALSE., &
               &      mpicomm=comm_loc)
          IF (use_h5links) THEN
             ! Create link to mesh once
             CALL create_external_link('grid.h5', '/node_coords', fid, '/node_coords')
          ELSE
             ! Write grid and mesh once
             CALL putarr(fid, '/s', sgrid_loc)
             CALL putarr(fid, '/chi', chigrid_loc)
             CALL putarr(fid, '/phi', phigrid_loc)
             CALL creatg(fid, '/node_coords')
             CALL putarr(fid, '/node_coords/X', xmesh_loc_3D, pardim=3)
             CALL putarr(fid, '/node_coords/Z', zmesh_loc_3D, pardim=3)
             CALL putarr(fid, '/node_coords/Y', ymesh_loc_3D, pardim=3)
          END IF
       ELSE ! first_write
          IF ((fid.EQ.0) .AND. lfclose) THEN
             CALL openf(filename, fid, &
                  &      mpicomm=comm_loc)
          END IF
       END IF ! first_write
       IF(lwritedatagroup) CALL creatg(fid, TRIM(datagroup))
       dataname = TRIM(datagroup)//'/'//TRIM(fieldname)
       CALL putarr(fid, TRIM(dataname), field_out_3D_ptr, pardim=3)
       CALL attach(fid, '/', 'last_step', write_cnt) ! for restarts
    ELSE ! local_grid
       buf_size=SIZE(field_out_3D_ptr,1)*SIZE(field_out_3D_ptr,2)*SIZE(field_out_3D_ptr,3)
       ALLOCATE(field_buf(SIZE(field_out_3D_ptr,1),SIZE(field_out_3D_ptr,2),SIZE(field_out_3D_ptr,3)))
       CALL MPI_REDUCE(field_out_3D_ptr, field_buf, buf_size, MPI_DOUBLE_PRECISION, MPI_SUM, &
            0, comm_loc, ierr)
       IF (me_loc.EQ.0) THEN
          IF (lwritenewfile) THEN
             CALL creatf(filename, fid, &
                  &      desc="Output3D", &
                  &      real_prec=data_type)
             IF (use_h5links) THEN
                ! Create link to mesh once
                CALL create_external_link('grid.h5', '/node_coords', fid, '/node_coords')
             ELSE
                ! Write grid and mesh once
                CALL putarr(fid, '/s', sgrid_loc)
                CALL putarr(fid, '/chi', chigrid_loc)
                CALL putarr(fid, '/phi', phigrid_loc)
                CALL creatg(fid, '/node_coords')
                CALL putarr(fid, '/node_coords/X', xmesh_loc_3D)
                CALL putarr(fid, '/node_coords/Z', zmesh_loc_3D)
                CALL putarr(fid, '/node_coords/Y', ymesh_loc_3D)
             END IF
           ELSE ! first_write
              IF ((fid.EQ.0) .AND. lfclose) THEN
                 CALL openf(filename, fid)
              END IF
          END IF ! first_write
          IF (lwritedatagroup) CALL creatg(fid, TRIM(datagroup))
          dataname = TRIM(datagroup)//'/'//TRIM(fieldname)
          CALL putarr(fid, TRIM(dataname), field_buf)
          CALL attach(fid, '/', 'last_step', write_cnt)  ! for restarts
       END IF ! me_loc
       DEALLOCATE(field_buf)
    END IF ! local_grid
    !
    ! Write the pixie attributes
    !
    IF (lwritedatagroup) THEN
       CALL attach(fid, '/'//TRIM(datagroup), 'coords', attr_data)
       CALL attach(fid, '/'//TRIM(datagroup), 'Time', REAL(ztime, 8))
    END IF
    !
    ! Close file
    !
    IF ((fid.NE.0) .AND. lfclose) THEN
       CALL closef(fid)
       fid = 0
    END IF
    !
  END SUBROUTINE write_pixie
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE write_pixie_grid()
    CHARACTER(len=7) :: filename = 'grid.h5'     !! Output file name
    ! 3d file identifier
    INTEGER :: fid = 0
    !
    CALL creatf(filename, fid, &
         &      desc="Output3D", &
         &      real_prec=data_type, &
         &      mpiposix=.FALSE., &
         &      mpicomm=comm_loc)
    ! Write grid and mesh once
    CALL putarr(fid, '/s', sgrid_loc)
    CALL putarr(fid, '/chi', chigrid_loc)
    CALL putarr(fid, '/phi', phigrid_loc)
    CALL creatg(fid, '/node_coords')
    CALL putarr(fid, '/node_coords/X', xmesh_loc_3D, pardim=3)
    CALL putarr(fid, '/node_coords/Z', zmesh_loc_3D, pardim=3)
    CALL putarr(fid, '/node_coords/Y', ymesh_loc_3D, pardim=3)
    !
    ! Close file
    !
    IF (fid .NE. 0) CALL closef(fid)
    !
  END SUBROUTINE write_pixie_grid
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE write_manyfiles_h5(file_3d, fieldname)
    !
    ! Write 3D data in XDMF format
    ! one file x time frame
    !
    CHARACTER(*), INTENT(IN) :: file_3d     !! Output file name
    CHARACTER(*), INTENT(IN) :: fieldname    !! Output field name
    CHARACTER(len=128) :: filename     !! Output file name
    INTEGER :: ierr, buf_size
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: field_buf
    LOGICAL :: file_exists
    ! 3d file identifier
    INTEGER :: fid
    !
    filename= TRIM(file_3d)//'.h5'  ! filename of the XDMF file
    IF (local_grid) THEN
       CALL creatf(filename, fid, &
            &      desc="Output3D", &
            &      real_prec=data_type, &
            &      mpiposix=.FALSE., &
            &      mpicomm=comm_loc)
       !
       CALL putarr(fid, '/s', sgrid_loc)
       CALL putarr(fid, '/chi', chigrid_loc)
       CALL putarr(fid, '/phi', phigrid_loc)
       CALL putarr(fid, '/meshx', xmesh_loc, pardim=1)
       CALL putarr(fid, '/meshz', zmesh_loc, pardim=1)
       CALL putarr(fid, '/meshy', ymesh_loc, pardim=1)
       CALL putarr(fid, '/data', field_out_ptr, pardim=1)
       CALL closef(fid)
    ELSE
       buf_size = SIZE(field_out_ptr)
       ALLOCATE(field_buf(1:buf_size))
       CALL MPI_REDUCE(field_out_ptr, field_buf, buf_size, MPI_DOUBLE_PRECISION, MPI_SUM, &
            0, comm_loc, ierr)
       IF (me_loc.EQ.0) THEN
          CALL creatf(filename, fid, &
               &      desc="Output3D", &
               &      real_prec=data_type)
          !
          CALL putarr(fid, '/s', sgrid_loc)
          CALL putarr(fid, '/chi', chigrid_loc)
          CALL putarr(fid, '/phi', phigrid_loc)
          CALL putarr(fid, '/meshx', xmesh_loc)
          CALL putarr(fid, '/meshz', zmesh_loc)
          CALL putarr(fid, '/meshy', ymesh_loc)
          CALL putarr(fid, '/data', field_buf)
          CALL closef(fid)
       END IF
       DEALLOCATE(field_buf)
    END IF
    !
    ! Write the xmf wrapper
    !
    IF (me_loc.EQ.0) CALL write_manyfiles_xmf(file_3d, fieldname)
    !
  END SUBROUTINE write_manyfiles_h5
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE write_manyfiles_xmf(filename, fieldname)
    !
    ! Write xmf wrapper (xdmf format) to the XDMF file
    !
    CHARACTER(*), INTENT(IN) :: filename     !! Output file name
    CHARACTER(*), INTENT(IN) :: fieldname     !! Output field name
    CHARACTER(len=64) :: str1='    <DataItem Dimensions="'
    CHARACTER(len=64) :: str2='" NumberType="Float" Precision="4" Format="HDF">'
    CHARACTER(len=3) :: str3 = '"/>'
    OPEN (fid_xmf, FILE=TRIM(filename)//'.xmf', STATUS='UNKNOWN', FORM='FORMATTED')
    WRITE(fid_xmf,*) '<!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>'
    WRITE(fid_xmf,*) '<Xdmf Version="2.0">'
    WRITE(fid_xmf,*) '  <Domain>'
    WRITE(fid_xmf,*) '  <Grid Name="mesh3D" GridType="Uniform">'
    WRITE(fid_xmf,'(A53,3I5,A4)') '  <Topology TopologyType="3DSMesh" NumberOfElements="',&
         &      mesh_size(3),mesh_size(2),mesh_size(1),str3
    WRITE(fid_xmf,*) '    <Geometry GeometryType="X_Y_Z">'
    WRITE(fid_xmf,'(A27,3I5,A50)') str1, mesh_size(3),mesh_size(2),mesh_size(1), str2
    WRITE(fid_xmf,'(a)') TRIM(filename)//'.h5:/meshx'
    WRITE(fid_xmf,*) '      </DataItem>'
    WRITE(fid_xmf,'(A27,3I5,A50)') str1, mesh_size(3),mesh_size(2),mesh_size(1), str2
    WRITE(fid_xmf,'(a)') TRIM(filename)//'.h5:/meshy'
    WRITE(fid_xmf,*) '      </DataItem>'
    WRITE(fid_xmf,'(A27,3I5,A50)') str1, mesh_size(3),mesh_size(2),mesh_size(1), str2
    WRITE(fid_xmf,'(a)') TRIM(filename)//'.h5:/meshz'
    WRITE(fid_xmf,*) '      </DataItem>'
    WRITE(fid_xmf,*) '      </Geometry>'
    WRITE(fid_xmf,*) '      <Attribute Name="'//TRIM(fieldname)//'" AttributeType="Scalar" Center="Node">'
    WRITE(fid_xmf,'(A27,3I5,A50)') str1, mesh_size(3),mesh_size(2),mesh_size(1), str2
    WRITE(fid_xmf,'(a)') TRIM(filename)//'.h5:/data'
    WRITE(fid_xmf,*) '     </DataItem>'
    WRITE(fid_xmf,*) '  </Attribute> '
    WRITE(fid_xmf,*) '  </Grid> '
    WRITE(fid_xmf,*) '  </Domain> '
    WRITE(fid_xmf,*) '</Xdmf>  '
    CLOSE (fid_xmf)
  END SUBROUTINE write_manyfiles_xmf
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE write_onefile_h5(file_3d, ztime, fieldname)
    !
    ! Write 3D data in XDMF format
    ! one file x time frame
    !
    CHARACTER(*), INTENT(IN) :: file_3d     !! Output file name
    CHARACTER(*), INTENT(IN) :: fieldname    !! Output field name
    INTEGER, INTENT(IN) :: ztime
    CHARACTER(len=128) :: filename     !! Output file name
    CHARACTER(len=12) :: datagroup    !! Output data group
    CHARACTER(len=17) :: dataname
    INTEGER :: ierr, buf_size
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: field_buf
    LOGICAL :: file_exists
    ! 3d file identifier
    INTEGER :: fid
    !
    filename= TRIM(file_3d)//'.h5'  ! filename of the XDMF file
    WRITE(datagroup,"('/time_',i6.6)") ztime
    ! Chech if the file was already opened
    INQUIRE(FILE=filename, EXIST=file_exists)
    !
    IF (local_grid) THEN
       IF (.NOT.file_exists) THEN
          CALL creatf(filename, fid, &
               &      desc="Output3D", &
               &      real_prec=data_type, &
               &      mpiposix=.FALSE., &
               &      mpicomm=comm_loc)
          ! Write grid and mesh once
          CALL putarr(fid, '/s', sgrid_loc)
          CALL putarr(fid, '/chi', chigrid_loc)
          CALL putarr(fid, '/phi', phigrid_loc)
          CALL putarr(fid, '/meshx', xmesh_loc, pardim=1)
          CALL putarr(fid, '/meshz', zmesh_loc, pardim=1)
          CALL putarr(fid, '/meshy', ymesh_loc, pardim=1)
       ELSE ! first_write
          CALL openf(filename, fid, &
               &      mpicomm=comm_loc)
       END IF ! first_write
       CALL creatg(fid, datagroup)
       dataname = datagroup//'/time'
       CALL creatd(fid, 0, (/0/), dataname, 'Time frame')
       CALL append(fid, dataname, DBLE(ztime))
       dataname = datagroup//'/data'
       CALL putarr(fid, dataname, field_out_ptr, pardim=1)
       CALL closef(fid)
    ELSE ! local_grid
       buf_size = SIZE(field_out_ptr)
       ALLOCATE(field_buf(1:buf_size))
       CALL MPI_REDUCE(field_out_ptr, field_buf, buf_size, MPI_DOUBLE_PRECISION, MPI_SUM, &
            0, comm_loc, ierr)
       IF (me_loc.EQ.0) THEN
          IF (.NOT.file_exists) THEN
             CALL creatf(filename, fid, &
                  &      desc="Output3D", &
                  &      real_prec=data_type)
             ! Write grid and mesh once
             CALL putarr(fid, '/s', sgrid_loc)
             CALL putarr(fid, '/chi', chigrid_loc)
             CALL putarr(fid, '/phi', phigrid_loc)
             CALL putarr(fid, '/meshx', xmesh_loc)
             CALL putarr(fid, '/meshz', zmesh_loc)
             CALL putarr(fid, '/meshy', ymesh_loc)
          ELSE ! first_write
             CALL openf(filename, fid)
          END IF ! first_write
          CALL creatg(fid, datagroup)
          dataname = datagroup//'/time'
          CALL creatd(fid, 0, (/0/), dataname, 'Time frame')
          CALL append(fid, dataname, DBLE(ztime))
          dataname = datagroup//'/data'
          CALL putarr(fid, dataname, field_buf)
          CALL closef(fid)
       END IF ! me_loc
       DEALLOCATE(field_buf)
    END IF ! local_grid
    IF (me_loc.EQ.0) CALL write_onefile_xmf(file_3d, ztime, dataname, fieldname)
    !
  END SUBROUTINE write_onefile_h5
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE write_onefile_xmf(filename,ztime,dataname,fieldname)
    !
    ! Write xmf wrapper (xdmf format) to the XDMF file
    !
    CHARACTER(*), INTENT(IN) :: filename     !! Output file name
    CHARACTER(*), INTENT(IN) :: fieldname    !! Output field name
    INTEGER, INTENT(IN) :: ztime
    CHARACTER(*), INTENT(IN) :: dataname     !! Output file name
    LOGICAL :: file_exists
    CHARACTER(len=64) :: str1='    <DataItem Dimensions="'
    CHARACTER(len=64) :: str2='" NumberType="Float" Precision="4" Format="HDF">'
    CHARACTER(len=3) :: str3='"/>'
    ! Chech if the file was already opened
    INQUIRE(FILE=TRIM(filename)//'.xmf', EXIST=file_exists)
    !
    IF (.NOT.file_exists) THEN
       OPEN (fid_xmf, FILE=TRIM(filename)//'.xmf', STATUS='REPLACE', FORM='FORMATTED')
       WRITE(fid_xmf,*) '<!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>'
       WRITE(fid_xmf,*) '<Xdmf Version="2.0">'
       WRITE(fid_xmf,*) '  <Domain>'
       WRITE(fid_xmf,*) '  <Grid Name="TimeCells" GridType="Collection" CollectionType="Temporal">'
    ELSE
       OPEN (fid_xmf, FILE=TRIM(filename)//'.xmf', STATUS='OLD', FORM='FORMATTED', POSITION='APPEND')
    END IF
    WRITE(fid_xmf,'(a, i6.6, a)')  '  <Time Value="',ztime,'" />'
    WRITE(fid_xmf,*)               '  <Grid Name="mesh3D" GridType="Uniform">'
    WRITE(fid_xmf,'(A53,3I5,A4)')  '  <Topology TopologyType="3DSMesh" NumberOfElements="',&
         &                       mesh_size(3),mesh_size(2),mesh_size(1),str3
    WRITE(fid_xmf,*)               '    <Geometry GeometryType="X_Y_Z">'
    WRITE(fid_xmf,'(A27,3I5,A50)') str1, mesh_size(3),mesh_size(2),mesh_size(1), str2
    WRITE(fid_xmf,'(     a)')           TRIM(filename)//'.h5:/meshx'
    WRITE(fid_xmf,*)               '      </DataItem>'
    WRITE(fid_xmf,'(A27,3I5,A50)') str1, mesh_size(3),mesh_size(2),mesh_size(1), str2
    WRITE(fid_xmf,'(     a)')           TRIM(filename)//'.h5:/meshy'
    WRITE(fid_xmf,*)               '      </DataItem>'
    WRITE(fid_xmf,'(A27,3I5,A50)') str1, mesh_size(3),mesh_size(2),mesh_size(1), str2
    WRITE(fid_xmf,'(     a)')           TRIM(filename)//'.h5:/meshz'
    WRITE(fid_xmf,*)               '      </DataItem>'
    WRITE(fid_xmf,*)               '      </Geometry>'
    WRITE(fid_xmf,*)               '      <Attribute Name="'//TRIM(fieldname)//'" AttributeType="Scalar" Center="Node">'
    WRITE(fid_xmf,'(A27,3I5,A50)') str1, mesh_size(3),mesh_size(2),mesh_size(1), str2
    WRITE(fid_xmf,'(     a)')           TRIM(filename)//'.h5:'//dataname
    WRITE(fid_xmf,*)               '     </DataItem>'
    WRITE(fid_xmf,*)               '  </Attribute> '
    WRITE(fid_xmf,*)               '  </Grid> '
    CLOSE (fid_xmf)
  END SUBROUTINE write_onefile_xmf
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE SET_3D_PARAMETERS(param3d)
    TYPE(DIAG3D_PARAMETERS), INTENT(IN) :: param3d
    !
    ! Set module variables
    !
    mesh_type =      param3d%mesh_type
    mesh_size =      param3d%mesh_size
    data_type =      param3d%data_type
    output_format =  param3d%output_format
    slim =           param3d%slim
    chilim =         param3d%chilim
    philim =         param3d%philim
    forcelocal =     param3d%forcelocal
    get_coords =>    param3d%coordf_ptr
    !
  END SUBROUTINE SET_3D_PARAMETERS
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE WRITE_3D_PARAMETERS()
    ! Write a summart of the 3d module parameter to std output
    WRITE(*,*) '##################################################################'
    WRITE(*,*) '3D output parameters'
    WRITE(*,*) ''
    WRITE(*,*) 'Output format: ', output_format
    WRITE(*,*) 'Output format type: ', output_format_type
    WRITE(*,*) 'Mesh type(sc: s, chi, phi; fa: field aligned): ', mesh_type
    WRITE(*,*) 'Data type( s: single precision; d: double precision): ', data_type
    WRITE(*,"(a,i0,'x',i0,'x',i0)") ' Mesh size: ', mesh_size(1), mesh_size(2), mesh_size(3)
    WRITE(*,"(a,f6.3,' -',f6.3)") ' s mesh boundaries:', slim(1), slim(2)
    WRITE(*,"(a,f6.3,' -',f6.3)") ' chi mesh boundaries:', chilim(1), chilim(2)
    WRITE(*,"(a,f6.3,' -',f6.3)") ' phi mesh boundaries:', philim(1), philim(2)
    WRITE(*,*) 'Force parallelized grid: ', forcelocal
    WRITE(*,*) 'The grid is parallelized: ', local_grid
    WRITE(*,*) 'Number of slices per procs: ', nphip
    WRITE(*,*) '##################################################################'
    !
  END SUBROUTINE WRITE_3D_PARAMETERS
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE SET_3D_GRID()
    ! Generate 3D grid (s, chi, phi)
    !
    INTEGER :: ii, jj, zz
    INTEGER :: drank_displ !correction to the grid shift in the parallelized dimension
    !
    ! Parallelized dimension, local size
    SELECT CASE(mesh_type)
    CASE('sc')
       ! Check if the grid is local
       CALL CHECK_IF_LOCAL(drank_displ)
       IF (local_grid) THEN
          nphip = mesh_size(3)/nvp_loc
          pshift = me_loc*nphip           ! set the shift in parallel arrays
       ELSE
          nphip = mesh_size(3)
          pshift = 0
       END IF
    CASE('fa')
       drank_displ = 1
       local_grid = .FALSE.
       nphip = mesh_size(3)
       pshift = 0
    END SELECT
    !
    ! Allocate grid arrays
    !
    ALLOCATE( sgrid_loc(1:mesh_size(1)), chigrid_loc(1:mesh_size(2)), phigrid_loc(1:mesh_size(3)))
    !
    ! Grid is shifted by 1/2 d*grid compared to *lim(1)
    dsgrid = (slim(2)-slim(1)) / REAL(mesh_size(1),KIND(dsgrid))
    dchigrid = (chilim(2)-chilim(1)) /REAL(mesh_size(2),KIND(dchigrid))
    dphigrid = (philim(2)-philim(1)) /REAL(mesh_size(3),KIND(dphigrid))
    sgrid_loc   = (/ (slim(1)+(ii-0.5)*dsgrid,   ii=1,mesh_size(1)) /)
    chigrid_loc = (/ (chilim(1)+(jj-0.5)*dchigrid,    jj=1,mesh_size(2)) /)
    phigrid_loc =  (/ (philim(1)+(zz-1+0.5/drank_displ)*dphigrid,    zz=1,mesh_size(3)) /)
    !
  END SUBROUTINE SET_3D_GRID
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE CHECK_IF_LOCAL(drank)
    ! TESTS
    INTEGER, INTENT(OUT) :: drank
    LOGICAL :: size_check=.TRUE., low_check=.TRUE., up_check=.TRUE., ltmp
    INTEGER, DIMENSION(:), ALLOCATABLE :: loc_ranks
    INTEGER :: ii, ierr, ginit, gsg, comm_new, tmp1, tmp2
    DOUBLE PRECISION :: dphi_procs
    !
    drank = 1
    !
    IF (philim(1) .GT. 0) low_check=.FALSE.
    IF ((phi_up_default-philim(2)) .GT. 0.000001) THEN
       up_check=.FALSE.
    ELSE
       philim(2) = phi_up_default
    END IF
    ! If forcelocal and the grid boundaries do not correspond to the
    ! default ones, recompute the grid parameters to get a local grid
    IF (((.NOT.low_check).OR.(.NOT.up_check)).AND.forcelocal) THEN
       dphi_procs = phi_up_default/(nvp_loc*nphip)
       tmp1 = FLOOR(philim(1)/dphi_procs)
       tmp2 = CEILING(philim(2)/dphi_procs)
       tmp1 = FLOOR(DBLE(tmp1)/nphip)
       tmp2 = CEILING(DBLE(tmp2)/nphip)
       philim(1) = tmp1*nphip*dphi_procs
       philim(2) = (tmp2*nphip-1)*dphi_procs
       mesh_size(3) = (tmp2 - tmp1) * nphip
       low_check = .TRUE.
       up_check = .TRUE.
    END IF
    ! Check if the mesh is local, and create a subcommincator if needed
    IF ((mesh_size(3)/nvp_loc.LT.1).OR.forcelocal) THEN
       ! check if the mesh grid is a subset of the main grid
       ltmp = (MOD(nvp_loc,mesh_size(3)).EQ.0).AND.low_check.AND.up_check
       IF (ltmp.OR.forcelocal) THEN
          ! create a new communicator for the subset mesh grid
          ! stride between the ranks of old communicator
          IF (lverbose) THEN
             WRITE(*,*) '##################################################################'
             WRITE(*,*) 'WARNING: 3D phi mesh is a subset of code grid'
          END IF
          IF(forcelocal) THEN
             ALLOCATE(loc_ranks(tmp2-tmp1))
             drank = 1
             ! select new ranks
             loc_ranks = (/ (ii*drank,   ii=tmp1,tmp2-1) /)
          ELSE
             ALLOCATE(loc_ranks(mesh_size(3)))
             drank = nvp_loc/mesh_size(3)
             ! select new ranks
             loc_ranks = (/ ((ii-1)*drank,   ii=1,mesh_size(3)) /)
          END IF
          ! create mpi group
          CALL MPI_COMM_GROUP(comm_loc, ginit, ierr)
          CALL MPI_GROUP_INCL(ginit, size(loc_ranks), loc_ranks, gsg, ierr)
          CALL MPI_COMM_CREATE(comm_loc, gsg, comm_new, ierr)
          CALL MPI_GROUP_FREE(gsg, ierr)
          CALL MPI_GROUP_FREE(ginit, ierr)
          comm_loc = comm_new
          IF (comm_loc.EQ.MPI_COMM_NULL) THEN
             me_active = .FALSE.
             me_loc = 0
             nvp_loc = SIZE(loc_ranks)
          ELSE
             me_loc = floor(DBLE(me_loc)/DBLE(drank))
             CALL MPI_COMM_SIZE(comm_loc, nvp_loc, ierr)
          END IF
          IF (lverbose) THEN
             WRITE(*,*) 'WARNING: 3D communicator redefined'
             WRITE(*,*) '3D communicator domains: ', nvp_loc
             WRITE(*,'(a,10000i5)') 'Selected ranks (from cart communicator) :', loc_ranks
             WRITE(*,*) '##################################################################'
          END IF
          DEALLOCATE(loc_ranks)
       ELSE
          size_check=.FALSE.
       END IF
    ELSE ! if mesh size > nvp_loc
       IF (MOD(mesh_size(3),nvp_loc).NE.0) THEN
          size_check=.FALSE.
       END IF
    END IF
    IF (size_check .AND. low_check .AND. up_check) local_grid=.TRUE.
    IF (lverbose) THEN
       IF (.NOT.local_grid) THEN
          ! If the grid is not local: issuing a warning that the 3D output would not be parallel
          WRITE(*,*) '##################################################################'
          WRITE(*,*) 'WARNING: 3D phi mesh is not consistent with cartesian communicator'
          WRITE(*,*) 'WARNING: 3D ouput will not be parallel'
          WRITE(*,*) 'Check on mesh size: ', size_check
          WRITE(*,*) '    Mesh size: ', mesh_size(3), '; Number of domains: ', nvp_loc
          WRITE(*,*) 'Chech on mesh lower bound: ', low_check
          WRITE(*,*) '    Mesh lower bound: ', philim(1), '; Required: 0'
          WRITE(*,*) 'Chech on mesh upper bound: ', up_check
          WRITE(*,*) '    Mesh upper bound: ', philim(2), '; Required: 2PI'
          WRITE(*,*) '##################################################################'
       END IF
    END IF
  END SUBROUTINE CHECK_IF_LOCAL
  !
  !---------------------------------------------------------------------------
  !
  SUBROUTINE checkout3dfile(out_field)
    CHARACTER(*), INTENT(IN) :: out_field
    CHARACTER(len=36) :: filename
    INTEGER :: ierr
    IF (me_active.AND.(me_loc.EQ.0)) THEN
       SELECT CASE(output_format)
       CASE('XDMF_one')
          ! Generate filename according to output field
          filename='output_3d_'//TRIM(out_field)
          OPEN (fid_xmf, FILE=TRIM(filename)//'.xmf', STATUS='OLD', FORM='FORMATTED', POSITION='APPEND')
          WRITE(fid_xmf,*)               '  </Grid> '
          WRITE(fid_xmf,*)               '  </Domain> '
          WRITE(fid_xmf,*)               '</Xdmf>  '
       END SELECT
    END IF
  END SUBROUTINE checkout3dfile
  !
  !---------------------------------------------------------------------------
  !
END MODULE vis3d
