    INCLUDE "mpif.h"
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name
    INTEGER, INTENT(in) :: pardim(:)
    INTEGER, INTENT(in), OPTIONAL :: garea(:)
!
    INTEGER(HID_T) :: dtype, realpart, impart
    INTEGER(HID_T) :: id, did, dspace_id, memspace_id, plist_id
    INTEGER(HSIZE_T), DIMENSION(SIZE(SHAPE(array))) :: dims, mdims, maxdims
    INTEGER(HSIZE_T), DIMENSION(SIZE(SHAPE(array))) :: offsets, locdims
    INTEGER :: i, s, rank, mrank, nlocal, nglobal, pdim, ierr, mxdims, sub, topokind
    LOGICAL, ALLOCATABLE :: remdim(:)
!
    id = file_id(fid)                   ! file id
    CALL mpi_topo_test(file_comm(fid), topokind, ierr)
    IF( topokind .NE. MPI_CART) THEN
       PRINT*, 'Cartesian Topology attached to the communicator required!'
       CALL mpi_abort(MPI_COMM_WORLD, -9, ierr)
    END IF
!
!  Data type of array (memory)
    SELECT CASE (ctype)
    CASE('C')
       CALL h5tcopy_f(H5T_NATIVE_REAL, dtype, ierr)
    CASE('Z')
       CALL h5tcopy_f(H5T_NATIVE_DOUBLE, dtype, ierr)
    END SELECT
    CALL mem_complex(dtype, realpart, impart)
!
!  Data transfer property (default is H5FD_MPIO_INDEPENDENT_F)
    CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, ierr)
!
!  Partial read/write
    CALL h5pset_preserve_f(plist_id, .TRUE., ierr)
!
!  Get dataset from file
    CALL h5dopen_f(id, name, did, ierr)
!
!  Get file dataspace
    CALL h5dget_space_f(did, dspace_id, ierr)
    CALL h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, ierr)
    rank = ierr
!
!  Set memory dataspace
    mdims=SHAPE(array)
    mrank = SIZE(mdims)
    IF( mrank .NE. rank) THEN
       PRINT*, 'rank mismatch while reading ', name(1:LEN_TRIM(name))
       STOP
    END IF
    CALL h5screate_simple_f(rank, mdims, memspace_id, ierr)
    offsets = 0
    IF( PRESENT(garea) ) THEN   ! Exclude ghost area in memory
       DO i=1,SIZE(pardim)
          pdim = pardim(i)
          mdims(pdim) = mdims(pdim) - 2*garea(i)
          offsets(pdim) = garea(i)
       END DO
    END IF
    CALL h5sselect_hyperslab_f(memspace_id, H5S_SELECT_SET_F, offsets, mdims, ierr)
!
!  Selection on file space based on processor grid
    CALL mpi_cartdim_get(file_comm(fid), mxdims, ierr)
    ALLOCATE(remdim(mxdims))
    locdims = dims ! The whole dataset in file
    offsets = 0
    DO i=1,SIZE(pardim)
       pdim = pardim(i)
       remdim = .FALSE.
       remdim(i) = .TRUE.
       CALL mpi_cart_sub(file_comm(fid), remdim, sub, ierr)
       nlocal = mdims(pdim)
       CALL part1d(sub, nlocal, s, nglobal)
       offsets(pdim) = s
       locdims(pdim) = nlocal
       CALL mpi_comm_free(sub, ierr)
    END DO
    CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, offsets, locdims, ierr)
    DEALLOCATE(remdim)
!!$!
!!$!  Selection of memory space
!!$    offsets = 0
!!$    CALL h5sselect_hyperslab_f(memspace_id, H5S_SELECT_SET_F, offsets, mdims, ierr)
!
!  Check consistency between file and memory dataspace
    DO i=1,rank
       IF( mdims(i) .LT. locdims(i) ) THEN
          PRINT*, 'dim.', i, ' too small while reading ', &
               &   name(1:LEN_TRIM(name))
          STOP
       END IF
    END DO
!
!  Read dataset
    CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, ierr)
    CALL h5dread_f(did, realpart, temp1, dims, ierr, memspace_id, dspace_id, plist_id)
    CALL h5dread_f(did, impart, temp2, dims, ierr, memspace_id, dspace_id, plist_id)
    array = CMPLX(temp1, temp2)
!
    CALL h5tclose_f(dtype, ierr)
    CALL h5tclose_f(realpart, ierr)
    CALL h5tclose_f(impart, ierr)
!
    CALL h5pclose_f(plist_id, ierr)
    CALL h5sclose_f(dspace_id, ierr)
    CALL h5sclose_f(memspace_id, ierr)
    CALL h5dclose_f(did, ierr)
!
