    INCLUDE "mpif.h"
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name
    INTEGER, INTENT(in) :: pardim(:)
    INTEGER, INTENT(in), OPTIONAL :: garea(:)
    CHARACTER(len=*), INTENT(in), OPTIONAL :: desc
    LOGICAL, INTENT(in), OPTIONAL :: compress
!
    INTEGER(HID_T) ::  dtype
    INTEGER(HID_T) :: dspace_id, id, did, cprop_id, memspace_id, plist_id
    INTEGER(HSIZE_T), DIMENSION(SIZE(SHAPE(array))) :: locdims, dims, mdims, offsets
    INTEGER :: rank, pdim, start, nglobal, nlocal, ierr, i, maxdims, sub, topokind
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
    CASE('R')
       CALL h5tcopy_f(H5T_NATIVE_DOUBLE, dtype, ierr)
    CASE('S')
       CALL h5tcopy_f(H5T_NATIVE_REAL, dtype, ierr)
    CASE('I')
       CALL h5tcopy_f(H5T_NATIVE_INTEGER, dtype, ierr)
    END SELECT
!
!  Data transfer property (default is H5FD_MPIO_INDEPENDENT_F)
    CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, ierr)
!
!  Memory dataspace
    locdims = SHAPE(array)   ! local dim sizes
    rank = SIZE(locdims)
    CALL h5screate_simple_f(rank, locdims, memspace_id, ierr)
    offsets = 0
    mdims = locdims
    IF( PRESENT(garea) ) THEN   ! Exclude ghost area in memory
       DO i=1,SIZE(pardim)
          pdim = pardim(i)
          mdims(pdim) = locdims(pdim) - 2*garea(i)
          offsets(pdim) = garea(i)
       END DO
    END IF
    CALL h5sselect_hyperslab_f(memspace_id, H5S_SELECT_SET_F, offsets, mdims, ierr)
!
!  File dataspace
    CALL mpi_cartdim_get(file_comm(fid), maxdims, ierr)
    ALLOCATE(remdim(maxdims))
    dims = mdims        ! Init global dims
    offsets = 0         ! and offsets
    DO i=1,SIZE(pardim)
       pdim = pardim(i)
       remdim = .FALSE.
       remdim(i) = .TRUE.
       CALL mpi_cart_sub(file_comm(fid), remdim, sub, ierr)
       nlocal = mdims(pdim)
       CALL part1d(sub, nlocal, start, nglobal)
       dims(pdim) = nglobal
       offsets(pdim) = start
       CALL mpi_comm_free(sub, ierr)
    END DO
    CALL h5screate_simple_f(rank, dims, dspace_id, ierr)
    DEALLOCATE(remdim)
!
!  Compress data (with gzip) if required
    CALL h5pcreate_f(H5P_DATASET_CREATE_F, cprop_id, ierr)
    IF( PRESENT(compress) ) THEN
       IF(compress) THEN
          CALL h5pset_chunk_f(cprop_id, rank, dims, ierr)
          CALL h5pset_deflate_f(cprop_id, 6, ierr)
       END IF
    END IF
!
!  Create dataset
    IF( ctype .EQ. 'S' .OR. ctype .EQ. 'R' ) THEN
       CALL h5dcreate_f(id, name, prec(fid), dspace_id, did, ierr, cprop_id)
    ELSE IF( ctype .EQ. 'I' ) THEN
       CALL h5dcreate_f(id, name, H5T_STD_I32LE, dspace_id, did, ierr, cprop_id)
    END IF
!
!  Write to dataset
    CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, offsets, mdims, ierr)
    CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, ierr)
    CALL h5dwrite_f(did, dtype, array, locdims, ierr, &
         &       memspace_id, dspace_id, plist_id)
!
    IF( PRESENT(desc) ) THEN
       CALL annote(did, desc)
    END IF
!
    CALL h5tclose_f(dtype, ierr)
    CALL h5pclose_f(cprop_id, ierr)
    CALL h5pclose_f(plist_id, ierr)
    CALL h5sclose_f(memspace_id, ierr)
    CALL h5sclose_f(dspace_id, ierr)
    CALL h5dclose_f(did, ierr)
!
