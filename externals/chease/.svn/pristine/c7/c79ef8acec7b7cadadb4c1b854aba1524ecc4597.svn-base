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
    INTEGER(HID_T) ::  complex_type, realpart, impart
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
!  File datatype
    CALL file_complex(prec(fid), complex_type)
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
    CALL h5dcreate_f(id, name, complex_type, dspace_id, did, ierr, cprop_id)
!
!  Write to dataset
    CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, offsets, mdims, ierr)
    CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, ierr)
    temp = REAL(array)
    CALL h5dwrite_f(did, realpart, temp, locdims, ierr, &
         &       memspace_id, dspace_id, plist_id)
    temp = AIMAG(array)
    CALL h5dwrite_f(did, impart, temp, locdims, ierr, &
         &       memspace_id, dspace_id, plist_id)
!
    IF( PRESENT(desc) ) THEN
       CALL annote(did, desc)
    END IF
!
    CALL h5tclose_f(complex_type, ierr)
    CALL h5tclose_f(dtype, ierr)
    CALL h5tclose_f(realpart, ierr)
    CALL h5tclose_f(impart, ierr)
!
    CALL h5pclose_f(cprop_id, ierr)
    CALL h5pclose_f(plist_id, ierr)
    CALL h5sclose_f(memspace_id, ierr)
    CALL h5sclose_f(dspace_id, ierr)
    CALL h5dclose_f(did, ierr)
!
