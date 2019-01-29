    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name
    CHARACTER(len=*), INTENT(in), OPTIONAL :: desc
    LOGICAL, INTENT(in), OPTIONAL :: compress
    INTEGER, INTENT(in), OPTIONAL :: pardim
    INTEGER, INTENT(in), OPTIONAL :: ionode
!
    INTEGER(HID_T) ::  dtype
    INTEGER(HID_T) :: dspace_id, id, did, cprop_id, memspace_id, plist_id
    INTEGER(HSIZE_T), DIMENSION(SIZE(SHAPE(array))) :: dims, offset, count
    INTEGER :: rank, pdim, nlocal, nglobal, start, ierr
    LOGICAL :: nlio
!
    INTEGER(HID_T) ::  complex_type, realpart, impart
!
    id = file_id(fid)                   ! file id
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
!  Am I the io node ?
    nlio = .TRUE.
    IF( ispara(fid) .AND. PRESENT(ionode) ) THEN ! Ignore "ionode" if not parallel IO
       nlio = mpi_rank(fid) .EQ. ionode
    END IF
!
!  Data transfer property (default is H5FD_MPIO_INDEPENDENT_F)
    CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, ierr)
!
!  Partial read/write
    CALL h5pset_preserve_f(plist_id, .TRUE., ierr)
!
!  Memory dataspace
    dims = SHAPE(array)
    rank = SIZE(dims)
    IF( ispara(fid) .AND. PRESENT(pardim) ) THEN
       pdim=pardim
       count(1:rank) = dims(1:rank)
       offset(1:rank) = 0
       nlocal = count(pdim)
       CALL part1d( file_comm(fid), nlocal, start, nglobal)
       dims(pdim) = nglobal
       offset(pdim) = start
       CALL h5screate_simple_f(rank, count, memspace_id, ierr)
    ELSE
       CALL h5screate_simple_f(rank, dims, memspace_id, ierr)
    END IF
!
!  File dataspace
    CALL h5screate_simple_f(rank, dims, dspace_id, ierr)
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
!  IO node write to dataset
    IF( ispara(fid) .AND. PRESENT(pardim) ) THEN
       CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, offset, count, ierr)
       CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, ierr)
    END IF
    IF( nlio ) THEN
       temp = REAL(array)
       CALL h5dwrite_f(did, realpart, temp, count, ierr, memspace_id, &
               &       dspace_id, plist_id)
       temp = AIMAG(array)
       CALL h5dwrite_f(did, impart, temp, count, ierr, memspace_id, &
               &       dspace_id, plist_id)
    END IF
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
