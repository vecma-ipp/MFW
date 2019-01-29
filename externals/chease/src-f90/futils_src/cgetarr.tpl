    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name
    INTEGER, INTENT(in), OPTIONAL :: pardim
    INTEGER, INTENT(in), OPTIONAL :: ionode
    INTEGER, INTENT(in), OPTIONAL :: offsets(SIZE(SHAPE(array)))
!
    INTEGER(HID_T) :: dtype, realpart, impart
    INTEGER(HID_T) :: id, did, dspace_id, memspace_id, plist_id
    INTEGER(HSIZE_T), DIMENSION(SIZE(SHAPE(array))) :: dims, mdims, maxdims
    INTEGER(HSIZE_T), DIMENSION(SIZE(SHAPE(array))) :: starts, counts
    INTEGER :: i, s, rank, mrank, nlocal, nglobal, pdim, ierr
    LOGICAL :: nlio
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
!
!  Selection on file space based on 1D parallel partition
    starts = 0
    counts = dims ! The whole dataset in file
    IF( ispara(fid) .AND. PRESENT(pardim) ) THEN
       pdim = pardim
       IF( .NOT. ispara(fid) ) THEN
          PRINT*, 'File was not open with paralleilization option'
          STOP
       END IF
       nlocal = mdims(pdim) ! Assume the part. was specified by array shape.
       CALL part1d( file_comm(fid), nlocal, s, nglobal)
       starts(pdim) = s
       counts(pdim) = nlocal
       CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, starts, counts, ierr)
    END IF
!
!  Select slice in file
    IF( PRESENT(offsets) ) THEN
       IF( PRESENT(pardim) ) THEN
          PRINT*, 'GETARR: Distributed array not allowed for partial read!'
          STOP
       END IF
       counts(rank) = mdims(rank)
       starts = offsets
       CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, starts, counts, ierr)
    END IF
!
!  Selection of memory space
    starts = 0
    CALL h5sselect_hyperslab_f(memspace_id, H5S_SELECT_SET_F, starts, counts, ierr)
!
!  Check consistency between file and memory dataspace
    DO i=1,rank
       IF( mdims(i) .LT. counts(i) ) THEN
          PRINT*, 'dim.', i, ' too small while reading ', &
               &   name(1:LEN_TRIM(name))
          STOP
       END IF
    END DO
!
!  IO node read dataset
    IF( ispara(fid) .AND. PRESENT(pardim) ) THEN
       CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, ierr)
    END IF
    IF( nlio ) THEN
       CALL h5dread_f(did, realpart, temp1, dims, ierr, memspace_id, dspace_id, plist_id)
       CALL h5dread_f(did, impart, temp2, dims, ierr, memspace_id, dspace_id, plist_id)
       array = CMPLX(temp1, temp2)
    END IF
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
