    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name
    INTEGER, INTENT(in), OPTIONAL :: pardim
    INTEGER, INTENT(in), OPTIONAL :: ionode
    INTEGER, DIMENSION(:), INTENT(in), OPTIONAL :: offset
!
    INTEGER(HID_T) :: id, did, dspace_id, memspace_id, plist_id
    INTEGER(HSIZE_T), DIMENSION(SIZE(SHAPE(array))+1) :: &
         &   dims, maxdims, starts, counts, offset_out
    INTEGER(HSIZE_T), DIMENSION(SIZE(SHAPE(array))) :: ddims
    INTEGER :: n, rank, rank_array, ierr
    INTEGER :: pdim, s, nlocal, nglobal
    LOGICAL :: nlio
!
    id = file_id(fid)                   ! file id
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
!  Get dims/rank of dataset
    CALL h5dopen_f(id, name, did, ierr)
    CALL h5dget_space_f(did, dspace_id, ierr)
    CALL h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, ierr)
    rank = ierr
    CALL h5sclose_f(dspace_id, ierr)
!
!  Memory dataspace
    ddims = SHAPE(array)
    rank_array = SIZE(ddims)
    CALL h5screate_simple_f(rank_array, ddims, memspace_id, ierr)
!
!  Determine slab of output array written
    counts(1:rank-1) = ddims(1:rank-1)
    IF( rank .EQ. rank_array ) THEN
       counts(rank) = ddims(rank_array)
    ELSE IF( rank .EQ. rank_array+1 ) THEN
       counts(rank) = 1
    ELSE
       WRITE(*, '(a,a)') "Data shape mismatch for", name(1:LEN_TRIM(name))
       WRITE(*,'(a,10i6)') 'rank, rank_array', rank, rank_array
       STOP
    END IF

    IF(PRESENT(offset)) THEN
       offset_out(1:rank-1) = offset(1:rank-1) 
       offset_out(rank) =  0    
    ELSE
       offset_out = 0	
    END IF
!
!  Extend the array in the time direction if writing a full slice.
    IF (.NOT.PRESENT(offset)) THEN	
       dims(rank) = dims(rank) + counts(rank)
       CALL h5dextend_f(did, dims, ierr)	
       CALL h5dget_space_f(did, dspace_id, ierr)	       
    END IF
!
!  Starting coordinates for write
    starts(1:rank-1) = offset_out(1:rank-1)
    starts(rank) = dims(rank) - counts(rank) + offset_out(rank)
!
!  Collective write  partitionned array
    IF(ispara(fid) .AND. PRESENT(pardim) ) THEN ! Ignore "pardim" if not parallel IO
       pdim = pardim
       nlocal = ddims(pdim)
       CALL part1d( file_comm(fid), nlocal, s, nglobal)
       starts(pdim) = s
       CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, ierr)
    END IF

    CALL h5dget_space_f(did, dspace_id, ierr)
!
!  Write to the end of dataset
    CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, starts, counts, &
         &                     ierr)
    IF( nlio) THEN
       CALL h5dwrite_f(did, H5T_NATIVE_DOUBLE, array, ddims, ierr, &
            &          memspace_id, dspace_id, plist_id)
    END IF
!
    CALL h5pclose_f(plist_id, ierr)
    CALL h5dclose_f(did, ierr) 
    CALL h5sclose_f(dspace_id, ierr)
    CALL h5sclose_f(memspace_id, ierr)
