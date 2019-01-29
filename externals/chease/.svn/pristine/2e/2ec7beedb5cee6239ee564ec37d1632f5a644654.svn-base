MODULE futils
!
!   FUTILS: MODULE OF WRAPPER ROUTINES FOR HDF5 ROUTINES.
!
!   T.M. Tran, B. McMillan, S. Brunner, P. Angelino CRPP/EPFL.
!   December 2007
!
  USE hdf5
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: creatf, openf, closef, creatg, creatd, append, putarr, getarr, &
       &    putfile, getfile, attach, getatt, getsize, isdataset, isgroup, &
       &    split, numatts, allatts, closeall, extend, putarrnd, getarrnd, &
       &    getdims, geth5ver, check_gexist, create_external_link
!
  INTEGER, PARAMETER :: nfmax=128         ! Maximum number of files
  INTEGER, PARAMETER :: maxrank=8         ! Maximum rank of arrays
  LOGICAL :: used(nfmax) = .FALSE.        ! Used fid management
  INTEGER(HID_T), SAVE :: file_id(nfmax)  ! File id
  INTEGER(HID_T), SAVE :: prec(nfmax)     ! Precision of reals
  INTEGER, SAVE ::        file_comm(nfmax)! MPI comm. associated to file
  INTEGER, SAVE ::        mpi_rank(nfmax) ! MPI process rank
  LOGICAL, SAVE ::        ispara(nfmax)   ! Is file I/O parallel
  INTEGER :: current_fid=0
  LOGICAL :: isinit=.FALSE.
!
  INTERFACE putarr
     MODULE PROCEDURE putarr1, putarr2, putarr3, putarr4, putarr5, putarr6
     MODULE PROCEDURE sputarr1, sputarr2, sputarr3, sputarr4, sputarr5, sputarr6
     MODULE PROCEDURE iputarr1, iputarr2, iputarr3, iputarr4, iputarr5, iputarr6
     MODULE PROCEDURE cputarr1, cputarr2, cputarr3, cputarr4, cputarr5, cputarr6
     MODULE PROCEDURE zputarr1, zputarr2, zputarr3, zputarr4, zputarr5, zputarr6
  END INTERFACE
  INTERFACE putarrnd
     MODULE PROCEDURE putarrnd2, putarrnd3, putarrnd4, putarrnd5, putarrnd6
     MODULE PROCEDURE sputarrnd2, sputarrnd3, sputarrnd4, sputarrnd5, sputarrnd6
     MODULE PROCEDURE iputarrnd2, iputarrnd3, iputarrnd4, iputarrnd5, iputarrnd6
     MODULE PROCEDURE cputarrnd2, cputarrnd3, cputarrnd4, cputarrnd5, cputarrnd6
     MODULE PROCEDURE zputarrnd2, zputarrnd3, zputarrnd4, zputarrnd5, zputarrnd6
  END INTERFACE
  INTERFACE getarr
     MODULE PROCEDURE getarr1, getarr2, getarr3, getarr4, getarr5, getarr6
     MODULE PROCEDURE sgetarr1, sgetarr2, sgetarr3, sgetarr4, sgetarr5, sgetarr6
     MODULE PROCEDURE igetarr1, igetarr2, igetarr3, igetarr4, igetarr5, igetarr6
     MODULE PROCEDURE cgetarr1, cgetarr2, cgetarr3, cgetarr4, cgetarr5, cgetarr6
     MODULE PROCEDURE zgetarr1, zgetarr2, zgetarr3, zgetarr4, zgetarr5, zgetarr6
  END INTERFACE
  INTERFACE getarrnd
     MODULE PROCEDURE getarrnd2, getarrnd3, getarrnd4, getarrnd5, getarrnd6
     MODULE PROCEDURE sgetarrnd2, sgetarrnd3, sgetarrnd4, sgetarrnd5, sgetarrnd6
     MODULE PROCEDURE igetarrnd2, igetarrnd3, igetarrnd4, igetarrnd5, igetarrnd6
     MODULE PROCEDURE cgetarrnd2, cgetarrnd3, cgetarrnd4, cgetarrnd5, cgetarrnd6
     MODULE PROCEDURE zgetarrnd2, zgetarrnd3, zgetarrnd4, zgetarrnd5, zgetarrnd6
  END INTERFACE
  INTERFACE append
     MODULE PROCEDURE append0, append1, append2, append3, append4
     MODULE PROCEDURE zappend0,zappend1,zappend2,zappend3,zappend4
  END INTERFACE
  INTERFACE attach
     MODULE PROCEDURE attach_i, attach_r, attach_f, attach_l, attach_s, attach_sa
  END INTERFACE
  INTERFACE getatt
     MODULE PROCEDURE getatt_i, getatt_r, getatt_f, getatt_l, getatt_s
  END INTERFACE
!
CONTAINS
!===========================================================================
  SUBROUTINE closeall(ierr)
!
!   Close all files
!
    IMPLICIT NONE
    INTEGER :: i, ierr
    DO i=1,nfmax
       IF( used(i)) THEN
          CALL closef(i)
       END IF
    END DO
!
!    Reset globals
    current_fid = 0
    isinit = .FALSE.
!
!    Flushes all data to disk, closes file identifiers,
!    and cleans up memory
    CALL h5close_f(ierr)
  END SUBROUTINE closeall
!===========================================================================
  INTEGER FUNCTION next_fid()
!
! Defiver next unudes fid
!
    IMPLICIT NONE
    INTEGER :: i
    DO i=1,nfmax
       IF( .NOT. used(i) ) THEN
          used(i) = .TRUE.
          current_fid = i
          next_fid = i
          RETURN
       END IF
    END DO
    PRINT*, "Number of created files exceeds NFMAX =", nfmax
    STOP
  END FUNCTION next_fid
!===========================================================================
  SUBROUTINE creatf(file, fid, desc, real_prec, mpicomm, mpiposix)
!
!   Create a new file with filename file and
!   a title attribute desc
!
    IMPLICIT NONE
    INCLUDE 'mpif.h'
    CHARACTER(len=*), INTENT(in) :: file
    INTEGER, INTENT(out) :: fid
    CHARACTER(len=*), INTENT(in), OPTIONAL :: desc
    CHARACTER(len=1), INTENT(in), OPTIONAL :: real_prec
    INTEGER, INTENT(in), OPTIONAL :: mpicomm
    LOGICAL, INTENT(in), OPTIONAL :: mpiposix
    INTEGER(HID_T) :: id, root_id, plist_id
    INTEGER :: ierr, mpiinfo
    LOGICAL :: mpiio
    CHARACTER(len=1) :: real_prec_saved
    CHARACTER(len=16) :: libver
    INTEGER :: l
!
! One time init f90 interface
!
    IF( .NOT. isinit) THEN
       CALL h5open_f(ierr)
       isinit = .TRUE.
    END IF
!
    fid = next_fid()
!
! Parallel/serial acces to file
!
    ispara(fid) = .FALSE.   ! Serial by default
    IF( PRESENT(mpicomm) ) THEN
       ispara(fid) = .TRUE.
       file_comm(fid) = mpicomm
       CALL mpi_comm_rank(mpicomm, mpi_rank(fid), ierr)
       mpiinfo = MPI_INFO_NULL
       mpiio = .TRUE.
       CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, ierr)
       IF( PRESENT(mpiposix) ) THEN
          IF( mpiposix ) mpiio = .FALSE.
       END IF
       IF( mpiio ) THEN
          CALL h5pset_fapl_mpio_f(plist_id, mpicomm, mpiinfo, ierr)
       ELSE
          CALL h5pset_fapl_mpiposix_f(plist_id, mpicomm, .FALSE., ierr)
       END IF
       CALL h5fcreate_f(file, H5F_ACC_TRUNC_F, id, ierr, access_prp=plist_id)
       CALL h5pclose_f(plist_id, ierr)
    ELSE
       CALL h5fcreate_f(file, H5F_ACC_TRUNC_F, id, ierr)
    END IF
    file_id(fid) = id
!
! Precision of REALs for this file
!
    real_prec_saved = 's'       ! Use 32 bit real by default
    prec(fid) = H5T_NATIVE_REAL
    IF( PRESENT(real_prec) ) THEN
       IF( real_prec .EQ. 'd' .OR. real_prec .EQ. 'D') THEN
          real_prec_saved = 'd'
          prec(fid) = H5T_NATIVE_DOUBLE
       END IF
    END IF
    CALL attach(fid, '/', 'prec', real_prec_saved)
!
! Description of this file
!
    IF( PRESENT(desc) ) THEN
       CALL h5gopen_f(id, "/", root_id, ierr)
       CALL annote(root_id, desc)
       CALL h5gclose_f(root_id, ierr)
    END IF
!
!  HDF5 library version
!
    CALL geth5ver(libver, l)
    CALL attach(fid, '/', 'h5ver', libver(1:l))
!
  END SUBROUTINE creatf
!==========================================================================
  SUBROUTINE openf(file, fid, mode, real_prec, mpicomm, mpiposix)
!
!   Open an existing file with filename file
!
    IMPLICIT NONE
    INCLUDE 'mpif.h'
    CHARACTER(len=*), INTENT(in) :: file
    INTEGER, INTENT(out) :: fid
    CHARACTER(len=*), OPTIONAL, INTENT(in) :: mode
    CHARACTER(len=1), OPTIONAL, INTENT(in) :: real_prec
    INTEGER, INTENT(in), OPTIONAL :: mpicomm
    LOGICAL, INTENT(in), OPTIONAL :: mpiposix
    INTEGER(HID_T) :: id, plist_id
    INTEGER :: acc, ierr, mpiinfo
    LOGICAL :: found, mpiio
    CHARACTER(len=1) :: real_prec_read
!
! Check if file exists
!
    INQUIRE(file=file, exist=found)
    IF( .NOT. found) THEN
       PRINT*, 'HDF5 file ' // TRIM(file) // ' not found!'
       STOP
    END IF
!
! One time init f90 interface
!
    IF( .NOT. isinit) THEN
       CALL h5open_f(ierr)
       isinit = .TRUE.
    END IF
!
    acc = H5F_ACC_RDWR_F
    IF( PRESENT(mode) ) THEN
       IF( mode(1:1) .EQ. 'r'.OR. mode(1:1) .EQ. 'R' ) THEN
          acc = H5F_ACC_RDONLY_F
       END IF
    END IF
!
    fid = next_fid()
!
    ispara(fid) = .FALSE.   ! Serial by default
    IF( PRESENT(mpicomm) ) THEN
       ispara(fid) = .TRUE.
       file_comm(fid) = mpicomm
       CALL mpi_comm_rank(mpicomm, mpi_rank(fid), ierr)
       mpiinfo = MPI_INFO_NULL
       mpiio = .TRUE.
       CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, ierr)
       IF( PRESENT(mpiposix) ) THEN
          IF( mpiposix ) mpiio = .FALSE.
       END IF
       IF( mpiio ) THEN
          CALL h5pset_fapl_mpio_f(plist_id, mpicomm, mpiinfo, ierr)
       ELSE
          CALL h5pset_fapl_mpiposix_f(plist_id, mpicomm, .FALSE., ierr)
       END IF
       CALL h5fopen_f(file, acc, id, ierr, plist_id)
       CALL h5pclose_f(plist_id, ierr)
    ELSE
       CALL h5fopen_f(file, acc, id, ierr)
    END IF
    file_id(fid) = id
!
! Precision of REALs for this file
!
    IF(PRESENT(real_prec)) THEN
       IF( real_prec .EQ. 'd'  .OR. real_prec .EQ. 'D') THEN
          prec(fid) = H5T_NATIVE_DOUBLE
       ELSE
          prec(fid) = H5T_NATIVE_REAL
       END IF
    ELSE
       CALL getatt(fid, '/', 'prec', real_prec_read, ierr)
       IF( ierr .NE. -2) THEN
          IF( real_prec_read .EQ. 'd' ) THEN
             prec(fid) = H5T_NATIVE_DOUBLE
          ELSE
             prec(fid) = H5T_NATIVE_REAL
          END IF
       ELSE   ! Fallback to old versions
          CALL getatt(fid, '/', 'prec', prec(fid), ierr)
       END IF
    END IF
!
  END SUBROUTINE openf
!===========================================================================
  SUBROUTINE creatg(fid, name, desc)
!
!   Create a group in fid
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in):: name
    INTEGER(HID_T) :: id, gid
    CHARACTER(len=*), INTENT(in), OPTIONAL :: desc
    INTEGER :: ierr
!
    id = file_id(fid)      ! file id
    CALL h5gcreate_f(id, name, gid, ierr)
        IF( PRESENT(desc) ) THEN
       CALL annote(gid, desc)
    END IF
    CALL h5gclose_f(gid, ierr)
  END SUBROUTINE creatg
!===========================================================================
  SUBROUTINE creatd(fid, r, d, name, desc, compress, pardim, chunking, &
       &            iscomplex)
!
!   Dataset for arrays of rank r and shape d, with UNLIMITED
!   size for the r+1 dimension (Note: d is unused when r=0)
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid, r, d(:)
    CHARACTER(len=*), INTENT(in):: name
    CHARACTER(len=*), INTENT(in), OPTIONAL :: desc
    LOGICAL, INTENT(in), OPTIONAL :: compress, iscomplex
    INTEGER, INTENT(in), OPTIONAL :: pardim
    INTEGER, INTENT(in),DIMENSION(:), OPTIONAL :: chunking
!
    INTEGER(HSIZE_T), DIMENSION(r+1) :: dims, maxdims
    INTEGER(HID_T) :: id, space_id, cprop_id, did, complex_type
    INTEGER :: rank, ierr
    INTEGER :: pdim, nlocal, s, nglobal
    LOGICAL :: iskomplex
!
    id = file_id(fid)      ! file id
!
!  Define file dataspace
    rank = r+1
    IF( r .GT. 0 ) THEN
       dims(1:rank-1) = d(1:r)
    END IF
    dims(rank) = 0 ! Initial time dimension
!
!  Take into account the 1d partition
    IF( ispara(fid) .AND. r .GT. 0 ) THEN
       pdim = r   ! By default, the last dim is partitionned
       IF( PRESENT(pardim) ) pdim=pardim
       nlocal = dims(pdim)
       CALL part1d(file_comm(fid), nlocal, s, nglobal)
       dims(pdim) = nglobal
    END IF
    maxdims = dims
    maxdims(rank) = H5S_UNLIMITED_f
    CALL h5screate_simple_f(rank, dims, space_id, ierr, maxdims)
!
!  Define chunking for last dimension
    CALL h5pcreate_f(H5P_DATASET_CREATE_F, cprop_id, ierr)
    dims(rank) = 4
!  Standard chunking mean that we always do I/O on an entire slice.
!  Efficient when writing whole timeslices: otherwise should set
!  chunking appropriately.
    IF(PRESENT(chunking)) THEN
       dims(1:size(chunking))=chunking
    END IF
    CALL h5pset_chunk_f(cprop_id, rank, dims, ierr)
!
!  Compress data (with gzip) if required
    IF( PRESENT(compress) ) THEN
       IF(compress) CALL h5pset_deflate_f(cprop_id, 6, ierr)
    END IF
!
! Create data set
    iskomplex = .FALSE.
    IF( PRESENT(iscomplex) ) iskomplex = iscomplex
    IF( iskomplex ) THEN
       CALL file_complex(prec(fid), complex_type)
       CALL h5dcreate_f(id, name, complex_type, space_id, did, ierr, cprop_id)
       CALL h5tclose_f(complex_type, ierr)
    ELSE
       CALL h5dcreate_f(id, name, prec(fid), space_id, did, ierr, cprop_id)
    END IF
    IF( PRESENT(desc) ) THEN
       CALL annote(did, desc)
    END IF
!
    CALL h5sclose_f(space_id, ierr)
    CALL h5pclose_f(cprop_id, ierr)
    CALL h5dclose_f(did, ierr)
  END SUBROUTINE creatd
!===========================================================================
  SUBROUTINE append0(fid, name, scal, ionode)
!
!   Add a scalar at the end of dataset
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name
    DOUBLE PRECISION, INTENT(in) :: scal
    INTEGER, INTENT(in), OPTIONAL :: ionode
    DOUBLE PRECISION :: array(1)
    INTEGER(HID_T) :: id, did, dspace_id, memspace_id
    INTEGER(HSIZE_T), DIMENSION(1) :: dims, maxdims, starts, counts, ddims
    INTEGER :: rank, ierr
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
!  Get dims of dataset
    CALL h5dopen_f(id, name, did, ierr)
    CALL h5dget_space_f(did, dspace_id, ierr)
    CALL h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, ierr)
    rank = ierr
    CALL h5sclose_f(dspace_id, ierr)
    IF( rank .NE. 1 ) THEN
       WRITE(*, '(a,a)') "Data shape mismatch for", name(1:LEN_TRIM(name))
       STOP
    END IF
!
!  Extend the dataset in the last dimension
    starts(1) = dims(1)
    counts(1) = 1
    dims(1) = dims(1) + 1
    CALL h5dextend_f(did, dims, ierr)
    CALL h5dget_space_f(did, dspace_id, ierr)
!
!  Memory dataspace
    ddims(1) = 1
    CALL h5screate_simple_f(1, ddims, memspace_id, ierr)
!
!  Write to the end of dataset
    CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, starts, counts, &
         &                     ierr)
    array(1) = scal
    IF( nlio )  THEN
          CALL h5dwrite_f(did, H5T_NATIVE_DOUBLE, array, ddims, ierr, &
               &  memspace_id, dspace_id)
    END IF
!
    CALL h5dclose_f(did, ierr)
    CALL h5sclose_f(dspace_id, ierr)
    CALL h5sclose_f(memspace_id, ierr)
  END SUBROUTINE append0
!===========================================================================
  SUBROUTINE zappend0(fid, name, scal, ionode)
!
!   Add a DOUBLE COMPLEX scalar at the end of dataset
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name
    DOUBLE COMPLEX, INTENT(in) :: scal
    INTEGER, INTENT(in), OPTIONAL :: ionode
    DOUBLE COMPLEX :: array(1)
    DOUBLE PRECISION :: temp(SIZE(array,1))
    INTEGER(HID_T) :: id, did, dspace_id, memspace_id, plist_id
    INTEGER(HSIZE_T), DIMENSION(1) :: dims, maxdims, starts, counts, ddims
    INTEGER :: rank, ierr
    LOGICAL :: nlio
    INTEGER(HID_T) ::  realpart, impart
!
    id = file_id(fid)                   ! file id
!
!  Am I the io node ?
    nlio = .TRUE.
    IF( ispara(fid) .AND. PRESENT(ionode) ) THEN ! Ignore "ionode" if not parallel IO
       nlio = mpi_rank(fid) .EQ. ionode
    END IF
!
!  Get dims of dataset
    CALL h5dopen_f(id, name, did, ierr)
    CALL h5dget_space_f(did, dspace_id, ierr)
    CALL h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, ierr)
    rank = ierr
    CALL h5sclose_f(dspace_id, ierr)
    IF( rank .NE. 1 ) THEN
       WRITE(*, '(a,a)') "Data shape mismatch for", name(1:LEN_TRIM(name))
       STOP
    END IF
!
!  Extend the dataset in the last dimension
    starts(1) = dims(1)
    counts(1) = 1
    dims(1) = dims(1) + 1
    CALL h5dextend_f(did, dims, ierr)
    CALL h5dget_space_f(did, dspace_id, ierr)
!
!  Memory dataspace
    ddims(1) = 1
    CALL h5screate_simple_f(1, ddims, memspace_id, ierr)
!
!  Partial read/write of complex (compound) data type
    CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, ierr)
    CALL h5pset_preserve_f(plist_id, .TRUE., ierr)
!
!  Write to the end of dataset
    CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, starts, counts, &
         &                     ierr)
    array(1) = scal
    IF( nlio )  THEN
       CALL mem_complex(H5T_NATIVE_DOUBLE, realpart, impart)
       temp = REAL(array)
       CALL h5dwrite_f(did, realpart, temp, ddims, ierr, memspace_id, &
            &          dspace_id, plist_id)
       temp = AIMAG(array)
       CALL h5dwrite_f(did, impart, temp, ddims, ierr, memspace_id, dspace_id, &
            &          plist_id)
       CALL h5tclose_f(realpart, ierr)
       CALL h5tclose_f(impart, ierr)
    END IF
!
    CALL h5pclose_f(plist_id, ierr)
    CALL h5dclose_f(did, ierr)
    CALL h5sclose_f(dspace_id, ierr)
    CALL h5sclose_f(memspace_id, ierr)
  END SUBROUTINE zappend0
!===========================================================================
! append(n) subroutines:
! Add arrays to the end of a dataset, possibly with an offset into the
! dataset space.
! If an offset is not specified, the dataset is also automatically extended.
!===========================================================================
  SUBROUTINE append1(fid, name, array, pardim, ionode, offset)
!
!   Add 1d double array at the end of dataset
!
    IMPLICIT NONE
    DOUBLE PRECISION, DIMENSION(:), INTENT(in) :: array
    INCLUDE 'append.tpl'
  END SUBROUTINE append1
!===========================================================================
  SUBROUTINE zappend1(fid, name, array, pardim, ionode, offset)
!
!   Add 1d double complex array at the end of dataset
!
    IMPLICIT NONE
    DOUBLE COMPLEX, DIMENSION(:), INTENT(in) :: array
    DOUBLE PRECISION :: temp(SIZE(array,1))
    INCLUDE 'zappend.tpl'
  END SUBROUTINE zappend1
!===========================================================================
  SUBROUTINE append2(fid, name, array, pardim, ionode, offset)
!
!   Add 2d double array at the end of dataset
!
    IMPLICIT NONE
    DOUBLE PRECISION, DIMENSION(:,:), INTENT(in) :: array
    INCLUDE 'append.tpl'
  END SUBROUTINE append2
!===========================================================================
  SUBROUTINE zappend2(fid, name, array, pardim, ionode, offset)
!
!   Add 2d double complex array at the end of dataset
!
    IMPLICIT NONE
    DOUBLE COMPLEX, DIMENSION(:,:), INTENT(in) :: array
    DOUBLE PRECISION :: temp(SIZE(array,1),SIZE(array,2))
    INCLUDE 'zappend.tpl'
  END SUBROUTINE zappend2
!===========================================================================
  SUBROUTINE append3(fid, name, array, pardim, ionode, offset)
!
!   Add 3d double array at the end of dataset
!
    IMPLICIT NONE
    DOUBLE PRECISION, DIMENSION(:,:,:), INTENT(in) :: array
    INCLUDE 'append.tpl'
  END SUBROUTINE append3
!===========================================================================
  SUBROUTINE zappend3(fid, name, array, pardim, ionode, offset)
!
!   Add 3d double complex array at the end of dataset
!
    IMPLICIT NONE
    DOUBLE COMPLEX, DIMENSION(:,:,:), INTENT(in) :: array
    DOUBLE PRECISION :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3))
    INCLUDE 'zappend.tpl'
  END SUBROUTINE zappend3
!===========================================================================
  SUBROUTINE append4(fid, name, array, pardim, ionode, offset)
!
!   Add 4d double array at the end of dataset
!
    IMPLICIT NONE
    DOUBLE PRECISION, DIMENSION(:,:,:,:), INTENT(in) :: array
    INCLUDE 'append.tpl'
  END SUBROUTINE append4
!===========================================================================
  SUBROUTINE zappend4(fid, name, array, pardim, ionode, offset)
!
!   Add 4d double complex array at the end of dataset
!
    IMPLICIT NONE
    DOUBLE COMPLEX, DIMENSION(:,:,:,:), INTENT(in) :: array
    DOUBLE PRECISION :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4))
    INCLUDE 'zappend.tpl'
  END SUBROUTINE zappend4
!===========================================================================
  SUBROUTINE putarr1(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 1d double array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE putarr1
!===========================================================================
  SUBROUTINE putarr2(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 2d double array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE putarr2
!===========================================================================
  SUBROUTINE putarr3(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 3d double array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:,:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE putarr3
!===========================================================================
  SUBROUTINE putarr4(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 4d double array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE putarr4
!===========================================================================
  SUBROUTINE putarr5(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 5d double array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE putarr5
!===========================================================================
  SUBROUTINE putarr6(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 6d double array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:,:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE putarr6
!===========================================================================
  SUBROUTINE sputarr1(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 1d real array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE sputarr1
!===========================================================================
  SUBROUTINE sputarr2(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 2d real array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE sputarr2
!===========================================================================
  SUBROUTINE sputarr3(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 3d real array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:,:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE sputarr3
!===========================================================================
  SUBROUTINE sputarr4(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 4d real array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE sputarr4
!===========================================================================
  SUBROUTINE sputarr5(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 5d real array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE sputarr5
!===========================================================================
  SUBROUTINE sputarr6(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 6d real array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:,:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE sputarr6
!===========================================================================
  SUBROUTINE iputarr1(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 1d integer array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE iputarr1
!===========================================================================
  SUBROUTINE iputarr2(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 2d integer array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE iputarr2
!===========================================================================
  SUBROUTINE iputarr3(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 3d integer array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:,:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE iputarr3
!===========================================================================
  SUBROUTINE iputarr4(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 4d integer array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE iputarr4
!===========================================================================
  SUBROUTINE iputarr5(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 5d integer array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE iputarr5
!===========================================================================
  SUBROUTINE iputarr6(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 6d integer array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:,:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarr.tpl'
  END SUBROUTINE iputarr6
!===========================================================================
  SUBROUTINE cputarr1(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 1d complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:), INTENT(in) :: array
    REAL :: temp(SIZE(array,1))
    INCLUDE 'cputarr.tpl'
  END SUBROUTINE cputarr1
!===========================================================================
  SUBROUTINE zputarr1(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 1d double complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:), INTENT(in) :: array
    DOUBLE PRECISION :: temp(SIZE(array,1))
    INCLUDE 'cputarr.tpl'
  END SUBROUTINE zputarr1
!===========================================================================
  SUBROUTINE cputarr2(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 2d complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:), INTENT(in) :: array
    REAL :: temp(SIZE(array,1),SIZE(array,2))
    INCLUDE 'cputarr.tpl'
  END SUBROUTINE cputarr2
!===========================================================================
  SUBROUTINE zputarr2(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 2d double complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:), INTENT(in) :: array
    DOUBLE PRECISION :: temp(SIZE(array,1),SIZE(array,2))
    INCLUDE 'cputarr.tpl'
  END SUBROUTINE zputarr2
!===========================================================================
  SUBROUTINE cputarr3(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 3d complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:,:), INTENT(in) :: array
    REAL :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3))
    INCLUDE 'cputarr.tpl'
  END SUBROUTINE cputarr3
!===========================================================================
  SUBROUTINE zputarr3(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 3d double complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:,:), INTENT(in) :: array
    DOUBLE PRECISION :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3))
    INCLUDE 'cputarr.tpl'
  END SUBROUTINE zputarr3
!===========================================================================
  SUBROUTINE cputarr4(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 4d complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:,:,:), INTENT(in) :: array
    REAL :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4))
    INCLUDE 'cputarr.tpl'
  END SUBROUTINE cputarr4
!===========================================================================
  SUBROUTINE cputarr5(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 5d complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:,:,:,:), INTENT(in) :: array
    REAL :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4),&
         &  SIZE(array,5))
    INCLUDE 'cputarr.tpl'
  END SUBROUTINE cputarr5
!===========================================================================
  SUBROUTINE cputarr6(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 6d complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:,:,:,:,:), INTENT(in) :: array
    REAL :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4),&
         &  SIZE(array,5),SIZE(array,6))
    INCLUDE 'cputarr.tpl'
  END SUBROUTINE cputarr6
!===========================================================================
  SUBROUTINE zputarr4(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 4d double complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:,:,:), INTENT(in) :: array
    DOUBLE PRECISION :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4))
    INCLUDE 'cputarr.tpl'
  END SUBROUTINE zputarr4
!===========================================================================
  SUBROUTINE zputarr5(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 5d double complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:,:,:,:), INTENT(in) :: array
    DOUBLE PRECISION :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3),&
         &              SIZE(array,4),SIZE(array,5))
    INCLUDE 'cputarr.tpl'
  END SUBROUTINE zputarr5
!===========================================================================
  SUBROUTINE zputarr6(fid, name, array, desc, compress, pardim, ionode)
!
!  Write 6d double complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:,:,:,:,:), INTENT(in) :: array
    DOUBLE PRECISION :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3),&
         &              SIZE(array,4),SIZE(array,5),SIZE(array,6))
    INCLUDE 'cputarr.tpl'
  END SUBROUTINE zputarr6
!===========================================================================
  SUBROUTINE putarrnd2(fid, name, array, pardim, garea, desc, compress)
!
!  Write 2d double precision real array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:), INTENT(in) :: array
    INCLUDE 'putarrnd.tpl'
  END SUBROUTINE putarrnd2
!===========================================================================
  SUBROUTINE sputarrnd2(fid, name, array, pardim, garea, desc, compress)
!
!  Write 2d real array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:), INTENT(in) :: array
    INCLUDE 'putarrnd.tpl'
  END SUBROUTINE sputarrnd2
!===========================================================================
  SUBROUTINE iputarrnd2(fid, name, array, pardim, garea, desc, compress)
!
!  Write 2d integer array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:), INTENT(in) :: array
    INCLUDE 'putarrnd.tpl'
  END SUBROUTINE iputarrnd2
!===========================================================================
  SUBROUTINE cputarrnd2(fid, name, array, pardim, garea, desc, compress)
!
!  Write 2d complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:), INTENT(in) :: array
    REAL :: temp(SIZE(array,1),SIZE(array,2))
    INCLUDE 'cputarrnd.tpl'
  END SUBROUTINE cputarrnd2
!===========================================================================
  SUBROUTINE zputarrnd2(fid, name, array, pardim, garea, desc, compress)
!
!  Write 2d double complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:), INTENT(in) :: array
    DOUBLE PRECISION :: temp(SIZE(array,1),SIZE(array,2))
    INCLUDE 'cputarrnd.tpl'
  END SUBROUTINE zputarrnd2
!===========================================================================
  SUBROUTINE putarrnd3(fid, name, array, pardim, garea, desc, compress)
!
!  Write 3d double precision real array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:,:), INTENT(in) :: array
    INCLUDE 'putarrnd.tpl'
  END SUBROUTINE putarrnd3
!===========================================================================
  SUBROUTINE sputarrnd3(fid, name, array, pardim, garea, desc, compress)
!
!  Write 3d real array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:,:), INTENT(in) :: array
    INCLUDE 'putarrnd.tpl'
  END SUBROUTINE sputarrnd3
!===========================================================================
  SUBROUTINE iputarrnd3(fid, name, array, pardim, garea, desc, compress)
!
!  Write 3d integer array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:,:), INTENT(in) :: array
    INCLUDE 'putarrnd.tpl'
  END SUBROUTINE iputarrnd3
!===========================================================================
  SUBROUTINE cputarrnd3(fid, name, array, pardim, garea, desc, compress)
!
!  Write 3d complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:,:), INTENT(in) :: array
    REAL :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3))
    INCLUDE 'cputarrnd.tpl'
  END SUBROUTINE cputarrnd3
!===========================================================================
  SUBROUTINE zputarrnd3(fid, name, array, pardim, garea, desc, compress)
!
!  Write 3d double complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:,:), INTENT(in) :: array
    DOUBLE PRECISION :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3))
    INCLUDE 'cputarrnd.tpl'
  END SUBROUTINE zputarrnd3
!===========================================================================
  SUBROUTINE putarrnd4(fid, name, array, pardim, garea, desc, compress)
!
!  Write 4d double precision real array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarrnd.tpl'
  END SUBROUTINE putarrnd4
!===========================================================================
  SUBROUTINE sputarrnd4(fid, name, array, pardim, garea, desc, compress)
!
!  Write 4d real array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarrnd.tpl'
  END SUBROUTINE sputarrnd4
!===========================================================================
  SUBROUTINE iputarrnd4(fid, name, array, pardim, garea, desc, compress)
!
!  Write 4d integer array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarrnd.tpl'
  END SUBROUTINE iputarrnd4
!===========================================================================
  SUBROUTINE cputarrnd4(fid, name, array, pardim, garea, desc, compress)
!
!  Write 4d complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:,:,:), INTENT(in) :: array
    REAL :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4))
    INCLUDE 'cputarrnd.tpl'
  END SUBROUTINE cputarrnd4
!===========================================================================
  SUBROUTINE zputarrnd4(fid, name, array, pardim, garea, desc, compress)
!
!  Write 4d double complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:,:,:), INTENT(in) :: array
    DOUBLE PRECISION :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4))
    INCLUDE 'cputarrnd.tpl'
  END SUBROUTINE zputarrnd4
!===========================================================================
  SUBROUTINE putarrnd5(fid, name, array, pardim, garea, desc, compress)
!
!  Write 5d double precision real array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarrnd.tpl'
  END SUBROUTINE putarrnd5
!===========================================================================
  SUBROUTINE sputarrnd5(fid, name, array, pardim, garea, desc, compress)
!
!  Write 5d real array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarrnd.tpl'
  END SUBROUTINE sputarrnd5
!===========================================================================
  SUBROUTINE iputarrnd5(fid, name, array, pardim, garea, desc, compress)
!
!  Write 5d integer array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarrnd.tpl'
  END SUBROUTINE iputarrnd5
!===========================================================================
  SUBROUTINE cputarrnd5(fid, name, array, pardim, garea, desc, compress)
!
!  Write 5d complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:,:,:,:), INTENT(in) :: array
    REAL :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4),SIZE(array,5))
    INCLUDE 'cputarrnd.tpl'
  END SUBROUTINE cputarrnd5
!===========================================================================
  SUBROUTINE zputarrnd5(fid, name, array, pardim, garea, desc, compress)
!
!  Write 5d double complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:,:,:,:), INTENT(in) :: array
    DOUBLE PRECISION :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4),SIZE(array,5))
    INCLUDE 'cputarrnd.tpl'
  END SUBROUTINE zputarrnd5
!===========================================================================
  SUBROUTINE putarrnd6(fid, name, array, pardim, garea, desc, compress)
!
!  Write 6d double precision real array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:,:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarrnd.tpl'
  END SUBROUTINE putarrnd6
!===========================================================================
  SUBROUTINE sputarrnd6(fid, name, array, pardim, garea, desc, compress)
!
!  Write 6d real array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:,:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarrnd.tpl'
  END SUBROUTINE sputarrnd6
!===========================================================================
  SUBROUTINE iputarrnd6(fid, name, array, pardim, garea, desc, compress)
!
!  Write 6d integer array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:,:,:,:,:), INTENT(in) :: array
    INCLUDE 'putarrnd.tpl'
  END SUBROUTINE iputarrnd6
!===========================================================================
  SUBROUTINE cputarrnd6(fid, name, array, pardim, garea, desc, compress)
!
!  Write 6d complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:,:,:,:,:), INTENT(in) :: array
    REAL :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4),SIZE(array,5),SIZE(array,6))
    INCLUDE 'cputarrnd.tpl'
  END SUBROUTINE cputarrnd6
!===========================================================================
  SUBROUTINE zputarrnd6(fid, name, array, pardim, garea, desc, compress)
!
!  Write 6d double complex array to a new dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:,:,:,:,:), INTENT(in) :: array
    DOUBLE PRECISION :: temp(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4),SIZE(array,5),SIZE(array,6))
    INCLUDE 'cputarrnd.tpl'
  END SUBROUTINE zputarrnd6
!===========================================================================
  SUBROUTINE putfile(fid, name, path, desc, compress, ionode)
!
!   Write the file specified in path to a new dataset
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name
    CHARACTER(len=*), INTENT(in) :: path
    CHARACTER(len=*), INTENT(in), OPTIONAL :: desc
    LOGICAL, INTENT(in), OPTIONAL :: compress
    INTEGER, INTENT(in), OPTIONAL :: ionode
!
    INTEGER :: ierr, fsize, rank
    INTEGER(SIZE_T) :: l
    CHARACTER(len=256) :: cstr
    CHARACTER(len=1), DIMENSION(:), ALLOCATABLE :: stream
    INTEGER(HID_T) :: dspace_id, id, did, cprop_id, str_id
    INTEGER(HSIZE_T) :: dims(1)
    LOGICAL :: ok, lcomp
    LOGICAL :: nlio
    EXTERNAL fsize
!
    id = file_id(fid)
!
!  Am I the io node ?
    nlio = .TRUE.
    IF( ispara(fid) .AND. PRESENT(ionode) ) THEN ! Ignore "ionode" if not parallel IO
       nlio = mpi_rank(fid) .EQ. ionode
    END IF
!
    lcomp = .FALSE.
    IF( PRESENT(compress) ) THEN
       lcomp = compress
    END IF
!
!  Get stream from path
    INQUIRE(file=path, exist=ok)
    IF( .NOT. ok ) THEN
       PRINT*, 'PUTFILE: '//TRIM(path)//' does not exist!'
       STOP
    END IF
    cstr = path(1:LEN_TRIM(path)) // CHAR(0)
    l = fsize(cstr)
    ALLOCATE(stream(l))
    CALL ftos(cstr, l, stream)
!
!  Data type for string
    CALL h5tcopy_f(H5T_NATIVE_CHARACTER, str_id, ierr)
    CALL h5tset_size_f(str_id, l, ierr)
!
!  Dataspace for stream
    rank = 1
    dims = 1
    CALL h5screate_simple_f(rank, dims, dspace_id, ierr)
!
!  Property with optional compression
    CALL h5pcreate_f(H5P_DATASET_CREATE_F, cprop_id, ierr)
    IF( lcomp ) THEN
       CALL h5pset_chunk_f(cprop_id, rank, dims, ierr)
       CALL h5pset_deflate_f(cprop_id, 6, ierr)
    END IF
!
!  Create data set and write the stream
    CALL h5dcreate_f(id, name, str_id, dspace_id, &
         &           did, ierr, cprop_id)
    IF( nlio ) CALL h5dwrite_f(did, str_id, stream, dims, ierr)
!
    CALL h5tclose_f(str_id, ierr)
    CALL h5pclose_f(cprop_id, ierr)
    CALL h5sclose_f(dspace_id, ierr)
!
     IF( PRESENT(desc) ) THEN
       CALL annote(did, desc)
    END IF
!
    DEALLOCATE(stream)
    CALL h5dclose_f(did, ierr)
  END SUBROUTINE putfile
!===========================================================================
  SUBROUTINE getfile(fid, name, path)
!
!   Get file in dataset "name" and put it in "path"
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name
    CHARACTER(len=*), INTENT(in), OPTIONAL :: path
    INTEGER(HID_T) :: id, did, typeid
    CHARACTER(len=1), DIMENSION(:), ALLOCATABLE :: stream
    CHARACTER(len=256) :: cstr
    INTEGER(SIZE_T) :: sz
    INTEGER(HSIZE_T) :: dims(1)
    INTEGER :: ierr
!
    id = file_id(fid)
!
!  Get type and type size of dataset
    CALL h5dopen_f(id, name, did, ierr)
    CALL h5dget_type_f(did, typeid, ierr)
    CALL h5tget_size_f(typeid, sz, ierr)
!
!  Read into stream
    ALLOCATE(stream(sz))
    dims(1) = sz
    CALL h5dread_f(did, typeid, stream, dims, ierr)
!
! and send it to output
    IF( PRESENT(path) ) THEN
       cstr = path(1:LEN_TRIM(path)) // CHAR(0)
       CALL stof(cstr, sz, stream)
    ELSE
       CALL stostdout(sz, stream)
    END IF
!
    DEALLOCATE(stream)
    CALL h5tclose_f(typeid, ierr)
    CALL h5dclose_f(did, ierr)
  END SUBROUTINE getfile
!===========================================================================
  SUBROUTINE closef(fid)
!
!   Close the hdf5 file fid
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    INTEGER(HID_T) :: id
    INTEGER :: ierr
!
    id = file_id(fid)       ! file id
    CALL h5fclose_f(id, ierr)
    used(fid) = .FALSE.      ! Release fid
  END SUBROUTINE closef
!===========================================================================
  SUBROUTINE annote(id, str)
!
!   Annote an object (group/dataset) id with str,
!   using object attributes
!
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(in) :: id
    CHARACTER(len=*), INTENT(in) :: str
    INTEGER(HID_T) :: str_id, ann_space, title_id
    INTEGER(SIZE_T) :: l
    INTEGER :: ierr
    INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
!
    CALL h5tcopy_f(H5T_NATIVE_CHARACTER, str_id, ierr)
    l=LEN_TRIM(str)
    CALL h5tset_size_f(str_id, l, ierr)
    CALL h5screate_f(H5S_SCALAR_F, ann_space, ierr)
    CALL h5acreate_f(id, 'title', str_id, ann_space, title_id, ierr)
    CALL h5awrite_f(title_id, str_id, str(1:l), one, ierr)
!
    CALL h5tclose_f(str_id, ierr)
    CALL h5sclose_f(ann_space, ierr)
    CALL h5aclose_f(title_id, ierr)
  END SUBROUTINE annote
!===========================================================================
  SUBROUTINE getsize(fid, name, n)
!
!   Get size of last dim of data set
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    INTEGER, INTENT(out) :: n
    CHARACTER(len=*), INTENT(in) :: name
    INTEGER(HID_T) :: id, did
    INTEGER(HSIZE_T) :: dims(7), maxdims(7)
    INTEGER(HID_T) :: dspace_id
    INTEGER :: rank, ierr
!
    id = file_id(fid)                   ! file id
    CALL h5dopen_f(id, name, did, ierr) ! data id
    CALL h5dget_space_f(did, dspace_id, ierr)
    CALL h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, rank)
    n = dims(rank)
    CALL h5dclose_f(did, ierr)
    CALL h5sclose_f(dspace_id, ierr)
   END SUBROUTINE getsize
!===========================================================================
  SUBROUTINE getdims(fid, name, rank, dims)
!
!   Get rank and dimensions of data set
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    INTEGER, INTENT(out) :: rank, dims(:)
    CHARACTER(len=*), INTENT(in) :: name
    INTEGER(HID_T) :: id, did
    INTEGER(HSIZE_T) :: xdims(7), maxdims(7)
    INTEGER(HID_T) :: dspace_id
    INTEGER :: ierr
!
    id = file_id(fid)                   ! file id
    CALL h5dopen_f(id, name, did, ierr) ! data id
    CALL h5dget_space_f(did, dspace_id, ierr)
    CALL h5sget_simple_extent_dims_f(dspace_id, xdims, maxdims, rank)
    dims(1:rank) = xdims(1:rank)
    CALL h5dclose_f(did, ierr)
    CALL h5sclose_f(dspace_id, ierr)
  END SUBROUTINE getdims
!===========================================================================
  SUBROUTINE split(fullname, group, name)
    IMPLICIT NONE
    CHARACTER(len=*), INTENT(in) :: fullname
    CHARACTER(len=*), INTENT(out) :: group, name
    INTEGER :: found
!
    group = ' '
    name = ' '
    found = SCAN(fullname, '/', back=.TRUE.)
    IF( found.EQ.0 ) THEN
       group(1:1) = '/'
       name(:) = fullname
    ELSE
       group(:) = fullname(1:found)
       name(:) = fullname(found+1:)
    END IF
  END SUBROUTINE split
!===========================================================================
  LOGICAL FUNCTION isgroup(fid, name)
!
!  Is name a group?
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name
    INTEGER :: ierr
    INTEGER(HID_T) :: id, did
!
    id = file_id(fid)                   ! file id
    CALL h5eset_auto_f(0, ierr)         ! Turn error print off
    CALL h5gopen_f(id, name, did, ierr)
    IF( ierr .EQ. 0) THEN
       isgroup = .TRUE.
       CALL h5gclose_f(did, ierr)
    ELSE
       isgroup = .FALSE.
    END IF
    CALL h5eset_auto_f(1, ierr)         ! Turn error print on
  END FUNCTION isgroup
!===========================================================================
  LOGICAL FUNCTION isdataset(fid, name)
!
!  Is name a dataset?
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name
    INTEGER :: ierr
    INTEGER(HID_T) :: id, did
!
    id = file_id(fid)                   ! file id
    CALL h5eset_auto_f(0, ierr)         ! Turn error print off
    CALL h5dopen_f(id, name, did, ierr)
    IF( ierr .EQ. 0) THEN
       isdataset = .TRUE.
       CALL h5dclose_f(did, ierr)
    ELSE
       isdataset = .FALSE.
    END IF
    CALL h5eset_auto_f(1, ierr)         ! Turn error print on
  END FUNCTION isdataset
!===========================================================================
  SUBROUTINE getoid(fid, name, oid)
!
!  Get id of object (group or dataset)
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name
    INTEGER(HID_T), INTENT(out) :: oid
    INTEGER :: ierr
    INTEGER(HID_T) :: id
!
    id = file_id(fid)                   ! file id
    CALL h5eset_auto_f(0, ierr)         ! Turn error print off
    CALL h5dopen_f(id, name, oid, ierr)
    IF( ierr .EQ. -1) THEN
       CALL h5gopen_f(id, name, oid, ierr)
    END IF
    CALL h5eset_auto_f(1, ierr)         ! Turn error print on
  END SUBROUTINE getoid
!===========================================================================
  SUBROUTINE closeid(id)
!
!   Close id of group or dataset
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: id
    INTEGER :: ierr
!
    CALL h5eset_auto_f(0, ierr)         ! Turn error print off
    CALL h5dclose_f(id, ierr)
    IF( ierr .EQ. -1 ) THEN
       CALL h5gclose_f(id, ierr)
    END IF
    CALL h5eset_auto_f(1, ierr)         ! Turn error print on
  END SUBROUTINE closeid
!===========================================================================
  SUBROUTINE attach_r(fid, name, attr, val)
!
!  Attach an real attribute to group/dataset
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name, attr
    DOUBLE PRECISION, INTENT(in) :: val
    INTEGER :: ierr
    INTEGER(HID_T) :: id, oid, attr_space, attr_id
    INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
!
    id = file_id(fid)                   ! file id
    CALL getoid(fid, name, oid)
!
    CALL h5eset_auto_f(0, ierr)         ! Turn error print off
    CALL h5aopen_name_f(oid, attr, attr_id, ierr)
    IF( ierr .EQ. -1 ) THEN
       CALL h5screate_f(H5S_SCALAR_F, attr_space, ierr)
       CALL h5acreate_f(oid, attr, H5T_NATIVE_DOUBLE, attr_space, attr_id, ierr)
       CALL h5sclose_f(attr_space, ierr)
    END IF
    CALL h5eset_auto_f(1, ierr)         ! Turn error print on
!
    CALL h5awrite_f(attr_id, H5T_NATIVE_DOUBLE, val, one, ierr)
!
    CALL h5aclose_f(attr_id, ierr)
    CALL closeid(oid)
  END SUBROUTINE attach_r
!===========================================================================
  SUBROUTINE attach_f(fid, name, attr, val)
!
!  Attach an real SP attribute to group/dataset
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name, attr
    REAL, INTENT(in) :: val
    INTEGER :: ierr
    INTEGER(HID_T) :: id, oid, attr_space, attr_id
    INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
!
    id = file_id(fid)                   ! file id
    CALL getoid(fid, name, oid)
!
    CALL h5eset_auto_f(0, ierr)         ! Turn error print off
    CALL h5aopen_name_f(oid, attr, attr_id, ierr)
    IF( ierr .EQ. -1 ) THEN
       CALL h5screate_f(H5S_SCALAR_F, attr_space, ierr)
       CALL h5acreate_f(oid, attr, H5T_NATIVE_REAL, attr_space, attr_id, ierr)
       CALL h5sclose_f(attr_space, ierr)
    END IF
    CALL h5eset_auto_f(1, ierr)         ! Turn error print on
!
    CALL h5awrite_f(attr_id, H5T_NATIVE_REAL, val, one, ierr)
!
    CALL h5aclose_f(attr_id, ierr)
    CALL closeid(oid)
  END SUBROUTINE attach_f
!===========================================================================
  SUBROUTINE attach_i(fid, name, attr, ival)
!
!  Attach a integer attribute to group/dataset
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name, attr
    INTEGER, INTENT(in) :: ival
    INTEGER :: ierr
    INTEGER(HID_T) :: id, oid, attr_space, attr_id
    INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
!
    id = file_id(fid)                   ! file id
    CALL getoid(fid, name, oid)
!
    CALL h5eset_auto_f(0, ierr)         ! Turn error print off
    CALL h5aopen_name_f(oid, attr, attr_id, ierr)
    IF( ierr .EQ. -1 ) THEN
       CALL h5screate_f(H5S_SCALAR_F, attr_space, ierr)
       CALL h5acreate_f(oid, attr, H5T_STD_I32LE, attr_space, attr_id, ierr)
       CALL h5sclose_f(attr_space, ierr)
    END IF
    CALL h5eset_auto_f(1, ierr)         ! Turn error print on
!
    CALL h5awrite_f(attr_id, H5T_NATIVE_INTEGER, ival, one, ierr)
!
    CALL h5aclose_f(attr_id, ierr)
    CALL closeid(oid)
  END SUBROUTINE attach_i
!===========================================================================
  SUBROUTINE attach_s(fid, name, attr, sval)
!
!  Attach a string attribute to group/dataset
!  Warning: could not be overwriten!
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name, attr
    CHARACTER(len=*), INTENT(in) :: sval
    INTEGER :: ierr
    INTEGER(SIZE_T) :: l
    INTEGER(HID_T) :: id, oid, str_id, attr_space, attr_id
    INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
!
    id = file_id(fid)                   ! file id
    CALL getoid(fid, name, oid)
!
    CALL h5tcopy_f(H5T_NATIVE_CHARACTER, str_id, ierr)
    l=LEN_TRIM(sval)
    CALL h5tset_size_f(str_id, l, ierr)
!
    CALL h5screate_f(H5S_SCALAR_F, attr_space, ierr)
    CALL h5acreate_f(oid, attr, str_id, attr_space, attr_id, ierr)
!
    CALL h5awrite_f(attr_id, str_id, sval, one, ierr)
!
    CALL h5sclose_f(attr_space, ierr)
    CALL h5tclose_f(str_id, ierr)
    CALL h5aclose_f(attr_id, ierr)
    CALL closeid(oid)
  END SUBROUTINE attach_s
!===========================================================================
  SUBROUTINE attach_sa(fid, dsetname, aname, attr_data)
!
!  Attach a string array attribute to group/dataset
!  Warning: could not be overwriten!
!
!
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: fid
    CHARACTER(LEN=*), INTENT(IN) :: dsetname    ! Dataset name
    CHARACTER(LEN=*), INTENT(IN) :: aname       ! Attribute name
    CHARACTER(LEN=*), DIMENSION(:) ::  attr_data  ! Attribute data
!
    INTEGER(HID_T) :: id       ! File identifier
    INTEGER(HID_T) :: dset_id       ! Dataset identifier
    INTEGER(HID_T) :: attr_id       ! Attribute identifier
    INTEGER(HID_T) :: aspace_id     ! Attribute Dataspace identifier
    INTEGER(HID_T) :: atype_id      ! Attribute Dataspace identifier
    INTEGER(HSIZE_T), DIMENSION(1) :: adims = (/2/) ! Attribute dimension
    INTEGER     ::   arank = 1                      ! Attribure rank
    INTEGER(SIZE_T) :: attrlen    ! Length of the attribute string
    INTEGER     ::   error ! Error flag
    INTEGER(HSIZE_T), DIMENSION(1) :: data_dims
!
    adims(1) = SIZE(attr_data)
    attrlen = LEN(attr_data(1))
!
! Get file id
!
    id = file_id(fid)
!
! Open an existing dataset.
!
    CALL h5eset_auto_f(0, error)
    CALL h5dopen_f(id, dsetname, dset_id, error)
    IF (error .EQ. -1) THEN
       CALL h5gopen_f(id, dsetname, dset_id, error)
    END IF
    CALL h5eset_auto_f(1, error)
!
! Create scalar data space for the attribute.
!
    CALL h5screate_simple_f(arank, adims, aspace_id, error)
!
! Create datatype for the attribute.
!
    CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
    CALL h5tset_size_f(atype_id, attrlen, error)
!
! Create dataset attribute.
!
    CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, &
         attr_id, error)
!
! Write the attribute data.
!
    data_dims(1) = adims(1)
    CALL h5awrite_f(attr_id, atype_id, attr_data, data_dims, error)
!
! Close the attribute.
!
    CALL h5aclose_f(attr_id, error)
!
! Terminate access to the data space.
!
    CALL h5sclose_f(aspace_id, error)
!
! End access to the dataset and release resources used by it.
!
    CALL h5eset_auto_f(0, error)
    CALL h5dclose_f(dset_id, error)
    IF (error .EQ. -1) THEN
       CALL h5gclose_f(dset_id, error)
    END IF
    CALL h5eset_auto_f(1, error)
!
  END SUBROUTINE attach_sa
!===========================================================================
  SUBROUTINE attach_l(fid, name, attr, lval)
!
!  Attach a logical attribute to group/dataset
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name, attr
    LOGICAL, INTENT(in) :: lval
    CHARACTER(len=1) :: c
    INTEGER :: ierr
    INTEGER(HID_T) :: id, oid, attr_space, attr_id
    INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
!
    id = file_id(fid)                   ! file id
    CALL getoid(fid, name, oid)
!
    CALL h5eset_auto_f(0, ierr)         ! Turn error print off
    CALL h5aopen_name_f(oid, attr, attr_id, ierr)
    IF( ierr .EQ. -1 ) THEN
       CALL h5screate_f(H5S_SCALAR_F, attr_space, ierr)
       CALL h5acreate_f(oid, attr, H5T_NATIVE_CHARACTER, attr_space, attr_id, ierr)
       CALL h5sclose_f(attr_space, ierr)
    END IF
    CALL h5eset_auto_f(1, ierr)         ! Turn error print on
!
    c = 'n'
    IF (lval) c='y'
    CALL h5awrite_f(attr_id, H5T_NATIVE_CHARACTER, c, one, ierr)
!
    CALL h5aclose_f(attr_id, ierr)
    CALL closeid(oid)
  END SUBROUTINE attach_l
!===========================================================================
  SUBROUTINE getatt_i(fid, name, attr, ival, err)
!
!  Get a integer attribute from group/dataset
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name, attr
    INTEGER, INTENT(out) :: ival
    INTEGER, INTENT(out), OPTIONAL :: err
    INTEGER :: ierr
    INTEGER(HID_T) :: id, oid, attr_id
    INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
!
    id = file_id(fid)                   ! file id
    CALL getoid(fid, name, oid)
!
    CALL h5eset_auto_f(0, ierr)         ! Turn error print off
    CALL h5aopen_name_f(oid, attr, attr_id, ierr)
    IF( ierr .EQ. -1 ) THEN   ! attr not found
       IF(PRESENT(err)) err = -1
    ELSE
       CALL h5aread_f(attr_id, H5T_NATIVE_INTEGER, ival, one, ierr)
       IF( ierr .EQ. -1 ) THEN ! wrong attr type
          IF(PRESENT(err)) err = -2
       END IF
    END IF
    CALL h5eset_auto_f(1, ierr)         ! Turn error print on
!
    CALL h5aclose_f(attr_id, ierr)
    CALL closeid(oid)
  END SUBROUTINE getatt_i
!===========================================================================
  SUBROUTINE getatt_s(fid, name, attr, sval, err)
!
!  Get a string attribute from group/dataset
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name, attr
    CHARACTER(len=*), INTENT(out) :: sval
    INTEGER, INTENT(out), OPTIONAL :: err
    INTEGER :: ierr
    INTEGER(SIZE_T) :: l
    INTEGER(HID_T) :: id, oid, attr_id, str_id
    INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
!
    id = file_id(fid)                   ! file id
    CALL getoid(fid, name, oid)
!
    l = LEN(sval)
    CALL h5tcopy_f(H5T_NATIVE_CHARACTER, str_id, ierr)
    CALL h5tset_size_f(str_id, l, ierr)
!
    CALL h5eset_auto_f(0, ierr)         ! Turn error print off
    CALL h5aopen_name_f(oid, attr, attr_id, ierr)
    IF( ierr .EQ. -1 ) THEN   ! attr not found
       IF(PRESENT(err)) err = -1
    ELSE
       CALL h5aread_f(attr_id, str_id, sval, one, ierr)
       IF( ierr .EQ. -1 ) THEN ! wrong attr type
          IF(PRESENT(err)) err = -2
       END IF
    END IF
    CALL h5eset_auto_f(1, ierr)         ! Turn error print on
!
    CALL h5tclose_f(str_id, ierr)
    CALL h5aclose_f(attr_id, ierr)
    CALL closeid(oid)
  END SUBROUTINE getatt_s
!===========================================================================
  SUBROUTINE getatt_r(fid, name, attr, val, err)
!
!  Get a real attribute from group/dataset
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name, attr
    DOUBLE PRECISION, INTENT(out) :: val
    INTEGER, INTENT(out), OPTIONAL :: err
    INTEGER :: ierr
    INTEGER(HID_T) :: id, oid, attr_id
    INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
!
    id = file_id(fid)                   ! file id
    CALL getoid(fid, name, oid)
!
    CALL h5eset_auto_f(0, ierr)         ! Turn error print off
    CALL h5aopen_name_f(oid, attr, attr_id, ierr)
    IF( ierr .EQ. -1 ) THEN   ! attr not found
       IF(PRESENT(err)) err = -1
    ELSE
       CALL h5aread_f(attr_id, H5T_NATIVE_DOUBLE, val, one, ierr)
       IF( ierr .EQ. -1 ) THEN ! wrong attr type
          IF(PRESENT(err)) err = -2
       END IF
    END IF
    CALL h5eset_auto_f(1, ierr)         ! Turn error print on
!
    CALL h5aclose_f(attr_id, ierr)
    CALL closeid(oid)
  END SUBROUTINE getatt_r
!===========================================================================
  SUBROUTINE getatt_f(fid, name, attr, val, err)
!
!  Get a real SP attribute from group/dataset
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name, attr
    REAL, INTENT(out) :: val
    INTEGER, INTENT(out), OPTIONAL :: err
    INTEGER :: ierr
    INTEGER(HID_T) :: id, oid, attr_id
    INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
!
    id = file_id(fid)                   ! file id
    CALL getoid(fid, name, oid)
!
    CALL h5eset_auto_f(0, ierr)         ! Turn error print off
    CALL h5aopen_name_f(oid, attr, attr_id, ierr)
    IF( ierr .EQ. -1 ) THEN   ! attr not found
       IF(PRESENT(err)) err = -1
    ELSE
       CALL h5aread_f(attr_id, H5T_NATIVE_REAL, val, one, ierr)
       IF( ierr .EQ. -1 ) THEN ! wrong attr type
          IF(PRESENT(err)) err = -2
       END IF
    END IF
    CALL h5eset_auto_f(1, ierr)         ! Turn error print on
!
    CALL h5aclose_f(attr_id, ierr)
    CALL closeid(oid)
  END SUBROUTINE getatt_f
!===========================================================================
  SUBROUTINE getatt_l(fid, name, attr, lval, err)
!
!  Get a real attribute from group/dataset
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name, attr
    LOGICAL, INTENT(out) :: lval
    INTEGER, INTENT(out), OPTIONAL :: err
    CHARACTER(len=1) :: c
    INTEGER :: ierr
    INTEGER(HID_T) :: id, oid, attr_space, attr_id
    INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
!
    id = file_id(fid)                   ! file id
    CALL getoid(fid, name, oid)
!
    CALL h5eset_auto_f(0, ierr)         ! Turn error print off
    CALL h5aopen_name_f(oid, attr, attr_id, ierr)
    IF( ierr .EQ. -1 ) THEN   ! attr not found
       IF(PRESENT(err)) err = -1
    ELSE
       CALL h5aread_f(attr_id, H5T_NATIVE_CHARACTER, c, one, ierr)
       IF( ierr .EQ. -1 ) THEN ! wrong attr type
          IF(PRESENT(err)) err = -2
       END IF
    END IF
    CALL h5eset_auto_f(1, ierr)         ! Turn error print on
    IF( c .EQ. 'y' ) THEN
       lval = .TRUE.
    ELSE
       lval = .FALSE.
    END IF
!
    CALL h5aclose_f(attr_id, ierr)
    CALL closeid(oid)
  END SUBROUTINE getatt_l
!===========================================================================
  SUBROUTINE part1d(comm, nlocal, start, nglobal)
!
!  1D partition
!
    IMPLICIT NONE
    INCLUDE 'mpif.h'
    INTEGER, INTENT(in) :: comm, nlocal
    INTEGER, INTENT(out) :: nglobal, start
    INTEGER :: ierr
!
    CALL mpi_allreduce(nlocal, nglobal, 1, MPI_INTEGER, MPI_SUM, comm, ierr)
    CALL mpi_scan(nlocal, start, 1, MPI_INTEGER, MPI_SUM, comm, ierr)
    start = start-nlocal
  END SUBROUTINE part1d
!===========================================================================
  SUBROUTINE getarr1(fid, name, array, pardim, ionode, offsets)
!
!  Read 1d double array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE getarr1
!===========================================================================
  SUBROUTINE getarr2(fid, name, array, pardim, ionode, offsets)
!
!  Read 2d double array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE getarr2
!===========================================================================
  SUBROUTINE getarr3(fid, name, array, pardim, ionode, offsets)
!
!  Read 3d double array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:,:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE getarr3
!===========================================================================
  SUBROUTINE getarr4(fid, name, array, pardim, ionode, offsets)
!
!  Read 4d double array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE getarr4
!===========================================================================
  SUBROUTINE getarr5(fid, name, array, pardim, ionode, offsets)
!
!  Read 5d double array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE getarr5
!===========================================================================
  SUBROUTINE getarr6(fid, name, array, pardim, ionode, offsets)
!
!  Read 6d double array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:,:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE getarr6
!===========================================================================
  SUBROUTINE sgetarr1(fid, name, array, pardim, ionode, offsets)
!
!  Read 1d real array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE sgetarr1
!===========================================================================
  SUBROUTINE sgetarr2(fid, name, array, pardim, ionode, offsets)
!
!  Read 2d real array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE sgetarr2
!===========================================================================
  SUBROUTINE sgetarr3(fid, name, array, pardim, ionode, offsets)
!
!  Read 3d real array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:,:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE sgetarr3
!===========================================================================
  SUBROUTINE sgetarr4(fid, name, array, pardim, ionode, offsets)
!
!  Read 4d real array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE sgetarr4
!===========================================================================
  SUBROUTINE sgetarr5(fid, name, array, pardim, ionode, offsets)
!
!  Read 5d real array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE sgetarr5
!===========================================================================
  SUBROUTINE sgetarr6(fid, name, array, pardim, ionode, offsets)
!
!  Read 6d real array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:,:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE sgetarr6
!===========================================================================
  SUBROUTINE igetarr1(fid, name, array, pardim, ionode, offsets)
!
!  Read 1d integer array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE igetarr1
!===========================================================================
  SUBROUTINE igetarr2(fid, name, array, pardim, ionode, offsets)
!
!  Read 2d integer array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE igetarr2
!===========================================================================
  SUBROUTINE igetarr3(fid, name, array, pardim, ionode, offsets)
!
!  Read 3d integer array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:,:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE igetarr3
!===========================================================================
  SUBROUTINE igetarr4(fid, name, array, pardim, ionode, offsets)
!
!  Read 4d integer array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE igetarr4
!===========================================================================
  SUBROUTINE igetarr5(fid, name, array, pardim, ionode, offsets)
!
!  Read 5d integer array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE igetarr5
!===========================================================================
  SUBROUTINE igetarr6(fid, name, array, pardim, ionode, offsets)
!
!  Read 6d integer array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:,:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarr.tpl'
  END SUBROUTINE igetarr6
!===========================================================================
  SUBROUTINE cgetarr1(fid, name, array, pardim, ionode, offsets)
!
!  Read 1d complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:), INTENT(out) :: array
    REAL, DIMENSION(SIZE(array)) :: temp1, temp2
    INCLUDE 'cgetarr.tpl'
  END SUBROUTINE cgetarr1
!===========================================================================
  SUBROUTINE zgetarr1(fid, name, array, pardim, ionode, offsets)
!
!  Read 1d double complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:), INTENT(out) :: array
    DOUBLE PRECISION, DIMENSION(SIZE(array)) :: temp1, temp2
    INCLUDE 'cgetarr.tpl'
  END SUBROUTINE zgetarr1
!===========================================================================
  SUBROUTINE cgetarr2(fid, name, array, pardim, ionode, offsets)
!
!  Read 2d complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:), INTENT(out) :: array
    REAL, DIMENSION(SIZE(array,1),SIZE(array,2)) :: temp1, temp2
    INCLUDE 'cgetarr.tpl'
  END SUBROUTINE cgetarr2
!===========================================================================
  SUBROUTINE zgetarr2(fid, name, array, pardim, ionode, offsets)
!
!  Read 2d double complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:), INTENT(out) :: array
    DOUBLE PRECISION, DIMENSION(SIZE(array,1),SIZE(array,2)) :: temp1, temp2
    INCLUDE 'cgetarr.tpl'
  END SUBROUTINE zgetarr2
!===========================================================================
  SUBROUTINE cgetarr3(fid, name, array, pardim, ionode, offsets)
!
!  Read 3d complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:,:), INTENT(out) :: array
    REAL, DIMENSION(SIZE(array,1),SIZE(array,2),SIZE(array,3)) :: temp1, temp2
    INCLUDE 'cgetarr.tpl'
  END SUBROUTINE cgetarr3
!===========================================================================
  SUBROUTINE zgetarr3(fid, name, array, pardim, ionode, offsets)
!
!  Read 3d double complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:,:), INTENT(out) :: array
    DOUBLE PRECISION, DIMENSION(SIZE(array,1),SIZE(array,2),SIZE(array,3)) :: temp1, temp2
    INCLUDE 'cgetarr.tpl'
  END SUBROUTINE zgetarr3
!===========================================================================
  SUBROUTINE cgetarr4(fid, name, array, pardim, ionode, offsets)
!
!  Read 4d complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:,:,:), INTENT(out) :: array
    REAL, DIMENSION(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4)) :: temp1, temp2
    INCLUDE 'cgetarr.tpl'
  END SUBROUTINE cgetarr4
!===========================================================================
  SUBROUTINE cgetarr5(fid, name, array, pardim, ionode, offsets)
!
!  Read 5d complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:,:,:,:), INTENT(out) :: array
    REAL, DIMENSION(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4),&
         &   SIZE(array,5)) :: temp1, temp2
    INCLUDE 'cgetarr.tpl'
  END SUBROUTINE cgetarr5
!===========================================================================
  SUBROUTINE cgetarr6(fid, name, array, pardim, ionode, offsets)
!
!  Read 6d complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:,:,:,:,:), INTENT(out) :: array
    REAL, DIMENSION(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4),&
         &   SIZE(array,5), SIZE(array,6)) :: temp1, temp2
    INCLUDE 'cgetarr.tpl'
  END SUBROUTINE cgetarr6
!===========================================================================
  SUBROUTINE zgetarr4(fid, name, array, pardim, ionode, offsets)
!
!  Read 4d double complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:,:,:), INTENT(out) :: array
    DOUBLE PRECISION, DIMENSION(SIZE(array,1),SIZE(array,2),SIZE(array,3),&
         &  SIZE(array,4)) :: temp1, temp2
    INCLUDE 'cgetarr.tpl'
  END SUBROUTINE zgetarr4
!===========================================================================
  SUBROUTINE zgetarr5(fid, name, array, pardim, ionode, offsets)
!
!  Read 5d double complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:,:,:,:), INTENT(out) :: array
    DOUBLE PRECISION, DIMENSION(SIZE(array,1),SIZE(array,2),SIZE(array,3),&
         &  SIZE(array,4), SIZE(array,5)) :: temp1, temp2
    INCLUDE 'cgetarr.tpl'
  END SUBROUTINE zgetarr5
!===========================================================================
  SUBROUTINE zgetarr6(fid, name, array, pardim, ionode, offsets)
!
!  Read 6d double complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:,:,:,:,:), INTENT(out) :: array
    DOUBLE PRECISION, DIMENSION(SIZE(array,1),SIZE(array,2),SIZE(array,3),&
         &  SIZE(array,4),SIZE(array,5),SIZE(array,6)) :: temp1, temp2
    INCLUDE 'cgetarr.tpl'
  END SUBROUTINE zgetarr6
!===========================================================================
  SUBROUTINE getarrnd2(fid, name, array, pardim, garea)
!
!  Read 2d double array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:), INTENT(out) :: array
    INCLUDE 'getarrnd.tpl'
  END SUBROUTINE getarrnd2
!===========================================================================
  SUBROUTINE getarrnd3(fid, name, array, pardim, garea)
!
!  Read 3d double array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:,:), INTENT(out) :: array
    INCLUDE 'getarrnd.tpl'
  END SUBROUTINE getarrnd3
!===========================================================================
  SUBROUTINE getarrnd4(fid, name, array, pardim, garea)
!
!  Read 4d double array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarrnd.tpl'
  END SUBROUTINE getarrnd4
!===========================================================================
  SUBROUTINE getarrnd5(fid, name, array, pardim, garea)
!
!  Read 5d double array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarrnd.tpl'
  END SUBROUTINE getarrnd5
!===========================================================================
  SUBROUTINE getarrnd6(fid, name, array, pardim, garea)
!
!  Read 6d double array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'R'
    DOUBLE PRECISION, DIMENSION(:,:,:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarrnd.tpl'
  END SUBROUTINE getarrnd6
!===========================================================================
  SUBROUTINE sgetarrnd2(fid, name, array, pardim, garea)
!
!  Read 2d real array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:), INTENT(out) :: array
    INCLUDE 'getarrnd.tpl'
  END SUBROUTINE sgetarrnd2
!===========================================================================
  SUBROUTINE sgetarrnd3(fid, name, array, pardim, garea)
!
!  Read 3d real array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:,:), INTENT(out) :: array
    INCLUDE 'getarrnd.tpl'
  END SUBROUTINE sgetarrnd3
!===========================================================================
  SUBROUTINE sgetarrnd4(fid, name, array, pardim, garea)
!
!  Read 4d real array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarrnd.tpl'
  END SUBROUTINE sgetarrnd4
!===========================================================================
  SUBROUTINE sgetarrnd5(fid, name, array, pardim, garea)
!
!  Read 5d real array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarrnd.tpl'
  END SUBROUTINE sgetarrnd5
!===========================================================================
  SUBROUTINE sgetarrnd6(fid, name, array, pardim, garea)
!
!  Read 6d real array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'S'
    REAL, DIMENSION(:,:,:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarrnd.tpl'
  END SUBROUTINE sgetarrnd6
!===========================================================================
  SUBROUTINE igetarrnd2(fid, name, array, pardim, garea)
!
!  Read 2d integer array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:), INTENT(out) :: array
    INCLUDE 'getarrnd.tpl'
  END SUBROUTINE igetarrnd2
!===========================================================================
  SUBROUTINE igetarrnd3(fid, name, array, pardim, garea)
!
!  Read 3d integer array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:,:), INTENT(out) :: array
    INCLUDE 'getarrnd.tpl'
  END SUBROUTINE igetarrnd3
!===========================================================================
  SUBROUTINE igetarrnd4(fid, name, array, pardim, garea)
!
!  Read 4d integer array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarrnd.tpl'
  END SUBROUTINE igetarrnd4
!===========================================================================
  SUBROUTINE igetarrnd5(fid, name, array, pardim, garea)
!
!  Read 5d integer array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarrnd.tpl'
  END SUBROUTINE igetarrnd5
!===========================================================================
  SUBROUTINE igetarrnd6(fid, name, array, pardim, garea)
!
!  Read 6d integer array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'I'
    INTEGER, DIMENSION(:,:,:,:,:,:), INTENT(out) :: array
    INCLUDE 'getarrnd.tpl'
  END SUBROUTINE igetarrnd6
!===========================================================================
  SUBROUTINE cgetarrnd2(fid, name, array, pardim, garea)
!
!  Read 2d complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:), INTENT(out) :: array
    REAL, DIMENSION(SIZE(array,1),SIZE(array,2)) :: temp1, temp2
    INCLUDE 'cgetarrnd.tpl'
  END SUBROUTINE cgetarrnd2
!===========================================================================
  SUBROUTINE cgetarrnd3(fid, name, array, pardim, garea)
!
!  Read 3d complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:,:), INTENT(out) :: array
    REAL, DIMENSION(SIZE(array,1),SIZE(array,2),SIZE(array,3)) :: temp1, temp2
    INCLUDE 'cgetarrnd.tpl'
  END SUBROUTINE cgetarrnd3
!===========================================================================
  SUBROUTINE cgetarrnd4(fid, name, array, pardim, garea)
!
!  Read 4d complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:,:,:), INTENT(out) :: array
    REAL, DIMENSION(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4)) :: temp1, temp2
    INCLUDE 'cgetarrnd.tpl'
  END SUBROUTINE cgetarrnd4
!===========================================================================
  SUBROUTINE cgetarrnd5(fid, name, array, pardim, garea)
!
!  Read 5d complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:,:,:,:), INTENT(out) :: array
    REAL, DIMENSION(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4),SIZE(array,5)) :: temp1, temp2
    INCLUDE 'cgetarrnd.tpl'
  END SUBROUTINE cgetarrnd5
!===========================================================================
  SUBROUTINE cgetarrnd6(fid, name, array, pardim, garea)
!
!  Read 6d complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'C'
    COMPLEX, DIMENSION(:,:,:,:,:,:), INTENT(out) :: array
    REAL, DIMENSION(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4),SIZE(array,5),SIZE(array,6)) :: temp1, temp2
    INCLUDE 'cgetarrnd.tpl'
  END SUBROUTINE cgetarrnd6
!===========================================================================
  SUBROUTINE zgetarrnd2(fid, name, array, pardim, garea)
!
!  Read 2d double complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:), INTENT(out) :: array
    DOUBLE PRECISION, DIMENSION(SIZE(array,1),SIZE(array,2)) :: temp1, temp2
    INCLUDE 'cgetarrnd.tpl'
  END SUBROUTINE zgetarrnd2
!===========================================================================
  SUBROUTINE zgetarrnd3(fid, name, array, pardim, garea)
!
!  Read 3d double complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:,:), INTENT(out) :: array
    DOUBLE PRECISION, DIMENSION(SIZE(array,1),SIZE(array,2),SIZE(array,3)) :: temp1, temp2
    INCLUDE 'cgetarrnd.tpl'
  END SUBROUTINE zgetarrnd3
!===========================================================================
  SUBROUTINE zgetarrnd4(fid, name, array, pardim, garea)
!
!  Read 4d double complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:,:,:), INTENT(out) :: array
    DOUBLE PRECISION, DIMENSION(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4)) :: temp1, temp2
    INCLUDE 'cgetarrnd.tpl'
  END SUBROUTINE zgetarrnd4
!===========================================================================
  SUBROUTINE zgetarrnd5(fid, name, array, pardim, garea)
!
!  Read 5d double complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:,:,:,:), INTENT(out) :: array
    DOUBLE PRECISION, DIMENSION(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4),SIZE(array,5)) :: temp1, temp2
    INCLUDE 'cgetarrnd.tpl'
  END SUBROUTINE zgetarrnd5
!===========================================================================
  SUBROUTINE zgetarrnd6(fid, name, array, pardim, garea)
!
!  Read 6d double complex array from dataset
!
    IMPLICIT NONE
    CHARACTER(len=1), PARAMETER :: ctype = 'Z'
    DOUBLE COMPLEX, DIMENSION(:,:,:,:,:,:), INTENT(out) :: array
    DOUBLE PRECISION, DIMENSION(SIZE(array,1),SIZE(array,2),SIZE(array,3),SIZE(array,4),SIZE(array,5),SIZE(array,6)) :: temp1, temp2
    INCLUDE 'cgetarrnd.tpl'
  END SUBROUTINE zgetarrnd6
!===========================================================================
  INTEGER FUNCTION numatts(fid, name)
!
!  Number of attributes in group or dataset
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name
    INTEGER :: ierr
    INTEGER(HID_T) :: id, oid
!
    id = file_id(fid)                   ! file id
    CALL getoid(fid, name, oid)         ! object id
    CALL h5aget_num_attrs_f(oid, numatts, ierr)
    CALL closeid(oid)
  END FUNCTION numatts
!===========================================================================
  SUBROUTINE allatts(fid, name, attnames, atttypes, attsizes)
!
!  Get all attributes in group or dataset
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name
    CHARACTER(len=*), DIMENSION(:), INTENT(out) :: attnames
    CHARACTER(len=1), DIMENSION(:), INTENT(out) :: atttypes
    INTEGER(SIZE_T), DIMENSION(:), intent(out) :: attsizes
    INTEGER :: i, ierr
    INTEGER(HID_T) :: id, oid, attr_space, attr_id, type_id
    INTEGER(SIZE_T) :: s, lattnm
!
    id = file_id(fid)                   ! file id
    CALL getoid(fid, name, oid)         ! object id
    DO i=1,SIZE(attnames,1)
       CALL h5aopen_idx_f(oid, i-1, attr_id, ierr)
       lattnm=LEN(attnames(i))
       CALL h5aget_name_f(attr_id, lattnm, attnames(i), ierr)
       CALL h5aget_type_f(attr_id, type_id, ierr)
       CALL gettype(type_id, atttypes(i), attsizes(i))
       CALL h5tclose_f(type_id, ierr)
       CALL h5aclose_f(attr_id, ierr)
    END DO
    CALL closeid(oid)
  END SUBROUTINE allatts
!===========================================================================
  SUBROUTINE gettype(type_id, t, s)
!
!  Data type and size (in bytes) from type id
!
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(in) :: type_id
    CHARACTER(len=1), INTENT(out) :: t
    INTEGER(SIZE_T), INTENT(out) :: s
    INTEGER :: ierr, class
!
    CALL h5tget_class_f(type_id, class, ierr)
    CALL h5tget_size_f(type_id, s, ierr)
!
    t = 'U'
    IF( class .EQ. H5T_FLOAT_F ) THEN
       IF( s .EQ. 4) THEN
          t = 'S'
       ELSE IF( s .EQ. 8 ) THEN
          t = 'R'
       END IF
    ELSE IF( class .EQ. H5T_INTEGER_F ) THEN
       t = 'I'
    ELSE IF( class .EQ. H5T_STRING_F ) THEN
       t= 'C'
    END IF
  END SUBROUTINE gettype
!===========================================================================
  SUBROUTINE extend( fid, name, length )
!
!    Extend a dataset.
!
    INTEGER, INTENT(IN) :: length
    INTEGER, INTENT(in) :: fid
    CHARACTER(len=*), INTENT(in) :: name
!
    INTEGER(HID_T) :: id, did, dspace_id
    INTEGER(HSIZE_T), DIMENSION(maxrank) :: dims, maxdims
    INTEGER :: n, rank, rank_array, ierr
    INTEGER :: pdim, s, nlocal, nglobal
!
    id = file_id(fid)                   ! file id
!
!  Get dims/rank of dataset
    CALL h5dopen_f(id, name, did, ierr)
    CALL h5dget_space_f(did, dspace_id, ierr)
    CALL h5sget_simple_extent_ndims_f(dspace_id, rank, ierr)
    IF(rank>maxrank) THEN
       PRINT*, "Rank of matrix exceeds MAXRANK =", maxrank
       STOP
    END IF
    CALL h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, ierr)
    rank = ierr
    CALL h5sclose_f(dspace_id, ierr)
    dims(rank) = dims(rank) + length
!
!
    CALL h5dextend_f(did, dims, ierr)
    CALL h5dclose_f(did, ierr)
  END SUBROUTINE extend
!===========================================================================
  SUBROUTINE file_complex(real_type, complex_type)
!
!    Complex type for file using HDF5 compound type
!
    INTEGER(HID_T), INTENT(in) :: real_type
    INTEGER(HID_T) , INTENT(out) :: complex_type
    INTEGER(SIZE_T) :: offset
    INTEGER(SIZE_T) :: tsize_real, tsize_complex
    INTEGER :: ierr
!
    CALL h5tget_size_f(real_type, tsize_real, ierr)
    tsize_complex = 2*tsize_real
    CALL h5tcreate_f(H5T_COMPOUND_F, tsize_complex, complex_type, ierr)
    offset = 0
    CALL h5tinsert_f(complex_type, 'real', offset, real_type, ierr)
    offset = offset + tsize_real
    CALL h5tinsert_f(complex_type, 'imaginary', offset, real_type, ierr)

  END SUBROUTINE file_complex
!===========================================================================
  SUBROUTINE mem_complex(real_type, realpart, impart)
!
!    Complex type for memory using HDF5 compound type
!
    INTEGER(HID_T), INTENT(in) :: real_type
    INTEGER(HID_T) , INTENT(out) :: realpart, impart
    INTEGER(SIZE_T) :: offset
    INTEGER(SIZE_T) :: tsize_real
    INTEGER :: ierr
!
    CALL h5tget_size_f(real_type, tsize_real, ierr)
    CALL h5tcreate_f(H5T_COMPOUND_F, tsize_real, realpart, ierr)
    offset = 0
    CALL h5tinsert_f(realpart, 'real', offset, real_type, ierr)
    CALL h5tcreate_f(H5T_COMPOUND_F, tsize_real, impart, ierr)
    offset = 0
    CALL h5tinsert_f(impart, 'imaginary', offset, real_type, ierr)

  END SUBROUTINE mem_complex
!===========================================================================
  SUBROUTINE geth5ver(libver, l)
!
!    Get hdf5 library version
!
    CHARACTER(len=*), INTENT(out) :: libver
    INTEGER, intent(out)          :: l
!
    INTEGER :: majnum, minnum, relnum, ierr
    CHARACTER(len=3) :: majn, minn, reln
    CALL h5get_libversion_f(majnum, minnum, relnum, ierr)
    WRITE(majn,'(i3)') majnum
    WRITE(minn,'(i3)') minnum
    WRITE(reln,'(i3)') relnum
    libver = TRIM(ADJUSTL(majn))//'.'//TRIM(ADJUSTL(minn))//'.'//TRIM(ADJUSTL(reln))
    l = LEN_TRIM(libver)
  END SUBROUTINE geth5ver
!===========================================================================
  SUBROUTINE flushh5(fid,scope)
    IMPLICIT NONE
    INTEGER, INTENT(IN)           :: fid    ! Object identifier
    INTEGER, INTENT(IN),optional  :: scope  ! Flag with two possible values:
                                            ! H5F_SCOPE_GLOBAL_F
                                            ! H5F_SCOPE_LOCAL_F (default)
    INTEGER :: hdferr
    INTEGER(HID_T) :: id

    id = file_id(fid)

    if (present(scope)) then
       call h5fflush_f(id,scope,hdferr)
    else
       call h5fflush_f(id,H5F_SCOPE_LOCAL_F,hdferr)
    endif

  END SUBROUTINE flushh5
!===========================================================================
  SUBROUTINE check_gexist(fid, group_name, group_exists, hdferr)
    ! Check if group with the given name exists.
    IMPLICIT NONE
    INTEGER, INTENT(IN)           :: fid    ! File identifier
    CHARACTER(len=*), INTENT(IN) :: group_name
    LOGICAL, INTENT(OUT) :: group_exists
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T) :: id

    id = file_id(fid)

    CALL h5lexists_f(id, group_name, group_exists, hdferr)
  END SUBROUTINE check_gexist
!===========================================================================
  SUBROUTINE create_external_link(file_name, obj_name, fid, link_name)
    ! Creates an external link, a soft link to an object in a different file.
    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN) :: file_name
    ! Name of the file containing the target object. Neither
    ! the file nor the target object is required to exist.
    ! May be the file the link is being created in.
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    ! Name of the target object, which need not already exist.
    INTEGER, INTENT(IN)           :: fid
    ! The file identifier for the new link.
    CHARACTER(LEN=*), INTENT(IN) :: link_name
    ! The name of the new link.
    !
    INTEGER(HID_T) :: id
    INTEGER :: hdferr
    ! Error code:
    ! 0 on success and -1 on failure

    id = file_id(fid)

    CALL h5lcreate_external_f(file_name, obj_name, id, link_name, hdferr)

  END SUBROUTINE create_external_link
!===========================================================================
END MODULE futils
