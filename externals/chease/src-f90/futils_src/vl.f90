PROGRAM main
  USE hdf5
  IMPLICIT NONE
  CHARACTER(len=32) :: file ='vl.h5'
  INTEGER :: iarr(5,6), siz(6), i, j
  INTEGER :: ierr
  INTEGER(HID_T) :: fid, dsid, vltype, did
  INTEGER(HSIZE_T) :: dims(2) = (/5,6/)
!===========================================================================
!
!   Define variable lenght array
!
  iarr = -100
  DO j=1,6
     siz(j) = j-1
     DO i=1,j-1
        iarr(i,j) = siz(j)
     END DO
  END DO
!
  WRITE(*, '(a,10i3)') 'siz', siz
  DO i=1,5
     WRITE(*,'(10i5)') iarr(i,:)
  END DO
!
!   HDF5 initialization
!
  CALL h5open_f(ierr)
  CALL h5fcreate_f(file, H5F_ACC_TRUNC_F, fid, ierr)
!
!   Create dataset
!
  CALL h5screate_simple_f(1, (/6_HSIZE_T/), dsid, ierr)
!
!   Create VL type
!
  CALL h5tvlen_create_f(H5T_NATIVE_INTEGER, vltype, ierr)
!
!   Create Dataset
!
  CALL h5dcreate_f(fid, "VLarray", vltype, dsid, did, ierr)
!
!   Write the dataset
!
  CALL h5dwrite_vl_f(did, vltype, iarr, dims, siz, ierr)
!
!   Clean up
!
  CALL h5dclose_f(did, ierr)
  CALL h5tclose_f(vltype, ierr)
  CALL h5sclose_f(dsid, ierr)
  CALL h5fclose_f(fid, ierr)
END PROGRAM main
