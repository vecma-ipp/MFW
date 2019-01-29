PROGRAM main
! HDF5 datatype codes
!
  USE hdf5
  IMPLICIT NONE
  INTEGER :: ierr, d0=0
  INTEGER :: majnum, minnum, relnum
  CHARACTER(len=3) :: majn, minn, reln
  CHARACTER(len=64) :: libver
!
  CALL h5open_f(ierr)
  CALL h5get_libversion_f(majnum, minnum, relnum, ierr)
  WRITE(majn,'(i3)') majnum
  WRITE(minn,'(i3)') minnum
  WRITE(reln,'(i3)') relnum
  libver = TRIM(ADJUSTL(majn))//'.'//TRIM(ADJUSTL(minn))//'.'//TRIM(ADJUSTL(reln))
  WRITE(*,'(a)') TRIM(libver)
!!$  d0=H5T_NATIVE_INTEGER
  WRITE(*,'(a24,i12)') 'H5T_NATIVE_INTEGER', H5T_NATIVE_INTEGER - d0
  WRITE(*,'(a24,i12)') 'H5T_NATIVE_REAL', H5T_NATIVE_REAL - d0
  WRITE(*,'(a24,i12)') 'H5T_NATIVE_DOUBLE', H5T_NATIVE_DOUBLE - d0
  WRITE(*,'(a24,i12)') 'H5T_NATIVE_CHARACTER', H5T_NATIVE_CHARACTER - d0
  WRITE(*,'(a24,i12)') 'H5T_STD_I8BE', H5T_STD_I8BE - d0
  WRITE(*,'(a24,i12)') 'H5T_STD_I8LE', H5T_STD_I8LE - d0
  WRITE(*,'(a24,i12)') 'H5T_STD_I16BE', H5T_STD_I16BE - d0
  WRITE(*,'(a24,i12)') 'H5T_STD_I16LE', H5T_STD_I16LE - d0
  WRITE(*,'(a24,i12)') 'H5T_STD_I32BE', H5T_STD_I32BE - d0
  WRITE(*,'(a24,i12)') 'H5T_STD_I32LE', H5T_STD_I32LE - d0
  WRITE(*,'(a24,i12)') 'H5T_STD_I64BE', H5T_STD_I64BE - d0
  WRITE(*,'(a24,i12)') 'H5T_STD_I64LE', H5T_STD_I64LE - d0
  WRITE(*,'(a24,i12)') 'H5T_STD_U8BE', H5T_STD_U8BE - d0
  WRITE(*,'(a24,i12)') 'H5T_STD_U8LE', H5T_STD_U8LE - d0
  WRITE(*,'(a24,i12)') 'H5T_STD_U16BE', H5T_STD_U16BE - d0
  WRITE(*,'(a24,i12)') 'H5T_STD_U16LE', H5T_STD_U16LE - d0
  WRITE(*,'(a24,i12)') 'H5T_STD_U32BE', H5T_STD_U32BE - d0
  WRITE(*,'(a24,i12)') 'H5T_STD_U32LE', H5T_STD_U32LE - d0
  WRITE(*,'(a24,i12)') 'H5T_STD_U64BE', H5T_STD_U64BE - d0
  WRITE(*,'(a24,i12)') 'H5T_STD_U64LE', H5T_STD_U64LE - d0
  WRITE(*,'(a24,i12)') 'H5T_IEEE_F32BE', H5T_IEEE_F32BE - d0
  WRITE(*,'(a24,i12)') 'H5T_IEEE_F32LE', H5T_IEEE_F32LE - d0
  WRITE(*,'(a24,i12)') 'H5T_IEEE_F64BE', H5T_IEEE_F64BE - d0
  WRITE(*,'(a24,i12)') 'H5T_IEEE_F64LE', H5T_IEEE_F64LE - d0
END PROGRAM main
