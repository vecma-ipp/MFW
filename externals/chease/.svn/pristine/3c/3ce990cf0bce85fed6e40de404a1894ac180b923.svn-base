PROGRAM main
!
!   Extract files from HDF5 datasets
!
  USE futils
  IMPLICIT NONE
  CHARACTER(len=256) :: file, path, name
  INTEGER :: n, fid, ierr, nargs
!===========================================================================
!
  nargs = command_argument_count()
  IF( nargs .GE. 2 ) THEN
     CALL get_command_argument(1, file, n, ierr)
     CALL get_command_argument(2, name, n, ierr)
     IF (nargs .EQ. 3) THEN
        CALL get_command_argument(3, path, n, ierr)
     END IF
  ELSE
     WRITE(*,'(a)') 'Usage: getfile <file> <name> [path]'
     STOP
  END IF
!
  CALL openf(file, fid, mode='r')
  IF( nargs .EQ. 2 )  THEN  
     CALL getfile(fid, name)
  ELSE
     CALL getfile(fid, name, path)
  END IF
!
  CALL closef(fid)
END PROGRAM main
