!        ###################
!
!                                        AUTHORS:
!                                        X. LAPILLONNE, CRPP-EPFL
!                                        O. SAUTER,  CRPP-EPFL
!*******************************************************************************************
!                                                                                          *
! ROUTINES used in ogyropsi.f90 to write out in ASCII and HDF5 format                          *
!*******************************************************************************************

SUBROUTINE INIT_WRITE_OUT(ioutgyro,hdf5_ioutgyro,pfile)
  ! Create ASCII fill
  IMPLICIT NONE
  INTEGER :: ioutgyro, hdf5_ioutgyro
  LOGICAL :: op
  CHARACTER :: PFILE*(*)
  !
  ! Open ASCII file
  ioutgyro = 30
  DO
     INQUIRE(unit=ioutgyro,opened=op)
     IF (.NOT.op) EXIT
     ioutgyro = ioutgyro+1
  END DO
  print*, ioutgyro
  OPEN(ioutgyro,file=PFILE//'.dat')

END SUBROUTINE INIT_WRITE_OUT

SUBROUTINE CLOSE_WRITE_OUT(ioutgyro,hdf5_ioutgyro)
  ! Close ASCII fill
  IMPLICIT NONE
  INTEGER :: ioutgyro, hdf5_ioutgyro
  !
  close(ioutgyro)
END SUBROUTINE CLOSE_WRITE_OUT

SUBROUTINE WRITE_OUT_SCALAR_INT(data_name,data,ioutgyro,hdf5_ioutgyro,h5_path)
  IMPLICIT NONE
  INTEGER :: ioutgyro, hdf5_ioutgyro, data
  CHARACTER(*) :: data_name, h5_path
  ! WRITE ASCII
  WRITE(ioutgyro,9200) data_name
  WRITE(ioutgyro,9101) data

9200 format(A)
9101 format(I4)
END SUBROUTINE WRITE_OUT_SCALAR_INT

SUBROUTINE WRITE_OUT_SCALAR_RE(data_name,data,ioutgyro,hdf5_ioutgyro,h5_path)
  USE prec_const
  IMPLICIT NONE
  INTEGER :: ioutgyro, hdf5_ioutgyro
  REAL(RKIND) :: data
  CHARACTER(*) :: data_name, h5_path
  ! WRITE ASCII
  WRITE(ioutgyro,9200) data_name
  WRITE(ioutgyro,9102) data

9200 format(A)
9102 format(1PE20.10)
END SUBROUTINE WRITE_OUT_SCALAR_RE

SUBROUTINE WRITE_OUT_1D(data_name,data,N,ioutgyro,hdf5_ioutgyro,h5_path)
  USE prec_const
  IMPLICIT NONE
  INTEGER :: ioutgyro, hdf5_ioutgyro, N, i
  REAL(RKIND), DIMENSION(1:N) :: data
  CHARACTER(*) :: data_name, h5_path
  ! WRITE ASCII
  WRITE(ioutgyro,9200) data_name
  WRITE(ioutgyro,9100) (data(i),i=1,N)


9200 format(A)
9100 format(1P5E20.10)
END SUBROUTINE WRITE_OUT_1D

SUBROUTINE WRITE_OUT_2D(data_name,data,N1,N2,ioutgyro,hdf5_ioutgyro,h5_path)
  USE prec_const
  IMPLICIT NONE
  INTEGER :: ioutgyro, hdf5_ioutgyro, N1, N2,i ,j
  REAL(RKIND), DIMENSION(1:N1,1:N2) :: data
  CHARACTER(*) :: data_name, h5_path
  ! WRITE ASCII
  WRITE(ioutgyro,9200) data_name
  WRITE(ioutgyro,9100) ((data(i,j),i=1,N1),j=1,N2)

9200 format(A)
9100 format(1P5E20.10)
END SUBROUTINE WRITE_OUT_2D
