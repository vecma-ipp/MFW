!        ###################
!
!                                        AUTHORS:
!                                        X. LAPILLONNE, CRPP-EPFL
!                                        O. SAUTER,  CRPP-EPFL
!*******************************************************************************************
!                                                                                          *
! ROUTINES used in ogyropsi.f90 to write out in ASCII and HDF5 format                          *
!*******************************************************************************************

SUBROUTINE INIT_WRITE_OUT(ioutgyro,ioutgyro_hdf5,pfile)
  ! Create HDF5 and ASCII fill
  USE futils
  IMPLICIT NONE
  INTEGER :: ioutgyro, ioutgyro_hdf5
  LOGICAL :: op
  CHARACTER :: PFILE*(*)
  !
  ! Open hdf5 file
  CALL creatf(PFILE//'.h5', ioutgyro_hdf5, &
       &      desc="Output from CHEASE for gyrokinetic codes GENE & ORB5", &
       &      real_prec='d')
  ! Create data groups
  CALL creatg(ioutgyro_hdf5, "/data", "group containing CHEASE output")
  CALL creatg(ioutgyro_hdf5, "/data/grid", "PSI - CHI grid")
  CALL creatg(ioutgyro_hdf5, "/data/var1d", "1-dim quantities fct of (PSI)")
  CALL creatg(ioutgyro_hdf5, "/data/var2d","2-dim quantities fct of (PSI,CHI)")
  CALL creatg(ioutgyro_hdf5, "/inputs", "Inputs specific to the run")
  ! Write INPUT file in HDDF5
  CALL putfile(ioutgyro_hdf5, '/inputs/STDIN', 'chease_namelist')
  !
  !
  ! Open ASCII file
  OPEN(ioutgyro,file=PFILE//'.dat')

END SUBROUTINE INIT_WRITE_OUT

SUBROUTINE CLOSE_WRITE_OUT(ioutgyro,ioutgyro_hdf5)
  ! Close HDF5 and ASCII fill
  USE futils
  IMPLICIT NONE
  INTEGER :: ioutgyro, ioutgyro_hdf5
  !
  close(ioutgyro)
  CALL closef(ioutgyro_hdf5)
END SUBROUTINE CLOSE_WRITE_OUT

SUBROUTINE WRITE_OUT_SCALAR_INT(data_name,data,ioutgyro,ioutgyro_hdf5,h5_path)
  USE futils
  IMPLICIT NONE
  INTEGER :: ioutgyro, ioutgyro_hdf5, data
  CHARACTER(*) :: data_name, h5_path
  ! WRITE ASCII
  WRITE(ioutgyro,9200) data_name
  WRITE(ioutgyro,9101) data
  ! WRITE HDF5
  CALL attach(ioutgyro_hdf5,h5_path ,data_name , data)

9200 format(A)
9101 format(I4)
END SUBROUTINE WRITE_OUT_SCALAR_INT

SUBROUTINE WRITE_OUT_SCALAR_RE(data_name,data,ioutgyro,ioutgyro_hdf5,h5_path)
  USE futils
  USE prec_const
  IMPLICIT NONE
  INTEGER :: ioutgyro, ioutgyro_hdf5
  REAL(RKIND) :: data
  CHARACTER(*) :: data_name, h5_path
  ! WRITE ASCII
  WRITE(ioutgyro,9200) data_name
  WRITE(ioutgyro,9102) data
  ! WRITE HDF5
  CALL attach(ioutgyro_hdf5,h5_path,data_name,data)

9200 format(A)
9102 format(1PE20.10)
END SUBROUTINE WRITE_OUT_SCALAR_RE

SUBROUTINE WRITE_OUT_1D(data_name,data,N,ioutgyro,ioutgyro_hdf5,h5_path)
  USE futils
  USE prec_const
  IMPLICIT NONE
  INTEGER :: ioutgyro, ioutgyro_hdf5, N, i
  REAL(RKIND), DIMENSION(1:N) :: data
  CHARACTER(*) :: data_name, h5_path
  ! WRITE ASCII
  WRITE(ioutgyro,9200) data_name
  WRITE(ioutgyro,9100) (data(i),i=1,N)
  ! WRITE HDF5
  CALL putarr(ioutgyro_hdf5, h5_path , data)

9200 format(A)
9100 format(1P5E20.10)
END SUBROUTINE WRITE_OUT_1D

SUBROUTINE WRITE_OUT_2D(data_name,data,N1,N2,ioutgyro,ioutgyro_hdf5,h5_path)
  USE futils
  USE prec_const
  IMPLICIT NONE
  INTEGER :: ioutgyro, ioutgyro_hdf5, N1, N2,i ,j
  REAL(RKIND), DIMENSION(1:N1,1:N2) :: data
  CHARACTER(*) :: data_name, h5_path
  ! WRITE ASCII
  WRITE(ioutgyro,9200) data_name
  WRITE(ioutgyro,9100) ((data(i,j),i=1,N1),j=1,N2)
  ! WRITE HDF5
  CALL putarr(ioutgyro_hdf5, h5_path , data)

9200 format(A)
9100 format(1P5E20.10)
END SUBROUTINE WRITE_OUT_2D
