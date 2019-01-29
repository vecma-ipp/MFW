!> Module for performing FFT.
!
! isign=-1 gives a forward FFT, i.e. exp(-2 pi i)
! isign=1  gives a backward FFT, i.e. exp(2 pi i)
!
MODULE fft
!
  IMPLICIT NONE
!
  PRIVATE
  PUBLIC :: four1D_real, fourcol_real, fourrow_real
  PUBLIC :: four2D_real
  PUBLIC :: four1D, fourcol, fourrow
  PUBLIC :: FFT_FORWARD, FFT_BACKWARD
!
  INCLUDE 'fftw3.f'
!
  TYPE int_para
     INTEGER, DIMENSION(2) :: par ! size of transform
  END TYPE int_para
!
! Global parameters
!
  INTEGER, PARAMETER :: I8 = SELECTED_INT_KIND(18)
  INTEGER, PARAMETER :: FFT_FORWARD=-1, FFT_BACKWARD=1
!
! define the maximum number of FFT Subroutines.
!
  INTEGER, PARAMETER :: MAXSUBS1=8
  INTEGER, PARAMETER :: MAXSUBS2=1
!
! define the maximum number of plans.
!
  INTEGER, PARAMETER :: MAXPLAN=16
!
! Global variables
!
  INTEGER(I8),    DIMENSION(MAXPLAN,MAXSUBS1), SAVE :: plan1d      ! plans for 1-dim FFT
  TYPE(int_para), DIMENSION(MAXPLAN,MAXSUBS1), SAVE :: n1d_par
  INTEGER,        DIMENSION(MAXSUBS1),         SAVE :: n1d_saved=0 ! number of plans saved
!
  INTEGER(I8),    DIMENSION(MAXPLAN,MAXSUBS2), SAVE :: plan2d      ! plans for 2-dim FFT
  TYPE(int_para), DIMENSION(MAXPLAN,MAXSUBS2), SAVE :: n2d_par
  INTEGER,        DIMENSION(1),                SAVE :: n2d_saved=0 ! number of plans saved
!
  INTERFACE four1D_real
     MODULE PROCEDURE four1D_ra_ca
  END INTERFACE
!
  INTERFACE fourcol_real
     MODULE PROCEDURE fourcol_ra_ca, fourcol_raa_caa
  END INTERFACE
!
  INTERFACE fourrow_real
     MODULE PROCEDURE fourrow_ra_ca
  END INTERFACE
!
  INTERFACE four2D_real
     MODULE PROCEDURE four2D_ra_ca
  END INTERFACE
!
  INTERFACE four1D
     MODULE PROCEDURE four1D_ca
  END INTERFACE
!
  INTERFACE fourcol
     MODULE PROCEDURE fourcol_ca, fourcol_caa
  END INTERFACE
!
  INTERFACE fourrow
     MODULE PROCEDURE fourrow_ca
  END INTERFACE
!
CONTAINS
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
  SUBROUTINE four1D_ra_ca(vec_ra, vec_ca, isign)
!
! Dummy arguments
!
    REAL,    DIMENSION(:), INTENT(INOUT) :: vec_ra
    COMPLEX, DIMENSION(:), INTENT(INOUT) :: vec_ca
    INTEGER,               INTENT(IN)    :: isign
!
! Local parameters
!
    INTEGER, PARAMETER :: NUM=1
!
! Local variables
!
    INTEGER :: k
    INTEGER :: dim1_ra, dim1_ca, i, id, istat
    REAL,    DIMENSION(:), ALLOCATABLE :: vec_ra_tmp
    COMPLEX, DIMENSION(:), ALLOCATABLE :: vec_ca_tmp
!
!
    dim1_ra = SIZE(vec_ra)
!
! test if a plan that fits is already created.
!
    id = -1
    k = isign*dim1_ra
    DO i = 1,n1d_saved(NUM)
       IF (k == n1d_par(i,NUM)%par(1)) THEN
          id = i
          EXIT
       END IF
    END DO
!
    SELECT CASE (id)
    CASE (-1)
!
! test if the maximal number of plans is alredy reached.
!
       IF (n1d_saved(NUM) == MAXPLAN) THEN
          WRITE (*,*) 'FOUR1D_RA_CA: MAXPLAN too small! Increase it and recompile'
          STOP 'FOUR1D_RA_CA'
       END IF
!
       ALLOCATE(vec_ra_tmp(dim1_ra), stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOUR1D_RA_CA: Allocation of  vec_ra_tmp  failed!'
          STOP 'FOUR1D_RA_CA'
       END IF
!
       dim1_ca = SIZE(vec_ca)
       ALLOCATE(vec_ca_tmp(dim1_ca), stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOUR1D_RA_CA: Allocation of  vec_ca_tmp  failed!'
          STOP 'FOUR1D_RA_CA'
       END IF
!
       n1d_saved(NUM) = n1d_saved(NUM)+1
       n1d_par(n1d_saved(NUM),NUM)%par(1) = k
       id = n1d_saved(NUM)
!
       SELECT CASE (isign)
       CASE (-1)
          CALL dfftw_plan_dft_r2c_1d(plan1d(id,NUM), dim1_ra, vec_ra_tmp(1), &
               vec_ca_tmp(1), FFTW_ESTIMATE)
       CASE (1)
          CALL dfftw_plan_dft_c2r_1d(plan1d(id,NUM), dim1_ra, vec_ca_tmp(1), &
               vec_ra_tmp(1), FFTW_ESTIMATE)
       END SELECT
!
       DEALLOCATE(vec_ra_tmp, stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOUR1D_RA_CA: Dellocation of  vec_ra_tmp  failed!'
          STOP 'FOUR1D_RA_CA'
       END IF
!
       DEALLOCATE(vec_ca_tmp, stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOUR1D_RA_CA: Dellocation of  vec_ca_tmp  failed!'
          STOP 'FOUR1D_RA_CA'
       END IF
!
    END SELECT
!
! Using the Guru execution of plans.
!
    SELECT CASE (isign)
    CASE (-1)
       CALL dfftw_execute_dft_r2c(plan1d(id,NUM), vec_ra(1), vec_ca(1))
    CASE (1)
       CALL dfftw_execute_dft_c2r(plan1d(id,NUM), vec_ca(1), vec_ra(1))
    END SELECT
!
  END SUBROUTINE four1D_ra_ca
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
  SUBROUTINE fourcol_ra_ca(arr_ra, arr_ca, isign)
!
! Dummy arguments
!
    REAL,    DIMENSION(:,:), INTENT(INOUT) :: arr_ra
    COMPLEX, DIMENSION(:,:), INTENT(INOUT) :: arr_ca
    INTEGER,                 INTENT(IN)    :: isign
!
! Local parameters
!
    INTEGER, PARAMETER :: NUM=2, RANK=1 
!
! Local variables
!
    INTEGER :: k
    INTEGER :: dim1_ra, dim2_ra, dim1_ca, dim2_ca
    INTEGER :: idist, odist, howmany, i, id, istat
    INTEGER, DIMENSION(RANK)             :: n_arr, inembed, onembed
    REAL,    DIMENSION(:,:), ALLOCATABLE :: arr_ra_tmp
    COMPLEX, DIMENSION(:,:), ALLOCATABLE :: arr_ca_tmp
!
    dim1_ra = SIZE(arr_ra,1)
    dim2_ra = SIZE(arr_ra,2)
    howmany = dim2_ra
!
! test if a plan that fits is already created.
!
    id = -1
    k = isign*dim1_ra
    DO i = 1,n1d_saved(NUM)
       IF (k == n1d_par(i,NUM)%par(1) .AND. &
            howmany == n1d_par(i,NUM)%par(2)) THEN
          id = i
          EXIT
       END IF
    END DO
!
    SELECT CASE (id)
    CASE (-1)
!
! test if the maximal number of plans is alredy reached.
!
       IF (n1d_saved(NUM) == MAXPLAN) THEN
          WRITE (*,*) 'FOURCOL_RA_CA: MAXPLAN too small! Increase it and recompile'
          STOP 'FOURCOL_RA_CA'
       END IF
!
       ALLOCATE(arr_ra_tmp(dim1_ra, dim2_ra), stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURCOL_RA_CA: Allocation of  arr_ra_tmp  failed!'
          STOP 'FOURCOL_RA_CA'
       END IF
!
       dim1_ca = SIZE(arr_ca,1)
       dim2_ca = SIZE(arr_ca,2)
       ALLOCATE(arr_ca_tmp(dim1_ca, dim2_ca), stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURCOL_RA_CA: Allocation of  arr_ca_tmp  failed!'
          STOP 'FOURCOL_RA_CA'
       END IF
!
       n1d_saved(NUM) = n1d_saved(NUM)+1
       n1d_par(n1d_saved(NUM),NUM)%par(1) = k
       n1d_par(n1d_saved(NUM),NUM)%par(2) = howmany
       id = n1d_saved(NUM)
!
       SELECT CASE (isign)
       CASE (-1)
          n_arr(1) = dim1_ra
          howmany  = dim2_ra
          inembed(1) = SIZE(arr_ra)
          onembed(1) = SIZE(arr_ca)
          idist = dim1_ra
          odist = dim1_ca
          CALL dfftw_plan_many_dft_r2c(plan1d(id,NUM), RANK, n_arr, howmany, &
               arr_ra_tmp(1,1), inembed, 1, idist, &
               arr_ca_tmp(1,1), onembed, 1, odist, &
               FFTW_ESTIMATE)
       CASE (1)
          n_arr(1) = dim1_ra
          howmany  = dim2_ca
          inembed(1) = SIZE(arr_ca)
          onembed(1) = SIZE(arr_ra)
          idist = dim1_ca
          odist = dim1_ra
          CALL dfftw_plan_many_dft_c2r(plan1d(id,NUM), RANK, n_arr, howmany, &
               arr_ca_tmp(1,1), inembed, 1, idist, &
               arr_ra_tmp(1,1), onembed, 1, odist, &
               FFTW_ESTIMATE)
       END SELECT
!
       DEALLOCATE(arr_ra_tmp, stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURCOL_RA_CA: Dellocation of  arr_ra_tmp  failed!'
          STOP 'FOURCOL_RA_CA'
       END IF
!
       DEALLOCATE(arr_ca_tmp, stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURCOL_RA_CA: Dellocation of  arr_ca_tmp  failed!'
          STOP 'FOURCOL_RA_CA'
       END IF
!
    END SELECT
!
! Using the Guru execution of plans.
!
    SELECT CASE (isign)
    CASE (-1)
       CALL dfftw_execute_dft_r2c(plan1d(id,NUM), arr_ra(1,1), arr_ca(1,1))
    CASE (1)
       CALL dfftw_execute_dft_c2r(plan1d(id,NUM), arr_ca(1,1), arr_ra(1,1))
    END SELECT
!
  END SUBROUTINE fourcol_ra_ca
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
  SUBROUTINE fourcol_raa_caa(arr_raa, arr_caa, isign)
!
! Dummy arguments
!
    REAL,    DIMENSION(:,:,:), INTENT(INOUT) :: arr_raa
    COMPLEX, DIMENSION(:,:,:), INTENT(INOUT) :: arr_caa
    INTEGER,                   INTENT(IN)    :: isign
!
! Local parameters
!
    INTEGER, PARAMETER :: NUM=3, RANK=1
!
! Local variables
!
    INTEGER :: k
    INTEGER :: dim1_raa, dim2_raa, dim3_raa, dim1_caa, dim2_caa, dim3_caa
    INTEGER :: idist, odist, howmany, i, id, istat
    INTEGER, DIMENSION(RANK)               :: n_arr, inembed, onembed
    REAL,    DIMENSION(:,:,:), ALLOCATABLE :: arr_raa_tmp
    COMPLEX, DIMENSION(:,:,:), ALLOCATABLE :: arr_caa_tmp
!
    dim1_raa = SIZE(arr_raa,1)
    dim2_raa = SIZE(arr_raa,2)
    dim3_raa = SIZE(arr_raa,3)
    howmany = dim2_raa*dim3_raa
!
! test if a plan that fits is already created.
!
    id = -1
    k = isign*dim1_raa
    DO i = 1,n1d_saved(NUM)
       IF (k == n1d_par(i,NUM)%par(1) .AND. &
            howmany == n1d_par(i,NUM)%par(2)) THEN
          id = i
          EXIT
       END IF
    END DO
!
    SELECT CASE (id)
    CASE (-1)
!
! test if the maximal number of plans is alredy reached.
!
       IF (n1d_saved(NUM) == MAXPLAN) THEN
          WRITE (*,*) 'FOURCOL_RAA_CAA: MAXPLAN too small! Increase it and recompile'
          STOP 'FOURCOL_RAA_CAA'
       END IF
!
       ALLOCATE(arr_raa_tmp(dim1_raa, dim2_raa, dim3_raa), stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURCOL_RAA_CAA: Allocation of  arr_raa_tmp  failed!'
          STOP 'FOURCOL_RAA_CAA'
       END IF
!
       dim1_caa = SIZE(arr_caa,1)
       dim2_caa = SIZE(arr_caa,2)
       dim3_caa = SIZE(arr_caa,3)
       ALLOCATE(arr_caa_tmp(dim1_caa, dim2_caa, dim3_caa), stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURCOL_RAA_CAA: Allocation of  arr_caa_tmp  failed!'
          STOP 'FOURCOL_RAA_CAA'
       END IF
!
       n1d_saved(NUM) = n1d_saved(NUM)+1
       n1d_par(n1d_saved(NUM),NUM)%par(1) = k
       n1d_par(n1d_saved(NUM),NUM)%par(2) = howmany
       id = n1d_saved(NUM)
!
       SELECT CASE (isign)
       CASE (-1)
          n_arr(1) = dim1_raa
          howmany  = dim2_raa*dim3_raa
          inembed(1) = SIZE(arr_raa)
          onembed(1) = SIZE(arr_caa)
          idist = dim1_raa
          odist = dim1_caa
          CALL dfftw_plan_many_dft_r2c(plan1d(id,NUM), RANK, n_arr, howmany, &
               arr_raa_tmp(1,1,1), inembed, 1, idist, &
               arr_caa_tmp(1,1,1), onembed, 1, odist, &
               FFTW_ESTIMATE)
       CASE (1)
          n_arr(1) = dim1_raa
          howmany  = dim2_caa*dim3_caa
          inembed(1) = SIZE(arr_caa)
          onembed(1) = SIZE(arr_raa)
          idist = dim1_caa
          odist = dim1_raa
          CALL dfftw_plan_many_dft_c2r(plan1d(id,NUM), RANK, n_arr, howmany, &
               arr_caa_tmp(1,1,1), inembed, 1, idist, &
               arr_raa_tmp(1,1,1), onembed, 1, odist, &
               FFTW_ESTIMATE)
       END SELECT
!
       DEALLOCATE(arr_raa_tmp, stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURCOL_RAA_CAA: Dellocation of  arr_raa_tmp  failed!'
          STOP 'FOURCOL_RAA_CAA'
       END IF
!
       DEALLOCATE(arr_caa_tmp, stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURCOL_RAA_CAA: Dellocation of  arr_caa_tmp  failed!'
          STOP 'FOURCOL_RAA_CAA'
       END IF
!
    END SELECT
!
! Using the Guru execution of plans.
!
    SELECT CASE (isign)
    CASE (-1)
       CALL dfftw_execute_dft_r2c(plan1d(id,NUM), arr_raa(1,1,1), arr_caa(1,1,1))
    CASE (1)
       CALL dfftw_execute_dft_c2r(plan1d(id,NUM), arr_caa(1,1,1), arr_raa(1,1,1))
    END SELECT
!
  END SUBROUTINE fourcol_raa_caa
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
  SUBROUTINE fourrow_ra_ca(arr_ra, arr_ca, isign)
!
! Dummy arguments
!
    REAL,    DIMENSION(:,:), INTENT(INOUT) :: arr_ra
    COMPLEX, DIMENSION(:,:), INTENT(INOUT) :: arr_ca
    INTEGER,                 INTENT(IN)    :: isign
!
! Local parameters
!
    INTEGER, PARAMETER :: NUM=4, RANK=1 
!
! Local variables
!
    INTEGER :: k
    INTEGER :: dim1_ra, dim2_ra, dim1_ca, dim2_ca
    INTEGER :: howmany, i, id, istat
    INTEGER, DIMENSION(RANK)             :: n_arr, inembed, onembed
    REAL,    DIMENSION(:,:), ALLOCATABLE :: arr_ra_tmp
    COMPLEX, DIMENSION(:,:), ALLOCATABLE :: arr_ca_tmp
!
    dim1_ra = SIZE(arr_ra,1)
    dim2_ra = SIZE(arr_ra,2)
    howmany = dim1_ra
!
! test if a plan that fits is already created.
!
    id = -1
    k = isign*dim2_ra
    DO i = 1,n1d_saved(NUM)
       IF (k == n1d_par(i,NUM)%par(1) .AND. &
            howmany == n1d_par(i,NUM)%par(2)) THEN
          id = i
          EXIT
       END IF
    END DO
!
    SELECT CASE (id)
    CASE (-1)
!
! test if the maximal number of plans is alredy reached.
!
       IF (n1d_saved(NUM) == MAXPLAN) THEN
          WRITE (*,*) 'FOURCOL_RA_CA: MAXPLAN too small! Increase it and recompile'
          STOP 'FOURCOL_RA_CA'
       END IF
!
       ALLOCATE(arr_ra_tmp(dim1_ra, dim2_ra), stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURCOL_RA_CA: Allocation of  arr_ra_tmp  failed!'
          STOP 'FOURCOL_RA_CA'
       END IF
!
       dim1_ca = SIZE(arr_ca,1)
       dim2_ca = SIZE(arr_ca,2)
       ALLOCATE(arr_ca_tmp(dim1_ca, dim2_ca), stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURCOL_RA_CA: Allocation of  arr_ca_tmp  failed!'
          STOP 'FOURCOL_RA_CA'
       END IF
!
       n1d_saved(NUM) = n1d_saved(NUM)+1
       n1d_par(n1d_saved(NUM),NUM)%par(1) = k
       n1d_par(n1d_saved(NUM),NUM)%par(2) = howmany
       id = n1d_saved(NUM)
!
       SELECT CASE (isign)
       CASE (-1)
          n_arr(1) = dim2_ra
          howmany  = dim1_ra
          inembed(1) = SIZE(arr_ra)
          onembed(1) = SIZE(arr_ca)
          CALL dfftw_plan_many_dft_r2c(plan1d(id,NUM), RANK, n_arr, howmany, &
               arr_ra_tmp(1,1), inembed, howmany, 1, &
               arr_ca_tmp(1,1), onembed, howmany, 1, &
               FFTW_ESTIMATE)
       CASE (1)
          n_arr(1) = dim2_ra
          howmany  = dim1_ca
          inembed(1) = SIZE(arr_ca)
          onembed(1) = SIZE(arr_ra)
          CALL dfftw_plan_many_dft_c2r(plan1d(id,NUM), RANK, n_arr, howmany, &
               arr_ca_tmp(1,1), inembed, howmany, 1,&
               arr_ra_tmp(1,1), onembed, howmany, 1, &
               FFTW_ESTIMATE)
       END SELECT
!
       DEALLOCATE(arr_ra_tmp, stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURCOL_RA_CA: Dellocation of  arr_ra_tmp  failed!'
          STOP 'FOURCOL_RA_CA'
       END IF
!
       DEALLOCATE(arr_ca_tmp, stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURCOL_RA_CA: Dellocation of  arr_ca_tmp  failed!'
          STOP 'FOURCOL_RA_CA'
       END IF
!
    END SELECT
!
! Using the Guru execution of plans.
!
    SELECT CASE (isign)
    CASE (-1)
       CALL dfftw_execute_dft_r2c(plan1d(id,NUM), arr_ra(1,1), arr_ca(1,1))
    CASE (1)
       CALL dfftw_execute_dft_c2r(plan1d(id,NUM), arr_ca(1,1), arr_ra(1,1))
    END SELECT
!
  END SUBROUTINE fourrow_ra_ca
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
  SUBROUTINE four2D_ra_ca(arr_ra, arr_ca, isign)
!
! Dummy arguments
!
    REAL,    DIMENSION(:,:), INTENT(INOUT) :: arr_ra
    COMPLEX, DIMENSION(:,:), INTENT(INOUT) :: arr_ca
    INTEGER,                 INTENT(IN)    :: isign
!
! Local parameters
!
    INTEGER, PARAMETER :: NUM=MAXSUBS2
!
! Local variables
!
    INTEGER :: k
    INTEGER :: dim1_ra, dim2_ra, dim1_ca, dim2_ca, i, id, istat
    REAL,    DIMENSION(:,:), ALLOCATABLE :: arr_ra_tmp
    COMPLEX, DIMENSION(:,:), ALLOCATABLE :: arr_ca_tmp
!
!
    dim1_ra = SIZE(arr_ra,1)
    dim2_ra = SIZE(arr_ra,2)
!
! test if a plan that fits is already created.
!
    id = -1
    k = isign*dim1_ra
    DO i = 1,n2d_saved(NUM)
       IF (k == n2d_par(i,NUM)%par(1) .AND. &
            dim2_ra == n2d_par(i,NUM)%par(2)) THEN
          id = i
          EXIT
       END IF
    END DO
!
    SELECT CASE (id)
    CASE (-1)
!
! test if the maximal number of plans is alredy reached.
!
       IF (n2d_saved(NUM) == MAXPLAN) THEN
          WRITE (*,*) 'FOUR2D_RA_CA: MAXPLAN too small! Increase it and recompile'
          STOP 'FOUR2D_RA_CA'
       END IF
!
       ALLOCATE(arr_ra_tmp(dim1_ra, dim2_ra), stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOUR2D_RA_CA: Allocation of  arr_ra_tmp  failed!'
          STOP 'FOUR2D_RA_CA'
       END IF
!
       dim1_ca = SIZE(arr_ca,1)
       dim2_ca = SIZE(arr_ca,2)
       ALLOCATE(arr_ca_tmp(dim1_ca, dim2_ca), stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOUR2D_RA_CA: Allocation of  arr_ca_tmp  failed!'
          STOP 'FOUR2D_RA_CA'
       END IF
!
       n2d_saved(NUM) = n2d_saved(NUM)+1
       n2d_par(n2d_saved(NUM),NUM)%par(1) = k
       n2d_par(n2d_saved(NUM),NUM)%par(2) = dim2_ra
       id = n2d_saved(NUM)
!
       SELECT CASE (isign)
       CASE (-1)
          CALL dfftw_plan_dft_r2c_2d(plan2d(id,NUM), dim1_ra, dim2_ra, &
               arr_ra_tmp(1,1), arr_ca_tmp(1,1), FFTW_ESTIMATE)
       CASE (1)
          CALL dfftw_plan_dft_c2r_2d(plan2d(id,NUM), dim1_ra, dim2_ra, &
               arr_ca_tmp(1,1), arr_ra_tmp(1,1), FFTW_ESTIMATE)
       END SELECT
!
       DEALLOCATE(arr_ra_tmp, stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOUR2D_RA_CA: Dellocation of  arr_ra_tmp  failed!'
          STOP 'FOUR2D_RA_CA'
       END IF
!
       DEALLOCATE(arr_ca_tmp, stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOUR2D_RA_CA: Dellocation of  arr_ca_tmp  failed!'
          STOP 'FOUR2D_RA_CA'
       END IF
!
    END SELECT
!
! Using the Guru execution of plans.
!
    SELECT CASE (isign)
    CASE (-1)
       CALL dfftw_execute_dft_r2c(plan2d(id,NUM), arr_ra(1,1), arr_ca(1,1))
    CASE (1)
       CALL dfftw_execute_dft_c2r(plan2d(id,NUM), arr_ca(1,1), arr_ra(1,1))
    END SELECT
!
  END SUBROUTINE four2D_ra_ca
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
  SUBROUTINE four1D_ca(vec_ca, isign)
!
! Dummy arguments
!
    COMPLEX, DIMENSION(:), INTENT(INOUT) :: vec_ca
    INTEGER,               INTENT(IN)    :: isign
!
! Local parameters
!
    INTEGER, PARAMETER :: NUM=5
!
! Local variables
!
    INTEGER :: k
    INTEGER :: dim1_ca, i, id, istat
    COMPLEX, DIMENSION(:), ALLOCATABLE :: vec_ca_tmp
!
!
    dim1_ca = SIZE(vec_ca)
!
! test if a plan that fits is already created.
!
    id = -1
    k = isign*dim1_ca
    DO i = 1,n1d_saved(NUM)
       IF (k == n1d_par(i,NUM)%par(1)) THEN
          id = i
          EXIT
       END IF
    END DO
!
    SELECT CASE (id)
    CASE (-1)
!
! test if the maximal number of plans is alredy reached.
!
       IF (n1d_saved(NUM) == MAXPLAN) THEN
          WRITE (*,*) 'FOUR1D_CA: MAXPLAN too small! Increase it and recompile'
          STOP 'FOUR1D_CA'
       END IF
!
       ALLOCATE(vec_ca_tmp(dim1_ca), stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOUR1D_CA: Allocation of  vec_ca_tmp  failed!'
          STOP 'FOUR1D_CA'
       END IF
!
       n1d_saved(NUM) = n1d_saved(NUM)+1
       n1d_par(n1d_saved(NUM),NUM)%par(1) = k
       id = n1d_saved(NUM)
!
       SELECT CASE (isign)
       CASE (-1)
          CALL dfftw_plan_dft_1d(plan1d(id,NUM), dim1_ca, &
               vec_ca_tmp(1), vec_ca_tmp(1), &
               FFTW_FORWARD, FFTW_ESTIMATE)
       CASE (1)
          CALL dfftw_plan_dft_1d(plan1d(id,NUM), dim1_ca, &
               vec_ca_tmp(1), vec_ca_tmp(1), &
               FFTW_BACKWARD, FFTW_ESTIMATE)
       END SELECT
!
       DEALLOCATE(vec_ca_tmp, stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOUR1D_CA: Dellocation of  vec_ca_tmp  failed!'
          STOP 'FOUR1D_CA'
       END IF
!
    END SELECT
!
    CALL dfftw_execute_dft(plan1d(id,NUM), vec_ca(1), vec_ca(1))
!
  END SUBROUTINE four1D_ca
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
  SUBROUTINE fourcol_ca(arr_ca, isign)
!
! Dummy arguments
!
    COMPLEX, DIMENSION(:,:), INTENT(INOUT) :: arr_ca
    INTEGER,                 INTENT(IN)    :: isign
!
! Local parameters
!
    INTEGER, PARAMETER :: NUM=6, RANK=1 
!
! Local variables
!
    INTEGER :: k
    INTEGER :: dim1_ca, dim2_ca, howmany, i, id, istat, n
    INTEGER, DIMENSION(RANK)             :: n_arr, nembed
    COMPLEX, DIMENSION(:,:), ALLOCATABLE :: arr_ca_tmp
!
    dim1_ca = SIZE(arr_ca,1)
    dim2_ca = SIZE(arr_ca,2)
    howmany = SIZE(arr_ca,2)
!
! test if a plan that fits is already created.
!
    id = -1
    k = isign*dim1_ca
    DO i = 1,n1d_saved(NUM)
       IF (k == n1d_par(i,NUM)%par(1) .AND. &
            howmany == n1d_par(i,NUM)%par(2)) THEN
          id = i
          EXIT
       END IF
    END DO
!
    SELECT CASE (id)
    CASE (-1)
!
! test if the maximal number of plans is alredy reached.
!
       IF (n1d_saved(NUM) == MAXPLAN) THEN
          WRITE (*,*) 'FOURCOL_CA: MAXPLAN too small! Increase it and recompile'
          STOP 'FOUR1D_CA'
       END IF
!
       ALLOCATE(arr_ca_tmp(dim1_ca, dim2_ca), stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURCOL_CA: Allocation of  arr_ca_tmp  failed!'
          STOP 'FOUR1D_CA'
       END IF
!
       nembed(1) = SIZE(arr_ca)
       n_arr(1)  = dim1_ca
       n         = n_arr(1)
!
       n1d_saved(NUM) = n1d_saved(NUM)+1
       n1d_par(n1d_saved(NUM),NUM)%par(1) = k
       n1d_par(n1d_saved(NUM),NUM)%par(2) = howmany
       id = n1d_saved(NUM)
!
       SELECT CASE (isign)
       CASE (-1)
          CALL dfftw_plan_many_dft(plan1d(id,NUM), RANK, n_arr, howmany, &
               arr_ca_tmp(1,1), nembed, 1, n, &
               arr_ca_tmp(1,1), nembed, 1, n, &
               FFTW_FORWARD, FFTW_ESTIMATE)
       CASE (1)
          CALL dfftw_plan_many_dft(plan1d(id,NUM), RANK, n_arr, howmany, &
               arr_ca_tmp(1,1), nembed, 1, n, &
               arr_ca_tmp(1,1), nembed, 1, n, &
               FFTW_BACKWARD, FFTW_ESTIMATE)
       END SELECT
!
       DEALLOCATE(arr_ca_tmp, stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURCOL_CA: Dellocation of  arr_ca_tmp  failed!'
          STOP 'FOUR1D_CA'
       END IF
!
    END SELECT
!
    CALL dfftw_execute_dft(plan1d(id,NUM), arr_ca(1,1), arr_ca(1,1))
!
  END SUBROUTINE fourcol_ca
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
  SUBROUTINE fourcol_caa(arr_caa, isign)
!
! Dummy arguments
!
    COMPLEX, DIMENSION(:,:,:), INTENT(INOUT) :: arr_caa
    INTEGER,                   INTENT(IN)    :: isign
!
! Local parameters
!
    INTEGER, PARAMETER :: NUM=7, RANK=1
!
! Local variables
!
    INTEGER :: k
    INTEGER :: dim1_caa, dim2_caa, dim3_caa, howmany, i, id, istat, n
    INTEGER, DIMENSION(RANK)               :: n_arr, nembed
    COMPLEX, DIMENSION(:,:,:), ALLOCATABLE :: arr_caa_tmp
!
    dim1_caa = SIZE(arr_caa,1)
    dim2_caa = SIZE(arr_caa,2)
    dim3_caa = SIZE(arr_caa,3)
    howmany = dim2_caa*dim3_caa
!
! test if a plan that fits is already created.
!
    id = -1
    k = isign*dim1_caa
    DO i = 1,n1d_saved(NUM)
       IF (k == n1d_par(i,NUM)%par(1) .AND. &
            howmany == n1d_par(i,NUM)%par(2)) THEN
          id = i
          EXIT
       END IF
    END DO
!
    SELECT CASE (id)
    CASE (-1)
!
! test if the maximal number of plans is alredy reached.
!
       IF (n1d_saved(NUM) == MAXPLAN) THEN
          WRITE (*,*) 'FOURCOL_CAA: MAXPLAN too small! Increase it and recompile'
          STOP 'FOURCOL_CAA'
       END IF
!
       ALLOCATE(arr_caa_tmp(dim1_caa, dim2_caa, dim3_caa), stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURCOL_CAA: Allocation of  arr_caa_tmp  failed!'
          STOP 'FOURCOL_CAA'
       END IF
!
       nembed(1) = SIZE(arr_caa)
       n_arr(1)  = dim1_caa
       n         = n_arr(1)
!
       n1d_saved(NUM) = n1d_saved(NUM)+1
       n1d_par(n1d_saved(NUM),NUM)%par(1) = k
       n1d_par(n1d_saved(NUM),NUM)%par(2) = howmany
       id = n1d_saved(NUM)
!
       SELECT CASE (isign)
       CASE (-1)
          CALL dfftw_plan_many_dft(plan1d(id,NUM), RANK, n_arr, howmany, &
               arr_caa_tmp(1,1,1), nembed, 1, n, &
               arr_caa_tmp(1,1,1), nembed, 1, n, &
               FFTW_FORWARD, FFTW_ESTIMATE)
       CASE (1)
          CALL dfftw_plan_many_dft(plan1d(id,NUM), RANK, n_arr, howmany, &
               arr_caa_tmp(1,1,1), nembed, 1, n, &
               arr_caa_tmp(1,1,1), nembed, 1, n, &
               FFTW_BACKWARD, FFTW_ESTIMATE)
       END SELECT
!
       DEALLOCATE(arr_caa_tmp, stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURCOL_CAA: Dellocation of  arr_caa_tmp  failed!'
          STOP 'FOURCOL_CAA'
       END IF
!
    END SELECT
!
    CALL dfftw_execute_dft(plan1d(id,NUM), arr_caa(1,1,1), arr_caa(1,1,1))
!
  END SUBROUTINE fourcol_caa
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
  SUBROUTINE fourrow_ca(arr_ca, isign)
!
! Dummy arguments
!
    COMPLEX, DIMENSION(:,:), INTENT(INOUT) :: arr_ca
    INTEGER,                 INTENT(IN)    :: isign
!
! Local parameters
!
    INTEGER, PARAMETER :: NUM=MAXSUBS1, RANK=1
!
! Local variables
!
    INTEGER :: k
    INTEGER :: dim1_ca, dim2_ca, howmany, i, id, istat
    INTEGER, DIMENSION(RANK)             :: n_arr, nembed
    COMPLEX, DIMENSION(:,:), ALLOCATABLE :: arr_ca_tmp
!
    dim1_ca = SIZE(arr_ca,1)
    dim2_ca = SIZE(arr_ca,2)
    howmany = dim1_ca
!
! test if a plan that fits is already created.
!
    id = -1
    k = isign*dim2_ca
    DO i = 1,n1d_saved(NUM)
       IF (k == n1d_par(i,NUM)%par(1) .AND. &
            howmany == n1d_par(i,NUM)%par(2)) THEN
          id = i
          EXIT
       END IF
    END DO
!
    SELECT CASE (id)
    CASE (-1)
!
! test if the maximal number of plans is alredy reached.
!
       IF (n1d_saved(NUM) == MAXPLAN) THEN
          WRITE (*,*) 'FOURROW_CA: MAXPLAN too small! Increase it and recompile'
          STOP 'FOURROW_CA'
       END IF
!
       ALLOCATE(arr_ca_tmp(dim1_ca, dim2_ca), stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURROW_CA: Allocation of  arr_ca_tmp  failed!'
          STOP 'FOURROW_CA'
       END IF
!
       nembed(1) = SIZE(arr_ca)
       n_arr(1)  = SIZE(arr_ca,2)
       howmany   = SIZE(arr_ca,1)
!
       n1d_saved(NUM) = n1d_saved(NUM)+1
       n1d_par(n1d_saved(NUM),NUM)%par(1) = k
       n1d_par(n1d_saved(NUM),NUM)%par(2) = howmany
       id = n1d_saved(NUM)
!
       SELECT CASE (isign)
       CASE (-1)
          CALL dfftw_plan_many_dft(plan1d(id,NUM), RANK, n_arr, howmany, &
               arr_ca_tmp(1,1), nembed, howmany, 1, &
               arr_ca_tmp(1,1), nembed, howmany, 1, &
               FFTW_FORWARD, FFTW_ESTIMATE)
       CASE (1)
          CALL dfftw_plan_many_dft(plan1d(id,NUM), RANK, n_arr, howmany, &
               arr_ca_tmp(1,1), nembed, howmany, 1, &
               arr_ca_tmp(1,1), nembed, howmany, 1, &
               FFTW_BACKWARD, FFTW_ESTIMATE)
       END SELECT
!
       DEALLOCATE(arr_ca_tmp, stat=istat)
       IF (istat /= 0) THEN
          WRITE (*,*) 'FOURROW_CA: Dellocation of  arr_ca_tmp  failed!'
          STOP 'FOURROW_CA'
       END IF
!
    END SELECT
!
    CALL dfftw_execute_dft(plan1d(id,NUM), arr_ca(1,1), arr_ca(1,1))
!
  END SUBROUTINE fourrow_ca
!
END MODULE fft
!
!
!
!
!
!

