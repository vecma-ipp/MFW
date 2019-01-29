!*DECK MR03
!*CALL PROCESS
SUBROUTINE DWY(A,U,N,NP,M,MP,NKV)
  !        ---------------------------------
  !
  !     SOLVE D*W=Y                                                    
  !                                                                    
  !     VERSION 1C           13.9.74     RALF GRUBER    CRPP LAUSANNE  
  !                                                                    
  !  U               KV SETS OF N-VECTORS, (BOTH X AND Y)              
  !  A               DIAGONAL MATRIX D                                 
  !                    (DIAGONAL IS FIRST COMPONENT OF BAND OF LENGTH M)
  !
  !         USE globals, except_a => a
  USE prec_const
  IMPLICIT NONE
  INTEGER          ::     NKV
  INTEGER          ::     M
  REAL(RKIND)      ::     A
  REAL(RKIND)      ::     U
  INTEGER          ::     J1
  INTEGER          ::     IKD
  INTEGER          ::     NP
  INTEGER          ::     MP
  INTEGER          ::     N
  DIMENSION &
       &   A(N*MP),   U(NP)
  !
  !---------------------------------------------------------------------
  !
  IKD = 1
  !
  !      STORE VALUE
  !
  DO J1=1,N
     U(J1) = U(J1) / A(IKD)
     IKD = IKD + MP
  END DO
  !
  RETURN
END SUBROUTINE DWY
