!*DECK MR04
!*CALL PROCESS
SUBROUTINE LTXW(A,U,N,NP,M,MP)
  !        ------------------------------
  !
  !     SOLVE LT*X=W                                                   
  !                                                                    
  !     VERSION 1C           13.9.74     RALF GRUBER    CRPP LAUSANNE  
  !                                                                    
  !     LT IS STORED IN THE OFF DIAGONAL PART OF THE BAND MATRIX A     
  !     WITH HALF BAND WIDTH M AND LENGTH N .                          
  !     U ARE THE KV VECTORS OF N COMPONENTS , REPRESENTING AT INPUT   
  !     THE RIGHT SIDE PART OF THE SYSTEM OF LINEAR EQUATIONS .        
  !     ALL CALCULATIONS ARE PERFORMED IN U .                          
  !                                                                    
  !     FIRST COMPONENT UNCHANGED ( LT CONTAINS 1 ON DIAGONAL)         
  !
  !         USE globals, except_a => a
  USE prec_const
  IMPLICIT NONE
  REAL(RKIND)      ::     A
  REAL(RKIND)      ::     DDOT
  REAL(RKIND)      ::     U
  REAL(RKIND)      ::     UJ
  INTEGER          ::     IJV
  INTEGER          ::     M
  INTEGER          ::     LOPBND
  INTEGER          ::     J2
  INTEGER          ::     IKD
  INTEGER          ::     NP
  INTEGER          ::     MP
  INTEGER          ::     N
  DIMENSION &
       &   A(N*MP),   U(NP)
  !
  !     SCAN OVER ALL COMPONENTS
  !
  IKD = (N - 1) * MP
  !
  DO J2=2,N
     LOPBND = M
     !
     IF (J2 .LT. M) LOPBND = J2
     !
     IJV = N - J2
     IKD = IKD - MP
     !
     UJ = U(IJV+1)-DDOT(LOPBND-1,U(IJV+2),1,A(IKD+2),1)
     !
     U(IJV+1) = UJ
  END DO
  !
  RETURN
END SUBROUTINE LTXW
