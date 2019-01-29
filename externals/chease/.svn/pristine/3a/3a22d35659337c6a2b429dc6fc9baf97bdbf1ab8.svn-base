!*DECK MR02
!*CALL PROCESS
SUBROUTINE LYV(A,U,N,NP,M,MP)
  !        -----------------------------
  !
  !     SOLVE L*Y=V                                                    
  !                                                                     
  !     VERSION 1C           13.9.74     RALF GRUBER    CRPP LAUSANNE   
  !                                                                     
  !     L IS STORED AS LT IN THE OFF DIAGONAL PART OF THE BAND MATRIX A 
  !     WITH HALF BAND WIDTH M AND LENGTH N .                           
  !     U ARE THE KV VECTORS OF N COMPONENTS , REPRESENTING AT INPUT    
  !     THE RIGHT SIDE PART OF THE SYSTEM OF LINEAR EQUATIONS .         
  !     ALL CALCULATIONS ARE PERFORMED IN U .                           
  !
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
  INTEGER          ::     M1
  INTEGER          ::     NP
  INTEGER          ::     MP
  INTEGER          ::     N
  DIMENSION &
       &   A(N*MP),   U(NP)
  !
  M1 = MP - 1
  !
  !     FIRST COMPONENT UNCHANGED (L CONTAINS 1 ON DIAGONAL)
  !
  IKD = 1
  !
  !     SCAN OVER ALL COMPONENTS
  !
  DO J2=2,N
     LOPBND = M
     !
     IF (J2 .LT. M) LOPBND = J2
     !
     IKD = IKD + MP
     IJV = J2 + 1
     !
     !%OS  needs the lowest index as start-up, to keep positive dimension in DDOT
     UJ = U(J2)-DDOT(LOPBND-1,U(IJV-LOPBND),-1, &
          &                               A(IKD-(LOPBND-1)*M1),-M1)
     !
     U(IJV-1) = UJ
  END DO
  !
  RETURN
END SUBROUTINE LYV
