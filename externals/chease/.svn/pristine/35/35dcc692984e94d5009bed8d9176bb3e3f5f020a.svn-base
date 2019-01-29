!*DECK MR01
!*CALL PROCESS
SUBROUTINE ALDLT(A,EPS,N,M,MP,NSING)
  !        ------------------------------------
  !
  !     DECOMPOSE A=L*D*LT                                             
  !                                                                    
  !     VERSION 1C           13.9.74     RALF GRUBER    CRPP LAUSANNE  
  !                                                                    
  !     A IS A BAND MATRIX WITH HALF WIDTH M AND LENGTH N              
  !     L CONTAINS 1 IN THE DIAGONAL                                   
  !     AS OUTPUT D REPLACES THE DIAGONAL OF A AND                     
  !     LT WITHOUT ITS DIAGONAL THE REST OF A                          
  !     ALL CALCULATIONS ARE PERFORMED IN A                            
  !     NSING = -1 WHEN A IS SINGULAR                                  
  !
  !
  !         USE globals, except_a => a
  USE prec_const
  IMPLICIT NONE
  REAL(RKIND)      ::     TOP
  INTEGER          ::     ITOP
  INTEGER          ::     JJB
  INTEGER          ::     IJ
  INTEGER          ::     I1
  INTEGER          ::     M
  INTEGER          ::     LOPBND
  INTEGER          ::     NSING
  REAL(RKIND)      ::     DIAG
  INTEGER          ::     JIB
  REAL(RKIND)      ::     EPS
  REAL(RKIND)      ::     A
  REAL(RKIND)      ::     AD
  INTEGER          ::     IKD
  INTEGER          ::     M1
  INTEGER          ::     MP
  INTEGER          ::     N
  DIMENSION &
       &   A(N*MP)
  !
  !     INITIALIZE
  !
  M1  = MP - 1
  IKD = 0
  AD  = ABS(A(1)) * EPS
  !
  !     SCAN OVER THE WHOLE LENGTH OF A
  !
  DO JIB=2,N
     DIAG=A(IKD+1)
     !
     !     TEST FOR ZERO PIVOT
     !
     IF (ABS(DIAG) .LT. AD) THEN
        NSING = -1
        RETURN
     ENDIF
     !
     !     RESTRICTION OF LOOP FOR NOT EXCEEDING BAND MATRIX
     !
     LOPBND = M
     I1     = N - JIB + 2
     !
     IF (I1 .LT. M) LOPBND = I1
     !
     !     DIAGONAL ELEMENT BEFORE GAUSS ELIMINATION
     !
     IJ = IKD + MP + 1
     AD = ABS(A(IJ)) * EPS
     !
     !     SETS THE ROW OF THE TRANSPOSED LEFT HAND SIDE MATRIX LT
     !
     DO JJB=2,LOPBND
        ITOP    = IKD + JJB
        TOP     = A(ITOP)
        A(ITOP) = A(ITOP) / DIAG
        !
        !     GAUSS RECTANGULAR RULE GOING DOWNWARDS
        !
        CALL DAXPY(JJB-1,-TOP,A(IKD+2),1,A(ITOP+M1),M1)
     END DO
     IKD = IKD + MP
  END DO
  !
  !     LAST DIAGONAL ELEMENT
  !
  IKD = (N - 1) * MP + 1
  IJ  = IKD - MP
  !
  IF (ABS(A(IKD)) .LT. ABS(A(IJ))*EPS) THEN
     NSING = -1
  ENDIF
  !
  RETURN
END SUBROUTINE ALDLT
