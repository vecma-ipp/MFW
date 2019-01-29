MODULE interpol
  USE prec_const
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: flinear                         ! Linear interpolation
  PUBLIC :: fqqq0, fqqq1, fqqq2             ! Quadratic interpolation
  PUBLIC :: fqdq0, fqdq1, fqdq2             ! Quadratic interpolation
  PUBLIC :: fd0, fd1, fd2
  PUBLIC :: fb0, fb1, fb2
  PUBLIC :: fcccc0, fcccc1, fcccc2, fcccc3  ! Cubic interpolation
  PUBLIC :: fcdcd0, fcdcd1, fcdcd2, fcdcd3  ! Cubic interpolation
  PUBLIC :: fc0, fc1, fc2, fc3

CONTAINS
  FUNCTION flinear(x1,x2,y1,y2,x)
    REAL(rkind) :: flinear
    REAL(rkind), INTENT(in) :: x1, x2, y1, y2, x
    flinear = y1 + (x-x1)*(y2-y1)/(x2-x1)
  END FUNCTION flinear
  ! ----------------------------------------------------------------------
  ! --     STATEMENT FUNCTION FOR QUADRATIC INTERPOLATION               --
  ! --                         19.01.87            AR        CRPP       --
  ! --                                                                  --
  ! -- QUADRATIC INTERPOLATION OF A FUNCTION F(X)                       --
  ! -- THE SIX PARAMETERS A1,A2,A3,B1,B2,B3 ARE DEFINED AS FOLLOWS:     --
  ! -- F(B1) = A1 , F(B2) = A2 , F(B3) = A3                             --
  ! ----------------------------------------------------------------------

  FUNCTION fb2(pa1,pa2,pa3,pb1,pb2,pb3)
    REAL(rkind) :: fb2
    REAL(rkind), INTENT(in) :: pa1,pa2,pa3,pb1,pb2,pb3
    fb2 = ((pa1-pa2)/(pb1-pb2)-(pa1-pa3)/(pb1-pb3))/(pb2-pb3)
  END FUNCTION fb2

  FUNCTION fb1(pa1,pa2,pa3,pb1,pb2,pb3)
    REAL(rkind) :: fb1
    REAL(rkind), INTENT(in) :: pa1,pa2,pa3,pb1,pb2,pb3
    fb1 = ((pa1-pa2)/(pb1-pb2)) -  fb2(pa1,pa2,pa3,pb1,pb2,pb3)*(pb1+pb2)
  END FUNCTION fb1

  FUNCTION fb0(pa1,pa2,pa3,pb1,pb2,pb3)
    REAL(rkind) :: fb0
    REAL(rkind), INTENT(in) :: pa1,pa2,pa3,pb1,pb2,pb3
    fb0 = pa1-fb1(pa1,pa2,pa3,pb1,pb2,pb3)*pb1 &
         & - fb2(pa1,pa2,pa3,pb1,pb2,pb3)*pb1*pb1
  END FUNCTION fb0
  FUNCTION fqqq0(pa1,pa2,pa3,pb1,pb2,pb3,pqx)
    ! -- FQQQ0 GIVES THE VALUE OF THE FUNCTION AT THE POINT PQX            --
    ! -- FQQQ0(......,PQX) = F(PQX)                                         --
    REAL(rkind) :: fqqq0
    REAL(rkind), INTENT(in) :: pa1,pa2,pa3,pb1,pb2,pb3,pqx
    fqqq0 = fb0(pa1,pa2,pa3,pb1,pb2,pb3) +&
         & pqx * (fb1(pa1,pa2,pa3,pb1,pb2,pb3) +&
         & pqx * fb2(pa1,pa2,pa3,pb1,pb2,pb3))
  END FUNCTION fqqq0

  FUNCTION fqqq1(pa1,pa2,pa3,pb1,pb2,pb3,pqx)
    ! -- FQQQ1 GIVES THE VALUE OF THE FIRST DERIVATIVE OF F(X) AT PQX      --
    ! -- FQQQ1(......,PQX) = DF/DX (PQX)                                    --
    REAL(rkind) :: fqqq1
    REAL(rkind), INTENT(in) :: pa1,pa2,pa3,pb1,pb2,pb3,pqx
    fqqq1 = fb1(pa1,pa2,pa3,pb1,pb2,pb3) +&
         &  2._rkind * pqx * fb2(pa1,pa2,pa3,pb1,pb2,pb3)
  END FUNCTION fqqq1

  FUNCTION fqqq2(pa1,pa2,pa3,pb1,pb2,pb3)
    ! -- FQQQ2 GIVES THE VALUE OF THE SECOND DERIVATIVE OF F(X) AT PQX     --
    ! -- FQQQ2(......,PQX) = D2F/DX2 (PQX)                                  --
    REAL(rkind) :: fqqq2
    REAL(rkind), INTENT(in) :: pa1,pa2,pa3,pb1,pb2,pb3
    fqqq2 = 2._rkind * fb2(pa1,pa2,pa3,pb1,pb2,pb3)
  END FUNCTION fqqq2
  ! ----------------------------------------------------------------------
  ! --     STATEMENT FUNCTION FOR QUADRATIC INTERPOLATION               --
  ! --                         19.01.87            AR        CRPP       --
  ! --                                                                  --
  ! -- QUADRATIC INTERPOLATION OF A FUNCTION F(X)                       --
  ! -- THE FIVE PARAMETERS X1,F1,P1,X2,F2    ARE DEFINED AS FOLLOWS:    --
  ! -- F(X1) = F1 , DF/DX(X1) = P1 , F(X2) = F2                         --
  ! ----------------------------------------------------------------------

  FUNCTION fd2(xq1,fq1,pq1,xq2,fq2)
    REAL(rkind) :: fd2
    REAL(rkind), INTENT(in) :: xq1,fq1,pq1,xq2,fq2
    fd2 = ((fq2-fq1)/(xq2-xq1) - pq1) / (xq2-xq1)
  END FUNCTION fd2

  FUNCTION fd1(xq1,fq1,pq1,xq2,fq2)
    REAL(rkind) :: fd1
    REAL(rkind), INTENT(in) :: xq1,fq1,pq1,xq2,fq2
    fd1 = pq1 - 2._rkind*xq1*fd2(xq1,fq1,pq1,xq2,fq2)
  END FUNCTION fd1

  FUNCTION fd0(xq1,fq1,pq1,xq2,fq2)
    REAL(rkind) :: fd0
    REAL(rkind), INTENT(in) :: xq1,fq1,pq1,xq2,fq2
    fd0 = fq1 - xq1*(xq1*fd2(xq1,fq1,pq1,xq2,fq2) + fd1(xq1,fq1,pq1,xq2,fq2))
  END FUNCTION fd0

  FUNCTION fqdq0(xq1,fq1,pq1,xq2,fq2,pdqx)
    ! -- FQDQ0 GIVES THE VALUE OF THE FUNCTION AT POINT PDQX                --
    ! -- FQDQ0(......,PDQX) = F(PDQX)                                         --
    REAL(rkind) :: fqdq0
    REAL(rkind), INTENT(in) :: xq1,fq1,pq1,xq2,fq2,pdqx
    fqdq0 = fd0(xq1,fq1,pq1,xq2,fq2) + pdqx * (fd1(xq1,fq1,pq1,xq2,fq2) +&
         & pdqx * fd2(xq1,fq1,pq1,xq2,fq2))
  END FUNCTION fqdq0

  FUNCTION fqdq1(xq1,fq1,pq1,xq2,fq2,pdqx)
    ! -- FQDQ1 GIVES THE VALUE OF THE FIRST DERIVATIVE OF F(X) AT PDQX      --
    ! -- FQDQ1(......,PDQX) = DF/DX (PDQX)                                    --
    REAL(rkind) :: fqdq1
    REAL(rkind), INTENT(in) :: xq1,fq1,pq1,xq2,fq2,pdqx
    fqdq1 = fd1(xq1,fq1,pq1,xq2,fq2) +&
         & 2._rkind* pdqx * fd2(xq1,fq1,pq1,xq2,fq2)
  END FUNCTION fqdq1

  FUNCTION fqdq2(xq1,fq1,pq1,xq2,fq2)
    ! -- FQDQ2 GIVES THE VALUE OF THE SECOND DERIVATIVE OF F(X) AT PDQX     --
    ! -- FQDQ2(......,PDQX) = D2F/DXQ2 (PDQX)                                  --
    REAL(rkind) :: fqdq2
    REAL(rkind), INTENT(in) :: xq1,fq1,pq1,xq2,fq2
    fqdq2 = 2._rkind * fd2(xq1,fq1,pq1,xq2,fq2)
  END FUNCTION fqdq2
  ! ----------------------------------------------------------------------
  ! --     STATEMENT FUNCTION FOR CUBIC INTERPOLATION                   --
  ! --                         23.04.88            AR        CRPP       --
  ! --                                                                  --
  ! -- CUBIC INTERPOLATION OF A FUNCTION F(X)                           --
  ! -- THE EIGHT ARGUMENTS A1,A2,A3,A4,B1,B2,B3,B4 ARE DEFINED BY:      --
  ! -- F(B1) = A1 , F(B2) = A2 , F(B3) = A3 , F(B4) = A4                --
  ! ----------------------------------------------------------------------

  FUNCTION fa3(a1,a2,a3,a4,b1,b2,b3,b4)
    REAL(rkind) :: fa3
    REAL(rkind), INTENT(in) :: a1,a2,a3,a4,b1,b2,b3,b4
    fa3 = &
         &        (a1-a2) / ((b1-b2)*(b2-b4)*(b2-b3)) + &
         &        (a1-a3) / ((b4-b3)*(b3-b1)*(b3-b2)) + &
         &        (a1-a4) / ((b1-b4)*(b2-b4)*(b3-b4))
  END FUNCTION fa3

  FUNCTION fa2(a1,a2,a3,a4,b1,b2,b3,b4)
    REAL(rkind) :: fa2
    REAL(rkind), INTENT(in) :: a1,a2,a3,a4,b1,b2,b3,b4
    fa2 = &
         &        (a1-a2) / ((b2-b1)*(b3-b2)) + &
         &        (a3-a1) / ((b3-b1)*(b3-b2)) - &
         &        (b1+b2+b3) * fa3(a1,a2,a3,a4,b1,b2,b3,b4)
  END FUNCTION fa2

  FUNCTION fa1(a1,a2,a3,a4,b1,b2,b3,b4)
    REAL(rkind) :: fa1
    REAL(rkind), INTENT(in) :: a1,a2,a3,a4,b1,b2,b3,b4
    fa1 = &
         &        (a1-a2) / (b1-b2) - &
         &        (b1+b2) * fa2(a1,a2,a3,a4,b1,b2,b3,b4) - &
         &        (b1*b1+b1*b2+b2*b2) * fa3(a1,a2,a3,a4,b1,b2,b3,b4)
  END FUNCTION fa1

  FUNCTION fa0(a1,a2,a3,a4,b1,b2,b3,b4)
    REAL(rkind) :: fa0
    REAL(rkind), INTENT(in) :: a1,a2,a3,a4,b1,b2,b3,b4
    fa0 = &
         &        a1 - &
         &        b1 * (fa1(a1,a2,a3,a4,b1,b2,b3,b4) + &
         &              b1 * (fa2(a1,a2,a3,a4,b1,b2,b3,b4) + &
         &                    b1 * fa3(a1,a2,a3,a4,b1,b2,b3,b4)))
  END FUNCTION fa0
  FUNCTION fcccc0(a1,a2,a3,a4,b1,b2,b3,b4,px)
    ! ----------------------------------------------------------------------
    ! -- FCCCC0 GIVES THE VALUE OF THE FUNCTION AT POINT PX:              --
    ! -- FCCCC0(......,PX) = F(PX)                                        --
    ! ----------------------------------------------------------------------
    REAL(rkind) :: fcccc0
    REAL(rkind), INTENT(in) :: a1,a2,a3,a4,b1,b2,b3,b4,px
    fcccc0 = &
         &              fa0(a1,a2,a3,a4,b1,b2,b3,b4) + &
         &              px * (fa1(a1,a2,a3,a4,b1,b2,b3,b4) + &
         &                    px * (fa2(a1,a2,a3,a4,b1,b2,b3,b4) + &
         &                          px * fa3(a1,a2,a3,a4,b1,b2,b3,b4)))
  END FUNCTION fcccc0

  FUNCTION fcccc1(a1,a2,a3,a4,b1,b2,b3,b4,px)
    ! ----------------------------------------------------------------------
    ! -- FCCCC1 GIVES THE VALUE OF THE FIRST DERIVATIVE OF F(X) AT PX:    --
    ! -- FCCCC1(......,PX) = DF/DX (PX)                                   --
    ! ----------------------------------------------------------------------
    REAL(rkind) :: fcccc1
    REAL(rkind), INTENT(in) :: a1,a2,a3,a4,b1,b2,b3,b4,px
    fcccc1 = &
         &              fa1(a1,a2,a3,a4,b1,b2,b3,b4) + &
         &              px * (2._rkind * fa2(a1,a2,a3,a4,b1,b2,b3,b4) + &
         &                    3._rkind * px * fa3(a1,a2,a3,a4,b1,b2,b3,b4))
  END FUNCTION fcccc1

  FUNCTION fcccc2(a1,a2,a3,a4,b1,b2,b3,b4,px)
    ! ----------------------------------------------------------------------
    ! -- FCCCC2 GIVES THE VALUE OF THE SECOND DERIVATIVE OF F(X) AT PX:   --
    ! -- FCCCC2(......,PX) = D2F/DX2 (PX)                                 --
    ! ----------------------------------------------------------------------
    REAL(rkind) :: fcccc2
    REAL(rkind), INTENT(in) :: a1,a2,a3,a4,b1,b2,b3,b4,px
    fcccc2 = &
         &             2._rkind * fa2(a1,a2,a3,a4,b1,b2,b3,b4) + &
         &             6._rkind * fa3(a1,a2,a3,a4,b1,b2,b3,b4) * px
  END FUNCTION fcccc2

  FUNCTION fcccc3(a1,a2,a3,a4,b1,b2,b3,b4,px)
    ! ----------------------------------------------------------------------
    ! -- FCCCC3 GIVES THE VALUE OF THE THIRD DERIVATIVE OF F(X) AT PX:     -
    ! -- FCCCC3(......,PX) = D3F/DX3 (PX)                                  -
    ! ----------------------------------------------------------------------
    REAL(rkind) :: fcccc3
    REAL(rkind), INTENT(in) :: a1,a2,a3,a4,b1,b2,b3,b4,px
    fcccc3 =  6._rkind * fa3(a1,a2,a3,a4,b1,b2,b3,b4)
  END FUNCTION fcccc3
  ! ----------------------------------------------------------------------
  ! --     STATEMENT FUNCTION FOR CUBIC INTERPOLATION                   --
  ! --                         19.01.87            AR        CRPP       --
  ! --                                                                  --
  ! -- CUBIC INTERPOLATION OF A FUNCTION F(X)                           --
  ! -- THE SIX ARGUMENTS X1,F1,P1,X2,F2,P2 ARE DEFINED AS FOLLOWS:      --
  ! -- F(X1) = F1 , F(X2) = F2 , DF/DX(X1) = P1 , DF/DX(X2) = P2        --
  ! ----------------------------------------------------------------------
  FUNCTION fc3(x1,f1,p1,x2,f2,p2)
    REAL(rkind) :: fc3
    REAL(rkind), INTENT(in) :: x1,f1,p1,x2,f2,p2
    fc3 = &
         &      (2._rkind * (f2 - f1) / (x1 - x2) + (p1 + p2)) / &
         &      ((x1 - x2) * (x1 - x2))
  END FUNCTION fc3

  FUNCTION fc2(x1,f1,p1,x2,f2,p2)
    REAL(rkind) :: fc2
    REAL(rkind), INTENT(in) :: x1,f1,p1,x2,f2,p2
    fc2 = &
         &      (3._rkind * (x1 + x2) * (f1 - f2) / (x1 - x2) - &
         &       p1 * (x1 + 2._rkind * x2) - p2 * (x2 + 2._rkind * x1)) / &
         &      ((x1 - x2) * (x1 - x2))
  END FUNCTION fc2

  FUNCTION fc1(x1,f1,p1,x2,f2,p2)
    REAL(rkind) :: fc1
    REAL(rkind), INTENT(in) :: x1,f1,p1,x2,f2,p2
    FC1 = &
         &      (P1 + P2 - 2._RKIND*FC2(X1,F1,P1,X2,F2,P2)*(X1+X2) - &
         & 3._RKIND*FC3(X1,F1,P1,X2,F2,P2)*(X1*X1+X2*X2))/2._RKIND
  END FUNCTION fc1

  FUNCTION fc0(x1,f1,p1,x2,f2,p2)
    REAL(rkind) :: fc0
    REAL(rkind), INTENT(in) :: x1,f1,p1,x2,f2,p2
    FC0 = &
         & 0.5_RKIND * (F1 + F2 - FC1(X1,F1,P1,X2,F2,P2)*(X1+X2) - &
         & FC2(X1,F1,P1,X2,F2,P2)*(X1*X1+X2*X2) - &
         & FC3(X1,F1,P1,X2,F2,P2)*(X1**3+X2**3))
  END FUNCTION fc0
  FUNCTION fcdcd0(x1,f1,p1,x2,f2,p2,pcx)
    ! ----------------------------------------------------------------------
    ! -- FCDCD0 GIVES THE VALUE OF THE FUNCTION AT POINT PCX               --
    ! -- FCDCD0(......,PCX) = F(PCX)                                        --
    ! ----------------------------------------------------------------------
    REAL(rkind) :: fcdcd0
    REAL(rkind), INTENT(in) :: x1,f1,p1,x2,f2,p2,pcx
    fcdcd0 = &
         &              fc0(x1,f1,p1,x2,f2,p2) + &
         &              pcx * (fc1(x1,f1,p1,x2,f2,p2) + &
         &                    pcx * (fc2(x1,f1,p1,x2,f2,p2) + &
         &                          pcx * fc3(x1,f1,p1,x2,f2,p2)))
  END FUNCTION fcdcd0

  ! ----------------------------------------------------------------------
  ! -- FCDCD1 GIVES THE VALUE OF THE FIRST DERIVATIVE OF F(X) AT PCX:    --
  ! -- FCDCD1(......,PCX) = DF/DX (PCX)                                   --
  ! ----------------------------------------------------------------------
  FUNCTION fcdcd1(x1,f1,p1,x2,f2,p2,pcx)
    REAL(rkind) :: fcdcd1
    REAL(rkind), INTENT(in) :: x1,f1,p1,x2,f2,p2,pcx
    fcdcd1 = &
         &              fc1(x1,f1,p1,x2,f2,p2) + &
         &              pcx * (2._rkind * fc2(x1,f1,p1,x2,f2,p2) + &
         &                    3._rkind * pcx * fc3(x1,f1,p1,x2,f2,p2))
  END FUNCTION fcdcd1

  ! ----------------------------------------------------------------------
  ! -- FCDCD2 GIVES THE VALUE OF THE SECOND DERIVATIVE OF F(X) AT PCX:   --
  ! -- FCDCD2(......,PCX) = D2F/DX2 (PCX)                                 --
  ! ----------------------------------------------------------------------
  FUNCTION fcdcd2(x1,f1,p1,x2,f2,p2,pcx)
    REAL(rkind) :: fcdcd2
    REAL(rkind), INTENT(in) :: x1,f1,p1,x2,f2,p2,pcx
    fcdcd2 = &
         &             2._rkind * fc2(x1,f1,p1,x2,f2,p2) + &
         &             6._rkind * fc3(x1,f1,p1,x2,f2,p2) * pcx
  END FUNCTION fcdcd2

  ! ----------------------------------------------------------------------
  ! -- FCDCD3 GIVES THE VALUE OF THE THIRD DERIVATIVE OF F(X) AT PCX:    --
  ! -- FCDCD3(......,PCX) = D3F/DX3 (PCX)                                 --
  ! ----------------------------------------------------------------------
  FUNCTION fcdcd3(x1,f1,p1,x2,f2,p2,pcx)
    REAL(rkind) :: fcdcd3
    REAL(rkind), INTENT(in) :: x1,f1,p1,x2,f2,p2,pcx
    fcdcd3 =  6._rkind * fc3(x1,f1,p1,x2,f2,p2)
  END FUNCTION fcdcd3

END MODULE interpol
