!*COMDECK HERMIT
! ----------------------------------------------------------------------
! --     STATEMENT FUNCTION FOR CUBIC HERMIT BASIS FUNCTIONS ,        --
! --     FIRST AND SECOND DERIVATIVES OF CUBIC HERMIT BASIS FUBCTIONS --
! --                                                                  --
! --                         11.07.88            HL        CRPP       --
! --                                                                  --
! ----------------------------------------------------------------------
! --                                                                  --
! -- 1.  ONE DIMENSION HERMIT CUBIC BASIS FUNCTIONS                   --
! --                                                                  --
! ----------------------------------------------------------------------
!
 REAL(rkind) FN1, FN3, FN2, FN4
 REAL(rkind) DFN1, DFN3, DFN2, DFN4
 REAL(rkind) D2FN1, D2FN3, D2FN2, D2FN4 
 REAL(rkind) X,XJ,XJ1
!
         FN1(X,XJ,XJ1) = (X - XJ1) * (X - XJ1) * (XJ1 - 3*XJ + 2*X) / &
     &                   ((XJ1 - XJ) * (XJ1 - XJ) * (XJ1 - XJ))
         FN3(X,XJ,XJ1) = (X - XJ) * (X - XJ) * (3*XJ1 - XJ - 2*X) / &
     &                   ((XJ1 - XJ) * (XJ1 - XJ) * (XJ1 - XJ))
         FN2(X,XJ,XJ1) = (X - XJ1) * (X - XJ1) * (X - XJ) / &
     &                   ((XJ1 - XJ) * (XJ1 - XJ))
         FN4(X,XJ,XJ1) = (X - XJ) * (X - XJ) * (X - XJ1) / &
     &                   ((XJ1 - XJ) * (XJ1 - XJ))
!
! ----------------------------------------------------------------------
! --                                                                  --
! -- 2. FIRST DERIVATIVE OF ONE DIMENSION CUBIC HERMIT BASIS FUNCTIONS--
! --                                                                  --
! ----------------------------------------------------------------------
!
         DFN1(X,XJ,XJ1) = 6 * (X - XJ1) * (X - XJ) / &
     &                    ((XJ1 - XJ) * (XJ1 - XJ) * (XJ1 - XJ))
         DFN3(X,XJ,XJ1) = 6 * (XJ1 - X) * (X - XJ) / &
     &                    ((XJ1 - XJ) * (XJ1 - XJ) * (XJ1 - XJ))
         DFN2(X,XJ,XJ1) = (X - XJ1) * (3 * X - 2 * XJ - XJ1) / &
     &                    ((XJ1 - XJ) * (XJ1 - XJ))
         DFN4(X,XJ,XJ1) = (X - XJ) * (3 * X - XJ - 2 * XJ1) / &
     &                    ((XJ1 - XJ) * (XJ1 - XJ))
!
! ----------------------------------------------------------------------
! --                                                                  --
! -- 3. SECOND DERIVATIVE OF ONE DIMENSION CUBIC HERMIT BASIS         --
! --    FUNCTIONS                                                     --
! --                                                                  --
! ----------------------------------------------------------------------
!
         D2FN1(X,XJ,XJ1) = 6 * (2 * X - XJ - XJ1) / &
     &                     ((XJ1 - XJ) * (XJ1 - XJ) * (XJ1 - XJ))
         D2FN3(X,XJ,XJ1) = 6 * (XJ + XJ1 - 2 * X) / &
     &                     ((XJ1 - XJ) * (XJ1 - XJ) * (XJ1 - XJ))
         D2FN2(X,XJ,XJ1) = 2 * (3 * X - XJ - 2 * XJ1) / &
     &                     ((XJ1 - XJ) * (XJ1 - XJ))
         D2FN4(X,XJ,XJ1) = 2 * (3 * X - 2 * XJ - XJ1) / &
     &                     ((XJ1 - XJ) * (XJ1 - XJ))
!
