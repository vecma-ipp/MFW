!*COMDECK BNDIND
! ----------------------------------------------------------------------
! --     STATEMENT FUNCTION FOR BAND MATRIX ADRESSES                  --
! --                         11.07.88            HL        CRPP       --
! --                                                                  --
! --     INDCOL AND INDROW ARE COMPUTING BAND MATRIX ADRESSES STARTING--
! --     FROM SQUARE MATRIX ADRESSES. IF THE ADRESS IS LOWER-DIAGONAL,--
! --     THESE FUNCTIONS FIND OUT AUTOMATICALY THE SYMETRIC UPPER-    --
! --     DIAGONAL ADRESS.                                             --
! --                                                                  --
! ----------------------------------------------------------------------
!
         INTEGER   INDCOL, INDROW, ICOLB, IROWB
!
         INDCOL(ICOLB,IROWB) = SIGN(1,ICOLB - IROWB) * (ICOLB - IROWB) + 1
         INDROW(ICOLB,IROWB) = ((IROWB + ICOLB) + (IROWB - ICOLB) * &
     &                        SIGN(1,ICOLB - IROWB)) / 2
!
