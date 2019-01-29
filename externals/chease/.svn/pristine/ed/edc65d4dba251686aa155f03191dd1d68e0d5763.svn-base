SUBROUTINE COCOS(KCOCOS,Kexp_Bp,Ksigma_Bp,Ksigma_RphiZ,Ksigma_rhothetaphi,Ksign_q_pos,Ksign_pprime_pos)
  !
  ! return values of exp_Bp, sigma_Bp, sigma_rhothetaphi, sign_q_pos, sign_pprime_pos
  ! from the input value KCOCOS and according to O. Sauter and S. Yu. Medvevdev paper and Table I
  ! (see paper in chease directory)
  !
  IMPLICIT NONE
  INTEGER, intent(in) :: KCOCOS
  INTEGER, intent(out) :: Kexp_Bp, Ksigma_Bp, Ksigma_RphiZ, Ksigma_rhothetaphi, Ksign_q_pos, Ksign_pprime_pos
  !
  ! cocos=i or 10+i have similar coordinate conventions except psi/2pi for cocos=i and psi for cocos=10+i
  !
  ! 2 pi factor:
  Kexp_Bp = 0
  if (KCOCOS .ge. 11) Kexp_Bp = 1
  !
  ! Other parameters from Table I
  !
  SELECT CASE (KCOCOS)
  CASE (1, 11)
    ! ITER, Boozer are cocos=11
    Ksigma_Bp = +1
    Ksigma_RphiZ = +1
    Ksigma_rhothetaphi = +1
    Ksign_q_pos = +1
    Ksign_pprime_pos = -1
    !
  CASE (2, 12)
    ! CHEASE, ONETWO, Hinton-Hazeltine, LION is cocos=2
    Ksigma_Bp = +1
    Ksigma_RphiZ = -1
    Ksigma_rhothetaphi = +1
    Ksign_q_pos = +1
    Ksign_pprime_pos = -1
    !
  CASE (3, 13)
    ! Freidberg, CAXE, KINX are cocos=3
    ! EU-ITM up to end of 2011 is COCOS=13
    Ksigma_Bp = -1
    Ksigma_RphiZ = +1
    Ksigma_rhothetaphi = -1
    Ksign_q_pos = -1
    Ksign_pprime_pos = +1
    !
  CASE (4, 14)
    ! 
    Ksigma_Bp = -1
    Ksigma_RphiZ = -1
    Ksigma_rhothetaphi = -1
    Ksign_q_pos = -1
    Ksign_pprime_pos = +1
    !
  CASE (5, 15)
    ! 
    Ksigma_Bp = +1
    Ksigma_RphiZ = +1
    Ksigma_rhothetaphi = -1
    Ksign_q_pos = -1
    Ksign_pprime_pos = -1
    !
  CASE (6, 16)
    ! 
    Ksigma_Bp = +1
    Ksigma_RphiZ = -1
    Ksigma_rhothetaphi = -1
    Ksign_q_pos = -1
    Ksign_pprime_pos = -1
    !
  CASE (7, 17)
    ! TCV psitbx is cocos=7
    Ksigma_Bp = -1
    Ksigma_RphiZ = +1
    Ksigma_rhothetaphi = +1
    Ksign_q_pos = +1
    Ksign_pprime_pos = +1
    !
  CASE (8, 18)
    ! 
    Ksigma_Bp = -1
    Ksigma_RphiZ = -1
    Ksigma_rhothetaphi = +1
    Ksign_q_pos = +1
    Ksign_pprime_pos = +1
    !
  CASE DEFAULT
    ! SAhould not be here since all cases defined
    PRINT *,' ERROR IN COCOS: COCOS = ',KCOCOS,' DOES NOT EXIST'
    RETURN
  END SELECT
  !
END SUBROUTINE COCOS
