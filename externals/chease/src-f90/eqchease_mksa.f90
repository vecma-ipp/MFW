SUBROUTINE EQCHEASE_MKSA(Kexp_Bp,Ksigma_Bp,Ksigma_RphiZ,Ksigma_rhothetaphi,Ksign_q_pos,Ksign_pprime_pos)
  !
  ! transforms all eqchease... inputs into mksa units using
  !
  ! R0EXP
  ! B0EXP
  ! B0XPSGN
  ! IPXPSGN
  !
  ! In this way, eqchease arrays can be used within chease and actually have replaced former chease arrays
  !
  ! Should be called near end of chease run
  !
  USE globals
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: Kexp_Bp, Ksigma_Bp, Ksigma_RphiZ, Ksigma_rhothetaphi, Ksign_q_pos, Ksign_pprime_pos
  REAL(RKIND) :: ZMU0
  !
  ZMU0 = 4.E-07_RKIND * CPI
  !
  ! 1. eqchease_out_add_1d
  !
  eqchease_out_add_1d(:,iirgeo) = eqchease_out_add_1d(:,iirgeo) * R0EXP
  eqchease_out_add_1d(:,iiamin) = eqchease_out_add_1d(:,iiamin) * R0EXP
  eqchease_out_add_1d(:,iidqdpsi) =  SIGNB0XP * Ksigma_rhothetaphi * Ksigma_Bp / twopi**Kexp_Bp * &
       & eqchease_out_add_1d(:,iidqdpsi) / R0EXP**2 / B0EXP
  eqchease_out_add_1d(:,iid2qdpsi2) = SIGNIPXP * SIGNB0XP * Ksigma_rhothetaphi / twopi**(2*Kexp_Bp) * &
       & eqchease_out_add_1d(:,iid2qdpsi2)/(B0EXP*R0EXP**2)**2
  eqchease_out_add_1d(:,iidsheardpsi) = SIGNIPXP * Ksigma_Bp / twopi**Kexp_Bp * &
       & eqchease_out_add_1d(:,iidsheardpsi) / R0EXP**2 / B0EXP
!!$  eqchease_out_add_1d(:,iidVdpsi) = eqchease_out_add_1d(:,iidVdpsi) * R0EXP / B0EXP / TWOPI * (-signipxp)
!!$  eqchease_out_add_1d(:,iidpsidrhotor) = eqchease_out_add_1d(:,iidpsidrhotor) * B0EXP * R0EXP * TWOPI * (-signipxp)
   ! <| grd PSI |>
  eqchease_out_add_1d(:,iigradpsi_av) = twopi**Kexp_Bp * eqchease_out_add_1d(:,iigradpsi_av) * R0EXP * B0EXP
  ! < r >
  eqchease_out_add_1d(:,iia_av) = eqchease_out_add_1d(:,iia_av) * R0EXP
  ! < R >
  eqchease_out_add_1d(:,iiR_av) = eqchease_out_add_1d(:,iiR_av) * R0EXP
  eqchease_out_add_1d(:,iiBmax) = SIGNB0XP * eqchease_out_add_1d(:,iiBmax) * B0EXP
  eqchease_out_add_1d(:,iiBmin) = SIGNB0XP * eqchease_out_add_1d(:,iiBmin) * B0EXP
  eqchease_out_add_1d(:,iiIplas) = SIGNIPXP * eqchease_out_add_1d(:,iiIplas) * R0EXP * B0EXP / ZMU0
  ! Te's, ne's already in eV or m^-3 since from EXPTNZ, but d/dpsi not normalized yet
  eqchease_out_add_1d(:,iidTedpsi) = SIGNIPXP * Ksigma_Bp / twopi**Kexp_Bp * eqchease_out_add_1d(:,iidTedpsi) / B0EXP / R0EXP**2
  eqchease_out_add_1d(:,iidnedpsi) = SIGNIPXP * Ksigma_Bp / twopi**Kexp_Bp * eqchease_out_add_1d(:,iidnedpsi) / B0EXP / R0EXP**2
  eqchease_out_add_1d(:,iidTidpsi) = SIGNIPXP * Ksigma_Bp / twopi**Kexp_Bp * eqchease_out_add_1d(:,iidTidpsi) / B0EXP / R0EXP**2
  eqchease_out_add_1d(:,iidnidpsi) = SIGNIPXP * Ksigma_Bp / twopi**Kexp_Bp * eqchease_out_add_1d(:,iidnidpsi) / B0EXP / R0EXP**2
  ! signeo has already SI units?
  ! [j] is B0/R0/mu0, [B]=B0 and sign(jbs) is sign(Ip)
  eqchease_out_add_1d(:,iijbsBav) = SIGNIPXP * eqchease_out_add_1d(:,iijbsBav) * B0EXP**2 / ZMU0 / R0EXP
  !
  ! 2. eqchease_out_add_2d
  !
  eqchease_out_add_2d(:,:,iiB) = SIGNB0XP * eqchease_out_add_2d(:,:,iiB) * B0EXP   ! B
  eqchease_out_add_2d(:,:,iidBdpsi) = SIGNB0XP * SIGNIPXP * Ksigma_Bp / twopi**Kexp_Bp * &
       & eqchease_out_add_2d(:,:,iidBdpsi) / R0EXP**2 ! dBdpsi
  eqchease_out_add_2d(:,:,iidBdchi) = SIGNB0XP * eqchease_out_add_2d(:,:,iidBdchi) * B0EXP   ! dBdchi
  eqchease_out_add_2d(:,:,iidpsidR) = SIGNIPXP * Ksigma_Bp * twopi**Kexp_Bp * eqchease_out_add_2d(:,:,iidpsidR) * R0EXP*B0EXP ! dPsidr
  eqchease_out_add_2d(:,:,iidpsidZ) = SIGNIPXP * Ksigma_Bp * twopi**Kexp_Bp * eqchease_out_add_2d(:,:,iidpsidZ) * R0EXP*B0EXP ! dPsidz
  eqchease_out_add_2d(:,:,iidchidR) = eqchease_out_add_2d(:,:,iidchidR) /R0EXP                                  ! dChidr
  eqchease_out_add_2d(:,:,iidchidZ) = eqchease_out_add_2d(:,:,iidchidZ) /R0EXP                                  ! dChidz

  eqchease_out_add_2d(:,:,iidRdpsi) = SIGNIPXP * Ksigma_Bp / twopi**Kexp_Bp * eqchease_out_add_2d(:,:,iidRdpsi) / R0EXP / B0EXP
  eqchease_out_add_2d(:,:,iidRdchi) = eqchease_out_add_2d(:,:,iidRdchi) * R0EXP
  eqchease_out_add_2d(:,:,iidZdpsi) = SIGNIPXP * Ksigma_Bp / twopi**Kexp_Bp * eqchease_out_add_2d(:,:,iidZdpsi) / R0EXP / B0EXP
  eqchease_out_add_2d(:,:,iidZdchi) = eqchease_out_add_2d(:,:,iidZdchi) * R0EXP
 
  eqchease_out_add_2d(:,:,iiAh) = eqchease_out_add_2d(:,:,iiAh) / R0EXP**2
  eqchease_out_add_2d(:,:,iidAhdpsi) = SIGNIPXP * Ksigma_Bp / twopi**Kexp_Bp * eqchease_out_add_2d(:,:,iidAhdpsi) / R0EXP**4 / B0EXP

  return
end SUBROUTINE EQCHEASE_MKSA
