SUBROUTINE g_0
  !
  !   Computed array dimensions from input
  !
  USE globals
  IMPLICIT NONE
  !---------------------------------------------------------------
  !
  if( nideal == 3 ) niso = npsi
  nps = ns
  npt = nt
  npchi = nchi
  nppsi = npsi
  if( nppsi < nppr ) nppsi = nppr
  if( nppsi < niso ) nppsi = niso
  if( ntnova > npchi ) npchi = ntnova
  !
  if( nideal .EQ. 1 .or. nideal .EQ. 2 .or. nideal .EQ. 6 .or. nideal .EQ. 8) mflgerl = 1
  if( nideal .EQ. 0 )                                       mflgmar = 1
  if( nideal .EQ. 3 )                                       mflgnvw = 1
  if( nideal .EQ. 4 .or. nideal .eq. 5)  mflgpen = 1
  npblc0 = nblc0
  npturn = nturn
  mpsmax = 20*mflgmar+1*(1-mflgmar)
  npsmax = 1*mflgmar+1*(1-mflgmar)
  npv = nv
  npmgs = nmgaus
  npsgs = nsgaus
  nptgs = ntgaus
  npdiag1 = 40*mflgdiag1+1*(1-mflgdiag1)
  ndeq = 25*mflgerl+1*(1-mflgerl)
  if (nideal .eq. 2) ndeq = 29*mflgerl+1*(1-mflgerl)
  npband = 4*npt+12
  npchi1 = npchi+1
  npgaus = npsgs*nptgs
  nppsi1 = nppsi+1
  nppsnvw = nppsi1*mflgnvw+1*(1-mflgnvw)
  ! npiso = nppsi1*(((npmgs+1)/2+1)*mflgpen+1*(1-mflgpen))
  ! npisoeff should depend on premap for the final mapping on value of NISO1EFF+1
  ! NIDEAL= 1, 2, 6, 7, 8, 9 and 10:
  npisoeff = NPPSI + 2
  IF (NVERBOSE .GE. 3) print *,'npisoeff= ',npisoeff
  if (nideal.eq. 0 .OR. NIDEAL .EQ. 3 .OR. NIDEAL .EQ. 5) npisoeff = 2 * NPPSI + 1
  if (nideal.eq. 4) npisoeff = NPPSI * (npmgs+2) + 1
  npv1 = npv+1
  nsnt = (nps+1)*npt
  nsp1 = nps+1
  ntp1 = npt+1
  ntp2 = npt+2
  np4nst = 4*nsnt
  n4nt = 4*npt
  n8nt = 8*npt
  n16nt = 16*npt
  !
END SUBROUTINE g_0
