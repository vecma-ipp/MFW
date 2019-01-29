SUBROUTINE g_2
  !
  !   Allocate dynamic arrays in globals
  !
  USE globals
  IMPLICIT NONE
  INTEGER :: k1, k2, k3, k4, k5, k6, k7
  INTEGER :: nw, nwtot = 0
  !----------------------------------------------------------------------
  !
!!$  ! on smisop1
!!$  k1 = 2*npiso+1
!!$  ALLOCATE( 
  ! mesh on psiiso and called in surface for example. Set to max nb of points in premap options
  ! Note: many should move to contain psiisop1 points including on-axis and be shifted by one!!
  ! start with aratio, defined below
  k1 = npisoeff
  ALLOCATE( cdq(k1), cid0(k1), cid0o(k1), cid2(k1), &
       &cid2o(k1), cidq(k1), cidr(k1), cidrtor(k1), cidrtoro(k1), cipr(k1), cp(k1), cpdp(k1), &
       &cppr(k1), cppro(k1), cpr(k1), csipr(k1), csipri(k1), csipro(k1), d2cid0(k1), d2cid0o(k1), &
       &d2cid2(k1), d2cid2o(k1), d2cidq(k1), d2cidr(k1), d2cidrtor(k1), d2cidrtoro(k1),d2cipr(k1), &
       &d2cppr(k1), d2cppro(k1), d2cpr(k1), d2tmf(k1), d2tmfo(k1), &
       &d2ttp(k1), densty(k1), psiiso(k1), qpsi(k1), rare(k1), &
       &rb2av(k1), rbpol0(k1), rell(k1), rfcirc(k1), fm(6,k1), &
       &rfpbp(k1), rib(k1), rib2(k1), ribi2(k1), rir2(k1), riie(k1), riir(k1), rip(k1), &
       &rip2(k1), ripr(k1), rivol(k1), rj1(k1), rj2(k1), rj3(k1), &
       &rj4(k1), rj5(k1), rj5p(k1), rj6(k1), rj7s(k1,10), &
       &rjdotb(k1), rjpar(k1), rleng(k1), rleng1(k1), rnustar(k1), &
       &smiso(k1), smisop1(k1+1), temper(k1), tmf(k1), tmfo(k1), ttp(k1), ttpo(k1) )
  nw = (74+6)*(k1)+1
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  k1 = npisoeff+1
  ALLOCATE( aratio(k1))
  nw = (1)*(k1)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npisoeff
  k2 = 4
  ALLOCATE( rjbsos(k1,k2) )
  nw = k1*k2
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated rjbsos(k1,k2) ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npisoeff
  k2 = npdiag1
  IF (NVERBOSE .GE. 3) write(0,*) ' k1= ',k1,' k2= ',k2
  ALLOCATE( diagars(k1,k2), rbdiag(k1,k2) )
  nw = 2*(k1*k2)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated diagars(k1,k2), rbdiag(k1,k2)', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = 4*2*nppsi1
  ALLOCATE( rrcurv(k1), rzcurv(k1) )
  nw = 2*(k1)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = mpsmax
  k2 = npsmax
  ALLOCATE( rm(k1,k2) )
  nw = k1*k2
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = ndeq
  k2 = npchi
  k3 = npisoeff
  ALLOCATE( eq(k1,k2,k3) )
  nw = k1*k2*k3
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = np4nst
  ALLOCATE( b(k1), cpsi(k1), cpsicl(k1), cpsili(k1), cpsio(k1) )
  nw = 5*(k1)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npband
  k2 = np4nst
  ALLOCATE( a(k1,k2) )
  nw = k1*k2
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npblc0
  ALLOCATE( chi0(k1) )
  nw = k1
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npbps
  ALLOCATE( bpscos(k1), bpssin(k1), bsd2ne(k1), bsd2te(k1), bsd2ti(k1), &
       &bsd2zef(k1), bsdense(k1), bstempe(k1), bstempi(k1), bszeff(k1), &
       &d2rbps(k1), d2rfun(k1), d2rppf(k1), d2zbps(k1), fcsm(k1), &
       &fcsmtnz(k1), qpsiin(k1), rfun(k1), rppf(k1), rrbps(k1), rrbpsou(k1), &
       &rzbps(k1), rzbpsou(k1), tetbps(k1), wallposr(k1), wallposz(k1) )
  nw = 26*(k1)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npbps
  k2 = npbps
  ALLOCATE( eqdspsi(k1,k2) )
  nw = k1*k2
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npchi
  ALLOCATE( chim(k1) )
  nw = k1
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npchi*max(npmgs+1,mdt)
  ALLOCATE( ctpen(k1), sigpen(k1), tetpen(k1) )
  nw = 3*(k1)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npchi
  k2 = 3
  ALLOCATE( drhopi(k1,k2), rhovaci(k1,k2), tetvaci(k1,k2) )
  nw = 3*(k1*k2)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npchi
  k2 = 2*nppsi1
  ALLOCATE( cnr1(k1,k2), cnr2(k1,k2), cnz1(k1,k2), cnz2(k1,k2), cr(k1,k2), &
       &cz(k1,k2), eq13(k1,k2), eq22(k1,k2), eq24(k1,k2), rshear(k1,k2), &
       &sigchi(k1,k2), tetchi(k1,k2),  tetchi_sorted(k1,k2))
  nw = 12*(k1*k2)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npchi1
  ALLOCATE( chi(k1), chiold(k1), rhovac(k1), rhovacm(k1), tetvac(k1), &
       &tetvacm(k1) )
  nw = 6*(k1)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npchi1
  k2 = mdt*npchi1
  ALLOCATE( ctxt(k1), ctxt_refined(k2))
  nw = k1+k2
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npdiag1
  ALLOCATE( diagartx(k1), nflgaddia(k1), rbdiagtx(k1) )
  nw = 3*(k1)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npdianeo
  ALLOCATE( globneo(k1), titglneo(k1), titsneo(k1) )
  nw = 3*(k1)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npdianeo
  k2 = 10
  ALLOCATE( valsneo(k1,k2) )
  nw = k1*k2
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npgaus
  ALLOCATE( cw(k1) )
  nw = k1
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npgaus
  k2 = 2
  ALLOCATE( dzeta(k1,k2) )
  nw = k1*k2
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npisoeff
  ALLOCATE(areacsm(k1), betapcsm(k1), betab(k1), cdrq(k1), &
       &d2rprm(k1), dprime(k1), hmercr(k1), n2bal(k1), ncbal(k1), &
       &np0(k1), np1(k1), np2(k1), np3(k1), np4(k1), pcs(k1), pcsm(k1), &
       &rdedr(k1), rdi(k1), rprm(k1), rsy(k1), &
       &smerci(k1), smercr(k1), x2srch(k1), xlamb(k1), &
       &xp0(k1), xp1(k1), xp2(k1), xp3(k1), xp4(k1), xpprdf(k1), &
       &xpprmn(k1), xpprmx(k1) )
  nw = 32*(k1)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npisoeff
  k2 = mpsmax
  ALLOCATE( b2f(k1,k2), b2fc(k1,k2), b2fcm(k1,k2), b2fm(k1,k2), b3f(k1,k2), &
       &b3fc(k1,k2), b3fcm(k1,k2), b3fm(k1,k2), dg11l(k1,k2), &
       &dg11lm(k1,k2), dg12l(k1,k2), dg12lm(k1,k2), dg22l(k1,k2), &
       &dg22lm(k1,k2), dg33l(k1,k2), dg33lm(k1,k2), djcof(k1,k2), &
       &djcofm(k1,k2), drhos(k1,k2), drhosm(k1,k2), drot(k1,k2), &
       &drotm(k1,k2), eqrho(k1,k2), eqrhom(k1,k2), eqrot(k1,k2), &
       &eqrotm(k1,k2), feq(k1,k2), feqm(k1,k2), gbr(k1,k2), gbrm(k1,k2), &
       &gbz(k1,k2), gbzm(k1,k2), gcfc(k1,k2), gcfcm(k1,k2), gcfs(k1,k2), &
       &gcfsm(k1,k2), gchdz(k1,k2), gchdzm(k1,k2), gscc(k1,k2), &
       &gsccm(k1,k2), gscs(k1,k2), gscsm(k1,k2), gsdz(k1,k2), &
       &gsdzm(k1,k2), gsfc(k1,k2), gsfcm(k1,k2), gsfs(k1,k2), &
       &gsfsm(k1,k2), idiy2(k1,k2), idiy2m(k1,k2), idiy3(k1,k2), &
       &idiy3m(k1,k2), idrxx(k1,k2), idrxxm(k1,k2), idryx(k1,k2), &
       &idryxm(k1,k2), idrzx(k1,k2), idrzxm(k1,k2), ig122(k1,k2), &
       &ig122m(k1,k2), ig123(k1,k2), ig123m(k1,k2), igf22(k1,k2), &
       &igf22m(k1,k2), igpx2(k1,k2), igpx2m(k1,k2), igpx3(k1,k2), &
       &igpx3m(k1,k2), igpy2(k1,k2), igpy2m(k1,k2), igpy3(k1,k2), &
       &igpy3m(k1,k2), ij0qx(k1,k2), ij0qxm(k1,k2), ij0qy(k1,k2), &
       &ij0qym(k1,k2), ij0qz(k1,k2), ij0qzm(k1,k2), inxx(k1,k2), &
       &inxxm(k1,k2), inxy(k1,k2), inxym(k1,k2), inyy(k1,k2), &
       &inyym(k1,k2), inzz(k1,k2), inzzm(k1,k2), irxz(k1,k2), &
       &irxzm(k1,k2), iryx(k1,k2), iryxm(k1,k2), irzy(k1,k2), &
       &irzym(k1,k2), ivs11(k1,k2), ivs11m(k1,k2), ivs12(k1,k2), &
       &ivs12m(k1,k2), ivs21(k1,k2), ivs21m(k1,k2), ivs22(k1,k2), &
       &ivs22m(k1,k2), iwsq1(k1,k2), iwsq1m(k1,k2), iwsq2(k1,k2), &
       &iwsq2m(k1,k2), iwsq3(k1,k2), iwsq3m(k1,k2), jacobi(k1,k2), &
       &jacobm(k1,k2), jacof(k1,k2), jacofm(k1,k2), jacos(k1,k2), &
       &jacosm(k1,k2), jg11l(k1,k2), jg11lm(k1,k2), jg12l(k1,k2), &
       &jg12lm(k1,k2), jg22l(k1,k2), jg22lm(k1,k2), jg33l(k1,k2), &
       &jg33lm(k1,k2), visxz(k1,k2), visxzm(k1,k2), visyz(k1,k2), &
       &visyzm(k1,k2) )
  nw = 124*(k1*k2)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npisoeff
  k2 = mpsmax
  k3 = npsmax
  ALLOCATE( b2e(k1,k2,k3), b2u(k1,k2,k3), b3e(k1,k2,k3), b3u(k1,k2,k3), &
       &dpeds(k1,k2,k3), dpedsm(k1,k2,k3), j2e(k1,k2,k3), j2u(k1,k2,k3), &
       &j3e(k1,k2,k3), j3u(k1,k2,k3), peq(k1,k2,k3), pre(k1,k2,k3) )
  nw = 12*(k1*k2*k3)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npisoeff
  k2 = npblc0
  ALLOCATE( ncblns(k1,k2) )
  nw = k1*k2
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npmgs*ntp1
  ALLOCATE( bchiso(k1), chiiso(k1) )
  nw = 2*(k1)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npmgs*ntp1
  k2 = 17
  ALLOCATE( eq3(k1,k2) )
  nw = k1*k2
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npmgs*ntp1
  k2 = 19
  ALLOCATE( eql(k1,k2) )
  nw = k1*k2
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npmgs*ntp1
  k2 = npisoeff
  ALLOCATE( bndiso(k1,k2), dgniso(k1,k2), dgriso(k1,k2), dgziso(k1,k2), &
       &dpriso(k1,k2), dpsiso(k1,k2), dptiso(k1,k2), dpziso(k1,k2), &
       &drniso(k1,k2), gpiso(k1,k2), rhoiso(k1,k2), rriso(k1,k2), &
       &rziso(k1,k2), sigpsi(k1,k2), tetpsi(k1,k2), wgtpsi(k1,k2) )
  nw = 16*(k1*k2)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npmgs*ntp1
  k2 = 32
  ALLOCATE( eqi(k1,k2) )
  nw = k1*k2
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npoutflg
  ALLOCATE( nloutp1(k1), nloutp2(k1), nloutp3(k1), nloutp4(k1) )
  nw = 4*(k1)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = nppsbal
  k2 = 2
  k3 = 2*npturn*npchi+2
  ALLOCATE( abal(k1,k2,k3) )
  nw = k1*k2*k3
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = nppsi1
  k2 = npisoeff
  ALLOCATE( cs(k1), csm(k2), csmtor(k2) )
  nw = 1*k1+2*k2
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = nppsi1*(npmgs+1)
  ALLOCATE( cspen(k1) )
  nw = k1
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = nps
  k2 = npgaus
  ALLOCATE( rsint(k1,k2) )
  nw = k1*k2
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npsmax
  ALLOCATE( rn(k1) )
  nw = k1
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npt
  k2 = 4
  ALLOCATE( nplac(k1,k2) )
  nw = k1*k2
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npt
  k2 = 4
  k3 = 4
  ALLOCATE( mpla(k1,k2,k3) )
  nw = k1*k2*k3
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npt
  k2 = npgaus
  ALLOCATE( rtint(k1,k2), ydrsdt(k1,k2), yrst(k1,k2) )
  nw = 3*(k1*k2)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npt
  k2 = npgaus
  k3 = 16
  k4 = nps
  ALLOCATE( fb(k1,k2,k3,k4) )
  nw = k1*k2*k3*k4
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npv
  ALLOCATE( csmv(k1) )
  nw = k1
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npv
  k2 = mpsmax
  ALLOCATE( dg11lmv(k1,k2), dg12lmv(k1,k2), dg22lmv(k1,k2), dg33lmv(k1,k2) )
  nw = 4*(k1*k2)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npv1
  ALLOCATE( csv(k1) )
  nw = k1
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npv1
  k2 = mpsmax
  ALLOCATE( dg11lv(k1,k2), dg12lv(k1,k2), dg22lv(k1,k2), dg33lv(k1,k2) )
  nw = 4*(k1*k2)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = nsnt
  ALLOCATE( cpsi1t(k1), d2pstn(k1), d2pstt(k1), diffds(k1), diffdt(k1), &
       &diffp(k1), diffst(k1), dpdsnu(k1), dpdsth(k1), dpdtnu(k1), &
       &dpdtth(k1), nupdwn(k1) )
  nw = 12*(k1)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = nsp1
  ALLOCATE( csig(k1), csigo(k1) )
  nw = 2*(k1)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = ntp1
  ALLOCATE( ct(k1), cto(k1), drsdt(k1), rhos(k1) )
  nw = 4*(k1)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = ntp2
  k2 = npisoeff
  ALLOCATE( bchin(k1,k2), bchio(k1,k2), chin(k1,k2), chio(k1,k2), sigmap(k1,k2), &
       &tetmap(k1,k2) )
  nw = 6*(k1*k2)
  nwtot = nwtot + nw
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = ntp2
  k2 = npisoeff
  ALLOCATE( ah(k1,k2), ahpr(k1,k2), ahprcor(k1,k2))
  nw = 3*(k1*k2)
  nwtot = nwtot + nw
  !------
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !------
  k1 = npchi1
  k2 = npisoeff
  k3 = npopulations
  ALLOCATE( gammaparxt(k1,k2,k3),tetparxt(k1,k2,k3),sigparxt(k1,k2,k3),rhoparxt(k1,k2,k3) )
  nw = k1*k2*k3
  nwtot = nwtot + nw
  !------
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Allocated ', REAL(nw,rkind)/1e3, ' kW'
  !
  IF (NVERBOSE .GE. 3) WRITE(*,'(a,f10.3,a)') 'Total ', REAL(nwtot,rkind)/1e6, ' MW'
  !
END SUBROUTINE g_2
