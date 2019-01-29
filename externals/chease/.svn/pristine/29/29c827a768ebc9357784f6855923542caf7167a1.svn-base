% dat2mat.m - read CHEASE output file

load chease.dat
NPSI1 = chease(1) ;
INSUR = chease(2) ;
INS = chease(3) ;
INR = chease(4) ;
NNBBPP = chease(5) ;
NCURV = chease(6) ;
NCHI = chease(7) ;
ii = 7 ;

%  BALLOONING (IN)STABILITY FLAG ON EACH FLUX SURFACE: (1)0 -> IBALL(NPSI1)
IBALL(1:NPSI1) = chease(ii+1:ii+NPSI1) ;
ii = ii+NPSI1 ;
%  IDEAL INTERCHANGE (IN)STABILITY FLAG: (1)0 -> IMERCI(NPSI1)
IMERCI(1:NPSI1) = chease(ii+1:ii+NPSI1) ;
ii = ii+NPSI1 ;
%  RESISTIVE INTERCHANGE (IN)STABILITY FLAG: (1)0 -> IMERCR(NPSI1)
IMERCR(1:NPSI1) = chease(ii+1:ii+NPSI1) ;
ii = ii+NPSI1 ;
%
%  R-VECTOR FOR PLASMA SURFACE -> ZRSUR(INSUR)
ZRSUR(1:INSUR) = chease(ii+1:ii+INSUR) ;
ii = ii+INSUR ;
%  Z-VECTOR FOR PLASMA SURFACE -> ZZSUR(INSUR)
ZZSUR(1:INSUR) = chease(ii+1:ii+INSUR) ;
ii = ii+INSUR ;
%
%  S-VECTOR FOR PROFILES -> ZABS(NPSI1) (S_k mesh)
ZABS(1:NPSI1) = chease(ii+1:ii+NPSI1) ;
ii = ii+NPSI1 ;
%  S-VECTOR FOR PROFILES -> ZABSM(INS) (S_(k+1/2) mesh)
ZABSM(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%  R-VECTOR FOR PROFILES -> ZABR(INR)
ZABR(1:INR) = chease(ii+1:ii+INR) ;
ii = ii+INR ;
%  Q(S)-PROFILE -> ZOQS(INS)
ZOQS(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%  Q(R)-PROFILE -> ZOQR(INR)
ZOQR(1:INR) = chease(ii+1:ii+INR) ;
ii = ii+INR ;
%  Q'(S)-PROFILE -> ZODQS(INS)
ZODQS(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%  Q'(R)-PROFILE -> ZODQR(INR)
ZODQR(1:INR) = chease(ii+1:ii+INR) ;
ii = ii+INR ;
%  SHEAR S(S)-PROFILE -> ZOSHS(INS)
ZOSHS(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%  SHEAR S(R)-PROFILE -> ZOSHR(INR)
ZOSHR(1:INR) = chease(ii+1:ii+INR) ;
ii = ii+INR ;
%  <J.B>/<B.GRAD-PHI> S-PROFILE -> ZOJBS(INS)
ZOJBS(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%  <J.B>/<B.GRAD-PHI> R-PROFILE -> ZOJBR(INR)
ZOJBR(1:INR) = chease(ii+1:ii+INR) ;
ii = ii+INR ;
%
% BOOTSTRAP CURRENT <JBOOT.B> S-PROFILE -> ZOJBSS(INS,4)
ZOJBSS = zeros(INS,4) ;
for jj=1:4
  ZOJBSS(1:INS,jj) = chease(ii+1:ii+INS,1) ;
  ii = ii+INS ;
end
% BOOTSTRAP CURRENT <JBOOT.B> R-PROFILE -> ZOJBSR(INR,4)
ZOJBSR = zeros(INR,4) ;
for jj=1:4
  ZOJBSS(1:INR,jj) = chease(ii+1:ii+INR,1) ;
  ii = ii+INR ;
end
%
% TOTAL CURRENT <JTOT.B> S-PROFILE -> ZOJPS(INS)
ZOJPS(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
% TOTAL CURRENT <JTOT.B> R-PROFILE -> ZOJPR(INR)
ZOJPR(1:INR) = chease(ii+1:ii+INR) ;
ii = ii+INR ;
%
% TRAPPED FRACTION S-PROFILE -> ZOTRS(INS)
ZOTRS(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%  TRAPPED FRACTION R-PROFILE -> ZOTRR(INR)
ZOTRR(1:INR) = chease(ii+1:ii+INR) ;
ii = ii+INR ;
%
%  H OF GLASSER-GREENE-JOHNSON S-PROFILE -> ZOHS(INS)
ZOHS(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%  IDEAL MERCIER COEFFICIENT -DI(S) -> ZODIS(INS)
ZODIS(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%  RESISTIVE MERCIER COEFFICIENT -DR(S) -> ZODRS(INS)
ZODRS(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%  J1 INTEGRAL S-PROFILE -> ZOJ1S(INS)
ZOJ1S(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%  J4 INTEGRAL S-PROFILE -> ZOJ4S(INS)
ZOJ4S(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%  J5 INTEGRAL S-PROFILE -> ZOJ5S(INS)
ZOJ5S(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%  J6 INTEGRAL S-PROFILE -> ZOJ6S(INS)
ZOJ6S(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%
%  P'(S)-PROFILE -> ZOPPS(INS)
ZOPPS(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%  P'(R)-PROFILE -> ZOPPR(INR)
ZOPPR(1:INR) = chease(ii+1:ii+INR) ;
ii = ii+INR ;
%  P(S)-PROFILE -> ZOPS(INS)
ZOPS(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%  P(R)-PROFILE -> ZOPR(INR)
ZOPR(1:INR) = chease(ii+1:ii+INR) ;
ii = ii+INR ;
%
%  TT'(S)-PROFILE -> ZOTTS(INS)
ZOTTS(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%  TT'(R)-PROFILE -> ZOTTR(INR)
ZOTTR(1:INR) = chease(ii+1:ii+INR) ;
ii = ii+INR ;
%  T(S)-PROFILE -> ZOTS(INS)
ZOTS(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%  T(R)-PROFILE -> ZOTR(INR)
ZOTR(1:INR) = chease(ii+1:ii+INR) ;
ii = ii+INR ;
%
%  I*(S)-PROFILE -> ZOIPS(INS)
ZOIPS(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%  I*(R)-PROFILE -> ZOIPR(INR)
ZOIPR(1:INR) = chease(ii+1:ii+INR) ;
ii = ii+INR ;
%
%  BETA POLOIDAL R-PROFILE -> ZOBETR(INR)
ZOBETR(1:INR) = chease(ii+1:ii+INR) ;
ii = ii+INR ;
%  BETA POLOIDAL S-PROFILE -> ZOBETS(INS)
ZOBETS(1:INS) = chease(ii+1:ii+INS) ;
ii = ii+INS ;
%
%  POLOIDAL FLUX R-PROFILE -> ZOFR(INR)
ZOFR(1:INR) = chease(ii+1:ii+INR) ;
ii = ii+INR ;
%
%  SQRT(VOLUME OF FLUX TUBE) S-PROFILE -> ZOARS(NPSI1)
ZOARS(1:NPSI1) = chease(ii+1:ii+NPSI1) ;
ii = ii+NPSI1 ;
%
%  TOROIDAL CURRENT DENSITY R-PROFILE -> ZOJR(INR)
ZOJR(1:INR) = chease(ii+1:ii+INR) ;
ii = ii+INR ;
%
%  R-MATRIX FOR ISOPSI-SURFACES -> RRISO(NNBBPP,NPSI1)
RRISO = zeros(NNBBPP,NPSI1) ;
for jj=1:NNBBPP
  RRISO(jj,1:NPSI1) = chease(ii+1:ii+NPSI1,1)' ;
  ii = ii+NPSI1 ;
end
%  Z-MATRIX FOR ISOPSI-SURFACES -> RZISO(NNBBPP,NPSI1)
RZISO = zeros(NNBBPP,NPSI1) ;
for jj=1:NNBBPP
  RZISO(jj,1:NPSI1) = chease(ii+1:ii+NPSI1,1)' ;
  ii = ii+NPSI1 ;
end
%
%  R-VECTOR FOR ZERO-CURVATURE CURVE -> ZRCURV(NCURV)
ZRCURV(1:NCURV) = chease(ii+1:ii+NCURV) ;
ii = ii+NCURV ;
%  Z-VECTOR FOR ZERO-CURVATURE CURVE -> ZZCURV(NCURV)
ZZCURV(1:NCURV) = chease(ii+1:ii+NCURV) ;
ii = ii+NCURV ;
%
%  LOCAL_SHEAR-MATRIX ON (CR,CZ) MESH -> RSHEAR,CR&CZ(NCHI,NPSI1)
RSHEAR = zeros(NCHI,NPSI1) ;
for jj=1:NCHI
  RSHEAR(jj,1:NPSI1) = chease(ii+1:ii+NPSI1,1)' ;
  ii = ii+NPSI1 ;
end
CR = zeros(NCHI,NPSI1) ;
for jj=1:NCHI
  CR(jj,1:NPSI1) = chease(ii+1:ii+NPSI1,1)' ;
  ii = ii+NPSI1 ;
end
CZ = zeros(NCHI,NPSI1) ;
for jj=1:NCHI
  CZ(jj,1:NPSI1) = chease(ii+1:ii+NPSI1,1)' ;
  ii = ii+NPSI1 ;
end
