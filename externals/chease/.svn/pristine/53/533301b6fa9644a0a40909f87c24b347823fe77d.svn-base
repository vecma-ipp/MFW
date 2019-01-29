function [eqdskval]=read_eqdsk(fname,varargin);
%
% Read eqdsk files
%
% function [eqdskval]=read_eqdsk(fname,varargin);
%
% if fname is empty or not given, prompts for it
% if fname is a structure, assume basic eqdsk structure and compute other fields as if varargin{2}=1
%
% for plots: plot_eqdsk(eqdskval)
%
% varargin{1}: COCOS index value (see paper in chease/trunk for details). Assume 2 (as CHEASE/EFIT) if not given
% varargin{2}: 0 or 1 to calculate (if 1) or not (default) B, BR, BZ, Grad-Shafranov, etc (-1: as 0 and do not print anything)
% varargin{3}: tension (= -0.5 if not given), used only if varargin{1}=1;
% varargin{4}: tension2D (= -1.0 if not given) used for R_equi, Z_equi, rho(theta) calculations (useful to limit d2rho/dtheta2)
% varargin{5}: nverbose: 0 (no displays), 1 (warnings), 3 (detailed information, file written, etc (default))
%

cocos_in = 2;
if nargin>1 && ~isempty(varargin{1})
  cocos_in = varargin{1};
end

docalculateBRetc = 0;
if nargin>2 && ~isempty(varargin{2})
  docalculateBRetc = varargin{2};
end

tension1D=-0.5;
if nargin>3 && ~isempty(varargin{3})
  tension1D = varargin{3};
end

tension2D=-1.;
if nargin>4 && ~isempty(varargin{4})
  tension2D = varargin{4};
end

nverbose=3;
if nargin>5 && ~isempty(varargin{5})
  nverbose = varargin{5};
end

% check file name and input
doread_fname = 1;
if nargin == 0
  [fname,pathname] = uigetfile([{'EQDSK*;eqdsk*'},{'EQDSK* or eqdsk* files'}; ...
                    {'*EQDSK*;*eqdsk*'},{'*EQDSK* or *eqdsk* files'};{'*'},{'All files'}],'Select an EQDSK file');
  if isequal(fname,0) || isequal(pathname,0); return; end
  fnamefull = fullfile(pathname, fname);
elseif isstruct(fname)
  eqdskval = fname;
  doread_fname = 0;
  docalculateBRetc = 1;
else;
  fnamefull = fname;
  if ~exist(fnamefull); 
    if nverbose>=1; disp([fnamefull,' does not exist, prompts for it']); end
    [fname,pathname] = uigetfile([{'EQDSK*;eqdsk*'},{'EQDSK* or eqdsk* files'}; ...
                    {'*EQDSK*;*eqdsk*'},{'*EQDSK* or *eqdsk* files'};{'*'},{'All files'}],'Select an EQDSK file');
    if isequal(fname,0) || isequal(pathname,0); return; end
    fnamefull = fullfile(pathname, fname);
  else
    ii=findstr(fnamefull,filesep);
    if ~isempty(ii)
      fname=fnamefull(ii(end)+1:end);
      pathname=fnamefull(1:ii(end)-1);
    else
      fname=fnamefull;
      pathname=pwd;
    end
  end
end

if doread_fname
  eqdskval.fnamefull=fnamefull;
  eqdskval.fname=fname;
  eqdskval.pathname=pathname;
  eqdskval.cocos = cocos_in;

  fid=fopen(fnamefull,'r');
  if fid==-1; error(['error opening ',fnamefull]); end
  
  eqdskval.stitle=fscanf(fid,'%c',48);
  ind=fscanf(fid,'%d',3);
  % goes to next line aftar char(10)
  nextchar=fscanf(fid,'%c',1);
  ii=1;
  while double(nextchar)~=10
    nextchar=fscanf(fid,'%c',1);
    ii=ii+1;
  end
  eqdskval.ind1=ind(1);
  eqdskval.nr=ind(2);
  eqdskval.nz=ind(3);
  for i=1:4
    line(i,1:5)=fscanf(fid,'%g',5)';
  end
  eqdskval.F=fscanf(fid,'%g',eqdskval.nr);
  eqdskval.p=fscanf(fid,'%g',eqdskval.nr);
  eqdskval.FFprime=fscanf(fid,'%g',eqdskval.nr);
  eqdskval.pprime=fscanf(fid,'%g',eqdskval.nr);
  eqdskval.psirz=fscanf(fid,'%g',eqdskval.nr*eqdskval.nz);
  eqdskval.q=fscanf(fid,'%g',eqdskval.nr);
  eqdskval.psimesh=linspace(0,1,eqdskval.nr)';
  eqdskval.rhopsi=sqrt(eqdskval.psimesh);
  eqdskval.nbbound=fscanf(fid,'%d',1);
  eqdskval.nblim=fscanf(fid,'%d',1);
  rzplasma=fscanf(fid,'%g',2*eqdskval.nbbound);
  eqdskval.rplas=rzplasma(1:2:end);
  eqdskval.zplas=rzplasma(2:2:end);
  rzlimiter=fscanf(fid,'%g',2*eqdskval.nblim);
  eqdskval.rlim=rzlimiter(1:2:end);
  eqdskval.zlim=rzlimiter(2:2:end);
  %
  % read extra lines as comments in extralines cell array
  % footer of file
  ii=1;
  while 1
    tline = fgetl(fid);
    if ~ischar(tline);break;end;
    footer{ii}=tline;
    ii=ii+1;
  end
  if length(footer)>1 && isempty(footer{1})
    eqdskval.extralines = footer(2:end);
  else
    eqdskval.extralines=footer;
  end
  % strvcat(footer)
  
  fclose(fid);

  %
  eqdskval.psi=NaN*ones(eqdskval.nr,eqdskval.nz);
  for j=1:eqdskval.nz
    eqdskval.psi(:,j)=eqdskval.psirz((j-1)*eqdskval.nr+[1:eqdskval.nr]);
  end

  eqdskval.rboxlen=line(1,1);
  eqdskval.zboxlen=line(1,2);
  eqdskval.r0=line(1,3);
  if nverbose>=1; 
    if eqdskval.r0<0
      disp('***********************************************************************')
      disp(['                 WARNING: R0 is negatif: R0= ' num2str(eqdskval.r0)])
      disp('***********************************************************************')
    end
  end
  eqdskval.rboxleft=line(1,4);
  eqdskval.zmid=line(1,5);
  eqdskval.raxis=line(2,1);
  eqdskval.zaxis=line(2,2);
  eqdskval.psiaxis=line(2,3);
  eqdskval.psiedge=line(2,4);
  eqdskval.b0=line(2,5);
  eqdskval.ip=line(3,1);
  
  eqdskval.rmesh=linspace(eqdskval.rboxleft,eqdskval.rboxleft+eqdskval.rboxlen,eqdskval.nr)';
  eqdskval.zmesh=linspace(eqdskval.zmid-eqdskval.zboxlen/2,eqdskval.zmid+eqdskval.zboxlen/2,eqdskval.nz)';

  % construct limiter from box if needed
  if length(eqdskval.rlim)==1 & eqdskval.nblim==1
    eqdskval.rlim=[eqdskval.rboxleft ; eqdskval.rboxleft+eqdskval.rboxlen ; ...
		   eqdskval.rboxleft+eqdskval.rboxlen ; eqdskval.rboxleft ; ...
		   eqdskval.rboxleft];
    eqdskval.zlim=[-eqdskval.zboxlen/2 ; -eqdskval.zboxlen/2 ; ...
		   +eqdskval.zboxlen/2 ; +eqdskval.zboxlen/2 ; ...
		   -eqdskval.zboxlen/2];
    eqdskval.nblim = 5;
  elseif length(eqdskval.rlim)==1
    if nverbose>=1
      disp(['Inconsistency: length(eqdskval.rlim)= ' num2str(length(eqdskval.rlim)) ...
	    ' but eqdskval.nblim = ' num2str(eqdskval.nblim)]);
    end
  end
  
end

if ~isfield(eqdskval,'cocos')
  eqdskval.cocos = cocos_in
end
cocos_in = eqdskval.cocos;

% check COCOS consistency
% check q sign
% Get assumed sign relations given the COCOS index
cocos_struct = cocos(cocos_in);
qsign=sign(eqdskval.q(end));
if nverbose>=1
  if qsign*cocos_struct.sigma_rhothetaphi*sign(eqdskval.ip)*sign(eqdskval.b0) < 0
    warning(['sign of q is: ' num2str(qsign)])
    warning(['But is should be equal to sign_rhothetaphi*sign(Ip)*sign(B0) = ' num2str(cocos_struct.sigma_rhothetaphi) '*' ...
             num2str(sign(eqdskval.ip)) '*' num2str(sign(eqdskval.b0)) ' = ' ...
             num2str(cocos_struct.sigma_rhothetaphi*sign(eqdskval.ip)*sign(eqdskval.b0))])
  end
  if sign(eqdskval.F)*sign(eqdskval.b0) < 0
    warning('Signs of F and B0 are not consistent')
  end
  if sign(eqdskval.psiedge-eqdskval.psiaxis)*cocos_struct.sigma_Bp*sign(eqdskval.ip) < 0
    if eqdskval.psiedge > eqdskval.psiaxis
      warning(['psi should be decreasing  with sign(Ip) = ' num2str(sign(eqdskval.ip)) ' for COCOS = ' num2str(cocos_struct.cocos)]);
    else
      warning(['psi should be increasing  with sign(Ip) = ' num2str(sign(eqdskval.ip)) ' for COCOS = ' num2str(cocos_struct.cocos)]);
    end
  elseif sign(mean(eqdskval.pprime))*sign(eqdskval.ip)*cocos_struct.sigma_Bp > 0
    warning(['sign(pprime) should be ' num2str(-sign(eqdskval.ip)*cocos_struct.sigma_Bp)])
  end
end

% add rhotor if interpos is available
intwhich = which('interpos');
if ~isempty(intwhich)
  % q = sigma_Bp sigma_rhothetphi / (2 pi)^(1-e_Bp) * dPhi_tor/dpsi
  % Phi_tor = int(B_phi dS_phi) within LCFS
  % rho_tor = sqrt( Phi_tor /pi / B0)
  % rho_tor_norm =  rho_tor / rho_tor(end);
  % int (sigma_Bp sigma_rhothetphi * (2 pi)^(1-e_Bp) * q dpsi)_0_psi to get Phi_tor
  % use d|psi-psi_axis| = sigma_Ip sigma_Bp dpsi to have increasing psi for interpos
  qtofit = sign(eqdskval.ip).*cocos_struct.sigma_rhothetaphi.*(2.*pi).^(1-cocos_struct.exp_Bp).*eqdskval.q;
  % not clear if psimesh has already psi_axis and psi_edge or is [0,1]. Make sure
  psi_incr = sign(eqdskval.ip).*cocos_struct.sigma_Bp.*(eqdskval.psimesh-eqdskval.psimesh(1))./(eqdskval.psimesh(end)-eqdskval.psimesh(1)).*(eqdskval.psiedge-eqdskval.psiaxis);
  [~,~,~,PhitorsignB0]=interpos(psi_incr,qtofit,-0.1,[2 2],[qtofit(1) qtofit(end)]);
  % Note that sigma_Ip sigma_rhothetaphi * q has sign(B0), thus Phi/B0 should be positive
  if min(sign(eqdskval.b0).*PhitorsignB0) < 0. && nverbose>=1
    warning('Phitor negative, check formula and signs');
  end
  eqdskval.rhotor = sqrt(abs(PhitorsignB0 / pi / eqdskval.b0));
  eqdskval.rhotor_norm = eqdskval.rhotor./eqdskval.rhotor(end);
end

% Add rho(theta) of plasma boundary and 1st, 2nd derivatives for testing input
eqdskval.rho_LCFS=sqrt((eqdskval.rplas-eqdskval.raxis).^2 + (eqdskval.zplas-eqdskval.zaxis).^2);
eqdskval.theta_LCFS=atan2(eqdskval.zplas-eqdskval.zaxis,eqdskval.rplas-eqdskval.raxis);
[eqdskval.thetasorted_LCFS,isort]=sort(eqdskval.theta_LCFS);
eqdskval.rhosorted_LCFS=eqdskval.rho_LCFS(isort);
[zz,eqdskval.drhodthetasorted_LCFS, eqdskval.d2rhodtheta2sorted_LCFS]=interpos(eqdskval.thetasorted_LCFS,eqdskval.rhosorted_LCFS, ...
          eqdskval.thetasorted_LCFS,tension2D,[-1 -1],2.*pi);

% Input R,Z points often from crossing R,Z grid so can have very nearby points. Reconstruct R,Zs from equidistant theta mesh
eqdskval.theta_equi=linspace(0.,2.*pi,length(eqdskval.rplas))'; % add redundant point for plotting reasons...
[eqdskval.rho_equi]=interpos(eqdskval.thetasorted_LCFS,eqdskval.rhosorted_LCFS, ...
          eqdskval.theta_equi,tension2D,[-1 -1],2.*pi);
[zz,eqdskval.drhodtheta_equi, eqdskval.d2rhodtheta2_equi]=interpos(eqdskval.theta_equi,eqdskval.rho_equi, ...
          eqdskval.theta_equi,-0.1,[-1 -1],2.*pi);
eqdskval.rplas_equi = eqdskval.raxis + eqdskval.rho_equi .* cos(eqdskval.theta_equi);
eqdskval.zplas_equi = eqdskval.zaxis + eqdskval.rho_equi .* sin(eqdskval.theta_equi);

if docalculateBRetc ~= 1
  if docalculateBRetc ~= -1 && nverbose>=3; disp('do not calculate BR, BZ, Bphi, etc'); end
  return
end
if nverbose>=3; disp('do calculate BR, BZ, Bphi, etc'); end

% Compute BR, BZ and Bphi
% Rjphi and Delta_star psi (gradshafranov)
%
ioptos=13;

clear psiz_1 dpsidZ_1 d2psidZ2_1
for iR=1:length(eqdskval.rmesh)
  [psiz_1(iR,:),dpsidZ_1(iR,:),d2psidZ2_1(iR,:)]= ...
      interpos(ioptos,eqdskval.zmesh,eqdskval.psi(iR,:),tension1D);
end

clear psir_1 dpsidR_1 d2psidR2_1
for iZ=1:length(eqdskval.zmesh)
  [psir_1(:,iZ),dpsidR_1(:,iZ),d2psidR2_1(:,iZ)]=interpos(ioptos,eqdskval.rmesh,eqdskval.psi(:,iZ),tension1D);
end

% compute Grad-Shafranov terms
% find pprime and ttprime on r,z mesh
clear pprime_RZ FFprime_RZ
for iR=1:length(eqdskval.rmesh)
  pprime_RZ(iR,:)= interpos(63,eqdskval.psimesh,eqdskval.pprime, ...
          (eqdskval.psi(iR,:)-eqdskval.psiaxis)./(eqdskval.psiedge-eqdskval.psiaxis),0.3*tension1D, ...
          [2 2],[eqdskval.pprime(1) eqdskval.pprime(end)]);
  FFprime_RZ(iR,:)= interpos(63,eqdskval.psimesh,eqdskval.FFprime, ...
          (eqdskval.psi(iR,:)-eqdskval.psiaxis)./(eqdskval.psiedge-eqdskval.psiaxis),0.3*tension1D, ...
          [2 2],[eqdskval.FFprime(1) eqdskval.FFprime(end)]);
  F_RZ(iR,:)= interpos(63,eqdskval.psimesh,eqdskval.F, ...
          (eqdskval.psi(iR,:)-eqdskval.psiaxis)./(eqdskval.psiedge-eqdskval.psiaxis),0.3*tension1D, ...
          [2 2],[eqdskval.F(1) eqdskval.F(end)]);
  p_RZ(iR,:)= interpos(63,eqdskval.psimesh,eqdskval.p, ...
          (eqdskval.psi(iR,:)-eqdskval.psiaxis)./(eqdskval.psiedge-eqdskval.psiaxis),0.3*tension1D, ...
          [2 2],[eqdskval.p(1) eqdskval.p(end)]);
end
R_RZ=eqdskval.rmesh * ones(size(eqdskval.zmesh'));
% values in vacuum
ij=find((psir_1-eqdskval.psiaxis)./(eqdskval.psiedge-eqdskval.psiaxis)>1);
pprime_RZ(ij)=0.;
FFprime_RZ(ij)=0.;
F_RZ(ij)=eqdskval.F(end);
p_RZ(ij)=0.;

% Use COCOS paper for general formulas
coeffBRZ = cocos_struct.sigma_RphiZ.*cocos_struct.sigma_Bp./(2.*pi).^cocos_struct.exp_Bp;
eqdskval.BR = coeffBRZ .* dpsidZ_1 ./ R_RZ;
eqdskval.BZ = - coeffBRZ .* dpsidR_1 ./ R_RZ;
eqdskval.Bphi = F_RZ ./ R_RZ;
eqdskval.B = sqrt(eqdskval.Bphi.^2 + eqdskval.BR.^2 + eqdskval.BZ.^2);
eqdskval.Bpol = sqrt(eqdskval.BR.^2 + eqdskval.BZ.^2) ./ R_RZ;
eqdskval.pprime_RZ = pprime_RZ;
eqdskval.FFprime_RZ = FFprime_RZ;
eqdskval.R_RZ = R_RZ;
eqdskval.p_RZ = p_RZ;
eqdskval.F_RZ = F_RZ;

mu0=4e-7.*pi;
eqdskval.rjphi=-R_RZ.^2.*mu0.*(2.*pi).^(2*cocos_struct.exp_Bp) .* pprime_RZ - (2.*pi).^(2*cocos_struct.exp_Bp) .* FFprime_RZ;

eqdskval.gradshaf= d2psidR2_1 -1./R_RZ.*dpsidR_1 + d2psidZ2_1;
