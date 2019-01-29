function [eqdskval_new, varargout]=eqdsk_transform(eqdskval,nrmesh,nzmesh,varargin)
%
% [eqdskval_new]=eqdsk_transform(eqdskval,nrmesh,nzmesh); % default tension and no plots
% [eqdskval_new]=eqdsk_transform(eqdskval,nrmesh,nzmesh,[],1) % makes plots including Grad-shfranov test
% [eqdskval_new]=eqdsk_transform(eqdskval,nrmesh,nzmesh,tension,1) % gives tension and makes plots including Grad-shfranov test
% [eqdskval_new]=eqdsk_transform(eqdskval,nrmesh,nzmesh,[],[],psiedge); % Use psiedge to calculate plasma boundary
%
%    varargin{1}: tension value (empty means use default within interpos: tension=-1)
%    varargin{2}: =1: do plots and tests Grad-Shafranov (default=0)
%    varargin{3}: psiedge value used to recompute plasma boundary (default use eqdskval.psiedge)
%                 This allows for example to be closer to last closed flux surface
%    varargin{4}: zshift to zaxis (11111), no shift if empty or 10000 (default). Needs to extrapolate extra points in box
%                 Otherwise shift z to zshift=varargin{4}
%

set_defaults_matlab;

tension1D=0.1.*((min(diff(eqdskval.rmesh))+min(diff(eqdskval.zmesh)))./2).^3;
tension1D=-0.1;
if nargin>=4 && ~isempty(varargin{1}); tension1D = varargin{1}; end
doplot=0;
if nargin>=5 && ~isempty(varargin{2}); doplot = varargin{2}; end
recompute_edge=0;
psiedge_newbnd=eqdskval.psiedge;
if nargin>=6 && ~isempty(varargin{3})
  psiedge_newbnd = varargin{3}; 
  recompute_edge = 1;
end
zshift=10000;
if nargin>=7 && ~isempty(varargin{4}); zshift = varargin{4}; end
dozshift=0;
if zshift~=10000; dozshift=1; end

zaxis_shift=0.;
if dozshift
  if zshift==11111
    zaxis_shift = eqdskval.zaxis;
  else
    zaxis_shift = zshift
  end
end

NR=nrmesh;
NZ=nzmesh;
rmeshfit=linspace(eqdskval.rmesh(1),eqdskval.rmesh(end),NR);

% can only just shift the plasma. But needs to extrapolate to keep max box
eqdskval.zmesh = eqdskval.zmesh - zaxis_shift;
eqdskval.zaxis = eqdskval.zaxis - zaxis_shift;
eqdskval.zplas = eqdskval.zplas - zaxis_shift;
eqdskval.zlim = eqdskval.zlim - zaxis_shift;
if isfield(eqdskval,'extralines')
  eqdskval.extralines{end+1} = ['plasma shifted in z by ' num2str(zaxis_shift)];
else
    eqdskval.extralines{1} = ['plasma shifted in z by ' num2str(zaxis_shift)];
end
zmaxbox=max(abs(eqdskval.zmesh(1)),abs(eqdskval.zmesh(end)));
zmeshfit=linspace(-zmaxbox,zmaxbox,NR);
eqdskval.zboxlen = 2.*zmaxbox;

ioptos=13;
clear psiz_1 dpsidZ_1 d2psidZ2_1
clear psiz0_1 dpsidZ0_1 d2psidZ20_1
for iR=1:length(eqdskval.rmesh)
  [psiz_1(iR,:),dpsidZ_1(iR,:),d2psidZ2_1(iR,:)]= ...
      interpos(ioptos,eqdskval.zmesh,eqdskval.psi(iR,:),zmeshfit,tension1D);
  [psiz0_1(iR,:),dpsidZ0_1(iR,:),d2psidZ20_1(iR,:)]= ...
      interpos(ioptos,eqdskval.zmesh,eqdskval.psi(iR,:),zmeshfit);
end
clear psir_1 dpsidR_1 d2psidR2_1
clear psir0_1 dpsidR0_1 d2psidR20_1
for iZ=1:length(zmeshfit)
  [psir_1(:,iZ),dpsidR_1(:,iZ),d2psidR2_1(:,iZ)]=interpos(ioptos,eqdskval.rmesh',psiz_1(:,iZ),rmeshfit',tension1D);
  [psir0_1(:,iZ),dpsidR0_1(:,iZ),d2psidR20_1(:,iZ)]=interpos(ioptos,eqdskval.rmesh',psiz0_1(:,iZ),rmeshfit');
end
% d2pdidz2: from psir_1 with 0 spline from d2psidZ2_1 and 0 spline? from psir0_1 and tension spline?
clear d2psidZ2_2 psiz_3 dpsidZ_3 d2psidZ2_3 psiz_4 dpsidZ_4 d2psidZ2_4 psiz0_4 dpsidZ0_4 d2psidZ20_4
for iZ=1:length(zmeshfit)
  [d2psidZ2_2(:,iZ)]=interpos(ioptos,eqdskval.rmesh',d2psidZ2_1(:,iZ),rmeshfit',0);
end
for iR=1:length(rmeshfit)
  [psiz_3(iR,:),dpsidZ_3(iR,:),d2psidZ2_3(iR,:)]=interpos(ioptos,zmeshfit,psir_1(iR,:),0);
  [psiz_4(iR,:),dpsidZ_4(iR,:),d2psidZ2_4(iR,:)]=interpos(ioptos,zmeshfit,psir0_1(iR,:),tension1D);
  [psiz0_4(iR,:),dpsidZ0_4(iR,:),d2psidZ20_4(iR,:)]=interpos(ioptos,zmeshfit,psir0_1(iR,:));
end
% They, _2, _3, _4 options, seem very equivalent so choose _2 option

% compute Grad-Shafranov terms
% find pprime and ttprime on r,z mesh
clear pprimeRZ FFprimeRZ
for iR=1:length(rmeshfit)
  pprimeRZ(iR,:)= interpos(63,eqdskval.psimesh,eqdskval.pprime,(psir_1(iR,:)-eqdskval.psiaxis)./(eqdskval.psiedge-eqdskval.psiaxis));
  FFprimeRZ(iR,:)= interpos(63,eqdskval.psimesh,eqdskval.FFprime,(psir_1(iR,:)-eqdskval.psiaxis)./(eqdskval.psiedge-eqdskval.psiaxis));
  FRZ(iR,:)= interpos(63,eqdskval.psimesh,eqdskval.F,(psir_1(iR,:)-eqdskval.psiaxis)./(eqdskval.psiedge-eqdskval.psiaxis));
  pRZ(iR,:)= interpos(63,eqdskval.psimesh,eqdskval.p,(psir_1(iR,:)-eqdskval.psiaxis)./(eqdskval.psiedge-eqdskval.psiaxis));
end
ij=find((psir_1-eqdskval.psiaxis)./(eqdskval.psiedge-eqdskval.psiaxis)>1);
pprimeRZ(ij)=0.;
FFprimeRZ(ij)=0.;

mu0=4e-7.*pi;
RRR=rmeshfit'*ones(1,length(zmeshfit));
rjphi=-RRR.^2.*mu0.*pprimeRZ-FFprimeRZ;

gradshaf_2= d2psidR2_1 -1./RRR.*dpsidR_1 + d2psidZ2_2;
gradshaf_3= d2psidR2_1 -1./RRR.*dpsidR_1 + d2psidZ2_3;
gradshaf_4= d2psidR2_1 -1./RRR.*dpsidR_1 + d2psidZ2_4;
gradshaf0= d2psidR20_1 -1./RRR.*dpsidR0_1 + d2psidZ20_4;

eqdskval.BR = - dpsidZ_3 ./ RRR;
eqdskval.BZ = dpsidR_1 ./ RRR;
eqdskval.Bphi = FRZ ./ RRR;
eqdskval.B = sqrt(eqdskval.Bphi.^2 + eqdskval.BR.^2 + eqdskval.BZ.^2);
eqdskval.Bpol = sqrt(eqdskval.BR.^2 + eqdskval.BZ.^2) ./ RRR;
eqdskval.pprime_RZ = pprimeRZ;
eqdskval.FFprime_RZ = FFprimeRZ;
eqdskval.R_RZ = RRR;
eqdskval.p_RZ = pRZ;
eqdskval.F_RZ = FRZ;
eqdskval.rjphi = rjphi;
eqdskval.gradshaf = gradshaf_2;


[zzz ir]=min(abs(eqdskval.rmesh-eqdskval.raxis));
[zzz iz]=min(abs(eqdskval.zmesh-eqdskval.zaxis));
[zzz irfit]=min(abs(rmeshfit-eqdskval.raxis));
[zzz izfit]=min(abs(zmeshfit-eqdskval.zaxis));

if doplot
  figure
  plot(rmeshfit,rjphi(:,izfit),'k')
  hold on
  plot(rmeshfit,gradshaf0(:,izfit),'b--')
  plotos(rmeshfit,gradshaf_2(:,izfit),'--',[],[],colos(2,:));
  plotos(rmeshfit,gradshaf_3(:,izfit),'-',[],[],colos(3,:));
  plotos(rmeshfit,gradshaf_4(:,izfit),'--',[],[],colos(4,:));
  xlabel(['R at Z=' num2str(eqdskval.zmesh(iz),'%.2f')])
  legend('R j_{\phi}','\Delta^* \psi std spline','\Delta^* \psi \tau spline','\Delta^* \psi \tau spline','\Delta^* \psi \tau spline')
  title('Grad-Shfranov terms')

  figure
  plot(zmeshfit,rjphi(irfit,:),'k')
  hold on
  plot(zmeshfit,gradshaf0(irfit,:),'b--')
  plot(zmeshfit,gradshaf_2(irfit,:),'r--')
  plotos(zmeshfit,gradshaf_2(irfit,:),'--',[],[],colos(2,:));
  plotos(zmeshfit,gradshaf_3(irfit,:),'-',[],[],colos(3,:));
  plotos(zmeshfit,gradshaf_4(irfit,:),'--',[],[],colos(4,:));
  xlabel(['Z at R=' num2str(eqdskval.rmesh(ir),'%.2f')])
  legend('R j_{\phi}','\Delta^* \psi std spline','\Delta^* \psi \tau spline','\Delta^* \psi \tau spline','\Delta^* \psi \tau spline')
  title('Grad-Shfranov terms')
  
  figure
  zzzmin=min(min(rjphi));
  zzzmax=max(max(rjphi));
  contour(rmeshfit,zmeshfit,gradshaf_2',linspace(zzzmin,zzzmax,100));
  axis equal
  
  figure
  contour(rmeshfit,zmeshfit,rjphi',linspace(zzzmin,zzzmax,100));
  axis equal
  
  figure
  contour(rmeshfit,zmeshfit,rjphi',linspace(zzzmin,zzzmax,60),'-');
  hold on
  contour(rmeshfit,zmeshfit,gradshaf_2',linspace(zzzmin,zzzmax,60),'--');
  axis equal
  
end

fig_cont=figure;
contour(eqdskval.rmesh,eqdskval.zmesh,eqdskval.psi',120,'k')
hold on
contour(rmeshfit,zmeshfit,psir_1',120,'--')
contour(eqdskval.rmesh,eqdskval.zmesh,eqdskval.psi',[eqdskval.psiedge eqdskval.psiedge],'k','linewidth',3);
contour(rmeshfit,zmeshfit,psir_1',[eqdskval.psiedge eqdskval.psiedge],'r--','linewidth',3);
[Cedge Hedge]=contour(rmeshfit,zmeshfit,psir_1',[psiedge_newbnd psiedge_newbnd],'m--','linewidth',3);

if ~doplot
  close(fig_cont)
end

% $$$ ii=find(abs(Cedge(1,:)-eqdskval.psiedge)<1e-4);
% $$$ [nbmax inbmax]=max(Cedge(2,ii));
% $$$ % Assume plasma boundary with largest nb points
% $$$ Redge_cnt=get(Hedge(inbmax),'XData');
% $$$ Zedge_cnt=get(Hedge(inbmax),'YData');
% $$$ plotos(Redge_cnt,Zedge_cnt,'--',[],[],colos(5,:));

ii=find(Cedge(1,:)==psiedge_newbnd);
iinb=Cedge(2,ii);
if isempty(ii)
  Redgenew=eqdskval.rplas;
  Zedgenew=eqdskval.zplas;
  psiedge_newbnd = eqdskval.psiedge;
else
  [iinbmax iimax]=max(iinb);
  for i=1:length(ii)
    if doplot
      plotos(Cedge(1,ii(i)+1:ii(i)+iinb(i)),Cedge(2,ii(i)+1:ii(i)+iinb(i)),'--',[],[],colos(5+i,:));
      if i==1; hold on; end
    end
    if i==iimax
      Redgenew=Cedge(1,ii(i)+1:ii(i)+iinb(i));
      Zedgenew=Cedge(2,ii(i)+1:ii(i)+iinb(i));
    end    
  end
end
if doplot
  axis equal
end

if doplot
  figure
  plot(zmeshfit,d2psidZ20_4(irfit,:),'-')
  hold on
  plotos(zmeshfit,d2psidZ2_2(irfit,:),'-',[],[],colos(2,:));
  plotos(zmeshfit,d2psidZ2_3(irfit,:),'-',[],[],colos(3,:));
  plotos(zmeshfit,d2psidZ2_4(irfit,:),'-',[],[],colos(4,:));
  plot(zmeshfit,d2psidR20_1(irfit,:),'--')
  plot(zmeshfit,d2psidR2_1(irfit,:),'r--')
  xlabel(['Z at R=' num2str(rmeshfit(irfit),'%.2f')])
  legend('d^2\psi/dZ^2 std spline (\tau=0)','d^2\psi/dZ^2 \tau spline', ...
      'd^2\psi/dR^2 std spline','d^2\psi/dR^2 \tau spline')
  
  figure
  plot(rmeshfit,d2psidZ20_4(:,izfit),'-')
  hold on
  plotos(rmeshfit,d2psidZ2_2(:,izfit),'-',[],[],colos(2,:));
  plotos(rmeshfit,d2psidZ2_3(:,izfit),'-',[],[],colos(3,:));
  plotos(rmeshfit,d2psidZ2_4(:,izfit),'-',[],[],colos(4,:));
  plot(rmeshfit,d2psidR20_1(:,izfit),'--')
  plot(rmeshfit,d2psidR2_1(:,izfit),'r--')
  xlabel(['R at Z=' num2str(zmeshfit(izfit),'%.2f')])
  legend('d^2\psi/dZ^2 std spline (\tau=0)','d^2\psi/dZ^2 \tau spline', ...
      'd^2\psi/dR^2 std spline','d^2\psi/dR^2 \tau spline')
end

eqdskval_new=eqdskval;
% refit 1D profiles
eqdskval_new.nr=length(rmeshfit);
eqdskval_new.nz=length(zmeshfit);
eqdskval_new.psimesh=linspace(eqdskval.psimesh(1),eqdskval.psimesh(end),eqdskval_new.nr);
eqdskval_new.rhopsi=sqrt(eqdskval_new.psimesh);
eqdskval_new.F=interpos(eqdskval.psimesh,eqdskval.F,eqdskval_new.psimesh);
eqdskval_new.FFprime=interpos(eqdskval.psimesh,eqdskval.FFprime,eqdskval_new.psimesh);
eqdskval_new.p=interpos(eqdskval.psimesh,eqdskval.p,eqdskval_new.psimesh);
eqdskval_new.pprime=interpos(eqdskval.psimesh,eqdskval.pprime,eqdskval_new.psimesh);
eqdskval_new.q=interpos(eqdskval.psimesh,eqdskval.q,eqdskval_new.psimesh);
% psi(R,Z) new
eqdskval_new.tension1D=tension1D;
eqdskval_new.rmesh=rmeshfit;
eqdskval_new.zmesh=zmeshfit;
eqdskval_new.psi=psir_1;
eqdskval_new.psirz=[];
for iz=1:eqdskval_new.nz
  eqdskval_new.psirz((iz-1)*eqdskval_new.nr+[1:eqdskval_new.nr])=eqdskval_new.psi(:,iz)';
end
eqdskval_new.psirz=reshape(eqdskval_new.psirz,length(eqdskval_new.psirz),1);
if recompute_edge
  eqdskval_new.nbbound = length(Redgenew);
  eqdskval_new.rplas = Redgenew';
  eqdskval_new.zplas = Zedgenew';
  eqdskval_new.psiedge = psiedge_newbnd;
end
eqdskval_new.extralines{end+1} = ['Through eqdsk_os ' date];
eqdskval_new.extralines{end+1} = ['tension= ' num2str(tension1D)];
if psiedge_newbnd ~= eqdskval.psiedge
  eqdskval_new.extralines{end+1} = ['psiedge changed from: ' num2str(eqdskval.psiedge) ' to ' num2str(psiedge_newbnd)];
end
