%
% to test eqdsk
%
set_defaults_matlab

fname=input('eqdsk filename: ','s');

eqdskval=read_eqdsk(fname);

figure
contour(eqdskval.rmesh,eqdskval.zmesh,eqdskval.psi',100);
hold on
plot(eqdskval.rplas,eqdskval.zplas,'k-');
[Cedge Hedge]=contour(eqdskval.rmesh,eqdskval.zmesh,eqdskval.psi',[eqdskval.psiedge eqdskval.psiedge],'r--');
% find Nb of curves, should be equal to nb of handles
nbcurve=length(Hedge);
ii=find(abs(Cedge(1,:)-eqdskval.psiedge)<1e-4);
if length(ii) ~= nbcurve
  disp('problem in test_eqdsk for edge contour? Ask O. Sauter')
  break
end
xx1=get(Hedge(1),'XData');
yy1=get(Hedge(1),'YData');
if length(Hedge)>1
  xx2=get(Hedge(2),'XData');
  yy2=get(Hedge(2),'YData');
else
end
[nbmax inbmax]=max(Cedge(2,ii));
% Assume plasma boundary with largest nb points
Redge_cnt=get(Hedge(inbmax),'XData');
Zedge_cnt=get(Hedge(inbmax),'YData');

% $$$ redge=NaN*ones(length(ii),nbmax);
% $$$ zedge=NaN*ones(length(ii),nbmax);
% $$$ for i=1:length(ii)
% $$$   istart=ii(i);
% $$$   npts(i)=Cedge(2,istart);
% $$$   redge(i,1:npts(i))=Cedge(1,istart+1:istart+npts(i));
% $$$   zedge(i,1:npts(i))=Cedge(2,istart+1:istart+npts(i));
% $$$ end

axis equal

% Ask if replace (R,Z) by contour edge data
changeRZ=input('Change (R,Z) plasma boundary to edge contour (red dashed line): (1 for yes, return for no) ','s');

if changeRZ
  eqdskval.nbbound = length(Zedge_cnt);
  eqdskval.rplas = reshape(Redge_cnt,eqdskval.nbbound,1);
  eqdskval.zplas = reshape(Zedge_cnt,eqdskval.nbbound,1);
  plot(eqdskval.rplas,eqdskval.zplas,'k--')
  disp('Plasma boundary changed to black dashed line')
  pause(5)
end

% xx and yy used for evaluation of spline form on full points
[xx,yy] = ndgrid(eqdskval.rmesh,eqdskval.zmesh);
tol2D=1e-1;
tol2D=3e-4;
tol2D=1e-8;
tol2D=3.*((min(diff(eqdskval.rmesh))+min(diff(eqdskval.zmesh)))./2).^3;
tol2D=0.5*((min(diff(eqdskval.rmesh))+min(diff(eqdskval.zmesh)))./2).^3;
tol2D=0.0001*((min(diff(eqdskval.rmesh))+min(diff(eqdskval.zmesh)))./2).^3
tic
sp = spaps({eqdskval.rmesh,eqdskval.zmesh},eqdskval.psi,tol2D);  
toc
tic
sp0 = spaps({eqdskval.rmesh,eqdskval.zmesh},eqdskval.psi,0);  
toc
% calculate smoothing from series of 1D smoothing
tension1D=1e-3;
tension1D=3e-5;
tension1D=2e-6;
tension1D=0.5.*((min(diff(eqdskval.rmesh))+min(diff(eqdskval.zmesh)))./2).^3
tension_in=input('change tension1D to: ?');
if ~isempty(tension_in)
  tension1D=tension_in;
end

ioptos=13;
clear psi2x1D_1
clear psi2x1Dos_1 dpsidZ2x1Dos_1 d2psidZ22x1Dos_1
for iR=1:length(eqdskval.rmesh)
  psi2x1D_1(iR,:)=csaps(eqdskval.zmesh,eqdskval.psi(iR,:),1-tension1D,eqdskval.zmesh);
end
tic
for iR=1:length(eqdskval.rmesh)
  [psi2x1Dos_1(iR,:),dpsidZ2x1Dos_1(iR,:),d2psidZ22x1Dos_1(iR,:)]=interpos(ioptos,eqdskval.zmesh,eqdskval.psi(iR,:),eqdskval.zmesh,tension1D);
end
toc
psi2x1D_2=NaN*ones(size(psi2x1D_1));
psi2x1Dos_2=NaN*ones(size(psi2x1Dos_1));
dpsidR2x1Dos_2=NaN*ones(size(psi2x1Dos_1));
d2psidR22x1Dos_2=NaN*ones(size(psi2x1Dos_1));
for iZ=1:length(eqdskval.zmesh)
  psi2x1D_2(:,iZ)=csaps(eqdskval.rmesh,psi2x1D_1(:,iZ),1-tension1D,eqdskval.rmesh');
end
tic
for iZ=1:length(eqdskval.zmesh)
  [psi2x1Dos_2(:,iZ),dpsidR2x1Dos_2(:,iZ),d2psidR22x1Dos_2(:,iZ)]=interpos(ioptos,eqdskval.rmesh',psi2x1Dos_1(:,iZ),eqdskval.rmesh',tension1D);
end
toc
clear psi2x1Dos_2b dpsidZ2x1Dos_2 d2psidZ22x1Dos_2 dpsidR2x1Dos_2b d2psidRZ2x1Dos_2
for iR=1:length(eqdskval.rmesh)
  [psi2x1Dos_2b(iR,:),dpsidZ2x1Dos_2(iR,:),d2psidZ22x1Dos_2(iR,:)]=interpos(ioptos,eqdskval.zmesh,psi2x1Dos_2(iR,:),eqdskval.zmesh,0);
  [dpsidR2x1Dos_2b(iR,:),d2psidRZ2x1Dos_2(iR,:)]=interpos(ioptos,eqdskval.zmesh,dpsidR2x1Dos_2(iR,:),eqdskval.zmesh,0);
end
sp2x1D0 = spaps({eqdskval.rmesh,eqdskval.zmesh},psi2x1D_2,0);  
sp2x1Dos = spaps({eqdskval.rmesh,eqdskval.zmesh},psi2x1Dos_2,0);  

clear psi2x1D_3
for iZ=1:length(eqdskval.zmesh)
  psi2x1D_3(:,iZ)=csaps(eqdskval.rmesh,eqdskval.psi(:,iZ),1-tension1D,eqdskval.rmesh');
end
psi2x1D_4=NaN*ones(size(psi2x1D_3));
for iR=1:length(eqdskval.rmesh)
  psi2x1D_4(iR,:)=csaps(eqdskval.zmesh,psi2x1D_3(iR,:),1-tension1D,eqdskval.zmesh);
end
sp2x1D0_b = spaps({eqdskval.rmesh,eqdskval.zmesh},psi2x1D_4,0);  

% derivatives along R, Z and diagonale (1st derivative)
deriv_todo=[1 0; 0 1;1 1]';
deriv=fndir(sp,deriv_todo);

% d2f/dR/dZ
derivRZ=fnder(sp,[1 1]);
% df/dR, df/dZ
derivR=fnder(sp,[1 0]);
derivZ=fnder(sp,[0 1]);
% d2f/dR2, d2f/dZ2
derivR2=fnder(sp,[2 0]);
derivZ2=fnder(sp,[0 2]);
% idem for sp0 with 0 smoothing
deriv0RZ=fnder(sp0,[1 1]);
deriv0R=fnder(sp0,[1 0]);
deriv0Z=fnder(sp0,[0 1]);
deriv0R2=fnder(sp0,[2 0]);
deriv0Z2=fnder(sp0,[0 2]);

% idem for sp2x1D0 with 0 smoothing
deriv2x1D0RZ=fnder(sp2x1D0,[1 1]);
deriv2x1D0RZ_b=fnder(sp2x1D0_b,[1 1]);
deriv2x1D0R=fnder(sp2x1D0,[1 0]);
deriv2x1D0Z=fnder(sp2x1D0,[0 1]);
deriv2x1D0R2=fnder(sp2x1D0,[2 0]);
deriv2x1D0Z2=fnder(sp2x1D0,[0 2]);
deriv2x1D0R2_b=fnder(sp2x1D0_b,[2 0]);
deriv2x1D0Z2_b=fnder(sp2x1D0_b,[0 2]);

clear Ronzaxis Zonraxis
Ronzaxis(1,:)=eqdskval.rmesh;
iz=fix((length(eqdskval.zmesh)-1)/2);
Ronzaxis(2,1:length(eqdskval.rmesh))=eqdskval.zmesh(iz);

ir=fix((length(eqdskval.rmesh)-1)/2);
Zonraxis(1,1:length(eqdskval.zmesh))=eqdskval.rmesh(ir);
Zonraxis(2,:)=eqdskval.zmesh;

% smoothed psi values
fspall=reshape(fnval(sp,[xx(:) yy(:)]'),length(eqdskval.rmesh),length(eqdskval.zmesh));
% Non-smoothed psi values (for checking)
fsp0all=reshape(fnval(sp0,[xx(:) yy(:)]'),length(eqdskval.rmesh),length(eqdskval.zmesh));

% dpsi/dR, d2/dR2, d2/dZ2 everywhere
dfdRall=reshape(fnval(derivR,[xx(:) yy(:)]'),length(eqdskval.rmesh),length(eqdskval.zmesh));
d2fdR2all=reshape(fnval(derivR2,[xx(:) yy(:)]'),length(eqdskval.rmesh),length(eqdskval.zmesh));
d2fdZ2all=reshape(fnval(derivZ2,[xx(:) yy(:)]'),length(eqdskval.rmesh),length(eqdskval.zmesh));
% idem for spline interpolation
dfdRall0=reshape(fnval(deriv0R,[xx(:) yy(:)]'),length(eqdskval.rmesh),length(eqdskval.zmesh));
d2fdR2all0=reshape(fnval(deriv0R2,[xx(:) yy(:)]'),length(eqdskval.rmesh),length(eqdskval.zmesh));
d2fdZ2all0=reshape(fnval(deriv0Z2,[xx(:) yy(:)]'),length(eqdskval.rmesh),length(eqdskval.zmesh));
% idem for 2x1D method
dfdRall2x1D=reshape(fnval(deriv2x1D0R,[xx(:) yy(:)]'),length(eqdskval.rmesh),length(eqdskval.zmesh));
d2fdR2all2x1D=reshape(fnval(deriv2x1D0R2,[xx(:) yy(:)]'),length(eqdskval.rmesh),length(eqdskval.zmesh));
d2fdZ2all2x1D=reshape(fnval(deriv2x1D0Z2,[xx(:) yy(:)]'),length(eqdskval.rmesh),length(eqdskval.zmesh));

psisp_onR=fnval(sp,Ronzaxis);
psisp0_onR=fnval(sp0,Ronzaxis);
psisp2x1D0_onR=fnval(sp2x1D0,Ronzaxis);

psisp_onZ=fnval(sp,Zonraxis);
psisp0_onZ=fnval(sp0,Zonraxis);
psisp2x1D0_onZ=fnval(sp2x1D0,Zonraxis);

figure
% psi along z=zaxis
plot(eqdskval.rmesh,eqdskval.psi(:,iz),'b')
hold on
plot(eqdskval.rmesh,psisp_onR,'r')
fff=fnval(sp0,Ronzaxis);
plot(eqdskval.rmesh,fff,'c--')
fff=fnval(sp2x1D0,Ronzaxis);
plot(eqdskval.rmesh,fff,'g-.')
plot(eqdskval.rmesh,psi2x1Dos_2(:,iz),'k-.')
legend('psi','tol2D','0 spline','2x1D matlab','2x1D OS')
title('psi along R(z=zaxis)')

figure
% psi along R=Raxis
plot(eqdskval.zmesh,eqdskval.psi(ir,:),'b')
hold on
plot(eqdskval.zmesh,psisp_onZ,'r')
fff=fnval(sp0,Zonraxis);
plot(eqdskval.zmesh,fff,'c--')
fff=fnval(sp2x1D0,Zonraxis);
plot(eqdskval.zmesh,fff,'g-.')
plot(eqdskval.zmesh,psi2x1Dos_2(ir,:),'k-.')
legend('psi','tol2D','0 spline','2x1D matlab','2x1D OS')
title('psi along Z(r=raxis)')

figure
psicont=[linspace(eqdskval.psiaxis+5e-3*(eqdskval.psiedge-eqdskval.psiaxis),eqdskval.psiedge-0.1*(eqdskval.psiedge-eqdskval.psiaxis),20) eqdskval.psiedge linspace(eqdskval.psiedge+0.1*(eqdskval.psiedge-eqdskval.psiaxis),eqdskval.psiedge-0.5*(eqdskval.psiedge-eqdskval.psiaxis),5)];
contour(eqdskval.rmesh,eqdskval.zmesh,eqdskval.psi',psicont);
hold on
contour(eqdskval.rmesh,eqdskval.zmesh,fspall',psicont,'--');
contour(eqdskval.rmesh,eqdskval.zmesh,psi2x1D_2',psicont,'-.');
contour(eqdskval.rmesh,eqdskval.zmesh,psi2x1Dos_2',psicont,':');
contour(eqdskval.rmesh,eqdskval.zmesh,psi2x1Dos_2',[eqdskval.psiedge eqdskval.psiedge],'k--');
title('psi orig (solid), 2D smooth (dashed), 2x1D smoothed -., OS :')

axis equal

figure
% dpsi/dR along z=zaxis
fffdR=fnval(deriv0R,Ronzaxis);
plot(eqdskval.rmesh,fffdR,'b')
hold on
fffdR=fnval(derivR,Ronzaxis);
plot(eqdskval.rmesh,fffdR,'r')
f1=csaps(eqdskval.rmesh,psisp0_onR,1-tension1D);
df1dR=fnder(f1,[1 0]);
fffdR=fnval(df1dR,eqdskval.rmesh);
plot(eqdskval.rmesh,fffdR,'m--')
fffdR=fnval(deriv2x1D0R,Ronzaxis);
plot(eqdskval.rmesh,fffdR,'g-.')
plot(eqdskval.rmesh,dpsidR2x1Dos_2(:,iz),'k-.')
plot(eqdskval.rmesh,dpsidR2x1Dos_2b(:,iz),'c:')
legend('0 spline','tol2D','1D on 0spline matlab','2x1D matlab','2x1D OS','2x1D OSb')

title('dpsi/dR along R(z=zaxis)')

figure
% dpsi/dZ along z=zaxis
fffdZ=fnval(deriv0Z,Ronzaxis);
plot(eqdskval.rmesh,fffdZ,'b')
hold on
fffdZ=fnval(derivZ,Ronzaxis);
plot(eqdskval.rmesh,fffdZ,'r')
fffdZ=fnval(deriv2x1D0Z,Ronzaxis);
plot(eqdskval.rmesh,fffdZ,'g-.')
plot(eqdskval.rmesh,dpsidZ2x1Dos_2(:,iz),'k:.')
legend('0 spline','tol2D','2x1D matlab','2x1D OS')

title('dpsi/dZ along R(z=zaxis)')

figure
% d2psi/dR2 along z=zaxis
fffdR2=fnval(deriv0R2,Ronzaxis);
plot(eqdskval.rmesh,fffdR2,'b')
hold on
fffdR2=fnval(derivR2,Ronzaxis);
plot(eqdskval.rmesh,fffdR2,'r')
fffdR2=fnval(deriv2x1D0R2,Ronzaxis);
plot(eqdskval.rmesh,fffdR2,'g--')
fffdR2=fnval(deriv2x1D0R2_b,Ronzaxis);
plot(eqdskval.rmesh,fffdR2,'c-.')
plot(eqdskval.rmesh,d2psidR22x1Dos_2(:,iz),'k:')
legend('0 spline','tol2D','2x1D matlab','2x1D_b matlab','2x1D OS')

title('d2psi/dR2 along R(z=zaxis)')

figure
% d2psi/dZ2 along z=zaxis
fffdZ2=fnval(deriv0Z2,Ronzaxis);
plot(eqdskval.rmesh,fffdZ2,'b')
hold on
fffdZ2=fnval(derivZ2,Ronzaxis);
plot(eqdskval.rmesh,fffdZ2,'r')
fffdZ2=fnval(deriv2x1D0Z2,Ronzaxis);
plot(eqdskval.rmesh,fffdZ2,'g--')
fffdZ2=fnval(deriv2x1D0Z2_b,Ronzaxis);
plot(eqdskval.rmesh,fffdZ2,'c-.')
plot(eqdskval.rmesh,d2psidZ22x1Dos_2(:,iz),'k:')
legend('0 spline','tol2D','2x1D matlab','2x1D_b matlab','2x1D OS')

title('d2psi/dZ2 along R(z=zaxis)')

figure
% d2psi/dZ2 along R=Raxis
fffdZ2=fnval(deriv0Z2,Zonraxis);
plot(eqdskval.zmesh,fffdZ2,'b')
hold on
fffdZ2=fnval(derivZ2,Zonraxis);
plot(eqdskval.zmesh,fffdZ2,'r')
fffdZ2=fnval(deriv2x1D0Z2,Zonraxis);
plot(eqdskval.zmesh,fffdZ2,'g--')
fffdZ2=fnval(deriv2x1D0Z2_b,Zonraxis);
plot(eqdskval.zmesh,fffdZ2,'c-.')
plot(eqdskval.zmesh,d2psidZ22x1Dos_2(ir,:),'k:')
legend('0 spline','tol2D','2x1D matlab','2x1D_b matlab','2x1D OS')

title('d2psi/dZ2 along Z(r=raxis)')

figure
% d2psi/dRZ along z=zaxis
fffdRZ=fnval(deriv0RZ,Ronzaxis);
plot(eqdskval.rmesh,fffdRZ,'b')
hold on
fffdRZ=fnval(derivRZ,Ronzaxis);
plot(eqdskval.rmesh,fffdRZ,'r')
fffdRZ=fnval(deriv2x1D0RZ,Ronzaxis);
plot(eqdskval.rmesh,fffdRZ,'g--')
fffdRZ=fnval(deriv2x1D0RZ_b,Ronzaxis);
plot(eqdskval.rmesh,fffdRZ,'c-.')
plot(eqdskval.rmesh,d2psidRZ2x1Dos_2(:,iz),'r:')
legend('0 spline','tol2D','2x1D matlab','2x1D_b matlab','2x1D OS')

title('d2psi/dRZ along R(z=zaxis)')

% compute Grad-Shafranov terms
% find pprime and ttprime on r,z mesh
pprimeRZ=csaps(eqdskval.psimesh.*(eqdskval.psiedge-eqdskval.psiaxis)+eqdskval.psiaxis,eqdskval.pprime,1,psi2x1D_2);
ij=find((psi2x1D_2-eqdskval.psiaxis)./(eqdskval.psiedge-eqdskval.psiaxis)>1);
pprimeRZ(ij)=0.;
FFprimeRZ=csaps(eqdskval.psimesh.*(eqdskval.psiedge-eqdskval.psiaxis)+eqdskval.psiaxis,eqdskval.FFprime,1,psi2x1D_2);
FFprimeRZ(ij)=0.;
RRR=eqdskval.rmesh'*ones(1,length(eqdskval.zmesh));
mu0=4e-7.*pi;
rjphi=-RRR.^2.*mu0.*pprimeRZ-FFprimeRZ;

gradshaf_sp2D=d2fdR2all -1./RRR.*dfdRall + d2fdZ2all;
gradshaf_sp0=d2fdR2all0 -1./RRR.*dfdRall0 + d2fdZ2all0;
gradshaf_sp2x1D=d2fdR2all2x1D -1./RRR.*dfdRall2x1D + d2fdZ2all2x1D;
gradshaf_2x1Dos=d2psidR22x1Dos_2 -1./RRR.*dpsidR2x1Dos_2 + d2psidZ22x1Dos_2;

figure
plot(eqdskval.rmesh,rjphi(:,iz),'k-')
hold on
plot(eqdskval.rmesh,gradshaf_sp2D(:,iz),'r-')
plot(eqdskval.rmesh,gradshaf_sp0(:,iz),'b-')
plot(eqdskval.rmesh,gradshaf_sp2x1D(:,iz),'c-')
plotos(eqdskval.rmesh,gradshaf_2x1Dos(:,iz),'-',0,0,colos(4,:));
legend('rjphi source','tol2D','0spline','2x1D matlab','2x1D OS')
title('Grad-Shafranov and source vs R along z=zaxis')

figure
plot(eqdskval.zmesh,rjphi(ir,:),'k')
hold on
plot(eqdskval.zmesh,gradshaf_sp2D(ir,:),'r-')
plot(eqdskval.zmesh,gradshaf_sp0(ir,:),'b-')
plot(eqdskval.zmesh,gradshaf_sp2x1D(ir,:),'c-')
plotos(eqdskval.zmesh,gradshaf_2x1Dos(ir,:),'-',0,0,colos(4,:));
legend('rjphi source','tol2D','0spline','2x1D matlab','2x1D OS')
title('Grad-Shafranov and source vs Z along R=Raxis')

figure
plot(eqdskval.rmesh,gradshaf_sp2D(:,iz)-rjphi(:,iz),'r-')
hold on
plot(eqdskval.rmesh,gradshaf_sp0(:,iz)-rjphi(:,iz),'b-')
plot(eqdskval.rmesh,gradshaf_sp2x1D(:,iz)-rjphi(:,iz),'c-')
plotos(eqdskval.rmesh,gradshaf_2x1Dos(:,iz)-rjphi(:,iz),'-',0,0,colos(4,:));
legend('tol2D','0spline','2x1D matlab','2x1D OS')
title('Error: Grad-Shafranov-source vs R along z=zaxis')
aa=axis;
axis([aa(1) aa(2) -0.03 +0.03])

figure
plot(eqdskval.zmesh,gradshaf_sp2D(ir,:)-rjphi(ir,:),'r-')
hold on
plot(eqdskval.zmesh,gradshaf_sp0(ir,:)-rjphi(ir,:),'b-')
plot(eqdskval.zmesh,gradshaf_sp2x1D(ir,:)-rjphi(ir,:),'c-')
plotos(eqdskval.zmesh,gradshaf_2x1Dos(ir,:)-rjphi(ir,:),'-',0,0,colos(4,:));
legend('tol2D','0spline','2x1D matlab','2x1D OS')
title('Error: Grad-Shafranov-source vs Z along R=Raxis')
bb=axis;
axis([bb(1) bb(2) -0.03 +0.03])

saveeqdsk=input('save new eqdsk: (1 for yes, return for no) ','s');

if saveeqdsk
  fnamenew=[fname '_smoothed'];
  eqdskvalnew=eqdskval;
  eqdskvalnew.psi = psi2x1Dos_2;
  eqdskvalnew.psirz = reshape(psi2x1Dos_2,size(eqdskval.psirz));
  extracomments{1}='psi(r,z) smoothed with test_eqdsk, using 1Dx1D cubic splines with tension';
  extracomments{2}=['tension1D = ' num2str(tension1D)];
  write_eqdsk(fnamenew,eqdskvalnew,[],[],[],extracomments);
end

