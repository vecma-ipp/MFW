set_defaults_matlab

iload=1;
if iload
  fid=fopen('fort.31','r');
  [kp,acount]=fscanf(fid,'%d',1);
  [psip,acount]=fscanf(fid,'%f',1);
  [aa,acount]=fscanf(fid,'%d',3);
  npsi=aa(1);
  nchi=aa(2);
  nbps=aa(3);
  fclose(fid);
  
  fid=fopen('fort.31','r');
  for i=1:npsi
    [kp,acount]=fscanf(fid,'%d',1);
    [psip(kp),acount]=fscanf(fid,'%f',1);
    [aa,acount]=fscanf(fid,'%d',3);
    [ztetchi(1:nchi,kp),acount]=fscanf(fid,'%f',nchi);
    [zsigchi(1:nchi,kp),acount]=fscanf(fid,'%f',nchi);
  end
  [zbnd_npsi(1:nchi),acount]=fscanf(fid,'%f',nchi);
  [tetbps(1:nbps),acount]=fscanf(fid,'%f',nbps);
  [rrbps(1:nbps),acount]=fscanf(fid,'%f',nbps);
  [rzbps(1:nbps),acount]=fscanf(fid,'%f',nbps);
  [d2rbps(1:nbps),acount]=fscanf(fid,'%f',nbps);
  [d2zbps(1:nbps),acount]=fscanf(fid,'%f',nbps);
  [BPS(1),acount]=fscanf(fid,'%f',1);
  [BPS(12),acount]=fscanf(fid,'%f',1);
% $$$   ntest=401;
% $$$   [zthet3,acount]=fscanf(fid,'%f',ntest);
% $$$   [zbnd3,acount]=fscanf(fid,'%f',ntest);
  fclose(fid);
% $$$   fid=fopen('fort.33','r');
% $$$   for i=1:npsi
% $$$     [kp,acount]=fscanf(fid,'%d',1);
% $$$     [aa,acount]=fscanf(fid,'%d',2);
% $$$     [tetchi(1:nchi,kp),acount]=fscanf(fid,'%f',aa(2));
% $$$     [sigchi(1:nchi,kp),acount]=fscanf(fid,'%f',aa(2));
% $$$   end
% $$$   [zbnd3,acount]=fscanf(fid,'%f',nchi);
% $$$   zthet3 = tetchi(:,npsi);
  zthet3=0;
  zbnd3=0.;
% $$$   fclose(fid);
  
  fid=fopen('fort.32','r');
  [aa,acount]=fscanf(fid,'%d',4);
  ns=aa(1);
  nt=aa(2);
  if npsi ~= aa(3) then
    disp('problem with aa(3)')
  end
  if nchi ~= aa(4) then
    disp('problem with aa(4)')
  end

  [cpsicl,acount]=fscanf(fid,'%f',(ns+1)*nt);
  [chin_1d,acount]=fscanf(fid,'%f',(nt+2)*npsi);
  [tetmap_1d,acount]=fscanf(fid,'%f',(nt+2)*npsi);
  [sigmap_1d,acount]=fscanf(fid,'%f',(nt+2)*npsi);
  [smiso,acount]=fscanf(fid,'%f',npsi);
  [chim,acount]=fscanf(fid,'%f',nchi);
  [chi,acount]=fscanf(fid,'%f',nchi+1);
  [csig,acount]=fscanf(fid,'%f',ns+1);
  [ct,acount]=fscanf(fid,'%f',nt+1);
  [rhos,acount]=fscanf(fid,'%f',nt+1);
  [R0,acount]=fscanf(fid,'%f',1);
  [RZ0,acount]=fscanf(fid,'%f',1);
  fclose(fid);

  clear psi_sigtet Rst Zst
  for is=1:ns+1
    psi_sigtet(is,1:nt) = cpsicl([(is-1)*nt+1:(is-1)*nt+nt]);
    Rst(is,1:nt+1) = csig(is) .* rhos(:)' .* cos(ct(:)') + R0;
    Zst(is,1:nt+1) = csig(is) .* rhos(:)' .* sin(ct(:)') + RZ0;
  end
  clear sigmap tetmap
  for is=1:npsi
    chin(is,1:nt+2)=chin_1d((is-1)*(nt+2)+1:(is-1)*(nt+2)+nt+2);
    sigmap(is,1:nt+2)=sigmap_1d((is-1)*(nt+2)+1:(is-1)*(nt+2)+nt+2);
    tetmap(is,1:nt+2)=tetmap_1d((is-1)*(nt+2)+1:(is-1)*(nt+2)+nt+2);
  end
end

%break
figure(1);clf
plot(Rst',Zst','*-')
axis equal

figure(2);clf
contour(csig,180/pi*ct(1:end-1),psi_sigtet',60);

% construct R,Z of (psi,chi) points using sigmap(psi,chi),tetmap(psi,chi)
clear rhos_tetmap Rpsichi Zpsichi
for is=1:npsi
  [rhos_tetmap(is,:)]=interpos(ct(1:end-1),rhos(1:end-1),tetmap(is,1:nt+2),-0.0,[-1 -1],[2*pi 2*pi]);
  Rpsichimap(is,1:nt+2) = sigmap(is,1:nt+2) .* rhos_tetmap(is,:) .* cos(tetmap(is,1:nt+2)) + R0;
  Zpsichimap(is,1:nt+2) = sigmap(is,1:nt+2) .* rhos_tetmap(is,:) .* sin(tetmap(is,1:nt+2)) + RZ0;
end
figure(3);clf
plot(Rpsichimap',Zpsichimap','-')
axis equal

% construct R,Z of (psi,chi) points using sigchi(psi,chi),tetchi(psi,chi)
clear rhos_ztetchi Rpsichi Zpsichi
ij=find(ztetchi(1:nchi,npsi)<0);
zteteff=ztetchi(1:nchi,npsi);
%zteteff(ij) = zteteff(ij) + 2*pi;
for is=1:npsi
  [rhos_ztetchi(is,:)]=interpos(zteteff,zbnd_npsi(1:nchi),ztetchi(1:nchi,is),-0.00,[-1 -1],[2*pi 2*pi]);
  Rpsichi(is,1:nchi) = zsigchi(1:nchi,is)' .* rhos_ztetchi(is,1:nchi) .* cos(ztetchi(1:nchi,is)') + R0;
  Zpsichi(is,1:nchi) = zsigchi(1:nchi,is)' .* rhos_ztetchi(is,:) .* sin(ztetchi(1:nchi,is)') + RZ0;
end
figure(4);clf
plot(Rpsichi',Zpsichi','*')
axis equal
hold on
eqdsk=readeqdsk('EQDSK_COCOS_02_POS.OUT',2,1);
contour(eqdsk.rmesh./eqdsk.r0,eqdsk.zmesh./eqdsk.r0,(eqdsk.psi-eqdsk.psiaxis)'./(eqdsk.psiedge-eqdsk.psiaxis)',psip.^2,'-')

figure(5);clf
plot(Rpsichi',Zpsichi','-')
axis equal
hold on
% eqdsk=readeqdsk('EQDSK_COCOS_02_POS.OUT',2,1);
contour(eqdsk.rmesh./eqdsk.r0,eqdsk.zmesh./eqdsk.r0,(eqdsk.psi-eqdsk.psiaxis)'./(eqdsk.psiedge-eqdsk.psiaxis)',psip.^2,'-')

figure(6);clf
plot(eqdsk.rplas./eqdsk.r0,eqdsk.zplas./eqdsk.r0,'k')
hold on
plot(Rpsichi(end,:),Zpsichi(end,:),'r*-')
plot(Rpsichimap(end,:),Zpsichimap(end,:),'-o')
plot(rrbps,rzbps,'m-s')
axis equal

% test bound
figure(7);clf
rhoeqdsk = sqrt((eqdsk.rplas./eqdsk.r0-R0).^2+(eqdsk.zplas./eqdsk.r0-RZ0).^2);
theteqdsk = atan2(eqdsk.zplas./eqdsk.r0-RZ0,eqdsk.rplas./eqdsk.r0-R0);
plot(theteqdsk,rhoeqdsk,'k')
hold on
rhopsichi = sqrt((Rpsichi(end,:)-R0).^2+(Zpsichi(end,:)-RZ0).^2);
thetpsichi = atan2(Zpsichi(end,:)-RZ0,Rpsichi(end,:)-R0);
plot(thetpsichi,rhopsichi,'r*-')
rhobps=sqrt((rrbps-R0).^2+(rzbps-RZ0).^2);
thetbps=atan2(rzbps-RZ0,rrbps-R0);
plot(thetbps(1:end-1),rhobps(1:end-1),'m-s')
plot(theteqdsk,rhoeqdsk,'k')
axis([-0.4 0.2 0.3 0.31])

figure(8);clf
ij=find(thetbps<0);
thetbps(ij)=thetbps(ij)+2*pi;
plot(thetbps(1:end-1),d2rbps(1:end-1),'m-s')
hold on
[aa,aa1,aa2]=interpos(thetbps(1:end-1),rrbps(1:end-1),thetbps(1:end-1),-0.00,[-1 -1],[2*pi 2*pi]);
plot(thetbps(1:end-1),aa2,'--')

figure(9);clf
plot(thetbps(1:end),d2zbps(1:end),'c-*')
hold on
[aa,aa1,aa2]=interpos(thetbps(1:end-1),rzbps(1:end-1),thetbps(1:end-1),-0.00,[-1 -1],[2*pi 2*pi]);
plot(thetbps(1:end-1),aa2,'--')

figure(10);clf
plot(zthet3,zbnd3,'c-*')
hold on
[zbnd3bis,aa1,aa2]=interpos(thetbps(1:end-1),rhobps(1:end-1),zthet3,-0.00,[-1 -1],[2*pi 2*pi]);
plot(zthet3,zbnd3bis,'ko-')

figure(11);clf
plot(eqdsk.rplas./eqdsk.r0,eqdsk.zplas./eqdsk.r0,'b-')
hold on
plot(R0+zbnd3bis'.*cos(zthet3),RZ0+zbnd3bis'.*sin(zthet3),'r--')

% find psi and chi values on R, Z mesh points from thetchi, sigchi, rhobound
clear rhoRZmesh thetaRZmesh
for iR=1:eqdsk.nr
  rhoRZmesh(iR,1:eqdsk.nz)=sqrt((eqdsk.rmesh(iR)-eqdsk.raxis).^2+(eqdsk.zmesh(:)-eqdsk.zmid).^2);
  thetaRZmesh(iR,1:eqdsk.nz)=atan2(eqdsk.zmesh(:)-eqdsk.zmid,eqdsk.rmesh(iR)-eqdsk.raxis);
end
rhoRZmesh = rhoRZmesh./eqdsk.r0;

ztetchi_pos=ztetchi;
ij=find(ztetchi_pos<0.);
ztetchi_pos(ij) = ztetchi_pos(ij) + 2.*pi;

ztetchi_pos=ztetchi;

% break

% $$$ tic
% $$$ clear rho_bound_RZ sig_thetaRZ chim2_thetaRZ smiso_thetaRZ chim_thetaRZ
% $$$ for iR=1:eqdsk.nr
% $$$   %iR=floor(eqdsk.nr/2)+30;
% $$$   for iZ=1:eqdsk.nz
% $$$     %iZ=floor(eqdsk.nz/2)+30;
% $$$     % rho_bound at theta(R,Z)
% $$$     rho_bound_RZ(iR,iZ)=interpos(thetbps(1:end-1),rhobps(1:end-1),thetaRZmesh(iR,iZ),0,[-1 -1],[2*pi 2*pi]);
% $$$     if rhoRZmesh(iR,iZ)./rho_bound_RZ(iR,iZ) <= 1.
% $$$       % do 2D spline to find psi and chi values on R,Z
% $$$       % find all sigma values on psi surfaces and theta=theta(R,Z)
% $$$       for ipsi=1:npsi-1
% $$$         sig_thetaRZ(iR,iZ,ipsi)=interpos(ztetchi_pos(:,ipsi),zsigchi(:,ipsi),thetaRZmesh(iR,iZ),0,[-1 -1],[2*pi 2*pi]);
% $$$         chim2_thetaRZ(iR,iZ,ipsi)=interpos([ztetchi_pos(:,ipsi)-2.*pi; ztetchi_pos(:,ipsi); ztetchi_pos(:,ipsi)+2*pi], ...
% $$$           [chim-2.*pi ; chim ; chim+2.*pi],thetaRZmesh(iR,iZ),0);
% $$$       end
% $$$       ipsi=npsi;
% $$$       sig_thetaRZ(iR,iZ,ipsi)=1.;
% $$$       chim2_thetaRZ(iR,iZ,ipsi)=interpos([ztetchi_pos(:,ipsi)-2.*pi; ztetchi_pos(:,ipsi); ztetchi_pos(:,ipsi)+2*pi], ...
% $$$           [chim-2.*pi ; chim ; chim+2.*pi],thetaRZmesh(iR,iZ),0);
% $$$       % find psi value
% $$$       smiso_thetaRZ(iR,iZ) = interpos([0.; squeeze(sig_thetaRZ(iR,iZ,:))],[0.; smiso], ...
% $$$           rhoRZmesh(iR,iZ)./rho_bound_RZ(iR,iZ),-0.001,[2 2],[0. smiso(npsi)]);
% $$$       % find chim value
% $$$       chim_thetaRZ(iR,iZ) = interpos([0.; squeeze(sig_thetaRZ(iR,iZ,:))],[chim2_thetaRZ(iR,iZ,1); squeeze(chim2_thetaRZ(iR,iZ,:))], ...
% $$$           rhoRZmesh(iR,iZ)./rho_bound_RZ(iR,iZ),-0.001,[1 2],[0. chim2_thetaRZ(iR,iZ,npsi)]);
% $$$     else
% $$$       smiso_thetaRZ(iR,iZ) = 1.1;
% $$$       chim_thetaRZ(iR,iZ) = NaN;
% $$$     end
% $$$   end
% $$$ end
% $$$ toc

%!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
tic
for iR=1:eqdsk.nr
  thetaRZmesh_1d((iR-1)*eqdsk.nz+[1:eqdsk.nz]) = thetaRZmesh(iR,1:eqdsk.nz)';
end
clear rho_bound_RZ_1d sig_thetaRZ chim2_thetaRZ smiso_thetaRZ chim_thetaRZ
% for iR=1:eqdsk.nr
  %iR=floor(eqdsk.nr/2)+30;
%  for iZ=1:eqdsk.nz
%iZ=floor(eqdsk.nz/2)+30;
% rho_bound at theta(R,Z)
rho_bound_RZ_1d=interpos(thetbps(1:end-1),rhobps(1:end-1),thetaRZmesh_1d,0,[-1 -1],[2*pi 2*pi]);
%if rhoRZmesh(iR,iZ)./rho_bound_RZ(iR,iZ) <= 1.
% do 2D spline to find psi and chi values on R,Z
% find all sigma values on psi surfaces and theta=theta(R,Z)

for ipsi=1:npsi-1
  sig_thetaRZ(ipsi,:)=interpos(ztetchi_pos(:,ipsi),zsigchi(:,ipsi),thetaRZmesh_1d,0,[-1 -1],[2*pi 2*pi]);
  chim2_thetaRZ(ipsi,:)=interpos([ztetchi_pos(:,ipsi)-2.*pi; ztetchi_pos(:,ipsi); ztetchi_pos(:,ipsi)+2*pi], ...
          [chim-2.*pi ; chim ; chim+2.*pi],thetaRZmesh_1d,0);
end
ipsi=npsi;
sig_thetaRZ(ipsi,:)=1.;
chim2_thetaRZ(ipsi,:)=interpos([ztetchi_pos(:,ipsi)-2.*pi; ztetchi_pos(:,ipsi); ztetchi_pos(:,ipsi)+2*pi], ...
          [chim-2.*pi ; chim ; chim+2.*pi],thetaRZmesh_1d,0);
for iR=1:eqdsk.nr
  for iZ=1:eqdsk.nz
    iRZ = (iR-1)*eqdsk.nz+iZ;
    % find psi value
    smiso_thetaRZ(iR,iZ) = interpos(13,[0.; squeeze(sig_thetaRZ(:,iRZ))],[0.; smiso], ...
          rhoRZmesh(iR,iZ)./rho_bound_RZ_1d(iRZ),-0.001,[2 2],[0. smiso(npsi)]);
    % find chim value
    chim_thetaRZ(iR,iZ) = interpos(63,[squeeze(sig_thetaRZ(:,iRZ))],[squeeze(chim2_thetaRZ(:,iRZ))], ...
          rhoRZmesh(iR,iZ)./rho_bound_RZ_1d(iRZ),-0.001,[0 2],[0. chim2_thetaRZ(npsi,iRZ)]);
  end
end

toc

figure(12);clf
contour(eqdsk.rmesh,eqdsk.zmesh,smiso_thetaRZ',100);
hold on
plot(eqdsk.rplas,eqdsk.zplas,'k');
axis equal

figure(13);clf
contour(eqdsk.rmesh,eqdsk.zmesh,chim_thetaRZ',100);
hold on
plot(eqdsk.rplas,eqdsk.zplas,'k');
axis equal
