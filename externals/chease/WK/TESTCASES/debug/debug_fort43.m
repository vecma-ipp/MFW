set_defaults_matlab

iload=1;
if iload
  fid=fopen('fort.43','r');
  [aa,acount]=fscanf(fid,'%d',2);
  nrbox=aa(1);
  nzbox=aa(2);
  [psimax,acount]=fscanf(fid,'%f',1);
  [chiRZ_1d,acount]=fscanf(fid,'%f',nrbox*nzbox);
  [smisoRZ_1d,acount]=fscanf(fid,'%f',nrbox*nzbox);

  clear chiRZ smisoRZ
  for iz=1:nzbox
    chiRZ(1:nrbox,iz) = chiRZ_1d([(iz-1)*nrbox+1:(iz-1)*nrbox+nrbox]);
    smisoRZ(1:nrbox,iz) = smisoRZ_1d([(iz-1)*nrbox+1:(iz-1)*nrbox+nrbox]);
  end

end

eqds=readeqdsk('EQDSK_COCOS_02_POS.OUT',2,1);
plot_eqdsk(eqds);

minpsi=min(min(eqds.psi));
maxpsi=max(max(eqds.psi));
zzval=linspace(minpsi,maxpsi,100);

psiRZ = smisoRZ.^2. .* (eqds.psiedge - eqds.psiaxis) + eqds.psiaxis;
figure
contour(eqds.rmesh,eqds.zmesh,eqds.psi',zzval)
hold on
plot(eqds.rplas,eqds.zplas,'k')
contour(eqds.rmesh,eqds.zmesh,psiRZ',zzval,'--')
axis equal

figure
plot(eqds.rplas,eqds.zplas,'k')
hold on
contour(eqds.rmesh,eqds.zmesh,chiRZ',100,'-')
axis equal
