set_defaults_matlab

iload=1;
if iload
  fid=fopen('fort.41','r');
  [aa,acount]=fscanf(fid,'%d',7);
  nchi=aa(1);
  niso1eff=aa(2);
  niso1eff1=aa(3);
  nt=aa(4);
  nmgaus=aa(5);
  nt2=aa(6);
  nt1=aa(7);
  [grid_s(1:nchi),acount]=fscanf(fid,'%f',nchi);
  [dzetadpsi_1d,acount]=fscanf(fid,'%f',niso1eff*nchi);
  [dzetadchi_1d,acount]=fscanf(fid,'%f',niso1eff*nchi);
  [g11_prev_1d,acount]=fscanf(fid,'%f',niso1eff*nchi);
  [g12_prev_1d,acount]=fscanf(fid,'%f',niso1eff*nchi);
  [g11_1d,acount]=fscanf(fid,'%f',niso1eff*nchi);
  [g12_1d,acount]=fscanf(fid,'%f',niso1eff*nchi);
  [dqdpsi_chk(1:niso1eff),acount]=fscanf(fid,'%f',niso1eff);
  [xin_psi(1:niso1eff1),acount]=fscanf(fid,'%f',niso1eff1);
  [yin_amin(1:niso1eff1),acount]=fscanf(fid,'%f',niso1eff1);
  [xout_psi_noaxis(1:niso1eff),acount]=fscanf(fid,'%f',niso1eff);
  [damindpsi(1:niso1eff),acount]=fscanf(fid,'%f',niso1eff);
  [d2amindpsi2(1:niso1eff),acount]=fscanf(fid,'%f',niso1eff);
  [djacdpsi(1:niso1eff),acount]=fscanf(fid,'%f',niso1eff);
  [dfdpsi(1:niso1eff),acount]=fscanf(fid,'%f',niso1eff);
  [tetmap_1d,acount]=fscanf(fid,'%f',nt2*niso1eff);
  [bchin_1d,acount]=fscanf(fid,'%f',nt2*niso1eff);
  [tetpsi_1d,acount]=fscanf(fid,'%f',nt1*nmgaus*niso1eff);
  fclose(fid);

  clear dzetadpsi dzetadchi dzetadpsi2 g11_prev g12_prev g11 g12
  for ichi=1:nchi
    dzetadpsi(:,ichi) = dzetadpsi_1d([(ichi-1)*niso1eff+1:(ichi-1)*niso1eff+niso1eff]);
    dzetadchi(:,ichi) = dzetadchi_1d([(ichi-1)*niso1eff+1:(ichi-1)*niso1eff+niso1eff]);
    g11_prev(:,ichi) = g11_prev_1d([(ichi-1)*niso1eff+1:(ichi-1)*niso1eff+niso1eff]);
    g12_prev(:,ichi) = g12_prev_1d([(ichi-1)*niso1eff+1:(ichi-1)*niso1eff+niso1eff]);
    g11(:,ichi) = g11_1d([(ichi-1)*niso1eff+1:(ichi-1)*niso1eff+niso1eff]);
    g12(:,ichi) = g12_1d([(ichi-1)*niso1eff+1:(ichi-1)*niso1eff+niso1eff]);
  end
  dzetadpsi2 = dzetadpsi;
  dzetadpsi2(:,nchi+1) = dqdpsi_chk;
  for is=1:niso1eff
    tetmap(:,is) = tetmap_1d([(is-1)*nt2+1:(is-1)*nt2+nt2]);
    bchin(:,is) = bchin_1d([(is-1)*nt2+1:(is-1)*nt2+nt2]);
    tetpsi(:,is) = tetpsi_1d([(is-1)*nmgaus*nt1+1:(is-1)*nt1*nmgaus+nt1*nmgaus]);
  end
end
grid_s_p1 = [grid_s  1.];
break
ifig=0;
ifig=ifig+1;figure(ifig);clf
plot(grid_s,dzetadpsi,'*')
hold on
plot(1.,dqdpsi_chk,'s')
plot(grid_s_p1,dzetadpsi2,'-')

ifig=ifig+1;figure(ifig);clf
plot(grid_s,dzetadchi,'*')

ifig=ifig+1;figure(ifig);clf
plot(xin_psi,yin_amin);
hold on
[a1,da1,da2]=interpos(12,xin_psi,yin_amin,xout_psi_noaxis,-0.1,[2 2],[yin_amin(1) yin_amin(end)]);
plot(xout_psi_noaxis,a1,'r--')

ifig=ifig+1;figure(ifig);clf
plot(xout_psi_noaxis,damindpsi);
hold on
plot(xout_psi_noaxis,da1,'r--')

ifig=ifig+1;figure(ifig);clf
plot(xout_psi_noaxis,d2amindpsi2);
hold on
plot(xout_psi_noaxis,da2,'r--')

ifig=ifig+1;figure(ifig);clf
plot(djacdpsi);

ifig=ifig+1;figure(ifig);clf
plot(dfdpsi);

ifig=ifig+1;figure(ifig);clf
plot(grid_s,g11,'-')

ifig=ifig+1;figure(ifig);clf
plot(grid_s,g12,'-')

ifig=ifig+1;figure(ifig);clf
plot(grid_s,g11_prev,'-')

ifig=ifig+1;figure(ifig);clf
is=20;
ij=find(tetmap<0);
tetmap2=tetmap;
tetmap2(ij)=tetmap2(ij)+2*pi;
ij=find(tetpsi<0);
tetpsi2=tetpsi;
tetpsi2(ij)=tetpsi2(ij)+2*pi;

plot(tetmap2(1:end-1,is),bchin(1:end-1,is),'*-')
[aa,bb]=interpos(tetmap2(1:end-1,is),bchin(1:end-1,is),tetpsi2(:,is),0.,[-1 -1],[2*pi 2*pi]);
hold on
plot(tetpsi2(:,is),aa,'r--')

ifig=ifig+1;figure(ifig);clf
plot(tetpsi2(:,is),bb,'r-')
[aa0,bb0]=interpos(tetmap2(1:end-1,is),bchin(1:end-1,is),tetpsi2(:,is),0.);
hold on
plot(tetpsi2(:,is),bb0,'c--')

ifig=ifig+1;figure(ifig);clf
plot(tetpsi2(:,is),aa0-aa,'-')
ifig=ifig+1;figure(ifig);clf
plot(tetpsi2(:,is),bb0-bb,'-')
