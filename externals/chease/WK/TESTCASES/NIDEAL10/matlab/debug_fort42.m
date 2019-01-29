set_defaults_matlab

iload=1;
if iload
  fid=fopen('fort.42','r');
  [aa,acount]=fscanf(fid,'%d',4);
  k=aa(1);
  nt2=aa(2);
  niso1eff=aa(3);
  igmax=aa(4);
  fclose(fid);
  fid=fopen('fort.42','r');
  clear ztet bchin zbetchi_orig zbetchi_os zthsha zsha_orig zsha_os
  for is=1:niso1eff
    [aa,acount]=fscanf(fid,'%d',4);
    k=aa(1);
    if k~=is; disp(['problem with k=' num2str(k) ' , is= ' num2str(is)]); end
    [ztet(1:nt2,is),acount]=fscanf(fid,'%f',nt2);
    [bchin(1:nt2,is),acount]=fscanf(fid,'%f',nt2);
    [tetpsi(1:igmax,is),acount]=fscanf(fid,'%f',igmax);
    [zbetchi_orig(1:igmax,is),acount]=fscanf(fid,'%f',igmax);
    [zbetchi_os(1:igmax,is),acount]=fscanf(fid,'%f',igmax);
    [zthsha(1:nt2,is),acount]=fscanf(fid,'%f',nt2);
    [zsha_orig(1:igmax,is),acount]=fscanf(fid,'%f',igmax);
    nt1=nt2-1;
    [ztet2eff(1:3*nt1,is),acount]=fscanf(fid,'%f',3*nt1);
    [zthsha2(1:3*nt1,is),acount]=fscanf(fid,'%f',3*nt1);
    [zsha_os(1:igmax,is),acount]=fscanf(fid,'%f',igmax);
    [pgwgt(1:igmax,is),acount]=fscanf(fid,'%f',igmax);
    [zb2(1:igmax,is),acount]=fscanf(fid,'%f',igmax);
    [zdfm(1:igmax,is),acount]=fscanf(fid,'%f',igmax);
    [aaa(1:2),acount]=fscanf(fid,'%f',2);
    rib(is) = aaa(1);
    rib2(is) = aaa(2);
  end
end

%break

ifig=0;
ifig=ifig+1;figure(ifig);clf
plot(ztet,bchin,'*')
hold on
tetpsi2=tetpsi;
ij=find(tetpsi2<1e-3);
tetpsi2(ij)=tetpsi2(ij)+2*pi;
plot(tetpsi2,zbetchi_orig,'-')

ifig=ifig+1;figure(ifig);clf
plot(ztet,bchin,'*')
hold on
plot(tetpsi2,zbetchi_os,'-')

ifig=ifig+1;figure(ifig);clf
plot(tetpsi2,zbetchi_orig-zbetchi_os,'-')

ifig=ifig+1;figure(ifig);clf
plot(ztet,zthsha,'*')
hold on
plot(tetpsi2,zsha_orig,'-')

ifig=ifig+1;figure(ifig);clf
plot(ztet2eff,zthsha2)

ifig=ifig+1;figure(ifig);clf
plot(ztet,zthsha,'*')
hold on
plot(tetpsi2,zsha_os,'-')

ifig=ifig+1;figure(ifig);clf
plot(tetpsi2,zsha_orig-zsha_os,'-')

ifig=ifig+1;figure(ifig);clf
plot(zsha_os(:,end-5:end),zb2(:,end-5:end),'-')

ifig=ifig+1;figure(ifig);clf
plot(zsha_os(:,end-5:end),zdfm(:,end-5:end),'*-')

for k=1:niso1eff
  for jfm=1:6
    zfm1 = sum((jfm).*cos((jfm).*zsha_os(:,k)).*sqrt(zb2(:,k)).*log(sqrt(zb2(:,k))).*zdfm(:,k));
    zfm2 = sum((jfm).*cos((jfm).*zsha_os(:,k)).*(zb2(:,k)).*zdfm(:,k));
    zfm3 = sum((jfm).*sin((jfm).*zsha_os(:,k)).*sqrt(zb2(:,k)).*log(sqrt(zb2(:,k))).*zdfm(:,k));
    zfm4 = sum((jfm).*sin((jfm).*zsha_os(:,k)).*(zb2(:,k)).*zdfm(:,k));
    fm(jfm,k)= 8.*pi.^2*(zfm1*zfm2+zfm3*zfm4)/rib(k).^3./rib2(k);
  end
end
