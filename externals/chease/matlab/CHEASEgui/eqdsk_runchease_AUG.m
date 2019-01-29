
shot=input('shot: ','s');
shot=str2num(shot);
time=input('time: ','s');
time=str2num(time);

gdatpaths
[eqdskAUG, equil_all_t, equil_t_index]=geteqdskAUG(shot,time);
nameeq=['AUG_' num2str(shot) 't' num2str(time)];
write_eqdsk([nameeq '_EQDSK'],eqdskAUG,17);
[fname_out,globalsvalues,namelist_struct,namelist_filename] = run_chease(2,['./' nameeq '_EQDSK' '_COCOS17'],17);
globalsvalues

% make EXPTNZ
equil=gdat(shot,'equil',0);
cxrs=gdat(shot,'cxrs_rho',0);
neterho=gdat(shot,'neterho',0);
ece=gdat(shot,'ece_rho',0);
isempty_ece=isempty(ece.data);

[zz itequil]=min(abs(equil.t-time));
[zz itcxrs]=min(abs(cxrs.t-time));
[zz ithomsoncore]=min(abs(neterho.time_core-time));
[zz ithomsonedge]=min(abs(neterho.time_edge-time));
[zz itece]=min(abs(ece.t-time));
[zz itece_rhos]=min(abs(ece.rhos.t-time));

figure
errorbar(neterho.core_rhopsinorm_on_rztime(:,ithomsoncore),neterho.te_core.value(:,ithomsoncore),neterho.te_core.error(:,ithomsoncore),'*')
aa=axis;
hold on
errorbar(neterho.edge_rhopsinorm_on_rztime(:,ithomsonedge),neterho.te_edge.value(:,ithomsonedge),neterho.te_edge.error(:,ithomsonedge),'s')
if ~isempty_ece; plot(ece.rhos.rhopsinorm_on_rztime(:,itece_rhos),ece.data(:,itece),'mo'); end
legend('VTA\_core','VTA\_edge','ECE')
ylabel('Te')
axis([0 1.1 0 1.2*aa(4)])

rhopol_teall_mix=[neterho.core_rhopsinorm_on_rztime(:,ithomsoncore); neterho.edge_rhopsinorm_on_rztime(:,ithomsonedge)];
teall_mix = [neterho.te_core.value(:,ithomsoncore) ; neterho.te_edge.value(:,ithomsonedge)];
teall_err_mix = [neterho.te_core.error(:,ithomsoncore) ; neterho.te_edge.error(:,ithomsonedge)];
[rhopol_teall_sorted irhopol_teall_sorted]=sort(rhopol_teall_mix);
teall_sorted=teall_mix(irhopol_teall_sorted);
teall_err_sorted = teall_err_mix(irhopol_teall_sorted);
ijk=find(teall_err_sorted>0);
teall_err_sorted=max(teall_err_sorted,mean(teall_err_sorted(ijk)));

xx=linspace(0,1,201);
if ~exist('tension_profiles'); tension_profiles=-3;end
tension_profiles
tefit=interpos([0 ; rhopol_teall_sorted],[teall_sorted(1) ; teall_sorted],xx,tension_profiles,[1 0],[0 0],[100*max(teall_err_sorted) ; teall_err_sorted]);

plot(xx,tefit,'r')

% VTA
delta_time=1; %+-delta_time (8ms => 16ms interval with 3 points)
rhopol_teall_mix1=[neterho.core_rhopsinorm_on_rztime(:,ithomsoncore-delta_time:ithomsoncore+delta_time); neterho.edge_rhopsinorm_on_rztime(:,ithomsonedge-delta_time:ithomsonedge+delta_time)];
rhopol_teall_mix=reshape(rhopol_teall_mix1,(2*delta_time+1)*size(rhopol_teall_mix1,1),1);
teall_mix1 = [neterho.te_core.value(:,ithomsoncore-delta_time:ithomsoncore+delta_time) ; neterho.te_edge.value(:,ithomsonedge-delta_time:ithomsonedge+delta_time)];
teall_mix = reshape(teall_mix1,(2*delta_time+1)*size(teall_mix1,1),1);
teall_err_mix1 = [neterho.te_core.error(:,ithomsoncore-delta_time:ithomsoncore+delta_time) ; neterho.te_edge.error(:,ithomsonedge-delta_time:ithomsonedge+delta_time)];
teall_err_mix = reshape(teall_err_mix1,(2*delta_time+1)*size(teall_err_mix1,1),1);
[rhopol_teall_sorted irhopol_teall_sorted]=sort(rhopol_teall_mix);
teall_sorted=teall_mix(irhopol_teall_sorted);
teall_err_sorted = max(teall_err_mix(irhopol_teall_sorted),500); % avoid zeroes
tefit2=interpos([0 ; rhopol_teall_sorted],[teall_sorted(1) ; teall_sorted],xx,tension_profiles,[1 0],[0 0],[100*max(teall_err_sorted) ; teall_err_sorted]);

errorbar(rhopol_teall_sorted,teall_sorted,teall_err_sorted,'v')
plot(xx,tefit2,'k')

% ECE
if ~isempty_ece
  delta_time=0.005; %+-delta_time (8ms => 16ms interval with 3 points)
  itece_int=find(ece.t>=time-delta_time & ece.t<=time+delta_time);
  itecerho_int=find(ece.rhos.t>=time-delta_time & ece.rhos.t<=time+delta_time);
  rhopol_teall_mean=mean(ece.rhos.rhopsinorm_on_rztime(:,itecerho_int),2);
  teall_mean = mean(ece.data(:,itece_int),2);
  teall_std=std(ece.data(:,itece_int),0,2);
  errorbar(rhopol_teall_mean,teall_mean,teall_std,'m^')
end

figure
errorbar(neterho.core_rhopsinorm_on_rztime(:,ithomsoncore),neterho.ne_core.value(:,ithomsoncore),neterho.ne_core.error(:,ithomsoncore),'*')
aa=axis;
hold on
errorbar(neterho.edge_rhopsinorm_on_rztime(:,ithomsonedge),neterho.ne_edge.value(:,ithomsonedge),neterho.ne_edge.error(:,ithomsonedge),'s')
axis([0 1.1 0 1.2*aa(4)])

rhopol_neall_mix=[neterho.core_rhopsinorm_on_rztime(:,ithomsoncore); neterho.edge_rhopsinorm_on_rztime(:,ithomsonedge)];
neall_mix = [neterho.ne_core.value(:,ithomsoncore) ; neterho.ne_edge.value(:,ithomsonedge)];
neall_err_mix = [neterho.ne_core.error(:,ithomsoncore) ; neterho.ne_edge.error(:,ithomsonedge)];
[rhopol_neall_sorted irhopol_neall_sorted]=sort(rhopol_neall_mix);
neall_sorted=neall_mix(irhopol_neall_sorted);
neall_err_sorted = neall_err_mix(irhopol_neall_sorted);
ijk=find(neall_err_sorted>0);
neall_err_sorted=max(neall_err_sorted,mean(neall_err_sorted(ijk)));
nefit=interpos([0 ; rhopol_neall_sorted],[neall_sorted(1) ; neall_sorted],xx,tension_profiles/10,[1 0],[0 0],[100*max(neall_err_sorted) ; neall_err_sorted]);

plot(xx,nefit,'r')

if ~isempty(cxrs.data) && ~isempty(itcxrs)
  figure
  errorbar(cxrs.rhopsinorm_on_rztime(:,itcxrs),cxrs.ti_c.value(:,itcxrs),cxrs.ti_c.error(:,itcxrs),'*')
  aa=axis;
  hold on
  axis([0 1.1 0 1.2*aa(4)])
  
  rhopol_tic_mix=[cxrs.rhopsinorm_on_rztime(:,itcxrs)];
  tic_mix = [cxrs.ti_c.value(:,itcxrs)];
  tic_err_mix = [cxrs.ti_c.error(:,itcxrs)];
  [rhopol_tic_sorted irhopol_tic_sorted]=sort(rhopol_tic_mix);
  tic_sorted=tic_mix(irhopol_tic_sorted);
  tic_err_sorted = tic_err_mix(irhopol_tic_sorted);
  ij=find(tic_err_sorted==0);
  tic_err_sorted(ij) = 1e4;
  ijok=find(tic_sorted>0);
  tifit=interpos([0 ; rhopol_tic_sorted(ijok)],[max(tic_sorted) ; tic_sorted(ijok)],xx,tension_profiles,[1 0],[0 0],[1e5 ; tic_err_sorted(ijok)]);
  
  plot(xx,tifit,'r')
end

zeff=1.6;
% assume Boron (5) and Nitrogen(7) thus on average Carbon-like (6)...
% $$$ neZeff-ni=nC36
% $$$ ne6-6ni=nC*36
% $$$ ne(6-zeff)/(6-1)=ni

% Assuming 2% Boron, rest N
% $$$ ne Zeff=ni+25 0.02 ne + 49 nN
% $$$ ne7 = 7ni + 35 0.02 ne + 49 nN
% $$$ ne(7-Zeff-0.2)=ni(7-1)
% $$$ ni=(7-Zeff-0.2)/(7-1) ne;

nifit2=(6.-zeff)./(6.-1.).*nefit;
nifit=(7.-zeff-10*0.02)./(7.-1.).*nefit; % note: 1% yields same as C nefit2...
figure
plot(xx,nefit,'r')
hold on
plot(xx,nifit2,'b--')
plot(xx,nifit,'b-')
legend('ne','ni_C','ni_2%B_N',3)

exptnz.nrho=length(xx);
exptnz.rhopsi=xx;
exptnz.te=tefit;
exptnz.ne=nefit;
exptnz.zeff=zeff*ones(size(xx));
if ~isempty(cxrs.data) && ~isempty(itcxrs)
  exptnz.ti=tifit;
else
  tiote = 1.;
  tiote_in = input(['no Ti from cxrs, give Ti/Te ratio to use (default=' num2str(tiote) '): '],'s');
  if ~isempty(tiote_in); tiote = str2num(tiote_in); end;
  disp(['no Ti from cxrs, use Ti/Te = ' num2str(tiote)]);
  exptnz.ti = tiote .* tefit;
end
exptnz.ni=nifit;
exptnz_out =  write_exptnz(exptnz,[nameeq '_EXPTNZ']);

namelist_struct.nbsexpq=1111;
[fname_out,globalsvalues,namelist_struct,namelist_filename] = run_chease(namelist_struct,['./' nameeq '_EQDSK' '_COCOS17'],17,[nameeq '_EXPTNZ'],'_EXPTNZ');
globalsvalues
for i=1:length(fname_out)
  ij=findstr(fname_out{i},'.cols');
  if ~isempty(ij)
    ocols_file = fname_out{i};
    outout_chease_file = fname_out{i}(1:ij-1);
  end
end
[Hocols,chease_output_struct] = plot_ocols(ocols_file);
ocols_file
outout_chease_file

mu0=4e-7*pi;
figure
plot(chease_output_struct.s_mesh.data,chease_output_struct.pressure.data.*globalsvalues.b0exp.^2./mu0)
hold on
ptot_tnz = 1.602e-19.*(exptnz.ne.*exptnz.te + exptnz.ni.*exptnz.ti);
plot(exptnz.rhopsi,ptot_tnz,'r-')
legend('chease','exptnz')
ylabel('Pressure')
xlabel('rhopsi')

vol_phys = chease_output_struct.volumeprofile.data.*globalsvalues.r0exp.^3;
[pfit,~,~,pint]=interpos(vol_phys,chease_output_struct.pressure.data.*globalsvalues.b0exp.^2./mu0,-0.1);
ptot_tnz_smesh=interpos(exptnz.rhopsi,ptot_tnz,chease_output_struct.s_mesh.data,-0.1,[1 2],[0 ptot_tnz(end)]);
[pfit_tnz,~,~,pint_tnz]=interpos(vol_phys,ptot_tnz_smesh,-0.1);
Wmhd = 1.5.*pint(end)
Wmhd_tnz = 1.5.*pint_tnz(end)
wmhdeqg=gdat(shot,'GQI/Wmhd',1);
wmhdeqg_time=interp1(wmhdeqg.t,wmhdeqg.data,time)
