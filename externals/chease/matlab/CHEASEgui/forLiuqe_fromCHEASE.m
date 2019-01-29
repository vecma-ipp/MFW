
shot=48158;
time=1.6503;
ochease_file = ['/home/sauter/RAPTOR/RAPTOR_varying_geometry_develop/chease_equils/TCV/o.chease.' num2str(shot) 't' num2str(time) '_1.2pTCV_Ipar'];
% assume .cols added for cols. Note can use following for plotting CHEASE results:
% eval(['plotdatafile(''' ochease_file '.cols'');'])

eval(['[globalsvalues,ocols_struct] = extractdatachease(''' ochease_file ''',''' ochease_file '.cols'');'])
r0exp = globalsvalues.r0exp;
b0exp = globalsvalues.b0exp;
mu0=4e-7*pi;

forLiuqe.psi = ocols_struct.psichease_eq_psi_over_2pi.data .* 2.*pi .* b0exp .* r0exp.^2;
forLiuqe.psi_norm = forLiuqe.psi./forLiuqe.psi(end);
forLiuqe.pprime = ocols_struct.pprime_eq_dp_over_dpsi.data .* b0exp ./ mu0 ./ r0exp.^2;
forLiuqe.ttprime = ocols_struct.txdt_over_dpsi.data .* b0exp;
forLiuqe.q = ocols_struct.qprofile.data;
forLiuqe.Phi = ocols_struct.rho_tor_eq_sqrt_phi_over_pi_over_b0.data.^2 .* pi .* b0exp .* r0exp.^2;
forLiuqe.Ip = globalsvalues.ipchease .* r0exp .* b0exp ./ mu0;
forLiuqe.shot = shot;
forLiuqe.time = time;
forLiuqe.cheasefilename = ochease_file;

figure
plot(forLiuqe.psi_norm,forLiuqe.pprime)
ylabel('pprime')
xlabel('psi\_norm')

figure
plot(forLiuqe.psi_norm,forLiuqe.ttprime)
ylabel('TTprime')
xlabel('psi\_norm')
