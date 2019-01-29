function fname_out = astra2expeq(fname_in,varargin)
% astra2expeq(fname_in{,it,suffix_out,fdir_out,nsttp,display})
% writes astra results .mat file as an EXPEQ and EXPTNZ files.
% These files can then be used as input to CHEASE, for example.
%
% The profiles are written on the rhopsi from ASTRA at the correct time
% This is also the rho written in the EXPEQ
%
%  INPUTS
%  fname_in 	: (path and) filename .mat file containing ASTRA data
%  ===Optional===:
%  itime	: index of time in astra results to use as equilibrium
%		    default: last time index.
%                   choose itime = 'any character'; to get a plot from which to choose a time
%                   <0 to set the time=abs(itime)
%  suffix_out 	: suffix of EXPEQ and EXPTNZ files to be written.
%		    resulting filenames will be ['EXPEQ_',suffix_out] and ['EXPTNZ_',suffix_out]
%		    default: name of input file after removing .mat
%  fdir_out 	: directory to which EXPEQ and EXPTNZ files will be written
%			default: current directory
%  nsttp 	: type of EXPEQ file written (with Ipar or with TT').
%			currently only nsttp = 3 is implemented. (=Only one allowed?)
%			default: 3
%  doplot	: set to 1 to plot the profiles that will go into the EXPEQ
%		        this is very useful to check the interpolations performed
%  ask_for_tension: 0 (default) do not ask if ok and option to change tension
%                   1: do prompt if ok
%
%  OUTPUTS
%  fname_out	: cell array containing names of EXPEQ and EXPTNZ files written
%
%  EXAMPLES
%    		  astra2expeq('my_astra_res.mat');
%    fname_out  = astra2expeq('my_astra_res.mat',4,'TEST','my_res_dir');

% Functionized by F.Felici, May 2009, based on C.Zucca's original script.

% ne = []; % need this to avoid conflict with ne function

TCV_astra = load(fname_in);
% get all the field names and put them into this function's workspace
% as separate variables (avoids astra_out.xxx) everywhere
% fields = fieldnames(TCV_astra.out);
% for ifield = 1:length(fields)
%     thisfield = TCV_astra.out(fields{ifield});
%     eval([fields{ifield},'=thisfield;']);
% end
% clear thisfield;
s = TCV_astra.out;

% handle optional inputs
if nargin < 1; error('not enough inputs'); end
if nargin >= 2 && ~isempty(varargin{1}) % index of astra equilibrum to use
  itime = varargin{1};
  if ischar(itime); % allow user to choose the appropriate time
    hf = figure;
    subplot(211);
    plot(s.T,s.IPL*1000,'.-');
    ylabel('Ip [kA]'); xlabel('t[s]'); grid on;
    subplot(212);
    plot(s.T,s.TE(1,:)*1000,'.-');
    ylabel('T_e [keV]'); xlabel('t[s]'); grid on;
    suptitle('Select the time to write to EXPEQ')
    [xx,yy] = ginput(1); close(hf)
    disp('Select the time to write to EXPEQ')
    itime = iround_OS(s.T,xx);
    disp([' chose t= ',num2str(xx,3),' closest time point: t=',num2str(s.T(itime))]); 
  elseif itime<0
    % time given instead of index
    itime = iround_OS(s.T,abs(itime));
    disp([' closest time point chosen: t=',num2str(s.T(itime))]);
  end
  if itime > length(s.T);
    error(['itime must be less than length(t) from ASTRA: ',int2str(length(s.T))]);
  end
  disp(['using data at ASTRA time ',num2str(s.T(itime)),'[s] to generate EXPEQ file']);
else
  itime = length(s.T);
  disp('using last time point in ASTRA file to generate EXPEQ file')
end

time_astra = s.T(itime);

[pathstr,suffix_out] = fileparts(fname_in); % remove .mat extension
if nargin >= 3 && ~isempty(varargin{2})
  suffix_out = varargin{2};
end

fdir_out = pwd; % current directory
if nargin >= 4 && ~isempty(varargin{3})
  fdir_out = varargin{3};
  if ~exist(fdir_out,'dir'); error([fdir_out,' does not exist']); end
end

nsttp = 3; % default
if nargin >= 5 && ~isempty(varargin{4})
  nsttp = varargin{4};
  if (nsttp ~= 3); error('nsttp must be 3 to use data from astra'); end
end

doplot = 1;
if nargin >= 6 && ~isempty(varargin{5})
  doplot = varargin{5};
end

ask_for_tension=0;
if nargin >= 7 && ~isempty(varargin{6})
  ask_for_tension = varargin{6}
end;

% check pi
% if exist('pi','var')~=5; pion = pi; clear pi; end; % if pi is not internal matlab function
mu0=4.e-07*pi;

% filename definitions
fname_EXPEQ  = fullfile(fdir_out,['EXPEQ_',suffix_out,'t',num2str(time_astra)]);
fname_EXPTNZ = fullfile(fdir_out,['EXPTNZ_',suffix_out,'t',num2str(time_astra)]);

% Some basic parameters
rtor = s.RTOR(itime); btor = s.BTOR(itime);
% shape
% Calculate psi(r,z) for parametrized equilibrium (a,kappa,delta)
Lth = 51; %number of theta points
theta=linspace(0,2*pi,Lth);
%   Antoine Merle 26/04/2013 In ASTRA the position of the LCFS does take into account shif(end,:)
r_bou=(rtor+s.SHIF(end,itime)+s.AMETR(end,itime)*(cos(theta)-s.TRIA(end,itime)*sin(theta).^2)).'/rtor;
z_bou=(s.SHIV(end,itime)+s.AMETR(end,itime)*s.ELON(end,itime)*sin(theta)).'/rtor;
epsilon=s.AMETR(end,itime)'./rtor;
rgeom=mean(r_bou); zgeom=mean(z_bou);

% To improve (first point is not magnetic axis)
rmag= rtor+s.SHIF(1,itime); zmag= s.SHIV(1,itime);

% Make zef a profile for consistency
if length(s.ZEF) == numel(s.ZEF); % make zef into a profile
  zef = ones(size(rho,1),1)*s.ZEF;
else
  zef = s.ZEF;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% COMPUTE PROFILES FOR EXPEQ and EXPTNZ  %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We will remap the astra profiles to a new rho_psi (rhopsi_new)
% This extrapolates to rho=0 (astra does not)
% and extrapolates rho(edge) since astra edge values can be inaccurate

% profiles
% 1e19m^3*keV * [(1.6e-19)*1000] = J/m^3 = N/m^2 = Pa
pressure_e= s.NE(:,itime).*s.TE(:,itime).*1.6.* 1000; %[To Pa]
pressure_i= s.NI(:,itime).*s.TI(:,itime).*1.6.* 1000; %[To Pa]
pressure = pressure_e + pressure_i; %[MKSA]
tcurrent= rtor*btor*s.IPOL(:,itime);

% fp is poloidal flux as Liuqe but in "radians" T m^2, thus 2pi times larger than CHEASE
polpsi=s.FP(:,itime)./2./pi;

% To determine poloidal flux at rho=0. I have to interpolate, since ametr(1)~=0, rho(1)~=0 etc...
% must use trick to get interpos to give me the zero derivative at rho=0; see interpos help
xin_eff = [0;s.AMETR(:,itime)]; yin_eff = [s.FP(1,itime);s.FP(:,itime)];
yout_eff = interpos(13,xin_eff,yin_eff,xin_eff,1e-9,[1 0],[0. 0.],[100;ones(size(s.AMETR(:,itime)))]);
% modified by Antoine Merle to avoid complex values of rhopsi
yout_eff(1) = min(yout_eff(1:2));
psi_axis = yout_eff(1);
psi_edge = s.FP(end,itime);
rhopsi = sqrt((s.FP(:,itime)-psi_axis)./(psi_edge-psi_axis));

% new psi adds point at axis, ignores last point
% last point to be ignored as edge values are often wrong
polpsi_new = [psi_axis; s.FP(1:end-1,itime); psi_edge] /(2*pi);
rhopsi_new = sqrt((polpsi_new-polpsi_new(1))./(polpsi_new(end)-polpsi_new(1))); % normalized rhopsi

tens = -0.1;
fits_ok = 0; % init
while ~fits_ok % allow user to change tension
  sigma = ones(1,length(polpsi)-1); sigma(1:3) = 100; % disregard last 3 points
  [P,Pprime]= interpos(13,polpsi(1:end-1),pressure(1:end-1),polpsi_new,tens,[0 0],[0 0],sigma);  % to get dp/dpsi;
  % keyboard
  [T,Tprime]= interpos(13,polpsi(1:end-1),tcurrent(1:end-1),polpsi_new,tens,[0 0],[0 0],sigma);  % to get dT/dpsi;
  % note that it would be wrong to impose dp/dpsi = 0 at edge.
  % dp/drho is in any case zero since dpsi/drho ~ 2rho -> 0 for rho=0!
  
  p0 = P(1); p_edge = P(end);
  p_edge_chease=p_edge./btor.^2.*mu0;
  
  % Convert pprime and TT' to CHEASE units
  TTprime_chease= T.*Tprime/btor;
  % NB Ttprime not really used anyway since nsttp =3 in these cases
  Pprime_chease= Pprime*mu0*rtor^2/btor;
  
  %%%%% Some other profiles to remap %%%%
  % Mapped via polpsi onto polpsi_new but this is equivalent to mapping on rhopsi_new
  
  % current profile to put into chease. Converted from j|| ASTRA to j|| chease
  jpartild_chease_physical = s.CU(:,itime).*s.VR(:,itime)./4./pi.^2./s.RHO(:,itime)./rtor; % to j|| chease physical
  jpartild_chease_raw = jpartild_chease_physical.*mu0.*rtor./btor*1e6;  %convert to CHEASE units
  jpartild_chease = interpos(13,polpsi(1:end-1),jpartild_chease_raw,polpsi_new,tens,[0 0],[0 0],sigma); % interpolate on new rho
  
  %cu_chease=s.CU(:,itime).*mu0.*rtor./btor*1e6; % cu in MA/m^2
  
  % Q profile to have correct q edge and q0
  qextraps = interpos(13,polpsi(1:end-1),1./s.MU(1:end-1,itime),polpsi_new,0);
  q0 = qextraps(1); q_edge = qextraps(end);
  
  % Temperature and density profiles to go into EXPTNZ
  te_remap = interpos(13,polpsi(1:end-1),s.TE(:,itime),polpsi_new,tens,[0 0],[0 0],sigma);
  ti_remap = interpos(13,polpsi(1:end-1),s.TI(:,itime),polpsi_new,tens,[0 0],[0 0],sigma);
  ne_remap = interpos(13,polpsi(1:end-1),s.NE(:,itime),polpsi_new,tens,[0 0],[0 0],sigma);
  zef_remap = interpos(13,polpsi(1:end-1),zef(:,itime),polpsi_new,tens,[0 0],[0 0],sigma);
  
  % some optional plots to check interpolations
  if doplot
    EXPEQ_color = [1 0.8 0.8];
    EXPTNZ_color = [0.8 0.8 1];
    hf = figure;
    
    hsp = NaN(6,1);
    hs  = NaN(15-6,1);
    
    hsp(1) = subplot(5,3,1); plot(polpsi(1:end-1),pressure(1:end-1),'r.'); hold on;plot(polpsi_new,P);
    ylabel('p [Pa]'); xlabel('\psi'); axis tight
    hsp(4) = subplot(5,3,4); plot(polpsi_new,Pprime);
    ylabel('dp/d\psi'); xlabel('\psi'); axis tight
    hsp(2) = subplot(5,3,2); plot(polpsi(1:end-1),tcurrent(1:end-1),'r.'); hold on; plot(polpsi_new,T);
    ylabel('T'); xlabel('\psi');axis tight
    hsp(5) = subplot(5,3,5); plot(polpsi_new,Tprime);
    ylabel('dT/d\psi'); xlabel('\psi');axis tight
    hsp(3) = subplot(5,3,3); plot(polpsi,1./s.MU(:,itime),'r.'); hold on; plot(polpsi_new,qextraps);
    ylabel('q'); xlabel('\psi');axis tight
    hsp(6) = subplot(5,3,6); plot(polpsi,jpartild_chease_raw,'r.'); hold on; plot(polpsi_new,jpartild_chease);
    ylabel('j_{||} [MA/m^2]'); xlabel('\psi'); axis tight
    
    hs(7-6) = subplot(5,3,7); plot(rhopsi,pressure,'r.'); hold on; plot(rhopsi_new,P);
    ylabel('p [Pa]'); xlabel('\rho_\psi');axis tight
    hs(10-6) = subplot(5,3,10); plot(rhopsi_new,Pprime);
    ylabel('dp/d\psi'); xlabel('\rho_\psi'); set(gca,'color',EXPEQ_color);axis tight
    hs(8-6) = subplot(5,3,8); plot(rhopsi,tcurrent,'r.'); hold on; plot(rhopsi_new,T);
    ylabel('T'); xlabel('\rho_\psi');axis tight
    hs(11-6) = subplot(5,3,11); plot(rhopsi_new,Tprime);
    ylabel('dT/d\psi'); xlabel('\rho_\psi');axis tight
    hs(9-6) = subplot(5,3,9); plot(rhopsi,1./s.MU(:,itime),'r.'); hold on; plot(rhopsi_new,qextraps);
    ylabel('q'); xlabel('\rho_\psi');axis tight
    hs(12-6) = subplot(5,3,12); plot(rhopsi,jpartild_chease_raw,'r.'); hold on; plot(rhopsi_new,jpartild_chease);
    ylabel('j_{||} [MA/m^2]'); xlabel('\rho_\psi'); set(gca,'color',EXPEQ_color);axis tight
    
    hs(13-6) = subplot(5,3,13); plot(rhopsi,s.TE(:,itime),'r.'); hold on;  plot(rhopsi_new,te_remap,'-');
    plot(rhopsi,s.TI(:,itime),'r.'); hold on;  plot(rhopsi_new,ti_remap,'--');
    ylabel('T_e(-),T_i(--) [keV]'); xlabel('\rho_\psi'); set(gca,'color',EXPTNZ_color);axis tight
    hs(14-6) = subplot(5,3,14); plot(rhopsi,s.NE(:,itime),'r.'); hold on;  plot(rhopsi_new,ne_remap);
    ylabel('n_e [10e19]'); xlabel('\rho_\psi'); set(gca,'color',EXPTNZ_color);axis tight
    hs(15-6) = subplot(5,3,15);  plot(rhopsi,zef(:,itime),'r.'); hold on;plot(rhopsi_new,zef_remap);
    set(gca,'ylim',[min(zef_remap)-1,max(zef_remap)+1]); % to avoid axis errors
    ylabel('Z_{eff}'); xlabel('\rho_\psi'); set(gca,'color',EXPTNZ_color);axis tight
    suptitle('Interpolated ASTRA profiles on \psi and \rho_\psi. Colored go to EXPEQ or EXPTNZ')
    
    linkaxes(hs,'x');
    linkaxes(hsp,'x'); % link psi and rhopsi axes individually
    set(gcf,'position',[100 150 600 700]);
    
    
    % plot boundary
    hf(2)=figure; plot(r_bou,z_bou); xlabel('R/R_0'); ylabel('Z/Z0'); grid on;
    axis equal
    title('Boundary to be written to EXPEQ');
    set(gcf,'position',[800 500 300 300]);
  end
  
  %% Ask user whether fits are ok
  disp(['spline fit tension:',num2str(tens)])
  if ask_for_tension
    uresp = input('Check the fits! Hit [return] if they are ok, otherwise enter other tension ');
    fprintf('\n');
  else
    uresp = [];
  end
  drawnow;
  
  if isempty(uresp)
    fits_ok=1;
  else
    tens = uresp;
    fits_ok=0;
    delete(hf(ishandle(hf))); 
  end
  
end % of while loop


% interpolation between profiles (formerly used for SECCD)
%interp = 0
%if interp
%  [interpstruct : structure containing parameters to allow interpolation of current profile
%			between two times
%			interpstruct.int1 = index of first time
%			interpstruct.int2 = index of second time
%			interpstruct.lambda = interpolation j = j(int1)*(lambda-1) + j(int2)*lambda
%  ] this part is not working now -> revisit when needed
%	error('this piece of code must be revisited since rhopsi from astra is not constant over time');
%	jpartild1=cu(:,int1).*vr(:,int1)./4./pi.^2./rho./rtor;
%	jpartild2=cu(:,int2).*vr(:,int2)./4./pi.^2./rho./rtor;
%	jpartild = jpartild1*(1-lambda) + jpartild2*(lambda);
%	figure(99);
%	plot(rhopsi(:,int1),jpartild1,'b'); hold on plot(rhopsi(:,int2),jpartild1,'r');
%	plot(rhopsi(:,int1),jpartild,'c'); plot(rhopsi(:,int2),jpartild,'m');
%end


% itot_chease= itot*mu0/rtor(1)/btor(1)*1e3; %itot in kA

% Define structure for write_EXPEQ
EXPEQdata.nsttp = nsttp;
EXPEQdata.nrhotype = 0;
EXPEQdata.epsilon = epsilon;
EXPEQdata.zgeom = zgeom;
EXPEQdata.pedge	= p_edge_chease;
EXPEQdata.RZ_psi = [r_bou,z_bou];
EXPEQdata.rho = rhopsi_new;
EXPEQdata.Pprime = Pprime_chease;
EXPEQdata.Jparallel = jpartild_chease;

% auxiliary quantities at the end of EXPEQ
EXPEQdata.aux.R0exp = rtor;
EXPEQdata.aux.B0exp = btor;
EXPEQdata.aux.Itot = s.IPL(itime)*1e6; % convert to MA
EXPEQdata.aux.elong = s.ELON(end,itime);
EXPEQdata.aux.q0 = q0;
EXPEQdata.aux.qedge = q_edge;
EXPEQdata.aux.p0 = p0;
EXPEQdata.aux.psi0 = psi_axis;
EXPEQdata.aux.aratio = (s.AMETR(end,itime)/rtor);
EXPEQdata.aux.volume = s.VOLUM(end,itime);
EXPEQdata.aux.rmin = min(r_bou);
EXPEQdata.aux.rmax = max(r_bou);
EXPEQdata.aux.zmin = min(z_bou);
EXPEQdata.aux.zmax = max(z_bou);
EXPEQdata.aux.rgeom = rgeom;

%%%%%%%%%%%%%%%%%%%%%%%%%%
% create EXPEQ
%%%%%%%%%%%%%%%%%%%%%%%%%%

[~,wstat] = write_expeq(EXPEQdata,fname_EXPEQ);
if ~wstat,
  disp(['Finished writing ',fname_EXPEQ])
else
  error(['error closing ',fname_EXPEQ]);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%
% create EXPZT
%%%%%%%%%%%%%%%%%%%%%%%%%%
% no. points, rhopsi, Te, ne, Zeff, Ti
% convert from ASTRA units to keV and m-3
EXPTNZdata.rhopsi = rhopsi_new;
EXPTNZdata.te = te_remap*1e3;
EXPTNZdata.ne = ne_remap*1e19;
EXPTNZdata.zeff = zef_remap;
EXPTNZdata.ti = ti_remap * 1e3;

[~,wstat_EXPTNZ] = write_exptnz(EXPTNZdata,fname_EXPTNZ);
if ~wstat_EXPTNZ
  disp(['Finished writing ',fname_EXPTNZ])
else
  error(['error closing ',fname_EXPTNZ]);
end

%%%%%%%%%%%%%%%%%%%
% ASSIGN OUTPUTS
%%%%%%%%%%%%%%%%%%%
if nargout == 1
  fname_out = [{fname_EXPEQ},{fname_EXPTNZ}];
end

return
%%%% END OF MAIN %%%%



% START WRITING FILE
%	fid=fopen(fname_EXPEQ,'w');
%	if fid == -1
%		error(['error opening ',fname_EXPEQ,' for writing'])
%	end
%	fprintf(fid,'%14.7f\n',epsilon);
%	fprintf(fid,'%14.7g\n',zgeom);
%	fprintf(fid,'%14.7g\n',pedge);
%	fprintf(fid,' %d\n',length(r_bou));
%	fprintf(fid,'%14.7f  %14.7f\n',[ r_bou ; z_bou ]);
%	fprintf(fid,' %d\n',length(pressure));
%	fprintf(fid,' %d\n',nsttp);
%	fprintf(fid,'%14.7f\n',rhopsi(:,itime));
%	fprintf(fid,'%14.7f\n',Pprime_chease);
%	if nsttp == 1
%		fprintf(fid,'%14.7f\n',TTprime_chease);
%	elseif nsttp == 3
%		fprintf(fid,'%14.7f\n',jpartild_chease);
%	end
%	% additional useful information
%	fprintf(fid,'\n%14.7f  %s %8.7f\n',polpsi(end)-polpsi(1),' [T m^2] psi(a)-psi(0) -> in chease units: ',(polpsi(end)-polpsi(1))./(rtor^2*btor));
%	fprintf(fid,'%14.7f %s\n ',rmag,'  r-magaxe ');
%	fprintf(fid,'%13.7g %s\n ',zmag,'  z-magaxe ');
%	fprintf(fid,'%13.7f %s\n ',rtor,'  rtor ');
%	fprintf(fid,'%13.7f %s\n ',btor,'  B0 ');
%	fprintf(fid,'%13.7f %s %8.7f\n',ipp,'  [kA] I_p -> in chease units: ',Ip_chease);
%	fprintf(fid,'%14.7f %s\n ',elon(end),'  kappa ');
%	fprintf(fid,'%13.7f %s\n ',q(1),'  q_0 ');
%	fprintf(fid,'%13.7f %s\n ',q(end),'  q_edge ');
%	fprintf(fid,'%13.7f %s\n ',lint(end),'  l_i ');
% %	fprintf(fid,'%13.0f %s\n ',0,'  number of X points ');
% %	fprintf(fid,'%13.2f %s\n ',tt+.25,'  time ');
% %	fprintf(fid,'%7.0f %3s\n ',shot,' shot number ');
% %	fprintf(fid,'%9s %1.0f\n', 'Liuqe version ',liuqe);
%
%
% LEGACY STUFF
%
% some calculations formerly done by fuf.m
%q=1./mu(:,itime); now done in store_astra_tcv.m
%cuohm=cu(:,itime)-cd(:,itime)-cubs(:,itime); % ohmic current

% rewrote this bit to handle only one time and look nicer
% this is actually an integration, maybe improve it later
%for tel=1:length(t)
%  qw=sum(1./ipol(2:end,tel).^2.*1000.*cd(2:end,tel).*vr(2:end,tel).*(rho(2:end,tel)-rho(1:end-1,tel)));
%  icd1(tel)=ipol(end,tel)/2/pi/rtor(1)*qw;
%  qw=sum(1./ipol(2:end,tel).^2.*1000.*cubs(2:end,tel).*vr(2:end,tel).*(rho(2:end,tel)-rho(1:end-1,tel)));
%  ibs1(tel)=ipol(end,tel)/2/pi/rtor(1)*qw;
%  qw=sum(1./ipol(2:end,tel).^2.*1000.*cuohm(2:end,tel).*vr(2:end,tel).*(rho(2:end,tel)-rho(1:end-1,tel)));
%  iohm1(tel)=ipol(end,tel)/2/pi/rtor(1)*qw;
%end

% integration basis
%aa	= [ipol(end,itime)/2/pi/rtor * (1./ipol(2:end,itime).^2.*1000.*vr(2:end,itime).*diff(rhopsi(:,itime))) ]';
% integrated currents
%icd	= aa * cd(2:end,itime);
%ibs	= aa * cubs(2:end,itime);
%iohm	= aa * cuohm(2:end);
%itot	= icd+ibs+iohm;

%Ip_chease=itot(end,itstart_eff)*1e3*mu0/rtor(1)/btor(1);
%ipp=ibs+icd+iohm;
% there's a discrepancy between cu and sum(cubs,cd,cuohm) at the 41st rho point, so use the sum because it gives the correct measured Ip (FF -> why?? )
%Ip_chease=ipp*1e3*mu0/rtor/btor;

% ignore all the above, Ip just given from ASTRA total
%Ip_chease=ipl(itime)*1e3*mu0/rtor/btor;
