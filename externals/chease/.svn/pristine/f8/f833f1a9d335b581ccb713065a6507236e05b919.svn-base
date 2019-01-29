function [fname_chease_out,chease_data] = astra2chease_equil(filename_astra,varargin)
%function [fname_chease_out,chease_data] = astra2chease_equil(filename_astra,{astratime,doplot})
% runs CHEASE from ASTRA data
%
% INPUTS
%  filename_astra : path to .mat file containing ASTRA data (as saved by astra_tcv.m)
%
%  astratime      : index of time in astra .mat file to use as equilibrium
%                   (default: last time), see help astra2expeq for details
%  doplot         : if set to 1, plots comparison of EXPEQ and EXPEQ.OUT,
%                   and q profiles. No plots if set to 0. (default: 1)
%
%  ask_for_tension: 0 (default) do not ask if ok and option to change tension
%                   1: do prompt if ok
%
% OUTPUTS
%  fname_chease_out: cell array of strings containing path of CHEASE output files
%  chease_data     : matlab variable containing o.cols readout
%
% Procedure:
%  *Creates EXPEQ using [astra2expeq.m] (user must confirm spline fits)
%  *Sets CHEASE namelist using [write_namelist_chease.m]
%  *runs CHEASE using [run_chease_expeq.m]
%  *reads o.cols file [using read_ocols.m]
%  *plots EXPEQ files and compares output q profiles (optionally) [using plot_expeq.m]
%

% F. Felici, A. Merle, O. Sauter CRPP

astratime = []; % default
if nargin >= 2
  if ~isempty(varargin{1})
    astratime=varargin{1};
  end;
end

doplot = 1;
if nargin >=3;
  if ~isempty(varargin{2})
    doplot = varargin{2};
  end;
end

ask_for_tension=0;
if nargin >=4;
  if ~isempty(varargin{3})
    ask_for_tension = varargin{3};
  end;
end

% set up tmpdir, create if necessary
[s,uname] = unix('whoami'); tmpdir = ['/tmp/',uname(~isspace(uname)),'/'];
if ~exist(tmpdir,'dir'), unix([ 'mkdir ',tmpdir]); end

%filename_astra = 'shots/38364_z0_eITB_RES.mat';

[astrafilepath,astrafname] = fileparts(filename_astra);
fnames_out = astra2expeq(filename_astra,astratime,astrafname,astrafilepath,3,doplot,ask_for_tension);

TCV_astra = load(filename_astra); % for B0exp, IpkA
s = TCV_astra.out;
fname_expeq = fnames_out{1}; fname_exptnz = fnames_out{2};

% APPEND STUFF TO STANDARD NAMELIST
nl.Title = astrafname;
nl.IpkA = s.IPL(end) * 1000;
nl.r0exp =  s.RTOR(end);
nl.b0exp =  s.BTOR(end);
% TODO: This might not be the same time as used in astra2expeq

nl.nsttp = 3; % given I//
nl.npropt = 3; % selector for reading experimental current profile -> I//
nl.neqdsk = 0; % no eqdsk but expeq is used as input

nl.ncscal = 1; % scale to given q


% %%%%%% COMPUTATION OF RHOPSI (already done in astra2expeq) %%%%%

% To determine poloidal flux at rho=0. I have to interpolate, since ametr(1)~=0, rho(1)~=0 etc...
% must use trick to get interpos to give me the zero derivative at rho=0; see interpos help
xin_eff = [0;s.AMETR(:,end)]; yin_eff = [s.FP(1,end);s.FP(:,end)];
yout_eff = interpos(13,xin_eff,yin_eff,xin_eff,1e-9,[1 0],[0. 0.],[100;ones(size(s.AMETR(:,end)))]);
% modified by Antoine Merle to avoid complex values of rhopsi
yout_eff(1) = min(yout_eff(1:2));
psi_axis = yout_eff(1);
psi_edge = s.FP(end,end);
rhopsi = sqrt((s.FP(:,end)-psi_axis)./(psi_edge-psi_axis));



% Rescale q95
q95 = interp1(rhopsi,1./s.MU(:,end),sqrt(0.95));
disp(['Fixed q_95=',num2str(q95),' in CHEASE namelist']) ;
nl.qspec = q95; nl.csspec = sqrt(0.95); % fix q95 = q(psi=95%) = q(rho_psi =sqrt(.95));

nl.nbsexpq=1111; % to use EXPTNZ

fname_namelist = [tmpdir,'astra_chease_namelist'];
write_namelist_chease(fname_namelist,nl);

% run chease
fname_chease_out = run_chease_expeq(fname_namelist,fname_expeq,fname_exptnz);

chease_data=read_ocols(fname_chease_out{end-1});

% search for EXPEQ.OUT amongst in outputs (to plot)
for ifile = 1:length(fname_chease_out)
  if ~isempty(strfind(fname_chease_out{ifile},'EXPEQ.OUT')) && ...
        isempty(strfind(fname_chease_out{ifile},'TOR')); % do not plot the EXPEQ.TOR but the first EXPEQ
    expeqfile_out = fname_chease_out{ifile}; break;
  end
end

if doplot
  figure
  plot_expeq(fname_expeq,'b'); hold on; plot_expeq(gcf,expeqfile_out,'r'); legend('IN','OUT');
  figure
  plot(rhopsi,1./s.MU(:,end),'b');hold on;
  plot(chease_data.s_mesh.data,chease_data.qprofile.data,'r');
  ylabel('q'); xlabel('rho_\psi');
  legend('astra input','chease output')
end
