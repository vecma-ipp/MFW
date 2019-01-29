function [EXPEQdataout,fclose_out] = write_expeq(varargin)
% ---------------------------------------------------------------------
% Script to write expeq from structure, creates EXPEQ file in current
% directory.
% ---------------------------------------------------------------------
% SYNTAX:
%
%  [EXPEQdataout,fclose_out] =  write_expeq(EXPEQdata,fnamefull); % to write EXPEQ file
%  [EXPEQdataout] = write_expeq; % to get some default values
%
% OUTPUT:
% EXPEQdataout: EXPEQdata structure including .fname, .fnamefull and .pathname
% fclose_out: status flag of fclose at the end of the function (0 if successful)
%
% INPUTS:
% fnamefull: filename of written EXPEQ. Typical example 'EXPEQ_xxx'
% nverbose : verbosity level
%
% EXPEQdata is a structure with the following fields:
% 	EXPEQdata.nsttp		: 1 -Give TT', 2 -Give I*, 3 -Give J_|| (see below)
% 	EXPEQdata.nrhotype	: 0 -rho_psi, 1 -rho_tor. Make sure 'datain' has NFUNRHO=1
% 	EXPEQdata.epsilon 	: inverse aspect ratio
%	EXPEQdata.zgeom		: Geometric mean of contour z position
%	EXPEQdata.pedge		: (normalized) pressure at edge (in CHEASE units)
%	EXPEQdata.RZ_psi	: nx2 matrix with R/R0 and Z/R0 coordinates of boundary
%	EXPEQdata.rho		: Grid of rho, can be rho_psi or rho_tor (set nrhotype accordingly)
%	EXPEQdata.Pprime	: Pressure derivative profile
%
%	Depending on the value of EXPEQdata.nsttp you should define either
%	EXPEQdata.TTprime	: TT', or
%	EXPEQdata.Istar		: I*, or
%	EXPEQdata.Jparallel	: J_||.
%
%	EXPEQdata.extralines		: Extra field containing structure with additional equilibrium data
%				  (see below)
%
% Additional information about the equlibrium can be specified at the end of the file.
% Define the field EXPEQdata.extralines as a structure with data in MKSA units.
%    Permitted fields:
%       extralines.R0exp	: Experimental R0 [m]
%       extralines.B0exp	: Experimental B0 [T]
%	extralines.psi0	: Psi at axis
%	extralines.rmag	: r of magnetic axis
%	extralines.zmag	: z of magnetic axis
%	extralines.z0		: z of ???
%	extralines.Itot	: total plasma current [A]
%	extralines.elong	: elongation
%	extralines.q0		: q at axis
%	extralines.q_edge	: q at edge
%	extralines.betap	: ??
%	extralines.betax	: ??
%	extralines.induc	: Internal inductance
%	extralines.p0		: pressure at magnetic axis
%	extralines.beta	: ??
%	extralines.betas	: ??
%	extralines.aratio	: aspect ratio (R/a)
%	extralines.volume	: plasma volume
%	extralines.area	: plasma poloidal surface area
%	extralines.length	: plasma contour length following magnetic axis
%	extralines.rmin	: minimum r of lcfs
%	extralines.rmax	: maximum r of lcfs
%	extralines.zmin	: minimum z of lcfs
%	extralines.zmax	: maximum z of lcfs
%	extralines.rgeom	: r of geometric centroid of lcfs
%	extralines.zgeom	: z of geometric centroid of lcfs
%
% see also read_expeq and plot_expeq
% For details, see the CHEASE documentation
% ---------------------------------------------------------------------

% F.Felici CRPP 2009

if nargin == 0 % empty call to get defaults
    EXPEQdata.nsttp       = 3;
    EXPEQdata.nrhotype    = 0;
    EXPEQdata.epsilon     = 1;
    EXPEQdata.zgeom       = 0;
    EXPEQdata.pedge       = 0;
    EXPEQdata.RZ_psi      = [1+0.5*cos(linspace(0,2*pi,21)'),0.5*sin(linspace(0,2*pi,21)')]; % Some elongation
    EXPEQdata.rho         = linspace(0,1,21);
    EXPEQdata.Pprime      = -EXPEQdata.rho.*exp(-EXPEQdata.rho.^2/0.5^2);
    EXPEQdata.TTprime     = [];
    EXPEQdata.Istar       = [];
    EXPEQdata.Jparallel   = exp(-EXPEQdata.rho.^2/0.5^2);
    EXPEQdata.extralines  = {};
    EXPEQdata.fname       = '';
    EXPEQdata.pathname    = '';
    EXPEQdata.fnamefull   = '';
    EXPEQdataout = EXPEQdata;
    fclose_out = [];
    return
elseif nargin == 2
    [EXPEQdata,fnamefull] = deal(varargin{:});
    nverbose = 0;
elseif nargin == 3
    [EXPEQdata,fnamefull,nverbose] = deal(varargin{:});
else
    error('incorrect number of input arguments');
end


%prepare values
if numel(EXPEQdata.RZ_psi) ~= 2*size(EXPEQdata.RZ_psi,1); error('wrong size for EXPEQdata.RZ_psi'); end
n_rpsi = length(EXPEQdata.RZ_psi);
n_rho = length(EXPEQdata.rho);
switch EXPEQdata.nsttp
    case 1
        other_profile = EXPEQdata.TTprime;
    case 2
        other_profile = EXPEQdata.Istar;
    case 3
        other_profile = EXPEQdata.Jparallel;
end

EXPEQdataout=EXPEQdata;

[pathname,fname,fext]=fileparts(fnamefull);
if isempty(pathname); pathname=pwd; end
EXPEQdataout.fname=[fname fext];
EXPEQdataout.fnamefull=fullfile(pathname,EXPEQdataout.fname);
EXPEQdataout.pathname=pathname;

% write structure to EXPEQ
fid=fopen(EXPEQdataout.fnamefull, 'w');
if fid==-1; error(['error opening ',EXPEQdataout.fnamefull]); end

fprintf(fid,'%18.8E\n',EXPEQdata.epsilon);
fprintf(fid,'%18.8E\n',EXPEQdata.zgeom);
fprintf(fid,'%18.8E\n',EXPEQdata.pedge);
fprintf(fid,'%5d\n',n_rpsi);
fprintf(fid,'%18.8E%18.8E\n',EXPEQdata.RZ_psi');
fprintf(fid,'%5d\n',n_rho);
fprintf(fid,'%5d%5d\n',[EXPEQdata.nsttp EXPEQdata.nrhotype]);
fprintf(fid,'%18.8E\n',EXPEQdata.rho);
fprintf(fid,'%18.8E\n',EXPEQdata.Pprime);
fprintf(fid,'%18.8E\n',other_profile); % current profile

% EXTRALINES in STANDARD FORMAT to be read by grep etc
if isfield(EXPEQdata,'extralines');
    for i=1:length(EXPEQdata.extralines)
        fprintf(fid,'%s\n',EXPEQdata.extralines{i});
    end
    % $$$   write_expeq_extralines(fid,EXPEQdata.extralines);
end

fclose_out = fclose(fid);

if ~fclose_out
    if nverbose >= 3, disp(['Wrote ',EXPEQdataout.fnamefull]);end
else
    error('something went wrong writing %s',EXPEQdataout.fnamefull)
end

return


% $$$ function write_expeq_extralines(fid,extralines)
% $$$ % write extralinesiliary information at the end of EXPEQ file.
% $$$ % Formats borrowed from CHEASE outmksa.f90
% $$$
% $$$ mu0 = 4.E-07*pi;
% $$$
% $$$ if any(~isfield(extralines,{'R0exp','B0exp'})) return; end % do not continue if not at least R0 and B0 are defined
% $$$
% $$$ R0 = extralines.R0exp; B0 = extralines.B0exp;
% $$$ SIGNB0XP = sign(B0);
% $$$
% $$$ fprintf(fid,'\n');
% $$$ fprintf(fid,' %s\n','*************************************');
% $$$ fprintf(fid,' %s\n','SOME QUANTITIES AND THEIR MKSA VALUES');
% $$$ fprintf(fid,' %s\n','*************************************');
% $$$
% $$$
% $$$ if isfield(extralines,'psi0');
% $$$ psi0_chease = extralines.psi0/(B0*R0^2);
% $$$ fprintf(fid,'\n%18.8E%s%18.8E', abs(psi0_chease), ' abs(PSI-AXIS) --> [T M**2] ', abs(extralines.psi0));
% $$$ end
% $$$ if isfield(extralines,'rmag'); fprintf(fid,'\n%18.8E%s%18.8E', extralines.rmag/R0,' R OF MAGAXE --> [M]   ',extralines.rmag); end
% $$$ if isfield(extralines,'zmag'); fprintf(fid,'\n%18.8E%s%18.8E', extralines.zmag/R0,' Z OF MAGAXE --> [M]   ',extralines.zmag); end
% $$$ if isfield(extralines,'z0'); fprintf(fid,'\n%18.8E%s%18.8E', extralines.z0/R0,' Z0 --> [M]   ',extralines.z0); end
% $$$ fprintf(fid,'\n%18.8E%s',R0,' R0 [M] USED FOR CONVERTING TO MKSA');
% $$$ fprintf(fid,'\n%18.8E%s',B0,' B0 [T] USED FOR CONVERTING TO MKSA');
% $$$ fprintf(fid,'\n%18.8E%s',SIGNB0XP,' SIGN OF B0 IN EXPERIMENT (CHEASE ASSUMES 1.0) ');
% $$$ if isfield(extralines,'Itot')
% $$$  SIGNIPXP = sign(extralines.Itot);
% $$$  fprintf(fid,'\n%18.8E%s%18.8E',extralines.Itot / (R0*B0)*mu0, ' TOTAL CURRENT --> [A] ',extralines.Itot);
% $$$  fprintf(fid,'\n%18.8E%s',SIGNIPXP,' SIGN OF IP IN EXPERIMENT (CHEASE ASSUMES 1.0) ');
% $$$ end
% $$$ if isfield(extralines,'elong'); fprintf(fid,'\n%18.8E%s',extralines.elong,' b/a'); end
% $$$ if isfield(extralines,'q0');
% $$$ fprintf(fid,'\n%18.8E%s%18.8E',extralines.q0,' Q_ZERO, USING SIGNS OF IP AND B0, WOULD GIVE: ', SIGNB0XP*SIGNIPXP*extralines.q0);
% $$$ end
% $$$ if isfield(extralines,'q_edge');
% $$$ fprintf(fid,'\n%18.8E%s%18.8E',extralines.q_edge,' Q_EDGE, USING SIGNS OF IP AND B0, WOULD GIVE: ', SIGNB0XP*SIGNIPXP*extralines.q_edge);
% $$$ end
% $$$ if isfield(extralines,'betap'); fprintf(fid,'%18.8E%s',extralines.betap,' POLOIDAL BETA'); end
% $$$ if isfield(extralines,'betax'); fprintf(fid,'%18.8E%s',extralines.betax,' BETA_EXP=<P>*2*MU0/B0**2'); end
% $$$ if isfield(extralines,'induc'); fprintf(fid,'%18.8E%s',extralines.induc,' LI'); end
% $$$ if isfield(extralines,'p0');
% $$$  p0_chease = extralines.p0 *(mu0 / (B0^2));
% $$$  p0_1019m3keV = p0_chease * B0^2/mu0/1.602e-16/1e19;
% $$$  fprintf(fid,'\n%18.8E%s%18.8E%s%18.8E',p0_chease, ' PRESSURE ON AXIS --> [Pa] ', extralines.p0,'  --> [10**19 M**-3 KEV]: ', p0_1019m3keV);
% $$$ end
% $$$ if isfield(extralines,'beta'); fprintf('%18.8E%s',extralines.beta,' BETA'); end
% $$$ if isfield(extralines,'betas'); fprintf('%18.8E%s',extralines.betas,' BETA* (SQRT(<P**2>))'); end
% $$$ if isfield(extralines,'psi0')
% $$$  fprintf(fid,'\n%18.8E%s%18.8E',psi0_chease, ' PSI-AXIS --> [T M**2] ', extralines.psi0);
% $$$  fprintf(fid,'\n%18.8E%s%18.8E',2*pi*psi0_chease,' 2*PI*PSI-AXIS -->     ', 2*pi*extralines.psi0);
% $$$  fprintf(fid,'\n%18.8E%s%18.8E',SIGNIPXP*psi0_chease, ' IP_SIGN*PSI-AXIS --> [T M**2] ', SIGNIPXP*psi0_chease);
% $$$  fprintf(fid,'\n%18.8E%s%18.8E',SIGNIPXP*2*pi*psi0_chease,' IP_SIGN*2*PI*PSI-AXIS -->     ', SIGNIPXP*2*pi*psi0_chease);
% $$$ end
% $$$ if isfield(extralines,'aratio'); fprintf(fid,'\n%18.8E%s%18.8E',extralines.aratio/R0,' ASPECT RATIO ; a/R= ', extralines.aratio); end
% $$$ if isfield(extralines,'volume'); fprintf(fid,'\n%18.8E%s%18.8E',extralines.volume/R0^3, ' VOLUME -> ',extralines.volume); end
% $$$ if isfield(extralines,'area'); fprintf(fid,'\n%18.8E%s%18.8E',extralines.area/R0^2,   ' AREA   -> ',extralines.area); end
% $$$ if isfield(extralines,'length'); fprintf(fid,'\n%18.8E%s%18.8E',extralines.length/R0, ' LENGTH -> ',extralines.length); end
% $$$ if isfield(extralines,'rmin'); fprintf(fid,'\n%18.8E%s%18.8E',extralines.rmin/R0,' RMIN -> RMIN [m] ',extralines.rmin); end
% $$$ if isfield(extralines,'rmax'); fprintf(fid,'\n%18.8E%s%18.8E',extralines.rmax/R0,' RMAX -> RMAX [m] ',extralines.rmax); end
% $$$ if isfield(extralines,'zmin'); fprintf(fid,'\n%18.8E%s%18.8E',extralines.zmin/R0,' ZMIN -> ZMIN [m] ',extralines.zmin); end
% $$$ if isfield(extralines,'zmax'); fprintf(fid,'\n%18.8E%s%18.8E',extralines.zmax/R0,' ZMAX -> ZMAX [m] ',extralines.zmax); end
% $$$ if isfield(extralines,'rgeom'); fprintf(fid,'\n%18.8E%s%18.8E',extralines.rgeom/R0,' RGEOM -> RGEOM [m] ', extralines.rgeom); end
% $$$ if isfield(extralines,'aratio'); fprintf(fid,'\n%18.8E%s%18.8E',extralines.aratio,' MINOR RADIUS -> A [m] ',extralines.aratio*R0); end
% $$$
% $$$ return
