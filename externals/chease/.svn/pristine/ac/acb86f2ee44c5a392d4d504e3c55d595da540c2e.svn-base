function [nl, fname_eff] = write_namelist_chease(fname,datanamelist,cocos_in,varargin)
%SYNTAX:
%  [nl, fname_eff] = write_namelist_chease(fname,datanamelist);
%
% INPUTS
%   fname : filename of written namelist, typical chease_namelistXXX (default: /tmp/$USER/default_namelist_chease_eff)
%
%   datanamelist : a structure containing chease namelist variables (in lowercase)
% 			additional allowed fields are:
%       datanamelist.Ipka   :  total plasma current in kA
%       datanamelist.Title  :   String containing title, to be written at top of EXPEQ
%
%   if datanamelist = 0 : uses read_namelist_chease(0) default namelist with no input files (boundary and profiles from namelist)
%   if datanamelist = 1 or empty: uses read_namelist_chease(1) default namelist assuming an EXPEQ-type file in input
%   if datanamelist = 2: uses read_namelist_chease(2) default namelist assuming an EQDSK-type file in input
%
%   cocos_in: input cocos value (optional, default=2)
%   nverbose: verbosity index (optional, default=0)
%
% OUTPUTS
%    the optional output argument nl returns the structure after corrections
%    and default values have been applied
%
%    fname_eff: written file
%


% CHECK

nverbose = 0;
if nargin>=4 && ~isempty(varargin{1})
    nverbose = varargin{1};
end

% these fields are allowed

% one day these should come from src-f90/COMDAT.inc
allowed_fields = {'aplace','awidth','b0exp','cfbal','cfnress','cplace','cpress','csspec','currt','cwidth','cwidth','dplace','dwidth', ...
    'eplace','epslon','etaei','ewidth','gamma','msmax','nbal','nblc0','nblopt','nbsexpq','nbsopt','nbstrp','nchi','ncscal','ndiagop', ...
    'ndifps','ndift','neqdsk','negp','neqdxtpo','ner','nfunc','nfunrho','ninmap','ninsca','nideal','niso','nmesha','nmeshb','nmeshc','nmeshd', ...
    'nmeshe','nmeshpol','nmeshpolexp','nopt','nplot','npoida','npoidc','npoidc','npoidd','npoide','npoidq','nppfun','npp','nppr','npropt','npsi','nrbox','nrscal', ...
    'ns','nsmooth','nsttp','nsurf','nsym','nt','ntcase','ntmf0','ntnova','nturn','nvexp','nzbox','psiscl','qplace','qspec','qwidth','r0w','z0w','r0','rz0', ...
    'r0exp','relax','rext','rpeop','rz0w','rzion','sgma','solpda','solpda','solpdc','solpdc','solpdd','solpde','solpdpol',...
    'nfixwall', 'rboxlen', 'rboxlft', 'zboxlen', 'zboxmid', ... % CHEASE namelist variables
    'rc','triang','elong','aspct','beans','ceta','theta0','rnu','xi','sgma','delta', ... % shaping
    'IpkA','Title','pedge','ap2','at2','tensbnd','tensprof','signb0xp','signipxp','cocos_in','cocos_out', ... % other allowed fields
    'nitmopt','nitmshot','nitmrun','treeitm'};

% these fields MUST be present, error otherwise
required_fields = {};

%
% Check input arguments
%
if ~exist('fname','var') || isempty(fname)
    user=getenv('USER');
    fname_eff=['/tmp/' user '/default_namelist_chease_eff'];
    if nverbose >= 1,
        warning('Name for writing namelist_chease required, uses %s',fname_eff);
    end
else
    fname_eff = fname;
end

if exist('datanamelist','var') && isstruct(datanamelist)
    datanamelist_eff = datanamelist;
elseif ~exist('datanamelist','var') || isempty(datanamelist) || datanamelist==1
    datanamelist_eff = read_namelist_chease(1);
elseif isnumeric(datanamelist) && datanamelist==2
    datanamelist_eff = read_namelist_chease(2);
elseif isnumeric(datanamelist) && datanamelist==0
    datanamelist_eff = read_namelist_chease(0);
else
    error('should not be here in write_namelist_chease');
end
if exist('cocos_in','var')
    datanamelist_eff.cocos_in = cocos_in;
else
    datanamelist_eff.cocos_in = 2;
end

% fields will be set to default values first
%nl = read_namelist_chease('/home/ffelici/matlab/CHEASEgui_develop/chease_expeq_default_namelist');
nl=struct;
% not used for now, do something nice later

% check field names and assign valid ones to 'nl' structure, replacing defaults
field_names = fieldnames(datanamelist_eff);
for tel = 1:length(field_names)
    argument = field_names{tel};
    value = datanamelist_eff.(argument);
    switch argument
        case allowed_fields
            % set structure
            nl.(argument) = value;
        otherwise
            if nverbose >= 1,
                warning('Field name %s is not allowed, check syntax or add it to allowed fields',argument);
            end
    end
end

% check whether all required fields are there
for telfields = 1:length(required_fields)
    if isempty(strfind(fieldnames(nl),required_fields{telfields}))
        error([required_fields{telfields}, ' must be specified'])
    end
end

% optional input
if isfield(nl,'Title')
    if length(nl.Title)>60; error('maximum length of ''Title'' is 60 chars'); end
    Title = num2str(nl.Title);
else
    Title = 'CHEASE NAMELIST FILE'; % default
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% CHECK FIELD VALUE CONSISTENCY   %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

allfields = fieldnames(nl);
% fields beginning with n must be scalars
for telfield = 1:length(allfields)
    thisfield = allfields{telfield};
    if strcmp(thisfield(1),'n') && sum(rem(nl.(thisfield),1))
        if nverbose >= 1, warning([thisfield ,' must be a scalar']);end
    end
end

% consistent nsttp with ncscal
if all(isfield(nl,{'nsttp','ncscal'}))
    if (nl.nsttp == 3) && (nl.ncscal == 2)
        if nverbose >= 1, warning('ncscal must not be 2 if nsttp=3');end
    elseif (nl.nsttp == 3) && (nl.ncscal == 3)
        if nverbose >= 1, warning('One should check that ncscal=3 is compatible with nsttp=3');end
    end
end

% qspec and csspec given with certain nscal
if isfield(nl,'ncscal')
    if (nl.ncscal == 1) || (nl.ncscal == 3)
        if ~all(isfield(nl,{'qspec','csspec'}))
            if nverbose >= 1, warning('must specify qspec and csspec when using ncscal=1 or ncscal=3');end
        end
    elseif (nl.ncscal == 2 && (isfield(nl,{'neqdsk'}) && nl.neqdsk~=1))
        if ~any(isfield(nl,{'IpkA','currt'}))
            if nverbose >= 1, warning('must specify IpkA or currt when using ncscal = 2');end
        end
    end
end

% warning shape params will be ignored for externally specified surface
if isfield(nl,'nsurf')
    if nl.nsurf == 2
        
    end
    if nl.nsurf == 4
        
    end
    if nl.nsurf == 6
        if any(isfield(nl,{'elong','triang','beans','ceta','sgma','aspct'}))
            if nverbose >= 1, warning('when nsurf=6, elong,triang,beans,cete,sgma,aspct will be ignored');end
        end
    end
end

% Convert IpkA in chease normalization
if isfield(nl,'IpkA')
    if ~all(isfield(nl,{'r0exp','b0exp'}))
        error('When giving IpkA you must also specify b0exp and r0exp for normalization')
    else
        nl.currt = abs(nl.IpkA./nl.r0exp./nl.b0exp.*4e-7*pi*1000);
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create namelist file "namelist_chease", input to CHEASE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fdir = fileparts(fname_eff); if isempty(fdir); fdir = '.'; end
if ~exist(fdir,'dir'); % create dir if necessary
    r=input(['Directory ',fdir,' does not exist, create it now [y/n]?','s']);
    if strfind(r,'y');
        mkdir(fdir);
    else
        error('did not create target directory')
    end
end

fid=fopen(fname_eff,'w'); % create and overwrite any other file
if fid==-1; error(['cannot open ',fname_eff]); end;

%%%%%%%%%%%%%%%% HEADER %%%%%%%%%%%%%%%
fprintf(fid,'*************************\n');
fprintf(fid,'***    %s\n',Title);
fprintf(fid,'***    namelist created by write_namelist_chease.m\n');
fprintf(fid,'*************************\n');
fprintf(fid,'&EQDATA\n');
fclose(fid);

%%%%%%%% APPEND DEFAULT NAMELIST %%%%%%%%%%%%%%%%%%%%%%
% find default_namelist
if ~exist('default_namelist_chease','file'); error('cannot find default_namelist_chease'); end
% dir where this function is located
fdir = fileparts(mfilename('fullpath'));
% append default namelist
[s,w] = unix(['cat ',fdir,'/default_namelist_chease >> ',fname_eff]);
if s; disp(w); error('Problem while attempting cat of default_namelist_chease'); end

%%%%%%%% APPEND CUSTOMIZED NAMELIST FIELDS %%%%%%%%%%%%
fopen(fname_eff,'a'); % append
chease_fields = setdiff(fieldnames(nl),{'IpkA','Title','pedge'}); % only these will actually be written to namelist file
for telfields = 1:length(chease_fields)
    thisfield = chease_fields{telfields};
    % First deal with special cases then others, so easier switches
    if strcmp(thisfield,'nitmshot') || strcmp(thisfield,'nitmrun')
        % array of 2 integers
        thisarray=nl.(thisfield);
        if length(thisarray) ~= 2 && nverbose >=1; warning('expects an array of size 2 for %s',thisfield); end
        fprintf(fid,'%s=%d, %d,\n',upper(thisfield),thisarray(1),thisarray(2));
    elseif strcmp(thisfield,'treeitm')
        % array of 2 char cell
        thisarray=nl.(thisfield);
        if length(thisarray) ~= 2 && nverbose >=1; warning('expects an array of size 2 for %s',thisfield); end
        fprintf(fid,'%s=%s, %s,\n',upper(thisfield),thisarray{1},thisarray{2});
    elseif strcmp(thisfield(1),'n')
        % fieldnames starting with 'n' are scalars except nitmshot and nitmrun
        fprintf(fid,'%s=%d,\n',upper(thisfield),nl.(thisfield));
    elseif length(nl.(thisfield))==1
        fprintf(fid,'%s=%8.7g,\n',upper(thisfield),nl.(thisfield));
    else
        % array
        thisarray=nl.(thisfield);
        fprintf(fid,'%s=%8.7g, ',upper(thisfield),thisarray(1));
        for i=2:length(thisarray)-1
            fprintf(fid,'%8.7g, ',thisarray(i));
        end
        fprintf(fid,'%8.7g,\n',thisarray(end));
    end
end
fprintf(fid,'/\n\n');
%%%%%%%% CLOSE FILE %%%%%%%%%%%%%%%
if ~fclose(fid);                            % <<< END OF DATAIN
    if nverbose >= 3, disp (['Wrote ' fname_eff]);end
else
    error('error closing file');
end

return

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% below is made obsolete by above
%% --- plasma specs ---
%fprintf(fid,'CURRT= %8.7f, R0EXP=%3.1f, B0EXP=%3.1f,\n',
%    nl.rtor, nl.btor);
%fprintf(fid,'NSURF=%3.3f, ELONG=%3.3f, TRIANG=%3.3f, BEANS=%3.3f, CETA=%3.3f, SGMA=%3.3f, ASPCT=%3.3f,\n\n',nl.nsurf,nl.elong,nl.delta,nl.beans,nl.ceta,nl.sgma,nl.aspct);
%% --- CHEASE parameters ---
%fprintf(fid,'NIDEAL=6, NPLOT=1, NDIAGOP= 1, NBSEXPQ=000,\n');
%fprintf(fid,'RELAX=0.3, NSMOOTH=1, EPSLON=1.0E-8, GAMMA=1.6667,\n');
%fprintf(fid,'NINMAP=30, NINSCA=30, NRBOX=256, NZBOX=256, PSISCL=1.0, MSMAX=1,\n');
%fprintf(fid,'NS=%3.0f, NT=%3.0f, NPSI=%3.0f, NCHI=%3.0f, NISO=%3.0f, NTNOVA=12,\n', nl.ns, nl.nt, nl.npsi, nl.nchi,nl.nchi);
%fprintf(fid,'NEQDSK=%d, NPROPT=%d, NOPT=0,\n',nl.npropt);
%fprintf(fid,'NPPFUN=4, NFUNRHO=%d, NSTTP=%d, NFUNC=4,\n\n',nl.nfunrho,nl.nsttp);
%% --- CHEASE subroutines, ie. ballooning stab. (NBAL) and jbs optimizations (NBSTRP) ---
%fprintf(fid,'NBLOPT=0, NBAL=1, CFBAL=10.00, NTURN=10, NPPR=24, NBLC0=16,\n');
%fprintf(fid,'NBSOPT=0, NBSTRP=1, ETAEI=0.1, RPEOP=0.70, RZION=1.5, \n\n');
%% --- CHEASE scaling ---
%fprintf(fid,'CPRESS=%2.0f, QSPEC=%8.7f, CSSPEC=%8.7f,\n',nl.cpress,nl.qspec,nl.csspec);
%fprintf(fid,'CFNRESS=1.00, NRSCAL=0, NCSCAL=%d, NTMF0=0,\n\n', nl.ncscal);
%% --- CHEASE mesh densification ---
%fprintf(fid,'NDIFPS=1, NDIFT=1,\n');
%fprintf(fid,'NMESHA=2, NPOIDQ=10, SOLPDA=.30,\n');
%fprintf(fid,'QPLACE=0.95, 0.95, 1.00, 1.00, 2.00, 2.00, 3.00, 3.00, 4.00, 4.00,\n');
%fprintf(fid,'QWIDTH=0.10, 0.06, 0.02, 0.08, 0.05, 0.02, 0.05, 0.02, 0.04, 0.01,\n\n');
%fprintf(fid,'NMESHA=1, NPOIDA=10, SOLPDA=.40,\n');
%fprintf(fid,'APLACE=0.95, 0.96, 0.96, 0.97, 0.97, 0.98, 0.98, 0.99, 0.99, 1.00,\n');
%fprintf(fid,'AWIDTH=0.10, 0.10, 0.02, 0.10, 0.02, 0.10, 0.02, 0.10, 0.02, 0.10,\n\n');
%fprintf(fid,'NMESHB=1, NPOIDC=4, SOLPDC=.70,\n');
%fprintf(fid,'CPLACE=.95, .97, .99, 1.0,\n');
%fprintf(fid,'CWIDTH=.05, .02, .02, .02,\n\n');
%fprintf(fid,'NMESHC=1, NPOIDC=4, SOLPDC=.70,\n');
%fprintf(fid,'CPLACE=0.95, 0.97, 0.99, 1.0,\n');
%fprintf(fid,'CWIDTH=0.05, 0.02, 0.02, 0.02,\n\n');
%fprintf(fid,'NMESHD=0, NPOIDD=2, SOLPDD=.60,\n');
%fprintf(fid,'DPLACE=-1.80, -1.80, 4.0,\n');
%fprintf(fid,'DWIDTH=0.18, 0.08, 0.05,\n\n');
%fprintf(fid,'NMESHE=0, NPOIDE=4, SOLPDE=.50,\n');
%fprintf(fid,'EPLACE=-1.70, -1.70, 1.70, 1.70,\n');
%fprintf(fid,'EWIDTH=0.18, 0.08, 0.18, 0.08,\n\n');
%% --- CHEASE re-run parameters ---
%fprintf(fid,'/\n\n');

%-- 2/10/09 10:00 AM --%
