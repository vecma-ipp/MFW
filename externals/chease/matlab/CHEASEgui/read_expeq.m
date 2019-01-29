function [EXPEQdata] = read_expeq(varargin)
% ---------------------------------------------------------------------
% Script to read data from EXPEQ file
% ---------------------------------------------------------------------
% SYNTAX:
%   EXPEQdata=read_expeq(fname{,display});
%
% If no varargin are given, prompt for a file
% fname : filename of expeq file to read
% display : set to display additional comments data at end of file
%
% ---------------------------------------------------------------------
% see also write_expeq and plot_expeq

% defaults
display = 0;

% check file name and input
if nargin ==0
    [fname,pathname] = uigetfile([{'EXPEQ*;expeq*'},{'EXPEQ* or expeq* files'}; ...
        {'*EXPEQ*;*expeq*'},{'*EXPEQ* or *expeq* files'};{'*'},{'All files'}],'Select an EXPEQ file');
    if isequal(fname,0) || isequal(pathname,0); return; end
    fnamefull = fullfile(pathname, fname);
elseif nargin >= 1;
    fnamefull = varargin{1};
    if ~exist(fnamefull,'file');
        disp([fnamefull,' does not exist']);
        [fname,pathname] = uigetfile([{'EXPEQ*;expeq*'},{'EXPEQ* or expeq* files'}; ...
            {'*EXPEQ*;*expeq*'},{'*EXPEQ* or *expeq* files'};{'*'},{'All files'}],'Select an EXPEQ file');
        if isequal(fname,0) || isequal(pathname,0); return; end
        fnamefull = fullfile(pathname, fname);
    else
        [pathname,fname,ext] = fileparts(fnamefull);
        fname = [fname,ext];
        if isempty(pathname)
            pathname=pwd;
        end
    end
end

if nargin >= 2;
    display = varargin{2};
end
EXPEQdata.fnamefull=fnamefull;
EXPEQdata.fname=fname;
EXPEQdata.pathname=pathname;

% read expeq and write to structure
fid = fopen(fnamefull); if fid==-1; error(['error opening ',fnamefull]); end;
EXPEQdata.epsilon = fscanf(fid,'%E',1);
EXPEQdata.zgeom = fscanf(fid,'%E',1);
EXPEQdata.pedge = fscanf(fid,'%E',1);
EXPEQdata.n_psi = fscanf(fid,'%d',1);
EXPEQdata.RZ_psi = fscanf(fid,'%E %E',[2,EXPEQdata.n_psi]).';
fgetl(fid); % to get end of line of last RZ point
tempstr=fgetl(fid); % to get nb of points and (optional) nppfun value (4=pprime, 8=p)
temp = sscanf(tempstr,'%d');
EXPEQdata.n_rho = temp(1);
nppfun=4;
if length(temp)>= 2
    nppfun=temp(2);
end
EXPEQdata.nppfun=nppfun;
if EXPEQdata.nppfun==8
    disp('Note that Pressure and not Pprime is given in Pprime array')
end
temp = fscanf(fid,'%d',[1,2]);
EXPEQdata.nsttp = temp(1);
EXPEQdata.nrhotype = temp(2);
EXPEQdata.rho = fscanf(fid,'%E',EXPEQdata.n_rho);
EXPEQdata.Pprime = fscanf(fid,'%E',EXPEQdata.n_rho);

% depending on run mode (nsttp), different profiles are assigned
EXPEQdata.TTprime = []; EXPEQdata.Istar = []; EXPEQdata.Jparallel = [];
switch EXPEQdata.nsttp
    case 1, cu_name = 'TTprime';
    case 2, cu_name = 'Istar';
    case 3, cu_name = 'Jparallel';
end
EXPEQdata.(cu_name) = fscanf(fid,'%f',EXPEQdata.n_rho);
fgetl(fid); % to get end of line of last profile point

% display footer of EXPEQ
S = textscan(fid,'%s','Delimiter','\n','whitespace','');
footer = S{1};
fclose(fid);

if exist('footer','var');
    EXPEQdata.footer = char(footer);
    EXPEQdata.extralines = footer;
else
    % TODO: (Most) probably never happens
    EXPEQdata.extralines = {};
end

if display
    fprintf('\n')
    disp('*** Displaying additional information at end of file ***');
    fprintf('\n');
    if isfield(EXPEQdata,'footer');
        disp(EXPEQdata.footer)
    end
    fprintf('\n')
    disp('*** end of EXPEQ file ***')
end
return
