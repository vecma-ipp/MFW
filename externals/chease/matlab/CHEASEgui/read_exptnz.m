function [EXPTNZdata] = read_exptnz(varargin);
% ---------------------------------------------------------------------
% Script to read data from EXPTNZ file
% ---------------------------------------------------------------------
% SYNTAX:
%   EXPTNZdata=read_exptnz(fnamefull);
%
% If no varargin are given, prompt for a file
% fnamefull : filename of expeq file to read

%
% ---------------------------------------------------------------------
% see also write_exptnz and plot_exptnz

% defaults
display = 0;

% check file name and input
if nargin ==0
  [fname,pathname] = uigetfile([{'EXPTNZ*;exptnz*'},{'EXPTNZ* or exptnz* files'}; ...
                    {'*EXPTNZ*;*exptnz*'},{'*EXPTNZ* or *exptnz* files'};{'*'},{'All files'}],'Select an EXPTNZ file');
  if isequal(fname,0) || isequal(pathname,0); return; end
  fnamefull = fullfile(pathname, fname);
elseif nargin >= 1;
  fnamefull = varargin{1};
  if ~exist(fnamefull); 
    disp([fnamefull,' does not exist']); 
    [fname,pathname] = uigetfile([{'EXPTNZ*;exptnz*'},{'EXPTNZ* or exptnz* files'}; ...
                    {'*EXPTNZ*;*exptnz*'},{'*EXPTNZ* or *exptnz* files'};{'*'},{'All files'}],'Select an EXPTNZ file');
    if isequal(fname,0) || isequal(pathname,0); return; end
    fnamefull = fullfile(pathname, fname);
  else
    ii=findstr(fnamefull,filesep);
    if ~isempty(ii)
      fname=fnamefull(ii(end)+1:end);
      pathname=fnamefull(1:ii(end)-1);
    else
      fname=fnamefull;
      pathname=pwd;
    end
  end
end

EXPTNZdata.fnamefull=fnamefull;
EXPTNZdata.fname=fname;
EXPTNZdata.pathname=pathname;

if nargin >= 2;
  display = varargin{2};
end

% read EXPTNZ and write to structure
fid = fopen(fnamefull); if fid==-1; error(['error opening ',fnamefull]); end;

% has text after nb of points to comment file, so mimic fortran read:
abc=fscanf(fid,'%c');
iilines=find(double(abc)==10);
EXPTNZdata.nrho =  sscanf(abc(1:iilines(1)-1),'%d\n',1);
clear exptnz
for ii=2:length(iilines)
  exptnz(ii-1)=sscanf(abc(iilines(ii-1)+1:iilines(ii)-1),'%f\n',1);;
end
nb_arrays=length(exptnz)/EXPTNZdata.nrho;
if nb_arrays>=1
  jj=1;
  EXPTNZdata.rhopsi = exptnz((jj-1)*EXPTNZdata.nrho+1:(jj)*EXPTNZdata.nrho);
end
if nb_arrays>=2
  jj=jj+1;
  EXPTNZdata.te = exptnz((jj-1)*EXPTNZdata.nrho+1:(jj)*EXPTNZdata.nrho);
end
if nb_arrays>=3
  jj=jj+1;
  EXPTNZdata.ne = exptnz((jj-1)*EXPTNZdata.nrho+1:(jj)*EXPTNZdata.nrho);
end
if nb_arrays>=4
  jj=jj+1;
  EXPTNZdata.zeff = exptnz((jj-1)*EXPTNZdata.nrho+1:(jj)*EXPTNZdata.nrho);
end
if nb_arrays>=5
  jj=jj+1;
  EXPTNZdata.ti = exptnz((jj-1)*EXPTNZdata.nrho+1:(jj)*EXPTNZdata.nrho);
end
if nb_arrays>=6
  jj=jj+1;
  EXPTNZdata.ni = exptnz((jj-1)*EXPTNZdata.nrho+1:(jj)*EXPTNZdata.nrho);
end

return
