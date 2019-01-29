function [EXPTNZdataout, fclose_out] = write_exptnz(EXPTNZdata,fnamefull,varargin)
% ---------------------------------------------------------------------
% Script to write EXPTNZ from structure, creates EXPTNZ file in current
% directory.
% ---------------------------------------------------------------------
% SYNTAX:
%  stat =  write_exptnz(EXPTNZdata,fnamefull)
% 
% OUTPUT: 
% EXPTNZdataout: EXPTNZdata structure including .fname, .fnamefull and .pathname
% fclose_out: status flag of fclose at the end of the function (0 if successful)
%
% INPUTS:
% fnamefull: filename of written EXPTNZ. Typical example 'EXPTNZ_xxx'
% nverbose : verbosity level
%
% EXPTNZdata is a structure with the following fields:
% 	EXPTNZdata.rhopsi	: a normalized rhopsi
% 	EXPTNZdata.te [eV]	: electron temperature profile
% 	EXPTNZdata.ne [m-3] 	: electron density profile
% 	EXPTNZdata.zeff		: z effective profile (may also give a scalar)
% 	EXPTNZdata.ti [eV]	: ion temperature profile
% 	[EXPTNZdata.ni [eV]	: ion density profile not always existing since can be extracted from zeff]
%
% see also write_expeq, read_exptnz, plot_exptnz

% F.Felici CRPP 2009

% check inputs

nverbose = 0;
if nargin>=3 && ~isempty(varargin{1})
    nverbose = varargin{1};
end

rhopsi = EXPTNZdata.rhopsi;
if numel(rhopsi) <= 1; error('rhopsi must be a vector'); end
if numel(EXPTNZdata.te) ~= numel(rhopsi); error('te must be a vector of the same size of rhopsi'); end
if numel(EXPTNZdata.ti) ~= numel(rhopsi); error('ti must be a vector of the same size of rhopsi'); end
if numel(EXPTNZdata.ne) ~= numel(rhopsi); error('ne must be a vector of the same size of rhopsi'); end
if isfield(EXPTNZdata,'ni')
  if numel(EXPTNZdata.ni) ~= numel(rhopsi); error('ni must be a vector of the same size of rhopsi'); end
end

if numel(EXPTNZdata.zeff) == 1; 
  EXPTNZdata.zeff = ones(size(rhopsi))*EXPTNZdata.zeff;
elseif numel(EXPTNZdata.zeff) ~= numel(rhopsi);
  error('zeff must be a scalar or vector of same size as rhopsi');
end

EXPTNZdataout=EXPTNZdata;

[pathname,fname,fext]=fileparts(fnamefull);
if isempty(pathname); pathname=pwd; end
EXPTNZdataout.fname=[fname fext];
EXPTNZdataout.fnamefull=fullfile(pathname,EXPTNZdataout.fname);
EXPTNZdataout.pathname=pathname;


fid=fopen(fnamefull,'w');
if fid == -1
	error(['error opening ',fnamefull,' for writing'])
end
fprintf(fid,' %d   rhopsi, Te, ne, Zeff, Ti, ni profiles\n',length(rhopsi));
fprintf(fid,'%16.6E\n',rhopsi);
fprintf(fid,'%16.6E\n',EXPTNZdata.te);
fprintf(fid,'%16.6E\n',EXPTNZdata.ne);
fprintf(fid,'%16.6E\n',EXPTNZdata.zeff);
fprintf(fid,'%16.6E\n',EXPTNZdata.ti);
if isfield(EXPTNZdata,'ni')
  fprintf(fid,'%16.6E\n',EXPTNZdata.ni);
end

fclose_out = fclose(fid);

if ~fclose_out
    if nverbose >= 3, disp(['Wrote ',fnamefull]);end
else
    error('something went wrong writing %s',fnamefull)
end
%----------- end of EXPTNZ -------------
return
