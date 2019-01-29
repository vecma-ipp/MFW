function [pest3_results] = read_pest3_output(varargin)
% [pest3_results] = read_pest3_output
% [pest3_results] = read_pest3_output(filename)
% filename is a .cdf file, if none is provided user is prompted
% returns matlab structure containing pest3 output data
%
% to get this to work the mexcdf package needs to be installed (download from the web)

% Adapted form A.Pletzer original version supplied with PEST3
% F.Felici CRPP May 2009

% paths necessary to get it to work (can be moved to standard toolbox folder later)
addpath /home/ffelici/matlab/mexcdf/mexnc;
addpath /home/ffelici/matlab/mexcdf/netcdf_toolbox/netcdf;
addpath /home/ffelici/matlab/mexcdf/netcdf_toolbox/netcdf/nctype/;
addpath /home/ffelici/matlab/mexcdf/netcdf_toolbox/netcdf/ncutility/;
addpath /home/ffelici/matlab/mexcdf/snctools

if nargin == 0
	[fname,fpath] = uigetfile('*.cdf','NetCDF files');
	file = fullfile(fpath,fname);
end
if nargin >= 1;
	file = varargin{1};
	if ~exist(file)
		error([file,' does not exist']);
	end
end
%if nargin >= 2;
%	for tel=1:nargin-1;
%		reqparams{tel} = varargin{tel+1};
%	end
%end

nc = netcdf(file, 'nowrite');

description = nc.description(:);
allvars = var(nc);                                 % Get variable data.
alldims = dim(nc);                                 % Get the dimensions.
allatts = att(nc);                                 % Get all attributes.

if ~exist('reqparams')
	% if no requested params specfied, get them all
	ncinfo = nc_info(file);
	for tel=1:length(ncinfo.Dataset)
		varname  = ncinfo.Dataset(tel).Name;
		varvalue = nc_varget(file,varname);
		eval(['o.',varname,'= varvalue;']);
	end
else % if params specified, get only those. %NOT used since dprim etc requires to get several parameters...
	for tel=1:length(reqparams)
		varname = reqparams{tel};
		try
			varvalue = nc_varget(file,varname);
		catch
			error([varname,' variable not found in ', file]);
		end
	end
end

o.deltap = o.dprim_re*o.cmatch*o.psisin^(2*o.xmu) + o.dprim_im*o.cmatch*o.psisin^(2*o.xmu) * i;
o.gammap = o.gprim_re*o.cmatch*o.psisin^(2*o.xmu) + o.dprim_im*o.cmatch*o.psisin^(2*o.xmu) * i;

nfourier = size(o.x1frbo_re, 1);
mf = -(nfourier-1)/2:(nfourier-1)/2;
[ns, nt1] = size(o.xa);
t = linspace(0, 2*pi, nt1);
cosmt = cos(t'*mf)';
sinmt = sin(t'*mf)';

o.large_solution = o.x1frbo_re'*cosmt - o.x1frbo_im'*sinmt;

o.small_solution = 0.5*( ...
o.dprim_re*interp1(o.psinod, o.xisolo_re', o.psinew)*cosmt - ...
o.dprim_im*interp1(o.psinod, o.xisolo_im', o.psinew)*sinmt + ...
o.gprim_re*interp1(o.psinod, o.xisole_re', o.psinew)*cosmt - ...
o.gprim_im*interp1(o.psinod, o.xisole_im', o.psinew)*sinmt );

o.total_solution = o.large_solution+o.small_solution;

o.rhopsi = sqrt(o.psinew/o.psimax);

pest3_results = o;

