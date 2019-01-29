% reads the CHEASE output file neoart.dat and builds a matlab structure with the information it contains
%	function [H]=read_neoart(flnm,flpth)
% Inputs:
%		flnm	file name
%		flpth file path, if no path is entered, the default path is used
% This routine is not written at all in a general way, therefore it only works for a specific output file structure

function [H]=read_neoart(flnm,flpth)


% default path
if ~exist('flpth')||isempty(flpth)
   flpth='./';
end

% reads file
if unix(['test -e ' flpth flnm])==0
	fid = fopen([flpth flnm], 'r');
else	
	error(['The file ' flpth flnm ' does not exist' ])
end
frewind(fid);
sss='';

% scalars
for ii=1:5
	sss=deblank(fgets(fid));
	eval(['H.' lower(sss) '=fscanf(fid,''%f'',1);'])
	fgets(fid);
end

% grid
	sss=deblank(fgets(fid));
	eval(['H.' lower(sss) '=fscanf(fid,''%f'',H.npsi);'])
	fgets(fid);

% 1_D quantities
for ii=1:25
	sss=deblank(fgets(fid));
	eval(['H.' lower(sss) '=fscanf(fid,''%f'',H.npsi);'])
	fgets(fid);
end


fclose(fid);
