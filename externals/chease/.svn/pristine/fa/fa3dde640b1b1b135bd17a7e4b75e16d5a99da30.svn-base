% reads the CHEASE output file hamada.dat and builds a matlab structure with the information it contains
%	function [H]=read_hamada(flnm,flpth)
% Inputs:
%		flnm	file name
%		flpth file path, if no path is entered, the default path is used
% This routine is not written at all in a general way, therefore it only works for a specific output file structure

function [H]=read_hamada(flnm,flpth)


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
	sss=deblank(fgets(fid));
	eval(['H.' lower(sss) '=fscanf(fid,''%f'',H.ns);'])
	fgets(fid);

% 1_D quantities
for ii=1:15
	sss=deblank(fgets(fid));
	eval(['H.' lower(sss) '=fscanf(fid,''%f'',H.npsi);'])
	fgets(fid);
end

% 2-D quantities
for ii=1:20
	sss=deblank(fgets(fid));
	tmp=fscanf(fid,'%f',H.npsi*H.ns);
	tmp=reshape(tmp,H.npsi,H.ns);
	eval(['H.' lower(sss) '=tmp;'])
	fgets(fid);
end

fclose(fid);
