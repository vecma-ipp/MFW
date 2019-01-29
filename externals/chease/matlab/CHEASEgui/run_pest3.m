function [fnames_out,ss]= run_pest3(varargin)
%RUN_PEST3 can be used to run the PEST3 stability code within matlab
%
%   [fnames_out,ss] = run_pest3('argument','value',...);
%    or
%   [fnames_out,ss] = run_pest3(params);
%
% INPUTS
%%   argument(/parameter field) options:
%   'n' 		: Toroidal mode number (2)
%   'm' 		: Poloidal mode number (3)
%   'q0' 		: axis safety factor - rescales q profile 
%   'qedge' 	: edge safety factor - rescales q profile 
%   'elements' 	: number of radial finite elements (can be array)
%   'fourier' 	: number of fourier modes 
%   'inputfile'	: full path of input file - prompt will appear if not specified
%   'inputfiletype': type of input file (only 'eqdsk', 'eqdsk-esc',and 'inp' implemented so far)
%   'wall'   	: distance of conducting wall, specified as (r_wall-r_edge)/a
%   'display'   : display pest3 command line output ('y'/'n')
%   'path_out'  : path where output files will be stored (default: input file path)
%
% OUTPUTS 
%   fnames_out	: names of output files, including .cdf file to be read with read_pest3_output.m
%   ss		: number of ideal instabilities, if >0 then something is very wrong
%
%  EXAMPLES
%   Run PEST3 for 2/1 mode using argument-value pairs;
%      	fname = '/home/ffelici/my_EQDSK'
%      	[s,DeltaP] = run_pest3('n',1,'m',2,'inputfile',fname)
%
%   Run PEST3 for 3/2 mode (default) specifying q0=1.05, using params structure: 
% 	params.q0 = 1.05;
% 	params.inputfile = '/home/ffelici/my_EQDSK'
% 	[s,DeltaP] = run_pest3(params)
%
%   details about input options can be obtained from the PEST3 help
%   !pest3.2 -h
%
% F. Felici CRPP 2009

% Default parameters
params.n = 2;
params.m = 3;

% check and assign parameters
allowed_fields = {'n','m','inputfiletype','inputfile','q0','q_edge','elements','fourier','wall','display','path_out'};

if nargin ==1 % params specified in structure
	if ~isstruct(varargin{1})
		error('arguments must be a single structure or must come in pairs')
	end	
	% check field names and assign to structure
	field_names = fieldnames(varargin{1});
	for tel = 1:length(field_names)
		argument = field_names{tel}; 
		value = getfield(varargin{1},argument);
		switch argument
			case allowed_fields
				% set structure
				params = setfield(params,argument,value);
			otherwise
				error(['unrecognized argument name ',argument])
		end
	end
elseif rem(nargin,2) == 0 
	% load user defined parameters
	for tel=1:(nargin/2)
		argument = varargin{2*(tel-1)+1};
		value = varargin{2*tel};
		switch argument
			case allowed_fields
				params = setfield(params,argument,value)
			otherwise
				error(['unrecognized argument name ',argument])
		end
	end
else
	% odd number of varargin >2
	error('arguments must be a single structure or must come in pairs')
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHECK input parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% input file and path
if ~isfield(params,'inputfile')
	fprintf('User did not specify input file \n');
	[fname,fpath] = uigetfile({'*eqdsk*;*EQDSK*','EQDSK files (*EQDSK*)'},'Select EQDSK files to use as input for PEST3');
	if isequal(fname,0) | isequal(fpath,0); error('user canceled'); end
	params.inputfile = fullfile(fpath,fname);
elseif ~exist(params.inputfile); 
	error(['Input file ',params.inputfile,' does not exist']);
end
% get complete path and file name
[fpath,fname,fext] = fileparts(params.inputfile); % path of fname
inputfname = [fname,fext]; % filename only, without path
if isempty(fpath); fpath = pwd; end;

% output path
if isfield(params,'path_out')
	path_out = params.path_out;
	if ~exist(path_out,'dir'); error([path_out,' directory does not exist']); end
else % default: same as input path
	path_out = fpath;
end

% q0 and qedge
if isfield(params,'q0') & isfield(params,'qedge')
	error('you can not specify both q0 and qedge')
end

% check that inputfiletype is specified, else try to get it automatically, else error
if ~isfield(params,'inputfiletype')
	if strfind(lower(params.inputfile),'eqdsk');
		params.inputfiletype = 'eqdsk';
		fprintf('\ninput file type not specified, guessed ''eqdsk''\n');
	elseif strfind(lower(params.inputfile),'inp');
		params.inputfiletype = 'inp';
		fprintf('\ninput file type not specified, guessed ''inp''\n');
	else
		error('inputfiletype not specified and could not guess it from inputfile name');
	end
end
	
%% CHECK that user can actually run PEST3 remotely
[s,w] = unix('whoami'); 
if ~strcmp(deblank(w),'ffelici');
	disp('WARNING, to run pest3 you need to:') 
	disp(' *have set up a remote ssh key on crpppc361'); 
	disp(' *make sure the correct modules are loaded automatically on the remote PC (typically in .cshrc)')
	disp(' *create a directory crpppc361:~/PEST3/ to which the input file will be copied')
	disp(' ');
	userkey = input('continue?','s'); if ~strcmp(userkey,'y'); return; end;
end

fprintf('Checking crpppc361 setup.')
% try whether the help works, if not there is something wrong with either the ssh or the modules on the remote pc
% in the current implemtation, the modules are loaded at startup in the .cshrc file
[s,w] = unix('ssh crpppc361 pest3.2 -h');
if s~=1; 
	error('could not run pest3.2 remotely by ''ssh crpppc361 pest3.2 -h'', are you sure a password-free ssh is set up?')
end
fprintf('.');
% check if remote ~/PEST3/ directory exists
[s,w] = unix('ssh crpppc361 ls ~/PEST3/');
if s
	error(w)
end
% if everything ok, continue
fprintf('ok.  ');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Construct string to call pest3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
optstring = '';
optstring = [optstring, ' -n',int2str(params.n)];
optstring = [optstring, ' -m',int2str(params.m)];
if isfield(params,'elements')
	elemstring = sprintf('%d ',params.elements); elemstring = elemstring(1:end-1); % cut trailing space
	optstring = [optstring,' -k\"',elemstring,'\"'];
end
if isfield(params,'fourier')
	optstring = [optstring,' -l',int2str(params.fourier)];
end
if isfield(params,'q0')
	optstring = [optstring,' -q-',num2str(params.q0)];
end
if isfield(params,'qedge')
	optstring = [optstring,' -q',num2str(params.qedge)];
end
if isfield(params,'wall')
	optstring = [optstring,' -b',int2str(params.wall)];
end
if isfield(params,'inputfiletype')
	switch params.inputfiletype
		case 'inp'
			optstring = [optstring,' -i-1'];
		case 'eqdsk-esc'
			optstring = [optstring,' -i3'];
		case 'eqdsk'
			optstring = [optstring,' -i4'];
		otherwise
			error(['Unknown inputfiletype ''',params.inputtype,'''']);
		end
end
optstring = [optstring,' -f',['~/PEST3/',inputfname]]; % default name on remote PC

% COPY EQDSK to remote PC
fprintf(['\nCopying: ',params.inputfile,' to remote PC...']);
[s,w] = unix(['scp ',params.inputfile,' crpppc361:~/PEST3/',inputfname]);
if s; error('error copying file to remote PC'); end

fprintf('Running PEST3...');
% RUN PEST3
[s,w] = unix(['ssh crpppc361 pest3.2',optstring]);
w_pest3 = w;
% [s,w] = unix(['pest3.2',optstring]); to run locally (not the case)

% COPY stuff back
% later.. now only use w shell output to get deltaprime

% display output if asked for or if error
if (isfield(params,'display') & strcmp(params.display,'y')) | s ~=0
		disp('\n*** PEST3 call ***');
		disp(['pest3.2',optstring]);
		disp('');
		disp('*** PEST3 output ***');
		disp('');
		disp(w);
end

if s~=0
	error('*** PEST3 failed ***')
else
	% move output files, copy them to local system
	[s,w] = unix(['ssh crpppc361 mv pest3* PEST3/']);
	%if s; disp(w); error('error moving remote output files'); end;
	[s,w] = unix(['scp crpppc361:PEST3/pest3* ',path_out]);
	if s; disp(w); error('error copying output files'); end;
	% store output file names
	fnames_out = {fullfile(path_out,'pest3.cdf'),fullfile(path_out,'pest3.log'),fullfile(path_out,'pest3_l.log')};
	fprintf('done\n');	
	[DeltaP,ss] = parse_pest3_output(w_pest3); % old
end

% no longer used function for parsing deltap and ideal instabilities from PEST stdout
% only ss is used, but to be replaced once I understand how to extract the # of ideal instabilities from the .cdf
function [deltap,idealinstab] = parse_pest3_output(w);
% deltap = parse_pest3_output(w);
% reads complex delatp from terminal output of w
% also reads # of ideal instabilities
Deltapstr = regexp(w,'Delta'' = (.*)\+.*\+ i .*\n','tokens');
deltap = eval(strrep(Deltapstr{1}{1},'i ','i*'));

% ideal instabilities
Iinstabstr = regexp(w,'(\d) ideal','tokens');
idealinstab = eval(Iinstabstr{1}{1});
