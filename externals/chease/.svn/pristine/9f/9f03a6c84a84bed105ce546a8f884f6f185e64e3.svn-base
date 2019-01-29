function [fname_out,globalsvalues,namelist_struct,namelistfile_eff] = run_chease(namelistfile,inputfile,cocos_in,varargin)
%
% [fname_out,globalsvalues,namelist_struct,namelistfile_eff] = run_chease(namelistfile,inputfile,cocos_in,varargin)
% [fname_out,globalsvalues,namelist_struct,namelistfile_eff] = run_chease(namelistfile,inputfile,cocos_in,{exptnzfile,suffix,chease_exe,nverbose})
%
% [~,~,namelist_struct] = run_chease; % returns the default namelist structure (EXPEQ)
% [~,~,namelist_struct] = run_chease(ii); % returns the default namelist structure for case 1, 2 or 0
%
% namelistfile: full path to CHEASE namelist (1 (default): if want default expeq namelist, 2 for default "eqdsk" namelist, 0 for an example with boundary and profiles given in namelist only)
% inputfile   : full path to input file (EXPEQ/EQDSK file)
% cocos_in    : cocos of input eqdsk (2 by default, not needed if EXPEQ file given)
% exptnzfile  : (optional) full path to EXPTNZ file
% suffix      : (optional) suffix to written files
% chease_exe  : chease executable name (default=chease) which should be in your path, set chease_develop to have develop version, chease_lac5, etc
% nverbose    : verbosity level
%
% fname_out   : cell containing full path of output files
% globalsvalues: structure containing main global results extracted from output: q0, qedge, q95, betaN, betap, li, kappa, delta, etc
% namelist_structure: variables and values in a matlab structure (from read_namelist)
% namelistfile_eff : namelist filename used for running chease
%
% Examples

fname_out='';
globalsvalues=[];

if ~exist('cocos_in','var')
    cocos_in=2;
end

if ~exist('namelistfile','var') || isempty(namelistfile)
    [nl,namelistfile_eff] = write_namelist_chease([],1,cocos_in);
elseif isnumeric(namelistfile) || isstruct(namelistfile)
    [nl,namelistfile_eff] = write_namelist_chease([],namelistfile,cocos_in);
else
    namelistfile_eff = namelistfile;
    nl = read_namelist_chease(namelistfile_eff);
end
namelist_struct = nl;

if nargin == 0
    return
end
if nargin ==1 && ~isempty(namelistfile) && isnumeric(namelistfile) && namelistfile~=0
    % needs inputfile except for namelistfile=0 option
    return
end

if ~exist('inputfile','var') || isempty(inputfile)
    if exist('namelistfile','var') && isnumeric(namelistfile) && namelistfile==0
        % no need of input files
        inputfile_eff = [];
    else
        % if nverbose >= 1,
        disp('inputfile not provided');
        % end
        return
    end
else
    inputfile_eff = inputfile;
end

if nargin>=4 && ~isempty(varargin{1});
    exptnzfile=varargin{1};
    usetnz = 1;
    if ~exist(exptnzfile,'file')
        error([exptnzfile,' does not exist']);
    end
else
    usetnz = 0;
    tnzignored = 0;
end

if nargin>=5  && ~isempty(varargin{2}) % file suffix manually set!
    suffix = varargin{2};
end

chease_exe = 'chease';
if nargin>=6  && ~isempty(varargin{3})
    chease_exe = varargin{3};
end

nverbose = 0;
if nargin>=7 && ~isempty(varargin{4})
    nverbose = varargin{4};
end

if ~exist(namelistfile_eff,'file')
    error([namelistfile_eff,' does not exist']);
end
if ~isempty(inputfile_eff) && ~exist(inputfile_eff,'file')
    error([inputfile_eff,' does not exist']);
end

% set up tmpdir, create if necessary
[s,uname] = unix('whoami');
if s, disp(uname);error('error getting username'); end
tmpdir = fullfile(tempdir,deblank(uname));
if ~exist(tmpdir,'dir'), mkdir(tmpdir); end

if ~isempty(inputfile_eff)
    
    [astat,aresult] = copyfile(inputfile_eff,fullfile(tmpdir,'EXPEQ'),'f'); %copy the expeq of actual equilibrium to EXPEQ(file which chease uses)
    if ~astat; disp(aresult); error('error copying EXPEQ'); end
    [bstat,bresult] = copyfile(namelistfile_eff,fullfile(tmpdir,'chease_namelist'),'f'); %copy the namelist
    if ~bstat; disp(bresult); error('error copying namelist'); end
end
if usetnz
    [cstat,cresult] = copyfile(exptnzfile,fullfile(tmpdir,'EXPTNZ'),'f'); %copy the exptnz file to the same dir
    if ~cstat; disp(cresult); error('error copying EXPTNZ'); end
    % CHECK that the namelist has the right option for EXPTNZ
    [s,w] = unix(['grep NBSEXP ',namelistfile_eff,' < /dev/null']);
    if s, disp(w);error('error grepping namelist'); end
    eval(strrep(w,',',';')); % sets value of NBSEXP. replace commas with ; to silence
    if ~exist('NBSEXPQ','var'),
        % Maybe this error comes from the standard input being fed to unix commands by default (see Tips section in unix command's help).
        % Let's see if this happens again with the addition of a /dev/null feed.
        warning('NBSEXPQ was not set. Something is wrong. Probably grep found no match. But why ? let''s find out ...');
        keyboard;
    elseif NBSEXPQ ~= 1111,
        if nverbose >= 1,
            warning('EXPTNZ file specified but NBSEXPQ is not 1111 in namelist! TNZ data will not be taken into account');
        end
        tnzignored = 1;
    end
end

% determine output file name (same as expeq but remove expeq)
if ~isempty(inputfile_eff)
    fname_expeq = dir(inputfile_eff);
    out_suffix = fname_expeq.name(strfind(lower(fname_expeq.name),'expeq')+length('expeq'):end);
    if isempty(out_suffix); out_suffix = fname_expeq.name; end % if nonstandard name
else
    out_suffix = 'stdrun';
end
if strcmp(out_suffix(1),'.') || strcmp(out_suffix(1),'_');
    out_suffix = out_suffix(2:end);
end
fname_out = fullfile(tmpdir,['o.chease.',out_suffix]);
ofname_out = fname_out;

% copy namelist
[cstat,cresult] = copyfile(fullfile(tmpdir,'chease_namelist'),fname_out,'f');
if ~cstat; disp(cresult); error('error copying namelist'); end

% RUN CHEASE
if nverbose >= 3,
    fprintf(['\nRunning ' chease_exe ' ...\n'])
    disp(['  namelist: ',namelistfile_eff])
    disp(['     EXPEQ: ',inputfile_eff])
    if usetnz
        disp(['    EXPTNZ: ',exptnzfile])
    elseif tnzignored
        disp(['    EXPTNZ: ',exptnzfile,' *** IGNORED ***'])
    else
        disp('    EXPTNZ: *** none ***')
    end
    disp(['    Output: ',fname_out]);
end

chease_launchtime = now;

olddir = pwd; cd(tmpdir);
%[dstat,dresult] = unix(['/home/sauter/bin/chease < ',namelistfile_eff,'>> ',fname_out]);
% chease should be in path, if it is not, assume in /home/sauter/bin
[a,~]=unix(['which ' chease_exe]);
if a==0
    [dstat,dresult] = unix([chease_exe ' >> ''',fname_out,'''']);
elseif exist(['/home/sauter/bin/' chease_exe],'file')
    [dstat,dresult] = unix(['/home/sauter/bin/' chease_exe ' >> ''',fname_out,'''']);
elseif exist(['/home/osauter/bin/' chease_exe],'file')
    [dstat,dresult] = unix(['/home/osauter/bin/chease >> ''',fname_out,'''']);
else
    error('Error no chease executable was found');
end
if exist('fort.0','file'); unix(['cat fort.0 >> ' fname_out]); end
if exist('fort.6','file'); unix(['cat fort.6 >> ' fname_out]); end

cd(olddir);
% append result to o.xxx file
% check that all went well, else display error
if dstat; disp(dresult); error(['error running ' chease_exe]); end

%transform the output file of chease in something readable by Matlab
if nverbose >= 3,
    fprintf('done....\n Transforming CHEASE output file in columns.... ')
end
[a,~]=unix('which o.chease_to_cols');
if a==0
    [fstat,fresult]=unix(['o.chease_to_cols ''',fname_out,''' ''',fname_out,'.cols''']);
elseif exist('/home/sauter/bin/o.chease_to_cols','file')
    [fstat,fresult]=unix(['/home/sauter/bin/o.chease_to_cols ''',fname_out,''' ''',fname_out,'.cols''']);
elseif exist('/home/osauter/bin/o.chease_to_cols','file')
    [fstat,fresult]=unix(['/home/osauter/bin/o.chease_to_cols ''',fname_out,''' ''',fname_out,'.cols''']);
else
    error('Error no o.chease_to_cols executable was found');
end
if fstat; disp(fresult); error('error converting to cols'); end
if nverbose >= 3,
    fprintf(['..done\n cols file: ' fname_out '.cols\n'])
end

if ~any([~cstat,dstat,fstat])
    % any files created after chease was launched are also outputs
    filestmpdir = dir(tmpdir);
    % save if created after chease launched and if a file (not dir)
    filestmpdir = filestmpdir(~cellfun(@isempty,{filestmpdir(:).datenum}));
    mask =  ([filestmpdir.datenum] > chease_launchtime) & ~[filestmpdir.isdir];
    fname_out = cellfun(@(x) fullfile(tmpdir,x),{filestmpdir(mask).name},'UniformOutput',false).';
else
    if ~cstat; disp(cresult); end
    if  dstat; disp(dresult); end
    if  fstat; disp(fresult); end
    error(['Error running ' chease_exe ' or converting outputs to cols'])
end

if ~exist('suffix','var') 
    if ~isempty(inputfile_eff)
        % extract an 'appropriate' suffix
        suffix = inputfile_eff(strfind(lower(inputfile_eff),'expeq')+length('expeq'):end);
    else
        suffix = '';
    end
end
if ~isempty(fname_out)
    fname_out_copied = cell(size(fname_out));
    for tel=1:length(fname_out)
        % copy output files with suffix
        if isempty(strfind(fname_out{tel},'o.chease'))
            fname_out_copied{tel} = [fname_out{tel},suffix];
            if ~strcmp(fname_out_copied{tel},fname_out{tel})
                copyfile(fname_out{tel},fname_out_copied{tel},'f');
                if nverbose >= 3, disp([fname_out{tel},' -> ',fname_out_copied{tel}]);end
            end
        else
            fname_out_copied{tel} = fname_out{tel};
            % o.chease* contain the suffix already from the start
        end
    end
    fname_out = fname_out_copied; % transpose for nicer visualization
else
    disp('length(fname_out)=0?');
    disp('fname_out = ');disp(fname_out);
    disp('type ''dbcont'' to continue, but you may ask O. Sauter to check what happened');
    keyboard
end

globalsvalues = extractdatachease(ofname_out,[ofname_out '.cols']);
