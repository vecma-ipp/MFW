function nl = read_namelist_chease(varargin)
%
% now can use read_namelist(file,'EQDATA') directly, no need for nl2matlab anymore
%
% Just keep options for default expeq and eqdsk namelists
%
% nl = read_namelist_chease
% nl = read_namelist_chease(filename)
% nl = read_namelist_chease([]);  Reads the default namelist (ready for using an EXPEQ-type input file)
% nl = read_namelist_chease(1); Use the the first default namelist file (identical to default for EXPEQ file)
% nl = read_namelist_chease(2); Use default namelist ready for using an EQDSK-type input file
% 
% uses read_namelist.m to read fortran namelist

% F. Felici CRPP2009
% set up tmpdir, create if necessary
uname = getenv('USER'); tmpdir = ['/tmp/',deblank(uname),'/'];
if ~exist(tmpdir,'dir'), mkdir(tmpdir); end
if nargin == 0
  [fname,fpath] = uigetfile('*.*','Select namelist file');
  if isequal(fname,0) && isequal(fpath,0)
    error('no file selected, give empty filename to use the default namelist file');
  end
elseif nargin == 1;
  if ~isnumeric(varargin{1}) && ~isempty(varargin{1}) && exist(varargin{1},'file'); 
    [fpath,fn,fext] = fileparts(varargin{1}); fname = [fn,fext];
    if isempty(fpath); fpath=pwd; end;
  else
    if isempty(varargin{1}) || varargin{1}==1
      def_nam = 'default_namelist_chease_expeq';
    elseif varargin{1}==2
      def_nam = 'default_namelist_chease_eqdsk';
    elseif varargin{1}==0
      def_nam = 'default_namelist_chease_nofiles';
    else
      error(['varargin{1} = ' varargin{1} ' in read_namelist_chease not yet an option, ask O. Sauter if needed']);
    end
    [a]=which('read_namelist_chease');
    [b,c]=fileparts(a);
    fpath = tmpdir;
    fname = def_nam;
    if exist(fullfile(tmpdir,fname),'file');unix(['rm ' tmpdir fname]); end
    unix(['echo ''*'' > ' tmpdir fname]);
    unix(['echo ''*'' >> ' tmpdir fname]);
    unix(['echo ''*'' >> ' tmpdir fname]);
    unix(['echo ''*'' >> ' tmpdir fname]);
    unix(['echo ''&EQDATA'' >> ' tmpdir fname]);
    unix(['cat ' fullfile(b,fname) ' >> ' tmpdir fname]);
    unix(['echo ''/'' >> ' fpath fname]);
    def_namelist = fullfile(tmpdir,fname);
    disp(['use default namelist: ' def_namelist]);
  end
end

file = fullfile(fpath,fname);

namelist_name = 'EQDATA';

nl = read_namelist(file,namelist_name);

