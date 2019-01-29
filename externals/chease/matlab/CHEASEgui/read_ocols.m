function chease_output_struct = read_ocols(varargin)
%
% function chease_output_struct = read_ocols(varargin)
% function chease_output_struct = read_ocols(file)
%
% reads chease_output_struct.cols file generated from CHEASE output file by unix script:
%    chease_output_struct.chease_to_cols chease_output_struct.chease_output chease_output_struct.cols_file
%
% to plot, use plot_ocols.m

% F.Felici CRPP May 2009
% Adapted from plotdatafile.m by O.Sauter

Swarningstate=warning('query');
warning off

if nargin == 0
  [fname,fpath]=uigetfile('*.cols','chease_output_struct.cols file');
  file = fullfile(fpath,fname);
elseif nargin==1
  file = varargin{1};
  if ~exist(file); error([file,' does not exist']); end
  [fpath,fname] = fileparts(file); % get fname and fpath
end

data = [];
labels=[];
chease_output_struct.data=data;
chease_output_struct.labels=labels;

try
  data=load(file);
catch
  error('error loading file, are you sure you selected the .cols file?');
end
% read comment line to get labels
fid = fopen(file);
[aa]=textscan(fid,'%[^\n]',1); % reads first line (assume starts with comment char)
fclose(fid);
if aa{1}{1}(1)=='%'
  labels=textscan(aa{1}{1}(2:end),'%s');
  labels = labels{1};
else
  error('first line does not start with comment character %');
end
if isempty(labels); error('error reading labels from .cols file'); end

chease_output_struct=struct;
for tel=1:length(labels);
  varvalue = data(:,tel);
  fieldname = labels{tel};
  if ~isempty(strfind(fieldname,'(CSM)'))
    mesh = ('CSM');
    fieldname = strrep(fieldname,'(CSM)','');
  elseif ~isempty(strfind(fieldname,'(CS)'))
    mesh = ('CS');
    fieldname = strrep(fieldname,'(CS)','');
  else
    mesh = '';
  end
  fieldname = lower(fieldname);
  fieldname = strrep(fieldname,'-','_');
  fieldname = strrep(fieldname,'**','pow');
  fieldname = strrep(fieldname,'*','x');
  fieldname = strrep(fieldname,'//','_par');
  fieldname = strrep(fieldname,'/','_over_');
  fieldname = strrep(fieldname,'|','_');
  fieldname = strrep(fieldname,'(','_');
  fieldname = strrep(fieldname,')','_');
  fieldname = strrep(fieldname,'<','av_');
  fieldname = strrep(fieldname,'>','_av');
  fieldname = strrep(fieldname,'[','_');
  fieldname = strrep(fieldname,']','_');
  fieldname = strrep(fieldname,'=','_eq_');
  fieldname = strrep(fieldname,'...','_');
  fieldname = strrep(fieldname,'.','_dot_');
  fieldname = strrep(fieldname,',','_');
  fieldname = strrep(fieldname,'''','_');
  fieldname = strrep(fieldname,':','_');
  fieldname = strrep(fieldname,'^','pow');
  fieldname = strrep(fieldname,'&','_');
  fieldname = strrep(fieldname,'___','_');
  fieldname = strrep(fieldname,'__','_');
  fieldname = regexprep(fieldname,'^1\+','X1p');
  fieldname = regexprep(fieldname,'^1','X1');
  while strcmp(fieldname(end),'_'); fieldname=fieldname(1:end-1); end;
  newfield =struct('data',varvalue,'mesh',mesh,'label',labels{tel});
  chease_output_struct=setfield(chease_output_struct,fieldname,newfield);
end

fields = fieldnames(chease_output_struct);
% extract data and fields from structure
for tel=1:length(fields)
  data(:,tel) = getfield(chease_output_struct,fields{tel},'data');
  labels{tel} = getfield(chease_output_struct,fields{tel},'label');
end
chease_output_struct.data=data;
chease_output_struct.labels=labels;

warning(Swarningstate)
