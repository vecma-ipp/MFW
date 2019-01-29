function [fname_out,globalsvalues] = run_chease_expeq(namelistfile,expeqfile,varargin)
%function fname_out = run_chease_expeq(namelistfile,expeqfile{,exptnzfile,suffix,nverbose})
% namelistfile: full path to CHEASE namelist
% expeqfile   : full path to EXPEQ file
% exptnzfile  : (optional) full path to EXPTNZ file
% suffix      : (optional) suffix to written files
% nverbose    : (optional) verbosity level
%
% fname_out   : cell containing full path of output files
% globalsvalues: structure containing main global results extracted from output: q0, qedge, q95, betaN, betap, li, kappa, delta, etc
%
% This function is now calling run_chease with correct syntax to avoid maintaining multiple 'run_chease' files.

cocos_in = 2;
chease_exe = '';

switch nargin,
    case 5,
        args = [{namelistfile,expeqfile,cocos_in},varargin(1:2),{chease_exe},varargin(3)];
        % We have to make chease_exe a cell to avoid concatenation with nearby elements
    case {3,4}
        args = [{namelistfile,expeqfile,cocos_in},varargin(1:nargin-2)];
    case 2
        args = {namelistfile,expeqfile};
    otherwise
        error('nargin = %d not supported in %s',nargin,mfilename);
end

if isnumeric(namelistfile) || isempty(namelistfile),
    args{1} = 1; % Override setting for expeq file input
end

[fname_out,globalsvalues] = run_chease(args{:});
