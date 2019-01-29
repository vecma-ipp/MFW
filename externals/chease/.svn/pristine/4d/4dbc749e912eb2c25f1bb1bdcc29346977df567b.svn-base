function [Hocols,chease_output_struct]=plot_ocols(varargin)
%
% Function to plot ochease.cols file obtained from unix script: o.chease_to_cols o.chease_output o.cols_file
%
% [Hocols,chease_output_struct] = plot_ocols
% or: 
% [Hocols,chease_output_struct] = plot_ocols('/path/to/my/o.cols_file');
% 
% NOTE: The first argument has to be named "Hocols", otherwise the callback do not work
%

% rewritten from plotdatafile to use read_ocols.m
% F.Felici CRPP May 2009
%

if nargout < 1
  error('must be called with at least first output argument named Hocols : Hocols = plot_ocols;');
end

if nargin == 0
  chease_output_struct = read_ocols; % call without inputs, user will be prompted
elseif nargin==1
  chease_output_struct = read_ocols(varargin{1}); % call with specified file
end

data = chease_output_struct.data;
labels=chease_output_struct.labels;
% $$$ fields = fieldnames(chease_output_struct);
% $$$ % extract data and fields from structure
% $$$ for tel=1:length(fields)
% $$$   data(:,tel) = getfield(chease_output_struct,fields{tel},'data');
% $$$   labels{tel} = getfield(chease_output_struct,fields{tel},'label');
% $$$ end

figure; Hocols.main=gcf;
% create UI list
set(Hocols.main,'Position',[40 250 400 400]);
x1=20; xlen=120; y1=20; ylen=300;
Hocols.listx=uicontrol(Hocols.main,'style','listbox','string',labels,'Position',[x1 y1 xlen ylen]);
Hocols.listx_lab=uicontrol(Hocols.main,'style','text','string','x-variable','Position',[x1+20 y1+ylen+10 80 20]);
x2=x1+xlen+10;
Hocols.listy=uicontrol(Hocols.main,'style','listbox','string',labels,'Position',[x2 20 xlen ylen]);
Hocols.listy_lab=uicontrol(Hocols.main,'style','text','string','y-variable','Position',[x2+20 y1+ylen+10 80 20]);

x3=x2+xlen+10; y3=y1+0.9*ylen;

Hocols.data = data; 
Hocols.labels = labels;

Hocols.figplot1D=11;
Hocols.plotcb=['if ~exist(''Hocols''); disp(''1st argument should be called Hocols''); return; end;', ...
               'ix=get(Hocols.listx,''value''); iy=get(Hocols.listy,''value'');',...
               'figure(Hocols.figplot1D); plot(Hocols.data(:,ix),Hocols.data(:,iy));',...
               'xlabel(Hocols.labels{ix}); ylabel(Hocols.labels{iy});', ...
              'disp([''plot(chease_output_struct.data(:,'' num2str(ix) ''),chease_output_struct.data(:,'' num2str(iy) '')) ; ylabel(chease_output_struct.labels{'' num2str(iy) ''}) ''])'];

Hocols.plot1D=uicontrol(Hocols.main,'style','pushbutton','string','plot(x,y)', ...
                                      'Position',[x3 y3 100 20],'callback',Hocols.plotcb);
