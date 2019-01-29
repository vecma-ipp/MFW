function [plotdatafile_H] = plotdatafile(fname_plotdatafile,option)
%
% [plotdatafile_H] = plotdatafile(fname_plotdatafile)
%
% reads a file with first line being: % col1_label col2_label etc, then data in column
% Creates GUI to be able to plot y versus x with x and y being either columns with labels given in first commented line
%
% allows this script to be run as if it was a function, if fname_plotdatafile is defined:
%
% plotdatafile_H: contains labels and data in output so can be easily called in local environment to make new plots
%
% option: 1 (default): create GUI for plotting
%         0          : only extract plotdatafile_H from file (so can use the data in other functions)
%

if ~exist('option','var'); option=1; end

if exist('fname_plotdatafile','var') && ~isempty(fname_plotdatafile) && exist(fname_plotdatafile,'file')
  fnamefull=fname_plotdatafile;
else
  [fname,pathname] = uigetfile([{'*cols*;*COLS*'},{'*cols* or *COLS* files'}; ...
                   {'*'},{'All files'}],'Select a file');
  if isequal(fname,0) || isequal(pathname,0); return; end
  %  fname=input('filename to load (fname.dat) (can have first line as headers): ','s')
  fnamefull=fullfile(pathname, fname);
end
plotdatafile_H.fnamefull=fnamefull;

plotdatafile_H.pdata_in=load(fnamefull);

% read comment line
fid = fopen(fnamefull);
[aa]=textscan(fid,'%[^\n]',1); % reads first line (assume starts with comment char)
fclose(fid);
if aa{1}{1}(1)=='%'
  labels=textscan(aa{1}{1}(2:end),'%s');
  labels = labels{1};
else
  labels=num2cell((1:size(plotdatafile_H.pdata_in,2))');
end
plotdatafile_H.labels=labels;
plotdatafile_H.main=[];

if option==0;
  return;
end

figure
plotdatafile_H.main=gcf;
myhandles = guihandles(plotdatafile_H.main);
posmain=[40 200 400 500];
set(plotdatafile_H.main,'Position',posmain)
x1=20;
xlen=120;
y1=20;
ylen=posmain(4)-100;
texttit=['fname_plotdatafile=' fnamefull];
plotdatafile_H.title1=uicontrol(plotdatafile_H.main,'style','edit','string',['>>fname_plotdatafile=' fnamefull],'HorizontalAlignment','left', ...
          'Position',[2 y1+ylen+60 posmain(3)-4 15]);
plotdatafile_H.title2=uicontrol(plotdatafile_H.main,'style','edit','string','>>plotdatafile_H=plotdatafile(''fname_plotdatafile'');','HorizontalAlignment','left', ...
          'Position',[2 y1+ylen+40 posmain(3)-4 15]);
plotdatafile_H.listx=uicontrol(plotdatafile_H.main,'style','listbox','string',labels,'Position',[x1 y1 xlen ylen]);
plotdatafile_H.listx_lab=uicontrol(plotdatafile_H.main,'style','text','string','x-variable','Position',[x1+20 y1+ylen+10 80 20]);
x2=x1+xlen+10;
plotdatafile_H.listy=uicontrol(plotdatafile_H.main,'style','listbox','string',labels,'Position',[x2 20 xlen ylen]);
plotdatafile_H.listy_lab=uicontrol(plotdatafile_H.main,'style','text','string','y-variable','Position',[x2+20 y1+ylen+10 80 20]);

x3=x2+xlen+10;
y3=y1+0.9*ylen;
if ~isfield(plotdatafile_H,'figplot1D')
  plotdatafile_H.figplot1D=figure;
end
cb=['myhandles = guidata(gcbo);plotdatafile_H=myhandles.plotdatafile_H;' ...
    'ix=get(plotdatafile_H.listx,''value''); iy=get(plotdatafile_H.listy,''value''); ' ...
    'figure(plotdatafile_H.figplot1D);' ...
    'plot(plotdatafile_H.pdata_in(:,ix),plotdatafile_H.pdata_in(:,iy)); ' ...
    'xlabel(plotdatafile_H.labels{ix}); ylabel(plotdatafile_H.labels{iy});' ...
    'disp([char(10) ''ix='' num2str(ix) ''; iy='' num2str(iy) '';'']);' ...
    'disp([''plot(plotdatafile_H.pdata_in(:,ix),plotdatafile_H.pdata_in(:,iy),''''-'''');'']);' ...
    'disp([''xlabel(plotdatafile_H.labels{ix}); ylabel(plotdatafile_H.labels{iy});'']);'];
plotdatafile_H.plot1D=uicontrol(plotdatafile_H.main,'style','pushbutton','string','plot(x,y)', ...
              'Position',[x3 y3 100 20],'callback',cb);

cbhold='ison=get(plotdatafile_H.fighold,''value''); figure(plotdatafile_H.figplot1D);if ison==0; hold off; set(plotdatafile_H.fighold,''string'',''hold off''); else; hold on; set(plotdatafile_H.fighold,''string'',''hold on''); end';

plotdatafile_H.fignumbtext=uicontrol(plotdatafile_H.main,'style','text','string','plot fig #:','Position',[x3 y3-50 75 20]);
plotdatafile_H.fignumbedit=uicontrol(plotdatafile_H.main,'style','edit','string',num2str(plotdatafile_H.figplot1D), ...
              'Position',[x3+80 y3-50 35 20],'callback',['myhandles = guidata(gcbo);plotdatafile_H=myhandles.plotdatafile_H;nfig=get(plotdatafile_H.fignumbedit,''string'');plotdatafile_H.figplot1D=str2num(nfig);myhandles.plotdatafile_H=plotdatafile_H;guidata(plotdatafile_H.main,myhandles);' cbhold]);

plotdatafile_H.fighold=uicontrol(plotdatafile_H.main,'style','toggle','string','hold off', ...
              'Position',[x3 y3-80 75 20],'callback',cbhold);

myhandles.plotdatafile_H=plotdatafile_H;
guidata(plotdatafile_H.main,myhandles);
