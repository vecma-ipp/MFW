function [customizelist_H] = customize_options(customizelist_in,option);
%
% [customizelist_H] = customize_options(customizelist_in,option)
%
% customizelist_in: if filename, assume a 3 columns string data with one header line
%         1st line  : "% keywords(no space) default_values labels(can have space)
%         next lines: keyword data label
%
% customizelist_in: if structure
%         3 fields expected: .keywords{:}, .default_values{:}, .labels{:} (cell char array)
%
% Creates GUI to be able to change the values
%
% customizelist_H: contains labels and data in output so can be easily called in local environment to make new plots
%                  is changed in "workspace", "ok/continue" closes the window
%
% if called from within a GUI, add the folowing lines:
%               >> [customizelist_H] = customize_options(customizelist_in);
%               >> waitfor(customizelist_H.handles.ok);
%               >> customizelist_H=evalin('base','customizelist_H'); % if needed
%
% option: 1 (default): creates GUI
%         0          : only extract customizelist_H from file (so can use the data in other functions)
%

if ~exist('option'); option=1; end

fnamefull=[];
if exist('customizelist_in') && ~isempty(customizelist_in) && ischar(customizelist_in) && exist(customizelist_in,'file')
  fnamefull=customizelist_in;
elseif exist('customizelist_in') && ~isempty(customizelist_in) && isstruct(customizelist_in)
  fnamefull=customizelist_in;
else
  [fname,pathname] = uigetfile([{'*custom*;*CUSTOM*'},{'*custom* or *CUSTOM* files'}; ...
                   ;{'*'},{'All files'}],'Select a file');
  if isequal(fname,0) || isequal(pathname,0); return; end
  %  fname=input('filename to load (fname.dat) (can have first line as headers): ','s')
  fnamefull=fullfile(pathname, fname);
end
customizelist_H.fnamefull=fnamefull;

% Note: treat inputs as strings. In the code usage the person should know if str2num is required
% if input value is a text with space, put it in between quotes as for the labels
if isstruct(fnamefull)
  customizelist_H.keywords = fnamefull.keywords;
  customizelist_H.default_values = fnamefull.default_values;
  customizelist_H.labels = fnamefull.labels;
else
  [kwds_list,def_vals_list,labels_list]=textread(fnamefull,'%s%q%q\n','headerlines',1);
  customizelist_H.keywords = kwds_list;
  customizelist_H.default_values = def_vals_list;
  customizelist_H.labels = labels_list;
end
customizelist_H.values = customizelist_H.default_values;
for i=1:length(customizelist_H.keywords)
  eval(['customizelist_H.i' customizelist_H.keywords{i} ' = ' num2str(i) ';'])
end

customizelist_H.main=[];

if option==0;
  return;
end

nboxtot = length(customizelist_H.keywords);

figure
customizelist_H.main=gcf;
myhandles = guihandles(customizelist_H.main);
posmain=[400 500 700 500];
if nboxtot>8; posmain=[400 900 700 900]; end
set(customizelist_H.main,'Position',posmain)

zdx=[0.02 0.45 0.02 0.5];
zdy0=0.1;
zdy_width=20/posmain(4);
zdy=(1-2*zdy0-nboxtot*zdy_width)/(nboxtot-1+1);
yeff=(1.-zdy0-zdy_width)*posmain(4);

for i=1:nboxtot;
  labeltext=customizelist_H.labels{i};
  aatext=[zdx(1)*posmain(3) yeff zdx(2)*posmain(3) zdy_width*posmain(4)];
  customizelist_H.handles.labtext(i) = uicontrol(customizelist_H.main,'style','text','position',aatext, ...
	  'string',labeltext,'fontUnits','normalized');
  aatext=[aatext(1)+aatext(3)+zdx(3)*posmain(3) aatext(2) zdx(4)*posmain(3) aatext(4)];
  cb=['myhandles = guidata(gcbo);customizelist_H=myhandles.customizelist_H;' ...
      'newvalstr=get(customizelist_H.handles.value(' num2str(i) '),''string'');' ...
      'customizelist_H.values(' num2str(i) ') = newvalstr;' ...
      'myhandles.customizelist_H=customizelist_H; guidata(customizelist_H.main,myhandles);'   ];
  customizelist_H.handles.value(i) = uicontrol(customizelist_H.main,'style','edit','position',aatext ...
	  ,'fontSize',[10],'callback',cb);
  set(customizelist_H.handles.value(i),'string',customizelist_H.values(i));
  yeff=yeff - (zdy_width+zdy)*posmain(4);
end

% OK button at bottom right
cb=['myhandles = guidata(gcbo);customizelist_H=myhandles.customizelist_H;' ...
    'close(customizelist_H.main);'   ];
aatext=[zdx(1)*posmain(3) yeff zdx(2)*posmain(3) zdy_width*posmain(4)];
aatext=[aatext(1)+aatext(3)+zdx(3)*posmain(3) aatext(2) zdx(4)*posmain(3) aatext(4)];
customizelist_H.handles.ok = uicontrol('style','pushbutton','string','Ok/Continue','position', ...
          [0.6*posmain(3) 0.02*posmain(4) 200 15],'callback',cb);

myhandles.customizelist_H=customizelist_H;
guidata(customizelist_H.main,myhandles);

