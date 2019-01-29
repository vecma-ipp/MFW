function [object] = plotos(x,y, mark, size,iopt, color);
%
% function [object] = plotos(x,y, mark, size,iopt, color);
%
% to plot(x,y) with type of line and marker given by mark
%
% object returns handle to the plot
%
% size,iopt, color are optional:
%
% size <= 0: use default size
% size > 0 : set size of marker
% if size of length 2, then:
%     size_line = size(1)  (use 0 to keep default value)
%     size_marker = size(2)  (use 0 to keep default value)
%
% iopt = 0: open symbol
% iopt = 1: solid symbol
%
% color can be introduced in mark or not, it will superseed the color set in
% variable mark
%
% Typical calls are:
%
% plotos(x,y);
% plotos(x,y,'ro-',8,1);                       with filled marker of size 8
% plotos(x,y,'r-',[2 0]);                      To plot with line size=2
% hh=plotos(x,y,'ro',8,1,'[0.2 0.5 0.8]);    with specific color
%

if nargin == 1;
  object=plot(x);
elseif nargin == 2;
  object=plot(x,y);
else  
  object=plot(x,y,mark);
end

% set sizes

if nargin <= 3 ; 
  return
elseif nargin >= 4;
  size_line = 0;
  size_mark = 0;
  if length(size) == 2;
    size_line = size(1);
    size_mark = size(2);
  else
    if size > 0;
      size_mark = size;
    end
  end
  if size_line > 0;
    set(object,'LineWidth',size_line);
  end
  if size_mark > 0;
    set(object,'MarkerSize',size_mark);
  end
end

% set open/solid and colors

if nargin == 5;
  if iopt == 1;
    obj_col = get(object,'Color');
    set(object,'MarkerFaceColor',obj_col);
  end;
elseif nargin == 6;
  set(object,'Color',color);
  if iopt == 1;
    set(object,'MarkerFaceColor',color);
  end
end
