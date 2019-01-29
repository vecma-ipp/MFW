function [EXPEQdata,ph] = plot_expeq(varargin)
% PLOT_EXPEQ Function to plot data from an EXPEQ file.
%  [EXPEQdata plothandle]= PLOT_EXPEQ({fighandle},{fname/EXPEQdata},{plotOptions})
% 
%  Uses read_expeq.m
%
% Can be run without input arguments, in that case the user is prompted for
% a file.
% 
% The first argument can optionally be a figure handle, to plot on. In that
% case, the plot color is automatically changed using the hold('all')
% feature.
%
% The user provides the EXPEQ data via a valid filename or an existing
% structure containing the data.
% 
% Optionally, plotOptions can be used to assign matlab plot options. It is
% an argument of variable size (similar to varargin).
%
% NB: - In this new version, it is mandatory to specify the figure handle 
%     in order to plot another equilibrium on the same figure.
%     - If you want to use the plotOptions feature, you have to specify the
%     a valid EXPEQ (either a file or a structure)
%
% Example 1: Plotting a single equilibrium (file chosen in prompt window)
% plot_expeq;
%
% Example 2: Plotting 2 equilibriums on the same figure
% [~,ph] = plot_expeq('EXPEQ_A');
% [~,ph] = plot_expeq(ph.fig,'EXPEQ_B');
%
% Example 1: Specifying custom plot options
% plot_expeq('EXPEQ_A','--k','Linewidth',2,'DisplayName','Original Data');
% 
%
% See also READ_EXPEQ and WRITE_EXPEQ


% if first varargin is a handle then this is the figure. Shift all other
% varargins by one

iarg = 0; % varargin shift in case the first is a handle
if (nargin ~=0) && isnumeric(varargin{1})
    % Check that it is indeed a figure handle
    if (length(varargin{1}) == 1) && (findobj(varargin{1},'type','figure','-depth',0) == varargin{1});
        ph.fig = varargin{1};
        iarg = 1;
    else
        error('Invalid figure handle as 1st argument');
    end
end

if nargin == iarg  || isempty(varargin{1+iarg}); 
  EXPEQdata = read_expeq;
  fnamefull = EXPEQdata.fnamefull;
end

if nargin >= 1+iarg; 
  if ~isempty(varargin{1+iarg}) && isstruct(varargin{1+iarg})
    % EXPEQ structure is given
    EXPEQdata = varargin{1+iarg};
    if isfield(EXPEQdata,'fnamefull'),
        fnamefull = EXPEQdata.fnamefull;
    else
        % Gets structure name at the function call
        fnamefull = ['struct ',inputname(1+iarg)];
    end
  else
    % expeq filename is known, read expeq
    if ~isempty(varargin{1+iarg})
      fnamefull = varargin{1+iarg};
      EXPEQdata = read_expeq(fnamefull);
    end
  end
end

% All the other arguments are passed to plotOptions (can be empty)
plotOptions = varargin(2+iarg:end);


% Check extralines field
if ~isfield(EXPEQdata,'extralines') || isempty(EXPEQdata.extralines)
  EXPEQdata.extralines{1}=['From file: ' fnamefull];
else
  EXPEQdata.extralines{end+1}=['From file: ' fnamefull];
end

%%%%%%%%%%%%%%%%%%%%%%%%
% plot
%%%%%%%%%%%%%%%%%%%%%%%%
if exist('ph','var')
  figure(ph.fig);
  ax_xy = findobj(ph.fig(1),'Tag','ax_xy');
  ax_pp = findobj(ph.fig(1),'Tag','ax_pp');
  ax_cu = findobj(ph.fig(1),'Tag','ax_cu');
else
  % create new figure
  ph.fig = figure;


  switch EXPEQdata.nrhotype 
   case 0
    rho_label=('\rho_{\psi}');
   case 1
    rho_label=('\rho_{tor}');
  end
  
  % create axes for the boundary
  ax_xy=subplot(2,2,[1,3]);hold(ax_xy,'all');
  set(ax_xy,'OuterPosition',[0.0 0.0 0.5 1.0],'Tag','ax_xy');
  axis(ax_xy,'equal');
  title('Boundary ');
  xlabel('r/R_0');
  ylabel('z/R_0');
  
  % create axes for the pressure profile
  ax_pp=subplot(2,2,2);hold(ax_pp,'all');
  set(ax_pp,'OuterPosition',[0.5 0.5 0.5 0.5],'Tag','ax_pp');
  title('p'' chease');
  xlabel(rho_label);
  ylabel('chease units');
  
  % create axes for the current profile
  ax_cu=subplot(2,2,4);hold(ax_cu,'all');
  set(ax_cu,'OuterPosition',[0.5 0.0 0.5 0.5],'Tag','ax_cu');
  switch EXPEQdata.nsttp
   case 1
    name_cu = 'TTprime';
    title('TT'' chease');
   case 2
    name_cu = 'Istar';
    title('I^* chease');
   case 3
    name_cu = 'Jparallel';
    title('j_{||} tilde chease');
  end
  xlabel(rho_label);
  ylabel('chease units');
  
  loose_inset = get(0,'DefaultAxesLooseInset');
  % Manage insets to avoid crowded figure
  tight_inset = get(ax_xy,'TightInset');
  inset = loose_inset./[0.5 1 0.5 1];
  inset(3) = tight_inset(3)*2;
  set(ax_xy,'LooseInset',inset);
  tight_inset = get(ax_pp,'TightInset');
  inset = loose_inset./0.5;
  inset([1,2]) = tight_inset([1,2])./0.5;
  set(ax_pp,'LooseInset',inset);
  tight_inset = get(ax_cu,'TightInset');
  inset = loose_inset./0.5;
  inset([1,4]) = tight_inset([1,4])./0.5;
  set(ax_cu,'LooseInset',inset);
end

% Set Figure name
set(ph.fig,'name',EXPEQdata.extralines{end})

switch EXPEQdata.nsttp
 case 1
  name_cu = 'TTprime';
 case 2
  name_cu = 'Istar';
 case 3
  name_cu = 'Jparallel';
end

% Plot boundary and profiles
ph.handle(1,1) = plot(ax_xy,EXPEQdata.RZ_psi(:,1),EXPEQdata.RZ_psi(:,2),plotOptions{:});

ph.handle(2,1) = plot(ax_pp,EXPEQdata.rho,EXPEQdata.Pprime,plotOptions{:});

ph.handle(3,1) = plot(ax_cu,EXPEQdata.rho,EXPEQdata.(name_cu),plotOptions{:});

end