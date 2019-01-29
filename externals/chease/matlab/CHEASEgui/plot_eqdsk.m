function [eqdskout,varargout]=plot_eqdsk(varargin)
%
% [eqdskout,eqdsk257]=plot_eqdsk(varargin)
%
% Script to plot an eqdsk.
%
%  By defaults, plots the profiles (p, pprime, T, TTprime and q) and the psi(R,Z) contours
%
%  varargin{1}: eqdskin
%               if it is an eqdsk structure, plots the various values
%               if it is a name, assumes it is an eqdsk filename and executes first:
%                   eqdskout=read_eqdsk(eqdskin);
%
%  varargin{2}: ifig: Starting number for figures (default: -30))
%               if ifig<0, starts from abs(ifig) and overlays plots (hold on before plotting)
%               ifig=0: open new figures
%
%  varargin{3}: 0 (default): do not check Grad-Shfranov
%               1 : Checks Grad-Shafranov and returns interpolated eqdsk on 257x257 mesh (=eqdsk257)
%
%  varargin{4}: 0 (default) plot all figures
%               1 plot only contours of left- and right-hand side of Grad-Shafranov equation
%
% Examples:
%
% eq1=plot_eqdsk('EQDSK_A'); eq2=plot_eqdsk('EQDSK_B'); % figures 30 to 34 will have both eqdsks
%
% eq=plot_eqdsk; % will open window to choose an eqdsk file
%
% [eq1,eq257]=plot_eqdsk('EQDSKfilename',[],1);
%
% see also read_eqdsk, write_eqdsk, eqdsk_transform
%

set_defaults_matlab

if nargin == 0 || isempty(varargin{1}); 
  eqdskin = read_eqdsk;
  fnamefull = eqdskin.fnamefull;
  if ~exist(fnamefull); return; end
  eqdskin.extralines{end+1}=['From file: ' fnamefull];
elseif ~isstruct(varargin{1})
  % eqdsk filename is known, read eqdsk
  fnamefull = varargin{1};
  eqdskin = read_eqdsk(fnamefull);
  if ~exist(fnamefull); return; end
  eqdskin.extralines{end+1}=['From file: ' fnamefull];
else
  % eqdsk structure is given
  eqdskin = varargin{1};
end

eqdskout=eqdskin;

ifiggiven=0;
if nargin >= 2 && ~isempty(varargin{2}); ifiggiven = varargin{2};  end

icall_eqtransform=0;
if nargin >= 3 && ~isempty(varargin{3}); icall_eqtransform = varargin{3};  end

iplot_only_gs = 0;
if nargin >= 4 && ~isempty(varargin{4}); iplot_only_gs = varargin{4};  end

if nargin >= 5; error('wrong number of inputs'); return; end

%%%%%%%%%%%%%%%%%%%%%%%%
% plot pressure and pprime
%%%%%%%%%%%%%%%%%%%%%%%%
if ~iplot_only_gs
  if ifiggiven==0
    ifiggiven=figure;
  else
    figure(abs(ifiggiven));
  end
  if ifiggiven>0; clf; end

  icol=1;
  subplot(2,1,1)
  if ifiggiven<0
    hold on
    icol=length(get(gca,'child'))+1;
  end
  plotos(eqdskin.psimesh,eqdskin.p,'-',[],[],colos(icol,:));
  ylabel('p')
  xlabel('psi')

  subplot(2,1,2)
  plotos(eqdskin.psimesh,eqdskin.pprime,'-',[],[],colos(icol,:));
  xlabel('psi')
  ylabel('pprime')
  
  %%%%%%%%%%%%%%%%%%%%%%%%
  % plot F and FFprime
  %%%%%%%%%%%%%%%%%%%%%%%%
  ifig=abs(ifiggiven)+1;
  figure(ifig)
  if ifiggiven>0; clf; end
  subplot(2,1,1)
  if ifiggiven<0
    hold on
  end
  plotos(eqdskin.psimesh,eqdskin.F,'-',[],[],colos(icol,:));
  ylabel('F')
  xlabel('psi')
  
  subplot(2,1,2)
  plotos(eqdskin.psimesh,eqdskin.FFprime,'-',[],[],colos(icol,:));
  xlabel('psi')
  ylabel('FFprime')
  
  %%%%%%%%%%%%%%%%%%%%%%%%
  % plot q
  %%%%%%%%%%%%%%%%%%%%%%%%
  ifig=ifig+1;
  figure(ifig)
  if ifiggiven>0; clf; end
  subplot(2,1,1)
  if ifiggiven<0
    hold on
  end
  plotos(eqdskin.psimesh,eqdskin.q,'-',[],[],colos(icol,:));
  ylabel('q')
  xlabel('psi')
  
  subplot(2,1,2)
  plotos(eqdskin.rhopsi,eqdskin.q,'-',[],[],colos(icol,:));
  xlabel('rho')
  ylabel('q')
  
  %%%%%%%%%%%%%%%%%%%%%%%%
  % plot just plasma boundary
  %%%%%%%%%%%%%%%%%%%%%%%%
  ifig=ifig+1;
  figure(ifig)
  if ifiggiven>0; clf; end
  if ifiggiven<0
    hold on
  end
  plotos(eqdskin.rplas,eqdskin.zplas,'-',[],[],colos(icol,:));
  ylabel('Z')
  xlabel('R')
  title('plasma boundary')
  axis equal
  
  %%%%%%%%%%%%%%%%%%%%%%%%
  % plot psi(R,Z)
  %%%%%%%%%%%%%%%%%%%%%%%%
  ifig=ifig+1;
  figure(ifig)
  if ifiggiven>0; clf; end
  if ifiggiven<0
    hold on
  end
  lnstyle=[{'-'} {'--'} {':'} {'-.'}];
  contour(eqdskin.rmesh,eqdskin.zmesh,eqdskin.psi',100,lnstyle{mod(icol-1,4)+1});
  hold on
  plotos(eqdskin.rplas,eqdskin.zplas,lnstyle{mod(icol-1,4)+1},[2],[],colos(icol,:));
  if eqdskin.nblim>=5; plotos(eqdskin.rlim,eqdskin.zlim,lnstyle{mod(icol-1,4)+1},[],[],colos(icol,:)); end
  
  ylabel('Z')
  xlabel('R')
  title('psi(R,Z)')
  axis equal
  
  if ~isfield(eqdskin,'rjphi') || isempty(eqdskin.rjphi)
    % read_eqdsk was called without calculating Grad-Shafranov related fields
    return
  end

end

%%%%%%%%%%%%%%%%%%%%%%%%
% Plot rjphi and Gradshafranov from read_eqdsk (new)
%%%%%%%%%%%%%%%%%%%%%%%%

[zzz iraxis]=min(abs(eqdskin.rmesh-eqdskin.raxis));
[zzz izaxis]=min(abs(eqdskin.zmesh-eqdskin.zaxis));
rjphi_axis=eqdskin.rjphi(iraxis,izaxis);
zzzmax=sign(rjphi_axis)*max([abs(eqdskin.rjphi(iraxis,izaxis)), ...
                    abs(eqdskin.gradshaf(iraxis,izaxis)),abs(-eqdskin.FFprime(1)-4e-7*pi*eqdskin.pprime(1)*eqdskin.raxis.^2)]);
zzzmin=zzzmax;
if zzzmax>0
  zzzmin=0.1*zzzmax;
else
  zzzmax=0.1*zzzmin;
end
% remove values outside plasma since no meaning and slows down contour plot
ijk=find(eqdskin.rjphi==0);
eqdskin.gradshaf(ijk)=-0.1*rjphi_axis;

if ~iplot_only_gs

  figure
  contour(eqdskin.rmesh,eqdskin.zmesh,eqdskin.rjphi',linspace(zzzmin,zzzmax,100));
  colorbar
  hold on
  plotos(eqdskin.rplas,eqdskin.zplas,'k-');
  axis equal
  title(['eqdsk.rjphi on linspace(' num2str(zzzmin) ',' num2str(zzzmax) ',' num2str(100) ')'])
 
end

figure
contour(eqdskin.rmesh,eqdskin.zmesh,eqdskin.rjphi',linspace(zzzmin,zzzmax,60),'-');
hold on
contour(eqdskin.rmesh,eqdskin.zmesh,eqdskin.gradshaf',linspace(zzzmin,zzzmax,60),'--');
plotos(eqdskin.rplas,eqdskin.zplas,'k-');
axis equal
title(['eqdsk.rjphi (solid) ; eqdsk.gradshaf(dashed) on linspace(' num2str(zzzmin) ',' num2str(zzzmax) ',' num2str(100) ')'])

if ~iplot_only_gs
  
  %%%%%%%%%%%%%%%%%%%%%%%%
  % Plot B components
  %%%%%%%%%%%%%%%%%%%%%%%%

  figure
  contour(eqdskin.rmesh,eqdskin.zmesh,eqdskin.BR',100);
  hold on
  plotos(eqdskin.rplas,eqdskin.zplas,'k-');
  axis equal
  title(['eqdsk.BR'])
  
  figure
  contour(eqdskin.rmesh,eqdskin.zmesh,eqdskin.BZ',100);
  axis equal
  hold on
  plotos(eqdskin.rplas,eqdskin.zplas,'k-');
  title(['eqdsk.BZ'])
  
  figure
  contour(eqdskin.rmesh,eqdskin.zmesh,eqdskin.Bphi',100);
  axis equal
  hold on
  plotos(eqdskin.rplas,eqdskin.zplas,'k-');
  title(['eqdsk.Bphi'])
  
  figure
  contour(eqdskin.rmesh,eqdskin.zmesh,eqdskin.B',100);
  axis equal
  hold on
  BX2=82.7e9*2*pi./1.602e-19.*0.911e-30/2;
  [hh1 hh2]=contour(eqdskin.rmesh,eqdskin.zmesh,eqdskin.B',[BX2 BX2],'k--','linewidth',2);
  xxx=get(get(hh2,'children'),'xdata');
  yyy=get(get(hh2,'children'),'ydata');
  if ~iscell(xxx)
    if ~isempty(xxx) && ~isempty(yyy)
      text(xxx(1)-0.01,yyy(1),'X2')
    end
  end
  BX3=118e9*2*pi./1.602e-19.*0.911e-30/3;
  [hh1 hh2]=contour(eqdskin.rmesh,eqdskin.zmesh,eqdskin.B',[BX3 BX3],'k--','linewidth',2);
  xxx=get(get(hh2,'children'),'xdata');
  yyy=get(get(hh2,'children'),'ydata');
  if ~iscell(xxx)
    if ~isempty(xxx) && ~isempty(yyy)
      text(xxx(1)-0.01,yyy(1),'X3')
    end
  end

  plotos(eqdskin.rplas,eqdskin.zplas,'k-');
  title(['eqdsk.B'])
  
  figure
  contour(eqdskin.rmesh,eqdskin.zmesh,eqdskin.Bpol',100);
  hold on
  plotos(eqdskin.rplas,eqdskin.zplas,'k-');
  axis equal
  title(['eqdsk.Bpol'])

end

%%%%%%%%%%%%%%%%%%%%%%%%
% check Grad-Shafranov and interpolate to 257x257
%%%%%%%%%%%%%%%%%%%%%%%%

if icall_eqtransform
  varargout{1}=eqdsk_transform(eqdskin,257,257,[],1) % makes plots including Grad-shfranov test
end
