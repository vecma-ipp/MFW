function EXPTNZdata=plot_exptnz(fname,plotOptions)
%
% function EXPTNZdata=plot_exptnz(fname/EXPTNZdata,{plotOptions})
%
% Script to plot from EXPTNZ file. Uses read_exptnz.m
%
% Can be run without input arguments, in that case if 
% If fname variable exists in workspace it is used directly.
% Otherwise the user is prompted for a file.
% 
% Optionally, plotOptions variable in workspace can be used 
%  to assign matlab plot options.
%
% Example:
% plot_exptnz('EXPTNZ_A','b'); hold on; plot_exptnz('EXPTNZ_B','r')
%
% see also read_exptnz and write_exptnz

% F.Felici CRPP - Feb 2010

if nargin == 0  || isempty(fname); 
  EXPTNZdata = read_exptnz;
  fnamefull = EXPTNZdata.fnamefull;
  text_title=['Plot of ', fnamefull];
end

if nargin >= 1; 
  if ~isempty(fname) && isstruct(fname)
    % EXPTNZ structure is given
    EXPTNZdata = fname;
    text_title='Plot of input structure';
  else
    % exptnz filename is known, read exptnz
    if ~isempty(fname)
      fnamefull = fname;
      EXPTNZdata = read_exptnz(fnamefull);
      text_title=['Plot of ', fnamefull];
    end
  end
end

if nargin<2 || isempty(plotOptions)
  plotOptions='b';
  figure; % to avoid plotting on unwanted previous figure
end

if nargin >=3; error('wrong number of inputs'); end

%%%%%%%%%%%%%%%%%%%%%%%%
% plot
%%%%%%%%%%%%%%%%%%%%%%%%

set(gcf,'name',[num2str(gcf) ' ; ' text_title ' dens'])

subplot(222);
plotos(EXPTNZdata.rhopsi,EXPTNZdata.ne,'-',0,0,plotOptions);
hold on
if isfield(EXPTNZdata,'ni')
  plotos(EXPTNZdata.rhopsi,EXPTNZdata.ni,'--',0,0,plotOptions);
  legend('ne','ni')
else
  
end
xlabel('\rho_{\psi}');
ylabel('[m^{-3}]');

subplot(221);
plot(EXPTNZdata.rhopsi,EXPTNZdata.te,plotOptions);
xlabel('\rho_{\psi}'); ylabel('T_e [eV]');grid on; hold on;

subplot(223);
plot(EXPTNZdata.rhopsi,EXPTNZdata.ti,plotOptions);
xlabel('\rho_{\psi}'); ylabel('T_i [eV]'); grid on; hold on;

subplot(224);
plot(EXPTNZdata.rhopsi,EXPTNZdata.zeff,plotOptions);
xlabel('\rho_{\psi}'); ylabel('Z_{eff}');grid on; hold on;
set(gca,'ylim',[min(EXPTNZdata.zeff)-1,max(EXPTNZdata.zeff)+1]);
