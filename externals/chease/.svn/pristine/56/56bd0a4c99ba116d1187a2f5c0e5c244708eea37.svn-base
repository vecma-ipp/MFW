%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chease_Plot_2.m : Graphic output from CHEASE %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% EQUILIBRIUM 2
clf
figure(HandleList(4));
set(HandleList(4),...
    'Position',[150,50,500,500],...
    'PaperUnits','centimeters',...
    'Paperposition',[0. 0. 21. 29.7],...
    'PaperOrientation','portrait')

% PLOT FLUX SURFACES
subplot(2,2,1);
hold on
if jmax<maxsurf
  stepsurf = 1 ;
else
  stepsurf = (jmax-rem(jmax,maxsurf))/maxsurf ;
end
plot(R0EXP*CR(:,jmax:-stepsurf:max(1,jmax-stepsurf*maxsurf)), ...
     R0EXP*CZ(:,jmax:-stepsurf:max(1,jmax-stepsurf*maxsurf)),'b')
% PLOT CHI LINES
plot(R0EXP*CR(1:stepchi:min(imax,stepchi*maxchi+1),:)', ...
     R0EXP*CZ(1:stepchi:min(imax,stepchi*maxchi+1),:)','b')
axis('square');
axis('equal');
xlabel('R-Raxis');
ylabel('Z-Zaxis');
title('Magnetic Surfaces')

% PLOT PPRIME
subplot(2,2,2);
y1=min((B0EXP/mu0/R0EXP/R0EXP)*ZOPPR);
y2=max((B0EXP/mu0/R0EXP/R0EXP)*ZOPPR);
plot(R0EXP*ZABR,(B0EXP/mu0/R0EXP/R0EXP)*ZOPPR);
axis([x1 x2 y1 y2]);
axis('square');
xlabel('R-Raxis');
ylabel('Grad(p)');
grid
title('Pressure gradient');

%  PLOT FLUX FUNCTION PSI-PROFILE VERSUS R
subplot(2,2,3)
y1=min((B0EXP*R0EXP*R0EXP)*ZOFR);
y2=max((B0EXP*R0EXP*R0EXP)*ZOFR);
plot(R0EXP*ZABR,(B0EXP*R0EXP*R0EXP)*ZOFR);
axis([x1 x2 y1 y2]);
axis('square');
xlabel('R-Raxis');
ylabel('Psi');
grid
title('Poloidal flux function');

% PLOT TOROIDAL CURRENT DENSITY VERSUS R
subplot(2,2,4);
y1=min((B0EXP/mu0/R0EXP)*ZOJR);
y2=max((B0EXP/mu0/R0EXP)*ZOJR);
plot(R0EXP*ZABR,(B0EXP/mu0/R0EXP)*ZOJR);
axis([x1 x2 y1 y2]);
axis('square');
xlabel('R-Raxis');
ylabel('Jtor');
grid
title('Toroidal current density');

% Push-button to print graph
uicontrol(HandleList(4),...
  'String','Print',...
  'BackgroundColor','red',...
  'ForegroundColor','black',...
  'Position',[50 5 100 20],...
  'CallBack',[...
    'print ;'...
    'Chease_Plot_3']) ;

% Push-button to plot previous graph
uicontrol(HandleList(4),...
  'String','Previous',...
  'BackgroundColor','red',...
  'ForegroundColor','black',...
  'Position',[200 5 100 20],...
  'CallBack',[...
    'Chease_Plot_1']) ;
  
% Push-button to plot next graph
uicontrol(HandleList(4),...
  'String','Next',...
  'BackgroundColor','red',...
  'ForegroundColor','black',...
  'Position',[350 5 100 20],...
  'CallBack',[...
    'Chease_Plot_3']) ;
