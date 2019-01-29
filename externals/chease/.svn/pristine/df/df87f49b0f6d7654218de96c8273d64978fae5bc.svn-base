%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chease_Plot_3.m : Graphic output from CHEASE %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% CURRENT PROFILE
clf;
figure(HandleList(4));
set(HandleList(4),...
    'Position',[150,50,500,500],...
    'PaperUnits','centimeters',...
    'Paperposition',[0. 0. 21. 29.7],...
    'PaperOrientation','portrait')

% PLOT TOROIDAL CURRENT DENSITY VERSUS R
subplot(2,2,1);
y1=min((B0EXP/mu0/R0EXP)*ZOJR);
y2=max((B0EXP/mu0/R0EXP)*ZOJR);
plot(R0EXP*ZABR,(B0EXP/mu0/R0EXP)*ZOJR);
axis([x1 x2 y1 y2]);
axis('square');
xlabel('R-Raxis');
ylabel('Jtor');
grid
title('Toroidal current density');

% PLOT <J.B> / <B.GRAD-PHI> PROFILE VERSUS R
subplot(2,2,2);
y1=min((B0EXP/mu0/R0EXP)*ZOJBR);
y2=max((B0EXP/mu0/R0EXP)*ZOJBR);
plot(R0EXP*ZABR,(B0EXP/mu0/R0EXP)*ZOJBR)
axis([x1 x2 y1 y2]);
axis('square');
xlabel('R-Raxis');
ylabel('<J.B>/<B.Grad(phi)>');
grid
title('Average parallel current density')

% PLOT <Jtot.B> & <Jboot.B>
subplot(2,2,3);
y1=min((B0EXP/mu0/R0EXP)*ZOJPR);
y2=max((B0EXP/mu0/R0EXP)*ZOJPR);
plot(R0EXP*ZABR,(B0EXP/mu0/R0EXP)*ZOJPR)
hold on
plot(R0EXP*ZABR,(B0EXP/mu0/R0EXP)*ZOJBSS(:,2),'r');
axis([x1 x2 y1 y2]);
axis('square');
xlabel('R-Raxis');
ylabel('<Jtot.B>&<Jboot.B>');
grid
title('Bootstrap current (Sauter)')

% PLOT TTPRIME-PROFILE VERSUS R
subplot(2,2,4);
y1=min(B0EXP*ZOTTR);
y2=max(B0EXP*ZOTTR);
plot(R0EXP*ZABR,B0EXP*ZOTTR);
axis([x1 x2 y1 y2]);
axis('square');
xlabel('R-Raxis');
ylabel('TTprime');
grid
title('T=R.Bphi & derivative/Psi');

% Push-button to print graph
uicontrol(HandleList(4),...
  'String','Print',...
  'BackgroundColor','red',...
  'ForegroundColor','black',...
  'Position',[50 5 100 20],...
  'CallBack',[...
    'print ;'...
    'Chease_Plot_4']) ;
    
% Push-button to plot previous graph
uicontrol(HandleList(4),...
  'String','Previous',...
  'BackgroundColor','red',...
  'ForegroundColor','black',...
  'Position',[200 5 100 20],...
  'CallBack',[...
    'Chease_Plot_2']) ;
    
% Push-button to plot next graph
uicontrol(HandleList(4),...
  'String','Next',...
  'BackgroundColor','red',...
  'ForegroundColor','black',...
  'Position',[350 5 100 20],...
  'CallBack',[...
    'Chease_Plot_4']) ;
