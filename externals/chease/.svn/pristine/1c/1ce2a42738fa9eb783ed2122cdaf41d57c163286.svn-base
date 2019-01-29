%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chease_Plot_4.m : Graphic output from CHEASE %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% PRESSURE PROFILE
clf;
figure(HandleList(4));
set(HandleList(4),...
    'Position',[150,50,500,500],...
    'PaperUnits','centimeters',...
    'Paperposition',[0. 0. 21. 29.7],...
    'PaperOrientation','portrait')

% PLOT PRESSURE-PROFILE VERSUS R
subplot(2,2,1);
y1=min((B0EXP*B0EXP/mu0)*ZOPR);
y2=max((B0EXP*B0EXP/mu0)*ZOPR);
plot(R0EXP*ZABR,(B0EXP*B0EXP/mu0)*ZOPR);
axis([x1 x2 y1 y2]);
axis('square');
xlabel('R-Raxis');
ylabel('P');
grid
title('Pressure');

% PLOT PPRIME-PROFILE VERSUS R
subplot(2,2,2);
y1=min((B0EXP/mu0/R0EXP/R0EXP)*ZOPPR);
y2=max((B0EXP/mu0/R0EXP/R0EXP)*ZOPPR);
plot(R0EXP*ZABR,(B0EXP/mu0/R0EXP/R0EXP)*ZOPPR);
axis([x1 x2 y1 y2]);
axis('square');
xlabel('R-Raxis');
ylabel('Grad(p)');
grid
title('Pressure Gradient');

% PLOT POLOIDAL BETA VERSUS R
subplot(2,2,3);
y1=min(ZOBETR);
y2=max(ZOBETR);
plot(R0EXP*ZABR,ZOBETR)
axis([x1 x2 y1 y2]);
axis('square');
xlabel('R-Raxis');
ylabel('Beta-p');
grid
title('Poloidal beta')

% PLOT BOOTSTRAP CURRENT <JBOOT.B> VERSUS R
subplot(2,2,4);
y1=min(min((B0EXP/mu0/R0EXP)*ZOJBSS(:,:)));
y2=max(max((B0EXP/mu0/R0EXP)*ZOJBSS(:,:)));
plot(R0EXP*ZABR,(B0EXP/mu0/R0EXP)*ZOJBSS(:,1))
hold on
plot(R0EXP*ZABR,(B0EXP/mu0/R0EXP)*ZOJBSS(:,2),'r')
plot(R0EXP*ZABR,(B0EXP/mu0/R0EXP)*ZOJBSS(:,3),'c--')
plot(R0EXP*ZABR,(B0EXP/mu0/R0EXP)*ZOJBSS(:,4),'k')
axis([x1 x2 y1 y2]);
axis('square');
xlabel('R-Raxis');
ylabel('<Jboot.B>');
legend('\nue*=0,neTe','\nue*,ne,Te','\nue*,p*Lne','\nue*,L31p'',..')
grid
title('Bootstrap current (Sauter)');

% Push-button to print graph
uicontrol(HandleList(4),...
  'String','Print',...
  'BackgroundColor','red',...
  'ForegroundColor','black',...
  'Position',[50 5 100 20],...
  'CallBack',[...
    'print ;'...
    'Chease_Plot_5']) ;

% Push-button to plot previous graph
uicontrol(HandleList(4),...
  'String','Previous',...
  'BackgroundColor','red',...
  'ForegroundColor','black',...
  'Position',[200 5 100 20],...
  'CallBack',[...
    'Chease_Plot_3']) ;
    
% Push-button to plot next graph
uicontrol(HandleList(4),...
  'String','Next',...
  'BackgroundColor','red',...
  'ForegroundColor','black',...
  'Position',[350 5 100 20],...
  'CallBack',[...
    'Chease_Plot_5']) ;
