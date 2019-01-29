%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chease_Plot_1.m : Graphic output from CHEASE %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


mu0 = pi*4e-7 ;

dat2mat ;

%% EQUILIBRIUM 1
clf
figure(HandleList(4))
set(HandleList(4),...
    'Position',[150,50,500,500],...
    'PaperUnits','centimeters',...
    'Paperposition',[0. 0. 21. 29.7],...
    'PaperOrientation','portrait')

% PLOT FLUX SURFACES
subplot(2,2,1);
hold on
jmax = size(CR,2)-1 ;
CR = CR(:,2:jmax+1) ;
CZ = CZ(:,2:jmax+1) ;
RSHEAR = RSHEAR(:,2:jmax+1) ;
maxsurf = 10 ;
if jmax<maxsurf
  stepsurf = 1 ;
else
  stepsurf = (jmax-rem(jmax,maxsurf))/maxsurf ;
end
plot(R0EXP*CR(:,jmax:-stepsurf:max(1,jmax-stepsurf*maxsurf)), ...
     R0EXP*CZ(:,jmax:-stepsurf:max(1,jmax-stepsurf*maxsurf)),'b')
% PLOT CHI LINES
imax = size(CR,1) ;
maxchi = 25 ;
stepchi = round(imax/maxchi) ;
plot(R0EXP*CR(1:stepchi:min(imax,stepchi*maxchi+1),:)', ...
     R0EXP*CZ(1:stepchi:min(imax,stepchi*maxchi+1),:)','b')
axis('square');
axis('equal');
xlabel('R-Raxis');
ylabel('Z-Zaxis');
title('Magnetic Surfaces')

% PLOT <J.B> / <B.GRAD-PHI> PROFILE VERSUS R
subplot(2,2,2);
x1=min(R0EXP*ZABR);
x2=max(R0EXP*ZABR);
y1=min((B0EXP/mu0/R0EXP)*ZOJBR);
y2=max((B0EXP/mu0/R0EXP)*ZOJBR);
plot(R0EXP*ZABR,(B0EXP/mu0/R0EXP)*ZOJBR)
axis([x1 x2 y1 y2]);
axis('square');
xlabel('R-Raxis');
ylabel('<J.B>/<B.Grad(phi)>');
grid
title('Average parallel current density')

% PLOT Q_PROFILE VERSUS R
subplot(2,2,3);
y1=0.0;
y2=max(ZOQR);
plot(R0EXP*ZABR,ZOQR);
axis([x1 x2 y1 y2]);
axis('square');
xlabel('R-Raxis');
ylabel('q');
grid
title('Safety factor');

% PLOT PRESSURE PROFILE
subplot(2,2,4);
y1=min((B0EXP*B0EXP/mu0)*ZOPR);
y2=max((B0EXP*B0EXP/mu0)*ZOPR);
plot(R0EXP*ZABR,(B0EXP*B0EXP/mu0)*ZOPR)
axis([x1 x2 y1 y2]);
axis('square');
xlabel('R-Rmax');
ylabel('p');
grid
title('Pressure')

% Push-button to print graph
uicontrol(HandleList(4),...
  'String','Print',...
  'BackgroundColor','red',...
  'ForegroundColor','black',...
  'Position',[100 5 100 20],...
  'CallBack',[...
    'print ;'...
    'Chease_Plot_2']) ;
    
% Push-button to plot next graph
uicontrol(HandleList(4),...
  'String','Next',...
  'BackgroundColor','red',...
  'ForegroundColor','black',...
  'Position',[300 5 100 20],...
  'CallBack',[...
    'Chease_Plot_2']) ;
