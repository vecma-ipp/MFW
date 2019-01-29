%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chease_Plot_6.m : Graphic output from CHEASE %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% STABILITY 2
clf;
figure(HandleList(4));
set(HandleList(4),...
    'Position',[150,50,500,500],...
    'PaperUnits','centimeters',...
    'Paperposition',[0. 0. 21. 29.7],...
    'PaperOrientation','portrait')

% PLOT NORMALIZED RADIAL VARIABLE S VERSUS MAJOR RADIUS R
ZOS=zeros(size(ZABR));
ZOS(1:INS)=fliplr(ZABSM);
ZOS(INS:INR)=ZABSM;
ZOS(INS)=0.;
subplot(2,2,1);
y1=min(ZOS);
y2=max(ZOS);
plot(R0EXP*ZABR,ZOS);
axis([x1 x2 y1 y2]);
axis('square');
xlabel('R-Raxis');
ylabel('S');
grid
title('S=sqrt[(Psi-PsiAxis)/(PsiSurf-PsiAxis)]');

% PLOT MERCIER COEFFICIENT -DI VERSUS S
subplot(2,2,2);
x1=min(ZABSM);x2=max(ZABSM);y1=min(ZODIS);y2=max(ZODIS);
plot(ZABSM,ZODIS);
axis([x1 x2 y1 y2]);
axis('square');
xlabel('S');
ylabel('-Di');
grid
title('Ideal interchange index');

% PLOT RESISTIVE INTERCHANGE COEFFICIENT -DR VERSUS S
subplot(2,2,3);
y1=min(ZODRS);y2=max(ZODRS);
plot(ZABSM,ZODRS)
axis([x1 x2 y1 y2]);
axis('square');
xlabel('S');
ylabel('-Dr');
grid
title('Resistive Interchange Index')

% H OF GLASSER, GREENE AND JOHNSON
subplot(2,2,4);
y1=min(min(ZOHS));y2=max(max(ZOHS));
plot(ZABSM,ZOHS)
axis([x1 x2 y1 y2]);
axis('square');
xlabel('S');
ylabel('H');
grid
title('H of G.G.J');

% Push-button to print graph
uicontrol(HandleList(4),...
  'String','Print',...
  'BackgroundColor','red',...
  'ForegroundColor','black',...
  'Position',[50 5 100 20],...
  'CallBack',[...
    'print ;'...
    'close(HandleList(4)) ;'...
    'HandleList = HandleList(1:3) ;']) ;

% Push-button to plot previous graph
uicontrol(HandleList(4),...
  'String','Previous',...
  'BackgroundColor','red',...
  'ForegroundColor','black',...
  'Position',[200 5 100 20],...
  'CallBack',[...
    'Chease_Plot_5']) ;
    
% Push-button to close graph window
uicontrol(HandleList(4),...
  'String','Close',...
  'BackgroundColor','red',...
  'ForegroundColor','black',...
  'Position',[350 5 100 20],...
  'CallBack',[...
    'close(HandleList(4)) ;'...
    'HandleList = HandleList(1:3) ;']) ;


% Call routine to perform Hegna's calculations
%Hegna


